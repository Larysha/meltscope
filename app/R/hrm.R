# MeltScope analysis functions: plain R, no Shiny, so they can be scripted and tested.
# Workflow mirrors Thermo Fisher HRM Software: raw -> normalise (pre/post-melt
# regions) -> optional temperature shift -> difference plot -> variant calling.

norm_name <- function(x) gsub("[^a-z0-9]", "", tolower(x))

# Sort wells A1, A2, ..., A10 (not A1, A10, A2)
sort_wells <- function(w) {
  w[order(match(substr(w, 1, 1), LETTERS), suppressWarnings(as.integer(substring(w, 2))), w)]
}

num <- function(x) if (is.numeric(x)) x else suppressWarnings(as.numeric(gsub(",", "", x)))

# ---- Import -------------------------------------------------------------------

# Turn a table whose header row may sit below instrument metadata into long format:
# data.frame(well, target, temp, fluor). Accepts
#  * long exports (QuantStudio "Melt Curve Raw": Well Position / Temperature / Fluorescence)
#  * wide matrices (a Temperature column + one column per well; Bio-Rad CFX, or hrmR format)
parse_table <- function(tab) {
  tab <- as.data.frame(tab, stringsAsFactors = FALSE)
  first_rows <- min(nrow(tab), 100)
  hdr_of <- function(pred) {
    for (i in seq_len(first_rows)) if (pred(norm_name(unlist(tab[i, ])))) return(i)
    NA
  }
  long_row <- hdr_of(function(r) any(r %in% c("wellposition", "well")) && any(grepl("fluorescence|^rn$|^rfu$", r)))
  if (!is.na(long_row)) {
    names(tab) <- norm_name(unlist(tab[long_row, ]))
    d <- tab[-seq_len(long_row), , drop = FALSE]
    wcol <- if ("wellposition" %in% names(d)) "wellposition" else "well"
    fcol <- intersect(c("fluorescence", "rn", "rfu"), names(d))[1]
    tcol <- intersect(c("temperature", "temp"), names(d))[1]
    if (is.na(tcol)) stop("Found a well column but no Temperature column.")
    gcol <- intersect(c("target", "targetname", "target name"), names(d))[1]
    out <- data.frame(
      well = trimws(as.character(d[[wcol]])),
      target = if (is.na(gcol)) "All" else as.character(d[[gcol]]),
      temp = num(d[[tcol]]),
      fluor = num(d[[fcol]]),
      stringsAsFactors = FALSE
    )
    return(out[!is.na(out$temp) & !is.na(out$fluor) & nzchar(out$well), ])
  }
  wide_row <- hdr_of(function(r) any(r == "temperature"))
  if (!is.na(wide_row)) {
    hdr <- trimws(as.character(unlist(tab[wide_row, ])))
    d <- tab[-seq_len(wide_row), , drop = FALSE]
    tcol <- which(norm_name(hdr) == "temperature")[1]
    temp <- num(d[[tcol]])
    keep <- setdiff(which(!is.na(hdr) & nzchar(hdr) & hdr != "NA"), tcol)
    keep <- keep[vapply(keep, function(j) any(!is.na(num(d[[j]]))), logical(1))]
    out <- do.call(rbind, lapply(keep, function(j) {
      data.frame(well = hdr[j], target = "All", temp = temp, fluor = num(d[[j]]), stringsAsFactors = FALSE)
    }))
    return(out[!is.na(out$temp) & !is.na(out$fluor), ])
  }
  stop("Could not find a header row. Expected 'Well Position' + 'Temperature' + 'Fluorescence' ",
       "columns (long export) or a 'Temperature' column followed by one column per well.")
}

read_melt <- function(path, name = path) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("xlsx", "xls")) {
    sheets <- readxl::excel_sheets(path)
    # Prefer the raw melt sheet in full QuantStudio exports
    sheets <- c(grep("melt.*raw", sheets, ignore.case = TRUE, value = TRUE), sheets)
    for (s in unique(sheets)) {
      tab <- readxl::read_excel(path, sheet = s, col_names = FALSE, col_types = "text", .name_repair = "minimal")
      res <- tryCatch(parse_table(tab), error = function(e) NULL)
      if (!is.null(res) && nrow(res)) return(res)
    }
    stop("No sheet in this workbook looks like melt-curve data.")
  }
  lines <- readLines(path, warn = FALSE)
  hdr <- grep("temperature", lines, ignore.case = TRUE)[1]
  if (is.na(hdr)) stop("Could not find a 'Temperature' column in this file.")
  counts <- vapply(c(",", ";", "\t"), function(s) sum(gregexpr(s, lines[hdr], fixed = TRUE)[[1]] > 0), 0)
  sep <- names(which.max(counts))
  tab <- utils::read.table(text = lines[hdr:length(lines)], sep = sep, header = FALSE, quote = "\"",
                           colClasses = "character", fill = TRUE, comment.char = "", na.strings = c("", "NA"))
  parse_table(tab)
}

# Long data -> common temperature grid. Instruments record a slightly different temperature
# per heating zone, so each well is interpolated onto one shared grid rather than averaged.
to_matrix <- function(long) {
  wells <- sort_wells(unique(long$well))
  per <- split(long[c("temp", "fluor")], factor(long$well, levels = wells))
  lo <- max(vapply(per, function(d) min(d$temp), 0))
  hi <- min(vapply(per, function(d) max(d$temp), 0))
  step <- stats::median(diff(sort(per[[1]]$temp)))
  temp <- seq(lo, hi, by = step)
  F <- vapply(per, function(d) {
    d <- d[order(d$temp), ]
    stats::approx(d$temp, d$fluor, xout = temp, ties = mean)$y
  }, numeric(length(temp)))
  colnames(F) <- wells
  list(temp = temp, F = F)
}

# ---- Curve processing ------------------------------------------------------

# Smoothing spline per well; returns smoothed fluorescence and -dF/dT.
smooth_curves <- function(temp, F, spar = 0.6) {
  fits <- lapply(seq_len(ncol(F)), function(j) stats::smooth.spline(temp, F[, j], spar = spar))
  S <- vapply(fits, function(f) stats::predict(f, temp)$y, numeric(length(temp)))
  D <- -vapply(fits, function(f) stats::predict(f, temp, deriv = 1)$y, numeric(length(temp)))
  dimnames(S) <- dimnames(D) <- dimnames(F)
  list(S = S, D = D)
}

# Melting temperatures from -dF/dT peaks. Tm2 = second-highest peak if it reaches
# min_frac of the main peak and lies at least min_sep degrees away.
find_tm <- function(temp, D, min_frac = 0.25, min_sep = 1) {
  t(apply(D, 2, function(d) {
    n <- length(d)
    pk <- which(diff(sign(diff(d))) == -2) + 1
    if (!length(pk)) return(c(Tm1 = NA, Tm2 = NA, height = NA))
    refine <- function(i) {  # parabolic interpolation around the sampled maximum
      if (i <= 1 || i >= n) return(temp[i])
      y <- d[(i - 1):(i + 1)]
      den <- y[1] - 2 * y[2] + y[3]
      off <- if (den == 0) 0 else 0.5 * (y[1] - y[3]) / den
      temp[i] + off * (temp[i + 1] - temp[i])
    }
    pk <- pk[order(d[pk], decreasing = TRUE)]
    top <- pk[1]
    rest <- pk[d[pk] >= min_frac * d[top] & abs(temp[pk] - temp[top]) >= min_sep]
    c(Tm1 = refine(top), Tm2 = if (length(rest)) refine(rest[1]) else NA, height = d[top])
  }))
}

# Suggest pre/post-melt regions: per well, find where the melt peak rises above / falls
# below `frac` of its height over the local floor on each side (raw -dF/dT has a sloping
# background before the melt). Use the earliest start / latest end across wells (5th/95th
# percentile, so one odd well can't drag the windows), then 1 degree windows just outside.
# Failed wells / NTCs (peak < 25% of the median peak) are ignored: their "peak" is noise at
# the edge of the run, and a full-plate export can hold more of them than the percentile absorbs.
auto_windows <- function(temp, D, frac = 0.1, width = 1, gap = 1) {
  prom <- apply(D, 2, function(m) max(m) - min(m))
  ok <- prom >= 0.25 * stats::median(prom)
  if (any(ok)) D <- D[, ok, drop = FALSE]
  n <- nrow(D)
  edges <- apply(D, 2, function(m) {
    top <- which.max(m)
    lmin <- min(m[seq_len(top)])
    rmin <- min(m[top:n])
    c(max(c(1, which(m[seq_len(top)] < lmin + frac * (m[top] - lmin)))),
      top - 1 + min(c(n - top + 1, which(m[top:n] < rmin + frac * (m[top] - rmin)))))
  })
  left <- stats::quantile(temp[edges[1, ]], 0.05, names = FALSE)
  right <- stats::quantile(temp[edges[2, ]], 0.95, names = FALSE)
  pre_hi <- max(temp[1] + width, left - gap)
  post_lo <- min(temp[length(temp)] - width, right + gap)
  r <- function(x) round(x, 1)
  list(pre = r(c(pre_hi - width, pre_hi)), post = r(c(post_lo, post_lo + width)))
}

# Thermo/Wittwer normalisation: fit a straight line to each of the pre- and post-melt
# regions; 100% = pre-melt line, 0% = post-melt line.
normalise <- function(temp, S, pre, post) {
  ip <- temp >= pre[1] & temp <= pre[2]
  iq <- temp >= post[1] & temp <= post[2]
  if (sum(ip) < 3 || sum(iq) < 3) stop("Pre- and post-melt regions must each cover at least 3 readings.")
  N <- apply(S, 2, function(y) {
    a <- stats::lm.fit(cbind(1, temp[ip]), y[ip])$coefficients
    b <- stats::lm.fit(cbind(1, temp[iq]), y[iq])$coefficients
    top <- a[1] + a[2] * temp
    bot <- b[1] + b[2] * temp
    100 * (y - bot) / (top - bot)
  })
  dimnames(N) <- dimnames(S)
  N
}

# Temperature shift: slide each normalised curve along the temperature axis so all cross
# `threshold`% at the same temperature (the mean crossing). Removes small Tm offsets
# (e.g. well position, salt) so shape differences stand out. Note: also removes the
# Tm difference that separates some homozygous variants -> off by default.
# Only the analysis `region` is searched: outside it the extrapolated baselines make
# normalised values meaningless (and they can cross the threshold anywhere).
temp_shift <- function(temp, N, threshold = 5, use = rep(TRUE, ncol(N)), region = rep(TRUE, length(temp))) {
  ri <- which(region)
  cross <- apply(N[ri, , drop = FALSE], 2, function(y) {
    t <- temp[ri]
    i <- which(y[-length(y)] >= threshold & y[-1] < threshold)
    if (!length(i)) return(NA)
    i <- i[length(i)]
    t[i] + (y[i] - threshold) / (y[i] - y[i + 1]) * (t[i + 1] - t[i])
  })
  shift <- cross - mean(cross[use], na.rm = TRUE)  # reference: analysed wells only
  shift[is.na(shift)] <- 0
  out <- vapply(seq_len(ncol(N)), function(j) stats::approx(temp - shift[j], N[, j], xout = temp, rule = 2)$y,
                numeric(length(temp)))
  dimnames(out) <- dimnames(N)
  attr(out, "shift") <- stats::setNames(shift, colnames(N))
  out
}

# ---- Variant calling ---------------------------------------------------------

# Cluster wells (rows of X). Curves are reduced with PCA first (clustering ~600 correlated
# temperature points directly is unstable). Returns variant number (1 = largest group),
# confidence (%) = posterior probability under spherical Gaussian clusters, and chosen k.
# min_diff: in Auto mode, groups whose average curves differ by less than this (max
# absolute difference, in the units of X: % fluorescence or degrees C) are not split.
# first: logical, rows that are control wells; groups holding controls are numbered first.
call_variants <- function(X, k = NA, method = c("hierarchical", "kmeans"), k_max = 6, min_diff = 0,
                          first = rep(FALSE, nrow(X))) {
  method <- match.arg(method)
  X <- X0 <- as.matrix(X)
  n <- nrow(X)
  if (n < 3) return(list(variant = rep(1L, n), confidence = rep(100, n), k = 1L, silhouette = NULL, scores = X))
  if (ncol(X) > 1) {
    pc <- stats::prcomp(X, center = TRUE, scale. = FALSE)
    ve <- cumsum(pc$sdev^2) / sum(pc$sdev^2)
    X <- pc$x[, seq_len(min(5, n - 1, which(ve >= 0.95)[1])), drop = FALSE]
  }
  fit <- function(kk) {
    if (kk == 1) return(rep(1L, n))
    set.seed(1)
    if (method == "kmeans") stats::kmeans(X, kk, nstart = 50)$cluster
    else stats::cutree(stats::hclust(stats::dist(X), "ward.D2"), kk)
  }
  d <- stats::dist(X)
  ks <- 2:min(k_max, n - 1)
  fits <- lapply(ks, fit)
  sil <- vapply(fits, function(cl) mean(cluster::silhouette(cl, d)[, 3]), 0)
  if (is.na(k)) {
    # Auto: silhouette is scale-free (even pure noise can "separate"), so a split also has to
    # change the curves by at least min_diff. Among valid k, take the fewest groups within
    # 0.05 of the best silhouette (near-ties don't flip when one well is excluded).
    gap <- vapply(fits, function(cl) {
      mu <- do.call(rbind, lapply(split(seq_len(n), cl), function(i) colMeans(X0[i, , drop = FALSE])))
      min(apply(utils::combn(nrow(mu), 2), 2, function(ij) max(abs(mu[ij[1], ] - mu[ij[2], ]))))
    }, 0)
    ok <- sil >= 0.5 & gap >= min_diff
    k <- if (any(ok)) ks[ok][which(sil[ok] >= max(sil[ok]) - 0.05)[1]] else 1L
  }
  k <- min(k, n)
  cl <- fit(k)
  # Renumber: groups holding control wells first, then the rest; each by size
  ord <- as.integer(names(sort(table(cl), decreasing = TRUE)))
  ord <- c(ord[ord %in% cl[first]], ord[!ord %in% cl[first]])
  cl <- match(cl, ord)
  centres <- do.call(rbind, lapply(seq_len(k), function(g) colMeans(X[cl == g, , drop = FALSE])))
  if (k == 1) {
    conf <- rep(100, n)
  } else {
    d2 <- vapply(seq_len(k), function(g) rowSums(sweep(X, 2, centres[g, ])^2), numeric(n))
    s2 <- max(sum(d2[cbind(seq_len(n), cl)]) / (n * ncol(X)), 1e-12)
    ll <- -d2 / (2 * s2)
    ll <- ll - apply(ll, 1, max)
    post <- exp(ll) / rowSums(exp(ll))
    conf <- 100 * post[cbind(seq_len(n), cl)]
  }
  list(variant = cl, confidence = conf, k = as.integer(k), silhouette = data.frame(k = ks, silhouette = sil),
       scores = X)
}

# ---- One-call pipeline (used by the app and tests) ----------------------------

# p: list(spar, pre, post, shift, shift_thr, neg_pct, features, method, k, min_conf,
#         min_diff_shape (%), min_diff_tm (degrees C))
# controls: named character vector, well -> control name (e.g. c(G1 = "Ch WT")). Each group holding
# control wells is named after them; the other groups are "Variant 1", "Variant 2", ...
analyse <- function(m, p, exclude = character(), controls = character()) {
  if (is.null(names(controls))) controls <- stats::setNames(rep("Control", length(controls)), controls)
  sm <- smooth_curves(m$temp, m$F, p$spar)
  reg <- m$temp >= p$pre[1] & m$temp <= p$post[2]  # Tm peaks only inside the analysis region
  tm <- find_tm(m$temp[reg], sm$D[reg, , drop = FALSE])
  ip <- m$temp >= p$pre[1] & m$temp <= p$pre[2]
  iq <- m$temp >= p$post[1] & m$temp <= p$post[2]
  amp <- colMeans(sm$S[ip, , drop = FALSE]) - colMeans(sm$S[iq, , drop = FALSE])
  negative <- amp < p$neg_pct / 100 * stats::median(amp)
  N <- normalise(m$temp, sm$S, p$pre, p$post)
  wells <- colnames(m$F)
  if (isTRUE(p$shift)) N <- temp_shift(m$temp, N, p$shift_thr, use = !negative & !wells %in% exclude, region = reg)
  use <- wells[!negative & !wells %in% exclude]
  call <- rep(NA_character_, length(wells))
  conf <- rep(NA_real_, length(wells))
  sil <- NULL
  scores <- NULL
  k <- 0L
  levels <- character()
  if (length(use)) {
    melt <- m$temp >= p$pre[2] & m$temp <= p$post[1]
    X <- if (p$features == "tm") tm[use, "Tm1", drop = FALSE] else t(N[melt, use, drop = FALSE])
    min_diff <- if (p$features == "tm") p$min_diff_tm else p$min_diff_shape
    cv <- call_variants(X, if (identical(p$k, "auto")) NA else as.integer(p$k), p$method, min_diff = min_diff %||% 0,
                        first = use %in% names(controls))
    ctrl <- unname(controls[use])
    levels <- vapply(seq_len(cv$k), function(v) {
      l <- unique(ctrl[cv$variant == v & !is.na(ctrl)])
      if (length(l)) paste(l, collapse = " / ") else NA_character_
    }, "")
    levels[is.na(levels)] <- paste("Variant", seq_len(sum(is.na(levels))))
    n <- stats::ave(seq_along(levels), levels, FUN = seq_along)  # same control name in two groups
    levels <- ifelse(n > 1, paste0(levels, " (", n, ")"), levels)
    i <- match(use, wells)
    call[i] <- levels[cv$variant]
    conf[i] <- cv$confidence
    sil <- cv$silhouette
    scores <- cv$scores
    rownames(scores) <- use
    k <- cv$k
  }
  status <- ifelse(!is.na(conf) & conf < p$min_conf, "Low confidence", "Called")
  status[negative] <- "Negative"
  status[wells %in% exclude] <- "Excluded"
  call_lab <- ifelse(status == "Called", call, ifelse(status == "Low confidence", "Unknown", status))
  list(
    temp = m$temp, F = m$F, S = sm$S, D = sm$D, N = N, k = k, levels = levels, silhouette = sil, scores = scores,
    table = data.frame(Well = wells, Call = call_lab, Variant = call, Confidence = round(conf, 1),
                       Tm1 = round(tm[, "Tm1"], 2), Tm2 = round(tm[, "Tm2"], 2),
                       Amplitude = signif(amp, 3), Status = status, stringsAsFactors = FALSE)
  )
}
