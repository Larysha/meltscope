# Run from the meltscope folder:  Rscript tests/test_hrm.R
source("app/R/hrm.R")

# --- Synthetic plate: two genotypes with known Tm, plus one negative well -----------
set.seed(42)
temp <- seq(70, 95, by = 0.05)
melt <- function(tm, w = 0.6) 1 / (1 + exp((temp - tm) / w))
mk <- function(tm, amp = 1e6) amp * melt(tm) + 2e5 - 3e3 * (temp - 70) + rnorm(length(temp), 0, 500)
F <- cbind(sapply(1:6, function(i) mk(82)), sapply(1:4, function(i) mk(83)), mk(82, amp = 1e4))
colnames(F) <- c(paste0("A", 1:6), paste0("B", 1:4), "C1")
m <- list(temp = temp, F = F)

sm <- smooth_curves(temp, F, 0.4)
tm <- find_tm(temp, sm$D)
stopifnot(abs(tm[1:6, "Tm1"] - 82) < 0.05, abs(tm[7:10, "Tm1"] - 83) < 0.05)

w <- auto_windows(temp, sm$D[, 1:10])
stopifnot(w$pre[2] < 81, w$post[1] > 84)

N <- normalise(temp, sm$S, w$pre, w$post)
ip <- temp >= w$pre[1] & temp <= w$pre[2]
stopifnot(abs(mean(N[ip, 1]) - 100) < 1)

p <- list(spar = 0.4, pre = w$pre, post = w$post, shift = FALSE, shift_thr = 5, neg_pct = 20,
          features = "shape", method = "hierarchical", k = "auto", min_conf = 80,
          min_diff_shape = 2, min_diff_tm = 0.2)
r <- analyse(m, p)
tab <- r$table
stopifnot(r$k == 2,
          tab["C1", "Status"] == "Negative",
          length(unique(tab$Variant[1:6])) == 1, length(unique(tab$Variant[7:10])) == 1,
          tab$Variant[1] == "Variant 1", tab$Variant[7] == "Variant 2")  # Variant 1 = largest group

# Temperature shift removes a pure Tm offset, so both groups then collapse into one
# (explicit windows well clear of the melt, so normalisation is exact)
r_shift <- analyse(m, modifyList(p, list(shift = TRUE, pre = c(74, 75), post = c(90, 91))))
stopifnot(r_shift$k == 1)

# Control wells: their group is named after them (and listed first, though it is the smaller group);
# the rest are numbered from Variant 1
r_ctrl <- analyse(m, p, controls = c("B1", "B2"))
stopifnot(r_ctrl$table["B1", "Variant"] == "Control", r_ctrl$table["B4", "Variant"] == "Control",
          r_ctrl$table["A1", "Variant"] == "Variant 1", identical(r_ctrl$levels, c("Control", "Variant 1")))
# Two named controls, as in Thermo's Task/Control (e.g. Ch WT, MV WT)
r_two <- analyse(m, p, controls = c(A1 = "Ch WT", B1 = "MV WT"))
stopifnot(identical(r_two$levels, c("Ch WT", "MV WT")), r_two$table["A6", "Variant"] == "Ch WT",
          r_two$table["B3", "Variant"] == "MV WT")
# Same name in two different groups: kept apart rather than merged
r_split <- analyse(m, p, controls = c(A1 = "WT", B1 = "WT"))
stopifnot(identical(r_split$levels, c("WT", "WT (2)")))

# Exclusion
stopifnot(analyse(m, p, exclude = "A1")$table["A1", "Status"] == "Excluded")

# --- Import: long and wide formats give the same matrix ------------------------------
long <- data.frame(well = rep(colnames(F), each = length(temp)), target = "X",
                   temp = rep(temp, ncol(F)), fluor = as.vector(F))
f_wide <- tempfile(fileext = ".csv")
write.csv(cbind(Temperature = temp, F), f_wide, row.names = FALSE)
f_long <- tempfile(fileext = ".csv")
writeLines(c("* Instrument metadata line", "* Another one", ""), f_long)
long_export <- data.frame(`Well Position` = long$well, Temperature = long$temp, Fluorescence = long$fluor,
                          check.names = FALSE)
suppressWarnings(write.table(long_export, f_long, sep = ",", row.names = FALSE, append = TRUE))
a <- to_matrix(read_melt(f_wide))
b <- to_matrix(suppressWarnings(read_melt(f_long)))
stopifnot(identical(colnames(a$F), colnames(F)), max(abs(a$F - b$F)) < 1e-6)
for (sep in c(";", "\t")) {  # semicolon- and tab-separated exports
  f_sep <- tempfile(fileext = ".txt")
  write.table(cbind(Temperature = temp, F), f_sep, sep = sep, row.names = FALSE)
  stopifnot(max(abs(to_matrix(read_melt(f_sep))$F - a$F)) < 1e-6)
}
stopifnot(identical(sort_wells(c("A10", "B1", "A2", "A1")), c("A1", "A2", "A10", "B1")))

# --- Synthetic example plate: genotypes recovered exactly ----------------------------
source("tools/make_example_data.R")
truth <- make_example_plate()$genotype
mx <- to_matrix(read_melt("app/example/example_plate_long.xlsx"))
wx <- auto_windows(mx$temp, smooth_curves(mx$temp, mx$F, 0.4)$D)
rx <- analyse(mx, modifyList(p, list(pre = wx$pre, post = wx$post, shift = TRUE)))$table
stopifnot(all(rx$Status[truth[rx$Well] == "NTC"] == "Negative"))
called <- rx$Status == "Called"
stopifnot(all(rowSums(table(truth[rx$Well[called]], rx$Variant[called]) > 0) == 1),  # each genotype -> one variant
          length(unique(rx$Variant[called])) == 3)

# --- Optional: real instrument export vs the instrument's own derivative peaks ------
# Set MELTSCOPE_REAL_XLSX to a QuantStudio "Melt Curve Raw" .xlsx (with a Derivative column) to run.
ex <- Sys.getenv("MELTSCOPE_REAL_XLSX")
if (nzchar(ex) && file.exists(ex)) {
  m1 <- to_matrix(read_melt(ex))
  raw <- readxl::read_excel(ex)
  inst <- sapply(split(raw, raw$Well_Position), function(d) d$Temperature[which.max(d$Derivative)])
  tm1 <- find_tm(m1$temp, smooth_curves(m1$temp, m1$F, 0.4)$D)[, "Tm1"]
  stopifnot(max(abs(tm1 - inst[names(tm1)])) < 0.06)
  cat("Real-data check passed.\n")
}

cat("All MeltScope tests passed.\n")
