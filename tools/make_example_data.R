# Generates MeltScope's SYNTHETIC example plate (no real samples).
# Run from the meltscope folder:  Rscript tools/make_example_data.R
#
# Model: a two-domain amplicon read with a saturating dye. A stabilising SNP in the lower
# domain raises it from 80.7 to 81.2 C (homozygous mutant); heterozygotes carry both
# homoduplexes plus less stable heteroduplexes (~79.8 C), giving the typical early shoulder.
# The dye signal also fades with temperature, there is a small post-melt background, read noise,
# well-to-well amplitude variation and small heating-zone (column pair) Tm offsets.

make_example_plate <- function(seed = 2026) {
  set.seed(seed)
  temp <- seq(65, 95, by = 0.05)
  domains <- list(
    WT  = list(tm = c(80.7, 82.3), w = c(0.30, 0.35), f = c(0.30, 0.70)),
    Hom = list(tm = c(81.2, 82.3), w = c(0.30, 0.35), f = c(0.30, 0.70)),
    Het = list(tm = c(80.7, 81.2, 79.8, 82.3), w = c(0.30, 0.30, 0.40, 0.35), f = c(0.075, 0.075, 0.15, 0.70))
  )
  wells <- paste0(rep(LETTERS[1:4], each = 12), 1:12)
  geno <- c(rep("WT", 3), rep("Het", 3), rep("Hom", 3), "NTC", "NTC",
            sample(c(rep("WT", 17), rep("Het", 13), rep("Hom", 7))))
  names(geno) <- wells
  zone_tm <- c(0.03, 0.05, 0.02, -0.08, 0.04, 0.01)  # per column pair 1-2, 3-4, ...
  zone_t  <- c(0.002, -0.003, 0.001, -0.004, 0.003, 0.000)

  curve <- function(g, col) {
    d <- domains[[if (g == "NTC") "WT" else g]]
    off <- zone_tm[(col + 1) %/% 2]
    ds <- Reduce(`+`, Map(function(tm, w, f) f / (1 + exp((temp - tm - off) / w)), d$tm, d$w, d$f))
    amp <- stats::rnorm(1, 1.6e6, 2e5) * if (g == "NTC") 0.01 else 1
    amp * ds * exp(-0.06 * (temp - 65)) + 5e4 - 500 * (temp - 85) + stats::rnorm(length(temp), 0, 1500)
  }
  long <- do.call(rbind, lapply(seq_along(wells), function(i) {
    col <- as.integer(substring(wells[i], 2))
    data.frame(Well = i, Well_Position = wells[i], Reading_Number = seq_along(temp), Target = "EXAMPLE",
               Temperature = round(temp + zone_t[(col + 1) %/% 2], 4), Fluorescence = round(curve(geno[i], col), 3))
  }))
  sample_names <- c(paste("Control WT", 1:3), paste("Control Het", 1:3), paste("Control Hom", 1:3),
                    paste("NTC", 1:2), sprintf("S%02d", 1:37))
  samples <- data.frame(Well = wells, Sample = sample_names)
  list(long = long, samples = samples, genotype = geno)
}

if (sys.nframe() == 0) {
  ex <- make_example_plate()
  dir <- "app/example"
  writexl::write_xlsx(ex$long, file.path(dir, "example_plate_long.xlsx"))
  utils::write.csv(ex$long, file.path(dir, "example_plate_long.csv"), row.names = FALSE)
  wide <- stats::reshape(ex$long[c("Well_Position", "Reading_Number", "Fluorescence")], idvar = "Reading_Number",
                         timevar = "Well_Position", direction = "wide")
  names(wide) <- sub("^Fluorescence\\.", "", names(wide))
  wide$Reading_Number <- tapply(ex$long$Temperature, ex$long$Reading_Number, mean)[as.character(wide$Reading_Number)]
  names(wide)[1] <- "Temperature"
  utils::write.csv(wide, file.path(dir, "example_plate_wide.csv"), row.names = FALSE)
  utils::write.csv(ex$samples, file.path(dir, "example_sample_names.csv"), row.names = FALSE)
  cat("Wrote example files to", dir, "\n")
}
