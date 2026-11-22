# MeltScope

**Free High Resolution Melt (HRM) analysis.** Upload the melt curve export from your qPCR machine and MeltScope
normalises the curves, draws difference and derivative plots, and groups wells into variants (genotypes), following
the same workflow as Thermo Fisher's HRM Software.

- Works with the file straight off the machine (Excel **or** CSV), with no reformatting.
- Every setting can be changed, and every plot updates immediately.
- Interactive plots: hover to see the well, zoom in, and save publication-quality PNG or SVG.
- Your data never leaves your computer, even in the browser version.

---

## How to open MeltScope

Pick **one** of these options.

### Option A: in your web browser (easiest, nothing to install)

Open **https://larysha.github.io/meltscope/** in Chrome, Firefox, Edge or Safari.

The first load takes about 20–60 seconds while the analysis engine downloads into your browser, and later visits are
faster. Everything runs on your own computer: files you upload are not sent anywhere.

### Option B: in RStudio, with one line

1. Install [R](https://cran.r-project.org) and [RStudio Desktop](https://posit.co/download/rstudio-desktop/) (free).
2. Open RStudio. In the **Console** (bottom-left panel), paste this line and press Enter. You only need to do this once:
   ```r
   install.packages(c("shiny", "bslib", "plotly", "DT", "readxl", "jsonlite"))
   ```
3. Every time you want to use MeltScope, paste this line and press Enter:
   ```r
   shiny::runGitHub("meltscope", "larysha", subdir = "app")
   ```
   The app opens in a window, or in your browser.

### Option C: download it and run offline

1. On the GitHub page, click the green **Code** button, then **Download ZIP**, and unzip it.
2. In RStudio, open `app/app.R` and click **▶ Run App** (top-right of the script). Do step 2 of Option B once first.

If a package is missing, MeltScope stops and shows you the exact `install.packages(...)` line to run.

---

## What file do I upload?

**The export from your instrument, unchanged.** MeltScope finds the data table inside the file by itself, even if
there are instrument notes above it. Excel (`.xlsx`, `.xls`) and text files (`.csv`, `.txt`; comma, semicolon or
tab separated) all work.

| Layout | What it looks like | Typical source |
|---|---|---|
| **Long** (one row per reading) | columns `Well Position`, `Temperature`, `Fluorescence` (optionally `Target`, `Reading`) | QuantStudio / Design & Analysis: *Export → Melt Curve Raw Data* |
| **Wide** (one column per well) | a `Temperature` column, then one column per well (`A1`, `A2`, …) | Bio-Rad CFX *Melt Curve RFU Results*, or a hand-made matrix |

Example files for both layouts are in [`app/example/`](app/example/), and can also be downloaded from inside the
app (step 1 in the sidebar). The example plate is **synthetic** (simulated WT, heterozygous and homozygous wells plus
NTCs; see [`tools/make_example_data.R`](tools/make_example_data.R)). If your file has several assays (`Target` column), you can choose which one to analyse.

**Sample names (optional):** a small table with a `Well` column and a `Sample` column (see
[`app/example/example_sample_names.csv`](app/example/example_sample_names.csv)). The app can generate a template
for your plate.

---

## Analysing a plate in six steps

1. **Load data.** Upload your file, or click *Load example data* to try it out.
2. **Check the melt regions.** On the *Raw data* tab, the blue (pre-melt) and red (post-melt) bands should sit on the
   flat parts before and after the melt. They are suggested automatically; drag the sliders to adjust.
3. **Look at the Normalised and Difference plots.** Wells with the same genotype have the same curve shape.
4. **Check the calls** on *Plate & calls*. Set *Number of variants* if you know how many genotypes to expect, and name
   them (e.g. `WT, Het, Hom`) once you know which is which from your controls.
5. **Remove bad wells** by clicking them on the plate map (click again to bring them back).
6. **Export.** Use the camera icon on any plot for a PNG or SVG, and *Results table → Download* for a spreadsheet of
   calls and Tm values. *Save settings* stores everything so the analysis can be repeated exactly.

The **Help** tab in the app explains every setting.

---

## Troubleshooting

| Problem | What to do |
|---|---|
| "Could not find a header row" | The file needs a `Temperature` column, plus either `Well Position` + `Fluorescence` columns or one column per well. Check that you exported *raw melt* data rather than the results summary. |
| "Pre-/post-melt regions must each cover at least 3 readings" | Widen the region sliders slightly. |
| Curves look jagged in the derivative plot | Increase *Smoothing* a little (this can shift Tm slightly; see Help). |
| A well is called *Negative* | Its fluorescence barely changed (e.g. NTC or failed PCR). Lower the *Negative* threshold if that is wrong. |
| Browser version stuck on "loading" | Wait up to a minute on first use. Try Chrome or Firefox, or use Option B. |

---

## For maintainers

```
meltscope/
├── app/
│   ├── app.R          # user interface + plots (Shiny)
│   ├── R/hrm.R        # all analysis maths, plain R (no Shiny), loaded automatically
│   ├── help.html      # in-app Help tab
│   └── example/       # synthetic example plate (long Excel/CSV, wide CSV) + sample names
├── tests/test_hrm.R   # run: Rscript tests/test_hrm.R
├── tools/make_example_data.R      # regenerates the synthetic example plate
└── .github/workflows/deploy.yml   # tests, builds the browser version, publishes to GitHub Pages
```

- **Publishing the browser version:** push to `main` on GitHub, then in the repo go to *Settings → Pages → Source:
  GitHub Actions*. Each push re-runs the tests and redeploys automatically.
- **Building the browser version locally:** `install.packages("shinylive")` (on Linux this first needs
  `sudo apt install libarchive-dev`), then `shinylive::export("app", "site")` and
  `httpuv::runStaticServer("site")`.
- **Code style:** `lintr::lint_dir(".")` (rules in `.lintr`).
- **Using the analysis without the app:** `source("app/R/hrm.R")`, then
  `m <- to_matrix(read_melt("file.xlsx"))` and `analyse(m, params)` (see `tests/test_hrm.R` for a full example).
- Only packages that also run in the browser (webR) can be used: currently shiny, bslib, plotly, DT, readxl,
  jsonlite and cluster.

## Credits and licence

Developed by Larysha Rothmann. Inspired by [hrmR](https://github.com/pavlohrab/hrmR) (Pavlo Hrab), rewritten from
scratch. The analysis follows the standard HRM workflow (pre/post-melt normalisation, temperature shifting, difference
plots, automatic variant calling) used by commercial HRM software.

Released under the [MIT licence](LICENSE).
