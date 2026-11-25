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

The first visit takes a little while (about a minute on a fast connection) while the analysis engine downloads into
your browser; later visits are much faster. Everything runs on your own computer: files you upload are not sent anywhere.

### Option B: in RStudio, with one line

1. Install [R](https://cran.r-project.org) and [RStudio Desktop](https://posit.co/download/rstudio-desktop/) (free).
2. Open RStudio. In the **Console** (bottom-left panel), paste this line and press Enter. You only need to do this once:
   ```r
   install.packages(c("shiny", "bslib", "readxl"))
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
app (the *1. Data* menu). The example plate is **synthetic** (simulated WT, heterozygous and homozygous wells plus
NTCs; see [`tools/make_example_data.R`](tools/make_example_data.R)). If your file has several assays (`Target` column), you can choose which one to analyse.

**Sample names (optional):** a small table with a `Well` column and a `Sample` column (see
[`app/example/example_sample_names.csv`](app/example/example_sample_names.csv)). The app can generate a template
for your plate.

---

## Analysing a plate in six steps

1. **Load data.** Upload your file, or click *or try an example plate*.
2. **Check the melt regions.** With *Raw* picked above the plot, the blue (pre-melt) and red (post-melt) bands should sit on the
   flat parts before and after the melt. They are suggested automatically; drag the sliders to adjust.
3. **Look at the Normalised and Difference plots.** Wells with the same genotype have the same curve shape.
4. **Check the calls** on the plate map (*Plate & curves* tab). Set *Number of variants* if you know how many genotypes to expect, and name
   them (e.g. `WT, Het, Hom`) once you know which is which from your controls.
5. **Compare and clean up.** Click or drag across wells on the plate map to select them: the plot next to the plate
   highlights them (with the controls) and greys out the rest. *Exclude* / *Include* removes or restores the selected
   wells.
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
| Browser version slow to start | The first visit downloads the analysis engine (about 40 MB), so it can take a few minutes on a slow connection; later visits are much faster. Use Chrome, Edge or Firefox, or Option B. |
| Very slow or never loads | Privacy and ad-blocking extensions (e.g. Privacy Badger, uBlock Origin) can block the background script the app needs, in any browser. Turn the extension off for this site (Privacy Badger: click its icon → *Disable for this site*) and reload. In Firefox, also switch off Enhanced Tracking Protection for the site (shield icon left of the address bar) and don't use a private window. Or use Option B. |

---

## Methods and credits

MeltScope follows the analysis workflow of Thermo Fisher's HRM software
([High Resolution Melt Analysis Module user guide](https://assets.thermofisher.com/TFS-Assets/LSG/manuals/MAN0018981_HighResolutionMelt_DA_SW_UG.pdf);
[High Resolution Melt Software v3.0 user guide](https://documents.thermofisher.com/TFS-Assets/LSG/manuals/4457847.pdf)):
pre- and post-melt regions that scale each curve from 100% to 0%, a difference plot against a reference, a
derivative plot, and automatic variant calls with a confidence value. Temperature shifting and the normalisation
maths follow the published HRM methods:

- Wittwer CT, Reed GH, Gundry CN, Vandersteen JG, Pryor RJ (2003). High-resolution genotyping by amplicon melting
  analysis using LCGreen. *Clinical Chemistry* 49(6):853–860. [PubMed 12765979](https://pubmed.ncbi.nlm.nih.gov/12765979/)
- Palais R, Wittwer CT (2009). Mathematical algorithms for high-resolution DNA melting analysis. *Methods in
  Enzymology* 454:323–343. [PubMed 19216933](https://pubmed.ncbi.nlm.nih.gov/19216933/)

Much of MeltScope's workflow is inspired by Thermo Fisher's HRM software, but the algorithms behind it (smoothing,
clustering, confidence scores) are different. Results should be close to Thermo's, but not identical, so check your
calls against known controls.

Developed by Larysha Rothmann. Released under the [MIT licence](LICENSE).
