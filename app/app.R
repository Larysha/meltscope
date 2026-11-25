# MeltScope: High Resolution Melt (HRM) analysis in the browser or in RStudio.
# Analysis functions live in R/hrm.R (loaded automatically by Shiny).

pkgs <- c("shiny", "bslib", "readxl", "cluster", "jsonlite")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  stop("MeltScope needs some R packages that are not installed. Run this line in the R console, then try again:\n\n",
       "  install.packages(c(", paste0('"', missing_pkgs, '"', collapse = ", "), "))\n", call. = FALSE)
}
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(readxl)   # used in R/hrm.R; listed here so Shinylive bundles it
  library(cluster)
})

EXAMPLE <- "example/example_plate_long.xlsx"          # synthetic plate, see tools/make_example_data.R
EXAMPLE_SAMPLES <- "example/example_sample_names.csv"
FIXED_COLS <- c(Unknown = "#3b3b3b", Negative = "#b5b5b5", Excluded = "#e3e3e3")

# Hand-picked palettes (no pale colours, which vanish on white); stretched if more groups than colours
CUSTOM_PALETTES <- list(
  "Orchid" = c("#6f3a9e", "#2aa9a4", "#d94f8f", "#3b2768", "#e889b6", "#1b6d76", "#b56fd6", "#87d0c8")
)

palette_for <- function(n, name) {
  if (n == 0) return(character())
  if (name %in% names(CUSTOM_PALETTES)) return(grDevices::colorRampPalette(CUSTOM_PALETTES[[name]])(max(n, 8))[seq_len(n)])
  if (name == "Okabe-Ito" && n <= 7) {
    okabe <- grDevices::palette.colors(9, "Okabe-Ito")[c(6, 7, 4, 2, 3, 8, 5)]  # no black/grey (used for Unknown etc.)
    return(unname(okabe[seq_len(n)]))
  }
  grDevices::hcl.colors(n, if (name == "Okabe-Ito") "Dark 3" else name)
}

# "Variant 2" -> user's 2nd name, if given
rename_variants <- function(x, nm) {
  new <- nm[suppressWarnings(as.integer(sub("^Variant ", "", x)))]
  ifelse(!is.na(new) & nzchar(new), new, x)
}

help_tip <- function(...) tags$small(class = "text-muted d-block mb-3", ...)

# Summary card above the tabs; cards wrap onto new rows on narrow screens
# Clicking one sets input$stat_click to its id; the server opens a pop-up with the details
stat_card <- function(id, title, value, detail, icon, small = FALSE) {
  tags$button(type = "button", class = "ms-stat",
      onclick = sprintf("Shiny.setInputValue('stat_click', '%s', {priority: 'event'})", id),
      tags$img(src = file.path("icons", icon), class = "ms-stat-icon", alt = ""),
      div(class = "ms-stat-body",
          div(class = "ms-stat-title", title),
          div(class = paste("ms-stat-value", if (small) "small"), value),
          div(class = "ms-stat-detail", detail)))
}

# Plain HTML table; click a column header to sort (www/meltscope.js). tint colours the Call column.
html_table <- function(t, tint = NULL) {
  rows <- lapply(seq_len(nrow(t)), function(i) {
    tags$tr(lapply(names(t), function(col) {
      v <- t[[col]][i]
      tags$td(style = if (col == "Call" && !is.null(tint)) paste0("background:", tint[i]),
              if (is.na(v)) "" else as.character(v))
    }))
  })
  tags$table(class = "table table-sm table-hover ms-sortable",
             tags$thead(tags$tr(lapply(names(t), tags$th))), tags$tbody(rows))
}

# Side by side on wide screens, stacked on laptops/tablets
two_columns <- function(...) layout_columns(col_widths = breakpoints(sm = 12, xl = c(6, 6)), ...)

# Figures are drawn in the browser by Plotly.js (www/), filled in by the server via render_fig()
plot_div <- function(id, height) div(id = id, class = "ms-plot", style = paste0("height:", height, ";"))

page_head <- tags$head(
  tags$script(src = "plotly-basic-2.35.2.min.js"),
  tags$script(src = "meltscope.js"),
  tags$style(HTML("
    .ms-plot-msg { padding: 2rem 1rem; color: #6c757d; }
    table.ms-sortable th { cursor: pointer; position: sticky; top: 0; background: #fff; white-space: nowrap; }
    table.ms-sortable th[data-sort='asc']::after { content: ' \\25B2'; }
    table.ms-sortable th[data-sort='desc']::after { content: ' \\25BC'; }
    #summary td { white-space: nowrap; }
    .ms-stats { display: grid; grid-template-columns: repeat(auto-fit, minmax(190px, 1fr)); gap: .75rem;
                margin-bottom: 1rem; }
    .ms-stat { border: 1px solid transparent; border-radius: .5rem; padding: .6rem .9rem; min-width: 0;
               background: rgba(42,169,164,.14); color: #123f4c;
               display: flex; align-items: center; gap: .75rem; width: 100%; text-align: left;
               font: inherit; cursor: pointer; transition: box-shadow .15s, transform .15s; }
    .ms-stat:hover, .ms-stat:focus-visible { box-shadow: 0 .25rem .75rem rgba(0,0,0,.12); transform: translateY(-1px); }
    .ms-stat-icon { width: 40px; height: 40px; flex-shrink: 0; }
    .ms-stat-body { min-width: 0; }
    .ms-stat-title { font-size: .78rem; text-transform: uppercase; letter-spacing: .04em; opacity: .75; }
    .ms-stat-value { font-size: 1.6rem; font-weight: 600; line-height: 1.25; overflow-wrap: anywhere; }
    .ms-stat-value.small { font-size: 1rem; padding: .3rem 0; }
    .ms-stat-detail { font-size: .85rem; opacity: .85; }
    .ms-top { display: flex; flex-wrap: wrap; align-items: center; gap: .4rem; padding: .75rem 0; }
    .ms-top .dropdown-menu { max-height: 80vh; overflow-y: auto; }
    .ms-plate { display: grid; gap: 3px; touch-action: none; user-select: none; -webkit-user-select: none; }
    .ms-ph { font-size: .75rem; color: #666; text-align: center; align-self: center; cursor: pointer; }
    .ms-well { aspect-ratio: 1; border-radius: 50%; border: 1px solid #555; cursor: pointer; position: relative; }
    .ms-well.empty { background: #fff; border-color: #ddd; cursor: default; }
    .ms-well { container-type: inline-size; }  /* lets the marks below scale with the well */
    .ms-well.ctrl { border: 2px solid #111; }
    .ms-well.ctrl::before, .ms-well.excl::after { position: absolute; inset: 0; display: flex; align-items: center;
                                                 justify-content: center; line-height: 1; }
    .ms-well.ctrl::before { content: '\\25B2'; font-size: 11px; font-size: 48cqi; color: #fff;
                            text-shadow: 0 0 2px #000, 0 0 1px #000; }
    .ms-well.excl::after { content: '\\00D7'; font-size: 14px; font-size: 70cqi; color: #666; font-weight: 700; }
    .ms-well.excl.ctrl::before { content: none; }
    .ms-ctrl-key { color: #fff; text-shadow: 0 0 2px #000, 0 0 1px #000; margin-right: .3rem; font-size: .8rem; }
    .ms-landing { max-width: 34rem; margin: 12vh auto 0; padding: 2.5rem; text-align: center;
                  border: 1px solid #dee2e6; border-radius: .75rem; }
    .ms-landing h2 { font-size: 1.5rem; font-weight: 600; }
    .ms-landing .form-group { margin: 1.5rem 0 .75rem; }
    .ms-inline-input .form-group { margin-bottom: 0; }
    .ms-bar { display: flex; flex-wrap: wrap; align-items: center; gap: .25rem 1.25rem; min-height: 2.4rem;
              margin-bottom: .5rem; }
    .ms-bar .form-group, .ms-bar .shiny-input-container { margin-bottom: 0; }
    .ms-bar .selectize-control { margin-bottom: 0; }
    .ms-well.sel { outline: 3px solid #f4a300; outline-offset: 1px; }
    .ms-legend { display: flex; flex-wrap: wrap; gap: .25rem .9rem; font-size: .85rem; margin-top: .5rem; }
    .ms-swatch { display: inline-block; width: .8rem; height: .8rem; border-radius: 50%; margin-right: .3rem;
                 vertical-align: -1px; border: 1px solid #555; }
  "))
)

# ---- UI ------------------------------------------------------------------------

# Settings live in dropdown menus along the top (Bootstrap dropdowns, so their inputs exist from the start),
# leaving the full width for the plate map and the plot side by side. They close after a file is loaded.
menu <- function(label, icon_name, ..., width = 340) {
  div(class = "dropdown",
      tags$button(class = "btn btn-outline-primary btn-sm dropdown-toggle", type = "button",
                  `data-bs-toggle` = "dropdown", `data-bs-auto-close` = "outside", `aria-expanded` = "false",
                  icon(icon_name), label),
      div(class = "dropdown-menu p-3 shadow", style = sprintf("width: %dpx;", width), ...))
}

MELT_FILES <- c(".xlsx", ".xls", ".csv", ".txt")

# Before any data: just a file picker. Menus, plate and plots appear once a file is loaded.
landing <- div(class = "ms-landing",
  tags$h2("Analyse a High Resolution Melt plate"),
  tags$p(class = "text-muted", "Load the melt curve export from your qPCR machine, unchanged (Excel or CSV)."),
  fileInput("file", NULL, accept = MELT_FILES, buttonLabel = "Choose file", width = "100%"),
  actionLink("example", "or try an example plate"),
  tags$p(class = "small text-muted mt-4 mb-0", "Your data stays on your computer.")
)

ui <- page_fluid(
  theme = bs_theme(version = 5, primary = "#1f6f8b", "font-size-base" = "0.95rem"),
  page_head,
  div(class = "ms-top",
      tags$span(class = "me-2", tags$b(class = "fs-5", "MeltScope"),
                tags$span(class = "text-muted ms-2", "High Resolution Melt analysis")),
      conditionalPanel(
        "output.has_data", div(class = "d-flex flex-wrap gap-1",  # d-flex on the panel itself would defeat hiding
        menu(
          "Data", "table",
          fileInput("file2", "Melt curve file", accept = MELT_FILES),
          selectInput("target", "Target", choices = NULL),
          fileInput("samples", "Sample names (optional)", accept = c(".csv", ".xlsx")),
          help_tip("Well and Sample columns, plus an optional Control column. Thermo's Results.csv works as is. ",
                   downloadLink("sample_template", "Template")),
          help_tip("Example files: ", downloadLink("ex_long", "long .xlsx"), " · ",
                   downloadLink("ex_long_csv", "long .csv"), " · ", downloadLink("ex_wide", "wide .csv"))
        ),
        menu(
          "Melt regions", "arrows-left-right-to-line",
          sliderInput("pre", "Pre-melt (°C)", min = 60, max = 95, value = c(70, 71), step = 0.1),
          sliderInput("post", "Post-melt (°C)", min = 60, max = 95, value = c(88, 89), step = 0.1),
          actionButton("auto_win", "Auto-detect", icon = icon("wand-magic-sparkles"), class = "btn-sm w-100 mb-2"),
          help_tip("Flat stretches just before and after the melt (shaded on the Raw plot).")
        ),
        menu(
          "Processing", "sliders",
          sliderInput("spar", "Smoothing", min = 0.2, max = 1, value = 0.4, step = 0.05),
          checkboxInput("shift", "Temperature shift", TRUE),
          conditionalPanel("input.shift",
                           sliderInput("shift_thr", "Align at (% fluorescence)", min = 1, max = 30, value = 5, step = 1)),
          help_tip("Shifting removes small Tm offsets so shape differences stand out. ",
                   "Turn it off if variants differ mainly by Tm.")
        ),
        menu(
          "Calling", "layer-group",
          radioButtons("features", "Group wells by", c("Curve shape" = "shape", "Tm only" = "tm")),
          selectInput("k", "Number of variants", c("Auto" = "auto", 1:8)),
          conditionalPanel(
            "input.k == 'auto'",
            conditionalPanel("input.features == 'shape'",
                             numericInput("min_diff_shape", "Auto: min. difference between variants (%)",
                                          value = 2, min = 0, step = 0.5)),
            conditionalPanel("input.features == 'tm'",
                             numericInput("min_diff_tm", "Auto: min. Tm difference between variants (°C)",
                                          value = 0.2, min = 0, step = 0.05))
          ),
          selectInput("method", "Clustering", c("Hierarchical (Ward)" = "hierarchical", "K-means" = "kmeans")),
          sliderInput("min_conf", "Minimum confidence (%)", min = 0, max = 100, value = 80, step = 5),
          numericInput("neg_pct", "Negative below (% of median signal)", value = 20, min = 0, max = 100, step = 5),
          textInput("variant_names", "Variant names", placeholder = "e.g. WT, Het, Hom")
        ),
        menu(
          "Appearance", "palette",
          selectInput("colour_by", "Colour curves by", c("Call" = "call", "Sample" = "sample", "Well" = "well")),
          selectInput("palette", "Colours",
                      c("Okabe-Ito (colour-blind safe)" = "Okabe-Ito", "Orchid", "Dark 3", "Set 2", "Viridis",
                        "Zissou 1")),
          sliderInput("lw", "Line width", min = 0.5, max = 4, value = 1.5, step = 0.25),
          sliderInput("font", "Text size", min = 10, max = 22, value = 14, step = 1),
          radioButtons("img_fmt", "Camera saves", c("PNG" = "png", "SVG (editable)" = "svg"), inline = TRUE)
        ),
        menu(
          "Save", "floppy-disk",
          downloadButton("save_settings", "Save settings", class = "btn-sm w-100 mb-2"),
          fileInput("load_settings", "Load settings", accept = ".json"),
          help_tip("Includes excluded and control wells.")
        ))
      )
  ),
  conditionalPanel("!output.has_data", landing),
  conditionalPanel(
    "output.has_data",
    uiOutput("status_boxes"),
    navset_card_underline(
      id = "tabs",
      nav_panel(
        "Plate & curves",
        layout_columns(
          col_widths = breakpoints(sm = 12, lg = c(5, 7)),
          div(
            div(class = "ms-bar",
                tags$b("Plate map"), tags$small(class = "text-muted", "drag to select · Shift / ⌘ adds · Esc clears"),
                textOutput("sel_info", inline = TRUE)),
            uiOutput("plate"),
            div(class = "d-flex flex-wrap align-items-center gap-1 mt-3",
                actionButton("sel_exclude", "Exclude", icon = icon("ban"), class = "btn-sm btn-outline-secondary"),
                actionButton("sel_include", "Include", icon = icon("check"), class = "btn-sm btn-outline-secondary"),
                tags$button(type = "button", class = "btn btn-sm btn-outline-secondary me-3",
                            onclick = "msClearSelection()", "Clear"),
                div(style = "width: 8rem;", class = "ms-inline-input",
                    textInput("control_name", NULL, placeholder = "Control name")),
                actionButton("sel_control", "Mark control", class = "btn-sm btn-outline-secondary"),
                actionButton("sel_uncontrol", "Unmark", class = "btn-sm btn-outline-secondary")),
            checkboxInput("hl_controls", "Show controls in the plot with the selection", TRUE, width = "100%"),
            div(class = "mt-3", style = "overflow-x: auto;", tableOutput("summary"))
          ),
          div(
            # One row, same height as the plate's title row, so plate and plot start level
            div(class = "ms-bar",
                radioButtons("view", NULL, inline = TRUE, selected = "diff",
                             c("Raw" = "raw", "Normalised" = "norm", "Difference" = "diff", "Derivative" = "deriv")),
                conditionalPanel("input.view == 'diff'",
                                 div(class = "d-flex align-items-center gap-2",
                                     tags$label(`for` = "ref", class = "text-nowrap", "Zero line:"),
                                     div(style = "width: 20rem;", selectInput("ref", NULL, choices = NULL)))),
                conditionalPanel("input.view == 'deriv'",
                                 radioButtons("deriv_type", NULL, inline = TRUE,
                                              c("Raw −dF/dT (as instrument)" = "raw", "Normalised" = "norm")))),
            conditionalPanel("input.view == 'diff' && input.ref == '__wells'",
                             selectizeInput("ref_wells", NULL, NULL, multiple = TRUE, width = "100%",
                                            options = list(placeholder = "Pick the wells for the zero line"))),
            plot_div("p_curve", "560px")
          )
        )
      ),
      nav_panel(
        "Results table",
        div(class = "mb-2",
            downloadButton("dl_results", "Results (CSV)", class = "btn-sm"),
            downloadButton("dl_curves", "Processed curves (CSV)", class = "btn-sm")),
        div(style = "max-height: 640px; overflow-y: auto;", uiOutput("results"))
      ),
      nav_panel(
        "Clustering check",
        two_columns(
          card(card_header("Wells in PCA space"), plot_div("p_pca", "420px")),
          card(card_header("How well do k groups separate?"), plot_div("p_sil", "420px"),
               help_tip("Closer to 1 = cleaner separation. Auto picks the fewest groups with silhouette ≥ 0.5 that ",
                        "differ by at least the minimum difference."))
        )
      ),
      nav_panel("Help", icon = icon("circle-question"), includeHTML("help.html"))
    )
  )
)

# ---- Server ----------------------------------------------------------------------

server <- function(input, output, session) {
  rv <- reactiveValues(long = NULL, name = NULL, excluded = character(), controls = character(), samples = NULL)

  load_data <- function(path, name) {
    long <- tryCatch(read_melt(path, name), error = function(e) {
      showNotification(paste("Could not read file:", conditionMessage(e)), type = "error", duration = NULL)
      NULL
    })
    req(long)
    rv$long <- long
    rv$name <- tools::file_path_sans_ext(basename(name))
    rv$excluded <- character()
    rv$controls <- character()
    session$sendCustomMessage("ms-clear-selection", TRUE)
    session$sendCustomMessage("ms-close-menus", TRUE)
    updateSelectInput(session, "target", choices = unique(long$target))
  }
  observeEvent(input$file, load_data(input$file$datapath, input$file$name))
  observeEvent(input$file2, load_data(input$file2$datapath, input$file2$name))
  output$has_data <- reactive(!is.null(rv$long))  # shows menus / plate instead of the landing page
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)
  observeEvent(input$example, {
    load_data(EXAMPLE, basename(EXAMPLE))
    smp <- utils::read.csv(EXAMPLE_SAMPLES)
    rv$samples <- stats::setNames(smp$Sample, smp$Well)
  })

  mat <- reactive({
    validate(need(rv$long, "No data loaded."))
    req(input$target %in% rv$long$target)
    to_matrix(rv$long[rv$long$target == input$target, ])
  })

  set_windows <- function() {
    m <- mat()
    w <- auto_windows(m$temp, smooth_curves(m$temp, m$F, input$spar)$D)
    rng <- round(range(m$temp), 1)
    updateSliderInput(session, "pre", min = rng[1], max = rng[2], value = w$pre)
    updateSliderInput(session, "post", min = rng[1], max = rng[2], value = w$post)
  }
  observeEvent(mat(), set_windows())
  observeEvent(input$auto_win, set_windows())

  observeEvent(input$samples, {
    f <- input$samples
    tab <- tryCatch(if (grepl("xlsx?$", f$name, ignore.case = TRUE)) readxl::read_excel(f$datapath)
                    else utils::read.csv(f$datapath, check.names = FALSE, comment.char = "#"),  # Thermo "# ..." header
                    error = function(e) NULL)
    nm <- norm_name(names(tab))
    wc <- which(nm %in% c("well", "wellposition"))[1]
    sc <- which(nm %in% c("sample", "samplename", "name"))[1]
    if (is.null(tab) || is.na(wc) || is.na(sc)) {
      showNotification("Sample file needs a 'Well' column and a 'Sample' column.", type = "error")
      return()
    }
    ok <- !is.na(tab[[sc]]) & nzchar(as.character(tab[[sc]]))
    rv$samples <- stats::setNames(as.character(tab[[sc]])[ok], trimws(tab[[wc]])[ok])
    cc <- which(nm %in% c("control", "taskcontrol", "task"))[1]
    if (!is.na(cc)) {
      v <- trimws(as.character(tab[[cc]]))
      ok <- !is.na(v) & !tolower(v) %in% c("", "unknown", "ntc", "no", "false", "na")
      rv$controls <- stats::setNames(v[ok], trimws(tab[[wc]])[ok])
    }
    session$sendCustomMessage("ms-close-menus", TRUE)
  })

  example_download <- function(path) downloadHandler(function() basename(path), function(file) file.copy(path, file))
  output$ex_long <- example_download(EXAMPLE)
  output$ex_long_csv <- example_download("example/example_plate_long.csv")
  output$ex_wide <- example_download("example/example_plate_wide.csv")

  output$sample_template <- downloadHandler(
    filename = function() "sample_names_template.csv",
    content = function(file) {
      wells <- if (is.null(rv$long)) paste0(rep(LETTERS[1:8], each = 12), 1:12) else colnames(mat()$F)
      utils::write.csv(data.frame(Well = wells, Sample = "", Control = ""), file, row.names = FALSE)
    }
  )

  params <- debounce(reactive(list(
    spar = input$spar, pre = input$pre, post = input$post, shift = input$shift, shift_thr = input$shift_thr,
    neg_pct = if (is.na(input$neg_pct)) 0 else input$neg_pct, features = input$features,
    method = input$method, k = input$k, min_conf = input$min_conf,
    min_diff_shape = input$min_diff_shape %||% 2, min_diff_tm = input$min_diff_tm %||% 0.2
  )), 300)

  res <- reactive({
    p <- params()
    validate(need(p$pre[2] < p$post[1], "The pre-melt region must end before the post-melt region starts."))
    tryCatch(analyse(mat(), p, rv$excluded, rv$controls), error = function(e) validate(conditionMessage(e)))
  })

  # Results table with user-facing names
  variant_names <- reactive(trimws(strsplit(input$variant_names %||% "", ",")[[1]]))

  calls <- reactive({
    t <- res()$table
    t$Variant <- rename_variants(t$Variant, variant_names())
    t$Call <- rename_variants(t$Call, variant_names())
    smp <- if (is.null(rv$samples)) NA else unname(rv$samples[t$Well])
    t$Sample <- ifelse(is.na(smp), "", smp)
    ctrl <- unname(rv$controls[t$Well])
    t$Control <- ifelse(is.na(ctrl), "", ctrl)
    t[c("Well", "Sample", "Control", "Call", "Confidence", "Tm1", "Tm2", "Variant", "Amplitude", "Status")]
  })

  # Variant 1, 2, ... in order (Variant 1 = largest group), with user names applied
  variant_levels <- reactive(rename_variants(res()$levels, variant_names()))

  call_colours <- reactive({
    v <- variant_levels()
    c(stats::setNames(palette_for(length(v), input$palette), v), FIXED_COLS)
  })

  # Reference choices for the difference plot, under headings so it is clear what gets averaged:
  # the wells marked as controls, or every well called into a group. Switches to the controls when first marked.
  had_controls <- FALSE
  observe({
    v <- variant_levels()
    t <- calls()
    cn <- unique(unname(rv$controls))
    ctrl <- length(cn) > 0
    n_ctrl <- vapply(cn, function(x) sum(names(rv$controls)[rv$controls == x] %in% analysed()), 0)
    n_grp <- vapply(v, function(x) sum(t$Call == x), 0)
    ch <- list(
      "Marked control wells" = if (ctrl) stats::setNames(paste0("__ctrl:", cn), sprintf("%s (%d wells)", cn, n_ctrl)),
      "Called groups" = stats::setNames(v, sprintf("%s group (%d wells%s)", v, n_grp,
                                                   ifelse(seq_along(v) == 1 & !ctrl, ", largest", ""))),
      "Other" = c(stats::setNames("__mean", sprintf("All analysed wells (%d)", length(analysed()))),
                  "Wells I choose…" = "__wells"))
    ch <- Filter(length, ch)
    vals <- unlist(ch, use.names = FALSE)
    sel <- isolate(input$ref)
    if (ctrl && !had_controls) sel <- vals[1]
    had_controls <<- ctrl
    updateSelectInput(session, "ref", choices = ch, selected = if (!is.null(sel) && sel %in% vals) sel else vals[1])
  })
  observe({
    w <- calls()$Well[calls()$Status %in% c("Called", "Low confidence")]
    updateSelectizeInput(session, "ref_wells", choices = w, selected = intersect(isolate(input$ref_wells), w))
  })

  # ---- Plate selection (kept in the browser, www/meltscope.js; sent here when a click / drag ends)
  selected <- reactive(intersect(as.character(unlist(input$plate_sel)), colnames(mat()$F)))
  output$sel_info <- renderText({
    s <- selected()
    if (length(s) > 8) s <- c(s[1:8], "…")
    if (!length(s)) "" else sprintf("· %d selected: %s", length(selected()), paste(s, collapse = " "))
  })
  observeEvent(input$sel_exclude, rv$excluded <- union(rv$excluded, selected()))
  observeEvent(input$sel_include, rv$excluded <- setdiff(rv$excluded, selected()))
  observeEvent(input$sel_control, {
    name <- trimws(input$control_name %||% "")
    ctrl <- rv$controls
    ctrl[selected()] <- if (nzchar(name)) name else "Control"
    rv$controls <- ctrl
  })
  observeEvent(input$sel_uncontrol, rv$controls <- rv$controls[!names(rv$controls) %in% selected()])

  # Wells drawn in colour when some are selected (the rest are greyed out)
  highlighted <- reactive({
    s <- selected()
    if (length(s) && isTRUE(input$hl_controls)) union(s, names(rv$controls)) else s
  })

  # ---- Plot helpers
  labels <- reactive({
    t <- calls()
    stats::setNames(ifelse(nzchar(t$Sample), paste0(t$Well, " · ", t$Sample), t$Well), t$Well)
  })

  # Hover text for a well (plate map and curves): name, call, confidence, Tm, control
  well_info <- reactive({
    t <- calls()
    stats::setNames(paste0(
      "<b>", labels()[t$Well], "</b><br>", t$Call,
      ifelse(is.na(t$Confidence), "", sprintf(" (%.0f%% confidence)", t$Confidence)),
      ifelse(is.na(t$Tm1), "", paste0("<br>Tm1 ", t$Tm1, " °C")), ifelse(is.na(t$Tm2), "", paste0(" · Tm2 ", t$Tm2, " °C")),
      ifelse(nzchar(t$Control), paste0("<br>Control: ", t$Control), "")), t$Well)
  })

  # Named vector well -> group used for colouring, plus colours per group
  colouring <- reactive({
    t <- calls()
    grp <- switch(input$colour_by,
                  call = t$Call,
                  sample = ifelse(nzchar(t$Sample), t$Sample, t$Well),
                  well = t$Well)
    grp <- stats::setNames(grp, t$Well)
    if (input$colour_by == "call") {
      cols <- call_colours()
    } else {
      lv <- unique(grp)
      cols <- stats::setNames(palette_for(length(lv), input$palette), lv)
    }
    list(group = grp, cols = cols)
  })

  # Draw figure `id` in the browser (www/meltscope.js) whenever its inputs change, but only while its
  # tab is open. Using Plotly.js directly (not the plotly R package) keeps the in-browser download small.
  render_fig <- function(id, tab, expr) {
    fig_expr <- substitute(expr)
    env <- parent.frame()
    observe({
      req(input$tabs == tab)
      fig <- tryCatch(eval(fig_expr, env),
                      shiny.silent.error = function(e) list(message = conditionMessage(e)),
                      error = function(e) list(message = paste("Could not draw this plot:", conditionMessage(e))))
      session$sendCustomMessage("ms-plot", c(list(id = id), fig))
    })
  }

  # Assemble a Plotly.js figure; `fname` enables the camera (download) button with that file name
  fig <- function(traces, layout, fname = NULL) {
    base <- list(font = list(size = input$font), hovermode = "closest", margin = list(t = 30),
                 plot_bgcolor = "white", paper_bgcolor = "white")
    config <- list(displaylogo = FALSE, modeBarButtonsToRemove = I(c("lasso2d", "select2d", "autoScale2d")))
    if (is.null(fname)) {
      config$displayModeBar <- FALSE
    } else {
      config$toImageButtonOptions <- list(format = input$img_fmt, filename = paste(rv$name, fname, sep = "_"),
                                          width = 1100, height = 650, scale = if (input$img_fmt == "png") 3 else 1)
    }
    list(data = unname(traces), layout = utils::modifyList(base, layout), config = config)
  }

  axis_title <- function(x) list(text = x)

  # One trace per colour group (wells joined with gaps) keeps the legend short and plotting fast.
  # With wells selected on the plate, only those (plus controls) are coloured; the rest are drawn grey underneath.
  curve_plot <- function(x, M, wells, ylab, fname, shapes = list(), annotations = list()) {
    validate(need(length(wells), "No wells to show (all excluded or negative)."))
    cg <- colouring()
    lab <- well_info()
    line_trace <- function(w, name, colour, width) {
      list(type = "scatter", mode = "lines", name = name,
           x = I(rep(c(round(x, 3), NA), length(w))),
           y = I(signif(as.vector(rbind(M[, w, drop = FALSE], NA)), 6)),
           text = I(rep(unname(lab[w]), each = length(x) + 1)),
           hovertemplate = "%{text}<br>%{x:.2f} °C · %{y:.4g}<extra></extra>",
           line = list(color = colour, width = width), connectgaps = FALSE)
    }
    hl <- intersect(highlighted(), wells)
    grey <- if (length(hl) && length(setdiff(wells, hl)))
      list(line_trace(setdiff(wells, hl), "Other wells", "#d6d6d6", 0.8 * input$lw))
    if (length(hl)) wells <- hl
    grp <- cg$group[wells]
    groups <- unique(grp[order(match(grp, names(cg$cols)))])
    traces <- c(grey, lapply(groups, function(g) line_trace(wells[grp == g], g, unname(cg$cols[g]), input$lw)))
    fig(traces, list(
      xaxis = list(title = axis_title("Temperature (°C)"), zeroline = FALSE, showgrid = FALSE, ticks = "outside"),
      yaxis = list(title = axis_title(ylab), zeroline = FALSE, gridcolor = "#eeeeee", ticks = "outside"),
      shapes = shapes, annotations = annotations,
      legend = list(itemclick = "toggle", itemdoubleclick = "toggleothers")
    ), fname)
  }

  region_shapes <- function() {
    band <- function(r, col) {
      list(type = "rect", xref = "x", yref = "paper", x0 = r[1], x1 = r[2], y0 = 0, y1 = 1,
           fillcolor = col, opacity = 0.18, line = list(width = 0), layer = "below")
    }
    list(band(input$pre, "#1f6f8b"), band(input$post, "#d1495b"))
  }
  region_labels <- function() {
    lab <- function(r, txt) {
      list(x = mean(r), y = 1, xref = "x", yref = "paper", text = txt, showarrow = FALSE,
           yanchor = "bottom", font = list(size = input$font - 2, color = "#555"))
    }
    list(lab(input$pre, "pre-melt"), lab(input$post, "post-melt"))
  }

  analysed <- reactive(calls()$Well[calls()$Status %in% c("Called", "Low confidence")])
  in_region <- reactive(res()$temp >= input$pre[1] & res()$temp <= input$post[2])

  # ---- Outputs
  output$status_boxes <- renderUI({
    req(rv$long)
    t <- calls()
    div(class = "ms-stats",
        stat_card("data", "Data", rv$name, sprintf("%d wells · target %s", nrow(t), input$target), "data.svg",
                  small = TRUE),
        stat_card("variants", "Variants found", res()$k, {
          ctrl <- t[nzchar(t$Control) & !is.na(t$Variant), ]
          split <- names(which(tapply(ctrl$Variant, ctrl$Control, function(v) length(unique(v))) > 1))
          paste0(sprintf("%d wells called", sum(t$Status == "Called")),
                 if (length(split)) sprintf(" · %s controls split across groups!", paste(split, collapse = ", "))
                 else if (nrow(ctrl)) sprintf(" · %d controls", nrow(ctrl)))
        }, "variants.svg"),
        stat_card("look", "Need a look", sum(t$Status %in% c("Low confidence", "Negative")),
                  sprintf("%d unknown · %d negative · %d excluded", sum(t$Status == "Low confidence"),
                          sum(t$Status == "Negative"), sum(t$Status == "Excluded")), "look.svg"))
  })

  # Plate map: a plain HTML grid (not a plot), so clicks and drags work the same in every browser.
  # Every cell carries data-r / data-c (0 = header: whole row / column); selection is handled in www/meltscope.js.
  output$plate <- renderUI({
    t <- calls()
    rows <- match(substr(t$Well, 1, 1), LETTERS)
    cols <- as.integer(substring(t$Well, 2))
    big <- max(rows, na.rm = TRUE) > 8 || max(cols, na.rm = TRUE) > 12
    nr <- if (big) 16 else 8
    nc <- if (big) 24 else 12
    cc <- call_colours()
    tip <- gsub("<[^>]+>", "", gsub("<br>", "\n", well_info()))  # plain-text tooltip
    cell <- function(r, c) {
      i <- which(rows == r & cols == c)
      if (!length(i)) return(div(class = "ms-well empty", `data-r` = r, `data-c` = c))
      w <- t$Well[i]
      div(class = paste("ms-well", if (t$Status[i] == "Excluded") "excl", if (nzchar(t$Control[i])) "ctrl"),
          `data-r` = r, `data-c` = c, `data-well` = w, title = tip[[w]], style = paste0("background:", cc[[t$Call[i]]]))
    }
    tagList(
      div(class = "ms-plate", style = sprintf("grid-template-columns: 1.2em repeat(%d, 1fr);", nc),
          div(class = "ms-ph", `data-r` = 0, `data-c` = 0, title = "Select all", HTML("&#9698;")),
          lapply(seq_len(nc), function(c) div(class = "ms-ph", `data-r` = 0, `data-c` = c, c)),
          lapply(seq_len(nr), function(r) tagList(
            div(class = "ms-ph", `data-r` = r, `data-c` = 0, LETTERS[r]),
            lapply(seq_len(nc), function(c) cell(r, c))))),
      div(class = "ms-legend",
          lapply(intersect(names(cc), unique(t$Call)), function(g)
            tags$span(tags$span(class = "ms-swatch", style = paste0("background:", cc[[g]])), g)),
          if (any(nzchar(t$Control))) tags$span(tags$span(class = "ms-ctrl-key", HTML("&#9650;")), "Control"))
    )
  })

  summary_by_call <- reactive({
    t <- calls()
    t <- t[!t$Status %in% c("Excluded"), ]
    groups <- split(t, factor(t$Call, levels = intersect(names(call_colours()), t$Call)))
    do.call(rbind, lapply(groups, function(d) {
      tm <- d$Tm1[!is.na(d$Tm1)]
      data.frame(
        Call = d$Call[1], Wells = nrow(d),
        `Tm (°C)` = if (length(tm) > 1) sprintf("%.2f ± %.2f", mean(tm), stats::sd(tm)) else sprintf("%.2f", mean(tm)),
        `Confidence (%)` = if (all(is.na(d$Confidence))) "–" else sprintf("%.0f", mean(d$Confidence, na.rm = TRUE)),
        check.names = FALSE
      )
    }))
  })
  output$summary <- renderTable(summary_by_call(), striped = TRUE, width = "100%")

  # Pop-ups behind the summary cards
  observeEvent(input$stat_click, {
    t <- calls()
    tint <- function(t) paste0(call_colours()[t$Call], "55")
    scroll <- function(x) div(style = "max-height: 60vh; overflow: auto;", x)
    dlg <- switch(input$stat_click,
      data = {
        m <- mat()
        d <- data.frame(Temperature = m$temp, m$F, check.names = FALSE)
        d[] <- lapply(d, round, 2)
        modalDialog(title = paste("Data:", rv$name), size = "xl", easyClose = TRUE,
                    p(sprintf("Target %s · %d wells · %d readings from %.1f to %.1f °C. First 20 readings:",
                              input$target, ncol(m$F), length(m$temp), min(m$temp), max(m$temp))),
                    scroll(html_table(utils::head(d, 20))))
      },
      variants = modalDialog(title = sprintf("%d variants found", res()$k), size = "l", easyClose = TRUE,
                             html_table(summary_by_call()),
                             footer = tagList(actionButton("go_results", "Open full results table"),
                                              modalButton("Close"))),
      look = {
        look <- c("Low confidence", "Negative", "Excluded")
        t <- t[t$Status %in% look, ]
        t <- t[order(match(t$Status, look), t$Confidence), ]
        modalDialog(title = sprintf("%d wells need a look", nrow(t)), size = "l", easyClose = TRUE,
                    if (nrow(t)) tagList(
                      help_tip("Unknown = no confident call; Negative = little or no melt signal; ",
                               "Excluded = removed by you. Select wells on the plate map, then Exclude or Include."),
                      scroll(html_table(t, tint(t))))
                    else p("Nothing to check: every well got a confident call and none are excluded."))
      })
    showModal(dlg)
  })
  observeEvent(input$go_results, {
    removeModal()
    nav_select("tabs", "Results table")
  })

  ref_curve <- reactive({
    r <- res()
    t <- calls()
    req(input$ref)
    w <- switch(input$ref,
                "__mean" = analysed(),
                "__wells" = input$ref_wells,
                if (startsWith(input$ref, "__ctrl:"))
                  intersect(names(rv$controls)[rv$controls == sub("^__ctrl:", "", input$ref)], analysed())
                else t$Well[t$Call == input$ref])
    if (!length(w)) w <- analysed()
    rowMeans(r$N[, w, drop = FALSE])
  })

  norm_deriv <- reactive({
    r <- res()
    h <- diff(r$temp[1:2])
    n <- nrow(r$N)
    D <- r$N
    D[2:(n - 1), ] <- -(r$N[3:n, ] - r$N[1:(n - 2), ]) / (2 * h)
    D[c(1, n), ] <- NA
    D
  })

  # One plot next to the plate; the buttons above it pick which curves it shows
  render_fig("p_curve", "Plate & curves", {
    r <- res()
    i <- in_region()
    kept <- setdiff(colnames(r$F), rv$excluded)
    switch(input$view,
      raw = curve_plot(r$temp, r$F, kept, "Fluorescence", "raw", shapes = region_shapes(),
                       annotations = region_labels()),
      norm = curve_plot(r$temp[i], r$N[i, , drop = FALSE], analysed(), "Normalised fluorescence (%)", "normalised"),
      diff = {
        req(input$ref)
        lab <- switch(input$ref, "__wells" = "chosen wells", "__mean" = "all wells",
                      if (startsWith(input$ref, "__ctrl:")) {
                        nm <- sub("^__ctrl:", "", input$ref)
                        if (tolower(nm) == "control") "control wells" else paste(nm, "control wells")
                      }
                      else paste(input$ref, "group"))
        curve_plot(r$temp[i], (r$N - ref_curve())[i, , drop = FALSE], analysed(),
                   paste0("Difference from ", lab, " (%)"), "difference",
                   shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0,
                                      line = list(color = "#999", width = 1, dash = "dot"))))
      },
      deriv = if (input$deriv_type == "raw") {
        curve_plot(r$temp, r$D, kept, "−dF/dT", "derivative", shapes = region_shapes(), annotations = region_labels())
      } else {
        curve_plot(r$temp[i], norm_deriv()[i, , drop = FALSE], analysed(), "−d(normalised %)/dT",
                   "derivative_normalised")
      })
  })

  output$results <- renderUI({
    t <- calls()
    html_table(t, paste0(call_colours()[t$Call], "55"))
  })

  output$dl_results <- downloadHandler(
    filename = function() paste0(rv$name, "_MeltScope_results.csv"),
    content = function(file) utils::write.csv(calls(), file, row.names = FALSE)
  )

  output$dl_curves <- downloadHandler(
    filename = function() paste0(rv$name, "_MeltScope_curves.csv"),
    content = function(file) {
      r <- res()
      w <- colnames(r$F)
      d <- data.frame(
        Well = rep(w, each = length(r$temp)), Temperature = rep(r$temp, length(w)),
        Raw = as.vector(r$F), Smoothed = as.vector(r$S), Derivative = as.vector(r$D),
        Normalised = as.vector(r$N), Difference = as.vector(r$N - ref_curve())
      )
      d$Call <- calls()$Call[match(d$Well, calls()$Well)]
      inside <- d$Temperature >= input$pre[1] & d$Temperature <= input$post[2]
      d$Normalised[!inside] <- NA
      d$Difference[!inside] <- NA
      utils::write.csv(d, file, row.names = FALSE)
    }
  )

  render_fig("p_pca", "Clustering check", {
    r <- res()
    s <- r$scores
    validate(need(!is.null(s), "No wells were clustered."))
    t <- calls()[match(rownames(s), calls()$Well), ]
    x <- unname(s[, 1])  # named vectors would be sent as JSON objects, not arrays
    y <- if (ncol(s) > 1) unname(s[, 2]) else seq_along(x)
    cc <- call_colours()
    traces <- lapply(intersect(names(cc), unique(t$Call)), function(g) {
      i <- t$Call == g
      list(type = "scatter", mode = "markers", name = g, x = I(x[i]), y = I(y[i]),
           text = I(unname(labels()[t$Well[i]])), hovertemplate = "%{text}<extra></extra>",
           marker = list(size = 11, color = cc[[g]], line = list(color = "#333", width = 0.5)))
    })
    lab <- if (input$features == "tm") c("Tm (°C)", "well (index)") else c("PC1", "PC2")
    fig(traces, list(xaxis = list(title = axis_title(lab[1]), zeroline = FALSE),
                     yaxis = list(title = axis_title(lab[2]), zeroline = FALSE),
                     font = list(size = input$font - 1)), "clustering_pca")
  })

  render_fig("p_sil", "Clustering check", {
    s <- res()$silhouette
    validate(need(!is.null(s), "Too few wells to compare group numbers."))
    trace <- list(type = "scatter", mode = "lines+markers", x = I(s$k), y = I(round(s$silhouette, 3)),
                  line = list(color = "#1f6f8b"), marker = list(size = 9, color = "#1f6f8b"),
                  hovertemplate = "k = %{x}<br>silhouette %{y:.2f}<extra></extra>")
    fig(list(trace), list(
      xaxis = list(title = axis_title("Number of variants (k)"), dtick = 1),
      yaxis = list(title = axis_title("Average silhouette"), range = I(c(0, 1))),
      shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0.5, y1 = 0.5,
                         line = list(dash = "dot", color = "#999"))),
      font = list(size = input$font - 1)
    ), "clustering_silhouette")
  })

  # ---- Settings save / load
  sliders <- c("pre", "post", "spar", "shift_thr", "min_conf", "lw", "font")
  radios <- c("features", "img_fmt", "deriv_type")
  selects <- c("k", "method", "colour_by", "palette", "ref")
  numerics <- c("neg_pct", "min_diff_shape", "min_diff_tm")
  setting_ids <- c(sliders, radios, selects, numerics, "shift", "variant_names", "ref_wells")

  output$save_settings <- downloadHandler(
    filename = function() paste0(if (is.null(rv$name)) "MeltScope" else rv$name, "_settings.json"),
    content = function(file) {
      s <- lapply(stats::setNames(setting_ids, setting_ids), function(id) input[[id]])
      s$excluded_wells <- rv$excluded
      s$control_wells <- as.list(rv$controls)  # {"G1": "Ch WT", ...}
      s$data_file <- rv$name
      s$target <- input$target
      jsonlite::write_json(s, file, auto_unbox = TRUE, pretty = TRUE, null = "null")
    }
  )

  observeEvent(input$load_settings, {
    s <- tryCatch(jsonlite::read_json(input$load_settings$datapath, simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(s)) return(showNotification("Could not read that settings file.", type = "error"))
    for (id in intersect(sliders, names(s))) updateSliderInput(session, id, value = s[[id]])
    for (id in intersect(radios, names(s))) updateRadioButtons(session, id, selected = s[[id]])
    for (id in intersect(selects, names(s))) updateSelectInput(session, id, selected = s[[id]])
    for (id in intersect(numerics, names(s))) updateNumericInput(session, id, value = s[[id]])
    if (!is.null(s$shift)) updateCheckboxInput(session, "shift", value = s$shift)
    if (!is.null(s$variant_names)) updateTextInput(session, "variant_names", value = s$variant_names)
    if (!is.null(s$ref_wells)) updateSelectizeInput(session, "ref_wells", selected = s$ref_wells)
    rv$excluded <- as.character(unlist(s$excluded_wells))
    ctrl <- unlist(s$control_wells)
    rv$controls <- if (is.null(names(ctrl))) stats::setNames(rep("Control", length(ctrl)), ctrl) else ctrl
    session$sendCustomMessage("ms-close-menus", TRUE)
    showNotification("Settings loaded.", type = "message")
  })
}

shinyApp(ui, server)
