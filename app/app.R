# MeltScope: High Resolution Melt (HRM) analysis in the browser or in RStudio.
# Analysis functions live in R/hrm.R (loaded automatically by Shiny).

pkgs <- c("shiny", "bslib", "plotly", "DT", "readxl", "cluster", "jsonlite")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  stop("MeltScope needs some R packages that are not installed. Run this line in the R console, then try again:\n\n",
       "  install.packages(c(", paste0('"', missing_pkgs, '"', collapse = ", "), "))\n", call. = FALSE)
}
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(plotly)
  library(DT)
  library(readxl)   # used in R/hrm.R; listed here so Shinylive bundles it
  library(cluster)
})

EXAMPLE <- "example/example_plate_long.xlsx"          # synthetic plate, see tools/make_example_data.R
EXAMPLE_SAMPLES <- "example/example_sample_names.csv"
FIXED_COLS <- c(Unknown = "#3b3b3b", Negative = "#b5b5b5", Excluded = "#e3e3e3")

palette_for <- function(n, name) {
  if (n == 0) return(character())
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

# ---- UI ------------------------------------------------------------------------

ui <- page_sidebar(
  title = tags$span(tags$b("MeltScope"), tags$span(class = "text-muted ms-2 fs-6", "High Resolution Melt analysis")),
  theme = bs_theme(version = 5, primary = "#1f6f8b", "font-size-base" = "0.95rem"),
  fillable = FALSE,
  sidebar = sidebar(
    width = 340,
    accordion(
      open = c("1. Load data", "2. Melt regions"),
      accordion_panel(
        "1. Load data", icon = icon("file-arrow-up"),
        fileInput("file", NULL, accept = c(".xlsx", ".xls", ".csv", ".txt"), buttonLabel = "Upload…",
                  placeholder = "QuantStudio / CFX export"),
        actionButton("example", "Or load example data (synthetic plate)",
                     class = "btn-outline-primary btn-sm w-100 mb-3"),
        help_tip("Upload the raw instrument export as is (Excel or CSV): QuantStudio 'Melt Curve Raw', ",
                 "or any table with a Temperature column plus one column per well. Examples of both layouts: ",
                 downloadLink("ex_long", "long, Excel"), " · ", downloadLink("ex_long_csv", "long, CSV"), " · ",
                 downloadLink("ex_wide", "wide, CSV"), "."),
        selectInput("target", "Target / assay", choices = NULL),
        fileInput("samples", "Sample names (optional)", accept = c(".csv", ".xlsx"), buttonLabel = "Upload…"),
        help_tip("A table with a 'Well' and a 'Sample' column. ",
                 downloadLink("sample_template", "Download a template for this plate."))
      ),
      accordion_panel(
        "2. Melt regions", icon = icon("arrows-left-right-to-line"),
        sliderInput("pre", "Pre-melt region (°C)", min = 60, max = 95, value = c(70, 71), step = 0.1),
        sliderInput("post", "Post-melt region (°C)", min = 60, max = 95, value = c(88, 89), step = 0.1),
        actionButton("auto_win", "Auto-detect regions", icon = icon("wand-magic-sparkles"),
                     class = "btn-sm w-100 mb-2"),
        help_tip("Pick flat stretches just before and just after the melt (shaded on the Raw plot). ",
                 "Each curve is scaled from 100% (pre-melt) to 0% (post-melt).")
      ),
      accordion_panel(
        "3. Curve processing", icon = icon("sliders"),
        sliderInput("spar", "Smoothing", min = 0.2, max = 1, value = 0.4, step = 0.05),
        help_tip("Higher = smoother derivative, but heavy smoothing pulls Tm values. ",
                 "At 0.4, Tm agrees with the instrument's own values to about 0.01 °C."),
        checkboxInput("shift", "Temperature shift (align curves)", TRUE),
        conditionalPanel(
          "input.shift",
          sliderInput("shift_thr", "Align curves where they cross (% fluorescence)",
                      min = 1, max = 30, value = 5, step = 1)
        ),
        help_tip("Removes small Tm offsets caused by plate position (heating zones), salt or DNA amount, so ",
                 "curve shape stands out. Turn it off if your variants differ mainly by Tm ",
                 "(e.g. some homozygous SNPs).")
      ),
      accordion_panel(
        "4. Variant calling", icon = icon("layer-group"),
        radioButtons("features", "Group wells by",
                     c("Curve shape (difference curves)" = "shape", "Melting temperature (Tm) only" = "tm")),
        selectInput("k", "Number of variants", c("Auto" = "auto", 1:8)),
        conditionalPanel(
          "input.k == 'auto'",
          conditionalPanel(
            "input.features == 'shape'",
            numericInput("min_diff_shape", "Auto: minimum difference between variants (%)",
                         value = 2, min = 0, step = 0.5)
          ),
          conditionalPanel(
            "input.features == 'tm'",
            numericInput("min_diff_tm", "Auto: minimum Tm difference between variants (°C)",
                         value = 0.2, min = 0, step = 0.05)
          ),
          help_tip("Groups whose average curves differ by less than this are treated as one variant.")
        ),
        selectInput("method", "Clustering method", c("Hierarchical (Ward)" = "hierarchical", "K-means" = "kmeans")),
        sliderInput("min_conf", "Minimum confidence (%)", min = 0, max = 100, value = 80, step = 5),
        help_tip("Wells below this are called 'Unknown'."),
        numericInput("neg_pct", "Call 'Negative' if signal < (% of median)", value = 20, min = 0, max = 100, step = 5),
        textInput("variant_names", "Variant names (optional)", placeholder = "e.g. WT, Het, Hom"),
        help_tip("Comma-separated, in order Variant 1, 2, 3… (Variant 1 = largest group).")
      ),
      accordion_panel(
        "5. Difference plot reference", icon = icon("crosshairs"),
        selectInput("ref", "Subtract the average of", choices = NULL),
        conditionalPanel("input.ref == '__wells'",
                         selectizeInput("ref_wells", "Reference wells", NULL, multiple = TRUE))
      ),
      accordion_panel(
        "Plot appearance", icon = icon("palette"),
        selectInput("colour_by", "Colour curves by", c("Call" = "call", "Sample" = "sample", "Well" = "well")),
        selectInput("palette", "Colours",
                    c("Okabe-Ito (colour-blind safe)" = "Okabe-Ito", "Dark 3", "Set 2", "Viridis", "Zissou 1")),
        sliderInput("lw", "Line width", min = 0.5, max = 4, value = 1.5, step = 0.25),
        sliderInput("font", "Text size", min = 10, max = 22, value = 14, step = 1),
        radioButtons("img_fmt", "Camera button saves as",
                     c("PNG (high-res)" = "png", "SVG (editable)" = "svg"), inline = TRUE)
      ),
      accordion_panel(
        "Save / load settings", icon = icon("floppy-disk"),
        downloadButton("save_settings", "Save settings", class = "btn-sm w-100 mb-2"),
        fileInput("load_settings", NULL, accept = ".json", buttonLabel = "Load settings…"),
        help_tip("Saves every setting above plus excluded wells, so an analysis can be repeated exactly. ",
                 "Load data first.")
      )
    )
  ),
  uiOutput("status_boxes"),
  navset_card_underline(
    id = "tabs",
    nav_panel(
      "Plate & calls",
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Plate map", tags$small(class = "text-muted ms-2", "click a well to exclude / include it")),
             plotlyOutput("plate", height = "380px"),
             actionButton("clear_excl", "Include all wells again",
                          class = "btn-sm btn-outline-secondary", width = "220px")),
        card(card_header("Summary by call"), tableOutput("summary"))
      )
    ),
    nav_panel("Raw data", plotlyOutput("p_raw", height = "560px")),
    nav_panel("Normalised", plotlyOutput("p_norm", height = "560px")),
    nav_panel("Difference", plotlyOutput("p_diff", height = "560px")),
    nav_panel(
      "Derivative",
      radioButtons("deriv_type", NULL, inline = TRUE,
                   c("Raw: −dF/dT (as instrument)" = "raw", "Normalised: −d(%)/dT" = "norm")),
      plotlyOutput("p_deriv", height = "520px")
    ),
    nav_panel(
      "Results table",
      div(class = "mb-2",
          downloadButton("dl_results", "Download results (CSV)", class = "btn-sm"),
          downloadButton("dl_curves", "Download processed curves (CSV)", class = "btn-sm")),
      DTOutput("results")
    ),
    nav_panel(
      "Clustering check",
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Wells in PCA space"), plotlyOutput("p_pca", height = "420px")),
        card(card_header("How well do k groups separate?"), plotlyOutput("p_sil", height = "420px"),
             help_tip("Average silhouette: closer to 1 = cleaner separation. 'Auto' only considers splits with ",
                      "silhouette ≥ 0.5 whose groups differ by at least the minimum difference, then picks the ",
                      "smallest k within 0.05 of the best. Near-ties mean the number of variants is a judgement ",
                      "call: set it under 'Number of variants'."))
      )
    ),
    nav_panel("Help", icon = icon("circle-question"), includeHTML("help.html"))
  )
)

# ---- Server ----------------------------------------------------------------------

server <- function(input, output, session) {
  rv <- reactiveValues(long = NULL, name = NULL, excluded = character(), samples = NULL)

  load_data <- function(path, name) {
    long <- tryCatch(read_melt(path, name), error = function(e) {
      showNotification(paste("Could not read file:", conditionMessage(e)), type = "error", duration = NULL)
      NULL
    })
    req(long)
    rv$long <- long
    rv$name <- tools::file_path_sans_ext(basename(name))
    rv$excluded <- character()
    updateSelectInput(session, "target", choices = unique(long$target))
  }
  observeEvent(input$file, load_data(input$file$datapath, input$file$name))
  observeEvent(input$example, {
    load_data(EXAMPLE, basename(EXAMPLE))
    smp <- utils::read.csv(EXAMPLE_SAMPLES)
    rv$samples <- stats::setNames(smp$Sample, smp$Well)
  })

  mat <- reactive({
    validate(need(rv$long, "Upload a melt curve export, or click 'Load example data' in the sidebar."))
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
                    else utils::read.csv(f$datapath, check.names = FALSE), error = function(e) NULL)
    nm <- norm_name(names(tab))
    wc <- which(nm %in% c("well", "wellposition"))[1]
    sc <- which(nm %in% c("sample", "samplename", "name"))[1]
    if (is.null(tab) || is.na(wc) || is.na(sc)) {
      showNotification("Sample file needs a 'Well' column and a 'Sample' column.", type = "error")
      return()
    }
    ok <- !is.na(tab[[sc]]) & nzchar(as.character(tab[[sc]]))
    rv$samples <- stats::setNames(as.character(tab[[sc]])[ok], trimws(tab[[wc]])[ok])
  })

  example_download <- function(path) downloadHandler(function() basename(path), function(file) file.copy(path, file))
  output$ex_long <- example_download(EXAMPLE)
  output$ex_long_csv <- example_download("example/example_plate_long.csv")
  output$ex_wide <- example_download("example/example_plate_wide.csv")

  output$sample_template <- downloadHandler(
    filename = function() "sample_names_template.csv",
    content = function(file) {
      wells <- if (is.null(rv$long)) paste0(rep(LETTERS[1:8], each = 12), 1:12) else colnames(mat()$F)
      utils::write.csv(data.frame(Well = wells, Sample = ""), file, row.names = FALSE)
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
    tryCatch(analyse(mat(), p, rv$excluded), error = function(e) validate(conditionMessage(e)))
  })

  # Results table with user-facing names
  variant_names <- reactive(trimws(strsplit(input$variant_names %||% "", ",")[[1]]))

  calls <- reactive({
    t <- res()$table
    t$Variant <- rename_variants(t$Variant, variant_names())
    t$Call <- rename_variants(t$Call, variant_names())
    smp <- if (is.null(rv$samples)) NA else unname(rv$samples[t$Well])
    t$Sample <- ifelse(is.na(smp), "", smp)
    t[c("Well", "Sample", "Call", "Confidence", "Tm1", "Tm2", "Variant", "Amplitude", "Status")]
  })

  # Variant 1, 2, ... in order (Variant 1 = largest group), with user names applied
  variant_levels <- reactive(rename_variants(paste("Variant", seq_len(res()$k)), variant_names()))

  call_colours <- reactive({
    v <- variant_levels()
    c(stats::setNames(palette_for(length(v), input$palette), v), FIXED_COLS)
  })

  # Reference choices for the difference plot
  observe({
    v <- variant_levels()
    ch <- c(stats::setNames(v, paste0(v, if (length(v)) c(" (largest group)", rep("", length(v) - 1)))),
            "All analysed wells" = "__mean", "Choose wells…" = "__wells")
    sel <- isolate(input$ref)
    updateSelectInput(session, "ref", choices = ch, selected = if (!is.null(sel) && sel %in% ch) sel else ch[1])
  })
  observe({
    w <- calls()$Well[calls()$Status %in% c("Called", "Low confidence")]
    updateSelectizeInput(session, "ref_wells", choices = w, selected = intersect(isolate(input$ref_wells), w))
  })

  # ---- Exclusions via plate map
  plate_click <- reactive(suppressWarnings(event_data("plotly_click", source = "plate", priority = "event")))
  observeEvent(plate_click(), {
    w <- plate_click()$customdata
    req(length(w) == 1, !is.na(w), w %in% colnames(mat()$F))
    rv$excluded <- if (w %in% rv$excluded) setdiff(rv$excluded, w) else c(rv$excluded, w)
  })
  observeEvent(input$clear_excl, rv$excluded <- character())

  # ---- Plot helpers
  labels <- reactive({
    t <- calls()
    stats::setNames(ifelse(nzchar(t$Sample), paste0(t$Well, " · ", t$Sample), t$Well), t$Well)
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

  finish <- function(p, ylab, fname, xrange = NULL, shapes = list(), annotations = list()) {
    p |>
      layout(
        xaxis = list(title = "Temperature (°C)", range = xrange, zeroline = FALSE, showgrid = FALSE, ticks = "outside"),
        yaxis = list(title = ylab, zeroline = FALSE, gridcolor = "#eeeeee", ticks = "outside"),
        font = list(size = input$font), hovermode = "closest", shapes = shapes, annotations = annotations,
        legend = list(itemclick = "toggle", itemdoubleclick = "toggleothers"),
        margin = list(t = 30), plot_bgcolor = "white", paper_bgcolor = "white"
      ) |>
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
             toImageButtonOptions = list(format = input$img_fmt, filename = paste(rv$name, fname, sep = "_"),
                                         width = 1100, height = 650, scale = if (input$img_fmt == "png") 3 else 1))
  }

  # One trace per colour group (wells joined with NA gaps) keeps the legend short and plotting fast
  curve_plot <- function(x, M, wells, ylab, fname, xrange = NULL, shapes = list(), annotations = list()) {
    validate(need(length(wells), "No wells to show (all excluded or negative)."))
    cg <- colouring()
    grp <- cg$group[wells]
    lab <- labels()
    p <- plot_ly()
    for (g in unique(grp[order(match(grp, names(cg$cols)))])) {
      w <- wells[grp == g]
      p <- add_trace(
        p, type = "scatter", mode = "lines", name = g,
        x = rep(c(x, NA), length(w)), y = as.vector(rbind(M[, w, drop = FALSE], NA)),
        text = rep(lab[w], each = length(x) + 1),
        hovertemplate = "<b>%{text}</b><br>%{x:.2f} °C<br>%{y:.4g}<extra>%{fullData.name}</extra>",
        line = list(color = unname(cg$cols[g]), width = input$lw), connectgaps = FALSE
      )
    }
    finish(p, ylab, fname, xrange, shapes, annotations)
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
    if (is.null(rv$long)) {
      return(div(class = "alert alert-light border mb-3",
                 icon("circle-info"), " Start by uploading a file or loading the example data (sidebar, step 1). ",
                 "The Help tab walks through each step."))
    }
    t <- calls()
    layout_columns(
      fill = FALSE, height = "140px",
      value_box("Data", tags$span(class = "fs-6 text-break", rv$name),
                p(sprintf("%d wells · target %s", nrow(t), input$target)),
                showcase = icon("table"), theme = "light"),
      value_box("Variants found", res()$k, p(sprintf("%d wells called", sum(t$Status == "Called"))),
                showcase = icon("layer-group"), theme = "primary"),
      value_box("Need a look", sum(t$Status %in% c("Low confidence", "Negative")),
                p(sprintf("%d unknown · %d negative · %d excluded", sum(t$Status == "Low confidence"),
                          sum(t$Status == "Negative"), sum(t$Status == "Excluded"))),
                showcase = icon("triangle-exclamation"), theme = "light")
    )
  })

  output$plate <- renderPlotly({
    t <- calls()
    rows <- match(substr(t$Well, 1, 1), LETTERS)
    cols <- as.integer(substring(t$Well, 2))
    big <- max(rows, na.rm = TRUE) > 8 || max(cols, na.rm = TRUE) > 12
    nr <- if (big) 16 else 8
    nc <- if (big) 24 else 12
    grid <- expand.grid(r = seq_len(nr), c = seq_len(nc))
    cc <- call_colours()
    hover <- sprintf("<b>%s</b><br>%s%s<br>Tm %s °C", labels()[t$Well], t$Call,
                     ifelse(is.na(t$Confidence), "", sprintf(" (%.0f%%)", t$Confidence)), t$Tm1)
    sz <- if (big) 14 else 30
    p <- plot_ly(source = "plate") |>
      add_markers(x = grid$c, y = grid$r, hoverinfo = "skip", showlegend = FALSE,
                  marker = list(size = sz, color = "white", line = list(color = "#d0d0d0", width = 1)))
    for (g in intersect(names(cc), unique(t$Call))) {
      i <- which(t$Call == g)
      p <- add_markers(p, x = cols[i], y = rows[i], name = g, customdata = t$Well[i], text = hover[i],
                       hovertemplate = "%{text}<extra></extra>",
                       marker = list(size = sz, color = cc[[g]], symbol = if (g == "Excluded") "x" else "circle",
                                     line = list(color = "#555", width = 0.5)))
    }
    p |>
      layout(
        xaxis = list(tickvals = seq_len(nc), side = "top", range = c(0.4, nc + 0.6),
                     showgrid = FALSE, zeroline = FALSE, title = ""),
        yaxis = list(tickvals = seq_len(nr), ticktext = LETTERS[seq_len(nr)], autorange = "reversed",
                     showgrid = FALSE, zeroline = FALSE, title = ""),
        legend = list(orientation = "h", y = -0.05), margin = list(t = 30, l = 30), font = list(size = input$font - 1)
      ) |>
      config(displaylogo = FALSE, displayModeBar = FALSE) |>
      event_register("plotly_click")
  })

  output$summary <- renderTable({
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
  }, striped = TRUE, width = "100%")

  output$p_raw <- renderPlotly({
    r <- res()
    wells <- setdiff(colnames(r$F), rv$excluded)
    curve_plot(r$temp, r$F, wells, "Fluorescence", "raw", shapes = region_shapes(), annotations = region_labels())
  })

  output$p_norm <- renderPlotly({
    r <- res()
    i <- in_region()
    curve_plot(r$temp[i], r$N[i, , drop = FALSE], analysed(), "Normalised fluorescence (%)", "normalised")
  })

  ref_curve <- reactive({
    r <- res()
    t <- calls()
    req(input$ref)
    w <- switch(input$ref,
                "__mean" = analysed(),
                "__wells" = input$ref_wells,
                t$Well[t$Call == input$ref])
    if (!length(w)) w <- analysed()
    rowMeans(r$N[, w, drop = FALSE])
  })

  output$p_diff <- renderPlotly({
    req(input$ref)
    r <- res()
    i <- in_region()
    lab <- if (input$ref == "__wells") "selected wells" else if (input$ref == "__mean") "all wells" else input$ref
    curve_plot(r$temp[i], (r$N - ref_curve())[i, , drop = FALSE], analysed(),
               paste0("Difference from ", lab, " (%)"), "difference",
               shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0,
                                  line = list(color = "#999", width = 1, dash = "dot"))))
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

  output$p_deriv <- renderPlotly({
    r <- res()
    if (input$deriv_type == "raw") {
      curve_plot(r$temp, r$D, setdiff(colnames(r$F), rv$excluded), "−dF/dT", "derivative",
                 shapes = region_shapes(), annotations = region_labels())
    } else {
      i <- in_region()
      curve_plot(r$temp[i], norm_deriv()[i, , drop = FALSE], analysed(), "−d(normalised %)/dT",
                 "derivative_normalised")
    }
  })

  output$results <- renderDT({
    t <- calls()
    datatable(t, rownames = FALSE, filter = "top", extensions = character(),
              options = list(pageLength = 384, dom = "ti")) |>
      formatStyle("Call", backgroundColor = styleEqual(names(call_colours()), paste0(call_colours(), "55")))
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

  output$p_pca <- renderPlotly({
    r <- res()
    s <- r$scores
    validate(need(!is.null(s), "No wells were clustered."))
    t <- calls()[match(rownames(s), calls()$Well), ]
    x <- s[, 1]
    y <- if (ncol(s) > 1) s[, 2] else seq_along(x)
    cc <- call_colours()
    p <- plot_ly()
    for (g in intersect(names(cc), unique(t$Call))) {
      i <- t$Call == g
      p <- add_markers(p, x = x[i], y = y[i], name = g, text = labels()[t$Well[i]],
                       hovertemplate = "%{text}<extra></extra>",
                       marker = list(size = 11, color = cc[[g]], line = list(color = "#333", width = 0.5)))
    }
    lab <- if (input$features == "tm") c("Tm (°C)", "well (index)") else c("PC1", "PC2")
    p |> layout(xaxis = list(title = lab[1], zeroline = FALSE), yaxis = list(title = lab[2], zeroline = FALSE),
                font = list(size = input$font - 1)) |>
      config(displaylogo = FALSE)
  })

  output$p_sil <- renderPlotly({
    s <- res()$silhouette
    validate(need(!is.null(s), "Too few wells to compare group numbers."))
    plot_ly(s, x = ~k, y = ~silhouette, type = "scatter", mode = "lines+markers",
            line = list(color = "#1f6f8b"), marker = list(size = 9, color = "#1f6f8b"),
            hovertemplate = "k = %{x}<br>silhouette %{y:.2f}<extra></extra>") |>
      layout(xaxis = list(title = "Number of variants (k)", dtick = 1),
             yaxis = list(title = "Average silhouette", range = c(0, 1)),
             shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0.5, y1 = 0.5,
                                line = list(dash = "dot", color = "#999"))),
             font = list(size = input$font - 1)) |>
      config(displaylogo = FALSE)
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
    showNotification("Settings loaded.", type = "message")
  })
}

shinyApp(ui, server)
