# Shiny App V3 Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Evolve the $DESIGN Explorer Shiny app from V2 (single-run post-processing) to V3 (multi-run comparison, built-in examples, enriched plots, shrinkage/priors display).

**Architecture:** 4 incremental updates, each producing a working app. Update 1 adds quick wins (RSE rounding, shrinkages, priors). Update 2 restructures for multi-run comparison. Update 3 adds pedagogical warfarin examples. Update 4 adds enriched plots from PopED/PFIM.

**Tech Stack:** R, Shiny, bslib, DT, ggplot2, dplyr, tidyr, stringr, purrr, readr

**Spec:** `docs/superpowers/specs/2026-03-15-shiny-app-v3-design.md`

---

## Chunk 1: Update 1 — Quick Wins + Shrinkages + Priors

### Task 1: RSE to 2 decimal places

**Files:**
- Modify: `scripts/report_design.R` — `plot_rse()` lines 229, `plot_se()` line 525
- Modify: `app/R/mod_params.R` — line 116-117
- Modify: `app/R/helpers_ui.R` — `rse_badge()` line 17

- [ ] **Step 1: Update `rse_badge()` in helpers_ui.R**

Change `sprintf("%.1f%%", x)` to `sprintf("%.2f%%", x)` in the `rse_badge` function, and same for `ri_badge`.

```r
# In helpers_ui.R, rse_badge function (line 17):
rse_badge <- function(x) {
  if (is.na(x)) return(span("\u2014", class = "ri-na"))
  cls <- if (x < 20) "rse-good" else if (x < 50) "rse-moderate" else "rse-poor"
  span(sprintf("%.2f%%", x), class = cls)
}

# ri_badge function (line 22):
ri_badge <- function(x) {
  if (is.na(x)) return(span("\u2014", class = "ri-na"))
  cls <- if (x >= 50) "ri-good" else if (x >= 20) "ri-moderate" else "ri-poor"
  span(sprintf("%.2f%%", x), class = cls)
}
```

- [ ] **Step 2: Update param_table RSE/RelInf rounding in mod_params.R**

Change `round(rse_pct, 1)` to `round(rse_pct, 2)` and `round(relativeinf_pct, 1)` to `round(relativeinf_pct, 2)` in the `transmute` block (lines 116-117).

```r
# In mod_params.R, transmute block:
          `RSE (%)`       = round(rse_pct, 2),
          `RelInf (%)`    = if_else(!is.na(relativeinf_pct), round(relativeinf_pct, 2), NA_real_)
```

- [ ] **Step 3: Update plot_rse() text labels in report_design.R**

Change `sprintf("%.1f%%", rse_pct)` to `sprintf("%.2f%%", rse_pct)` in `geom_text` of `plot_rse()` (line 229).

```r
# In report_design.R, plot_rse(), geom_text:
    geom_text(
      aes(label = sprintf("%.2f%%", rse_pct)),
      vjust = -0.35, size = 2.9, color = "grey25"
    ) +
```

- [ ] **Step 4: Update plot_relativeinf() text labels**

Change `sprintf("%.1f%%", relativeinf_pct)` to `sprintf("%.2f%%", relativeinf_pct)` in `plot_relativeinf()` (line 141).

```r
# In report_design.R, plot_relativeinf(), geom_text:
    geom_text(
      aes(label = sprintf("%.2f%%", relativeinf_pct)),
      hjust = -0.12, size = 3.2, color = "grey25"
    ) +
```

- [ ] **Step 5: Update plot_se() text labels**

Change `sprintf("%.4g", se)` to `sprintf("%.4f", se)` in `plot_se()` (line 525) for consistency.

- [ ] **Step 6: Test the app**

Write a temp file `app/_test_run.R` with `shiny::runApp("app/", launch.browser=FALSE, port=7891)` then run: `cd c:/Users/abdou/Desktop/ClaudeProjets && "/c/Program Files/R/R-4.5.2/bin/Rscript" app/_test_run.R`

Load example1 warfarin.ext — verify RSE table shows 2 decimal places. Stop the app.

- [ ] **Step 7: Commit**

```bash
git add scripts/report_design.R app/R/mod_params.R app/R/helpers_ui.R
git commit -m "feat(app): display RSE/SE values with 2 decimal places"
```

---

### Task 2: Shrinkage display in Parametres tab

**Files:**
- Modify: `app/R/mod_params.R` — add shrinkage section
- Modify: `scripts/parse_design_outputs.R` — add `get_shrinkage()` function

- [ ] **Step 1: Add `get_shrinkage()` to parse_design_outputs.R**

Add after `get_relativeinf()` (after line 238). This extracts EBV shrinkage (TYPE 4) from `.shk`.

```r
#' Extraire les shrinkages EBV (TYPE 4) depuis read_shk()
#'
#' @param shk      Tibble retourne par read_shk()
#' @param table_no Numero de table (defaut : dernier)
#' @return Tibble : eta, shrinkage_pct
#' @export
get_shrinkage <- function(shk, table_no = NULL) {
  tbl <- table_no %||% max(shk$table_no)
  shk |>
    filter(.data$table_no == tbl, type_id == 4L) |>
    select(-c(table_no, type_id, subpop)) |>
    pivot_longer(everything(), names_to = "eta", values_to = "shrinkage_pct")
}
```

- [ ] **Step 2: Add shrinkage UI section in mod_params_ui**

Add a conditional panel after the param_table in `mod_params_ui()`. Insert after line 14 (before the closing `)`):

```r
mod_params_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("cards")),
    br(),
    div(
      class = "param-table-wrap",
      p(class = "section-title", "Table des parametres du design"),
      DTOutput(ns("param_table"))
    ),
    br(),
    div(
      class = "plot-card",
      p(class = "section-title", "Shrinkage EBV par ETA"),
      uiOutput(ns("shrinkage_content"))
    )
  )
}
```

- [ ] **Step 3: Add shrinkage server logic in mod_params_server**

Add after `ri_r` reactive (after line 30), and add the shrinkage output render inside the server function:

```r
    # Shrinkage EBV (TYPE 4)
    shrk_r <- reactive({
      shk <- shk_data()
      if (is.null(shk)) return(tibble(eta = character(), shrinkage_pct = numeric()))
      get_shrinkage(shk, tbl_no())
    })

    # Shrinkage content
    output$shrinkage_content <- renderUI({
      shrk <- shrk_r()
      ns <- session$ns
      if (nrow(shrk) == 0L) {
        return(div(class = "alert alert-info",
                   "Fichier .shk requis pour afficher les shrinkages."))
      }
      tagList(
        plotOutput(ns("shrinkage_plot"), height = "320px"),
        br(),
        DTOutput(ns("shrinkage_table"))
      )
    })

    output$shrinkage_plot <- renderPlot({
      shrk <- shrk_r(); req(nrow(shrk) > 0)
      lbls <- param_labels()
      if (!is.null(lbls)) {
        eta_labels <- setNames(lbls, paste0("ETA", seq_along(lbls)))
        shrk <- shrk |>
          mutate(eta = if_else(eta %in% names(eta_labels), eta_labels[eta], eta))
      }
      shrk <- shrk |>
        mutate(
          quality = factor(
            if_else(shrinkage_pct > 30, "> 30% (elevee)", "<= 30% (acceptable)"),
            levels = c("<= 30% (acceptable)", "> 30% (elevee)")
          )
        )
      ggplot(shrk, aes(x = reorder(eta, shrinkage_pct), y = shrinkage_pct, fill = quality)) +
        geom_col(width = 0.65, color = "white", linewidth = 0.3) +
        geom_hline(yintercept = 30, linetype = "dashed", color = "grey40", linewidth = 0.45) +
        geom_text(aes(label = sprintf("%.2f%%", shrinkage_pct)),
                  hjust = -0.12, size = 3.2, color = "grey25") +
        scale_fill_manual(
          values = c("<= 30% (acceptable)" = "#4CAF50", "> 30% (elevee)" = "#F44336"),
          name = NULL, drop = FALSE
        ) +
        coord_flip() +
        labs(title = "Shrinkage EBV (%) par ETA", x = NULL, y = "Shrinkage (%)") +
        theme_bw(base_size = 11) +
        theme(legend.position = "bottom", panel.grid.minor = element_blank(),
              panel.grid.major.y = element_blank())
    }, res = 110)

    output$shrinkage_table <- renderDT({
      shk <- shk_data(); req(shk)
      tbl <- tbl_no()
      # Show all shrinkage types for this table
      shk_tbl <- shk |>
        filter(.data$table_no == tbl) |>
        mutate(type_label = case_when(
          type_id == 4L  ~ "EBV Shrinkage SD (%)",
          type_id == 5L  ~ "EBV Shrinkage VR (%)",
          type_id == 8L  ~ "EPS Shrinkage SD (%)",
          type_id == 11L ~ "RELATIVEINF (%)",
          TRUE           ~ paste("Type", type_id)
        )) |>
        select(-c(table_no, subpop)) |>
        select(type_label, type_id, everything()) |>
        mutate(across(where(is.double), ~ round(.x, 2)))

      datatable(shk_tbl, rownames = FALSE, class = "stripe hover compact",
                options = list(pageLength = 15, dom = "tip", scrollX = TRUE))
    })
```

- [ ] **Step 4: Test the app**

Write a temp file `app/_test_run.R` with `shiny::runApp("app/", launch.browser=FALSE, port=7891)` then run: `cd c:/Users/abdou/Desktop/ClaudeProjets && "/c/Program Files/R/R-4.5.2/bin/Rscript" app/_test_run.R`

Load example1 warfarin files (.ext + .shk) — verify shrinkage plot and table appear in Parametres tab. Stop the app.

- [ ] **Step 5: Commit**

```bash
git add scripts/parse_design_outputs.R app/R/mod_params.R
git commit -m "feat(app): add shrinkage EBV display in Parametres tab"
```

---

### Task 3: Prior parsing from .ctl file

**Files:**
- Modify: `scripts/parse_design_outputs.R` — add `read_prior_nwpri()` function
- Modify: `app/R/mod_upload.R` — add .ctl upload field
- Modify: `app/R/mod_params.R` — add priors display section
- Modify: `app/app.R` — pass ctl_data to mod_params

- [ ] **Step 1: Add `read_prior_nwpri()` to parse_design_outputs.R**

Add at the end of the file. This parses `$PRIOR NWPRI`, `$THETAP`, `$THETAPV`, and `$OMEGAPD` blocks.

```r
#' Parser le bloc $PRIOR NWPRI depuis un fichier .ctl NONMEM
#'
#' Extrait les informations prior : THETAP (moyennes), THETAPV (variances),
#' OMEGAPD (degres de liberte). Inspire de summary.f90 (Bauer 2021 example 3).
#'
#' @param file Chemin vers le fichier .ctl
#' @return Liste avec composantes :
#'   - has_prior : logical, TRUE si $PRIOR NWPRI detecte
#'   - plev : numeric, niveau de probabilite (PLEV=)
#'   - thetap : numeric vector, moyennes des priors THETA
#'   - thetapv : matrix, variance-covariance des priors THETA
#'   - omega_df : numeric, degres de liberte OMEGA prior (si OMEGAPD present)
#'   - raw_prior_line : character, la ligne $PRIOR brute
#' @export
read_prior_nwpri <- function(file) {
  if (!file.exists(file)) stop("Fichier introuvable : ", file)

  lines <- readr::read_lines(file, progress = FALSE)
  # Remove comments (everything after ;)
  lines_clean <- str_replace(lines, ";.*$", "")

  result <- list(
    has_prior = FALSE, plev = NA_real_,
    thetap = numeric(), thetapv = NULL,
    omega_df = NA_real_, raw_prior_line = ""
  )

  # Find $PRIOR NWPRI
  prior_idx <- which(str_detect(lines_clean, "^\\s*\\$PRIOR\\s+NWPRI"))
  if (length(prior_idx) == 0L) return(result)

  result$has_prior <- TRUE
  prior_line <- lines_clean[prior_idx[1]]
  result$raw_prior_line <- trimws(lines[prior_idx[1]])

  # Extract PLEV
  plev_match <- str_extract(prior_line, "PLEV\\s*=\\s*[0-9.]+")
  if (!is.na(plev_match)) {
    result$plev <- as.numeric(str_extract(plev_match, "[0-9.]+$"))
  }

  # Find $THETAP block
  thetap_idx <- which(str_detect(lines_clean, "^\\s*\\$THETAP\\b"))
  if (length(thetap_idx) > 0) {
    tp_line <- lines_clean[thetap_idx[1]]
    # Extract values in parentheses: (value) or (value FIXED)
    vals <- str_extract_all(tp_line, "\\(\\s*(-?[0-9.eEdD]+)")[[1]]
    vals <- as.numeric(str_extract(vals, "-?[0-9.eEdD]+"))
    result$thetap <- vals
  }

  # Find $THETAPV BLOCK
  thetapv_idx <- which(str_detect(lines_clean, "^\\s*\\$THETAPV\\b"))
  if (length(thetapv_idx) > 0) {
    # Read block dimension
    dim_match <- str_extract(lines_clean[thetapv_idx[1]], "BLOCK\\s*\\(\\s*(\\d+)\\s*\\)")
    if (!is.na(dim_match)) {
      n <- as.integer(str_extract(dim_match, "\\d+"))
      # Read lower triangular values from subsequent lines
      all_vals <- numeric()
      i <- thetapv_idx[1] + 1
      expected <- n * (n + 1) / 2
      while (length(all_vals) < expected && i <= length(lines_clean)) {
        if (str_detect(lines_clean[i], "^\\s*\\$")) break
        nums <- str_extract_all(lines_clean[i], "-?[0-9.eEdD]+(?:[eEdD][+-]?\\d+)?")[[1]]
        nums <- nums[!nums %in% c("FIX", "FIXED")]
        all_vals <- c(all_vals, as.numeric(nums))
        i <- i + 1
      }
      # Build symmetric matrix
      mat <- matrix(0, n, n)
      idx <- 1
      for (row in seq_len(n)) {
        for (col in seq_len(row)) {
          if (idx <= length(all_vals)) {
            mat[row, col] <- all_vals[idx]
            mat[col, row] <- all_vals[idx]
            idx <- idx + 1
          }
        }
      }
      result$thetapv <- mat
    }
  }

  # Find $OMEGAPD (degrees of freedom for Omega prior)
  omegapd_idx <- which(str_detect(lines_clean, "^\\s*\\$OMEGAPD\\b"))
  if (length(omegapd_idx) > 0) {
    df_vals <- str_extract_all(lines_clean[omegapd_idx[1]], "[0-9.]+")[[1]]
    if (length(df_vals) > 0) result$omega_df <- as.numeric(df_vals[1])
  }

  result
}
```

- [ ] **Step 2: Add .ctl upload to mod_upload.R**

In `mod_upload_ui`, add `.ctl` to the accepted file types (line 13):

```r
                accept   = c(".ext", ".shk", ".coi", ".clt", ".tab",
                             ".ctl", ".tar.gz", ".tgz", ".gz"),
```

In `mod_upload_server`, add ctl detection in the file_paths reactiveVal (line 27) and in both tar.gz and multi-file branches:

```r
    # Line 27: add ctl to initial list
    file_paths <- reactiveVal(list(
      ext = NULL, shk = NULL, coi = NULL, clt = NULL, tab = NULL, ctl = NULL
    ))
```

In the tar.gz branch (after line 52, add):
```r
        ctl_i <- which(grepl("\\.ctl$", all_names, ignore.case = TRUE))[1]
```
After line 58, add:
```r
        if (!is.na(ctl_i)) paths$ctl <- all_files[ctl_i]
```

In the multi-file branch (after line 69, add):
```r
          if (grepl("\\.ctl$", nm, ignore.case = TRUE)) paths$ctl <- dp
```

In `file_status` output (after line 139, add):
```r
        status_line(".ctl", p$ctl),
```

Add ctl_data reactive (after tab_data reactive, line 120):
```r
    ctl_data <- reactive({
      p <- file_paths()$ctl
      if (is.null(p)) return(NULL)
      tryCatch(read_prior_nwpri(p), error = function(e) {
        showNotification(paste("Erreur .ctl :", e$message), type = "error"); NULL
      })
    })
```

Add to return list (line 150):
```r
      ctl_data   = ctl_data,
```

- [ ] **Step 3: Pass ctl_data to mod_params in app.R**

In `app.R`, add `ctl_data` to `mod_params_server` call (after line 150):

```r
  mod_params_server("params",
    ext_data     = upload$ext_data,
    shk_data     = upload$shk_data,
    ext_lines    = upload$ext_lines,
    tbl_no       = tbl_no,
    param_labels = param_labels_r,
    ctl_data     = upload$ctl_data
  )
```

- [ ] **Step 4: Add priors display in mod_params.R**

Update `mod_params_server` signature to accept `ctl_data`:

```r
mod_params_server <- function(id, ext_data, shk_data, ext_lines, tbl_no, param_labels, ctl_data = reactive(NULL)) {
```

Add a priors section in the UI (add to `mod_params_ui` after the shrinkage div):

```r
    br(),
    div(
      class = "param-table-wrap",
      p(class = "section-title", "Priors ($PRIOR NWPRI)"),
      uiOutput(ns("prior_content"))
    )
```

Add prior rendering in the server:

```r
    # Prior display
    output$prior_content <- renderUI({
      prior <- ctl_data()
      if (is.null(prior) || !prior$has_prior) {
        return(div(class = "alert alert-info",
                   "Uploadez un fichier .ctl contenant $PRIOR NWPRI pour afficher les priors."))
      }
      ns <- session$ns
      tagList(
        fluidRow(
          column(4, metric_card("Type", "NWPRI", prior$raw_prior_line, "blue")),
          column(4, metric_card("PLEV", if (!is.na(prior$plev)) prior$plev else "N/A",
                                "Niveau d'acceptation", "purple")),
          column(4, metric_card("THETAP", length(prior$thetap),
                                "parametres avec prior", "green"))
        ),
        br(),
        DTOutput(ns("prior_thetap_table")),
        if (!is.null(prior$thetapv)) {
          tagList(
            br(),
            p(class = "section-title", "Matrice variance-covariance des priors (THETAPV)"),
            DTOutput(ns("prior_thetapv_table"))
          )
        }
      )
    })

    output$prior_thetap_table <- renderDT({
      prior <- ctl_data(); req(prior, prior$has_prior, length(prior$thetap) > 0)
      lbls <- param_labels()
      tp <- tibble(
        Parametre = paste0("THETA", seq_along(prior$thetap)),
        `Prior (THETAP)` = prior$thetap,
        `Prior SD` = if (!is.null(prior$thetapv)) sqrt(diag(prior$thetapv)) else NA_real_
      )
      if (!is.null(lbls)) {
        tp <- tp |> mutate(Label = if_else(Parametre %in% names(lbls), lbls[Parametre], ""))
      }
      datatable(tp, rownames = FALSE, class = "stripe hover compact",
                options = list(pageLength = 15, dom = "t"))
    })

    output$prior_thetapv_table <- renderDT({
      prior <- ctl_data(); req(prior, !is.null(prior$thetapv))
      mat <- prior$thetapv
      n <- nrow(mat)
      rnames <- paste0("THETA", seq_len(n))
      df <- as.data.frame(mat)
      names(df) <- rnames
      df <- cbind(data.frame(Parametre = rnames), df)
      df[-1] <- round(df[-1], 6)
      datatable(df, rownames = FALSE, class = "stripe hover compact",
                options = list(pageLength = 15, dom = "t", scrollX = TRUE))
    })
```

- [ ] **Step 5: Test the app with example3 priortrue.ctl**

Run the app and upload `docs/bauer2021_examples/example3/priortrue.ext`, `priortrue.shk`, and `priortrue.ctl`. Verify:
- Prior section shows NWPRI, PLEV=0.99, 3 THETAP values
- THETAPV matrix displays correctly (3x3 diagonal 0.09)

- [ ] **Step 6: Commit**

```bash
git add scripts/parse_design_outputs.R app/R/mod_upload.R app/R/mod_params.R app/app.R
git commit -m "feat(app): parse and display $PRIOR NWPRI from .ctl files"
```

---

## Chunk 2: Update 2 — Multi-Run Comparison

### Task 4: Create mod_compare.R module

**Files:**
- Create: `app/R/mod_compare.R`
- Modify: `app/R/helpers_ui.R` — add run color palette

- [ ] **Step 1: Add run color palette to helpers_ui.R**

Add at the end of `helpers_ui.R`:

```r
# Multi-run color palette (max 4 runs)
.RUN_COLORS <- c(
  "Run A" = "#2563eb",  # blue
  "Run B" = "#dc2626",  # red
  "Run C" = "#16a34a",  # green
  "Run D" = "#d97706"   # amber
)

run_color <- function(run_name) {
  .RUN_COLORS[run_name] %||% "#6b7280"
}
```

- [ ] **Step 2: Create mod_compare.R**

```r
# =============================================================================
# mod_compare.R -- Multi-run comparison management
# =============================================================================

mod_compare_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "upload-box",
      tags$h6("Comparaison"),
      actionButton(ns("add_run"), "Ajouter un run", icon = icon("plus"),
                   class = "btn-sm btn-outline-primary w-100"),
      uiOutput(ns("run_list"))
    )
  )
}

mod_compare_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Store comparison runs: list of lists with name, file_paths, parsed data
    comp_runs <- reactiveValues(runs = list())
    run_counter <- reactiveVal(0L)

    observeEvent(input$add_run, {
      n <- run_counter() + 1L
      if (n > 3L) {
        showNotification("Maximum 3 runs de comparaison (4 total)", type = "warning")
        return()
      }
      run_counter(n)
      run_id <- paste0("run_", n)
      run_name <- c("Run B", "Run C", "Run D")[n]

      comp_runs$runs[[run_id]] <- list(
        id = run_id, name = run_name,
        file_paths = list(ext = NULL, shk = NULL, coi = NULL, clt = NULL, tab = NULL),
        ext_data = NULL, shk_data = NULL, coi_data = NULL, clt_data = NULL, tab_data = NULL
      )
    })

    # Render run list with upload inputs and remove buttons
    output$run_list <- renderUI({
      runs <- comp_runs$runs
      if (length(runs) == 0L) return(NULL)

      run_uis <- lapply(names(runs), function(rid) {
        r <- runs[[rid]]
        color <- run_color(r$name)
        div(class = "upload-box", style = paste0("border-left: 3px solid ", color, ";"),
          fluidRow(
            column(8, textInput(ns(paste0("name_", rid)), NULL, value = r$name,
                                width = "100%")),
            column(4, actionButton(ns(paste0("rm_", rid)), NULL, icon = icon("xmark"),
                                   class = "btn-sm btn-outline-danger"))
          ),
          fileInput(ns(paste0("upload_", rid)), NULL, multiple = TRUE,
                    accept = c(".ext", ".shk", ".coi", ".clt", ".tab",
                               ".tar.gz", ".tgz", ".gz"),
                    buttonLabel = "Fichiers"),
          uiOutput(ns(paste0("status_", rid)))
        )
      })
      tagList(run_uis)
    })

    # Track which run IDs have had observers created (avoid duplicates)
    observed_runs <- reactiveVal(character())

    # Create observers for new runs only (avoids leak from re-creating inside observe)
    observe({
      current_ids <- names(comp_runs$runs)
      already <- observed_runs()
      new_ids <- setdiff(current_ids, already)

      for (rid in new_ids) {
        local({
          local_rid <- rid

          observeEvent(input[[paste0("upload_", local_rid)]], {
            files <- input[[paste0("upload_", local_rid)]]
            req(files)
            paths <- list(ext = NULL, shk = NULL, coi = NULL, clt = NULL, tab = NULL)

            if (nrow(files) == 1L && grepl("\\.(tar\\.gz|tgz)$", files$name, ignore.case = TRUE)) {
              tmp <- file.path(tempdir(), paste0("comp_", local_rid, "_", format(Sys.time(), "%H%M%S")))
              dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
              untar(files$datapath, exdir = tmp)
              all_files <- list.files(tmp, recursive = TRUE, full.names = TRUE)
              all_names <- basename(all_files)
              for (ext_type in c("ext", "shk", "coi", "clt", "tab")) {
                idx <- which(grepl(paste0("\\.", ext_type, "$"), all_names, ignore.case = TRUE))[1]
                if (!is.na(idx)) paths[[ext_type]] <- all_files[idx]
              }
            } else {
              for (i in seq_len(nrow(files))) {
                nm <- files$name[i]
                dp <- files$datapath[i]
                for (ext_type in c("ext", "shk", "coi", "clt", "tab")) {
                  if (grepl(paste0("\\.", ext_type, "$"), nm, ignore.case = TRUE))
                    paths[[ext_type]] <- dp
                }
              }
            }

            run <- comp_runs$runs[[local_rid]]
            run$file_paths <- paths
            if (!is.null(paths$ext)) run$ext_data <- tryCatch(read_ext(paths$ext), error = function(e) NULL)
            if (!is.null(paths$shk)) run$shk_data <- tryCatch(read_shk(paths$shk), error = function(e) NULL)
            if (!is.null(paths$coi)) run$coi_data <- tryCatch(read_coi(paths$coi), error = function(e) NULL)
            if (!is.null(paths$clt)) run$clt_data <- tryCatch(read_clt(paths$clt), error = function(e) NULL)
            if (!is.null(paths$tab)) run$tab_data <- tryCatch(read_tab(paths$tab), error = function(e) NULL)
            comp_runs$runs[[local_rid]] <- run
          }, ignoreInit = TRUE)

          observeEvent(input[[paste0("name_", local_rid)]], {
            comp_runs$runs[[local_rid]]$name <- input[[paste0("name_", local_rid)]]
          }, ignoreInit = TRUE)

          observeEvent(input[[paste0("rm_", local_rid)]], {
            comp_runs$runs[[local_rid]] <- NULL
            run_counter(max(0L, run_counter() - 1L))
          }, ignoreInit = TRUE)
        })
      }

      observed_runs(union(already, new_ids))
    })

    # Return reactive list of all runs (primary + comparison)
    # The primary run is handled by mod_upload, comparison runs here
    list(
      comp_runs = reactive(comp_runs$runs)
    )
  })
}
```

- [ ] **Step 3: Commit**

```bash
git add app/R/mod_compare.R app/R/helpers_ui.R
git commit -m "feat(app): add mod_compare.R multi-run management module"
```

---

### Task 5: Integrate multi-run into app.R and update all modules

**Files:**
- Modify: `app/app.R` — add compare module, restructure data passing
- Modify: `app/R/mod_rse.R` — grouped barplot for multi-runs
- Modify: `app/R/mod_relativeinf.R` — grouped barplot
- Modify: `app/R/mod_params.R` — multi-run columns
- Modify: `app/R/mod_fim.R` — comparison table + dropdown
- Modify: `app/R/mod_times.R` — overlay
- Modify: `app/R/mod_convergence.R` — superposed curves
- Modify: `app/R/mod_raw.R` — dropdown selector

This is a large task. The key pattern: each module receives an `all_runs` reactive that returns a named list of `list(name, ext_data, shk_data, coi_data, clt_data, tab_data)`. The first element is always "Run A" (primary). Modules iterate over this list for their displays.

- [ ] **Step 1: Add compare module to app.R sidebar**

In the sidebar section of `app.R`, add after `mod_upload_ui("upload")` (line 49):

```r
    mod_compare_ui("compare"),
```

- [ ] **Step 2: Create `all_runs` reactive in app.R server**

After `upload <- mod_upload_server("upload")` (line 115), add:

```r
  compare <- mod_compare_server("compare")

  # Build all_runs: primary + comparison runs
  all_runs <- reactive({
    primary <- list(
      name     = "Run A",
      ext_data = upload$ext_data(),
      shk_data = upload$shk_data(),
      coi_data = upload$coi_data(),
      clt_data = upload$clt_data(),
      tab_data = upload$tab_data()
    )
    runs <- list(primary = primary)

    comp <- compare$comp_runs()
    for (rid in names(comp)) {
      r <- comp[[rid]]
      runs[[rid]] <- list(
        name     = r$name,
        ext_data = r$ext_data,
        shk_data = r$shk_data,
        coi_data = r$coi_data,
        clt_data = r$clt_data,
        tab_data = r$tab_data
      )
    }
    runs
  })
```

- [ ] **Step 3: Pass all_runs to each module**

Update each `mod_*_server()` call to also pass `all_runs`. Each module will use the primary run as before for backwards compatibility, plus `all_runs` for comparison views.

For `mod_rse_server`:
```r
  mod_rse_server("rse",
    ext_data     = upload$ext_data,
    tbl_no       = tbl_no,
    param_labels = param_labels_r,
    se_mode      = se_mode_r,
    all_runs     = all_runs
  )
```

Apply the same pattern to all modules. Each gets `all_runs = all_runs` added.

- [ ] **Step 4: Update mod_rse.R for multi-run grouped barplot**

Update `mod_rse_server` to handle multi-run. Keep the existing `renderPlot` structure, just add multi-run logic inside it. Note: `mod_rse` currently only has a plot (no DT table) — the DT table is in `mod_params`. So the replacement is safe:

```r
mod_rse_server <- function(id, ext_data, tbl_no, param_labels, se_mode, all_runs = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot({
      ext <- ext_data(); req(ext)
      runs <- all_runs()
      mode <- se_mode()

      # If only primary run, use existing plots
      if (length(runs) <= 1) {
        if (!is.null(mode) && mode == "SE absolues") {
          return(plot_se(ext, table_no = tbl_no(), param_labels = param_labels()))
        } else {
          return(plot_rse(ext, table_no = tbl_no(), param_labels = param_labels()))
        }
      }

      # Multi-run: build combined data
      show_se <- (!is.null(mode) && mode == "SE absolues")
      combined <- purrr::imap_dfr(runs, function(r, idx) {
        if (is.null(r$ext_data)) return(NULL)
        rse <- get_rse(r$ext_data, tbl_no())
        if (nrow(rse) == 0) return(NULL)
        lbls <- param_labels()
        if (!is.null(lbls)) {
          rse <- rse |> mutate(param = if_else(param %in% names(lbls), lbls[param], param))
        }
        rse |> mutate(run = r$name)
      })

      if (nrow(combined) == 0) return(NULL)

      y_var <- if (show_se) "se" else "rse_pct"
      y_lab <- if (show_se) "SE" else "RSE (%)"

      ggplot(combined, aes(x = param, y = .data[[y_var]], fill = run)) +
        geom_col(position = position_dodge(width = 0.75), width = 0.65,
                 color = "white", linewidth = 0.3) +
        {if (!show_se) geom_hline(yintercept = c(20, 50), linetype = "dashed",
                                   color = "grey40", linewidth = 0.45)} +
        scale_fill_manual(values = .RUN_COLORS, name = NULL) +
        labs(title = paste(y_lab, "-- Comparaison multi-runs"), x = NULL, y = y_lab) +
        theme_bw(base_size = 11) +
        theme(legend.position = "bottom", panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank(),
              axis.text.x = element_text(angle = 30, hjust = 1, size = 9))
    }, res = 110)
  })
}
```

- [ ] **Step 5: Update mod_relativeinf.R for multi-run**

Same pattern as RSE: if multiple runs, show grouped barplot.

```r
mod_relativeinf_server <- function(id, shk_data, tbl_no, param_labels, all_runs = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot({
      runs <- all_runs()

      # Single run
      if (length(runs) <= 1) {
        shk <- shk_data()
        if (is.null(shk)) {
          return(ggplot() + labs(title = "Chargez un fichier .shk pour afficher RELATIVEINF(%)") + theme_bw())
        }
        lbls <- param_labels()
        eta_labels <- NULL
        if (!is.null(lbls)) {
          eta_labels <- setNames(lbls, paste0("ETA", seq_along(lbls)))
        }
        return(plot_relativeinf(shk, table_no = tbl_no(), param_labels = eta_labels))
      }

      # Multi-run
      combined <- purrr::imap_dfr(runs, function(r, idx) {
        if (is.null(r$shk_data)) return(NULL)
        ri <- get_relativeinf(r$shk_data, tbl_no())
        if (nrow(ri) == 0) return(NULL)
        lbls <- param_labels()
        if (!is.null(lbls)) {
          eta_labels <- setNames(lbls, paste0("ETA", seq_along(lbls)))
          ri <- ri |> mutate(eta = if_else(eta %in% names(eta_labels), eta_labels[eta], eta))
        }
        ri |> mutate(run = r$name)
      })

      if (nrow(combined) == 0) return(ggplot() + labs(title = "Pas de RELATIVEINF") + theme_bw())

      ggplot(combined, aes(x = reorder(eta, relativeinf_pct), y = relativeinf_pct, fill = run)) +
        geom_col(position = position_dodge(width = 0.75), width = 0.65,
                 color = "white", linewidth = 0.3) +
        geom_hline(yintercept = c(20, 50), linetype = "dashed", color = "grey40", linewidth = 0.45) +
        scale_fill_manual(values = .RUN_COLORS, name = NULL) +
        coord_flip() +
        labs(title = "RELATIVEINF (%) -- Comparaison multi-runs", x = NULL, y = "RELATIVEINF (%)") +
        theme_bw(base_size = 11) +
        theme(legend.position = "bottom", panel.grid.minor = element_blank(),
              panel.grid.major.y = element_blank())
    }, res = 110)
  })
}
```

- [ ] **Step 6: Update mod_convergence.R for multi-run overlay**

```r
mod_convergence_server <- function(id, ext_data, log_conv, all_runs = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot({
      runs <- all_runs()

      if (length(runs) <= 1) {
        ext <- ext_data(); req(ext)
        return(plot_convergence(ext, log_iter = log_conv()))
      }

      # Multi-run: overlay convergence curves
      combined <- purrr::imap_dfr(runs, function(r, idx) {
        if (is.null(r$ext_data)) return(NULL)
        r$ext_data |>
          filter(type == "iteration") |>
          select(table_no, ITERATION, OBJ) |>
          filter(!is.na(OBJ), !is.na(ITERATION)) |>
          mutate(run = r$name)
      })

      if (nrow(combined) == 0) return(ggplot() + labs(title = "Pas de convergence") + theme_bw())

      p <- ggplot(combined, aes(x = ITERATION, y = OBJ, color = run)) +
        geom_line(linewidth = 0.75, alpha = 0.9) +
        scale_color_manual(values = .RUN_COLORS, name = NULL) +
        labs(title = "Convergence -- Comparaison multi-runs",
             x = "Iteration", y = "OFV") +
        theme_bw(base_size = 11) +
        theme(legend.position = "bottom")

      if (log_conv()) p <- p + scale_x_log10()
      p
    }, res = 110)
  })
}
```

- [ ] **Step 7: Update mod_fim.R — add comparison table**

Add `all_runs` parameter and a comparison summary row if multiple runs loaded. Add after the existing cards output:

```r
mod_fim_server <- function(id, ext_data, coi_data, clt_data, tbl_no, param_labels, all_runs = reactive(list())) {
```

In the `output$content` renderUI, add a comparison table if multiple runs. Insert before the eigenvalues section:

```r
        # Multi-run comparison summary
        if (length(all_runs()) > 1) {
          tagList(
            div(class = "param-table-wrap",
              p(class = "section-title", "Comparaison FIM multi-runs"),
              DTOutput(ns("compare_table"))
            ),
            br()
          )
        }
```

Add the render for compare_table:

```r
    output$compare_table <- renderDT({
      runs <- all_runs(); req(length(runs) > 1)
      comp_df <- purrr::imap_dfr(runs, function(r, idx) {
        if (is.null(r$ext_data)) return(NULL)
        ext <- r$ext_data
        ofv <- get_ofv(ext, tbl_no())
        rse <- get_rse(ext, tbl_no())
        n_params <- nrow(rse)
        d_crit <- get_d_criterion(ofv, n_params)
        cn <- get_condition_number(ext, tbl_no())
        tibble(
          Run = r$name,
          OFV = round(ofv, 4),
          `D-critere` = signif(d_crit, 4),
          `Params` = n_params,
          `Cond. #` = if (!is.na(cn$condition_number)) signif(cn$condition_number, 4) else NA,
          `RSE moy. (%)` = round(mean(rse$rse_pct, na.rm = TRUE), 2),
          `RSE max (%)` = round(max(rse$rse_pct, na.rm = TRUE), 2)
        )
      })
      datatable(comp_df, rownames = FALSE, class = "stripe hover compact",
                options = list(pageLength = 5, dom = "t"))
    })
```

For the heatmap: add a dropdown to select which run's FIM to display when multiple runs are loaded.

- [ ] **Step 8: Update mod_raw.R — dropdown selector**

```r
mod_raw_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "param-table-wrap",
      fluidRow(
        column(9, p(class = "section-title", "Contenu complet du fichier .ext")),
        column(3, uiOutput(ns("run_selector")))
      ),
      DTOutput(ns("raw_ext"))
    )
  )
}

mod_raw_server <- function(id, ext_data, all_runs = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$run_selector <- renderUI({
      runs <- all_runs()
      if (length(runs) <= 1) return(NULL)
      choices <- setNames(names(runs), sapply(runs, `[[`, "name"))
      selectInput(ns("selected_run"), "Run", choices = choices, selected = names(runs)[1])
    })

    selected_ext <- reactive({
      runs <- all_runs()
      if (length(runs) <= 1) return(ext_data())
      sel <- input$selected_run %||% names(runs)[1]
      runs[[sel]]$ext_data
    })

    output$raw_ext <- renderDT({
      ext <- selected_ext(); req(ext)
      datatable(
        ext |> mutate(across(where(is.double), ~ round(.x, 6))),
        rownames = FALSE, filter = "top",
        class = "stripe hover compact",
        options = list(pageLength = 20, scrollX = TRUE)
      )
    })
  })
}
```

- [ ] **Step 9: Update mod_params.R and mod_times.R to accept all_runs**

Add `all_runs = reactive(list())` parameter to both server function signatures:

```r
# mod_params.R:
mod_params_server <- function(id, ext_data, shk_data, ext_lines, tbl_no, param_labels,
                               ctl_data = reactive(NULL), all_runs = reactive(list())) {

# mod_times.R:
mod_times_server <- function(id, tab_data, all_runs = reactive(list())) {
```

For mod_times, add multi-run overlay in the gantt plot when multiple runs have tab_data:

```r
    output$gantt <- renderPlot({
      tab <- tab_data(); req(tab)
      runs <- all_runs()
      if (length(runs) <= 1) return(plot_optimal_times(tab))

      # Multi-run: combine tab data with run names
      combined <- purrr::imap_dfr(runs, function(r, idx) {
        if (is.null(r$tab_data)) return(NULL)
        obs <- r$tab_data
        if ("EVID" %in% names(obs)) obs <- filter(obs, EVID == 0)
        if (!"TSTRAT" %in% names(obs)) obs$TSTRAT <- 1
        obs |> mutate(run = r$name) |> select(any_of(c("TSTRAT", "TIME", "run")))
      })
      if (nrow(combined) == 0) return(plot_optimal_times(tab))

      combined <- combined |>
        mutate(group = factor(paste0("Groupe ", TSTRAT)))

      ggplot(combined, aes(x = TIME, y = group, color = run, shape = run)) +
        geom_point(size = 3, alpha = 0.85, position = position_dodge(width = 0.4)) +
        scale_color_manual(values = .RUN_COLORS, name = NULL) +
        labs(title = "Temps d'echantillonnage -- Comparaison multi-runs",
             x = "Temps", y = NULL) +
        theme_bw(base_size = 11) +
        theme(legend.position = "bottom", panel.grid.major.y = element_blank())
    }, res = 110)
```

- [ ] **Step 10: Update all module calls in app.R**

Ensure every `mod_*_server()` call passes `all_runs = all_runs`. This is the integration point.

- [ ] **Step 11: Test the app with 2 runs**

Load example1 warfarin files as primary. Add a comparison run and load example2 warfarin2 files. Verify:
- RSE grouped barplot shows both runs side by side
- FIM comparison table shows both runs' metrics
- Convergence shows overlaid curves
- Raw data dropdown switches between runs

- [ ] **Step 12: Commit**

```bash
git add app/app.R app/R/mod_compare.R app/R/mod_rse.R app/R/mod_relativeinf.R app/R/mod_params.R app/R/mod_fim.R app/R/mod_times.R app/R/mod_convergence.R app/R/mod_raw.R app/R/helpers_ui.R
git commit -m "feat(app): multi-run comparison across all tabs"
```

---

## Chunk 3: Update 3 — Warfarin Examples + Update 4 — Enriched Plots

### Task 6: Copy example files and create mod_examples.R

**Files:**
- Create: `app/examples/example1/` — copy warfarin.ext, .shk, .coi, .clt, .tab, .ctl
- Create: `app/examples/example2/` — copy warfarin2*.ext, .shk, .coi, .clt, .tab, .ctl
- Create: `app/examples/example4/` — copy warfarin_pkpd*.ext, .shk, .coi, .clt, .tab, .ctl
- Create: `app/R/mod_examples.R`

- [ ] **Step 1: Copy example files**

```bash
mkdir -p app/examples/example1 app/examples/example2 app/examples/example4

# Example 1
cp docs/bauer2021_examples/example1/warfarin.ext app/examples/example1/
cp docs/bauer2021_examples/example1/warfarin.shk app/examples/example1/
cp docs/bauer2021_examples/example1/warfarin.coi app/examples/example1/
cp docs/bauer2021_examples/example1/warfarin.clt app/examples/example1/
cp docs/bauer2021_examples/example1/warfarin.tab app/examples/example1/
cp docs/bauer2021_examples/example1/warfarin.ctl app/examples/example1/

# Example 2 - find the right files
cp docs/bauer2021_examples/example2/warfarin2b.ext app/examples/example2/
cp docs/bauer2021_examples/example2/warfarin2b.shk app/examples/example2/
cp docs/bauer2021_examples/example2/warfarin2b.coi app/examples/example2/
cp docs/bauer2021_examples/example2/warfarin2b.clt app/examples/example2/
cp docs/bauer2021_examples/example2/warfarin2b.tab app/examples/example2/
cp docs/bauer2021_examples/example2/warfarin2b.ctl app/examples/example2/

# Example 4
cp docs/bauer2021_examples/example4/warfarin_pkpd_eval.ext app/examples/example4/
cp docs/bauer2021_examples/example4/warfarin_pkpd_eval.shk app/examples/example4/
cp docs/bauer2021_examples/example4/warfarin_pkpd_eval.coi app/examples/example4/
cp docs/bauer2021_examples/example4/warfarin_pkpd_eval.clt app/examples/example4/
cp docs/bauer2021_examples/example4/warfarin_pkpd_eval.tab app/examples/example4/
cp docs/bauer2021_examples/example4/warfarin_pkpd_eval.ctl app/examples/example4/
```

Verify files exist after copy. Adjust filenames based on what actually exists in the example directories.

- [ ] **Step 2: Create mod_examples.R**

This module provides a button in the sidebar that opens a modal with example cards. When clicked, it loads the files and sets labels.

```r
# =============================================================================
# mod_examples.R -- Built-in warfarin examples (Bauer 2021)
# =============================================================================

.EXAMPLES <- list(
  example1 = list(
    title = "Exemple 1 : Evaluation d'un design",
    desc = "Modele warfarin 1-CMT, evaluation FIM bloc-diagonale (FIMDIAG=1). Pas d'optimisation.",
    dir = "examples/example1",
    prefix = "warfarin",
    labels = "THETA1=CL\nTHETA2=V\nTHETA3=KA",
    guide = list(
      context = "Modele warfarin 1-compartiment (ADVAN2 TRANS2), absorption premier ordre, erreur combinee. 32 sujets, GROUPSIZE=32.",
      points = c(
        "RSE de CL et V sont faibles (< 20%) : le design est informatif pour ces parametres",
        "RSE de KA est plus eleve : l'absorption est plus difficile a estimer",
        "RELATIVEINF montre la reduction d'incertitude apportee par le design sur chaque ETA",
        "MAXEVAL=0 : c'est une evaluation, pas une optimisation"
      )
    )
  ),
  example2 = list(
    title = "Exemple 2 : Optimisation des temps",
    desc = "Optimisation des temps de prelevement par Nelder-Mead (DESEL=TIME, MAXEVAL=4000).",
    dir = "examples/example2",
    prefix = "warfarin2b",
    labels = "THETA1=CL\nTHETA2=V\nTHETA3=KA",
    compare_with = "example1",
    guide = list(
      context = "Meme modele warfarin, mais avec optimisation des temps via NELDER. Les temps sont libres dans les fenetres definies par TMIN/TMAX.",
      points = c(
        "Comparez les RSE avant/apres optimisation (bouton 'Comparer avec l'evaluation')",
        "L'OFV (= -log(det(FIM))) diminue : le determinant de la FIM augmente",
        "Les temps optimaux dans l'onglet 'Temps optimaux' montrent ou prelever",
        "Consultez la convergence : le NELDER converge-t-il bien ?"
      )
    )
  ),
  example4 = list(
    title = "Exemple 4 : PK-PD multi-reponses",
    desc = "Modele warfarin PK-PD (concentration + effet), FIMTYPE=1 + VARCROSS=1.",
    dir = "examples/example4",
    prefix = "warfarin_pkpd_eval",
    labels = "THETA1=CL\nTHETA2=V\nTHETA3=KA\nTHETA4=EMAX\nTHETA5=EC50",
    guide = list(
      context = "Modele PK-PD multi-compartiment avec effet Emax. CMT==2 pour PK, CMT==3 pour PD. FIMTYPE=1 (bloc-diagonal) + VARCROSS=1.",
      points = c(
        "Plus de parametres a estimer (5 THETA + OMEGA + SIGMA PK et PD)",
        "FIMTYPE=1 + VARCROSS=1 equivaut a PFIM style bloc-diagonal",
        "Les RSE des parametres PD (EMAX, EC50) sont generalement plus grands",
        "Le design doit etre informatif pour les deux reponses simultanement"
      )
    )
  )
)

mod_examples_ui <- function(id) {
  ns <- NS(id)
  actionButton(ns("open_examples"), "Exemples", icon = icon("book-open"),
               class = "btn-sm btn-outline-secondary w-100",
               style = "margin-bottom: 8px;")
}

mod_examples_server <- function(id, session_main) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Selected example data to return
    selected <- reactiveValues(
      file_paths = NULL, labels = NULL, guide = NULL, compare_paths = NULL
    )

    observeEvent(input$open_examples, {
      showModal(modalDialog(
        title = "Exemples Bauer 2021 -- Warfarin",
        size = "l",
        easyClose = TRUE,
        fluidRow(
          lapply(names(.EXAMPLES), function(ex_id) {
            ex <- .EXAMPLES[[ex_id]]
            column(4,
              div(class = "upload-box", style = "cursor:pointer; min-height:200px;",
                tags$h6(ex$title),
                tags$p(style = "font-size:.85rem; color:#4b5563;", ex$desc),
                actionButton(ns(paste0("load_", ex_id)), "Charger",
                             class = "btn-sm btn-primary w-100")
              )
            )
          })
        )
      ))
    })

    # Load example handlers
    lapply(names(.EXAMPLES), function(ex_id) {
      observeEvent(input[[paste0("load_", ex_id)]], {
        ex <- .EXAMPLES[[ex_id]]
        base <- ex$dir

        paths <- list(ext = NULL, shk = NULL, coi = NULL, clt = NULL, tab = NULL)
        for (ext_type in names(paths)) {
          f <- file.path(base, paste0(ex$prefix, ".", ext_type))
          if (file.exists(f)) paths[[ext_type]] <- f
        }

        selected$file_paths <- paths
        selected$labels <- ex$labels
        selected$guide <- ex$guide

        # Handle compare_with
        if (!is.null(ex$compare_with)) {
          comp_ex <- .EXAMPLES[[ex$compare_with]]
          comp_paths <- list(ext = NULL, shk = NULL, coi = NULL, clt = NULL, tab = NULL)
          for (ext_type in names(comp_paths)) {
            f <- file.path(comp_ex$dir, paste0(comp_ex$prefix, ".", ext_type))
            if (file.exists(f)) comp_paths[[ext_type]] <- f
          }
          selected$compare_paths <- comp_paths
          selected$compare_name <- comp_ex$title
        } else {
          selected$compare_paths <- NULL
        }

        removeModal()
        showNotification(paste("Exemple charge :", ex$title), type = "message")
      })
    })

    list(
      file_paths    = reactive(selected$file_paths),
      labels        = reactive(selected$labels),
      guide         = reactive(selected$guide),
      compare_paths = reactive(selected$compare_paths),
      compare_name  = reactive(selected$compare_name)
    )
  })
}
```

- [ ] **Step 3: Integrate mod_examples into app.R**

Add `mod_examples_ui("examples")` in the sidebar before `mod_upload_ui("upload")`.

In the server, add:
```r
  examples <- mod_examples_server("examples", session)
```

Wire up example loading — observe `examples$file_paths()` and parse/load the data:

```r
  # When an example is loaded, parse files and inject into the upload module's reactives
  example_ext  <- reactiveVal(NULL)
  example_shk  <- reactiveVal(NULL)
  example_coi  <- reactiveVal(NULL)
  example_clt  <- reactiveVal(NULL)
  example_tab  <- reactiveVal(NULL)
  example_ctl  <- reactiveVal(NULL)

  observeEvent(examples$file_paths(), {
    paths <- examples$file_paths(); req(paths)
    if (!is.null(paths$ext)) example_ext(tryCatch(read_ext(paths$ext), error = function(e) NULL))
    if (!is.null(paths$shk)) example_shk(tryCatch(read_shk(paths$shk), error = function(e) NULL))
    if (!is.null(paths$coi)) example_coi(tryCatch(read_coi(paths$coi), error = function(e) NULL))
    if (!is.null(paths$clt)) example_clt(tryCatch(read_clt(paths$clt), error = function(e) NULL))
    if (!is.null(paths$tab)) example_tab(tryCatch(read_tab(paths$tab), error = function(e) NULL))
    # Read .ctl if present
    ctl_path <- file.path(dirname(paths$ext), paste0(tools::file_path_sans_ext(basename(paths$ext)), ".ctl"))
    if (file.exists(ctl_path)) example_ctl(tryCatch(read_prior_nwpri(ctl_path), error = function(e) NULL))
    # Set labels
    lbl <- examples$labels()
    if (!is.null(lbl)) updateTextAreaInput(session, "param_labels", value = lbl)
  })

  # Merge example data with upload data (example takes priority if set)
  merged_ext <- reactive({ example_ext() %||% upload$ext_data() })
  merged_shk <- reactive({ example_shk() %||% upload$shk_data() })
  merged_coi <- reactive({ example_coi() %||% upload$coi_data() })
  merged_clt <- reactive({ example_clt() %||% upload$clt_data() })
  merged_tab <- reactive({ example_tab() %||% upload$tab_data() })
  merged_ctl <- reactive({ example_ctl() %||% upload$ctl_data() })
```

Then update all module server calls to use `merged_*` reactives instead of `upload$*`.

Also wire the compare_with: when example has `compare_paths`, load those into comparison run:

```r
  observeEvent(examples$compare_paths(), {
    comp_paths <- examples$compare_paths(); req(comp_paths)
    # Programmatically inject as a comparison run via mod_compare
    # This will be handled by exposing a load_comparison() function from mod_compare
  })
```

- [ ] **Step 4: Add guide banner UI**

In `app.R` UI, add a `uiOutput("guide_banner")` as the first element inside `page_navbar`, using the `header` parameter (merge with existing CSS header):

```r
  header = tagList(
    tags$head(includeCSS("www/styles.css")),
    uiOutput("guide_banner")
  ),
```

In server, render the guide banner:

```r
  output$guide_banner <- renderUI({
    guide <- examples$guide()
    if (is.null(guide)) return(NULL)

    # Read .ctl content for display
    paths <- examples$file_paths()
    ctl_content <- NULL
    if (!is.null(paths$ext)) {
      ctl_path <- file.path(dirname(paths$ext), paste0(tools::file_path_sans_ext(basename(paths$ext)), ".ctl"))
      if (file.exists(ctl_path)) ctl_content <- paste(readLines(ctl_path), collapse = "\n")
    }

    div(class = "guide-banner",
      span(class = "dismiss-btn", onclick = "this.parentElement.style.display='none'", "x"),
      tags$h6(guide$context),
      if (!is.null(ctl_content)) tags$code(ctl_content),
      tags$ul(lapply(guide$points, tags$li))
    )
  })
```

- [ ] **Step 5: Test examples**

Run the app. Click "Exemples". Load Example 1. Verify files load, labels auto-populate, guide banner shows context and key points.

- [ ] **Step 6: Commit**

```bash
git add app/R/mod_examples.R app/examples/ app/app.R
git commit -m "feat(app): add built-in warfarin examples with pedagogical guides"
```

---

### Task 7: Add waterfall RSE plot

**Files:**
- Modify: `scripts/report_design.R` — add `plot_rse_waterfall()`
- Modify: `app/R/mod_rse.R` — add toggle between barplot and waterfall

- [ ] **Step 1: Add `plot_rse_waterfall()` to report_design.R**

```r
#' Waterfall plot des RSE (barres horizontales triees)
#'
#' @param ext          Tibble retourne par read_ext()
#' @param table_no     Numero de table
#' @param param_labels Vecteur nomme
#' @param title        Titre
#' @return Objet ggplot2
#' @export
plot_rse_waterfall <- function(ext, table_no = NULL, param_labels = NULL, title = NULL) {
  rse <- get_rse(ext, table_no)
  if (nrow(rse) == 0L) {
    return(ggplot() + labs(title = "Pas de donnees RSE") + .theme_design())
  }

  if (!is.null(param_labels)) {
    rse <- rse |> mutate(param = if_else(param %in% names(param_labels), param_labels[param], param))
  }

  rse <- rse |>
    mutate(quality = factor(
      .rse_quality(rse_pct),
      levels = c("< 20% (bon)", "20-50% (acceptable)", "> 50% (mediocre)", "Inconnu")
    )) |>
    arrange(desc(rse_pct))

  ttl <- title %||% "RSE predit par la FIM (%) -- Waterfall"

  ggplot(rse, aes(x = reorder(param, rse_pct), y = rse_pct, fill = quality)) +
    geom_col(width = 0.65, color = "white", linewidth = 0.3) +
    geom_hline(yintercept = c(20, 50), linetype = "dashed", color = "grey40", linewidth = 0.45) +
    geom_text(aes(label = sprintf("%.2f%%", rse_pct)),
              hjust = -0.12, size = 3, color = "grey25") +
    scale_fill_manual(values = .COLORS_RSE, name = NULL, drop = FALSE) +
    coord_flip() +
    labs(title = ttl, x = NULL, y = "RSE (%)") +
    .theme_design() +
    theme(panel.grid.major.y = element_blank())
}
```

- [ ] **Step 2: Add toggle in mod_rse.R UI**

Update `mod_rse_ui` to include a toggle:

```r
mod_rse_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "plot-card",
      fluidRow(
        column(9, p(class = "section-title", "RSE / SE predits par la FIM -- par parametre")),
        column(3, radioButtons(ns("plot_style"), NULL,
                               choices = c("Barplot" = "bar", "Waterfall" = "waterfall"),
                               selected = "bar", inline = TRUE))
      ),
      plotOutput(ns("plot"), height = "420px")
    )
  )
}
```

In the single-run branch of `mod_rse_server`, add waterfall handling:

```r
      # In single-run branch, after mode check:
      plot_style <- input$plot_style %||% "bar"
      if (plot_style == "waterfall" && (is.null(mode) || mode != "SE absolues")) {
        return(plot_rse_waterfall(ext, table_no = tbl_no(), param_labels = param_labels()))
      }
```

- [ ] **Step 3: Commit**

```bash
git add scripts/report_design.R app/R/mod_rse.R
git commit -m "feat(app): add waterfall RSE plot with toggle"
```

---

### Task 8: Add correlation matrix plot

**Files:**
- Modify: `scripts/parse_design_outputs.R` — add `get_cor_matrix()`
- Modify: `app/R/mod_fim.R` — add correlation matrix tab

- [ ] **Step 1: Add `get_cor_matrix()` to parse_design_outputs.R**

```r
#' Calculer la matrice de correlation a partir de la FIM
#'
#' Inverse la FIM (via solve()) pour obtenir var-cov, puis cov2cor().
#'
#' @param fim_matrix Matrice numerique nommee (FIM)
#' @return Matrice de correlation, ou NULL si FIM singuliere
#' @export
get_cor_matrix <- function(fim_matrix) {
  if (is.null(fim_matrix) || nrow(fim_matrix) == 0L) return(NULL)
  nonzero <- diag(fim_matrix) != 0
  if (sum(nonzero) < 2L) return(NULL)
  fim_sub <- fim_matrix[nonzero, nonzero]
  tryCatch({
    vcov <- solve(fim_sub)
    cov2cor(vcov)
  }, error = function(e) NULL)
}
```

- [ ] **Step 2: Add dedicated correlation matrix section in mod_fim.R**

The existing `plot_fim_heatmap()` already computes FIM -> inverse -> cov2cor and displays it. `get_cor_matrix()` extracts just the matrix for reuse. Add a separate labeled section in the `output$content` renderUI after the existing heatmap:

```r
        # In mod_fim_server, output$content renderUI, after the existing heatmap column:
        column(6,
          div(class = "plot-card",
            p(class = "section-title", "Matrice de correlation des parametres"),
            if (!is.null(fim)) {
              tagList(
                plotOutput(ns("cor_heatmap"), height = "400px"),
                p(style = "font-size:.8rem; color:#6b7280; margin-top:4px;",
                  "Calculee via FIM^-1 (solve) puis cov2cor. Valeurs dans les cellules.")
              )
            } else {
              div(class = "alert alert-warning", "Fichier .coi ou .clt requis.")
            }
          )
        )
```

Add the render (reuses existing `plot_fim_heatmap` which already shows numeric values in cells):

```r
    output$cor_heatmap <- renderPlot({
      fim <- fim_matrix(); req(fim)
      plot_fim_heatmap(fim, labels = param_labels(),
                       title = "Matrice de correlation des parametres")
    }, res = 110)
```

- [ ] **Step 3: Commit**

```bash
git add scripts/parse_design_outputs.R app/R/mod_fim.R
git commit -m "feat(app): add get_cor_matrix() and dedicated correlation display"
```

---

### Task 9: Add model prediction plot

**Files:**
- Modify: `scripts/report_design.R` — add `plot_model_prediction()`
- Modify: `app/R/mod_times.R` — add prediction plot

- [ ] **Step 1: Add `plot_model_prediction()` to report_design.R**

```r
#' Model prediction plot avec temps de sampling
#'
#' Inspire de PopED plot_model_prediction(). Trace IPRED vs TIME depuis le .tab
#' avec les points de sampling marques.
#'
#' @param tab_data   Tibble retourne par read_tab()
#' @param group_col  Colonne de groupement (defaut : "TSTRAT")
#' @param title      Titre
#' @return Objet ggplot2
#' @export
plot_model_prediction <- function(tab_data, group_col = "TSTRAT", title = NULL) {
  if (is.null(tab_data) || nrow(tab_data) == 0L) {
    return(ggplot() + labs(title = "Pas de donnees .tab") + .theme_design())
  }

  obs <- tab_data
  if ("EVID" %in% names(obs)) obs <- filter(obs, EVID == 0)
  if (nrow(obs) == 0L) {
    return(ggplot() + labs(title = "Aucune observation") + .theme_design())
  }

  # Determine Y variable
  y_col <- intersect(c("IPRED", "PRED", "DV", "CONC"), names(obs))[1]
  if (is.na(y_col)) {
    return(ggplot() + labs(title = "Colonne IPRED/PRED/DV absente") + .theme_design())
  }

  if (!group_col %in% names(obs)) obs[[group_col]] <- 1

  obs <- obs |>
    mutate(group = factor(paste0("Groupe ", .data[[group_col]])),
           y_val = .data[[y_col]])

  ttl <- title %||% paste("Prediction du modele (", y_col, ") vs Temps")

  ggplot(obs, aes(x = TIME, y = y_val, color = group)) +
    geom_line(linewidth = 0.8, alpha = 0.7) +
    geom_point(size = 2.5, alpha = 0.85) +
    scale_color_brewer(palette = "Set2", name = NULL) +
    labs(title = ttl, x = "Temps", y = y_col) +
    .theme_design()
}
```

- [ ] **Step 2: Add prediction plot to mod_times.R**

In `mod_times_server`, add the prediction plot alongside the existing gantt:

```r
      tagList(
        fluidRow(
          column(12,
            div(class = "plot-card",
              p(class = "section-title", "Prediction du modele et temps de sampling"),
              plotOutput(ns("prediction"), height = "350px")
            )
          )
        ),
        br(),
        fluidRow(
          # existing gantt and table...
        )
      )
```

Add render:
```r
    output$prediction <- renderPlot({
      tab <- tab_data(); req(tab)
      plot_model_prediction(tab)
    }, res = 110)
```

- [ ] **Step 3: Commit**

```bash
git add scripts/report_design.R app/R/mod_times.R
git commit -m "feat(app): add model prediction plot in Temps optimaux tab"
```

---

### Task 10: Final integration test and CSS updates

**Files:**
- Modify: `app/www/styles.css` — add multi-run styles

- [ ] **Step 1: Add multi-run CSS styles**

Add to `styles.css`:

```css
/* Multi-run comparison badges */
.run-badge {
  display: inline-block;
  padding: 2px 8px;
  border-radius: 4px;
  font-size: 0.75rem;
  font-weight: 600;
  color: white;
}
.run-badge.run-a { background-color: #2563eb; }
.run-badge.run-b { background-color: #dc2626; }
.run-badge.run-c { background-color: #16a34a; }
.run-badge.run-d { background-color: #d97706; }

/* Guide banner for examples */
.guide-banner {
  background: linear-gradient(135deg, #eff6ff, #dbeafe);
  border: 1px solid #93c5fd;
  border-radius: 10px;
  padding: 16px 20px;
  margin-bottom: 16px;
}
.guide-banner h6 {
  color: #1e40af;
  margin-bottom: 8px;
}
.guide-banner .dismiss-btn {
  float: right;
  cursor: pointer;
  color: #6b7280;
}
.guide-banner code {
  background: #f1f5f9;
  padding: 8px 12px;
  border-radius: 6px;
  display: block;
  margin: 8px 0;
  font-size: 0.8rem;
  max-height: 300px;
  overflow-y: auto;
  white-space: pre;
}
.guide-banner ul {
  margin: 8px 0 0 0;
  padding-left: 20px;
}
.guide-banner li {
  font-size: 0.85rem;
  color: #374151;
  margin-bottom: 4px;
}
```

- [ ] **Step 2: Full integration test**

Run the app. Test:
1. Upload warfarin.ext + .shk — verify RSE 2 decimals, shrinkage plot
2. Upload priortrue.ctl — verify priors section
3. Add comparison run with warfarin2b files — verify grouped plots
4. Click Exemples, load Example 1 — verify auto-load + guide
5. Switch to waterfall RSE view
6. Check FIM correlation heatmap
7. Check model prediction plot

- [ ] **Step 3: Final commit**

```bash
git add app/www/styles.css
git commit -m "style(app): add multi-run and guide banner CSS styles"
```

---

## Summary of all files

### New files
- `app/R/mod_compare.R` — multi-run management
- `app/R/mod_examples.R` — built-in examples
- `app/examples/example1/` — warfarin example files
- `app/examples/example2/` — warfarin optimization files
- `app/examples/example4/` — warfarin PK-PD files

### Modified files
- `app/app.R` — integrate compare, examples, pass all_runs
- `app/R/mod_upload.R` — .ctl upload, ctl_data
- `app/R/mod_params.R` — shrinkage, priors, multi-run
- `app/R/mod_rse.R` — 2 decimals, multi-run grouped barplot, waterfall toggle
- `app/R/mod_relativeinf.R` — multi-run grouped barplot
- `app/R/mod_fim.R` — comparison table, correlation matrix, dropdown
- `app/R/mod_times.R` — model prediction plot, multi-run overlay
- `app/R/mod_convergence.R` — multi-run overlay
- `app/R/mod_raw.R` — dropdown run selector
- `app/R/helpers_ui.R` — run colors, badge formatting to 2 decimals
- `scripts/parse_design_outputs.R` — get_shrinkage, get_cor_matrix, read_prior_nwpri
- `scripts/report_design.R` — plot_rse_waterfall, plot_model_prediction, 2 decimal labels
- `app/www/styles.css` — multi-run styles, guide banner
