## ============================================================================
##  Shared Plotting Helpers
##
##  Source this file at the top of each plotting script:
##    source(file.path(root, "A_Demo/democode/figures/_plot_helpers.R"))
##
##  Provides:
##    - root path detection
##    - x_breaks / x_labels for event study axes
##    - group_labels lookup
##    - plot_panel() function with consistent style
##    - parse_esttab() function to read Stata CSV exports
##    - period_map for variable name -> numeric period
## ============================================================================

library(ggplot2)
library(dplyr)
library(readr)
library(patchwork)
library(stringr)

## ── Unicode-safe PDF device ────────────────────────────────────────────────
## The default pdf() device used by ggsave() for .pdf files cannot render the
## Unicode marker U+25CB (the "○" hollow circle used in panel subtitles):
## it substitutes a dot for each UTF-8 byte, so "○ Overdose death counts"
## prints as "... Overdose death counts", and it also spaces the axis tick
## labels differently from the PNG. A Unicode-aware device fixes both and makes
## the PDF match the PNG. Use quartz(type = "pdf") on macOS, cairo_pdf elsewhere.
pdf_device <- function(filename, width, height, ...) {
  if (Sys.info()[["sysname"]] == "Darwin") {
    grDevices::quartz(file = filename, type = "pdf", width = width, height = height)
  } else {
    grDevices::cairo_pdf(filename = filename, width = width, height = height)
  }
}

## ── PNAS final-size scaling ────────────────────────────────────────────────
## PNAS prints figures at fixed column widths (2-column max = 17.8 cm = 7 in)
## and a maximum height of 22.5 cm (8.86 in), with all text >= 6 pt. The
## plotting scripts lay each figure out at a comfortable design size; this
## returns a single multiplicative factor that shrinks the *printed* figure
## to land within those limits while preserving the exact layout. The 0.75
## floor keeps the smallest design text (8 pt) at or above the 6 pt minimum.
pnas_scale <- function(width, height) {
  max_w <- 7.00   # 17.8 cm, 2-column
  max_h <- 8.86   # 22.5 cm
  f <- min(1, max_w / width, max_h / height)
  max(f, 0.75)
}

## Route every .pdf save through the Unicode-safe device and apply the PNAS
## final-size scaling automatically, so the individual plotting scripts can
## keep calling ggsave() unchanged.
ggsave <- function(filename, plot = ggplot2::last_plot(), width, height, ...) {
  args <- list(...)
  if (grepl("\\.pdf$", filename, ignore.case = TRUE) && is.null(args[["device"]])) {
    args[["device"]] <- pdf_device
  }
  if (is.null(args[["scale"]]) && !missing(width) && !missing(height)) {
    args[["scale"]] <- pnas_scale(width, height)
  }
  do.call(ggplot2::ggsave,
          c(list(filename = filename, plot = plot, width = width, height = height),
            args))
}

## ── Root path ──────────────────────────────────────────────────────────────
## Each script should set `root` before sourcing this file.
## `root` = the top-level replication folder (parent of A_Demo/).
## If not set, try to detect from RStudio or working directory.
if (!exists("root")) {
  root <- tryCatch({
    script_path <- dirname(rstudioapi::getSourceEditorContext()$path)
    dirname(dirname(dirname(script_path)))  # go up from A_Demo/democode/figures/ to root
  }, error = function(e) {
    # Fallback: assume working directory is root
    getwd()
  })
}

table_dir  <- file.path(root, "A_Demo/democode/output/tables")
fig_dir    <- file.path(root, "A_Demo/democode/output/figures")
data_dir   <- file.path(root, "A_Demo/demodata")
shp_dir    <- file.path(root, "A_Demo/demodata/shapefiles")

if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)


## ── X-axis setup ──────────────────────────────────────────────────────────
x_breaks <- -10:10
x_labels <- c("-10+", as.character(-9:-1), "0",
              as.character(1:9), "10+")


## ── Group labels ──────────────────────────────────────────────────────────
group_labels <- c(
  allpop        = "Overdose death counts",
  allpop_t      = "IHS transform of overdose death counts",
  allpop_rate   = "Overdose death rates",
  allpop_rate_t = "IHS transform of overdose death rates",
  male          = "Male",             male_t        = "Male",
  female        = "Female",           female_t      = "Female",
  white         = "White",            white_t       = "White",
  nonwhite      = "Nonwhite",         nonwhite_t    = "Nonwhite",
  leh           = "Low-education individuals",
  leh_t         = "Low-education individuals",
  hh            = "High-education individuals",
  hh_t          = "High-education individuals",
  male_rate     = "Male",             male_rate_t   = "Male",
  female_rate   = "Female",           female_rate_t = "Female",
  white_rate    = "White",            white_rate_t  = "White",
  nonwhite_rate = "Nonwhite",         nonwhite_rate_t = "Nonwhite",
  leh_rate      = "Low-education individuals",
  leh_rate_t    = "Low-education individuals",
  hh_rate       = "High-education individuals",
  hh_rate_t     = "High-education individuals",
  pdistressed1  = "Distressed counties",
  pdistressed0  = "Non-distressed counties",
  plowmdi1      = "Counties with lower median household income",
  plowmdi0      = "Counties with higher median household income",
  p_tdistressed1 = "Distressed counties",
  p_tdistressed0 = "Non-distressed counties",
  p_tlowmdi1    = "Counties with lower median household income",
  p_tlowmdi0    = "Counties with higher median household income",
  p_ratedistressed1 = "Distressed counties",
  p_ratedistressed0 = "Non-distressed counties",
  p_ratelowmdi1 = "Counties with lower median household income",
  p_ratelowmdi0 = "Counties with higher median household income",
  p_rate_tdistressed1 = "Distressed counties",
  p_rate_tdistressed0 = "Non-distressed counties",
  p_rate_tlowmdi1 = "Counties with lower median household income",
  p_rate_tlowmdi0 = "Counties with higher median household income"
)


## ── Main plotting function ────────────────────────────────────────────────

## Per-label horizontal alignment: nudge only the binned endpoints (-10+, 10+)
## outward so they clear their neighbours (-9, 9); all interior labels stay
## centered. The vector follows x_breaks order (ascending, -10 .. 10).
x_hjust <- rep(0.5, length(x_breaks))
x_hjust[1] <- 0.75                       # "-10+" -> shift left toward the edge
x_hjust[length(x_hjust)] <- 0.25         # "10+"  -> shift right toward the edge

plot_panel <- function(d, varname, panel_label, ylim = NULL) {

  dd <- d %>% filter(variable == varname)
  group_name <- ifelse(varname %in% names(group_labels),
                       group_labels[[varname]], varname)

  ## Long subgroup labels (the income splits) are wider than a half-panel, so
  ## nudge them gently left of centre; otherwise they crowd the right edge on
  ## the right-hand panel. Short labels stay centred.
  sub_hjust <- if (nchar(group_name) > 40) 0.43 else 0.5

  has_ci <- any(!is.na(dd$ci_lo) & !is.na(dd$ci_hi))

  p <- ggplot(dd, aes(x = period, y = coeff)) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "firebrick3", linewidth = 0.4, alpha = 0.7) +
    geom_vline(xintercept = -0.5, linetype = "dashed",
               color = "firebrick3", linewidth = 0.4, alpha = 0.7) +
    {if (has_ci) geom_linerange(aes(ymin = ci_lo, ymax = ci_hi),
                   color = "grey20", linewidth = 0.45)} +
    geom_point(shape = 21, fill = "white", color = "black",
               size = 1.5, stroke = 0.6) +
    scale_x_continuous(breaks = x_breaks, labels = x_labels,
                       limits = c(-10, 10), expand = expansion(mult = 0.03)) +
    labs(title    = panel_label,
         subtitle = paste0("\u25cb  ", group_name),
         x = "Year relative to flood", y = "Coefficients") +
    theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 9.5,
                                   hjust = 0.5, margin = margin(b = 4)),
      plot.subtitle = element_text(face = "bold", size = 9,
                                   hjust = sub_hjust, margin = margin(t = 0, b = 6)),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3,
                                        linetype = "dotted"),
      panel.border = element_rect(color = "grey55", linetype = "dotted",
                                  fill = NA, linewidth = 0.6),
      ## axis titles kept clearly below the subtitle so the panel descriptor
      ## stays the more prominent label even when it is long (e.g. income
      ## subgroups). Hierarchy: title 11 > subtitle 9 > axis title 8 > ticks
      ## (8 is the floor that still prints >= 6 pt after the PNAS down-scale).
      axis.title.x = element_text(face = "bold", size = 8,
                                  margin = margin(t = 3)),
      axis.title.y = element_text(face = "bold", size = 8,
                                  margin = margin(r = 2)),
      axis.text.x  = element_text(size = 7.8, color = "grey20", hjust = x_hjust),
      axis.text.y  = element_text(size = 8, color = "grey20"),
      axis.ticks        = element_line(color = "grey40", linewidth = 0.4),
      axis.ticks.length = unit(0.08, "cm"),
      plot.margin = margin(5, 6, 5, 5)
    )

  if (!is.null(ylim)) p <- p + coord_cartesian(ylim = ylim)
  p
}


## ── Period mapping ────────────────────────────────────────────────────────

period_map <- c(
  F_FH_10 = -10, F_FH_9 = -9, F_FH_8 = -8, F_FH_7 = -7,
  F_FH_6 = -6, F_FH_5 = -5, F_FH_4 = -4, F_FH_3 = -3,
  F_FH_2 = -2, F_FH_1 = -1,
  L_FH_0 = 0, L_FH_1 = 1, L_FH_2 = 2, L_FH_3 = 3,
  L_FH_4 = 4, L_FH_5 = 5, L_FH_6 = 6, L_FH_7 = 7,
  L_FH_8 = 8, L_FH_9 = 9, L_FH_10 = 10,
  # NOAA uses F_D / L_D
  F_D_10 = -10, F_D_9 = -9, F_D_8 = -8, F_D_7 = -7,
  F_D_6 = -6, F_D_5 = -5, F_D_4 = -4, F_D_3 = -3,
  F_D_2 = -2, F_D_1 = -1,
  L_D_0 = 0, L_D_1 = 1, L_D_2 = 2, L_D_3 = 3,
  L_D_4 = 4, L_D_5 = 5, L_D_6 = 6, L_D_7 = 7,
  L_D_8 = 8, L_D_9 = 9, L_D_10 = 10
)


## ── Parse esttab CSV ──────────────────────────────────────────────────────

parse_esttab <- function(filepath, col_names) {
  if (!file.exists(filepath)) {
    cat("  MISSING:", filepath, "\n")
    return(NULL)
  }
  lines <- readLines(filepath)
  lines <- lines[nchar(trimws(lines)) > 0]

  all_rows <- list()
  i <- 2
  while (i <= length(lines)) {
    ln <- lines[i]
    parts <- gsub('"', '', str_split(ln, ",")[[1]])
    varname <- trimws(parts[1])

    if (grepl("^(F_FH_|L_FH_|F_D_|L_D_)", varname)) {
      coef_vals <- suppressWarnings(as.numeric(parts[-1]))
      se_vals <- rep(NA_real_, length(col_names))

      if (i + 1 <= length(lines)) {
        se_line <- lines[i + 1]
        se_parts <- gsub('"', '', str_split(se_line, ",")[[1]])
        if (trimws(se_parts[1]) == "") {
          se_vals <- suppressWarnings(as.numeric(se_parts[-1]))
          i <- i + 1
        }
      }

      all_rows[[length(all_rows) + 1]] <- list(
        varname = varname,
        coef_vals = coef_vals[1:length(col_names)],
        se_vals = se_vals[1:length(col_names)])
    }
    i <- i + 1
  }

  result <- list()
  for (row in all_rows) {
    period <- period_map[row$varname]
    if (is.na(period)) next
    for (j in seq_along(col_names)) {
      b <- row$coef_vals[j]
      s <- row$se_vals[j]
      result[[length(result) + 1]] <- data.frame(
        variable = col_names[j], period = period, coeff = b, se = s,
        ci_lo = ifelse(!is.na(s) & s > 0, b - 1.96 * s, NA_real_),
        ci_hi = ifelse(!is.na(s) & s > 0, b + 1.96 * s, NA_real_),
        stringsAsFactors = FALSE)
    }
  }
  if (length(result) == 0) return(NULL)
  do.call(rbind, result)
}

cat("Plot helpers loaded. root =", root, "\n")
