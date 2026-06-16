## ============================================================================
##  Robustness Check Event Study Figures (Figures S5-2 to S5-7)
##
##  Reads esttab CSVs from 03_robustness_checks.do
##  Produces 5 figures (each 4-panel, 2x2)
##
##  Uses shared helpers from _plot_helpers.R
## ============================================================================

## Set root before sourcing helpers (edit if running standalone)
# root <- "/path/to/CodeShare_severefloods_opioidoverdosemortality_April2025"
source(file.path(root, "A_Demo/democode/figures/_plot_helpers.R"))


## ── Helper: make one 4-panel robustness figure ──────────────────────────────

col_names <- c("allpop", "allpop_t", "allpop_rate", "allpop_rate_t")

make_4panel <- function(df, figname, ylim_A = NULL, ylim_B = NULL,
                        ylim_C = NULL, ylim_D = NULL) {
  pA <- plot_panel(df, "allpop",        "Panel A", ylim = ylim_A)
  pB <- plot_panel(df, "allpop_t",      "Panel B", ylim = ylim_B)
  pC <- plot_panel(df, "allpop_rate",   "Panel C", ylim = ylim_C)
  pD <- plot_panel(df, "allpop_rate_t", "Panel D", ylim = ylim_D)

  fig <- (pA | pB) / (pC | pD)

  ggsave(file.path(fig_dir, paste0(figname, ".pdf")), fig, width = 9, height = 7.5)
  ggsave(file.path(fig_dir, paste0(figname, ".png")), fig, width = 9, height = 7.5, dpi = 300)
  cat("  Saved:", figname, "\n")
}


## ============================================================================
##  Figure S5-2: NOAA Deaths Definition
## ============================================================================

cat("Figure S5-2: NOAA flood definition...\n")
df <- parse_esttab(file.path(table_dir, "esttab_robust_noaa.csv"), col_names)
if (!is.null(df)) make_4panel(df, "FigS7-2_noaa",
  ylim_A = c(-2, 6), ylim_B = c(-0.5, 1), ylim_C = c(-5, 15), ylim_D = c(-1, 1))


## ============================================================================
##  Figure S5-3: Prescription Drug Deaths
## ============================================================================

cat("Figure S5-3: Prescription drug deaths...\n")
df <- parse_esttab(file.path(table_dir, "esttab_robust_predrug.csv"), col_names)
if (!is.null(df)) make_4panel(df, "FigS7-3_predrug",
  ylim_A = c(-2, 6), ylim_B = c(-0.5, 1), ylim_C = c(-5, 15), ylim_D = c(-1, 1))


## ============================================================================
##  Figure S5-4: Overall Drug-Related Deaths
## ============================================================================

cat("Figure S5-4: All drug deaths...\n")
df <- parse_esttab(file.path(table_dir, "esttab_robust_alldrug.csv"), col_names)
if (!is.null(df)) make_4panel(df, "FigS7-4_alldrug",
  ylim_A = c(-2, 6), ylim_B = c(-0.5, 1), ylim_C = c(-5, 15), ylim_D = c(-1, 1))


## ============================================================================
##  Figure S5-6: Urban + Rural
## ============================================================================

cat("Figure S5-6: Urban + Rural...\n")
df <- parse_esttab(file.path(table_dir, "esttab_robust_urbanrural.csv"), col_names)
if (!is.null(df)) make_4panel(df, "FigS7-6_urbanrural",
  ylim_A = c(-2, 6), ylim_B = c(-0.5, 1), ylim_C = c(-5, 15), ylim_D = c(-1, 1))


## ============================================================================
##  Figure S5-7: Exclude Alabama & Mississippi
## ============================================================================

cat("Figure S5-7: Exclude AL & MS...\n")
df <- parse_esttab(file.path(table_dir, "esttab_robust_dropALMS.csv"), col_names)
if (!is.null(df)) make_4panel(df, "FigS7-7_dropALMS",
  ylim_A = c(-2, 6), ylim_B = c(-0.5, 1), ylim_C = c(-5, 15), ylim_D = c(-1, 1))


cat("\nAll robustness figures saved to:", fig_dir, "\n")
