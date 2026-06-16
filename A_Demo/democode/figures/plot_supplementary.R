## ============================================================================
##  Supplementary Event Study Figures (Figures S4-1 to S4-6)
##
##  Six figures showing subpop and subsample results in
##  count, rate, and IHS(rate) transformations
##
##  Reads: esttab_subpop_count.csv, esttab_subpop_rate.csv,
##         esttab_subpop_ihsrate.csv, esttab_subsample_count.csv,
##         esttab_subsample_rate.csv, esttab_subsample_ihsrate.csv
## ============================================================================

## Set root before sourcing helpers (edit if running standalone)
# root <- "/path/to/CodeShare_severefloods_opioidoverdosemortality_April2025"
source(file.path(root, "A_Demo/democode/figures/_plot_helpers.R"))


## ============================================================================
##  Figure S4-1: Subpop Count — 6 panels (3x2)
## ============================================================================

cat("Creating Figure S4-1 (subpop count)...\n")

df <- parse_esttab(
  file.path(table_dir, "esttab_subpop_count.csv"),
  c("leh", "hh", "male", "female", "white", "nonwhite")
)

if (!is.null(df)) {
  pA <- plot_panel(df, "leh",      "Panel A", ylim = c(-1, 4))
  pB <- plot_panel(df, "hh",       "Panel B", ylim = c(-1, 4))
  pC <- plot_panel(df, "male",     "Panel C", ylim = c(-1, 3))
  pD <- plot_panel(df, "female",   "Panel D", ylim = c(-1, 3))
  pE <- plot_panel(df, "white",    "Panel E", ylim = c(-2, 4))
  pF <- plot_panel(df, "nonwhite", "Panel F", ylim = c(-1, 1))

  fig <- (pA | pB) / (pC | pD) / (pE | pF) +
    plot_annotation(title = "Overdose death counts",
                    theme = theme(plot.title = element_text(face = "bold", size = 16,
                                                            hjust = 0.5)))

  ggsave(file.path(fig_dir, "FigS4-1_subpop_count.pdf"), fig, width = 7.5, height = 10)
  ggsave(file.path(fig_dir, "FigS4-1_subpop_count.png"), fig, width = 7.5, height = 10, dpi = 300)
  cat("  Saved: FigS4-1_subpop_count\n")
}


## ============================================================================
##  Figure S4-2: Subpop Rate — 6 panels (3x2)
## ============================================================================

cat("Creating Figure S4-2 (subpop rate)...\n")

df <- parse_esttab(
  file.path(table_dir, "esttab_subpop_rate.csv"),
  c("leh_rate", "hh_rate", "male_rate", "female_rate", "white_rate", "nonwhite_rate")
)

if (!is.null(df)) {
  pA <- plot_panel(df, "leh_rate",      "Panel A", ylim = c(-10, 30))
  pB <- plot_panel(df, "hh_rate",       "Panel B", ylim = c(-10, 30))
  pC <- plot_panel(df, "male_rate",     "Panel C", ylim = c(-10, 15))
  pD <- plot_panel(df, "female_rate",   "Panel D", ylim = c(-10, 15))
  pE <- plot_panel(df, "white_rate",    "Panel E", ylim = c(-5, 15))
  pF <- plot_panel(df, "nonwhite_rate", "Panel F", ylim = c(-30, 20))

  fig <- (pA | pB) / (pC | pD) / (pE | pF) +
    plot_annotation(title = "Overdose death rates",
                    theme = theme(plot.title = element_text(face = "bold", size = 16,
                                                            hjust = 0.5)))

  ggsave(file.path(fig_dir, "FigS4-2_subpop_rate.pdf"), fig, width = 7.5, height = 10)
  ggsave(file.path(fig_dir, "FigS4-2_subpop_rate.png"), fig, width = 7.5, height = 10, dpi = 300)
  cat("  Saved: FigS4-2_subpop_rate\n")
}


## ============================================================================
##  Figure S4-3: Subpop IHS(Rate) — 6 panels (3x2)
## ============================================================================

cat("Creating Figure S4-3 (subpop IHS rate)...\n")

df <- parse_esttab(
  file.path(table_dir, "esttab_subpop_ihsrate.csv"),
  c("leh_rate_t", "hh_rate_t", "male_rate_t", "female_rate_t", "white_rate_t", "nonwhite_rate_t")
)

if (!is.null(df)) {
  pA <- plot_panel(df, "leh_rate_t",      "Panel A", ylim = c(-1, 1.5))
  pB <- plot_panel(df, "hh_rate_t",       "Panel B", ylim = c(-1, 1.5))
  pC <- plot_panel(df, "male_rate_t",     "Panel C", ylim = c(-1, 1.5))
  pD <- plot_panel(df, "female_rate_t",   "Panel D", ylim = c(-1, 1.5))
  pE <- plot_panel(df, "white_rate_t",    "Panel E", ylim = c(-1, 1.5))
  pF <- plot_panel(df, "nonwhite_rate_t", "Panel F", ylim = c(-1, 1.5))

  fig <- (pA | pB) / (pC | pD) / (pE | pF) +
    plot_annotation(title = "IHS transform of overdose death rates",
                    theme = theme(plot.title = element_text(face = "bold", size = 16,
                                                            hjust = 0.5)))

  ggsave(file.path(fig_dir, "FigS4-3_subpop_ihsrate.pdf"), fig, width = 7.5, height = 10)
  ggsave(file.path(fig_dir, "FigS4-3_subpop_ihsrate.png"), fig, width = 7.5, height = 10, dpi = 300)
  cat("  Saved: FigS4-3_subpop_ihsrate\n")
}


## ============================================================================
##  Figure S4-4: Subsample Count — 4 panels (2x2)
## ============================================================================

cat("Creating Figure S4-4 (subsample count)...\n")

df <- parse_esttab(
  file.path(table_dir, "esttab_subsample_count.csv"),
  c("pdistressed1", "pdistressed0", "plowmdi1", "plowmdi0")
)

if (!is.null(df)) {
  pA <- plot_panel(df, "pdistressed1", "Panel A", ylim = c(-5, 10))
  pB <- plot_panel(df, "pdistressed0", "Panel B", ylim = c(-5, 10))
  pC <- plot_panel(df, "plowmdi1",     "Panel C", ylim = c(-4, 8))
  pD <- plot_panel(df, "plowmdi0",     "Panel D", ylim = c(-4, 8))

  fig <- (pA | pB) / (pC | pD) +
    plot_annotation(title = "Overdose death counts",
                    theme = theme(plot.title = element_text(face = "bold", size = 16,
                                                            hjust = 0.5)))

  ggsave(file.path(fig_dir, "FigS4-4_subsample_count.pdf"), fig, width = 9, height = 7.5)
  ggsave(file.path(fig_dir, "FigS4-4_subsample_count.png"), fig, width = 9, height = 7.5, dpi = 300)
  cat("  Saved: FigS4-4_subsample_count\n")
}


## ============================================================================
##  Figure S4-5: Subsample Rate — 4 panels (2x2)
## ============================================================================

cat("Creating Figure S4-5 (subsample rate)...\n")

df <- parse_esttab(
  file.path(table_dir, "esttab_subsample_rate.csv"),
  c("p_ratedistressed1", "p_ratedistressed0", "p_ratelowmdi1", "p_ratelowmdi0")
)

if (!is.null(df)) {
  pA <- plot_panel(df, "p_ratedistressed1", "Panel A", ylim = c(-10, 30))
  pB <- plot_panel(df, "p_ratedistressed0", "Panel B", ylim = c(-10, 30))
  pC <- plot_panel(df, "p_ratelowmdi1",     "Panel C", ylim = c(-10, 20))
  pD <- plot_panel(df, "p_ratelowmdi0",     "Panel D", ylim = c(-10, 20))

  fig <- (pA | pB) / (pC | pD) +
    plot_annotation(title = "Overdose death rates",
                    theme = theme(plot.title = element_text(face = "bold", size = 16,
                                                            hjust = 0.5)))

  ggsave(file.path(fig_dir, "FigS4-5_subsample_rate.pdf"), fig, width = 9, height = 7.5)
  ggsave(file.path(fig_dir, "FigS4-5_subsample_rate.png"), fig, width = 9, height = 7.5, dpi = 300)
  cat("  Saved: FigS4-5_subsample_rate\n")
}


## ============================================================================
##  Figure S4-6: Subsample IHS(Rate) — 4 panels (2x2)
## ============================================================================

cat("Creating Figure S4-6 (subsample IHS rate)...\n")

df <- parse_esttab(
  file.path(table_dir, "esttab_subsample_ihsrate.csv"),
  c("p_rate_tdistressed1", "p_rate_tdistressed0", "p_rate_tlowmdi1", "p_rate_tlowmdi0")
)

if (!is.null(df)) {
  pA <- plot_panel(df, "p_rate_tdistressed1", "Panel A", ylim = c(-2, 2))
  pB <- plot_panel(df, "p_rate_tdistressed0", "Panel B", ylim = c(-2, 2))
  pC <- plot_panel(df, "p_rate_tlowmdi1",     "Panel C", ylim = c(-1, 2))
  pD <- plot_panel(df, "p_rate_tlowmdi0",     "Panel D", ylim = c(-1, 2))

  fig <- (pA | pB) / (pC | pD) +
    plot_annotation(title = "IHS transform of overdose death rates",
                    theme = theme(plot.title = element_text(face = "bold", size = 16,
                                                            hjust = 0.5)))

  ggsave(file.path(fig_dir, "FigS4-6_subsample_ihsrate.pdf"), fig, width = 9, height = 7.5)
  ggsave(file.path(fig_dir, "FigS4-6_subsample_ihsrate.png"), fig, width = 9, height = 7.5, dpi = 300)
  cat("  Saved: FigS4-6_subsample_ihsrate\n")
}


cat("\nAll supplementary figures saved to:", fig_dir, "\n")
