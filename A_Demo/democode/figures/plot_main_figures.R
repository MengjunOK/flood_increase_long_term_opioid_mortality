## ============================================================================
##  Main Text Event Study Figures (Figures 2, 3, 4)
##
##  Reads: esttab_main.csv, esttab_subpop_ihscount.csv,
##         esttab_subsample_ihscount.csv
##  Produces: fig2_main, fig3_subpop_ihscount, fig4_subsample_ihscount
## ============================================================================

## Set root before sourcing helpers (edit if running standalone)
# root <- "/path/to/CodeShare_severefloods_opioidoverdosemortality_April2025"
source(file.path(root, "A_Demo/democode/figures/_plot_helpers.R"))


## ============================================================================
##  FIGURE 2: Main — allpop, 4 outcomes (2x2)
## ============================================================================

cat("Creating Figure 2 (main)...\n")

df_main <- parse_esttab(
  file.path(table_dir, "esttab_main.csv"),
  c("allpop", "allpop_t", "allpop_rate", "allpop_rate_t")
)

if (!is.null(df_main)) {
  fig2A <- plot_panel(df_main, "allpop",        "Panel A", ylim = c(-2, 6))
  fig2B <- plot_panel(df_main, "allpop_t",      "Panel B", ylim = c(-0.5, 1))
  fig2C <- plot_panel(df_main, "allpop_rate",   "Panel C", ylim = c(-5, 15))
  fig2D <- plot_panel(df_main, "allpop_rate_t", "Panel D", ylim = c(-1, 1))

  fig2 <- (fig2A | fig2B) / (fig2C | fig2D)

  ggsave(file.path(fig_dir, "fig2_main.pdf"), fig2, width = 9, height = 7.5)
  ggsave(file.path(fig_dir, "fig2_main.png"), fig2, width = 9, height = 7.5, dpi = 300)
  cat("  Saved: fig2_main\n")
}


## ============================================================================
##  FIGURE 3: Subpop IHS(Count) — 6 panels (3x2)
## ============================================================================

cat("Creating Figure 3 (subpop IHS count)...\n")

df_sub <- parse_esttab(
  file.path(table_dir, "esttab_subpop_ihscount.csv"),
  c("leh_t", "hh_t", "male_t", "female_t", "white_t", "nonwhite_t")
)

if (!is.null(df_sub)) {
  fig3A <- plot_panel(df_sub, "leh_t",      "Panel A", ylim = c(-0.5, 1))
  fig3B <- plot_panel(df_sub, "hh_t",       "Panel B", ylim = c(-0.5, 1))
  fig3C <- plot_panel(df_sub, "male_t",     "Panel C", ylim = c(-0.5, 1))
  fig3D <- plot_panel(df_sub, "female_t",   "Panel D", ylim = c(-0.5, 1))
  fig3E <- plot_panel(df_sub, "white_t",    "Panel E", ylim = c(-0.5, 1))
  fig3F <- plot_panel(df_sub, "nonwhite_t", "Panel F", ylim = c(-0.5, 1))

  fig3 <- (fig3A | fig3B) / (fig3C | fig3D) / (fig3E | fig3F)

  ggsave(file.path(fig_dir, "fig3_subpop_ihscount.pdf"), fig3, width = 7.5, height = 10)
  ggsave(file.path(fig_dir, "fig3_subpop_ihscount.png"), fig3, width = 7.5, height = 10, dpi = 300)
  cat("  Saved: fig3_subpop_ihscount\n")
}


## ============================================================================
##  FIGURE 4: Subsample IHS(Count) — 4 panels (2x2)
## ============================================================================

cat("Creating Figure 4 (subsample IHS count)...\n")

df_ss <- parse_esttab(
  file.path(table_dir, "esttab_subsample_ihscount.csv"),
  c("p_tdistressed1", "p_tdistressed0", "p_tlowmdi1", "p_tlowmdi0")
)

if (!is.null(df_ss)) {
  fig4A <- plot_panel(df_ss, "p_tdistressed1", "Panel A", ylim = c(-1.0, 1.5))
  fig4B <- plot_panel(df_ss, "p_tdistressed0", "Panel B", ylim = c(-1.0, 1.5))
  fig4C <- plot_panel(df_ss, "p_tlowmdi1",     "Panel C", ylim = c(-1.0, 1.5))
  fig4D <- plot_panel(df_ss, "p_tlowmdi0",     "Panel D", ylim = c(-1.0, 1.5))

  fig4 <- (fig4A | fig4B) / (fig4C | fig4D)

  ggsave(file.path(fig_dir, "fig4_subsample_ihscount.pdf"), fig4, width = 9, height = 7.5)
  ggsave(file.path(fig_dir, "fig4_subsample_ihscount.png"), fig4, width = 9, height = 7.5, dpi = 300)
  cat("  Saved: fig4_subsample_ihscount\n")
}


cat("\nAll main figures saved to:", fig_dir, "\n")
