## ============================================================================
##  Migration, Composition, and Demographic Controls Figures
##
##  Reads: coefdata_migration_robustness.csv (from 04_migration_robustness.do)
##
##  Figure 1: Migration (3-panel) — pop_allpop, log_pop, rnetmig
##  Figure 2: Composition Stability (7-panel) — 6 demog shares + edupct_leh
##  Figure 3: Robustness Main with demog controls (4-panel)
##  Figure 4: Robustness Subgroup with demog controls (6-panel)
##  Figure 5: Robustness Subsample with demog controls (4-panel)
## ============================================================================

## Set root before sourcing helpers (edit if running standalone)
# root <- "/path/to/CodeShare_severefloods_opioidoverdosemortality_April2025"
source(file.path(root, "A_Demo/democode/figures/_plot_helpers.R"))


## ── Read data ────────────────────────────────────────────────────────────────

df <- read_csv(file.path(table_dir, "coefdata_migration_robustness.csv"),
               show_col_types = FALSE)


## ── Plotting function with F-test subtitle ───────────────────────────────────

plot_panel_ftest <- function(d, varname, panel_label, var_title,
                             y_label = "Coefficient", ylim = NULL,
                             drop_endpoints = FALSE) {

  dd <- d %>% filter(variable == varname)
  if (drop_endpoints) dd <- dd %>% filter(!period %in% c(-10, 10))

  jf_pre_p  <- dd$joint_f_pre_p[1]
  jf_post_p <- dd$joint_f_post_p[1]

  subtitle_txt <- sprintf(
    "Joint F-test:  Pre-trend p = %.3f,  Post-treatment p = %.3f",
    jf_pre_p, jf_post_p
  )

  p <- ggplot(dd, aes(x = period, y = coeff)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick3",
               linewidth = 0.4, alpha = 0.7) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "firebrick3",
               linewidth = 0.4, alpha = 0.7) +
    geom_linerange(aes(ymin = ci_lo, ymax = ci_hi),
                   color = "grey15", linewidth = 0.5) +
    geom_point(shape = 21, fill = "white", color = "black",
               size = 1.8, stroke = 0.75) +
    scale_x_continuous(breaks = x_breaks, labels = x_labels,
                       limits = c(-10, 10)) +
    labs(
      title    = paste0(panel_label, ": ", var_title),
      subtitle = subtitle_txt,
      x        = "Years Relative to Flood",
      y        = y_label
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title         = element_text(face = "bold", size = 11, hjust = 0.5,
                                        margin = margin(b = 4)),
      plot.subtitle      = element_text(size = 7.5, face = "bold", hjust = 0.5,
                                        color = "black",
                                        margin = margin(t = 0, b = 6)),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3,
                                        linetype = "dotted"),
      panel.border       = element_rect(color = "grey55", linetype = "dotted",
                                        fill = NA, linewidth = 0.6),
      axis.title.x       = element_text(face = "bold", size = 10,
                                        margin = margin(t = 4)),
      axis.title.y       = element_text(face = "bold", size = 10,
                                        margin = margin(r = 4)),
      axis.text.x        = element_text(size = 8, face = "bold", color = "grey20"),
      axis.text.y        = element_text(size = 9, face = "bold", color = "grey20"),
      axis.ticks         = element_line(color = "grey40", linewidth = 0.4),
      axis.ticks.length  = unit(0.12, "cm"),
      plot.margin        = margin(8, 10, 8, 8)
    )

  if (!is.null(ylim)) p <- p + coord_cartesian(ylim = ylim)
  p
}


## ============================================================================
##  FIGURE 1: Migration (3-panel vertical)
## ============================================================================

cat("Creating migration figure...\n")

df_mig <- df %>% filter(part == "migration")

pA <- plot_panel_ftest(df_mig, "pop_allpop", "Panel A", "Total Population",
                       ylim = c(-2000, 2000))
pB <- plot_panel_ftest(df_mig, "log_pop",    "Panel B", "Log(Total Population)",
                       ylim = c(-0.05, 0.05))
pC <- plot_panel_ftest(df_mig, "rnetmig",    "Panel C", "Net Migration Rate (per 1,000)",
                       ylim = c(-10, 10))

fig1 <- pA / pB / pC

ggsave(file.path(fig_dir, "FigS8-1_migration.pdf"), fig1, width = 7.5, height = 10)
ggsave(file.path(fig_dir, "FigS8-1_migration.png"), fig1, width = 7.5, height = 10, dpi = 300)
cat("  Saved: FigS8-1_migration\n")


## ============================================================================
##  FIGURE 2: Composition Stability (7-panel: 3x2 + 1)
## ============================================================================

cat("Creating composition stability figure...\n")

df_comp <- df %>% filter(part %in% c("composition", "composition_edu"))

pA2 <- plot_panel_ftest(df_comp, "pop_white",   "Panel A", "% White Population",
                        ylim = c(-2, 2))
pB2 <- plot_panel_ftest(df_comp, "pop_male",    "Panel B", "% Male Population",
                        ylim = c(-2, 2))
pC2 <- plot_panel_ftest(df_comp, "pop_less25",  "Panel C", "% Population Under 25",
                        ylim = c(-2, 2))
pD2 <- plot_panel_ftest(df_comp, "pop_2544",    "Panel D", "% Population 25-44",
                        ylim = c(-2, 2))
pE2 <- plot_panel_ftest(df_comp, "pop_4564",    "Panel E", "% Population 45-64",
                        ylim = c(-2, 2))
pF2 <- plot_panel_ftest(df_comp, "pop_over65",  "Panel F", "% Population 65+",
                        ylim = c(-2, 2))
pG2 <- plot_panel_ftest(df_comp, "edupct_leh",  "Panel G",
                        "% Low Education Population (2008-2017)",
                        ylim = c(-5, 5), drop_endpoints = TRUE)

fig2 <- (pA2 | pB2) / (pC2 | pD2) / (pE2 | pF2) / (pG2 | plot_spacer())

ggsave(file.path(fig_dir, "FigS8-2_composition.pdf"), fig2,
       width = 7.5, height = 12)
ggsave(file.path(fig_dir, "FigS8-2_composition.png"), fig2,
       width = 7.5, height = 12, dpi = 300)
cat("  Saved: FigS8-2_composition\n")


## ============================================================================
##  FIGURE 3: Robustness Main with demog controls (4-panel, 2x2)
## ============================================================================

cat("Creating robustness main with demographic controls...\n")

df_main <- df %>% filter(part == "robust_main")

p3A <- plot_panel(df_main, "allpop",        "Panel A", ylim = c(-2, 6))
p3B <- plot_panel(df_main, "allpop_t",      "Panel B", ylim = c(-0.5, 1))
p3C <- plot_panel(df_main, "allpop_rate",   "Panel C", ylim = c(-5, 15))
p3D <- plot_panel(df_main, "allpop_rate_t", "Panel D", ylim = c(-1, 1))

fig3 <- (p3A | p3B) / (p3C | p3D)

ggsave(file.path(fig_dir, "FigS8-3_demog_main.pdf"), fig3, width = 9, height = 7.5)
ggsave(file.path(fig_dir, "FigS8-3_demog_main.png"), fig3, width = 9, height = 7.5, dpi = 300)
cat("  Saved: FigS8-3_demog_main\n")


## ============================================================================
##  FIGURE 4: Robustness Subgroup (6-panel, 3x2)
## ============================================================================

cat("Creating robustness subgroup figure...\n")

df_sub <- df %>% filter(part == "robust_subgroup")

p4A <- plot_panel(df_sub, "leh_t",      "Panel A", ylim = c(-0.5, 1))
p4B <- plot_panel(df_sub, "hh_t",       "Panel B", ylim = c(-0.5, 1))
p4C <- plot_panel(df_sub, "male_t",     "Panel C", ylim = c(-0.5, 1))
p4D <- plot_panel(df_sub, "female_t",   "Panel D", ylim = c(-0.5, 1))
p4E <- plot_panel(df_sub, "white_t",    "Panel E", ylim = c(-0.5, 1))
p4F <- plot_panel(df_sub, "nonwhite_t", "Panel F", ylim = c(-0.5, 1))

fig4 <- (p4A | p4B) / (p4C | p4D) / (p4E | p4F)

ggsave(file.path(fig_dir, "FigS8-4_demog_subgroup.pdf"), fig4, width = 7.5, height = 10)
ggsave(file.path(fig_dir, "FigS8-4_demog_subgroup.png"), fig4, width = 7.5, height = 10, dpi = 300)
cat("  Saved: FigS8-4_demog_subgroup\n")


## ============================================================================
##  FIGURE 5: Robustness Subsample (4-panel, 2x2)
## ============================================================================

cat("Creating robustness subsample figure...\n")

df_ss <- df %>% filter(part == "robust_subsample")

p5A <- plot_panel(df_ss, "p_tdistressed1", "Panel A", ylim = c(-1.0, 1.5))
p5B <- plot_panel(df_ss, "p_tdistressed0", "Panel B", ylim = c(-1.0, 1.5))
p5C <- plot_panel(df_ss, "p_tlowmdi1",     "Panel C", ylim = c(-1.0, 1.5))
p5D <- plot_panel(df_ss, "p_tlowmdi0",     "Panel D", ylim = c(-1.0, 1.5))

fig5 <- (p5A | p5B) / (p5C | p5D)

ggsave(file.path(fig_dir, "FigS8-5_demog_subsample.pdf"), fig5, width = 9, height = 7.5)
ggsave(file.path(fig_dir, "FigS8-5_demog_subsample.png"), fig5, width = 9, height = 7.5, dpi = 300)
cat("  Saved: FigS8-5_demog_subsample\n")

cat("\nAll migration/composition/demographic figures saved to:", fig_dir, "\n")
