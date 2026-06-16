## ============================================================================
##  Maps: Main Appalachian Flood Map + Robustness Maps
##
##  Map 1: Main map — Treatment/Control/Urban/Dropped counties
##  Map 2: NOAA definition — Alternative flood definition
##  Map 3: Urban + Rural — Including urban counties
##
##  Requires: sf, tmap, tmaptools, haven, dplyr
## ============================================================================

## Set root before sourcing helpers (edit if running standalone)
# root <- "/path/to/CodeShare_severefloods_opioidoverdosemortality_April2025"
source(file.path(root, "A_Demo/democode/figures/_plot_helpers.R"))

library(sf)
library(tmap)
library(tmaptools)
library(haven)


## ── Load shared data ─────────────────────────────────────────────────────────

county <- st_read(file.path(shp_dir, "US_Continental_Counties.shp"))
states <- st_read(file.path(shp_dir, "tl_2023_us_state.shp"))
demo   <- read_dta(file.path(data_dir, "floodopioid_demo.dta"))

## Keep one row per county (year == 1999)
demo <- demo %>% filter(year == 1999)
demo$county_fips_str <- sprintf("%05d", demo$county_fips)


## ── Shared map builder ──────────────────────────────────────────────────────

build_map <- function(spatialcounty, year_col, group_col, note_text,
                      palette = NULL) {

  states_t <- st_transform(states, st_crs(spatialcounty))
  bbox <- st_bbox(spatialcounty)
  x_pad <- (bbox["xmax"] - bbox["xmin"]) * 0.06
  y_pad <- (bbox["ymax"] - bbox["ymin"]) * 0.03
  ## Note sits just below the legend (fraction of the data y-range, from bottom).
  note_y <- as.numeric(bbox["ymin"]) +
            0.845 * as.numeric(bbox["ymax"] - bbox["ymin"])
  bbox_expanded <- st_bbox(c(
    xmin = as.numeric(bbox["xmin"] - x_pad),
    ymin = as.numeric(bbox["ymin"] - y_pad),
    xmax = as.numeric(bbox["xmax"] + x_pad),
    ymax = as.numeric(bbox["ymax"] + y_pad)
  ), crs = st_crs(spatialcounty))
  bbox_poly <- st_as_sfc(bbox_expanded)

  states_crop <- st_intersection(states_t, bbox_poly)
  appa_fips <- c("01","13","21","24","28","36","37","39","42","45","47","51","54")
  states_appa <- states_crop %>% filter(STATEFP %in% appa_fips)

  states_appa$state_label <- toupper(states_appa$NAME)
  sl <- st_centroid(states_appa)
  lc <- st_coordinates(sl)
  cd <- data.frame(STATEFP = sl$STATEFP, x = lc[,1], y = lc[,2])

  ld <- data.frame(
    label = c("MISSISSIPPI","ALABAMA","GEORGIA","SOUTH\nCAROLINA",
              "TENNESSEE","KENTUCKY","VIRGINIA","WEST\nVIRGINIA",
              "OHIO","PENNSYLVANIA","NEW YORK","NORTH\nCAROLINA","MARYLAND"),
    STATEFP = c("28","01","13","45","47","21","51","54","39","42","36","37","24"),
    stringsAsFactors = FALSE)
  ld <- merge(ld, cd, by = "STATEFP", all.x = TRUE)

  adj <- list("28"=c(-50000,-80000),"01"=c(-50000,-120000),"13"=c(50000,-120000),
              "21"=c(-180000,-30000),"47"=c(-200000,-60000),"39"=c(-100000,30000),
              "36"=c(50000,70000),"42"=c(100000,-90000),"51"=c(180000,20000),
              "54"=c(180000,-10000),"37"=c(200000,-30000),"45"=c(70000,-100000),
              "24"=c(25000,30000))
  for (f in names(adj)) {
    ld$x[ld$STATEFP==f] <- ld$x[ld$STATEFP==f] + adj[[f]][1]
    ld$y[ld$STATEFP==f] <- ld$y[ld$STATEFP==f] + adj[[f]][2]
  }
  labels_sf <- st_as_sf(ld, coords = c("x","y"), crs = st_crs(spatialcounty))

  wvc <- cd[cd$STATEFP=="54",]; wvl <- ld[ld$STATEFP=="54",]
  leader <- st_sf(geometry = st_sfc(st_linestring(matrix(
    c(wvl$x,wvl$y,wvc$x,wvc$y), ncol=2, byrow=TRUE)),
    crs = st_crs(spatialcounty)))

  tmap_mode("plot")

  the_map <-
    tm_shape(states_appa, bbox = bbox_poly) +
    tm_polygons(fill = "#e8f5e9", col = "#4a4a4a", lwd = 2.0) +
    tm_shape(labels_sf) +
    tm_text("label", size = 0.5, col = "grey35", fontface = "bold") +
    tm_shape(leader) +
    tm_lines(col = "grey35", lwd = 1.0) +
    tm_shape(spatialcounty) +
    tm_polygons(
      fill = "cat_factor",
      fill.scale = tm_scale_categorical(
        values = if (!is.null(palette)) palette else
          c("white","gray","#A63603","#FDD49E")[seq_along(levels(spatialcounty$cat_factor))],
        value.na = "white"),
      fill.legend = tm_legend(title = "Legend",
        position = tm_pos_in("left","top"), frame = TRUE, bg.color = "white"),
      col = "black", lwd = 0.8) +
    tm_shape(states_appa) +
    tm_borders(col = "#4a4a4a", lwd = 2.0) +
    tm_shape(spatialcounty[!is.na(spatialcounty[[group_col]]),]) +
    tm_text(year_col, size = 0.5, col = "white", fontface = "bold") +
    tm_shape(st_sf(label = note_text,
      geometry = st_sfc(st_point(c(730000, note_y)),
                        crs = st_crs(spatialcounty)))) +
    tm_text("label", size = 0.7, col = "grey25", fontface = "italic",
            just = "left") +
    ## Tight page margins; small top inner margin holds the legend at the frame
    ## top while nudging the map+note down, leaving a gap below the legend.
    tm_layout(outer.margins = c(0.004, 0.008, 0.004, 0.008),
              inner.margins = c(0.02, 0.02, 0.05, 0.02))

  return(the_map)
}

save_map <- function(m, name) {
  ## PDF via pdf_device (quartz/cairo) so fonts are embedded -- the default
  ## pdf() device used by tmap_save() does NOT embed fonts, which PNAS rejects.
  ## Canvas sized near the map's own (near-square) aspect so it fills the frame
  ## with minimal white; 7 x 7.8 in = 17.8 x 19.8 cm, within PNAS limits.
  mw <- 7.0; mh <- 7.8
  pdf_device(file.path(fig_dir, paste0(name, ".pdf")), width = mw, height = mh)
  print(m); dev.off()
  pf <- file.path(fig_dir, paste0(name, ".png"))
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(pf, width = mw, height = mh, units="in", res=300)
  } else {
    png(pf, width = mw, height = mh, units="in", res=300, type="quartz")
  }
  print(m); dev.off()
}


## ============================================================================
##  MAP 1: Main Appalachian Flood Map (FEMA definition, rural only)
## ============================================================================

cat("Creating Map 1: Main Appalachian map...\n")

d0 <- demo %>%
  mutate(
    category = case_when(
      ruralregion != 1 ~ 1L,
      !is.na(all_fefl_fl1h_1st_year) & all_fefl_fl1h_1st_year <= 1999 ~ 2L,
      !is.na(fefl_fl1h_1st_year) & fefl_fl1h_1st_year >= 2000 &
        (is.na(all_fefl_fl1h_1st_year) | all_fefl_fl1h_1st_year > 1999) ~ 3L,
      TRUE ~ 4L
    ),
    floodhasyear = ifelse(category == 3, fefl_fl1h_1st_year, NA),
    floodgroup = ifelse(category == 3, 1, NA)
  ) %>%
  select(county_fips_str, category, floodhasyear, floodgroup)

sc0 <- merge(county, d0, by.x = "CNTYIDFP00", by.y = "county_fips_str")
sc0$floodhasyear2 <- substr(as.character(sc0$floodhasyear), 3, 4)
sc0$cat_factor <- factor(sc0$category, levels = c(1,2,3,4),
  labels = c("Urban counties",
             "Dropped rural counties (flooded in 1996\u20131999)",
             "Treatment counties (flooded in 2000\u20132017)",
             "Control counties (not flooded in 2000\u20132017)"))

map0 <- build_map(sc0, "floodhasyear2", "floodgroup",
  "Note: Number on a treatment county indicates the year\n(last two digits, e.g., 03 = 2003) of its first severe flood.")

save_map(map0, "map_appalachian_main")
cat("  Saved: map_appalachian_main\n")


## ============================================================================
##  MAP 2: NOAA Deaths Definition (rural only)
## ============================================================================

cat("Creating Map 2: NOAA definition...\n")

d1 <- demo %>%
  mutate(
    category = case_when(
      ruralregion != 1 ~ 1L,
      !is.na(all_fl1d_1st_year) & all_fl1d_1st_year <= 1999 ~ 2L,
      !is.na(fl1d_1st_year) & fl1d_1st_year >= 2000 &
        (is.na(all_fl1d_1st_year) | all_fl1d_1st_year > 1999) ~ 3L,
      TRUE ~ 4L
    ),
    floodhasyear = ifelse(category == 3, fl1d_1st_year, NA),
    floodgroup = ifelse(category == 3, 1, NA)
  ) %>%
  select(county_fips_str, category, floodhasyear, floodgroup)

sc1 <- merge(county, d1, by.x = "CNTYIDFP00", by.y = "county_fips_str")
sc1$floodhasyear2 <- substr(as.character(sc1$floodhasyear), 3, 4)
sc1$cat_factor <- factor(sc1$category, levels = c(1,2,3,4),
  labels = c("Urban counties",
             "Dropped rural counties (flooded in 1996\u20131999)",
             "Treatment counties (flooded in 2000\u20132017)",
             "Control counties (not flooded in 2000\u20132017)"))

map1 <- build_map(sc1, "floodhasyear2", "floodgroup",
  "Note: Number on a treatment county indicates the year\n(last two digits, e.g., 03 = 2003) of its first NOAA flood death.")

save_map(map1, "FigS7-1_noaa_map")
cat("  Saved: FigS7-1_noaa_map\n")


## ============================================================================
##  MAP 3: Both Rural and Urban Counties (FEMA definition)
## ============================================================================

cat("Creating Map 3: Urban + Rural...\n")

d2 <- demo %>%
  mutate(
    category = case_when(
      !is.na(all_fefl_fl1h_1st_year) & all_fefl_fl1h_1st_year <= 1999 ~ 2L,
      !is.na(fefl_fl1h_1st_year) & fefl_fl1h_1st_year >= 2000 &
        (is.na(all_fefl_fl1h_1st_year) | all_fefl_fl1h_1st_year > 1999) ~ 3L,
      TRUE ~ 4L
    ),
    floodhasyear = ifelse(category == 3, fefl_fl1h_1st_year, NA),
    floodgroup = ifelse(category == 3, 1, NA)
  ) %>%
  select(county_fips_str, category, floodhasyear, floodgroup)

sc2 <- merge(county, d2, by.x = "CNTYIDFP00", by.y = "county_fips_str")
sc2$floodhasyear2 <- substr(as.character(sc2$floodhasyear), 3, 4)
sc2$cat_factor <- factor(sc2$category, levels = c(2,3,4),
  labels = c("Dropped counties (flooded in 1996\u20131999)",
             "Treatment counties (flooded in 2000\u20132017)",
             "Control counties (not flooded in 2000\u20132017)"))

map2 <- build_map(sc2, "floodhasyear2", "floodgroup",
  "Note: Number on a treatment county indicates the year\n(last two digits, e.g., 03 = 2003) of its first severe flood.",
  palette = c("gray", "#A63603", "#FDD49E"))

save_map(map2, "FigS7-5_urbanrural_map")
cat("  Saved: FigS7-5_urbanrural_map\n")

cat("\nAll maps saved to:", fig_dir, "\n")
