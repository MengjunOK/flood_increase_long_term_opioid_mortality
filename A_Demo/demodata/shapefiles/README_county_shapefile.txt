County boundary shapefile NOT included (exceeds GitHub's 100 MB file limit).

The maps (plot_maps.R) require the county-boundary files:
    US_Continental_Counties.shp / .shx / .dbf / .prj / .cpg

These are public U.S. Census Bureau TIGER/Line shapefiles. Download the county
shapefile and place the files in this folder under the name "US_Continental_Counties.*"
(the layer must include a county-FIPS field matching CNTYIDFP00 used in plot_maps.R).

Source: U.S. Census Bureau, TIGER/Line Shapefiles
https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
(See DataSources.pdf, item 10.)

The state shapefile (tl_2023_us_state.*) is included.
