# ==============================================================================
# ASSESSMENT 4: Flood Risk Mapping using Remote Sensing
# ==============================================================================
# Region    : Tamil Nadu (South India) - Central and Southern regions
# Datasets  : USGS SRTM 30m DEM (5 tiles) + MODIS MCD12Q1 LULC (NASA AppEEARS)
# Approach  : Multi-criteria flood risk assessment
# Techniques: DEM analysis, land cover classification, spatial risk modeling
# Output    : Flood risk zonation map, 3D terrain visualization, risk statistics
# ==============================================================================

# Load required libraries
required_packages <- c(
  "terra", "sf", "tmap", "leaflet", "leaflet.extras", "ggplot2", "plotly",
  "dplyr", "RColorBrewer", "scales", "htmlwidgets", "raster"
)

options(timeout = max(1000, getOption("timeout")))
new_pkgs <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) {
  install.packages(new_pkgs, dependencies = TRUE, repos = "http://cran.us.r-project.org")
}
invisible(lapply(required_packages, library, character.only = TRUE))

# File paths and study area
DEM_TILE_1 <- "d:/Data/Github/SheldonC2005/Academics/Data Visualization/Data/n09_e078_1arc_v3.tif"
DEM_TILE_2 <- "d:/Data/Github/SheldonC2005/Academics/Data Visualization/Data/n09_e079_1arc_v3.tif"
DEM_TILE_3 <- "d:/Data/Github/SheldonC2005/Academics/Data Visualization/Data/n10_e077_1arc_v3.tif"
DEM_TILE_4 <- "d:/Data/Github/SheldonC2005/Academics/Data Visualization/Data/n10_e078_1arc_v3.tif"
DEM_TILE_5 <- "d:/Data/Github/SheldonC2005/Academics/Data Visualization/Data/n11_e077_1arc_v3.tif"
LULC_FILE  <- "d:/Data/Github/SheldonC2005/Academics/Data Visualization/Data/tamil_nadu_lulc_2023.tif"

# Study area extent for Tamil Nadu (9-12°N, 77-79°E)
STUDY_BBOX <- terra::ext(77.0, 79.5, 9.0, 12.0)

# Load and merge DEM tiles
dem_paths <- c(DEM_TILE_1, DEM_TILE_2, DEM_TILE_3, DEM_TILE_4, DEM_TILE_5)

dem_tile_1 <- terra::rast(DEM_TILE_1)
dem_tile_2 <- terra::rast(DEM_TILE_2)
dem_tile_3 <- terra::rast(DEM_TILE_3)
dem_tile_4 <- terra::rast(DEM_TILE_4)
dem_tile_5 <- terra::rast(DEM_TILE_5)

dem_merged <- terra::merge(dem_tile_1, dem_tile_2, dem_tile_3, dem_tile_4, dem_tile_5)
dem_cropped <- terra::crop(dem_merged, STUDY_BBOX)
terra::crs(dem_cropped) <- "EPSG:4326"
dem_cropped[dem_cropped < 0] <- 0

# Reduce resolution if raster is too large (>50M cells)
if (terra::ncell(dem_cropped) > 50e6) {
  dem_cropped <- terra::aggregate(dem_cropped, fact = 2, fun = "mean")
}

# Load and align LULC data
lulc_raw <- terra::rast(LULC_FILE)
lulc_cropped <- terra::crop(lulc_raw, STUDY_BBOX)
lulc_aligned <- terra::resample(lulc_cropped, dem_cropped, method = "near")

# MODIS MCD12Q1 IGBP classification
lulc_classes <- data.frame(
  value = 1:17,
  label = c(
    "Evergreen Needleleaf Forest", "Evergreen Broadleaf Forest",
    "Deciduous Needleleaf Forest", "Deciduous Broadleaf Forest",
    "Mixed Forest", "Closed Shrubland", "Open Shrubland",
    "Woody Savanna", "Savanna", "Grassland",
    "Permanent Wetland", "Cropland", "Urban/Built-up",
    "Cropland/Natural Mosaic", "Snow/Ice", "Barren", "Water Bodies"
  )
)

# Derive river network from DEM using flow accumulation
slope_raster <- terra::terrain(dem_cropped, v = "slope", unit = "degrees")
flow_dir <- terra::terrain(dem_cropped, v = "flowdir")

# Build flow accumulation proxy using focal window
focal_sum <- terra::focal(dem_cropped, w = matrix(1, 9, 9), fun = "sum", na.rm = TRUE)
flow_acc_proxy <- focal_sum - (dem_cropped * 81)

# Normalize to 0–1 range
acc_min <- terra::global(flow_acc_proxy, "min", na.rm = TRUE)[[1]]
acc_max <- terra::global(flow_acc_proxy, "max", na.rm = TRUE)[[1]]
flow_acc_norm <- (flow_acc_proxy - acc_min) / (acc_max - acc_min)

# Identify rivers as top 1% of flow accumulation
river_threshold <- terra::global(flow_acc_norm, fun = function(x) quantile(x, 0.99, na.rm = TRUE))[[1]]
rivers_raster <- terra::ifel(flow_acc_norm >= river_threshold, 1, NA)

# Create proximity risk zones based on flow accumulation
flow_acc_risk <- terra::classify(
  flow_acc_norm,
  rcl = matrix(c(
    0.00, 0.90,  0,   # Low accumulation: Low risk
    0.90, 0.95,  1,   # Moderate accumulation: Moderate risk
    0.95, 0.98,  2,   # High accumulation: High risk
    0.98, 1.00,  3    # Very high accumulation: Very high risk
  ), ncol = 3, byrow = TRUE)
)
proximity_risk <- flow_acc_risk

# Multi-criteria flood risk assessment
# Factor 1: Elevation risk (0-3)
elev_vals <- terra::values(dem_cropped, na.rm = TRUE)
q15 <- quantile(elev_vals, 0.15, na.rm = TRUE)
q35 <- quantile(elev_vals, 0.35, na.rm = TRUE)
q60 <- quantile(elev_vals, 0.60, na.rm = TRUE)

elev_risk <- terra::classify(
  dem_cropped,
  rcl = matrix(c(
    -Inf,  q15,  3,    # Very low elevation → High risk
     q15,  q35,  2,    # Low elevation → Medium risk
     q35,  q60,  1,    # Mid elevation → Low risk
     q60,  Inf,  0     # High elevation → Safe
  ), ncol = 3, byrow = TRUE)
)

# Factor 2: River proximity risk (0-3)
prox_risk <- proximity_risk

# Factor 3: Slope risk (0-2)
slope_risk <- terra::classify(
  slope_raster,
  rcl = matrix(c(
    0,  2,  2,   # Very flat → High risk
    2,  5,  1,   # Gentle → Medium risk
    5, Inf, 0    # Steep → Low risk
  ), ncol = 3, byrow = TRUE)
)

# Factor 4: Land cover risk (0-3)
lulc_risk <- terra::classify(
  lulc_aligned,
  rcl = matrix(c(
     1,  1,  0,    # Evergreen Needleleaf Forest → Safe
     2,  2,  0,    # Evergreen Broadleaf Forest → Safe
     3,  3,  0,    # Deciduous Needleleaf → Safe
     4,  4,  0,    # Deciduous Broadleaf → Safe
     5,  5,  0,    # Mixed Forest → Safe
     6,  6,  0,    # Closed Shrubland → Safe
     7,  7,  0,    # Open Shrubland → Safe
     8,  8,  1,    # Woody Savanna → Low
     9,  9,  1,    # Savanna → Low
    10, 10,  1,    # Grassland → Low
    11, 11,  2,    # Permanent Wetland → Medium
    12, 12,  2,    # Cropland → Medium
    13, 13,  2,    # Urban/Built-up → Medium
    14, 14,  2,    # Cropland/Natural Mosaic → Medium
    15, 15,  0,    # Snow/Ice → Safe
    16, 16,  0,    # Barren → Safe
    17, 17,  3     # Water Bodies → High
  ), ncol = 3, byrow = TRUE)
)

# Weighted composite risk score (Overlay Analysis)
flood_risk_score <- (elev_risk * 0.40) + (prox_risk * 0.35) + (slope_risk * 0.15) + (lulc_risk * 0.10)

# Classify into final flood zones
flood_zones <- terra::classify(
  flood_risk_score,
  rcl = matrix(c(
    0.00, 0.75, 1,   # Safe
    0.75, 1.50, 2,   # Low Risk
    1.50, 2.25, 3,   # Medium Risk
    2.25, 3.01, 4    # High Risk
  ), ncol = 3, byrow = TRUE)
)

levels(flood_zones) <- data.frame(
  value = 1:4,
  label = c("Safe", "Low Risk", "Medium Risk", "High Risk")
)

# Calculate flood risk zone statistics
zone_vals <- terra::values(flood_zones, na.rm = TRUE)
zone_table <- table(zone_vals)

risk_stats <- data.frame(
  Code = as.integer(names(zone_table)),
  Zone = c("Safe", "Low Risk", "Medium Risk", "High Risk")[as.integer(names(zone_table))],
  Pixels = as.integer(zone_table),
  stringsAsFactors = FALSE
) %>%
  dplyr::mutate(
    Area_km2 = round(Pixels * (terra::res(flood_zones)[1] * 111)^2, 0),
    Percentage = round(Pixels / sum(Pixels) * 100, 1)
  ) %>%
  dplyr::arrange(Code)

# Zone distribution bar chart
zone_colors <- c("Safe" = "#2ecc71", "Low Risk" = "#f1c40f",
                 "Medium Risk" = "#e67e22", "High Risk" = "#e74c3c")

risk_bar_plot <- ggplot2::ggplot(
  risk_stats,
  ggplot2::aes(x = forcats::fct_reorder(Zone, Code), y = Area_km2, fill = Zone)
) +
  ggplot2::geom_col(width = 0.6, color = "white") +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(Percentage, "%\n", scales::comma(Area_km2), " km²")),
    vjust = -0.3, size = 3.5, fontface = "bold"
  ) +
  ggplot2::scale_fill_manual(values = zone_colors) +
  ggplot2::scale_y_continuous(labels = scales::comma, 
                               expand = ggplot2::expansion(mult = c(0, 0.15))) +
  ggplot2::labs(
    title = "Flood Risk Zone Distribution — Tamil Nadu",
    subtitle = "Central & Southern Tamil Nadu regions",
    x = "Risk Zone", y = "Area (km²)"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    legend.position = "none"
  )

# Interactive flood risk zonation map (Leaflet)
flood_raster_compat <- raster::raster(flood_zones)

zone_pal <- leaflet::colorFactor(
  palette = c("#2ecc71", "#f1c40f", "#e67e22", "#e74c3c"),
  levels = c(1, 2, 3, 4),
  na.color = "transparent"
)

flood_risk_map <- leaflet::leaflet() %>%
  leaflet::addProviderTiles("CartoDB.Positron", group = "Light") %>%
  leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  leaflet::addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
  leaflet::addRasterImage(
    x = flood_raster_compat,
    colors = zone_pal,
    opacity = 0.72,
    group = "Flood Risk Zones"
  ) %>%
  leaflet::addLegend(
    position = "bottomright",
    colors = c("#2ecc71", "#f1c40f", "#e67e22", "#e74c3c"),
    labels = c("Safe", "Low Risk", "Medium Risk", "High Risk"),
    title = "Flood Risk Zone",
    opacity = 0.9
  ) %>%
  leaflet::addLayersControl(
    baseGroups = c("Light", "Satellite", "Dark"),
    overlayGroups = c("Flood Risk Zones"),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  ) %>%
  leaflet::addScaleBar(position = "bottomleft") %>%
  leaflet::addMiniMap(toggleDisplay = TRUE, minimized = FALSE)

# Land cover overlay map (Leaflet)
lulc_raster_compat <- raster::raster(lulc_aligned)

lulc_palette_colors <- c(
  "#1a6e1a", "#006400", "#228B22", "#32CD32", "#90EE90",  # Forests
  "#DEB887", "#F5DEB3",                                    # Shrublands
  "#9ACD32", "#ADFF2F", "#FFFF00",                         # Savannas/Grass
  "#4169E1",                                               # Wetland
  "#FFA500", "#FF4500", "#DAA520",                         # Cropland/Urban
  "#FFFFFF", "#808080", "#0000CD"                          # Snow/Barren/Water
)

lulc_pal <- leaflet::colorNumeric(
  palette = lulc_palette_colors,
  domain = 1:17,
  na.color = "transparent"
)

lulc_overlay_map <- leaflet::leaflet() %>%
  leaflet::addProviderTiles("CartoDB.Positron") %>%
  leaflet::addRasterImage(
    x = lulc_raster_compat,
    colors = lulc_pal,
    opacity = 0.75,
    group = "Land Cover (MODIS)"
  ) %>%
  leaflet::addLegend(
    position = "bottomright",
    colors = lulc_palette_colors[c(2, 9, 11, 12, 13, 17)],
    labels = c("Forest", "Savanna/Grass", "Wetland", "Cropland", "Urban", "Water"),
    title = "MODIS Land Cover",
    opacity = 0.9
  ) %>%
  leaflet::addLayersControl(
    overlayGroups = "Land Cover (MODIS)",
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )

# 3D terrain visualization (Plotly)
dem_agg <- terra::aggregate(dem_cropped, fact = 30, fun = "mean", na.rm = TRUE)
risk_agg <- terra::aggregate(flood_risk_score, fact = 30, fun = "mean", na.rm = TRUE)
risk_agg <- terra::resample(risk_agg, dem_agg, method = "bilinear")

dem_matrix <- terra::as.matrix(dem_agg, wide = TRUE)
risk_matrix <- terra::as.matrix(risk_agg, wide = TRUE)

ext_agg <- terra::ext(dem_agg)
lon_axis <- seq(ext_agg$xmin, ext_agg$xmax, length.out = ncol(dem_matrix))
lat_axis <- seq(ext_agg$ymin, ext_agg$ymax, length.out = nrow(dem_matrix))

terrain_3d <- plotly::plot_ly() %>%
  plotly::add_surface(
    x = lon_axis,
    y = lat_axis,
    z = dem_matrix,
    surfacecolor = risk_matrix,
    cmin = 0,
    cmax = 3,
    colorscale = list(
      list(0.00, "#2ecc71"),   # Safe → Green
      list(0.25, "#f1c40f"),   # Low risk → Yellow
      list(0.50, "#e67e22"),   # Medium risk → Orange
      list(0.75, "#e74c3c"),   # High risk → Red
      list(1.00, "#922b21")    # Very High → Dark Red
    ),
    colorbar = list(
      title = list(text = "Flood Risk\nScore", font = list(size = 12)),
      tickvals = c(0, 0.75, 1.5, 2.25, 3),
      ticktext = c("Safe", "Low", "Medium", "High", "Very High"),
      thickness = 18
    ),
    contours = list(
      z = list(
        show = TRUE,
        usecolormap = TRUE,
        highlightcolor = "#1a1a1a",
        project = list(z = FALSE)
      )
    ),
    opacity = 0.95,
    name = "Terrain"
  ) %>%
  plotly::layout(
    title = list(
      text = paste0(
        "<b>3D Flood Risk Terrain — Tamil Nadu</b><br>",
        "<sup>Central & Southern Tamil Nadu — Elevation colored by Flood Risk</sup>"
      ),
      font = list(size = 14),
      x = 0.05
    ),
    scene = list(
      xaxis = list(title = "Longitude (°E)", showgrid = TRUE, 
                   gridcolor = "rgba(255,255,255,0.3)"),
      yaxis = list(title = "Latitude (°N)", showgrid = TRUE, 
                   gridcolor = "rgba(255,255,255,0.3)"),
      zaxis = list(title = "Elevation (m)", showgrid = TRUE, 
                   gridcolor = "rgba(255,255,255,0.3)"),
      camera = list(
        eye = list(x = 1.8, y = -1.6, z = 1.0),
        up = list(x = 0, y = 0, z = 1)
      ),
      aspectmode = "manual",
      aspectratio = list(x = 1.5, y = 1.0, z = 0.35),
      bgcolor = "rgba(240,240,245,1)"
    ),
    margin = list(l = 0, r = 0, b = 0, t = 60),
    paper_bgcolor = "rgba(240,240,245,1)"
  )

# Display visualizations
print(flood_risk_map)
print(terrain_3d)
print(risk_bar_plot)
print(lulc_overlay_map)