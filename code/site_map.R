# Figure_1_site_map.R
# Figure 1: map of the eight tidal marsh study sites (25°N–55°N, 140°W–50°W),
# formatted to Global Change Biology figure requirements:
#   * >= 300 dpi, PNG
#   * no title or figure number on the figure, and no white border
#   * saved at its final printed size, so font sizes are true point sizes
#   * single panel, so no panel letters are needed
# Caption (in the manuscript) must include: "Map lines delineate study areas and do not
# necessarily depict accepted national boundaries."

# ---- Packages ----
required_packages <- c("readxl", "dplyr", "ggplot2", "sf",
                       "rnaturalearth", "rnaturalearthdata", "ggrepel", "here")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# ---- GCB figure settings (same as the main analysis script) ----
gcb_dpi  <- 300
gcb_full <- 180   # mm, full page width (check Wiley's current figure-size guidance)
gcb_base <- 8     # base font size (pt) at printed size

# ---- User settings ----
excel_path <- here::here("data", "Site characteristics final.xlsx")  # the analysis script reads "Site characteristics final.xlsx"; use the same file if coordinates differ
lat_col  <- "Latitude"
lon_col  <- "Longitude"
name_col <- "Site_ID"

# Jitter (in degrees). ~0.1° ≈ 10–11 km latitude near mid-latitudes
jitter_width  <- 0.1
jitter_height <- 0.1

# Offset radius for EXACT duplicate coordinates (degrees)
duplicate_offset_radius <- 0.12

# Map extent
xlim <- c(-140, -50)
ylim <- c(25, 55)

# ---- Load data ----
sites <- readxl::read_excel(excel_path) |>
  dplyr::select(
    site_id = tidyselect::all_of(name_col),
    lat     = tidyselect::all_of(lat_col),
    lon     = tidyselect::all_of(lon_col)
  ) |>
  dplyr::filter(!is.na(lat), !is.na(lon))

# ---- Handle EXACT duplicates by spreading them around a small circle ----
sites <- sites |>
  dplyr::group_by(lon, lat) |>
  dplyr::mutate(
    dup_n = dplyr::n(),
    dup_idx = dplyr::row_number()
  ) |>
  dplyr::ungroup()

adjust_for_lon_scale <- function(deg_lat) {
  # scale longitude offset by cos(latitude) so the circle looks round on the map
  cos(pi * deg_lat / 180)
}

sites <- sites |>
  dplyr::mutate(
    angle = ifelse(dup_n > 1, (dup_idx - 1) * (2 * pi / dup_n), 0),
    lon_offset = ifelse(
      dup_n > 1,
      duplicate_offset_radius * cos(angle) / pmax(adjust_for_lon_scale(lat), 0.2),
      0
    ),
    lat_offset = ifelse(
      dup_n > 1,
      duplicate_offset_radius * sin(angle),
      0
    ),
    lon_adj = lon + lon_offset,
    lat_adj = lat + lat_offset
  )

# ---- Add a small jitter for near-duplicates (applied to ALL, reproducible) ----
set.seed(42)
sites <- sites |>
  dplyr::mutate(
    lon_plot = jitter(lon_adj, amount = jitter_width),
    lat_plot = jitter(lat_adj, amount = jitter_height)
  )

# ---- Basemap ----
na <- rnaturalearth::ne_countries(continent = "North America", returnclass = "sf")

# ---- Degree labels (e.g., 120°W, 40°N) ----
deg_lbl <- function(x, hemi_pos = c("E","N"), hemi_neg = c("W","S"), axis = "x") {
  if (axis == "x") paste0(abs(x), "°", ifelse(x < 0, hemi_neg[1], hemi_pos[1]))
  else             paste0(abs(x), "°", ifelse(x < 0, hemi_neg[2], hemi_pos[2]))
}

# ---- Plot ----
# Point and label sizes are scaled for the printed figure (ggplot sizes are in mm; .pt converts from pt)
p <- ggplot() +
  geom_sf(data = na, fill = "grey80", color = "white", linewidth = 0.2) +
  
  # White halo under points for contrast
  geom_point(
    data = sites,
    aes(x = lon_plot, y = lat_plot),
    size = 2.4, color = "white"
  ) +
  # Foreground points
  geom_point(
    data = sites,
    aes(x = lon_plot, y = lat_plot),
    size = 1.6, color = "royalblue1"
  ) +
  
  # Repelled labels using the jittered/offset coordinates
  ggrepel::geom_text_repel(
    data = sites,
    aes(x = lon_plot, y = lat_plot, label = site_id),
    size = gcb_base / ggplot2::.pt,
    box.padding = 0.3,
    point.padding = 0.2,
    segment.size = 0.3,
    min.segment.length = 0,
    max.overlaps = Inf,
    seed = 42
  ) +
  
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  scale_x_continuous(
    breaks = seq(xlim[1], xlim[2], by = 10),
    labels = function(x) deg_lbl(x, axis = "x")
  ) +
  scale_y_continuous(
    breaks = seq(ylim[1], ylim[2], by = 5),
    labels = function(y) deg_lbl(y, axis = "y")
  ) +
  labs(title = NULL, x = NULL, y = NULL) +   # no title or figure number on the figure
  theme_minimal(base_size = gcb_base) +
  theme(
    axis.text        = element_text(size = gcb_base - 1, color = "black"),
    panel.grid.major = element_line(linewidth = 0.2, linetype = "dotted", color = "grey70"),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.margin      = margin(2, 4, 2, 2)    # minimal margin: no white border
  )

# ---- Save at printed size ----
# Height follows the map's aspect ratio (geographic coordinates: 1 / cos(mid-latitude)),
# so there is no empty band above or below the map.
axis_space_mm <- 10
panel_w <- gcb_full - axis_space_mm
panel_h <- panel_w * diff(ylim) / diff(xlim) / cos(mean(ylim) * pi / 180)
map_h   <- panel_h + axis_space_mm

dir.create(here::here("figures"), showWarnings = FALSE)
ggsave(here::here("figures", "Figure_1.png"), p,
       width = gcb_full, height = map_h, units = "mm",
       dpi = gcb_dpi, bg = "white")
print(p)
