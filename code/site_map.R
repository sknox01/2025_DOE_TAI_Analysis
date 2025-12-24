# plot_sites_north_america_zoom_distinguishable_largefont_coords.R
# Map site locations (25°N–55°N, -140°–-50°) with readable labels, separation for nearby points,
# larger text, and visible lat/long coordinates.

# ---- Packages ----
required_packages <- c("readxl", "dplyr", "ggplot2", "sf",
                       "rnaturalearth", "rnaturalearthdata", "ggrepel")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# ---- User settings ----
excel_path <- "data/Site characteristics.xlsx"  # adjust if needed
lat_col <- "Latitude"
lon_col <- "Longitude"
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
p <- ggplot() +
  geom_sf(data = na, fill = "grey80", color = "white", linewidth = 0.2) +
  
  # White halo under points for contrast
  geom_point(
    data = sites,
    aes(x = lon_plot, y = lat_plot),
    size = 4.5, color = "white"
  ) +
  # Foreground points
  geom_point(
    data = sites,
    aes(x = lon_plot, y = lat_plot),
    size = 3, color = "royalblue1"
  ) +
  
  # Repelled labels using the jittered/offset coordinates
  ggrepel::geom_text_repel(
    data = sites,
    aes(x = lon_plot, y = lat_plot, label = site_id),
    size = 5,
    box.padding = 0.4,
    point.padding = 0.3,
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
  theme_minimal(base_size = 20) +
  labs(
    x = "", y = ""
  ) +
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    plot.subtitle = element_text(size = 18),
    axis.text = element_text(size = 16, color = "black"),
    axis.title = element_text(size = 18, face = "bold"),
    panel.grid.major = element_line(linewidth = 0.3, linetype = "dotted", color = "grey70")
  )

# ---- Save & show ----
ggsave("figures/site_map.png", p, width = 12, height = 9, dpi = 300)
print(p)
