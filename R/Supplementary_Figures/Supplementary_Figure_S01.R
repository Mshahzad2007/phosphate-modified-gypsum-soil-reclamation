# =============================================================================
# SUPPLEMENTARY FIG. S1 — STUDY LOCATION AND USDA SOIL TEXTURE
# Nature Communications-style artwork; portrait Word manuscript
#
# (a) China provincial map, study site at 109.9 E, 40.1 N
# (b) USDA texture triangle: clay 28.5%, silt 55.0%, sand 16.5%
#
# Keeps original site coordinates, USDA classification, and composition.
# The regional palette/legend is replaced by one highlighted province to
# concentrate attention on the experimental site. The triangle is converted
# from base R to grid graphics so lines and labels remain vector elements
# in the exported PDF.
# =============================================================================

# 1. PACKAGES -----------------------------------------------------------------
packages <- c(
  "sf", "ggplot2", "dplyr", "rnaturalearth", "rnaturalearthdata",
  "soiltexture", "ggplotify", "cowplot", "patchwork", "grid"
)

missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

# High-resolution administrative polygons used by ne_states().
# If rnaturalearthhires is already installed, it will not be reinstalled.
if (!requireNamespace("rnaturalearthhires", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes", repos = "https://cloud.r-project.org")
  }
  remotes::install_github("ropensci/rnaturalearthhires", upgrade = "never")
}

suppressPackageStartupMessages({
  library(sf)
  library(ggplot2)
  library(dplyr)
  library(rnaturalearth)
  library(soiltexture)
  library(ggplotify)
  library(cowplot)
  library(patchwork)
  library(grid)
})

# 2. OUTPUT AND GLOBAL SETTINGS -----------------------------------------------
out_dir <- file.path("outputs", "Fig_S1_NC_Portrait")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

word_width_cm  <- 15.5
word_height_cm <- 14.5
# Double-column journal vector export, matching the Word aspect ratio.
nc_width_cm  <- 18.0
nc_height_cm <- nc_width_cm * word_height_cm / word_width_cm

font_family <- "Arial"
location_red <- "#C1272D"
map_base_col <- "#E4E7EA"
map_hi_col   <- "#8EAFC7"

# Original site coordinates and grain-size percentages are unchanged.
site <- data.frame(lon = 109.9, lat = 40.1)
texture <- data.frame(CLAY = 28.5, SILT = 55.0, SAND = 16.5)
stopifnot(abs(rowSums(texture)[1] - 100) < 1e-8)

# 3. PANEL (a) — CHINA MAP -----------------------------------------------------
# Use WGS84 and zero-length-safe longitude/latitude labels.
china <- tryCatch(
  rnaturalearth::ne_states(country = "China", returnclass = "sf"),
  error = function(e) stop(
    "Could not obtain China administrative boundaries. Check ",
    "rnaturalearthhires and internet access. Original error: ",
    conditionMessage(e), call. = FALSE
  )
)
china <- sf::st_transform(china, 4326)

site_sf <- sf::st_as_sf(site, coords = c("lon", "lat"), crs = 4326)

# Highlight the province containing the supplied study-site point instead
# of relying on alternative province naming conventions.
site_hits <- sf::st_intersects(site_sf, china)[[1]]
china$location_province <- "Other provinces"
if (length(site_hits)) {
  china$location_province[site_hits] <- "Study-site province"
} else {
  warning(
    "The site point did not intersect a province boundary. ",
    "Showing all provinces in grey; site coordinate is unchanged."
  )
}
china$location_province <- factor(
  china$location_province,
  levels = c("Other provinces", "Study-site province")
)

panelA <- ggplot() +
  geom_sf(
    data = china,
    aes(fill = location_province),
    colour = "white", linewidth = 0.18, show.legend = FALSE
  ) +
  geom_point(
    data = site,
    aes(x = lon, y = lat),
    shape = 21, fill = location_red, colour = "white",
    stroke = 0.75, size = 3.8
  ) +
  # One clear label, away from the red site marker.
  annotate(
    "segment", x = site$lon, y = site$lat,
    xend = 114.0, yend = 43.6,
    colour = location_red, linewidth = 0.36
  ) +
  annotate(
    "text", x = 114.4, y = 44.2,
    label = "Study site", colour = "black", hjust = 0,
    size = 2.9, family = font_family
  ) +
  scale_fill_manual(
    values = c(
      "Other provinces" = map_base_col,
      "Study-site province" = map_hi_col
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = seq(80, 140, 10),
    labels = function(x) {
      if (length(x) == 0L) character(0) else paste0(x, "\u00b0E")
    }
  ) +
  scale_y_continuous(
    breaks = seq(20, 50, 10),
    labels = function(y) {
      if (length(y) == 0L) character(0) else paste0(y, "\u00b0N")
    }
  ) +
  coord_sf(
    crs = sf::st_crs(4326),
    default_crs = sf::st_crs(4326),
    xlim = c(75, 140), ylim = c(18, 55),
    datum = sf::st_crs(4326),
    label_axes = list(bottom = "E", left = "N"),
    expand = FALSE
  ) +
  labs(x = "Longitude", y = "Latitude") +
  theme_classic(base_size = 8.5, base_family = font_family) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    axis.line = element_line(colour = "black", linewidth = 0.40),
    axis.ticks = element_line(colour = "black", linewidth = 0.35),
    axis.ticks.length = unit(1.2, "mm"),
    axis.title = element_text(size = 8.5, colour = "black"),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.title.y = element_text(margin = margin(r = 4)),
    axis.text = element_text(size = 7.6, colour = "black"),
    plot.margin = margin(t = 3, r = 8, b = 4, l = 7, unit = "pt")
  )

# 4. PANEL (b) — TRUE USDA TEXTURE TRIANGLE ------------------------------------
# No raster PNG is created as an intermediate graphic. ggplotify converts
# soiltexture's base-graphics elements into a grid plot for vector export.
texture_plot_function <- function() {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)
  par(family = "sans", bg = "white", mar = c(1.0, 1.1, 0.7, 1.1))

  geo <- soiltexture::TT.plot(
    class.sys = "USDA.TT",
    main = "",
    bg = "white", frame.bg.col = "white",
    class.p.bg.col = FALSE,
    class.line.col = "#818181",
    class.lab.col = "#353535",
    col.axis = "black", col.lab = "black",
    grid.show = TRUE, grid.col = "#E0E0E0",
    cex.axis = 0.78,
    cex.lab = 0.83,
    lwd.axis = 0.8,
    family.op = "sans"
  )
  soiltexture::TT.points(
    tri.data = texture, geo = geo,
    col = location_red, pch = 19, cex = 1.55
  )
}

panelB <- ggplotify::as.ggplot(texture_plot_function)

# 5. PANEL LABELS INSIDE, SAME STYLE AS OTHER SUPPLEMENTARY FIGURES ------------
A_panel <- cowplot::ggdraw(panelA) +
  cowplot::draw_label(
    "(a)", x = 0.022, y = 0.985,
    hjust = 0, vjust = 1,
    fontfamily = font_family, fontface = "bold",
    size = 11.4, colour = "black"
  )

B_panel <- cowplot::ggdraw(panelB) +
  cowplot::draw_label(
    "(b)", x = 0.022, y = 0.985,
    hjust = 0, vjust = 1,
    fontfamily = font_family, fontface = "bold",
    size = 11.4, colour = "black"
  )

# 6. FINAL VERTICAL ARRANGEMENT -----------------------------------------------
# Map first (a); USDA texture triangle below (b).
final_figure <- (A_panel / B_panel) +
  patchwork::plot_layout(heights = c(1.00, 1.18)) &
  theme(plot.background = element_rect(fill = "white", colour = NA))

print(final_figure)

# 7. EXPORT -------------------------------------------------------------------
word_png   <- file.path(out_dir, "Fig_S1_Word_Portrait_600dpi.png")
word_tif   <- file.path(out_dir, "Fig_S1_Word_Portrait_600dpi.tiff")
vector_pdf <- file.path(out_dir, "Fig_S1_NC_Vector.pdf")

ggsave(
  filename = word_png, plot = final_figure,
  width = word_width_cm, height = word_height_cm,
  units = "cm", dpi = 600, bg = "white", limitsize = FALSE
)

ggsave(
  filename = word_tif, plot = final_figure,
  width = word_width_cm, height = word_height_cm,
  units = "cm", dpi = 600, compression = "lzw",
  bg = "white", limitsize = FALSE
)

ggsave(
  filename = vector_pdf, plot = final_figure,
  width = nc_width_cm, height = nc_height_cm,
  units = "cm",
  device = if (capabilities("cairo")) cairo_pdf else "pdf",
  bg = "white", limitsize = FALSE
)

message("Fig. S1 exported successfully to: ", normalizePath(out_dir))
message(" - ", basename(word_png))
message(" - ", basename(word_tif))
message(" - ", basename(vector_pdf))
message("Study-site coordinates: 109.9 E, 40.1 N")
message("Texture: clay 28.5%, silt 55.0%, sand 16.5%")
