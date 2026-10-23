# ============================================================================
# SUPPLEMENTARY FIGURE S2 — XRD PATTERNS (NATURE COMMUNICATIONS STYLE)
# Portrait Word layout: 2 columns x 3 rows + ONE shared reference section
# Minor reflections are made clearer using a documented y-axis VIEWPORT ZOOM.
#
# (a) GP       (b) PEG-50
# (c) PEG-20   (d) PEG-10
# (e) PEG-5    (f) PEG-2
#             A-I common reference patterns below
#
# Preserved from the user's working script:
#   - original seven TXT filenames and sample-to-treatment mapping
#   - per-file intensity normalization
#   - all nine reference-phase peak positions and relative intensities
#   - control trace underneath each treatment
#   - panel order and H annotations for PEG-5 / PEG-2
#
# NOTE: The nine reference stick patterns are USER-SUPPLIED, not newly checked
# against a phase database. Likewise, H positions must be checked against
# experimental diffraction and competing CaSO4 reflections before submission.
#
# Packages: ggplot2, dplyr, purrr, tibble, patchwork, grid (base)
# Install once if needed:
# install.packages(c("ggplot2", "dplyr", "purrr", "tibble", "patchwork"))
# ============================================================================

rm(list = ls())
graphics.off()

library(ggplot2)
library(dplyr)
library(purrr)
library(tibble)
library(patchwork)
library(grid)

# ----------------------------------------------------------------------------
# 1. PATHS AND OUTPUT SETTINGS
# ----------------------------------------------------------------------------
xrd_dir <- "E:/PHD/Traditional_Amendments/PEG_dosage"
out_dir <- file.path(xrd_dir, "XRD_Figures", "Fig_S2_Portrait_Zoomed")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Change only these values if your Word layout has different available space.
word_width_cm  <- 15.5
word_height_cm <- 18.0
journal_width_cm  <- 18.0   # NC double-column width
journal_height_cm <- word_height_cm * journal_width_cm / word_width_cm

# Original H labels retained for PEG-5 and PEG-2. IMPORTANT: H assignments
# are provisional; visual XRD peak proximity alone does not prove that phase.
show_H_markers <- TRUE

# VISIBILITY CONTROL: lower the display ceiling to show minor reflections.
# This is a VIEWPORT ZOOM ONLY; normalized XRD data are NOT rescaled.
# Consequently, strong reflections above the ceiling are visually clipped.
# Disclose this in the Fig. S2 caption; do not interpret clipped peak heights.
panel_y_max <- 0.20  # try 0.25 for less zoom, 0.15 for more zoom
stopifnot(is.numeric(panel_y_max), length(panel_y_max) == 1,
          panel_y_max > 0.05, panel_y_max <= 1.05)

# Treatment trace is thicker/darker than the grey control overlay.
panel_line_trt <- 0.62
panel_line_ctl <- 0.40

# Increase treatment area slightly but protect reference-row readability.
# An extreme split (4.4 : 1.05) crowds the nine phase-reference labels.
ref_ratio_top <- 3.60
ref_ratio_bottom <- 1.40

# ----------------------------------------------------------------------------
# 2. ORIGINAL SOURCE FILE NAMES AND DISPLAY ORDER
# ----------------------------------------------------------------------------
sample_info <- tibble(
  sample = c("control", "GP_Et", "PEG_VLEt", "PEG_LEt",
             "PEG_MEt", "PEG_CEt", "PEG_H"),
  display = c("Control", "GP", "PEG-50", "PEG-20",
              "PEG-10", "PEG-5", "PEG-2"),
  cap_label = c("Control", "Ca:P = 0", "Ca:P = 50:1", "Ca:P = 20:1",
                "Ca:P = 10:1", "Ca:P = 5:1", "Ca:P = 2:1"),
  order = 0:6
)

# ----------------------------------------------------------------------------
# 3. READ USER'S TWO-COLUMN RAW XRD FILES
#    Accepts tabs, commas, semicolons or spaces, and ignores nonnumeric header.
# ----------------------------------------------------------------------------
read_xrd_txt <- function(file_path, sample_name) {
  raw_lines <- readLines(file_path, warn = FALSE)
  split_lines <- strsplit(trimws(raw_lines), "[,;\\t ]+")
  numeric_rows <- lapply(split_lines, function(z) suppressWarnings(as.numeric(z)))
  numeric_rows <- numeric_rows[vapply(numeric_rows, function(z) {
    sum(is.finite(z)) >= 2
  }, logical(1))]
  
  if (length(numeric_rows) == 0) {
    stop("No numeric XRD rows in: ", file_path)
  }
  
  numeric_matrix <- do.call(rbind, lapply(numeric_rows, function(z) {
    z <- z[is.finite(z)]
    z[1:2]
  }))
  
  out <- tibble(
    two_theta = as.numeric(numeric_matrix[, 1]),
    intensity = as.numeric(numeric_matrix[, 2]),
    sample = sample_name
  ) %>%
    filter(is.finite(two_theta), is.finite(intensity)) %>%
    arrange(two_theta)
  
  if (nrow(out) < 2 || max(out$intensity, na.rm = TRUE) <= 0) {
    stop("Insufficient or invalid intensity data in: ", file_path)
  }
  out
}

expected_files <- file.path(xrd_dir, paste0(sample_info$sample, ".txt"))
missing_files <- expected_files[!file.exists(expected_files)]
if (length(missing_files) > 0) {
  stop("Missing TXT files in xrd_dir:\n", paste(basename(missing_files), collapse = "\n"))
}

xrd_all <- purrr::map2_dfr(expected_files, sample_info$sample, read_xrd_txt) %>%
  group_by(sample) %>%
  mutate(intensity_norm = intensity / max(intensity, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(sample_info, by = "sample")

control_dat <- xrd_all %>% filter(sample == "control")
treatment_info <- sample_info %>% filter(sample != "control") %>% arrange(order)

# ----------------------------------------------------------------------------
# 4. ORIGINAL REFERENCE PHASE POSITIONS (A-I) — NO VALUES CHANGED
# ----------------------------------------------------------------------------
mkph <- function(phase, two_theta, rel_int) {
  stopifnot(length(two_theta) == length(rel_int))
  tibble(phase = phase, two_theta = two_theta, rel_int = rel_int)
}

phase_df <- bind_rows(
  mkph("A",
       c(11.70, 20.80, 23.40, 29.15, 31.10, 33.35, 40.70, 47.85, 50.35),
       c(1.00, 0.12, 0.26, 0.15, 0.06, 0.07, 0.04, 0.05, 0.03)),
  mkph("B",
       c(14.80, 25.50, 29.70, 31.70, 42.40, 55.80),
       c(0.30, 0.45, 1.00, 0.60, 0.20, 0.18)),
  mkph("C",
       c(20.85, 26.65, 36.55, 39.50, 50.15, 59.95),
       c(0.12, 1.00, 0.45, 0.25, 0.20, 0.12)),
  mkph("D",
       c(25.40, 31.50, 42.80, 49.80, 55.60),
       c(0.15, 0.20, 0.35, 1.00, 0.25)),
  mkph("E",
       c(11.62, 20.95, 23.55, 29.36, 30.50, 34.10, 39.80, 42.00, 48.90),
       c(0.20, 0.28, 0.40, 1.00, 0.55, 0.25, 0.18, 0.16, 0.12)),
  mkph("F",
       c(13.10, 26.40, 30.20, 31.80, 34.20, 35.50, 40.30, 49.10),
       c(0.18, 0.60, 1.00, 0.55, 0.48, 0.22, 0.20, 0.15)),
  mkph("G",
       c(25.87, 28.96, 31.77, 32.19, 32.90, 34.08, 40.00, 46.67, 49.48),
       c(0.22, 0.18, 0.90, 1.00, 0.78, 0.62, 0.20, 0.18, 0.16)),
  mkph("H",
       c(12.05, 21.05, 23.30, 24.25, 29.35, 30.20, 32.05, 34.25, 47.10),
       c(0.38, 0.25, 0.45, 0.30, 1.00, 0.55, 0.35, 0.28, 0.18)),
  mkph("I",
       c(25.90, 31.80, 32.20, 32.90, 34.05, 39.85, 46.70, 49.50, 53.20),
       c(0.18, 0.85, 1.00, 0.75, 0.60, 0.20, 0.22, 0.18, 0.15))
)

# Reference labels rendered with real chemical subscripts where available.
phase_meta <- tibble(
  phase = LETTERS[1:9],
  formula = c(
    "CaSO₄·2H₂O", "CaSO₄·0.5H₂O", "SiO₂", "CaSO₄",
    "CaHPO₄·2H₂O", "CaHPO₄", "Ca₃(PO₄)₂", "Ca(H₂PO₄)₂",
    "Ca₅(PO₄)₃OH"
  ),
  row = 9:1
) %>%
  mutate(label = paste(phase, formula, sep = "   "))

phase_df <- phase_df %>%
  left_join(phase_meta, by = "phase") %>%
  mutate(
    base_y = row - 0.20,
    peak_y = base_y + 0.48 * rel_int
  )

# ----------------------------------------------------------------------------
# 5. MANUAL H PEAK LABELS — ORIGINAL TWO LOCATIONS RETAINED
#    These are annotation positions, NOT an automated proof of phase identity.
#    Check x positions/phase assignments from your original phase analysis.
# ----------------------------------------------------------------------------
h_marker_df <- tibble(
  sample = c("PEG_CEt", "PEG_CEt", "PEG_H", "PEG_H"),
  two_theta = c(12.05, 21.05, 12.05, 21.05),
  y_text = rep(min(0.12, panel_y_max * 0.67), 4),
  label = "H"
)

# ----------------------------------------------------------------------------
# 6. CLEAN TYPOGRAPHY FOR SMALL PANELS (NO FACET STRIPS OR BOXES)
# ----------------------------------------------------------------------------
font_family <- "Arial"

panel_theme <- function(show_left_axis, show_bottom_axis, show_y_title) {
  theme_classic(base_family = font_family, base_size = 8.8) +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border = element_blank(),
      axis.line = element_line(linewidth = 0.45, colour = "black"),
      axis.ticks = element_line(linewidth = 0.37, colour = "black"),
      axis.ticks.length = unit(1.2, "mm"),
      axis.text = element_text(size = 8.5, colour = "black"),
      axis.title.y = element_text(size = 9.1, margin = margin(r = 5)),
      axis.title.x = element_blank(),
      legend.position = "none",
      plot.margin = margin(3, 5, 4, 6, unit = "pt"),
      axis.text.y = if (show_left_axis) element_text(size = 8.5) else element_blank(),
      axis.ticks.y = if (show_left_axis) element_line(linewidth = 0.37) else element_blank(),
      axis.line.y = if (show_left_axis) element_line(linewidth = 0.45) else element_blank(),
      axis.text.x = if (show_bottom_axis) element_text(size = 8.5) else element_blank(),
      axis.ticks.x = if (show_bottom_axis) element_line(linewidth = 0.37) else element_blank(),
      axis.line.x = element_line(linewidth = 0.45),
      axis.title.y.right = element_blank()
    )
}

make_treatment_panel <- function(i) {
  sample_id <- treatment_info$sample[i]
  sample_name <- treatment_info$display[i]
  trt <- xrd_all %>% filter(sample == sample_id)
  show_left <- (i %% 2 == 1)
  show_bottom <- i >= 5
  show_y_title <- i == 3
  
  p <- ggplot() +
    geom_line(
      data = control_dat,
      aes(x = two_theta, y = intensity_norm),
      colour = "grey65", linewidth = panel_line_ctl
    ) +
    geom_line(
      data = trt,
      aes(x = two_theta, y = intensity_norm),
      colour = "black", linewidth = panel_line_trt
    ) +
    annotate(
      "text", x = 13.4, y = panel_y_max * 0.98,
      label = paste0("(", letters[i], ")"),
      hjust = 0, vjust = 1,
      family = font_family, size = 3.55, fontface = "bold"
    ) +
    annotate(
      "text", x = 36.0, y = panel_y_max * 0.95,
      label = sample_name, hjust = 0.5, vjust = 1,
      family = font_family, size = 3.55, fontface = "plain"
    ) +
    scale_x_continuous(
      breaks = seq(10, 60, 10),
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous(
      breaks = seq(0, panel_y_max, by = 0.05),
      labels = function(z) ifelse(z == 0, "0", sprintf("%.2f", z)),
      expand = expansion(mult = 0)
    ) +
    # coord_cartesian zooms the display but retains the actual data,
    # unlike scale limits that would drop observations before drawing.
    coord_cartesian(xlim = c(10, 60), ylim = c(0, panel_y_max),
                    expand = FALSE, clip = "on") +
    labs(
      x = NULL,
      y = if (show_y_title) "Normalized intensity" else NULL
    ) +
    panel_theme(show_left, show_bottom, show_y_title)
  
  # H is deliberately restricted to only the two original treatment panels.
  # Change show_H_markers to FALSE to omit all H annotations.
  if (show_H_markers && sample_id %in% h_marker_df$sample) {
    p <- p + geom_text(
      data = h_marker_df %>% filter(sample == sample_id),
      aes(x = two_theta, y = y_text, label = label),
      inherit.aes = FALSE,
      colour = "#8B0000", family = font_family,
      size = 3.05, fontface = "bold", hjust = 0.5
    )
  }
  p
}

panels <- lapply(seq_len(nrow(treatment_info)), make_treatment_panel)

# ----------------------------------------------------------------------------
# 7. ONE SHARED REFERENCE STICK SECTION (A-I)
#    The horizontal scale is 2theta 10-60 degrees for every reference pattern.
# ----------------------------------------------------------------------------
ref_pattern_plot <- ggplot() +
  geom_segment(
    data = phase_meta,
    aes(x = 10, xend = 60, y = row - 0.20, yend = row - 0.20),
    linewidth = 0.36, colour = "grey65"
  ) +
  geom_segment(
    data = phase_df,
    aes(x = two_theta, xend = two_theta, y = base_y, yend = peak_y),
    linewidth = 0.48, colour = "grey20"
  ) +
  # Labels sit in the right margin, never covering diffraction sticks.
  geom_text(
    data = phase_meta,
    aes(x = 60.8, y = row - 0.20, label = label),
    hjust = 0, vjust = 0.5,
    family = font_family, colour = "black", size = 2.85
  ) +
  scale_x_continuous(breaks = seq(10, 60, 10),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(expand = expansion(mult = 0)) +
  coord_cartesian(xlim = c(10, 60), ylim = c(0.55, 9.65),
                  clip = "off", expand = FALSE) +
  labs(x = expression(2*theta~"(°)"), y = NULL) +
  theme_classic(base_family = font_family, base_size = 8.8) +
  theme(
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    axis.line.x = element_line(linewidth = 0.45, colour = "black"),
    axis.line.y = element_blank(),
    axis.ticks.x = element_line(linewidth = 0.37),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 8.5, colour = "black"),
    axis.text.y = element_blank(),
    axis.title.x = element_text(size = 9.2, margin = margin(t = 4)),
    # Extra right-side margin reserves room for the phase formulas.
    plot.margin = margin(3, 106, 3, 6, unit = "pt")
  )

shared_refs <- ref_pattern_plot

# ----------------------------------------------------------------------------
# 8. ASSEMBLE A4 PORTRAIT SUPPLEMENTARY FIGURE S2
# IMPORTANT: strong reflections exceed panel_y_max and are clipped. Declare
# this prominently in the figure caption, and retain full-intensity original
# datasets for phase assignments and quantitative diffraction analysis.
# ----------------------------------------------------------------------------
treatment_grid <- wrap_plots(panels, ncol = 2, byrow = TRUE) +
  plot_layout(heights = c(1, 1, 1))

fig_s2 <- (treatment_grid / shared_refs) +
  plot_layout(heights = c(ref_ratio_top, ref_ratio_bottom)) &
  theme(plot.background = element_rect(fill = "white", colour = NA))

print(fig_s2)

# ----------------------------------------------------------------------------
# 9. EXPORT: WORD PNG + HIGH-RES TIFF + NATURE VECTOR PDF
# ----------------------------------------------------------------------------
word_png <- file.path(out_dir, "Fig_S2_XRD_Word_A4_Portrait_Zoomed_600dpi.png")
word_tif <- file.path(out_dir, "Fig_S2_XRD_Word_A4_Portrait_Zoomed_600dpi.tiff")
nature_pdf <- file.path(out_dir, "Fig_S2_XRD_NatureCommunications_Zoomed_Vector.pdf")
ref_csv <- file.path(out_dir, "Fig_S2_XRD_ReferencePeaks_A_to_I.csv")

ggsave(
  filename = word_png, plot = fig_s2,
  width = word_width_cm, height = word_height_cm,
  units = "cm", dpi = 600, bg = "white", limitsize = FALSE
)

ggsave(
  filename = word_tif, plot = fig_s2,
  width = word_width_cm, height = word_height_cm,
  units = "cm", dpi = 600,
  compression = "lzw", bg = "white", limitsize = FALSE
)

# Prefer Cairo vector PDF when available, but fall back to standard PDF.
pdf_device <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
ggsave(
  filename = nature_pdf, plot = fig_s2,
  width = journal_width_cm, height = journal_height_cm,
  units = "cm", device = pdf_device,
  bg = "white", limitsize = FALSE
)

write.csv(
  phase_df %>% select(phase, formula, two_theta, rel_int),
  file = ref_csv, row.names = FALSE, fileEncoding = "UTF-8"
)

cat("\nFig. S2 exported to:\n", normalizePath(out_dir), "\n")
cat(" - ", basename(word_png), "\n")
cat(" - ", basename(word_tif), "\n")
cat(" - ", basename(nature_pdf), "\n")
cat(" - ", basename(ref_csv), "\n")
cat("\nIn portrait Word, insert the PNG at 15.5 cm width.\n")
cat("Panels show normalized intensity from 0 to", panel_y_max,
    "; stronger reflections are CLIPPED for visibility.\n")
cat("State this y-axis truncation in the Fig. S2 caption.\n")
cat("Verify the H assignments and all reference cards before submission.\n")