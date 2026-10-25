# =============================================================================
# SUPPLEMENTARY FIGURE S4 | SEM-EDS elemental composition and Ca/S mass ratio
# Nature Communications-inspired, portrait Word-ready formatting
#
# PANEL (a): elemental composition (wt%) at Sp.1-Sp.6
# PANEL (b): Ca/S mass ratio at Sp.1-Sp.6 (calculated from unrounded wt%)
# GP: Sp.1-Sp.2     PEG: Sp.3-Sp.6
#
# Unchanged from original script:
#   * original SEM_PEG.xlsx and six sheet names
#   * original spot-to-treatment mapping
#   * original element selection and ordering
#   * original wt% data and calculation Ca/S = Ca(wt%)/S(wt%)
#   * numerical value labels (one decimal wt%; two decimals Ca/S)
#   * sample-level, element-level, and Ca/S summary Excel tables
#
# Presentation changes:
#   * portrait-Word friendly 15.5 x 13.5 cm
#   * panel letters (a), (b) INSIDE their upper-left corners
#   * white background; GP / PEG headings and thin dashed separator
#   * a clear colourblind-considerate palette and compact shared legend
#   * readable value labels and all bars separated at final Word size
#   * 600 dpi PNG/TIFF + editable vector PDF
#
# NOTE: The original SEM_PEG.xlsx is archived under data/ and resolved
#       automatically. This script does NOT replace measured data or ratios.
# =============================================================================

rm(list = ls())
graphics.off()

# 1. PACKAGES ----------------------------------------------------------------
pkgs <- c("readxl", "dplyr", "tidyr", "ggplot2", "purrr",
          "stringr", "patchwork", "openxlsx", "tibble", "grid")
needed <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(needed)) install.packages(needed, dependencies = TRUE)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(purrr)
  library(stringr)
  library(patchwork)
  library(openxlsx)
  library(tibble)
  library(grid)
})

# 2. FILE PATHS AND FIGURE DIMENSIONS -----------------------------------------
repo_input_path <- function(filename) {
  candidates <- c(
    filename,
    file.path("data", filename),
    file.path("..", "data", filename),
    file.path("..", "..", "data", filename)
  )
  matches <- candidates[file.exists(candidates)]
  if (length(matches)) return(matches[[1]])
  filename
}
file_eds <- repo_input_path("SEM_PEG.xlsx")
out_dir <- file.path("outputs", "Fig_S4_NatureCommunications_Portrait")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

word_width_cm   <- 15.5
word_height_cm  <- 13.5
journal_width_cm  <- 18.0
journal_height_cm <- word_height_cm * journal_width_cm / word_width_cm

FONT_FAMILY <- "Arial"
BASE_SIZE   <- 9.0  # point size on the actual 15.5-cm Word version

expected_sheets <- paste("Spectrum", 1:6)

# 3. PRESERVED SAMPLE MAPPING AND ELEMENTS -----------------------------------
sample_map <- tibble(
  Spectrum_ID = 1:6,
  Spectrum = paste("Spectrum", 1:6),
  Sample = c("GP", "GP", "PEG", "PEG", "PEG", "PEG"),
  Spectrum_label = c("GP Sp.1", "GP Sp.2", "PEG Sp.3",
                     "PEG Sp.4", "PEG Sp.5", "PEG Sp.6"),
  Spectrum_short = paste0("Sp.", 1:6),
  Spectrum_num = 1:6
)

element_order <- c("Ca", "S", "P", "F", "Si", "Mg")

# Stable across all spots; chosen to improve distinction for colourblind readers.
elem_cols <- c(
  "Ca" = "#4477AA",  # blue
  "S"  = "#D55E00",  # vermilion
  "P"  = "#009E73",  # teal green
  "F"  = "#E69F00",  # amber
  "Si" = "#CC79A7",  # mauve
  "Mg" = "#666666"   # grey
)

# For panel (b), treatment identity is reinforced by GP/PEG headings.
sample_cols <- c("GP" = "#383838", "PEG" = "#56B4E9")

# Exactly the original bar order and spot-level x positions.
element_offsets <- c(
  "Ca" = -0.27, "S" = -0.16, "P" = -0.05,
  "F"  =  0.06, "Si" =  0.17, "Mg" = 0.28
)

# 4. CLEAN FIGURE THEME -------------------------------------------------------
theme_s4 <- function(base_size = BASE_SIZE) {
  theme_classic(base_size = base_size, base_family = FONT_FAMILY) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      axis.line = element_line(colour = "black", linewidth = 0.46),
      axis.ticks = element_line(colour = "black", linewidth = 0.39),
      axis.ticks.length = unit(1.5, "mm"),
      axis.text.x = element_text(size = 8.5, colour = "black",
                                 margin = margin(t = 3)),
      axis.text.y = element_text(size = 8.3, colour = "black"),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 9.1, colour = "black",
                                  margin = margin(r = 5)),
      plot.title = element_blank(),
      plot.tag = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.text = element_text(size = 8.2, colour = "black"),
      legend.key.size = unit(3.6, "mm"),
      legend.spacing.x = unit(2.2, "mm"),
      legend.margin = margin(0, 0, 0, 0),
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
    )
}

# 5. READ ORIGINAL DATA -------------------------------------------------------
if (!file.exists(file_eds)) {
  stop("SEM-EDS file not found: ", file_eds,
       "\nWorking directory: ", getwd(),
       "\nPlace the original SEM_PEG.xlsx in data/. The tabular SEM_PEG.csv export is not a direct replacement for this script.", call. = FALSE)
}

available_sheets <- readxl::excel_sheets(file_eds)
missing_sheets <- setdiff(expected_sheets, available_sheets)
if (length(missing_sheets)) {
  stop("Missing sheet(s): ", paste(missing_sheets, collapse = ", "),
       call. = FALSE)
}

read_one_eds_sheet <- function(file, sheet_name) {
  d <- readxl::read_excel(file, sheet = sheet_name,
                          .name_repair = "minimal")
  names(d) <- stringr::str_trim(names(d))
  d <- d %>% select(where(~ !all(is.na(.))))
  
  if (ncol(d) < 2) stop(sheet_name, " contains fewer than 2 usable columns.")
  element_col <- names(d)[1]
  wt_candidates <- names(d)[names(d) %in%
                              c("Wt%", "Wt.", "wt%", "Wt", "wt", "Weight%", "Weight %")]
  
  if (!length(wt_candidates)) {
    # The original script used an arbitrary numeric column when Wt% was absent.
    # Stop instead: accidental use of At% would change the scientific results.
    stop("No recognized weight-percent (Wt%) column in ", sheet_name,
         ".\nAvailable columns: ", paste(names(d), collapse = ", "),
         "\nVerify the sheet format, then update wt_candidates if needed.",
         call. = FALSE)
  }
  wt_col <- wt_candidates[1]
  spectrum_id <- as.integer(stringr::str_extract(sheet_name, "\\d+"))
  
  d %>%
    filter(!is.na(.data[[element_col]])) %>%
    mutate(
      Element = stringr::str_trim(as.character(.data[[element_col]])),
      Wt = suppressWarnings(as.numeric(.data[[wt_col]])),
      Spectrum_ID = spectrum_id,
      Spectrum = sheet_name
    ) %>%
    filter(!is.na(Element), Element != "", Element != "Element",
           is.finite(Wt)) %>%
    select(Spectrum_ID, Spectrum, Element, Wt)
}

eds_raw <- map_dfr(expected_sheets,
                   ~ read_one_eds_sheet(file_eds, .x)) %>%
  left_join(sample_map, by = c("Spectrum_ID", "Spectrum")) %>%
  mutate(Sample = factor(Sample, levels = c("GP", "PEG")))

if (nrow(eds_raw) == 0) stop("No EDS data loaded.")

# Reject duplicate measurements in the same spectrum/element: there should
# be exactly one element wt% per spot, not multiple bars silently repeated.
duplicates <- eds_raw %>%
  count(Spectrum_ID, Element) %>% filter(n > 1)
if (nrow(duplicates)) {
  stop("Duplicate element records found in one or more EDS sheets. ",
       "Please check the workbook before producing the figure.")
}

# 6. ELEMENTAL DATA, LABELS, AND Ca/S MASS RATIOS ------------------------------
eds_plot <- tidyr::expand_grid(
  Spectrum_ID = 1:6,
  Element = element_order
) %>%
  left_join(eds_raw %>% select(Spectrum_ID, Element, Wt),
            by = c("Spectrum_ID", "Element")) %>%
  mutate(Wt = ifelse(is.na(Wt), 0, Wt)) %>%
  left_join(sample_map %>%
              select(Spectrum_ID, Sample, Spectrum_label,
                     Spectrum_short, Spectrum_num),
            by = "Spectrum_ID") %>%
  mutate(
    Element = factor(Element, levels = element_order),
    Sample = factor(Sample, levels = c("GP", "PEG")),
    Spectrum_label = factor(Spectrum_label,
                            levels = sample_map$Spectrum_label),
    Spectrum_short = factor(Spectrum_short,
                            levels = sample_map$Spectrum_short),
    Spectrum_num = as.numeric(Spectrum_num),
    x_bar = Spectrum_num + unname(element_offsets[as.character(Element)])
  )

# No y-axis truncation of column charts (required for honest bar heights).
y_upper_eds <- max(65, ceiling(max(eds_plot$Wt, na.rm = TRUE) / 5) * 5 + 5)

# Show every originally annotated EDS value (Wt >= 0.5).
# Stagger small peak labels to avoid collisions in a 15.5-cm-wide figure.
eds_label_df <- eds_plot %>%
  filter(Wt >= 0.50) %>%
  mutate(
    # Ca labels are CENTERED over the Ca bars.
    # Labels for adjacent S/P/F bars start essentially at their bar centres,
    # and extend rightward to avoid covering the taller left-hand bar.
    # The shift is only 0.010 x-axis units (~2 pixels at Word width).
    # All bar positions, heights, widths and measured values are unchanged.
    label_x = if_else(as.character(Element) == "Ca", x_bar, x_bar + 0.010),
    label_hjust = if_else(as.character(Element) == "Ca", 0.5, 0),
    label_offset = case_when(
      as.character(Element) == "Ca" ~ 1.6,
      as.character(Element) == "S"  ~ 1.6,
      as.character(Element) == "P"  ~ 1.5,
      as.character(Element) == "F"  ~ 1.8,
      as.character(Element) == "Si" ~ 4.0,
      as.character(Element) == "Mg" ~ 6.5,
      TRUE ~ 1.5
    ),
    label_y = pmin(Wt + label_offset, y_upper_eds - 1.3),
    label_txt = sprintf("%.1f", Wt)
  )

eds_wide <- eds_plot %>%
  select(Sample, Spectrum_ID, Spectrum_label, Spectrum_short,
         Spectrum_num, Element, Wt) %>%
  mutate(Element = as.character(Element)) %>%
  tidyr::pivot_wider(names_from = Element, values_from = Wt,
                     values_fill = 0) %>%
  arrange(Spectrum_ID)

ratio_df <- eds_wide %>%
  mutate(
    Ca_S_ratio = ifelse(S > 0, Ca / S, NA_real_),
    Spectrum_short = factor(Spectrum_short, levels = paste0("Sp.", 1:6)),
    Spectrum_num = as.numeric(Spectrum_num)
  )

if (all(!is.finite(ratio_df$Ca_S_ratio))) {
  stop("Ca/S ratios are undefined: sulfur wt% is zero or absent in all spots.")
}

ratio_upper <- max(2.0, max(ratio_df$Ca_S_ratio, na.rm = TRUE) * 1.23)
ratio_label_df <- ratio_df %>%
  mutate(
    label_y = Ca_S_ratio + 0.060,
    label_txt = ifelse(is.finite(Ca_S_ratio),
                       sprintf("%.2f", Ca_S_ratio), NA_character_)
  )

# 7. PANEL (a): ELEMENTAL COMPOSITION ----------------------------------------
pA <- ggplot() +
  geom_vline(xintercept = 2.5, colour = "grey45",
             linetype = "dashed", linewidth = 0.40) +
  annotate("text", x = 0.53, y = y_upper_eds * 0.995,
           label = "(a)", hjust = 0, vjust = 1,
           family = FONT_FAMILY, fontface = "bold", size = 3.9) +
  annotate("text", x = 1.5, y = y_upper_eds * 0.955,
           label = "GP", size = 3.55, family = FONT_FAMILY) +
  annotate("text", x = 4.5, y = y_upper_eds * 0.955,
           label = "PEG", size = 3.55, family = FONT_FAMILY) +
  geom_col(data = eds_plot,
           aes(x = x_bar, y = Wt, fill = Element),
           width = 0.10, colour = "black", linewidth = 0.37,
           show.legend = TRUE) +
  geom_text(data = eds_label_df,
            aes(x = label_x, y = label_y, label = label_txt,
                hjust = label_hjust),
            size = 2.35, family = FONT_FAMILY, colour = "black",
            vjust = 0, check_overlap = FALSE) +
  scale_fill_manual(values = elem_cols, limits = element_order,
                    drop = FALSE, guide = guide_legend(nrow = 1, byrow = TRUE)) +
  scale_x_continuous(breaks = 1:6, labels = paste0("Sp.", 1:6),
                     limits = c(0.5, 6.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, y_upper_eds),
                     breaks = seq(0, y_upper_eds, by = 10),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = NULL, y = "Elemental composition (wt%)") +
  coord_cartesian(clip = "off") +
  theme_s4()

# 8. PANEL (b): Ca/S MASS RATIO -----------------------------------------------
pB <- ggplot() +
  geom_vline(xintercept = 2.5, colour = "grey45",
             linetype = "dashed", linewidth = 0.40) +
  annotate("text", x = 0.66, y = ratio_upper * 0.983,
           label = "(b)", hjust = 0, vjust = 1,
           family = FONT_FAMILY, fontface = "bold", size = 3.9) +
  annotate("text", x = 1.5, y = ratio_upper * 0.916,
           label = "GP", size = 3.55, family = FONT_FAMILY) +
  annotate("text", x = 4.5, y = ratio_upper * 0.916,
           label = "PEG", size = 3.55, family = FONT_FAMILY) +
  geom_col(data = ratio_df,
           aes(x = Spectrum_num, y = Ca_S_ratio, fill = Sample),
           width = 0.41, colour = "black", linewidth = 0.36,
           show.legend = FALSE) +
  geom_text(data = ratio_label_df,
            aes(x = Spectrum_num, y = label_y, label = label_txt),
            size = 2.90, family = FONT_FAMILY, colour = "black",
            hjust = 0.5, vjust = 0) +
  scale_fill_manual(values = sample_cols, guide = "none") +
  scale_x_continuous(breaks = 1:6, labels = paste0("Sp.", 1:6),
                     limits = c(0.5, 6.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, ratio_upper),
                     breaks = seq(0, ratio_upper, by = 0.5),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = NULL, y = "Ca/S mass ratio") +
  coord_cartesian(clip = "off") +
  theme_s4() +
  theme(legend.position = "none")

# 9. COMBINE: TWO PANELS, LEGEND AT BOTTOM ------------------------------------
figS4 <- (pA / pB) +
  plot_layout(heights = c(1.40, 0.95), guides = "collect") &
  theme(plot.background = element_rect(fill = "white", colour = NA),
        legend.position = "bottom")

print(figS4)

# 10. EXPORT -----------------------------------------------------------------
word_png <- file.path(out_dir, "Fig_S4_EDS_Word_Portrait_600dpi.png")
word_tif <- file.path(out_dir, "Fig_S4_EDS_Word_Portrait_600dpi.tiff")
vector_pdf <- file.path(out_dir, "Fig_S4_EDS_NC_Vector.pdf")

# Word version: insert at 15.5 cm (do not enlarge inside Word).
ggsave(filename = word_png, plot = figS4,
       width = word_width_cm, height = word_height_cm,
       units = "cm", dpi = 600, bg = "white", limitsize = FALSE)

ggsave(filename = word_tif, plot = figS4,
       width = word_width_cm, height = word_height_cm,
       units = "cm", dpi = 600, compression = "lzw",
       bg = "white", limitsize = FALSE)

# Fully vector PDF: use Cairo when available; otherwise use base PDF device.
vector_device <- if (capabilities("cairo")) grDevices::cairo_pdf else "pdf"
ggsave(filename = vector_pdf, plot = figS4,
       width = journal_width_cm, height = journal_height_cm,
       units = "cm", device = vector_device,
       bg = "white", limitsize = FALSE)

# 11. ORIGINAL SUMMARY TABLES ------------------------------------------------
eds_per_spectrum <- eds_plot %>%
  select(Sample, Spectrum_ID, Spectrum_label, Spectrum_short, Element, Wt) %>%
  arrange(Spectrum_ID, Element)

eds_summary <- eds_per_spectrum %>%
  group_by(Sample, Element) %>%
  summarise(
    mean_wt = mean(Wt, na.rm = TRUE),
    sd_wt = sd(Wt, na.rm = TRUE),
    n = sum(!is.na(Wt)),
    mean_sd = ifelse(is.na(sd_wt), sprintf("%.2f", mean_wt),
                     sprintf("%.2f ± %.2f", mean_wt, sd_wt)),
    .groups = "drop"
  ) %>% arrange(Sample, Element)

ratio_summary <- ratio_df %>%
  group_by(Sample) %>%
  summarise(
    mean_Ca_S_ratio = mean(Ca_S_ratio, na.rm = TRUE),
    sd_Ca_S_ratio = sd(Ca_S_ratio, na.rm = TRUE),
    n = sum(!is.na(Ca_S_ratio)),
    mean_sd = ifelse(is.na(sd_Ca_S_ratio),
                     sprintf("%.2f", mean_Ca_S_ratio),
                     sprintf("%.2f ± %.2f", mean_Ca_S_ratio, sd_Ca_S_ratio)),
    .groups = "drop"
  )

stats_out <- file.path(out_dir, "Fig_S4_EDS_Data_and_Statistics.xlsx")
openxlsx::write.xlsx(
  list(
    "Raw_clean_long" = eds_per_spectrum,
    "Per_spectrum_wide" = eds_wide,
    "Ca_S_ratio" = ratio_df,
    "Sample_summary_mean_SD" = eds_summary,
    "Ca_S_ratio_summary" = ratio_summary
  ),
  file = stats_out, overwrite = TRUE
)

message("\nFig. S4 complete. Outputs saved to: ",
        normalizePath(out_dir, mustWork = FALSE))
message("Word 600-dpi PNG: ", word_png)
message("Word 600-dpi TIFF: ", word_tif)
message("NC vector PDF: ", vector_pdf)
message("Data/statistics: ", stats_out)
message("Word insertion size: ", word_width_cm, " cm width, ",
        word_height_cm, " cm height")
message("Ca/S = Ca(wt%)/S(wt%) from original, unrounded EDS values")

# NOTE: X-ray EDS composition depends on sampled microregions. Do not interpret
# six spot measurements as six independent experimental replicates without
# documenting the independent sampling structure.