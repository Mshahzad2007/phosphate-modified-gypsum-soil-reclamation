# =============================================================================
# Supplementary Figure 13
# Sequencing depth and bacterial community composition under CK, GP and PEG
#
# SELF-CONTAINED FIGURE-GENERATION SCRIPT
# - Does not require an input PNG, Excel workbook, or CSV.
# - Creates the four panels from numerical values stored below.
# - Exports 600-dpi PNG/TIFF and a vector PDF.
#
# DATA PROVENANCE / REPRODUCIBILITY NOTE
# Panel (d) uses the treatment-level relative-abundance values reported in
# Supplementary Table 17. The displayed read depths, Bray-Curtis distances,
# and Venn-region counts in panels (a-c) are the summary values transcribed
# from the supplied four-panel figure. They are NOT recalculated from raw
# sequencing reads or an ASV count table. For full analytical reproducibility,
# replace these summaries with values recalculated from the original raw-read/
# ASV source data when that file is available.
#
# Taxonomic note:
# The reported table contains genus names plus some "norank"/unclassified
# family-, order-, or higher-rank groups. The panel-(d) legend therefore uses
# "Taxon" rather than implying that every displayed category is a genus.
# =============================================================================

rm(list = ls())
graphics.off()
options(stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------
# 1. Packages
# -----------------------------------------------------------------------------
required_packages <- c("ggplot2", "patchwork")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

# -----------------------------------------------------------------------------
# 2. Output folder and common styling
# -----------------------------------------------------------------------------
out_dir <- file.path(getwd(), "Supplementary_Fig13_Output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

base_family <- "Arial"
treatment_order <- c("CK", "GP", "PEG")

treatment_colours <- c(
  CK  = "#CFCFCF",
  GP  = "#B0445A",
  PEG = "#5B88B7"
)

treatment_outlines <- c(
  CK  = "#666666",
  GP  = "#963149",
  PEG = "#356E9F"
)

common_theme <- theme_classic(base_size = 9, base_family = base_family) +
  theme(
    axis.text = element_text(colour = "black", size = 8.5),
    axis.title = element_text(colour = "black", size = 9.5),
    plot.tag = element_text(face = "bold", size = 12, colour = "black"),
    plot.tag.position = c(0.01, 0.99),
    plot.background = element_rect(fill = "white", colour = NA)
  )

# -----------------------------------------------------------------------------
# 3. Panel (a): sequencing depth
# Values are the displayed summary values from the supplied figure.
# -----------------------------------------------------------------------------
depth_data <- data.frame(
  Treatment = factor(c("CK", "GP", "PEG"), levels = treatment_order),
  Reads = c(52200, 58927, 51099)
)

p_a <- ggplot(depth_data, aes(x = Treatment, y = Reads, fill = Treatment)) +
  geom_col(width = 0.62, colour = "#333333", linewidth = 0.35) +
  geom_text(
    aes(label = format(Reads, big.mark = ",", scientific = FALSE)),
    vjust = -0.38, size = 3.0, family = base_family, colour = "#222222"
  ) +
  scale_fill_manual(values = treatment_colours, guide = "none") +
  scale_y_continuous(
    breaks = seq(0, 60000, by = 20000),
    labels = function(x) format(x, big.mark = ",", scientific = FALSE),
    limits = c(0, 65000),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = NULL,
    y = "Sequencing depth (reads)",
    tag = "(a)"
  ) +
  common_theme +
  theme(
    axis.title.y = element_text(margin = margin(r = 5)),
    plot.margin = margin(8, 5, 4, 7)
  )

# -----------------------------------------------------------------------------
# 4. Panel (b): pairwise Bray-Curtis dissimilarity matrix
# Values are the displayed summary values from the supplied figure.
# -----------------------------------------------------------------------------
bray_matrix <- matrix(
  c(
    0.000, 0.340, 0.415,
    0.340, 0.000, 0.210,
    0.415, 0.210, 0.000
  ),
  nrow = 3,
  byrow = TRUE,
  dimnames = list(treatment_order, treatment_order)
)

bray_data <- as.data.frame(as.table(bray_matrix))
names(bray_data) <- c("RowTreatment", "ColumnTreatment", "Distance")
bray_data$RowTreatment <- factor(
  bray_data$RowTreatment,
  levels = c("PEG", "GP", "CK")
)
bray_data$ColumnTreatment <- factor(
  bray_data$ColumnTreatment,
  levels = treatment_order
)
bray_data$LabelColour <- ifelse(bray_data$Distance >= 0.30, "white", "#222222")

p_b <- ggplot(
  bray_data,
  aes(x = ColumnTreatment, y = RowTreatment, fill = Distance)
) +
  geom_tile(colour = "white", linewidth = 1.1) +
  geom_text(
    aes(label = sprintf("%.3f", Distance), colour = LabelColour),
    size = 3.0, family = base_family, show.legend = FALSE
  ) +
  scale_colour_identity() +
  scale_fill_gradient(
    low = "#F4F4F4",
    high = "#4D4D4D",
    limits = c(0, 0.45),
    guide = "none"
  ) +
  coord_equal() +
  labs(x = NULL, y = NULL, tag = "(b)") +
  common_theme +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 8.5, margin = margin(t = 2)),
    axis.text.y = element_text(size = 8.5, margin = margin(r = 2)),
    plot.margin = margin(8, 5, 4, 5)
  )

# -----------------------------------------------------------------------------
# 5. Panel (c): Venn diagram
# Region counts are the values displayed in the supplied figure.
# -----------------------------------------------------------------------------
circle_parameters <- data.frame(
  Set = c("CK", "GP", "PEG"),
  CentreX = c(-0.43, 0.43, 0.00),
  CentreY = c(0.15, 0.15, -0.35),
  Radius = c(0.72, 0.72, 0.72)
)

circle_data <- do.call(
  rbind,
  lapply(seq_len(nrow(circle_parameters)), function(i) {
    theta <- seq(0, 2 * pi, length.out = 361)
    data.frame(
      Set = circle_parameters$Set[i],
      X = circle_parameters$CentreX[i] +
        circle_parameters$Radius[i] * cos(theta),
      Y = circle_parameters$CentreY[i] +
        circle_parameters$Radius[i] * sin(theta)
    )
  })
)

circle_data$Set <- factor(circle_data$Set, levels = c("CK", "GP", "PEG"))

venn_counts <- data.frame(
  Region = c(
    "CK only", "GP only", "PEG only",
    "CK and GP only", "CK and PEG only", "GP and PEG only",
    "Shared by all three"
  ),
  X = c(-0.73, 0.73, 0.00, 0.00, -0.30, 0.30, 0.00),
  Y = c(0.12, 0.12, -0.82, 0.49, -0.25, -0.25, 0.02),
  Count = c(0, 0, 0, 2, 0, 1, 37)
)

venn_treatment_labels <- data.frame(
  Set = c("CK", "GP", "PEG"),
  X = c(-0.68, 0.68, 0.00),
  Y = c(1.02, 1.02, -1.17)
)

p_c <- ggplot() +
  geom_polygon(
    data = circle_data,
    aes(x = X, y = Y, group = Set, fill = Set),
    alpha = 0.20,
    colour = NA
  ) +
  geom_path(
    data = circle_data,
    aes(x = X, y = Y, group = Set, colour = Set),
    linewidth = 0.9,
    show.legend = FALSE
  ) +
  geom_text(
    data = venn_counts,
    aes(x = X, y = Y, label = Count),
    size = 3.1,
    family = base_family,
    colour = "#111111"
  ) +
  geom_text(
    data = venn_treatment_labels,
    aes(x = X, y = Y, label = Set, colour = Set),
    size = 4.1,
    family = base_family,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = treatment_colours) +
  scale_colour_manual(values = treatment_outlines) +
  coord_equal(
    xlim = c(-1.18, 1.18),
    ylim = c(-1.27, 1.15),
    expand = FALSE,
    clip = "off"
  ) +
  labs(x = NULL, y = NULL, tag = "(c)") +
  theme_void(base_size = 9, base_family = base_family) +
  theme(
    plot.tag = element_text(face = "bold", size = 12, colour = "black"),
    plot.tag.position = c(0.01, 0.99),
    plot.margin = margin(8, 7, 4, 5),
    plot.background = element_rect(fill = "white", colour = NA)
  )

# -----------------------------------------------------------------------------
# 6. Panel (d): relative abundance of dominant taxa
# Values below are transcribed from Supplementary Table 17.
# Each treatment column sums to 100%.
# -----------------------------------------------------------------------------
taxon_data <- data.frame(
  Taxon = c(
    "Adhaeribacter",
    "Algoriphagus",
    "Antarcticibacterium",
    "Blastomonas",
    "Brevundimonas",
    "Devosia",
    "Erythrobacter",
    "Flavisolibacter",
    "Geoalkalibacter",
    "Halomonas",
    "Hydrogenophaga",
    "Imperialibacter",
    "Luteimonas",
    "Lysobacter",
    "Massilia",
    "Methyloversatilis",
    "Other",
    "Polycyclovorans",
    "Pontibacter",
    "Pseudazoarcus",
    "Pseudomonas",
    "Ramlibacter",
    "Roseateles",
    "Salinimicrobium",
    "Shinella",
    "Sphingomonas",
    "norank_Actinomarinales",
    "norank_Alphaproteobacteria",
    "norank_Beijerinckiaceae",
    "norank_Caulobacteraceae",
    "norank_Gemmatimonadota",
    "norank_Longimicrobiaceae",
    "norank_Microscillaceae",
    "norank_Pedosphaeraceae",
    "norank_Planctomycetales",
    "norank_Prolixibacteraceae",
    "norank_Rhodothermaceae",
    "norank_Tepidisphaerales",
    "unclassified_Methylophilaceae",
    "unclassified_Paracoccaceae"
  ),
  CK = c(
    0.51, 0.00, 0.46, 0.69, 0.05, 0.05, 0.89, 0.08, 0.23, 0.42,
    0.42, 0.22, 1.69, 12.83, 0.02, 0.01, 27.78, 1.68, 12.24, 1.60,
    0.91, 0.38, 0.20, 17.78, 0.01, 1.95, 1.24, 1.94, 0.05, 0.46,
    5.96, 3.13, 0.15, 0.96, 0.41, 0.03, 1.44, 0.61, 0.04, 0.48
  ),
  GP = c(
    0.03, 0.68, 0.08, 2.65, 1.06, 0.22, 1.77, 0.47, 0.04, 0.16,
    0.86, 0.03, 0.27, 19.58, 7.92, 0.27, 31.09, 0.01, 5.81, 0.02,
    5.60, 0.24, 0.29, 0.40, 0.24, 5.89, 1.68, 0.81, 0.05, 0.29,
    3.69, 1.24, 0.28, 1.11, 0.82, 0.00, 0.43, 2.74, 0.40, 0.78
  ),
  PEG = c(
    0.01, 0.57, 0.06, 20.02, 0.94, 0.84, 1.75, 0.09, 0.01, 0.06,
    3.73, 0.02, 0.22, 13.44, 2.44, 0.41, 30.05, 0.11, 1.08, 0.00,
    1.43, 0.32, 3.17, 0.11, 2.41, 2.76, 0.84, 0.92, 0.05, 2.54,
    3.75, 0.82, 1.08, 0.40, 0.80, 0.00, 0.42, 0.53, 1.67, 0.13
  )
)

abundance_totals <- colSums(taxon_data[, c("CK", "GP", "PEG")])
if (any(abs(abundance_totals - 100) > 0.02)) {
  stop(
    "The embedded taxon abundance values do not sum to 100%. Totals: ",
    paste(names(abundance_totals), round(abundance_totals, 3),
          collapse = "; ")
  )
}

# Keep the dominant taxa individually labelled; aggregate the other listed
# low-abundance taxa into "Remaining genera". The source row "Other" is retained.
display_map <- c(
  "Other" = "Other",
  "Lysobacter" = "Lysobacter",
  "Blastomonas" = "Blastomonas",
  "Pontibacter" = "Pontibacter",
  "Salinimicrobium" = "Salinimicrobium",
  "norank_Gemmatimonadota" = "Gemmatimonadota",
  "Sphingomonas" = "Sphingomonas",
  "Massilia" = "Massilia",
  "Pseudomonas" = "Pseudomonas",
  "norank_Longimicrobiaceae" = "Longimicrobiaceae",
  "Hydrogenophaga" = "Hydrogenophaga",
  "Erythrobacter" = "Erythrobacter",
  "norank_Tepidisphaerales" = "Tepidisphaerales",
  "norank_Actinomarinales" = "Actinomarinales",
  "norank_Alphaproteobacteria" = "Alphaproteobacteria",
  "Roseateles" = "Roseateles",
  "norank_Caulobacteraceae" = "Caulobacteraceae",
  "Shinella" = "Shinella",
  "norank_Pedosphaeraceae" = "Pedosphaeraceae",
  "norank_Rhodothermaceae" = "Rhodothermaceae"
)

taxon_long <- do.call(
  rbind,
  lapply(c("CK", "GP", "PEG"), function(trt) {
    data.frame(
      OriginalTaxon = taxon_data$Taxon,
      Treatment = trt,
      Abundance = taxon_data[[trt]]
    )
  })
)

taxon_long$PlotTaxon <- unname(display_map[taxon_long$OriginalTaxon])
taxon_long$PlotTaxon[is.na(taxon_long$PlotTaxon)] <- "Remaining genera"

plot_abundance <- aggregate(
  Abundance ~ Treatment + PlotTaxon,
  data = taxon_long,
  FUN = sum
)
names(plot_abundance)[names(plot_abundance) == "PlotTaxon"] <- "Taxon"

# Left-to-right stacking order; final segment is the "Other" category.
stack_order <- c(
  "Remaining genera",
  "Rhodothermaceae",
  "Pedosphaeraceae",
  "Caulobacteraceae",
  "Shinella",
  "Actinomarinales",
  "Alphaproteobacteria",
  "Hydrogenophaga",
  "Erythrobacter",
  "Longimicrobiaceae",
  "Tepidisphaerales",
  "Roseateles",
  "Sphingomonas",
  "Pseudomonas",
  "Massilia",
  "Blastomonas",
  "Gemmatimonadota",
  "Salinimicrobium",
  "Pontibacter",
  "Lysobacter",
  "Other"
)

# Legend order mirrors the supplied figure.
legend_order <- c(
  "Other",
  "Lysobacter",
  "Blastomonas",
  "Pontibacter",
  "Salinimicrobium",
  "Gemmatimonadota",
  "Sphingomonas",
  "Massilia",
  "Pseudomonas",
  "Longimicrobiaceae",
  "Hydrogenophaga",
  "Erythrobacter",
  "Tepidisphaerales",
  "Actinomarinales",
  "Alphaproteobacteria",
  "Roseateles",
  "Caulobacteraceae",
  "Shinella",
  "Pedosphaeraceae",
  "Rhodothermaceae",
  "Remaining genera"
)

taxon_colours <- c(
  "Other" = "#DB9D85",
  "Lysobacter" = "#D0A374",
  "Blastomonas" = "#C2A968",
  "Pontibacter" = "#B1AF64",
  "Salinimicrobium" = "#9DB469",
  "Gemmatimonadota" = "#86B875",
  "Sphingomonas" = "#6DBC86",
  "Massilia" = "#53BE98",
  "Pseudomonas" = "#3DBEAB",
  "Longimicrobiaceae" = "#39BDBC",
  "Hydrogenophaga" = "#4CB9CC",
  "Erythrobacter" = "#69B4D8",
  "Tepidisphaerales" = "#87AEDD",
  "Actinomarinales" = "#A3A7E2",
  "Alphaproteobacteria" = "#BB9FE0",
  "Roseateles" = "#CD99D8",
  "Caulobacteraceae" = "#DA95CC",
  "Shinella" = "#E293BD",
  "Pedosphaeraceae" = "#E494AB",
  "Rhodothermaceae" = "#E29898",
  "Remaining genera" = "#D0D0D0"
)

plot_abundance$Taxon <- factor(plot_abundance$Taxon, levels = stack_order)
plot_abundance$Treatment <- factor(
  plot_abundance$Treatment,
  levels = c("PEG", "GP", "CK")
)
plot_abundance <- plot_abundance[
  order(plot_abundance$Treatment, plot_abundance$Taxon),
]

# Calculate explicit segment start/end positions to make the stacking order
# deterministic across R/ggplot2 versions.
plot_abundance$Y <- as.numeric(plot_abundance$Treatment)
plot_abundance$Start <- ave(
  plot_abundance$Abundance,
  plot_abundance$Treatment,
  FUN = function(z) c(0, head(cumsum(z), -1))
)
plot_abundance$End <- plot_abundance$Start + plot_abundance$Abundance

p_d <- ggplot(plot_abundance) +
  geom_rect(
    aes(
      xmin = Start,
      xmax = End,
      ymin = Y - 0.36,
      ymax = Y + 0.36,
      fill = Taxon
    ),
    colour = "white",
    linewidth = 0.22
  ) +
  scale_fill_manual(
    values = taxon_colours,
    breaks = legend_order,
    drop = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 25),
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = 1:3,
    labels = c("PEG", "GP", "CK"),
    limits = c(0.5, 3.5),
    expand = c(0, 0)
  ) +
  labs(
    x = "Relative abundance (%)",
    y = NULL,
    fill = "Taxon",
    tag = "(d)"
  ) +
  guides(
    fill = guide_legend(
      nrow = 5,
      byrow = TRUE,
      title = "Taxon",
      keywidth = grid::unit(0.24, "cm"),
      keyheight = grid::unit(0.20, "cm")
    )
  ) +
  common_theme +
  theme(
    axis.title.x = element_text(margin = margin(t = 5)),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 8.5),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 7.1, colour = "black"),
    legend.title = element_text(size = 8.5, colour = "black"),
    legend.key = element_rect(fill = "white", colour = NA),
    legend.background = element_blank(),
    legend.spacing.x = grid::unit(0.08, "cm"),
    legend.spacing.y = grid::unit(0.02, "cm"),
    legend.margin = margin(t = 1, r = 0, b = 0, l = 0),
    plot.margin = margin(4, 8, 2, 8)
  )

# -----------------------------------------------------------------------------
# 7. Assemble the four-panel figure
# -----------------------------------------------------------------------------
layout_design <- "ABC\nDDD"

figure_supp13 <- wrap_plots(
  A = p_a,
  B = p_b,
  C = p_c,
  D = p_d,
  design = layout_design,
  heights = c(1, 1.22)
) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )

# -----------------------------------------------------------------------------
# 8. Export figure and source tables
# -----------------------------------------------------------------------------
png_file <- file.path(out_dir, "Supplementary_Fig13_4panel_600dpi.png")
tiff_file <- file.path(out_dir, "Supplementary_Fig13_4panel_600dpi.tiff")
pdf_file <- file.path(out_dir, "Supplementary_Fig13_4panel_vector.pdf")

ggsave(
  filename = png_file,
  plot = figure_supp13,
  width = 15.5,
  height = 16.3,
  units = "cm",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = tiff_file,
  plot = figure_supp13,
  device = "tiff",
  width = 15.5,
  height = 16.3,
  units = "cm",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = pdf_file,
  plot = figure_supp13,
  device = "pdf",
  width = 15.5,
  height = 16.3,
  units = "cm",
  bg = "white"
)

write.csv(
  taxon_data,
  file.path(out_dir, "SourceData_TaxonRelativeAbundance.csv"),
  row.names = FALSE
)

write.csv(
  depth_data,
  file.path(out_dir, "PanelA_SequencingDepth_Summary.csv"),
  row.names = FALSE
)

write.csv(
  bray_data[, c("RowTreatment", "ColumnTreatment", "Distance")],
  file.path(out_dir, "PanelB_BrayCurtis_Summary.csv"),
  row.names = FALSE
)

write.csv(
  venn_counts,
  file.path(out_dir, "PanelC_VennRegionCounts_Summary.csv"),
  row.names = FALSE
)

caption <- paste(
  "Supplementary Fig. 13 | Sequencing depth and bacterial community composition",
  "under CK, GP and PEG treatments. (a) Sequencing depth of treatment-level",
  "composite samples. (b) Pairwise Bray-Curtis dissimilarity matrix.",
  "(c) Venn diagram showing shared and treatment-associated genera.",
  "(d) Relative abundance of dominant bacterial taxa; low-abundance taxa are",
  "grouped as Other/Remaining genera."
)

writeLines(
  caption,
  con = file.path(out_dir, "Supplementary_Fig13_Caption.txt"),
  useBytes = TRUE
)

# Print key checks and output paths.
cat("\nPanel (d) source-table totals (%):\n")
print(round(abundance_totals, 3))

cat("\nPanel (d) plotted totals (%), after grouping low-abundance taxa:\n")
print(
  round(
    tapply(plot_abundance$Abundance, plot_abundance$Treatment, sum),
    3
  )
)

cat("\nFigure files and supporting tables saved to:\n")
cat(normalizePath(out_dir), "\n")
cat("PNG:  ", normalizePath(png_file), "\n")
cat("TIFF: ", normalizePath(tiff_file), "\n")
cat("PDF:  ", normalizePath(pdf_file), "\n")

# Display figure in the active R graphics device.
print(figure_supp13)