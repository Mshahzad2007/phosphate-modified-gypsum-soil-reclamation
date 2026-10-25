# SUPPLEMENTARY FIG. S11 | VALIDATION ACROSS CONTRASTING SALINE-ALKALINE SOILS
rm(list = ls())
graphics.off()
cat("\014")

# =========================================================
# Combined validation figure (one A4 landscape page)
# Based on the existing validation.xlsx / validation workflow
#
# Final layout:
# (a) Left half  : plant photographs
#       - one larger Mongolian soil photo on top
#       - four smaller Xinjiang-site photos below (2 x 2)
# (b) Right-top  : Mongolian soil quantitative validation
#       - Plant height
#       - Biomass
#       - CK, GP, PEG bars with SD and significance brackets
# (c) Right-bottom: Four Xinjiang soils quantitative validation
#       - Plant height
#       - Biomass
#       - compact point ± SD plots across Xian 01–04
#       - note: CK showed no measurable growth
# =========================================================

# -----------------------------
# 1. Packages
# -----------------------------
need_pkgs <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "patchwork",
  "stringr", "purrr", "openxlsx", "cowplot", "grid",
  "jpeg", "png"
)

to_install <- need_pkgs[!sapply(need_pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}
invisible(lapply(need_pkgs, library, character.only = TRUE))

# -----------------------------
# 2. File information
# -----------------------------
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
file_name  <- repo_input_path("validation.xlsx")
sheet_name <- "validation"

if (!file.exists(file_name)) {
  stop("Validation workbook missing: data/validation.xlsx (or a local copy).")
}

available_sheets <- readxl::excel_sheets(file_name)
if (!sheet_name %in% available_sheets) {
  stop("Sheet not found. Your Excel file must contain a sheet named validation.")
}

out_dir <- file.path("outputs", "Fig_S11_Validation_Combined_A4")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# 3. Photo files (EDIT ONLY IF NEEDED)
# -----------------------------
# Optional local photographs are deliberately excluded from the repository.
# Without them, panels that show photographs remain blank; numerical panels
# continue to use the original measurements in data/validation.xlsx.
photo_dir <- if (dir.exists("validation_photos")) "validation_photos" else
             if (dir.exists(file.path("data", "validation_photos"))) file.path("data", "validation_photos") else "validation_photos"

photo_files <- c(
  "Mangolia" = file.path(photo_dir, "Mongolia.jpg"),
  "Xian 01"  = file.path(photo_dir, "Xian_01.jpg"),
  "Xian 02"  = file.path(photo_dir, "Xian_02.jpg"),
  "Xian 03"  = file.path(photo_dir, "Xian_03.jpg"),
  "Xian 04"  = file.path(photo_dir, "Xian_04.jpg")
)

if (!all(file.exists(photo_files))) {
  warning("Local validation photographs are absent. Photograph panels will be blank; numerical panels remain reproducible.", call. = FALSE)
}

# -----------------------------
# 4. Read data
# -----------------------------
dat_raw <- readxl::read_excel(file_name, sheet = sheet_name)

names(dat_raw) <- trimws(names(dat_raw))
names(dat_raw) <- gsub("\\s+", "_", names(dat_raw))
names(dat_raw) <- gsub("\\.", "_", names(dat_raw))

required_cols <- c(
  "Treatments",
  "Region",
  "Replicates",
  "response_variable",
  "Value"
)

missing_cols <- setdiff(required_cols, names(dat_raw))
if (length(missing_cols) > 0) {
  stop(paste(
    "Missing required column(s):",
    paste(missing_cols, collapse = ", ")
  ))
}

# -----------------------------
# 5. Data settings
# -----------------------------
treatment_order <- c("CK", "GP", "PEG")

trait_order_all <- c(
  "Height",
  "Root_dia",
  "Stem_dia",
  "Stem_DW",
  "Root_DW",
  "Biomass"
)

trait_keep_display <- c("Height", "Biomass")

trait_labels <- c(
  "Height"  = "Plant height (cm)",
  "Biomass" = "Biomass (g)"
)

# Initial properties of the four Xinjiang validation soils
# EC unit: mS cm^-1
# Site 01 EC was measured at 1:10 because the 1:5 extract exceeded the meter range.
# Sites 02-04 EC were measured at 1:5.
xinjiang_site_info <- c(
  "Xian 01" = "Site 01 (pH 8.5; EC 10.9)",
  "Xian 02" = "Site 02 (pH 7.9; EC 3.4)",
  "Xian 03" = "Site 03 (pH 8.4; EC 11.4)",
  "Xian 04" = "Site 04 (pH 8.3; EC 6.1)"
)

fill_cols <- c(
  "CK"  = "#D3D3D3",
  "GP"  = "#80B1D3",
  "PEG" = "#FDB462"
)

shape_cols <- c(
  "CK"  = "#D3D3D3",
  "GP"  = "#80B1D3",
  "PEG" = "#FDB462"
)

shape_border_cols <- c(
  "CK"  = "#777777",
  "GP"  = "#4E88B7",
  "PEG" = "#D88425"
)

shape_vals <- c(
  "CK"  = 21,
  "GP"  = 22,
  "PEG" = 24
)

# -----------------------------
# 6. Clean data
# -----------------------------
dat <- dat_raw %>%
  dplyr::mutate(
    Treatment = trimws(as.character(Treatments)),
    RegionRaw = trimws(as.character(Region)),
    Replicate = as.factor(Replicates),
    Trait = trimws(as.character(response_variable)),
    ValueNum = suppressWarnings(as.numeric(Value))
  ) %>%
  dplyr::filter(
    Treatment %in% treatment_order,
    Trait %in% trait_order_all,
    !is.na(ValueNum)
  ) %>%
  dplyr::mutate(
    RegionKey = dplyr::case_when(
      RegionRaw %in% c("Mangolia", "Mongolia") ~ "Mangolia",
      RegionRaw == "Xian 01" ~ "Xian 01",
      RegionRaw == "Xian 02" ~ "Xian 02",
      RegionRaw == "Xian 03" ~ "Xian 03",
      RegionRaw == "Xian 04" ~ "Xian 04",
      TRUE ~ RegionRaw
    )
  ) %>%
  dplyr::mutate(
    RegionTitle = dplyr::case_when(
      RegionKey == "Mangolia" ~ "Inner Mongolia soil (phosphogypsum-derived PEG)",
      RegionKey == "Xian 01"  ~ "Xinjiang Soil (desulfurized phosphogypsum-derived PEG)",
      RegionKey == "Xian 02"  ~ "Farm Land 01 (desulfurized phosphogypsum-derived PEG)",
      RegionKey == "Xian 03"  ~ "Farm Land 02 (desulfurized phosphogypsum-derived PEG)",
      RegionKey == "Xian 04"  ~ "Gobi Saline Soil (desulfurized phosphogypsum-derived PEG)",
      TRUE ~ RegionRaw
    ),
    RegionShort = dplyr::case_when(
      RegionKey == "Mangolia" ~ "Mongolia",
      RegionKey == "Xian 01"  ~ "Xinjiang",
      RegionKey == "Xian 02"  ~ "FL01",
      RegionKey == "Xian 03"  ~ "FL02",
      RegionKey == "Xian 04"  ~ "Gobi",
      TRUE ~ RegionRaw
    ),
    Treatment = factor(Treatment, levels = treatment_order),
    Trait = factor(Trait, levels = trait_order_all)
  )

# -----------------------------
# 7. Helper functions
# -----------------------------
p_to_label <- function(p) {
  if (is.na(p)) return("n.s")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  return("n.s")
}

safe_pair_p <- function(df_trait, g1, g2) {
  v1 <- df_trait %>% dplyr::filter(as.character(Treatment) == g1) %>% dplyr::pull(ValueNum)
  v2 <- df_trait %>% dplyr::filter(as.character(Treatment) == g2) %>% dplyr::pull(ValueNum)
  
  v1 <- v1[!is.na(v1)]
  v2 <- v2[!is.na(v2)]
  
  if (length(v1) < 2 || length(v2) < 2) return(NA_real_)
  
  m1 <- mean(v1); m2 <- mean(v2)
  s1 <- sd(v1); s2 <- sd(v2)
  if (is.na(s1)) s1 <- 0
  if (is.na(s2)) s2 <- 0
  
  if (s1 == 0 && s2 == 0) {
    if (abs(m1 - m2) < 1e-12) return(1) else return(0.00001)
  }
  
  pval <- tryCatch(stats::t.test(v1, v2)$p.value, error = function(e) NA_real_)
  if (is.na(pval)) {
    pval <- tryCatch(stats::wilcox.test(v1, v2, exact = FALSE)$p.value, error = function(e) NA_real_)
  }
  pval
}

make_pairwise_df <- function(df_trait) {
  present_trts <- intersect(treatment_order, unique(as.character(df_trait$Treatment)))
  if (length(present_trts) < 2) return(tibble::tibble())
  
  if (all(c("CK", "GP", "PEG") %in% present_trts)) {
    pairs_df <- tibble::tibble(group1 = c("CK", "GP", "CK"), group2 = c("GP", "PEG", "PEG"))
  } else if (all(c("CK", "PEG") %in% present_trts)) {
    pairs_df <- tibble::tibble(group1 = "CK", group2 = "PEG")
  } else if (all(c("GP", "PEG") %in% present_trts)) {
    pairs_df <- tibble::tibble(group1 = "GP", group2 = "PEG")
  } else {
    cmb <- combn(present_trts, 2, simplify = FALSE)
    pairs_df <- purrr::map_dfr(cmb, function(x) tibble::tibble(group1 = x[1], group2 = x[2]))
  }
  
  pairs_df$p_raw <- purrr::map2_dbl(pairs_df$group1, pairs_df$group2, ~ safe_pair_p(df_trait, .x, .y))
  if (nrow(pairs_df) > 1) pairs_df$p_adj <- p.adjust(pairs_df$p_raw, method = "holm") else pairs_df$p_adj <- pairs_df$p_raw
  pairs_df$label <- vapply(pairs_df$p_adj, p_to_label, character(1))
  pairs_df$x1 <- match(pairs_df$group1, present_trts)
  pairs_df$x2 <- match(pairs_df$group2, present_trts)
  pairs_df
}

make_summary_df <- function(df_trait) {
  present_trts <- intersect(treatment_order, unique(as.character(df_trait$Treatment)))
  df_trait %>%
    dplyr::filter(as.character(Treatment) %in% present_trts) %>%
    dplyr::mutate(Treatment = factor(as.character(Treatment), levels = present_trts)) %>%
    dplyr::group_by(Treatment) %>%
    dplyr::summarise(n = dplyr::n(), mean = mean(ValueNum, na.rm = TRUE), sd = sd(ValueNum, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(sd = ifelse(is.na(sd), 0, sd))
}

safe_read_image <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("jpg", "jpeg")) return(jpeg::readJPEG(path))
  if (ext == "png") return(png::readPNG(path))
  NULL
}

make_photo_plot <- function(region_key, bigger = FALSE) {
  path <- photo_files[[region_key]]
  
  if (is.null(path) || !file.exists(path)) {
    # Leave completely blank white space for manual photo insertion.
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = "white", colour = NA),
          panel.background = ggplot2::element_rect(fill = "white", colour = NA)
        )
    )
  }
  
  img <- safe_read_image(path)
  
  if (is.null(img)) {
    # Leave completely blank white space for manual photo insertion.
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = "white", colour = NA),
          panel.background = ggplot2::element_rect(fill = "white", colour = NA)
        )
    )
  }
  
  cowplot::ggdraw() +
    cowplot::draw_image(
      img,
      x = 0.01, y = 0.01,
      width = 0.98, height = 0.98,
      hjust = 0.5, vjust = 0.5
    )
}

make_site_header <- function(txt) {
  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::annotate(
      "text",
      x = 0.5, y = 0.48,
      label = txt,
      size = 3.2,
      fontface = "plain",
      colour = "black"
    ) +
    ggplot2::coord_cartesian(
      xlim = c(0, 1), ylim = c(0, 1),
      expand = FALSE
    ) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    )
}

make_photo_tile <- function(region_key, header_text) {
  header <- make_site_header(header_text)
  photo  <- make_photo_plot(region_key, bigger = FALSE)
  
  cowplot::plot_grid(
    header,
    photo,
    ncol = 1,
    rel_heights = c(0.11, 0.89),
    align = "v",
    axis = "lr"
  )
}

make_vertical_strip <- function(txt) {
  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
      fill = "#F2F2F2",
      colour = "black",
      linewidth = 0.40
    ) +
    ggplot2::annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = txt,
      angle = 90,
      size = 4.1,
      fontface = "plain",
      colour = "black"
    )
}

add_left_strip <- function(content_plot, strip_text, rel_widths = c(0.075, 0.925)) {
  cowplot::plot_grid(
    make_vertical_strip(strip_text),
    content_plot,
    nrow = 1,
    rel_widths = rel_widths,
    align = "h",
    axis = "tb"
  )
}

make_section_title <- function(txt) {
  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::geom_rect(ggplot2::aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1), fill = "#F2F2F2", colour = "black", linewidth = 0.40) +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = txt, size = 4.6, fontface = "plain", colour = "black")
}

make_mongolia_trait_plot <- function(df_region, trait_name) {
  df_trait <- df_region %>% dplyr::filter(Trait == trait_name) %>% dplyr::filter(!is.na(ValueNum)) %>% droplevels()
  if (nrow(df_trait) == 0) {
    return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "No data", size = 4))
  }
  
  sum_df <- make_summary_df(df_trait)
  present_trts <- intersect(treatment_order, unique(as.character(sum_df$Treatment)))
  df_trait <- df_trait %>% dplyr::mutate(Treatment = factor(as.character(Treatment), levels = present_trts))
  sum_df   <- sum_df   %>% dplyr::mutate(Treatment = factor(as.character(Treatment), levels = present_trts))
  bracket_df <- make_pairwise_df(df_trait)
  
  y_base <- max(sum_df$mean + sum_df$sd, na.rm = TRUE)
  if (!is.finite(y_base) || y_base <= 0) y_base <- 1
  
  if (nrow(bracket_df) > 0) {
    bracket_df <- bracket_df %>% dplyr::mutate(y = y_base * (1.17 + 0.18 * (dplyr::row_number() - 1)))
    y_lim <- max(c(sum_df$mean + sum_df$sd, bracket_df$y), na.rm = TRUE) * 1.15
  } else {
    y_lim <- y_base * 1.30
  }
  if (!is.finite(y_lim) || y_lim <= 0) y_lim <- 1
  
  p <- ggplot2::ggplot(sum_df, ggplot2::aes(x = Treatment, y = mean, fill = Treatment)) +
    ggplot2::geom_col(width = 0.62, colour = "black", linewidth = 0.45) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = pmax(mean - sd, 0), ymax = mean + sd), width = 0.14, linewidth = 0.45) +
    ggplot2::scale_fill_manual(values = fill_cols, drop = FALSE) +
    ggplot2::scale_y_continuous(limits = c(0, y_lim), expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(x = NULL, y = trait_labels[as.character(trait_name)], title = ifelse(as.character(trait_name) == "Height", "Plant height", "Biomass")) +
    ggplot2::theme_classic(base_size = 10) +
    ggplot2::theme(
      text = ggplot2::element_text(face = "plain", colour = "black"),
      plot.title = ggplot2::element_text(size = 10, hjust = 0.5, face = "plain"),
      axis.title.y = ggplot2::element_text(size = 9, margin = ggplot2::margin(r = 6)),
      axis.text.x  = ggplot2::element_text(size = 8),
      axis.text.y  = ggplot2::element_text(size = 8),
      axis.line = ggplot2::element_line(linewidth = 0.45),
      axis.ticks = ggplot2::element_line(linewidth = 0.45),
      legend.position = "none",
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(4, 6, 4, 8)
    )
  
  if (nrow(bracket_df) > 0) {
    tip_len <- 0.028 * y_lim
    for (i in seq_len(nrow(bracket_df))) {
      x1 <- bracket_df$x1[i]; x2 <- bracket_df$x2[i]; yy <- bracket_df$y[i]; lab <- bracket_df$label[i]
      p <- p +
        ggplot2::annotate("segment", x = x1, xend = x1, y = yy - tip_len, yend = yy, linewidth = 0.45) +
        ggplot2::annotate("segment", x = x2, xend = x2, y = yy - tip_len, yend = yy, linewidth = 0.45) +
        ggplot2::annotate("segment", x = x1, xend = x2, y = yy, yend = yy, linewidth = 0.45) +
        ggplot2::annotate("text", x = (x1 + x2) / 2, y = yy + 0.024 * y_lim, label = lab, size = 3.4, fontface = "plain", colour = "black")
    }
  }
  
  p
}

make_xinjiang_trait_plot <- function(df_xin, trait_name, show_legend = FALSE) {
  
  site_order <- c("Xian 01", "Xian 02", "Xian 03", "Xian 04")
  site_labels <- c(
    "Xian 01" = "Site 01",
    "Xian 02" = "Site 02",
    "Xian 03" = "Site 03",
    "Xian 04" = "Site 04"
  )
  
  # Use ONLY the actual observations present in validation.xlsx.
  # CK is NOT forced to zero.
  df_trait <- df_xin %>%
    dplyr::filter(Trait == trait_name) %>%
    dplyr::filter(!is.na(ValueNum)) %>%
    dplyr::filter(Treatment %in% c("CK", "GP", "PEG")) %>%
    droplevels()
  
  if (nrow(df_trait) == 0) {
    return(
      ggplot() +
        theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = "No data", size = 4)
    )
  }
  
  present_trts <- intersect(
    c("CK", "GP", "PEG"),
    unique(as.character(df_trait$Treatment))
  )
  
  sum_df <- df_trait %>%
    dplyr::group_by(RegionKey, RegionShort, Treatment) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(ValueNum, na.rm = TRUE),
      sd = sd(ValueNum, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      sd = ifelse(is.na(sd), 0, sd),
      RegionKey = factor(RegionKey, levels = site_order),
      Treatment = factor(as.character(Treatment), levels = present_trts)
    )
  
  y_upper <- max(sum_df$mean + sum_df$sd, na.rm = TRUE)
  if (!is.finite(y_upper) || y_upper <= 0) y_upper <- 1
  
  pd <- ggplot2::position_dodge(width = 0.42)
  
  ggplot2::ggplot(
    sum_df,
    ggplot2::aes(
      x = RegionKey,
      y = mean,
      shape = Treatment,
      fill = Treatment,
      colour = Treatment
    )
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = pmax(mean - sd, 0),
        ymax = mean + sd
      ),
      width = 0.10,
      linewidth = 0.45,
      position = pd,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(
      size = 3.0,
      stroke = 0.55,
      position = pd,
      na.rm = TRUE
    ) +
    ggplot2::scale_shape_manual(
      values = shape_vals[present_trts],
      breaks = present_trts,
      drop = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = shape_cols[present_trts],
      breaks = present_trts,
      drop = FALSE
    ) +
    ggplot2::scale_colour_manual(
      values = shape_border_cols[present_trts],
      breaks = present_trts,
      drop = FALSE
    ) +
    ggplot2::scale_x_discrete(labels = site_labels) +
    ggplot2::scale_y_continuous(
      limits = c(0, y_upper * 1.10),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::labs(
      x = NULL,
      y = trait_labels[as.character(trait_name)],
      title = NULL,
      fill = NULL,
      colour = NULL,
      shape = NULL
    ) +
    ggplot2::theme_classic(base_size = 10) +
    ggplot2::theme(
      text = ggplot2::element_text(face = "plain", colour = "black"),
      plot.title = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(
        size = 9,
        margin = ggplot2::margin(r = 6)
      ),
      axis.text.x = ggplot2::element_text(size = 8),
      axis.text.y = ggplot2::element_text(size = 8),
      axis.line = ggplot2::element_line(linewidth = 0.45),
      axis.ticks = ggplot2::element_line(linewidth = 0.45),
      panel.grid = ggplot2::element_blank(),
      legend.position = if (show_legend) "top" else "none",
      legend.direction = "horizontal",
      legend.text = ggplot2::element_text(size = 8),
      plot.margin = ggplot2::margin(4, 6, 4, 8)
    )
}

# -----------------------------
# 8. Data subsets
# -----------------------------
mongolia_df <- dat %>%
  dplyr::filter(RegionKey == "Mangolia", Trait %in% trait_keep_display) %>%
  droplevels()

xinjiang_df <- dat %>%
  dplyr::filter(RegionKey %in% c("Xian 01", "Xian 02", "Xian 03", "Xian 04"),
                Trait %in% trait_keep_display) %>%
  droplevels()

# -----------------------------
# 9. Panel (a): photo montage
# -----------------------------

# Inner Mongolia: large photo with no extra heading above it.
# The soil name is shown only in the vertical strip.
p_mong_photo <- make_photo_plot("Mangolia", bigger = TRUE)
mongolia_block <- add_left_strip(
  p_mong_photo,
  "Inner Mongolia soil",
  rel_widths = c(0.075, 0.925)
)

# Xinjiang: compact site titles directly attached to each photo.
p_x1_photo <- make_photo_tile(
  "Xian 01",
  xinjiang_site_info[["Xian 01"]]
)

p_x2_photo <- make_photo_tile(
  "Xian 02",
  xinjiang_site_info[["Xian 02"]]
)

p_x3_photo <- make_photo_tile(
  "Xian 03",
  xinjiang_site_info[["Xian 03"]]
)

p_x4_photo <- make_photo_tile(
  "Xian 04",
  xinjiang_site_info[["Xian 04"]]
)

# Automatically divide the four Xinjiang site panels into a 2 x 2 grid.
# cowplot is used here instead of patchwork to avoid wrap_dims() errors.
xinj_photo_grid <- cowplot::plot_grid(
  p_x1_photo,
  p_x2_photo,
  p_x3_photo,
  p_x4_photo,
  ncol = 2,
  nrow = 2,
  rel_widths = c(1, 1),
  rel_heights = c(1, 1),
  align = "hv",
  axis = "tblr"
)

xinjiang_block <- add_left_strip(
  xinj_photo_grid,
  "Xinjiang soil",
  rel_widths = c(0.075, 0.925)
)

# Automatically split Panel (a) vertically:
# ~1/3 for Inner Mongolia and ~2/3 for the four Xinjiang sites.
photo_content <- cowplot::plot_grid(
  mongolia_block,
  xinjiang_block,
  ncol = 1,
  rel_heights = c(0.78, 1.72),
  align = "v",
  axis = "lr"
)

panel_a <- cowplot::ggdraw() +
  cowplot::draw_plot(
    photo_content,
    x = 0.02, y = 0.00,
    width = 0.98, height = 0.96
  )

# -----------------------------
# 10. Panel (b): Inner Mongolia soil
# -----------------------------
p_mong_h <- make_mongolia_trait_plot(mongolia_df, "Height")
p_mong_b <- make_mongolia_trait_plot(mongolia_df, "Biomass")

mongolia_body <- p_mong_h + p_mong_b + patchwork::plot_layout(ncol = 2)
mongolia_title <- make_section_title("Inner Mongolia soil")
panel_b_core <- mongolia_title / mongolia_body + patchwork::plot_layout(heights = c(0.14, 1.00))

panel_b <- cowplot::ggdraw() +
  cowplot::draw_plot(panel_b_core, x = 0.04, y = 0.00, width = 0.96, height = 0.96)

# -----------------------------
# 11. Panel (c): Four Xinjiang soils
# -----------------------------
p_xin_h <- make_xinjiang_trait_plot(xinjiang_df, "Height", show_legend = TRUE)
p_xin_b <- make_xinjiang_trait_plot(xinjiang_df, "Biomass", show_legend = FALSE)

xin_title <- make_section_title("Four Xinjiang soils")
panel_c_core <- xin_title / p_xin_h / p_xin_b +
  patchwork::plot_layout(heights = c(0.13, 0.50, 0.50))

panel_c <- cowplot::ggdraw() +
  cowplot::draw_plot(panel_c_core, x = 0.04, y = 0.00, width = 0.96, height = 0.96)

# -----------------------------
# 12. Assemble right half
# -----------------------------
right_column <- panel_b / panel_c + patchwork::plot_layout(heights = c(0.86, 1.14))

# -----------------------------
# 13. Final figure
# -----------------------------
final_plot <- panel_a + right_column + patchwork::plot_layout(widths = c(0.48, 0.52))

print(final_plot)

# -----------------------------
# 14. Save figure
# -----------------------------
fig_w <- 11.69
fig_h <- 8.27

ggplot2::ggsave(
  filename = file.path(out_dir, "Fig_S11_Validation_Combined_A4.png"),
  plot = final_plot, width = fig_w, height = fig_h, dpi = 600, bg = "white"
)

ggplot2::ggsave(
  filename = file.path(out_dir, "Fig_S11_Validation_Combined_A4.pdf"),
  plot = final_plot, width = fig_w, height = fig_h, bg = "white"
)

ggplot2::ggsave(
  filename = file.path(out_dir, "Fig_S11_Validation_Combined_A4.tiff"),
  plot = final_plot, width = fig_w, height = fig_h, dpi = 600, compression = "lzw", bg = "white"
)

# -----------------------------
# 15. Export useful statistics
# -----------------------------
mongolia_summary <- mongolia_df %>%
  dplyr::group_by(Trait, Treatment) %>%
  dplyr::summarise(
    n = dplyr::n(), Mean = mean(ValueNum, na.rm = TRUE),
    SD = sd(ValueNum, na.rm = TRUE), SE = SD / sqrt(n), .groups = "drop"
  )

mongolia_pairwise <- mongolia_df %>%
  dplyr::group_by(Trait) %>%
  dplyr::group_modify(~ {
    out <- make_pairwise_df(.x)
    if (nrow(out) == 0) return(tibble::tibble())
    out %>% dplyr::select(group1, group2, p_raw, p_adj, label)
  }) %>%
  dplyr::ungroup()

xinjiang_summary <- xinjiang_df %>%
  dplyr::filter(Treatment %in% c("CK", "GP", "PEG")) %>%
  dplyr::group_by(RegionTitle, Trait, Treatment) %>%
  dplyr::summarise(
    n = dplyr::n(),
    Mean = mean(ValueNum, na.rm = TRUE),
    SD = sd(ValueNum, na.rm = TRUE),
    SE = SD / sqrt(n),
    .groups = "drop"
  )


site_properties <- data.frame(
  Site = c("Xinjiang site 01", "Xinjiang site 02", "Xinjiang site 03", "Xinjiang site 04"),
  Soil = c("Xinjiang soil", "Farm Land 01", "Farm Land 02", "Gobi saline soil"),
  pH = c(8.5, 7.9, 8.4, 8.3),
  EC_mS_cm = c(10.9, 3.4, 11.4, 6.1),
  EC_soil_water_ratio = c("1:10", "1:5", "1:5", "1:5"),
  stringsAsFactors = FALSE
)

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Clean_Data_Used")
openxlsx::writeData(wb, "Clean_Data_Used", dat)

openxlsx::addWorksheet(wb, "Mongolia_Summary")
openxlsx::writeData(wb, "Mongolia_Summary", mongolia_summary)

openxlsx::addWorksheet(wb, "Mongolia_Pairwise")
openxlsx::writeData(wb, "Mongolia_Pairwise", mongolia_pairwise)

openxlsx::addWorksheet(wb, "Xinjiang_Summary")
openxlsx::writeData(wb, "Xinjiang_Summary", xinjiang_summary)

openxlsx::addWorksheet(wb, "Xinjiang_Soil_Properties")
openxlsx::writeData(wb, "Xinjiang_Soil_Properties", site_properties)

openxlsx::addWorksheet(wb, "Note")
openxlsx::writeData(
  wb, "Note",
  x = data.frame(
    Note = c(
      "Combined one-page validation figure.",
      "Panel (b): Inner Mongolia soil shows CK, GP, PEG for Height and Biomass.",
      "Panel (c): Four Xinjiang soils are shown as compact site-level validation plots.",
      "Panel (c) uses the actual measured CK/GP/PEG values present in validation.xlsx; CK is not forced to zero.",
      "Initial Xinjiang soil properties: Site 01 pH 8.5, EC 10.9 mS cm^-1; Site 02 pH 7.9, EC 3.4; Site 03 pH 8.4, EC 11.4; Site 04 pH 8.3, EC 6.1.",
      "EC was measured at a 1:5 soil:water ratio for Sites 02-04; Site 01 used 1:10 because the 1:5 extract exceeded the meter range."
    )
  )
)

openxlsx::saveWorkbook(
  wb,
  file = file.path(out_dir, "Fig_S11_Validation_Combined_A4_statistics.xlsx"),
  overwrite = TRUE
)

cat("\nDone.\n")
cat("Combined one-page validation figure and statistics saved in folder:\n")
cat(normalizePath(out_dir), "\n")