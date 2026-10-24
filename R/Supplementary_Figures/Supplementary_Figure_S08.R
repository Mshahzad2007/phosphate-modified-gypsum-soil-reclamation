
rm(list = ls())
graphics.off()
cat("\014")

## ============================================================
## SUPPLEMENTARY FIG. 8 — FINAL INDICES
##
## A) fCF = CF / RC
## B) RCA = CF / (CF + CaCO3-Ca)
##
## CF = Ca_F3
## RC = Reactive_Ca
##
## IMPORTANT:
## CaCO3 is assumed to be stored in the Excel file as %
## and is converted to Ca-equivalent cmolc kg^-1 before RCA.
##
## Figure:
## - CK, GP, PEG only
## - mean ± SE
## - raw replicate points
## - Tukey HSD CLD letters
## - no bold text
## - short y-axis labels
## - 120 × 60 mm
## ============================================================


## ============================================================
## 0. PACKAGES
## ============================================================

pkgs <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "tibble",
  "ggplot2",
  "multcompView",
  "patchwork",
  "grid",
  "writexl"
)

for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

invisible(
  lapply(
    pkgs,
    library,
    character.only = TRUE
  )
)


## ============================================================
## 1. INPUT / OUTPUT
## ============================================================

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

file_path  <- repo_input_path("Conventional_Treatments.xlsx")
sheet_name <- "data"

out_dir <- "Supplementary_Fig8_Indices"

dir.create(
  out_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

treat_levels <- c(
  "CK",
  "GP",
  "PEG"
)

treat_cols <- c(
  "CK"  = "#E64B35",
  "GP"  = "#4DBBD5",
  "PEG" = "#3C5488"
)


## ============================================================
## 2. READ LONG-FORMAT DATA
## ============================================================

dat_long <- readxl::read_excel(
  file_path,
  sheet = sheet_name
) %>%
  
  dplyr::mutate(
    
    Treatments = stringr::str_trim(
      as.character(Treatments)
    ),
    
    Replicates = factor(
      Replicates
    ),
    
    response.varibale = stringr::str_trim(
      as.character(response.varibale)
    ),
    
    ## Excel column is "Value" with capital V
    value = suppressWarnings(
      as.numeric(Value)
    )
  ) %>%
  
  dplyr::mutate(
    Treatments = dplyr::recode(
      Treatments,
      "PGP" = "PEG"
    )
  ) %>%
  
  dplyr::filter(
    !is.na(Treatments),
    !is.na(Replicates),
    !is.na(response.varibale),
    !is.na(value),
    Treatments %in% treat_levels
  ) %>%
  
  dplyr::mutate(
    Treatments = factor(
      Treatments,
      levels = treat_levels
    )
  )


## ============================================================
## 3. REQUIRED VARIABLES
## ============================================================

need_vars <- c(
  "Reactive_Ca",
  "Ca_F3",
  "CaCO3"
)

missing_vars <- setdiff(
  need_vars,
  unique(dat_long$response.varibale)
)

if (length(missing_vars) > 0) {
  
  cat("\nAvailable variables:\n")
  
  print(
    sort(
      unique(dat_long$response.varibale)
    )
  )
  
  stop(
    paste0(
      "\nMissing required variables: ",
      paste(
        missing_vars,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


## ============================================================
## 4. CONVERT TO WIDE FORMAT
## ============================================================

dat_wide <- dat_long %>%
  
  dplyr::filter(
    response.varibale %in% need_vars
  ) %>%
  
  dplyr::group_by(
    Treatments,
    Replicates,
    response.varibale
  ) %>%
  
  dplyr::summarise(
    value = mean(
      value,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  
  tidyr::pivot_wider(
    names_from = response.varibale,
    values_from = value
  )


## ============================================================
## 5. CALCULATE INDICES
## ============================================================
##
## A) fCF
##
##       CF
## fCF = ----
##       RC
##
##
## B) RCA
##
##              CF
## RCA = ----------------
##       CF + CaCO3-Ca
##
##
## CaCO3 conversion:
##
## CaCO3 (%) → Ca-equivalent cmolc kg^-1
##
## 1% CaCO3 = 10 g CaCO3 kg^-1
##
## CaCO3 molecular weight = 100.0869 g mol^-1
##
## Ca2+ has 2 equivalents per mole.
##
## ============================================================

MW_CACO3 <- 100.0869

dat_idx <- dat_wide %>%
  
  dplyr::mutate(
    
    ## avoid division by zero
    RC_safe = dplyr::na_if(
      Reactive_Ca,
      0
    ),
    
    ## --------------------------------------------------------
    ## Convert CaCO3 (%) to Ca-equivalent cmolc kg^-1
    ## --------------------------------------------------------
    CaCO3_Ca = (
      CaCO3 * 10 / MW_CACO3
    ) * 200,
    
    ## --------------------------------------------------------
    ## A) OM-associated Ca relative to reactive Ca
    ## --------------------------------------------------------
    fCF = Ca_F3 / RC_safe,
    
    ## --------------------------------------------------------
    ## B) Relative Ca allocation to OM-associated pool
    ## --------------------------------------------------------
    RCA = Ca_F3 / (
      Ca_F3 + CaCO3_Ca
    )
  ) %>%
  
  dplyr::select(
    Treatments,
    Replicates,
    Reactive_Ca,
    Ca_F3,
    CaCO3,
    CaCO3_Ca,
    fCF,
    RCA
  )


## ============================================================
## 6. LONG FORMAT FOR PLOTTING
## ============================================================

idx_long <- dat_idx %>%
  
  tidyr::pivot_longer(
    cols = c(
      fCF,
      RCA
    ),
    names_to = "Index",
    values_to = "Value"
  ) %>%
  
  dplyr::filter(
    is.finite(Value)
  ) %>%
  
  dplyr::mutate(
    Index = factor(
      Index,
      levels = c(
        "fCF",
        "RCA"
      )
    )
  )


## ============================================================
## 7. SUMMARY — MEAN, SD, SE
## ============================================================

sum_df <- idx_long %>%
  
  dplyr::group_by(
    Index,
    Treatments
  ) %>%
  
  dplyr::summarise(
    
    n = dplyr::n(),
    
    mean = mean(
      Value,
      na.rm = TRUE
    ),
    
    sd = stats::sd(
      Value,
      na.rm = TRUE
    ),
    
    se = sd / sqrt(n),
    
    .groups = "drop"
  )


## ============================================================
## 8. ANOVA + TUKEY + CLD
## ============================================================

get_cld <- function(df) {
  
  fit <- stats::aov(
    Value ~ Treatments,
    data = df
  )
  
  tk <- stats::TukeyHSD(
    fit
  )
  
  letters_obj <- multcompView::multcompLetters4(
    fit,
    tk
  )
  
  tibble::tibble(
    
    Treatments = names(
      letters_obj$Treatments$Letters
    ),
    
    CLD = unname(
      letters_obj$Treatments$Letters
    )
  )
}


cld_df <- idx_long %>%
  
  dplyr::group_by(
    Index
  ) %>%
  
  dplyr::group_modify(
    ~ get_cld(.x)
  ) %>%
  
  dplyr::ungroup() %>%
  
  dplyr::mutate(
    Treatments = factor(
      Treatments,
      levels = treat_levels
    )
  )


## ============================================================
## 9. CLD LABEL POSITION
## ============================================================

offset_df <- sum_df %>%
  
  dplyr::group_by(
    Index
  ) %>%
  
  dplyr::summarise(
    
    ymax = max(
      mean + se,
      na.rm = TRUE
    ),
    
    ymin = min(
      mean - se,
      na.rm = TRUE
    ),
    
    range_y = ymax - ymin,
    
    offset = dplyr::if_else(
      range_y > 0,
      0.10 * range_y,
      0.05
    ),
    
    .groups = "drop"
  )


label_df <- sum_df %>%
  
  dplyr::left_join(
    offset_df,
    by = "Index"
  ) %>%
  
  dplyr::mutate(
    label_y = mean + se + offset
  ) %>%
  
  dplyr::left_join(
    cld_df,
    by = c(
      "Index",
      "Treatments"
    )
  )


## ============================================================
## 10. PLOT FUNCTION
## ============================================================

plot_index <- function(
    index_name,
    y_label,
    panel_tag
) {
  
  raw_plot <- idx_long %>%
    dplyr::filter(
      Index == index_name
    )
  
  mean_plot <- sum_df %>%
    dplyr::filter(
      Index == index_name
    )
  
  letters_plot <- label_df %>%
    dplyr::filter(
      Index == index_name
    )
  
  
  ggplot2::ggplot() +
    
    ## --------------------------------------------------------
  ## Mean bars
  ## --------------------------------------------------------
  ggplot2::geom_col(
    data = mean_plot,
    ggplot2::aes(
      x = Treatments,
      y = mean,
      fill = Treatments
    ),
    width = 0.48,
    colour = "black",
    linewidth = 0.35
  ) +
    
    ## --------------------------------------------------------
  ## SE
  ## --------------------------------------------------------
  ggplot2::geom_errorbar(
    data = mean_plot,
    ggplot2::aes(
      x = Treatments,
      ymin = mean - se,
      ymax = mean + se
    ),
    width = 0.12,
    linewidth = 0.42,
    colour = "black"
  ) +
    
    ## --------------------------------------------------------
  ## Individual replicates
  ## --------------------------------------------------------
  ggplot2::geom_jitter(
    data = raw_plot,
    ggplot2::aes(
      x = Treatments,
      y = Value
    ),
    width = 0.05,
    size = 1.45,
    shape = 21,
    fill = "white",
    colour = "black",
    stroke = 0.25,
    alpha = 0.90
  ) +
    
    ## --------------------------------------------------------
  ## Tukey CLD
  ## --------------------------------------------------------
  ggplot2::geom_text(
    data = letters_plot,
    ggplot2::aes(
      x = Treatments,
      y = label_y,
      label = CLD
    ),
    inherit.aes = FALSE,
    size = 3.4,
    fontface = "plain",
    colour = "black"
  ) +
    
    ggplot2::scale_fill_manual(
      values = treat_cols,
      limits = treat_levels,
      drop = FALSE
    ) +
    
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(
        mult = c(
          0,
          0.18
        )
      )
    ) +
    
    ggplot2::labs(
      x = NULL,
      y = y_label,
      tag = panel_tag
    ) +
    
    ggplot2::coord_cartesian(
      clip = "off"
    ) +
    
    ggplot2::theme_classic(
      base_size = 10
    ) +
    
    ggplot2::theme(
      
      legend.position = "none",
      
      ## ------------------------------------------------------
      ## No bold text
      ## ------------------------------------------------------
      text = ggplot2::element_text(
        face = "plain",
        colour = "black"
      ),
      
      axis.text.x = ggplot2::element_text(
        size = 9.5,
        colour = "black",
        face = "plain"
      ),
      
      axis.text.y = ggplot2::element_text(
        size = 9,
        colour = "black",
        face = "plain"
      ),
      
      ## ------------------------------------------------------
      ## SHORT Y-AXIS LABEL
      ## ------------------------------------------------------
      axis.title.y = ggplot2::element_text(
        size = 10,
        colour = "black",
        face = "plain",
        margin = ggplot2::margin(
          r = 5
        )
      ),
      
      axis.title.x = ggplot2::element_blank(),
      
      axis.line = ggplot2::element_line(
        linewidth = 0.55,
        colour = "black"
      ),
      
      axis.ticks = ggplot2::element_line(
        linewidth = 0.50,
        colour = "black"
      ),
      
      axis.ticks.length = grid::unit(
        1.3,
        "mm"
      ),
      
      ## ------------------------------------------------------
      ## PANEL TAG
      ## ------------------------------------------------------
      plot.tag = ggplot2::element_text(
        size = 11,
        face = "bold",
        colour = "black"
      ),
      
      plot.tag.position = c(
        -0.06,
        1.02
      ),
      
      plot.margin = ggplot2::margin(
        7,
        6,
        5,
        10,
        "mm"
      )
    )
}


## ============================================================
## 11. BUILD PANELS
## ============================================================

pA <- plot_index(
  index_name = "fCF",
  y_label = expression(
    italic(f)[CF]
  ),
  panel_tag = "(a)"
)

pB <- plot_index(
  index_name = "RCA",
  y_label = "RCA",
  panel_tag = "(b)"
)


## ============================================================
## 12. COMBINE
## ============================================================

fig_indices <- pA + pB +
  
  patchwork::plot_layout(
    nrow = 1,
    widths = c(
      1,
      1
    )
  )

print(
  fig_indices
)


## ============================================================
## 13. SAVE FIGURE
## ============================================================
##
## Compact two-panel supplementary figure:
## 120 mm × 60 mm
##
## ============================================================

fig_width_mm  <- 120
fig_height_mm <- 60


png_file <- file.path(
  out_dir,
  "Supplementary_Fig8_fCF_RCA.png"
)

tiff_file <- file.path(
  out_dir,
  "Supplementary_Fig8_fCF_RCA.tiff"
)

pdf_file <- file.path(
  out_dir,
  "Supplementary_Fig8_fCF_RCA.pdf"
)


## ------------------------------------------------------------
## PNG — 600 dpi
## ------------------------------------------------------------

ggplot2::ggsave(
  filename = png_file,
  plot = fig_indices,
  width = fig_width_mm,
  height = fig_height_mm,
  units = "mm",
  dpi = 600,
  bg = "white"
)


## ------------------------------------------------------------
## TIFF — 600 dpi
## ------------------------------------------------------------

ggplot2::ggsave(
  filename = tiff_file,
  plot = fig_indices,
  width = fig_width_mm,
  height = fig_height_mm,
  units = "mm",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)


## ------------------------------------------------------------
## PDF
## ------------------------------------------------------------

ggplot2::ggsave(
  filename = pdf_file,
  plot = fig_indices,
  width = fig_width_mm,
  height = fig_height_mm,
  units = "mm",
  device = grDevices::cairo_pdf,
  bg = "white"
)


## ============================================================
## 14. EXPORT ANOVA + TUKEY
## ============================================================

get_anova_tukey <- function(df) {
  
  idx_name <- unique(
    as.character(df$Index)
  )
  
  fit <- stats::aov(
    Value ~ Treatments,
    data = df
  )
  
  ## ANOVA
  anova_tab <- as.data.frame(
    summary(fit)[[1]]
  )
  
  anova_tab$Term <- rownames(
    anova_tab
  )
  
  rownames(anova_tab) <- NULL
  
  anova_tab$Index <- idx_name
  
  
  ## Tukey
  tukey_tab <- as.data.frame(
    stats::TukeyHSD(
      fit
    )$Treatments
  )
  
  tukey_tab$Comparison <- rownames(
    tukey_tab
  )
  
  rownames(tukey_tab) <- NULL
  
  tukey_tab$Index <- idx_name
  
  
  list(
    ANOVA = anova_tab,
    Tukey = tukey_tab
  )
}


stats_list <- idx_long %>%
  dplyr::group_split(
    Index
  ) %>%
  lapply(
    get_anova_tukey
  )


anova_export <- dplyr::bind_rows(
  lapply(
    stats_list,
    `[[`,
    "ANOVA"
  )
) %>%
  dplyr::select(
    Index,
    Term,
    dplyr::everything()
  )


tukey_export <- dplyr::bind_rows(
  lapply(
    stats_list,
    `[[`,
    "Tukey"
  )
) %>%
  dplyr::select(
    Index,
    Comparison,
    dplyr::everything()
  )


## ============================================================
## 15. EXPORT EXCEL SOURCE + STATISTICS
## ============================================================

excel_file <- file.path(
  out_dir,
  "Supplementary_Fig8_fCF_RCA_statistics.xlsx"
)


writexl::write_xlsx(
  
  list(
    
    ## Input + calculated indices
    Raw_and_Derived = dat_idx,
    
    ## Values used in figure
    Figure_Data = idx_long,
    
    ## Mean ± SD ± SE
    Summary = sum_df,
    
    ## Compact-letter display
    CLD = cld_df,
    
    ## ANOVA
    ANOVA = anova_export,
    
    ## Tukey HSD
    Tukey_HSD = tukey_export
  ),
  
  path = excel_file
)


## ============================================================
## 16. PRINT RESULTS
## ============================================================

cat("\n============================================\n")
cat("Supplementary Fig. 8 completed\n")
cat("============================================\n\n")

cat("Indices:\n")
cat("A) fCF = CF / RC\n")
cat("B) RCA = CF / (CF + CaCO3-Ca)\n\n")

cat("Figure size: 120 × 60 mm\n")
cat("PNG/TIFF resolution: 600 dpi\n\n")

cat("Summary statistics:\n")
print(sum_df)

cat("\nSaved files:\n")
cat("PNG:  ", normalizePath(png_file,  winslash = "/", mustWork = FALSE), "\n")
cat("TIFF: ", normalizePath(tiff_file, winslash = "/", mustWork = FALSE), "\n")
cat("PDF:  ", normalizePath(pdf_file,  winslash = "/", mustWork = FALSE), "\n")
cat("Excel:", normalizePath(excel_file, winslash = "/", mustWork = FALSE), "\n")