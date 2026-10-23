#14/10/26 =========================================================
# FIG. 6 FINAL REBUILT VERSION
# Final polishing:
# - safe font
# - thicker 2D-COS intensity bars
# - removed synchronous intensity title
# - adjusted panel (c) spacing
# - panel (a) MAOP legend repositioned
# - FTIR y-axis label: Absorbance
# =========================================================

rm(list = ls())
graphics.off()
cat("\014")

# =========================================================
# FIG. 6 — NC PORTRAIT LAYOUT
#
# Layout
#   (a) MCF vs RC decrease | (b) marginal beta effects
#   (c) full-width 2D-COS block
#   (d) PEG (POM) FTIR (1700–500 cm−1)
#   (e) PEG (MAOM) FTIR (1700–500 cm−1)
# =========================================================

# -----------------------------
# 1. Packages
# -----------------------------
pkg_needed <- c(
  "readxl", "dplyr", "tidyr", "stringr", "tibble",
  "ggplot2", "patchwork", "scales", "grid",
  "vegan", "openxlsx", "cowplot", "Matrix",
  "writexl", "gridExtra"
)

pkg_new <- pkg_needed[!(pkg_needed %in% installed.packages()[, "Package"])]
if (length(pkg_new)) install.packages(pkg_new)

invisible(lapply(pkg_needed, library, character.only = TRUE))

# -----------------------------
# 2. User paths
# -----------------------------
file_path  <- "Conventional_Treatments.xlsx"
sheet_data <- "data"

data_dir <- "E:/PHD/Traditional_Amendments/FTIR_WSMP"
out_dir  <- file.path(data_dir, "Fig6_NC_Portrait")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# -----------------------------
# 3. Global settings
# -----------------------------
treat_levels <- c("CK", "GP", "PEG")
B_BOOT <- 1200
base_font <- "sans"

# -----------------------------
# 4. Helpers
# -----------------------------
clean_var <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x <- stringr::str_replace_all(x, "\u00A0", "")
  x <- stringr::str_replace_all(x, "\\s+", "")
  x
}

clean_treatment <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x <- dplyr::recode(x, "PGP" = "PEG")
  factor(x, levels = treat_levels)
}

pick_col <- function(df, choices) {
  hit <- choices[choices %in% names(df)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

theme_maom <- ggplot2::theme_classic(base_size = 11.3, base_family = base_font) +
  ggplot2::theme(
    panel.grid        = ggplot2::element_blank(),
    axis.line         = ggplot2::element_line(linewidth = 0.55, colour = "black"),
    axis.ticks        = ggplot2::element_line(linewidth = 0.50, colour = "black"),
    axis.ticks.length = grid::unit(1.6, "mm"),
    axis.title        = ggplot2::element_text(size = 10.8, face = "plain", colour = "black"),
    axis.text         = ggplot2::element_text(size = 9.7, face = "plain", colour = "black"),
    plot.title        = ggplot2::element_blank(),
    plot.margin       = ggplot2::margin(4, 5, 4, 5),
    legend.title      = ggplot2::element_text(size = 9.6, face = "plain", colour = "black"),
    legend.text       = ggplot2::element_text(size = 9.1, face = "plain", colour = "black"),
    panel.background  = ggplot2::element_rect(fill = "white", colour = NA),
    plot.background   = ggplot2::element_rect(fill = "white", colour = NA)
  )

# =========================================================
# PART A : PANELS (a) and (b)
# =========================================================

# -----------------------------
# 5. Read soil data
# -----------------------------
if (!file.exists(file_path)) {
  stop("File not found: ", file_path)
}

soil_raw <- readxl::read_excel(file_path, sheet = sheet_data)

trt_col <- pick_col(soil_raw, c("Treatments", "Treatment", "treatments", "treatment"))
rep_col <- pick_col(soil_raw, c("Replicates", "Replicate", "replicates", "replicate"))
resp_col <- pick_col(
  soil_raw,
  c("response.varibale", "response.variable", "Response", "Variable", "response")
)
val_col <- pick_col(soil_raw, c("value", "Value", "values", "Values"))

if (is.na(trt_col) || is.na(rep_col) || is.na(resp_col) || is.na(val_col)) {
  stop(
    "The data sheet must contain treatment, replicate, response variable, and value columns.\n",
    "Columns found: ", paste(names(soil_raw), collapse = ", ")
  )
}

soil_long <- soil_raw %>%
  dplyr::rename(
    Treatments_raw = dplyr::all_of(trt_col),
    Replicates_raw = dplyr::all_of(rep_col),
    response_raw   = dplyr::all_of(resp_col),
    value_raw      = dplyr::all_of(val_col)
  ) %>%
  dplyr::mutate(
    Treatments   = clean_treatment(Treatments_raw),
    Replicates   = as.character(Replicates_raw),
    response_var = clean_var(response_raw),
    value        = suppressWarnings(as.numeric(value_raw)),
    SampleID     = paste(as.character(Treatments), Replicates, sep = "_")
  ) %>%
  dplyr::filter(
    !is.na(Treatments),
    !is.na(Replicates),
    !is.na(response_var),
    !is.na(value)
  )

soil_wide_all <- soil_long %>%
  dplyr::select(SampleID, Treatments, Replicates, response_var, value) %>%
  tidyr::pivot_wider(
    names_from  = response_var,
    values_from = value,
    values_fn   = mean
  ) %>%
  dplyr::arrange(Treatments, suppressWarnings(as.numeric(Replicates)))

if (!"CAI" %in% names(soil_wide_all)) {
  if (all(c("Reactive_Ca", "CaCO3") %in% names(soil_wide_all))) {
    soil_wide_all <- soil_wide_all %>%
      dplyr::mutate(CAI = Reactive_Ca / CaCO3)
  }
}

# -----------------------------
# 6. MAOM data
# -----------------------------
needed_maom <- c("Reactive_Ca", "MAOM_C", "MAOM_Ca_F3", "MAOM_P")
missing_maom <- setdiff(needed_maom, names(soil_wide_all))
if (length(missing_maom) > 0) {
  stop("Missing MAOM variables: ", paste(missing_maom, collapse = ", "))
}

df_maom <- soil_wide_all %>%
  dplyr::select(SampleID, Treatments, Replicates, dplyr::all_of(needed_maom)) %>%
  dplyr::filter(stats::complete.cases(dplyr::across(dplyr::all_of(needed_maom)))) %>%
  dplyr::mutate(
    RC_decrease = max(Reactive_Ca, na.rm = TRUE) - Reactive_Ca
  )

# -----------------------------
# 7. Model helpers
# -----------------------------
boot_pred_ribbon <- function(formula, data, newdata, B = 1200, seed = 1) {
  set.seed(seed)
  n <- nrow(data)
  preds <- matrix(NA_real_, nrow = B, ncol = nrow(newdata))
  
  for (b in seq_len(B)) {
    idx <- sample.int(n, n, replace = TRUE)
    fit <- try(lm(formula, data = data[idx, , drop = FALSE]), silent = TRUE)
    if (inherits(fit, "try-error")) next
    preds[b, ] <- suppressWarnings(as.numeric(predict(fit, newdata = newdata)))
  }
  
  fit0 <- lm(formula, data = data)
  mu <- suppressWarnings(as.numeric(predict(fit0, newdata = newdata)))
  
  lwr <- apply(preds, 2, function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 20) return(NA_real_)
    stats::quantile(x, 0.025, na.rm = TRUE)
  })
  
  upr <- apply(preds, 2, function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 20) return(NA_real_)
    stats::quantile(x, 0.975, na.rm = TRUE)
  })
  
  dplyr::bind_cols(
    newdata,
    tibble::tibble(fit = mu, lwr = lwr, upr = upr)
  )
}

std_data <- function(dat, cols) {
  d <- dat
  d[cols] <- lapply(d[cols], function(z) as.numeric(scale(z)))
  d
}

boot_coef_ci <- function(formula, data, B = 1200, seed = 1) {
  set.seed(seed)
  
  fit0  <- lm(formula, data = data)
  terms <- names(coef(fit0))
  n     <- nrow(data)
  
  mat <- matrix(NA_real_, nrow = B, ncol = length(terms))
  colnames(mat) <- terms
  
  for (b in seq_len(B)) {
    idx <- sample.int(n, n, replace = TRUE)
    fit <- try(lm(formula, data = data[idx, , drop = FALSE]), silent = TRUE)
    if (inherits(fit, "try-error")) next
    
    cb <- coef(fit)
    mat[b, names(cb)] <- cb
  }
  
  est <- coef(fit0)
  
  ci <- apply(mat, 2, function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 10) return(c(NA_real_, NA_real_))
    stats::quantile(x, c(0.025, 0.975), na.rm = TRUE)
  })
  
  out <- tibble::tibble(
    term      = names(est),
    estimate  = as.numeric(est),
    conf.low  = as.numeric(ci[1, ]),
    conf.high = as.numeric(ci[2, ])
  )
  
  ci_lm <- try(stats::confint(fit0), silent = TRUE)
  if (!inherits(ci_lm, "try-error")) {
    ci_lm <- as.data.frame(ci_lm) %>%
      tibble::rownames_to_column("term")
    names(ci_lm)[2:3] <- c("conf.low.lm", "conf.high.lm")
    
    out <- out %>%
      dplyr::left_join(ci_lm, by = "term") %>%
      dplyr::mutate(
        conf.low  = dplyr::if_else(is.na(conf.low),  conf.low.lm,  conf.low),
        conf.high = dplyr::if_else(is.na(conf.high), conf.high.lm, conf.high)
      ) %>%
      dplyr::select(term, estimate, conf.low, conf.high)
  }
  
  out
}

forest_driver_plot <- function(coef_df) {
  driver_cols <- c(
    "RC"   = "#C62828",
    "MCF"  = "#1565C0",
    "MAOP" = "#2E7D32"
  )
  
  term_order <- c("MCF", "MAOP", "RC")
  
  d <- coef_df %>%
    dplyr::filter(term %in% term_order) %>%
    dplyr::mutate(
      term = factor(term, levels = rev(term_order)),
      y_num = as.numeric(term)
    )
  
  x_vals <- c(d$conf.low, d$conf.high, d$estimate)
  x_vals <- x_vals[is.finite(x_vals)]
  
  if (length(x_vals) == 0) {
    x_lims <- c(-1.6, 1.6)
  } else {
    x_lims <- range(x_vals, na.rm = TRUE)
    x_pad <- max(0.18, diff(x_lims) * 0.16)
    if (!is.finite(x_pad) || x_pad == 0) x_pad <- 0.35
    x_lims <- c(x_lims[1] - x_pad, x_lims[2] + x_pad)
    x_lims[1] <- min(x_lims[1], -1.75)
    x_lims[2] <- max(x_lims[2],  1.55)
  }
  
  label_x <- x_lims[1] + 0.045 * diff(x_lims)
  
  ggplot2::ggplot(d, ggplot2::aes(y = y_num)) +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.35, colour = "black") +
    ggplot2::geom_text(
      data = d,
      ggplot2::aes(x = label_x, y = y_num, label = term),
      hjust = 0, vjust = 0.5,
      size = 3.8, family = base_font, colour = "black"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = conf.low, xend = conf.high, yend = y_num, colour = term),
      linewidth = 0.78, lineend = "round"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = conf.low, xend = conf.low, y = y_num - 0.12, yend = y_num + 0.12, colour = term),
      linewidth = 0.58
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = conf.high, xend = conf.high, y = y_num - 0.12, yend = y_num + 0.12, colour = term),
      linewidth = 0.58
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = estimate, colour = term),
      size = 3.7
    ) +
    ggplot2::scale_colour_manual(values = driver_cols, guide = "none") +
    ggplot2::scale_x_continuous(
      limits = x_lims,
      breaks = scales::pretty_breaks(n = 4),
      expand = ggplot2::expansion(mult = 0.02)
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(levels(d$term)),
      labels = NULL,
      limits = c(0.5, length(term_order) + 0.5),
      expand = c(0, 0)
    ) +
    ggplot2::labs(x = "Marginal std. \u03b2 (95% CI)", y = NULL) +
    theme_maom +
    ggplot2::theme(
      axis.text.y  = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      legend.position = "none",
      plot.margin = ggplot2::margin(8, 8, 5, 18)
    )
}

# -----------------------------
# 8. Panel (a)
# -----------------------------
pal_P <- c(
  "10" = "#C62828",
  "50" = "#1565C0",
  "90" = "#2E7D32"
)

P_levels <- as.numeric(
  stats::quantile(df_maom$MAOM_P, probs = c(0.10, 0.50, 0.90), na.rm = TRUE)
)
P_labels <- c("10", "50", "90")

x_rc_decrease <- seq(
  min(df_maom$RC_decrease, na.rm = TRUE),
  max(df_maom$RC_decrease, na.rm = TRUE),
  length.out = 160
)

ndA <- dplyr::bind_rows(lapply(seq_along(P_levels), function(i) {
  tibble::tibble(
    RC_decrease = x_rc_decrease,
    MAOM_P      = P_levels[i],
    MAOP_level  = P_labels[i]
  )
}))
ndA$MAOP_level <- factor(ndA$MAOP_level, levels = P_labels)

predA <- boot_pred_ribbon(
  MAOM_Ca_F3 ~ RC_decrease * MAOM_P,
  df_maom,
  ndA,
  B = B_BOOT,
  seed = 10
)

y_all_A <- c(predA$lwr, predA$upr, predA$fit, df_maom$MAOM_Ca_F3)
y_rng_A <- range(y_all_A[is.finite(y_all_A)], na.rm = TRUE)
y_pad_A <- diff(y_rng_A) * 0.16
if (!is.finite(y_pad_A) || y_pad_A == 0) y_pad_A <- 0.55
y_lims_A <- c(y_rng_A[1] - y_pad_A, y_rng_A[2] + y_pad_A)

x_all_A <- c(predA$RC_decrease, df_maom$RC_decrease)
x_rng_A <- range(x_all_A[is.finite(x_all_A)], na.rm = TRUE)
x_pad_A <- diff(x_rng_A) * 0.06
if (!is.finite(x_pad_A) || x_pad_A == 0) x_pad_A <- 0.20
x_lims_A <- c(x_rng_A[1] - x_pad_A, x_rng_A[2] + x_pad_A)

trt_legend_A <- tibble::tibble(
  Treatments = factor(treat_levels, levels = treat_levels),
  lab = treat_levels,
  x = x_lims_A[1] + c(0.17, 0.39, 0.63) * diff(x_lims_A),
  y = y_lims_A[1] + 0.105 * diff(y_lims_A)
)

p_legend_A <- tibble::tibble(
  MAOP_level = factor(c("10", "50", "90"), levels = c("10", "50", "90")),
  lab = c("10", "50", "90"),
  x0 = x_lims_A[1] + c(0.48, 0.64, 0.80) * diff(x_lims_A),
  x1 = x_lims_A[1] + c(0.54, 0.70, 0.86) * diff(x_lims_A),
  y  = y_lims_A[2] - 0.125 * diff(y_lims_A)
)

p_legend_title_A <- tibble::tibble(
  x = x_lims_A[1] + 0.48 * diff(x_lims_A),
  y = y_lims_A[2] - 0.045 * diff(y_lims_A),
  lab = "MAOP percentile"
)

pA <- ggplot2::ggplot() +
  ggplot2::geom_ribbon(
    data = predA,
    ggplot2::aes(x = RC_decrease, ymin = lwr, ymax = upr, fill = MAOP_level),
    alpha = 0.13, colour = NA
  ) +
  ggplot2::geom_line(
    data = predA,
    ggplot2::aes(x = RC_decrease, y = fit, colour = MAOP_level),
    linewidth = 0.82
  ) +
  ggplot2::geom_point(
    data = df_maom,
    ggplot2::aes(x = RC_decrease, y = MAOM_Ca_F3, shape = Treatments),
    size = 3.1, stroke = 0.40, colour = "black", fill = "white"
  ) +
  ggplot2::scale_colour_manual(values = pal_P, guide = "none") +
  ggplot2::scale_fill_manual(values = pal_P, guide = "none") +
  ggplot2::scale_shape_manual(values = c("CK" = 21, "GP" = 22, "PEG" = 24), guide = "none") +
  ggplot2::geom_point(
    data = trt_legend_A,
    ggplot2::aes(x = x, y = y, shape = Treatments),
    size = 2.9, stroke = 0.40, colour = "black", fill = "white", inherit.aes = FALSE
  ) +
  ggplot2::geom_text(
    data = trt_legend_A,
    ggplot2::aes(x = x + 0.035 * diff(x_lims_A), y = y, label = lab),
    hjust = 0, vjust = 0.5, size = 3.4, family = base_font, colour = "black", inherit.aes = FALSE
  ) +
  ggplot2::geom_text(
    data = p_legend_title_A,
    ggplot2::aes(x = x, y = y, label = lab),
    hjust = 0, vjust = 0.5, size = 3.5, family = base_font, colour = "black", inherit.aes = FALSE
  ) +
  ggplot2::geom_segment(
    data = p_legend_A,
    ggplot2::aes(x = x0, xend = x1, y = y, yend = y, colour = MAOP_level),
    linewidth = 0.82, lineend = "butt", inherit.aes = FALSE
  ) +
  ggplot2::geom_text(
    data = p_legend_A,
    ggplot2::aes(x = x1 + 0.018 * diff(x_lims_A), y = y, label = lab),
    hjust = 0, vjust = 0.5, size = 3.6, family = base_font, colour = "black", inherit.aes = FALSE
  ) +
  ggplot2::coord_cartesian(xlim = x_lims_A, ylim = y_lims_A, clip = "off") +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
  ggplot2::labs(x = "RC decrease", y = NULL) +
  theme_maom +
  ggplot2::theme(legend.position = "none", plot.margin = ggplot2::margin(8, 8, 6, 8)) +
  ggplot2::annotate(
    "text",
    x = x_lims_A[1] + 0.020 * diff(x_lims_A),
    y = y_lims_A[1] + 0.52 * diff(y_lims_A),
    label = "MCF",
    angle = 90,
    hjust = 0.5,
    vjust = 0.5,
    size = 3.2,
    colour = "black",
    family = base_font
  )

pA_tagged <- cowplot::ggdraw() +
  cowplot::draw_plot(pA, x = 0, y = 0, width = 1, height = 0.94) +
  cowplot::draw_label(
    "(a)", x = 0.01, y = 0.995,
    hjust = 0, vjust = 1,
    size = 15, fontface = "bold", color = "black",
    fontfamily = base_font
  )

# -----------------------------
# 9. Panel (b)
# -----------------------------
df_std_maom <- std_data(
  df_maom,
  c("MAOM_C", "MAOM_Ca_F3", "MAOM_P", "Reactive_Ca")
)

coefD <- dplyr::bind_rows(
  boot_coef_ci(MAOM_C ~ MAOM_Ca_F3, df_std_maom, B = B_BOOT, seed = 41) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::mutate(term = "MCF"),
  
  boot_coef_ci(MAOM_C ~ MAOM_P, df_std_maom, B = B_BOOT, seed = 42) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::mutate(term = "MAOP"),
  
  boot_coef_ci(MAOM_C ~ Reactive_Ca, df_std_maom, B = B_BOOT, seed = 43) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::mutate(term = "RC")
) %>%
  dplyr::select(term, estimate, conf.low, conf.high)

pB <- forest_driver_plot(coefD) +
  ggplot2::theme(plot.margin = ggplot2::margin(6, 8, 5, 12))

pB_tagged <- cowplot::ggdraw() +
  cowplot::draw_plot(pB, x = 0, y = 0, width = 1, height = 0.94) +
  cowplot::draw_label(
    "(b)", x = 0.01, y = 0.995,
    hjust = 0, vjust = 1,
    size = 15, fontface = "bold", color = "black",
    fontfamily = base_font
  )

# =========================================================
# PART B : FTIR processing + 2D-COS
# =========================================================

# -----------------------------
# 10. FTIR helpers
# -----------------------------
trapz_simple <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 2) return(NA_real_)
  ord <- order(x); x <- x[ord]; y <- y[ord]
  sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
}

als_baseline <- function(y, lambda = 1e6, p = 0.001, niter = 10) {
  y <- as.numeric(y)
  L <- length(y)
  D <- diff(Matrix::Diagonal(L), differences = 2)
  w <- rep(1, L)
  for (i in seq_len(niter)) {
    W <- Matrix::Diagonal(x = w)
    Z <- W + lambda * crossprod(D)
    z <- as.numeric(solve(Z, w * y))
    w <- p * (y > z) + (1 - p) * (y < z)
  }
  z
}

read_one_file <- function(file_path, file_base, treatment, fraction) {
  if (!file.exists(file_path)) stop(paste("File not found:", file_path))
  sheets <- readxl::excel_sheets(file_path)
  lapply(sheets, function(sh) {
    df <- readxl::read_excel(file_path, sheet = sh)
    df <- df[, 1:2]
    colnames(df) <- c("Wavenumber", "Intensity")
    df %>%
      dplyr::mutate(
        Wavenumber = as.numeric(Wavenumber),
        Intensity  = as.numeric(Intensity),
        FileBase   = file_base,
        Treatment  = treatment,
        Fraction   = fraction,
        Replicate  = sh
      ) %>%
      dplyr::select(Fraction, Treatment, FileBase, Replicate, Wavenumber, Intensity)
  }) %>% dplyr::bind_rows()
}

process_one_spectrum <- function(df) {
  df <- df %>%
    dplyr::arrange(Wavenumber) %>%
    dplyr::filter(is.finite(Wavenumber), is.finite(Intensity))
  df <- df %>% dplyr::filter(!(dplyr::row_number() == 1 & abs(Intensity) < 1e-12))
  baseline <- als_baseline(df$Intensity, 1e6, 0.001, 10)
  corrected <- df$Intensity - baseline
  corrected_positive <- pmax(corrected, 0)
  total_area <- trapz_simple(df$Wavenumber, corrected_positive)
  normalized <- if (is.na(total_area) || total_area == 0) corrected_positive else corrected_positive / total_area
  df %>% dplyr::mutate(
    Baseline = baseline,
    Corrected = corrected,
    Corrected_Positive = corrected_positive,
    Normalized = normalized
  )
}

# -----------------------------
# 11. Load FTIR files
# -----------------------------
file_map_all <- data.frame(
  FileBase = c("CKWS", "GPWS", "PEGWS", "PCK", "PGP", "PPEG", "MCK", "MGP", "MPEG"),
  Treatment = c("CK","GP","PEG","CK","GP","PEG","CK","GP","PEG"),
  Fraction  = c("Whole soil","Whole soil","Whole soil","POM","POM","POM","MAOM","MAOM","MAOM"),
  stringsAsFactors = FALSE
) %>%
  dplyr::mutate(FilePath = file.path(data_dir, paste0(FileBase, ".xlsx")))

missing <- file_map_all$FilePath[!file.exists(file_map_all$FilePath)]
if (length(missing) > 0) {
  print(missing)
  stop("Missing FTIR files.")
}

ftir_raw <- lapply(seq_len(nrow(file_map_all)), function(i) {
  read_one_file(
    file_map_all$FilePath[i],
    file_map_all$FileBase[i],
    file_map_all$Treatment[i],
    file_map_all$Fraction[i]
  )
}) %>% dplyr::bind_rows()

ftir_processed <- ftir_raw %>%
  dplyr::group_by(Fraction, Treatment, FileBase, Replicate) %>%
  dplyr::group_modify(~ process_one_spectrum(.x)) %>%
  dplyr::ungroup()

# =========================================================
# PART C : 2D-COS block (panel c)
# =========================================================

ftir_summary <- ftir_processed %>%
  dplyr::group_by(Fraction, Treatment, Wavenumber) %>%
  dplyr::summarise(
    Normalized_mean = mean(Normalized, na.rm = TRUE),
    Normalized_sd   = sd(Normalized, na.rm = TRUE),
    .groups = "drop"
  )

comparison_pairs <- list(
  "CK_vs_GP"  = c("CK", "GP"),
  "CK_vs_PEG" = c("CK", "PEG")
)

fraction_order <- c("Whole soil", "POM", "MAOM")
wn_grid <- seq(450, 4000, by = 5)

interpolate_spectrum <- function(df, wn_grid) {
  df <- df %>% dplyr::arrange(Wavenumber) %>%
    dplyr::filter(is.finite(Wavenumber), is.finite(Normalized_mean))
  approx(df$Wavenumber, df$Normalized_mean, xout = wn_grid, rule = 2)$y
}

make_spectrum_matrix <- function(frac_name, treat_pair) {
  dat <- ftir_summary %>%
    dplyr::filter(Fraction == frac_name, Treatment %in% treat_pair)
  present <- treat_pair[treat_pair %in% unique(dat$Treatment)]
  mat <- lapply(present, function(tr) {
    interpolate_spectrum(dat %>% dplyr::filter(Treatment == tr), wn_grid)
  }) %>% do.call(rbind, .)
  rownames(mat) <- present
  colnames(mat) <- as.character(wn_grid)
  list(mat = mat, wn = wn_grid)
}

calc_2dcos <- function(mat) {
  m <- nrow(mat)
  dyn  <- scale(mat, center = TRUE, scale = FALSE)
  sync <- crossprod(dyn) / (m - 1)
  list(synchronous = sync)
}

matrix_to_df <- function(M, wn) {
  expand.grid(Wavenumber_1 = wn, Wavenumber_2 = wn) %>%
    dplyr::mutate(Intensity = as.vector(M))
}

plot_2dcos_map <- function(df, lim_override = NULL,
                           show_legend = FALSE,
                           show_legend_title = FALSE) {
  lim <- if (is.null(lim_override)) {
    quantile(abs(df$Intensity), 0.995, na.rm = TRUE)
  } else lim_override
  if (!is.finite(lim) || lim == 0) lim <- max(abs(df$Intensity), na.rm = TRUE)
  
  contour_breaks <- pretty(c(-lim, lim), n = 11)
  contour_breaks <- contour_breaks[contour_breaks != 0]
  legend_title <- NULL
  
  ggplot(df, aes(Wavenumber_1, Wavenumber_2)) +
    geom_raster(aes(fill = Intensity), interpolate = TRUE, na.rm = TRUE) +
    geom_contour(
      aes(z = Intensity),
      breaks = contour_breaks[contour_breaks > 0],
      color = "#B2182B", linewidth = 0.20, na.rm = TRUE
    ) +
    geom_contour(
      aes(z = Intensity),
      breaks = contour_breaks[contour_breaks < 0],
      color = "#2166AC", linewidth = 0.20,
      linetype = "dashed", na.rm = TRUE
    ) +
    scale_x_reverse(
      limits = c(4000, 450),
      breaks = c(4000, 3000, 2000, 1000, 500),
      labels = c("4000", "3000", "2000", "1000", "500"),
      expand = expansion(mult = c(0, 0)),
      guide  = guide_axis(check.overlap = TRUE)
    ) +
    scale_y_reverse(
      limits = c(4000, 450),
      breaks = c(4000, 3000, 2000, 1000, 500),
      labels = c("4000", "3000", "2000", "1000", "500"),
      expand = expansion(mult = c(0, 0)),
      guide  = guide_axis(check.overlap = TRUE)
    ) +
    coord_cartesian(xlim = c(4000, 450), ylim = c(4000, 450), expand = FALSE, clip = "off") +
    scale_fill_gradient2(
      low  = "#2166AC",
      mid  = "white",
      high = "#B2182B",
      midpoint = 0,
      limits = c(-lim, lim),
      oob = scales::squish,
      name = legend_title
    ) +
    labs(title = NULL, x = NULL, y = NULL) +
    theme_classic(base_size = 12.0, base_family = base_font) +
    theme(
      plot.title   = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 8.9, color = "black"),
      axis.text.y = element_text(size = 8.9, color = "black"),
      axis.ticks.x = element_line(linewidth = 0.35, color = "black"),
      axis.ticks.y = element_line(linewidth = 0.35, color = "black"),
      axis.ticks.length = grid::unit(1.0, "mm"),
      axis.line = element_line(linewidth = 0.45, color = "black"),
      legend.position = if (show_legend) "right" else "none",
      legend.title = element_text(size = 5.8, color = "black", face = "bold", hjust = 0.5, lineheight = 0.85),
      legend.key.height = grid::unit(3.2, "mm"),
      legend.key.width  = grid::unit(1.2, "mm"),
      legend.text = element_text(size = 5.4, color = "black"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      plot.margin = margin(4, 2, 2, 2)
    )
}

sync_dfs <- list()
for (comp_name in names(comparison_pairs)) {
  for (frac in fraction_order) {
    key <- paste(comp_name, frac, sep = " | ")
    smat <- make_spectrum_matrix(frac, comparison_pairs[[comp_name]])
    cos_res <- calc_2dcos(smat$mat)
    sync_dfs[[key]] <- matrix_to_df(cos_res$synchronous, smat$wn)
  }
}

all_sync_intensity <- unlist(lapply(sync_dfs, function(x) x$Intensity))
global_sync_lim <- quantile(abs(all_sync_intensity), 0.995, na.rm = TRUE)
if (!is.finite(global_sync_lim) || global_sync_lim == 0) {
  global_sync_lim <- max(abs(all_sync_intensity), na.rm = TRUE)
}

make_cos_panel <- function(comp_name, frac_name, show_legend = FALSE, show_legend_title = FALSE) {
  key <- paste(comp_name, frac_name, sep = " | ")
  plot_2dcos_map(
    sync_dfs[[key]],
    lim_override = global_sync_lim,
    show_legend = show_legend,
    show_legend_title = show_legend_title
  )
}

make_row_label <- function(txt, size = 14.2, y_shift = 0.55, x_shift = 0.72) {
  cowplot::ggdraw(xlim = c(0, 1), ylim = c(0, 1)) +
    cowplot::draw_label(
      txt, x = x_shift, y = y_shift, size = size,
      fontface = "bold", color = "black", angle = 90,
      fontfamily = base_font
    )
}

make_col_label <- function(txt, size = 12.8) {
  cowplot::ggdraw(xlim = c(0, 1), ylim = c(0, 1)) +
    cowplot::draw_label(
      txt,
      x = 0.5, y = 0.38,
      hjust = 0.5, vjust = 0.5,
      size = size, fontface = "bold", color = "black",
      fontfamily = base_font
    )
}

# GP row
p_ws_gp_legend_src <- make_cos_panel("CK_vs_GP", "Whole soil", show_legend = TRUE, show_legend_title = FALSE)
gp_sync_legend <- cowplot::get_legend(p_ws_gp_legend_src)

p_ws_gp <- make_cos_panel("CK_vs_GP", "Whole soil", FALSE, FALSE) +
  ggplot2::theme(axis.text.x = ggplot2::element_blank())
p_pom_gp <- make_cos_panel("CK_vs_GP", "POM", FALSE, FALSE) +
  ggplot2::theme(axis.text.x = ggplot2::element_blank())
p_maom_gp <- make_cos_panel("CK_vs_GP", "MAOM", FALSE, FALSE) +
  ggplot2::theme(axis.text.x = ggplot2::element_blank())

row1_body <- cowplot::plot_grid(
  p_ws_gp, gp_sync_legend, p_pom_gp, p_maom_gp,
  ncol = 4, rel_widths = c(0.88, 0.28, 0.92, 0.92), align = "hv", axis = "tb"
)
row1 <- cowplot::plot_grid(
  make_row_label("GP"), row1_body,
  ncol = 2, rel_widths = c(0.06, 1), align = "h", axis = "tb"
)

# PEG row
p_ws_peg_legend_src <- make_cos_panel("CK_vs_PEG", "Whole soil", show_legend = TRUE, show_legend_title = FALSE)
peg_sync_legend <- cowplot::get_legend(p_ws_peg_legend_src)

p_ws_peg <- make_cos_panel("CK_vs_PEG", "Whole soil", FALSE, FALSE) +
  ggplot2::scale_x_reverse(
    limits = c(4000, 450),
    breaks = c(4000, 3000, 2000, 1000),
    labels = c("4000", "3000", "2000", "1000"),
    expand = ggplot2::expansion(mult = c(0, 0)),
    guide = ggplot2::guide_axis(check.overlap = TRUE)
  )
p_pom_peg <- make_cos_panel("CK_vs_PEG", "POM", FALSE, FALSE) +
  ggplot2::scale_x_reverse(
    limits = c(4000, 450),
    breaks = c(4000, 3000, 2000, 1000),
    labels = c("4000", "3000", "2000", "1000"),
    expand = ggplot2::expansion(mult = c(0, 0)),
    guide = ggplot2::guide_axis(check.overlap = TRUE)
  )
p_maom_peg <- make_cos_panel("CK_vs_PEG", "MAOM", FALSE, FALSE) +
  ggplot2::scale_x_reverse(
    limits = c(4000, 450),
    breaks = c(4000, 3000, 2000, 1000),
    labels = c("4000", "3000", "2000", "1000"),
    expand = ggplot2::expansion(mult = c(0, 0)),
    guide = ggplot2::guide_axis(check.overlap = TRUE)
  )

row2_body <- cowplot::plot_grid(
  p_ws_peg, peg_sync_legend, p_pom_peg, p_maom_peg,
  ncol = 4, rel_widths = c(0.88, 0.28, 0.92, 0.92), align = "hv", axis = "tb"
)
row2 <- cowplot::plot_grid(
  make_row_label("PEG"), row2_body,
  ncol = 2, rel_widths = c(0.06, 1), align = "h", axis = "tb"
)

col_label_row <- cowplot::plot_grid(
  make_col_label("Whole soil"),
  cowplot::ggdraw(),
  make_col_label("POM"),
  make_col_label("MAOM"),
  ncol = 4,
  rel_widths = c(1.00, 0.20, 0.96, 0.96)
)

cos_block_core <- cowplot::plot_grid(
  col_label_row,
  row1,
  row2,
  ncol = 1,
  rel_heights = c(0.11, 1, 1),
  align = "v"
)

cos_block <- cowplot::ggdraw() +
  cowplot::draw_plot(cos_block_core, x = 0, y = 0, width = 1, height = 0.965) +
  cowplot::draw_label(
    "(c)", x = 0.01, y = 0.995,
    hjust = 0, vjust = 1,
    size = 15, fontface = "bold", color = "black",
    fontfamily = base_font
  )

# =========================================================
# PART D : FTIR panels (d) and (e), simplified 1700–500
# =========================================================

# ---- POM peaks
peaks_POM <- data.frame(
  Wavenumber = c(525, 870, 993, 1434, 1610),
  Label      = c("525", "870", "993", "1434", "1610"),
  Assignment = c("HPO₄²⁻", "CO₃²⁻", "PO₄³⁻", "sym COO⁻", "asym COO⁻"),
  x_shift    = c(0, 0, 0, 0, 25),
  stringsAsFactors = FALSE
)

# ---- MAOM peaks
peaks_MAOM <- data.frame(
  Wavenumber = c(523, 598, 870, 993, 1430, 1610),
  Label      = c("523", "598", "870", "993", "1430", "1610"),
  Assignment = c("HPO₄²⁻", "PO₄³⁻ (Ca–P)", "CO₃²⁻", "PO₄³⁻", "sym COO⁻ (Ca)", "asym COO⁻"),
  x_shift    = c(-35, 35, 0, 0, 0, 25),
  stringsAsFactors = FALSE
)

find_peak_positions <- function(mean_spectrum, peaks_df, window = 15) {
  out <- lapply(seq_len(nrow(peaks_df)), function(i) {
    wn0 <- peaks_df$Wavenumber[i]
    sub <- mean_spectrum %>%
      dplyr::filter(Wavenumber >= wn0 - window, Wavenumber <= wn0 + window)
    if (nrow(sub) == 0) {
      tibble::tibble(
        Wavenumber = wn0,
        Label = peaks_df$Label[i],
        Assignment = peaks_df$Assignment[i],
        x_shift = peaks_df$x_shift[i],
        Apex_wn = wn0,
        Apex_y = NA_real_
      )
    } else {
      k <- which.max(sub$Absorbance_mean)
      tibble::tibble(
        Wavenumber = wn0,
        Label = peaks_df$Label[i],
        Assignment = peaks_df$Assignment[i],
        x_shift = peaks_df$x_shift[i],
        Apex_wn = sub$Wavenumber[k],
        Apex_y  = sub$Absorbance_mean[k]
      )
    }
  }) %>% dplyr::bind_rows()
  
  out <- out %>%
    dplyr::arrange(desc(Apex_wn)) %>%
    dplyr::mutate(
      idx = dplyr::row_number()
    )
  out
}

build_ftir_main <- function(mean_spectrum, peaks_df, panel_tag, panel_title, line_col) {
  dat <- mean_spectrum %>%
    dplyr::filter(Wavenumber >= 500, Wavenumber <= 1700) %>%
    dplyr::arrange(Wavenumber)
  
  ymax <- max(dat$Absorbance_mean, na.rm = TRUE)
  pk <- find_peak_positions(dat, peaks_df, window = 18)
  
  num_off <- c(0.08, 0.16, 0.08, 0.16, 0.12, 0.18)
  ass_off <- c(0.20, 0.30, 0.20, 0.30, 0.24, 0.34)
  
  pk <- pk %>%
    dplyr::mutate(
      y_num = Apex_y + ymax * num_off[pmin(idx, length(num_off))],
      y_ass = Apex_y + ymax * ass_off[pmin(idx, length(ass_off))],
      x_lab = Apex_wn + x_shift
    )
  
  y_top <- max(pk$y_ass, na.rm = TRUE) * 1.06
  if (!is.finite(y_top)) y_top <- ymax * 1.35
  
  p <- ggplot(dat, aes(x = Wavenumber, y = Absorbance_mean)) +
    geom_line(color = line_col, linewidth = 0.95, na.rm = TRUE) +
    geom_segment(
      data = pk,
      aes(x = Apex_wn, xend = Apex_wn, y = Apex_y, yend = y_num - 0.02 * ymax),
      linewidth = 0.32, linetype = "dotted", color = "black", inherit.aes = FALSE
    ) +
    geom_point(
      data = pk,
      aes(x = Apex_wn, y = Apex_y),
      size = 1.6, colour = line_col, inherit.aes = FALSE
    ) +
    geom_label(
      data = pk,
      aes(x = x_lab, y = y_num, label = Label),
      size = 3.1, label.size = 0.22,
      fill = "white", colour = "black",
      label.padding = unit(0.06, "lines"),
      family = base_font, inherit.aes = FALSE
    ) +
    geom_text(
      data = pk,
      aes(x = x_lab, y = y_ass, label = Assignment),
      angle = 90, hjust = 0, vjust = 0.5,
      size = 3.3, colour = "black", family = base_font,
      inherit.aes = FALSE
    ) +
    scale_x_reverse(
      limits = c(1700, 500),
      breaks = c(1700, 1500, 1300, 1100, 900, 700, 500),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_y_continuous(
      limits = c(0, y_top),
      expand = expansion(mult = c(0, 0.02))
    ) +
    coord_cartesian(clip = "off") +
    labs(x = expression(Wavenumber~(cm^{-1})), y = "Absorbance") +
    theme_classic(base_size = 11.2, base_family = base_font) +
    theme(
      axis.line = element_line(linewidth = 0.55, colour = "black"),
      axis.ticks = element_line(linewidth = 0.48, colour = "black"),
      axis.ticks.length = unit(1.4, "mm"),
      axis.title = element_text(size = 10.5, colour = "black"),
      axis.text = element_text(size = 9.4, colour = "black"),
      plot.margin = margin(20, 12, 8, 12),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  
  cowplot::ggdraw() +
    cowplot::draw_plot(p, x = 0, y = 0, width = 1, height = 0.96) +
    cowplot::draw_label(
      panel_tag, x = 0.01, y = 0.995,
      hjust = 0, vjust = 1,
      size = 15, fontface = "bold", color = "black",
      fontfamily = base_font
    ) +
    cowplot::draw_label(
      panel_title, x = 0.11, y = 0.995,
      hjust = 0, vjust = 1,
      size = 13, fontface = "plain", color = "black",
      fontfamily = base_font
    )
}

ftir_pom_clean <- ftir_processed %>%
  dplyr::filter(Fraction == "POM", Treatment == "PEG")

ftir_maom_clean <- ftir_processed %>%
  dplyr::filter(Fraction == "MAOM", Treatment == "PEG")

# Audit the actual replicate sheets used for Fig. 6(d,e) before plotting.
cat("\nFig. 6(d), PEG–POM replicate sheets: ",
    paste(sort(unique(ftir_pom_clean$Replicate)), collapse = ", "), "\n", sep = "")
cat("Fig. 6(e), PEG–MAOM replicate sheets: ",
    paste(sort(unique(ftir_maom_clean$Replicate)), collapse = ", "), "\n", sep = "")

ftir_pom_mean <- ftir_pom_clean %>%
  dplyr::group_by(Wavenumber) %>%
  dplyr::summarise(Absorbance_mean = mean(Normalized, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(Wavenumber)

ftir_maom_mean <- ftir_maom_clean %>%
  dplyr::group_by(Wavenumber) %>%
  dplyr::summarise(Absorbance_mean = mean(Normalized, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(Wavenumber)

panel_pom <- build_ftir_main(
  mean_spectrum = ftir_pom_mean,
  peaks_df = peaks_POM,
  panel_tag = "(d)",
  panel_title = "PEG (POM)",
  line_col = "#1F77B4"
)

panel_maom <- build_ftir_main(
  mean_spectrum = ftir_maom_mean,
  peaks_df = peaks_MAOM,
  panel_tag = "(e)",
  panel_title = "PEG (MAOM)",
  line_col = "#E67300"
)

# =========================================================
# PART E : Final portrait layout
# =========================================================

ab_row <- cowplot::plot_grid(
  pA_tagged, pB_tagged,
  ncol = 2,
  rel_widths = c(1, 1),
  align = "hv",
  axis = "tblr"
)

master_core <- cowplot::plot_grid(
  ab_row,
  cos_block,
  panel_pom,
  panel_maom,
  ncol = 1,
  rel_heights = c(0.80, 1.45, 0.72, 0.72),
  align = "v",
  axis = "lr"
)

master_fig <- cowplot::ggdraw() +
  cowplot::draw_plot(master_core, x = 0.018, y = 0.012, width = 0.964, height = 0.974)

print(master_fig)

# =========================================================
# PART F : Save — A4 portrait (NC style)
# =========================================================
save_set_portrait <- function(p, base) {
  ggplot2::ggsave(
    paste0(base, ".png"), p,
    width = 190, height = 250,
    units = "mm", dpi = 600, bg = "white"
  )
  ggplot2::ggsave(
    paste0(base, ".pdf"), p,
    width = 190, height = 250,
    units = "mm", bg = "white"
  )
  ggplot2::ggsave(
    paste0(base, ".tiff"), p,
    width = 190, height = 250,
    units = "mm", dpi = 600,
    compression = "lzw", bg = "white"
  )
}

save_set_portrait(
  master_fig,
  file.path(out_dir, "Fig6_NC_Portrait_Mechanism")
)

cat("\n====================================================\n")
cat("Fig. 6 saved successfully.\n")
cat("Layout:\n")
cat("  (a) and (b) side by side\n")
cat("  (c) 2D-COS full width\n")
cat("  (d) PEG (POM) FTIR full width\n")
cat("  (e) PEG (MAOM) FTIR full width\n")
cat("Output folder:\n")
cat(out_dir, "\n")
cat("====================================================\n")

# ============================================================================
# FIGURE 6 — MINIMAL NATURE COMMUNICATIONS SOURCE DATA
# Source-data export only; the figure calculations and plotting above are unchanged.
#
# Export contains:
#   (a) actual plotted soil observations + fitted curves and 95% bootstrap limits
#   (b) displayed marginal coefficients and 95% bootstrap limits
#   (c) treatment-mean normalized FTIR spectra on the exact 711-point grid
#   (d,e) mean normalized FTIR traces + plotted peak labels/assignments
#
# No replicate-level FTIR tables and no separate full-pixel 2D-COS CSV are exported.
# The nine mean spectra in the second worksheet are sufficient to reconstruct
# the six 2D-COS maps using the formula documented in that worksheet.
# ============================================================================

sd_xlsx <- file.path(out_dir, "Source_Data_Fig6_Minimal.xlsx")

sd_wb <- openxlsx::createWorkbook(creator = "Figure 6 minimal source data")
sd_main_sheet <- "Fig 6 Plot Data"
sd_cos_sheet  <- "Fig 6c Mean Spectra"

openxlsx::addWorksheet(sd_wb, sd_main_sheet, gridLines = FALSE)
openxlsx::addWorksheet(sd_wb, sd_cos_sheet, gridLines = FALSE)

sd_style_title <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 13, fontColour = "#FFFFFF",
  fgFill = "#17365D", textDecoration = "bold", valign = "center"
)
sd_style_section <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 10, fontColour = "#FFFFFF",
  fgFill = "#234A71", textDecoration = "bold", valign = "center"
)
sd_style_header <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 9, fontColour = "#243344",
  fgFill = "#E8F0F7", textDecoration = "bold", wrapText = TRUE,
  border = "Bottom", borderColour = "#BECBD8"
)
sd_style_note <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 9, fontColour = "#526579",
  wrapText = TRUE, valign = "center"
)
sd_style_num <- openxlsx::createStyle(numFmt = "0.##########")

# Sheet titles and short explanations
openxlsx::mergeCells(sd_wb, sd_main_sheet, cols = 1:7, rows = 1)
openxlsx::writeData(
  sd_wb, sd_main_sheet,
  "FIGURE 6 | MINIMAL PLOT SOURCE DATA",
  startRow = 1, startCol = 1
)
openxlsx::addStyle(
  sd_wb, sd_main_sheet, sd_style_title,
  rows = 1, cols = 1:7, gridExpand = TRUE
)
openxlsx::setRowHeights(sd_wb, sd_main_sheet, rows = 1, heights = 27)

main_note <- paste(
  "Panels (a,b): only plotted observations, fitted curves and displayed coefficients.",
  "Panels (d,e): mean normalized FTIR traces and plotted peak assignments only.",
  "Panels (d,e) include all available PEG FTIR replicate spectra for the corresponding fractions.",
  "No replicate-level FTIR values are exported."
)
openxlsx::mergeCells(sd_wb, sd_main_sheet, cols = 1:7, rows = 2)
openxlsx::writeData(sd_wb, sd_main_sheet, main_note, startRow = 2, startCol = 1)
openxlsx::addStyle(
  sd_wb, sd_main_sheet, sd_style_note,
  rows = 2, cols = 1:7, gridExpand = TRUE
)
openxlsx::setRowHeights(sd_wb, sd_main_sheet, rows = 2, heights = 42)

cos_note <- paste(
  "Each treatment/fraction column is the normalized mean FTIR spectrum interpolated",
  "to the exact 450–4000 cm-1 grid (5 cm-1 step) used for panel (c).",
  "For each treatment pair A and B, synchronous intensity(i,j) =",
  "(A_i - B_i) * (A_j - B_j) / 2. These mean spectra are sufficient to reconstruct",
  "the six maps; replicate-level spectra and the full pixel raster are not included."
)
openxlsx::mergeCells(sd_wb, sd_cos_sheet, cols = 1:10, rows = 1)
openxlsx::writeData(
  sd_wb, sd_cos_sheet,
  "FIGURE 6(c) | TREATMENT-MEAN FTIR SPECTRA FOR 2D-COS",
  startRow = 1, startCol = 1
)
openxlsx::addStyle(
  sd_wb, sd_cos_sheet, sd_style_title,
  rows = 1, cols = 1:10, gridExpand = TRUE
)
openxlsx::setRowHeights(sd_wb, sd_cos_sheet, rows = 1, heights = 27)
openxlsx::mergeCells(sd_wb, sd_cos_sheet, cols = 1:10, rows = 2)
openxlsx::writeData(sd_wb, sd_cos_sheet, cos_note, startRow = 2, startCol = 1)
openxlsx::addStyle(
  sd_wb, sd_cos_sheet, sd_style_note,
  rows = 2, cols = 1:10, gridExpand = TRUE
)
openxlsx::setRowHeights(sd_wb, sd_cos_sheet, rows = 2, heights = 54)

sd_next <- new.env(parent = emptyenv())
sd_next$main <- 4L

sd_write_section <- function(heading, dat) {
  dat <- as.data.frame(dat, check.names = FALSE)
  if (!nrow(dat) || !ncol(dat)) {
    stop("Source-data section is empty: ", heading)
  }
  if (anyDuplicated(names(dat))) {
    stop("Duplicate column names in source-data section: ", heading)
  }
  
  r <- sd_next$main
  openxlsx::writeData(sd_wb, sd_main_sheet, heading, startRow = r, startCol = 1)
  openxlsx::addStyle(
    sd_wb, sd_main_sheet, sd_style_section,
    rows = r, cols = 1:7, gridExpand = TRUE
  )
  openxlsx::setRowHeights(sd_wb, sd_main_sheet, rows = r, heights = 22)
  
  data_row <- r + 1L
  openxlsx::writeData(
    sd_wb, sd_main_sheet, dat,
    startRow = data_row, startCol = 1,
    colNames = TRUE, rowNames = FALSE,
    headerStyle = sd_style_header, keepNA = FALSE
  )
  
  numeric_cols <- which(vapply(dat, is.numeric, logical(1)))
  if (length(numeric_cols)) {
    openxlsx::addStyle(
      sd_wb, sd_main_sheet, sd_style_num,
      rows = seq.int(data_row + 1L, data_row + nrow(dat)),
      cols = numeric_cols, gridExpand = TRUE, stack = TRUE
    )
  }
  
  openxlsx::setRowHeights(
    sd_wb, sd_main_sheet,
    rows = data_row, heights = 28
  )
  sd_next$main <- data_row + nrow(dat) + 2L
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Panel (a): only the actual points and the fitted curves shown in the figure
# ---------------------------------------------------------------------------
sd_a_points <- df_maom %>%
  dplyr::mutate(
    Treatment = as.character(Treatments),
    Replicate = as.character(Replicates),
    MCF = MAOM_Ca_F3
  ) %>%
  dplyr::select(Treatment, Replicate, RC_decrease, MCF)

sd_write_section(
  "Panel (a) | plotted observations",
  sd_a_points
)

sd_a_curves <- predA %>%
  dplyr::mutate(MAOP_percentile = as.character(MAOP_level)) %>%
  dplyr::transmute(
    MAOP_percentile,
    MAOM_P_reference = MAOM_P,
    RC_decrease,
    Predicted_MCF = fit,
    CI95_lower = lwr,
    CI95_upper = upr
  )

sd_write_section(
  "Panel (a) | fitted curves and 95% bootstrap intervals",
  sd_a_curves
)

# ---------------------------------------------------------------------------
# Panel (b): only the estimates and confidence intervals displayed in the plot
# ---------------------------------------------------------------------------
sd_b_coefficients <- coefD %>%
  dplyr::transmute(
    Predictor = term,
    Standardized_beta = estimate,
    CI95_lower = conf.low,
    CI95_upper = conf.high
  )

sd_write_section(
  "Panel (b) | plotted marginal coefficients",
  sd_b_coefficients
)

# ---------------------------------------------------------------------------
# Panels (d,e): mean FTIR traces only; no replicate-level spectra are exported
# ---------------------------------------------------------------------------
sd_de_means <- dplyr::bind_rows(
  ftir_pom_mean %>%
    dplyr::filter(Wavenumber >= 500, Wavenumber <= 1700) %>%
    dplyr::transmute(
      Panel = "(d)", Fraction = "POM",
      Wavenumber_cm1 = Wavenumber,
      Mean_normalized_signal = Absorbance_mean
    ),
  ftir_maom_mean %>%
    dplyr::filter(Wavenumber >= 500, Wavenumber <= 1700) %>%
    dplyr::transmute(
      Panel = "(e)", Fraction = "MAOM",
      Wavenumber_cm1 = Wavenumber,
      Mean_normalized_signal = Absorbance_mean
    )
)

sd_write_section(
  "Panels (d,e) | mean normalized FTIR traces",
  sd_de_means
)

# Only peak labels/assignments and the corresponding mean-spectrum apex values
pom_peaks_apex <- find_peak_positions(
  ftir_pom_mean %>% dplyr::filter(Wavenumber >= 500, Wavenumber <= 1700),
  peaks_POM, window = 18
)

maom_peaks_apex <- find_peak_positions(
  ftir_maom_mean %>% dplyr::filter(Wavenumber >= 500, Wavenumber <= 1700),
  peaks_MAOM, window = 18
)

sd_peak_data <- dplyr::bind_rows(
  pom_peaks_apex %>%
    dplyr::transmute(
      Panel = "(d)", Fraction = "POM", Label, Assignment,
      Reference_wavenumber_cm1 = Wavenumber,
      Observed_apex_cm1 = Apex_wn,
      Mean_signal_at_apex = Apex_y
    ),
  maom_peaks_apex %>%
    dplyr::transmute(
      Panel = "(e)", Fraction = "MAOM", Label, Assignment,
      Reference_wavenumber_cm1 = Wavenumber,
      Observed_apex_cm1 = Apex_wn,
      Mean_signal_at_apex = Apex_y
    )
)

sd_write_section(
  "Panels (d,e) | peak labels and assignments",
  sd_peak_data
)

# ---------------------------------------------------------------------------
# Panel (c): nine treatment-mean spectra on the exact 711-point plotting grid.
# This replaces both the replicate-level FTIR export and the full-pixel CSV.
# ---------------------------------------------------------------------------
sd_cos_means <- data.frame(Wavenumber_cm1 = as.numeric(wn_grid))

for (frac in fraction_order) {
  for (tr in treat_levels) {
    d <- ftir_summary %>%
      dplyr::filter(Fraction == frac, Treatment == tr)
    
    if (!nrow(d)) {
      stop("No mean FTIR spectrum available for panel (c): ", frac, " / ", tr)
    }
    
    col_name <- paste0(
      gsub("[^A-Za-z0-9]+", "_", frac), "_",
      tr, "_mean_normalized"
    )
    sd_cos_means[[col_name]] <- as.numeric(interpolate_spectrum(d, wn_grid))
  }
}

if (nrow(sd_cos_means) != 711L || ncol(sd_cos_means) != 10L) {
  stop("Panel (c) mean-spectrum source data should contain 711 rows and 10 columns.")
}

openxlsx::writeData(
  sd_wb, sd_cos_sheet, sd_cos_means,
  startRow = 4, startCol = 1,
  colNames = TRUE, rowNames = FALSE,
  headerStyle = sd_style_header, keepNA = FALSE
)
openxlsx::addStyle(
  sd_wb, sd_cos_sheet, sd_style_num,
  rows = seq.int(5L, 4L + nrow(sd_cos_means)),
  cols = 1:10, gridExpand = TRUE, stack = TRUE
)

# Workbook formatting and checks
openxlsx::setColWidths(sd_wb, sd_main_sheet, cols = 1, widths = 18)
openxlsx::setColWidths(sd_wb, sd_main_sheet, cols = 2:7, widths = 20)
openxlsx::setColWidths(sd_wb, sd_cos_sheet, cols = 1, widths = 17)
openxlsx::setColWidths(sd_wb, sd_cos_sheet, cols = 2:10, widths = 22)
openxlsx::freezePane(sd_wb, sd_main_sheet, firstActiveRow = 4)
openxlsx::freezePane(sd_wb, sd_cos_sheet, firstActiveRow = 5)

openxlsx::saveWorkbook(sd_wb, sd_xlsx, overwrite = TRUE)

if (!file.exists(sd_xlsx) || file.info(sd_xlsx)$size <= 0) {
  stop("Minimal source-data workbook was not created successfully.")
}

expected_sheets <- c(sd_main_sheet, sd_cos_sheet)
if (!identical(openxlsx::getSheetNames(sd_xlsx), expected_sheets)) {
  stop("The minimal source-data workbook has unexpected worksheet names.")
}

status_path <- file.path(out_dir, "FIG6_MINIMAL_SOURCE_DATA_STATUS.txt")
writeLines(c(
  paste("Completed:", Sys.time()),
  paste("Minimal source-data workbook:", normalizePath(sd_xlsx, mustWork = TRUE)),
  "Worksheets: Fig 6 Plot Data; Fig 6c Mean Spectra",
  "FTIR source data: mean spectra only; no replicate-level FTIR rows exported.",
  "Panel (c): nine treatment/fraction mean spectra on the 711-point grid.",
  "No separate full-pixel 2D-COS CSV exported.",
  "Figure plotting and calculation code above the source-data section was retained."
), con = status_path)

cat("\n============================================================\n")
cat("FIGURE 6 MINIMAL SOURCE DATA EXPORT COMPLETE\n")
cat("Excel:", normalizePath(sd_xlsx, mustWork = TRUE), "\n")
cat("Status:", normalizePath(status_path, mustWork = TRUE), "\n")
cat("FTIR source values are means only; replicate-level FTIR tables were removed.\n")
cat("============================================================\n")