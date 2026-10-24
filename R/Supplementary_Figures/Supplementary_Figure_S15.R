rm(list = ls())
graphics.off()
cat("\014")

################################################################################
# SUPPLEMENTARY FIGURE S15 — FRACTION-SPECIFIC Ca-C-P RELATIONSHIPS ACROSS STP LEVELS
#
# y ~ x * STP
#
# MAOM: A) MCF vs RC   B) MAOC vs MCF   C) MAOP vs MCF
# POM : D) POC vs RC   E) POP  vs RC    F) POC  vs POP
#
# Nature Communications portrait format: 15.5 x 15.0 cm
# Layout: 3 columns x 2 rows (Option A)
#
#   ┌──────────────────────────────────────────┐
#   │          MAOM domain   (banner)          │
#   ├─────────────┬─────────────┬──────────────┤
#   │    (a)      │     (b)     │     (c)      │
#   ├─────────────┴─────────────┴──────────────┤
#   │          POM control   (banner)          │
#   ├─────────────┬─────────────┬──────────────┤
#   │    (d)      │     (e)     │     (f)      │
#   ├─────────────┴─────────────┴──────────────┤
#   │                Legend                    │
#   └──────────────────────────────────────────┘
################################################################################


## ============================================================
## 0. Packages
## ============================================================

pkgs <- c(
  "readxl", "dplyr", "tidyr", "stringr", "tibble",
  "purrr", "ggplot2", "patchwork", "grid", "scales",
  "broom", "writexl"
)

for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

invisible(lapply(pkgs, library, character.only = TRUE))

base_font <- if (.Platform$OS.type == "windows") "Arial" else "Helvetica"


## ============================================================
## 1. Input / output
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

file_path <- repo_input_path("Conventional_Treatments.xlsx")
sheet_name <- "data"

out_dir <- "Fig_S15_Fraction_Level_STP_Model_6Panels"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

## Nature Communications portrait page (15.5 x 15.0 cm)
fig_w_mm <- 155
fig_h_mm <- 150


## ============================================================
## 2. Colours
## ============================================================

COL_MAOM <- "#083C5A"
COL_POM  <- "#5E5175"

COL_STP_10 <- "#000000"
COL_STP_50 <- "#008080"
COL_STP_90 <- "#62c2cc"

trt_cols <- c(
  "CK"  = "grey35",
  "GP"  = "#1565C0",
  "PEG" = "#C62828"
)


## ============================================================
## 3. Helper functions
## ============================================================

keyize <- function(x) {
  tolower(gsub("[^a-zA-Z0-9]+", "", as.character(x)))
}

fmt_p <- function(x) {
  x <- as.numeric(x)
  out <- rep("P = NA", length(x))
  ok <- !is.na(x)
  out[ok & x < 0.001] <- "P < 0.001"
  out[ok & x >= 0.001] <- paste0(
    "P = ",
    format(round(x[ok & x >= 0.001], 3), nsmall = 3)
  )
  out
}

se <- function(x) {
  stats::sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
}

safe_read_excel <- function(path, sheet) {
  if (!file.exists(path)) {
    stop(
      paste0(
        "Input file not found: ", path,
        "\nPut Conventional_Treatments.xlsx in the working directory ",
        "or edit file_path."
      ),
      call. = FALSE
    )
  }
  readxl::read_excel(path, sheet = sheet)
}

canonical_var <- function(x) {
  k <- keyize(x)
  map <- c(
    "maomc" = "MAOM_C", "maoc" = "MAOM_C",
    "maomcarbon" = "MAOM_C", "maomcpercent" = "MAOM_C",
    "pomc" = "POM_C", "poc" = "POM_C",
    "pomcarbon" = "POM_C", "pomcpercent" = "POM_C",
    "maomp" = "MAOM_P", "maop" = "MAOM_P",
    "maomphosphorus" = "MAOM_P",
    "pomp" = "POM_P", "pop" = "POM_P",
    "pomphosphorus" = "POM_P",
    "maomcaf3" = "MAOM_Ca_F3", "mcf" = "MAOM_Ca_F3",
    "maomcf" = "MAOM_Ca_F3",
    "pomcaf3" = "POM_Ca_F3", "pcf" = "POM_Ca_F3",
    "pomcf" = "POM_Ca_F3",
    "reactiveca" = "Reactive_Ca", "rc" = "Reactive_Ca",
    "stp" = "STP", "totalp" = "STP", "tp" = "STP",
    "soiltotalp" = "STP", "totalphosphorus" = "STP",
    "soiltotalphosphorus" = "STP",
    "stpmgkg" = "STP", "totalpmgkg" = "STP",
    "totalphosphorusmgkg" = "STP", "sap" = "STP"
  )
  ifelse(k %in% names(map), unname(map[k]), as.character(x))
}

make_unique_names <- function(x) make.names(x, unique = TRUE)

get_formula_text <- function(model) {
  paste(deparse(stats::formula(model)), collapse = " ")
}

model_p_value <- function(model) {
  sm <- summary(model)
  f <- sm$fstatistic
  if (is.null(f) || any(is.na(f))) return(NA_real_)
  stats::pf(f[1], f[2], f[3], lower.tail = FALSE)
}

std_fit <- function(formula, data) {
  d <- data
  vars <- all.vars(formula)
  for (v in vars) {
    if (v %in% names(d) && is.numeric(d[[v]])) {
      d[[v]] <- as.numeric(scale(d[[v]]))
    }
  }
  stats::lm(formula, data = d)
}


## ============================================================
## 4. Read and standardise data
## ============================================================

raw <- safe_read_excel(file_path, sheet_name)
raw_names <- names(raw)
raw_key <- keyize(raw_names)

trt_col <- raw_names[raw_key %in% c("treatments", "treatment", "trt")][1]
rep_col <- raw_names[raw_key %in% c("replicates", "replicate", "rep", "block")][1]

if (is.na(trt_col)) {
  stop("Could not find Treatments/Treatment column.", call. = FALSE)
}

if (is.na(rep_col)) {
  raw$Replicates <- seq_len(nrow(raw))
  rep_col <- "Replicates"
}

resp_col <- raw_names[raw_key %in% c(
  "responsevaribale", "responsevariable",
  "variable", "var", "parameter", "trait"
)][1]

val_col <- raw_names[raw_key %in% c(
  "value", "values", "meanvalue", "data"
)][1]


if (!is.na(resp_col) && !is.na(val_col)) {
  
  dat_long <- raw %>%
    dplyr::transmute(
      Treatments        = as.character(.data[[trt_col]]),
      Replicates        = as.character(.data[[rep_col]]),
      response_variable = canonical_var(
        stringr::str_trim(as.character(.data[[resp_col]]))
      ),
      value             = suppressWarnings(as.numeric(.data[[val_col]]))
    ) %>%
    dplyr::filter(
      !is.na(Treatments),
      !is.na(Replicates),
      !is.na(response_variable),
      !is.na(value)
    )
  
  df <- dat_long %>%
    dplyr::mutate(
      Treatments = dplyr::recode(Treatments, "PGP" = "PEG")
    ) %>%
    tidyr::pivot_wider(
      id_cols     = c(Treatments, Replicates),
      names_from  = response_variable,
      values_from = value,
      values_fn   = mean
    )
  
} else {
  
  df <- raw
  names(df)[names(df) == trt_col] <- "Treatments"
  names(df)[names(df) == rep_col] <- "Replicates"
  
  keep_id <- c("Treatments", "Replicates")
  other_names <- setdiff(names(df), keep_id)
  names(df)[match(other_names, names(df))] <- canonical_var(other_names)
  
  df <- df %>%
    dplyr::mutate(
      Treatments = as.character(Treatments),
      Treatments = dplyr::recode(Treatments, "PGP" = "PEG"),
      Replicates = as.character(Replicates)
    )
}

names(df) <- make_unique_names(names(df))

df$Treatments <- factor(df$Treatments, levels = c("CK", "GP", "PEG"))

num_cols <- setdiff(names(df), c("Treatments", "Replicates"))
df <- df %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(num_cols),
      ~ suppressWarnings(as.numeric(.x))
    )
  )


## ============================================================
## 5. Required variables
## ============================================================

required_vars <- c(
  "Reactive_Ca", "STP",
  "MAOM_C", "MAOM_P", "MAOM_Ca_F3",
  "POM_C", "POM_P"
)

missing_vars <- setdiff(required_vars, names(df))
if (length(missing_vars) > 0) {
  stop(
    paste0(
      "Missing required variables:\n  ",
      paste(missing_vars, collapse = ", "),
      "\n\nDetected variables:\n  ",
      paste(names(df), collapse = ", ")
    ),
    call. = FALSE
  )
}


## ============================================================
## 6. Model specifications
## ============================================================

model_specs <- tibble::tribble(
  ~panel, ~domain, ~y,            ~x,             ~title,
  
  "A", "MAOM", "MAOM_Ca_F3", "Reactive_Ca", "RC x STP -> MCF",
  "B", "MAOM", "MAOM_C",     "MAOM_Ca_F3",  "MCF x STP -> MAOC",
  "C", "MAOM", "MAOM_P",     "MAOM_Ca_F3",  "MCF x STP -> MAOP",
  
  "D", "POM",  "POM_C",      "Reactive_Ca", "RC x STP -> POC",
  "E", "POM",  "POM_P",      "Reactive_Ca", "RC x STP -> POP",
  "F", "POM",  "POM_C",      "POM_P",       "POP x STP -> POC"
)


## ============================================================
## 7. Axis labels
## ============================================================

axis_labs <- list(
  Reactive_Ca = expression(RC~"(cmol"[c]~"kg"^{-1}*")"),
  MAOM_Ca_F3  = expression(MCF~"(cmol"[c]~"kg"^{-1}*")"),
  MAOM_C      = "MAOC (%)",
  POM_C       = "POC (%)",
  MAOM_P      = expression(MAOP~"(mg kg"^{-1}*")"),
  POM_P       = expression(POP~"(mg kg"^{-1}*")"),
  STP         = expression(STP~"(mg kg"^{-1}*")")
)


## ============================================================
## 8. Fit models
## ============================================================

fit_one_model <- function(spec_row) {
  y <- spec_row$y
  x <- spec_row$x
  
  d <- df %>%
    dplyr::select(
      Treatments, Replicates,
      dplyr::all_of(c(y, x, "STP"))
    ) %>%
    tidyr::drop_na()
  
  if (nrow(d) < 6) {
    stop(paste0("Not enough complete observations for panel ",
                spec_row$panel, "."), call. = FALSE)
  }
  
  form <- stats::as.formula(paste0(y, " ~ ", x, " * STP"))
  model <- stats::lm(form, data = d)
  
  list(model = model, data = d, formula = form)
}

fits <- purrr::map(seq_len(nrow(model_specs)),
                   function(i) fit_one_model(model_specs[i, ]))
names(fits) <- model_specs$panel


## ============================================================
## 9. Prediction data
## ============================================================

make_pred <- function(spec_row, fit_obj, n = 120) {
  y <- spec_row$y
  x <- spec_row$x
  d <- fit_obj$data
  model <- fit_obj$model
  
  x_seq <- seq(min(d[[x]], na.rm = TRUE),
               max(d[[x]], na.rm = TRUE),
               length.out = n)
  
  stp_q <- stats::quantile(
    d$STP, probs = c(0.10, 0.50, 0.90),
    na.rm = TRUE, names = FALSE
  )
  
  stp_lab <- c("10th percentile", "50th percentile", "90th percentile")
  
  pred_grid <- tidyr::expand_grid(x_value = x_seq, STP = stp_q) %>%
    dplyr::mutate(
      STP_level = factor(
        rep(stp_lab, each = length(x_seq)),
        levels = stp_lab
      )
    )
  
  names(pred_grid)[names(pred_grid) == "x_value"] <- x
  
  pr <- stats::predict(model, newdata = pred_grid, se.fit = TRUE)
  tcrit <- stats::qt(0.975, df = stats::df.residual(model))
  
  pred_grid %>%
    dplyr::mutate(
      fit = as.numeric(pr$fit),
      lwr = as.numeric(pr$fit - tcrit * pr$se.fit),
      upr = as.numeric(pr$fit + tcrit * pr$se.fit)
    )
}


## ============================================================
## 10. Theme
## ============================================================

frac_theme <- ggplot2::theme_classic(
  base_family = base_font,
  base_size   = 7
) +
  ggplot2::theme(
    axis.title = ggplot2::element_text(
      size = 7, face = "plain", colour = "black"
    ),
    axis.text = ggplot2::element_text(
      size = 6, face = "plain", colour = "black"
    ),
    axis.line  = ggplot2::element_line(linewidth = 0.45, colour = "black"),
    axis.ticks = ggplot2::element_line(linewidth = 0.45, colour = "black"),
    axis.ticks.length = ggplot2::unit(1.2, "pt"),
    
    plot.title = ggplot2::element_blank(),
    
    ## Panel letters INSIDE top-left
    plot.tag = ggplot2::element_text(
      size = 8.5, face = "bold",
      colour = "black", family = base_font
    ),
    plot.tag.position = c(0.02, 0.98),
    
    legend.position = "none",
    plot.margin     = ggplot2::margin(t = 3, r = 4, b = 3, l = 4)
  )


## ============================================================
## 11. Individual panels
## ============================================================

make_panel <- function(spec_row, fit_obj) {
  
  y <- spec_row$y
  x <- spec_row$x
  model <- fit_obj$model
  d     <- fit_obj$data
  
  pred <- make_pred(spec_row, fit_obj)
  
  sm <- summary(model)
  r2 <- sm$r.squared
  coef_tab <- sm$coefficients
  
  int_term     <- paste0(x, ":STP")
  int_term_alt <- paste0("STP:", x)
  
  if (int_term %in% rownames(coef_tab)) {
    p_show <- coef_tab[int_term, "Pr(>|t|)"]
  } else if (int_term_alt %in% rownames(coef_tab)) {
    p_show <- coef_tab[int_term_alt, "Pr(>|t|)"]
  } else {
    p_show <- NA_real_
  }
  
  xr <- range(d[[x]], na.rm = TRUE)
  yr <- range(c(d[[y]], pred$lwr, pred$upr), na.rm = TRUE)
  
  ann <- data.frame(
    x_ann = xr[1] + 0.055 * diff(xr),
    y_ann = yr[2] - 0.06  * diff(yr),
    lab   = paste0(
      "R\u00b2 = ", round(r2, 2),
      "\nPint = ", sub("P = ", "", fmt_p(p_show))
    )
  )
  
  ggplot2::ggplot() +
    
    ggplot2::geom_ribbon(
      data = pred,
      ggplot2::aes(x = .data[[x]], ymin = lwr, ymax = upr, fill = STP_level),
      alpha = 0.15, colour = NA
    ) +
    
    ggplot2::geom_line(
      data = pred,
      ggplot2::aes(x = .data[[x]], y = fit, colour = STP_level),
      linewidth = 0.75
    ) +
    
    ggplot2::geom_point(
      data = d,
      ggplot2::aes(x = .data[[x]], y = .data[[y]], fill = Treatments),
      shape = 21, size = 1.9, stroke = 0.35,
      colour = "black", alpha = 0.92
    ) +
    
    ggplot2::geom_text(
      data = ann,
      ggplot2::aes(x = x_ann, y = y_ann, label = lab),
      inherit.aes = FALSE,
      hjust = 0, vjust = 1,
      size = 2.3, fontface = "plain",
      family = base_font, lineheight = 0.90
    ) +
    
    ggplot2::scale_colour_manual(values = c(
      "10th percentile" = COL_STP_10,
      "50th percentile" = COL_STP_50,
      "90th percentile" = COL_STP_90
    )) +    
    ggplot2::scale_fill_manual(values = c(
      "10th percentile" = COL_STP_10,
      "50th percentile" = COL_STP_50,
      "90th percentile" = COL_STP_90,
      trt_cols
    )) +
    
    ggplot2::labs(
      x   = axis_labs[[x]],
      y   = axis_labs[[y]],
      tag = paste0("(", tolower(spec_row$panel), ")")
    ) +
    
    ggplot2::coord_cartesian(clip = "off") +
    frac_theme
}

plots <- purrr::map(seq_len(nrow(model_specs)),
                    function(i) make_panel(model_specs[i, ], fits[[i]]))
names(plots) <- model_specs$panel


## ============================================================
## 12. Banner strips
## (annotate size is in mm -> divide by .pt for true pt size)
## ============================================================

void_clean <- ggplot2::theme_void(base_family = base_font) +
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    plot.margin      = ggplot2::margin(0, 0, 0, 0)
  )


domain_strip <- function(label, fill_col) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "rect",
      xmin = 0, xmax = 1, ymin = 0, ymax = 1,
      fill = fill_col, colour = NA
    ) +
    ggplot2::annotate(
      "text",
      x = 0.5, y = 0.5,
      label = label,
      colour = "white",
      fontface = "plain",
      size = 8 / .pt,          ## 8 pt
      family = base_font
    ) +
    ggplot2::coord_cartesian(
      xlim = c(0, 1), ylim = c(0, 1),
      expand = FALSE
    ) +
    void_clean
}


## ============================================================
## 13. Shared legend
## ============================================================

shared_stp_legend <- function() {
  txt_size <- 7 / .pt     ## 7 pt
  seg_size <- 1.4
  
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text", x = 0.06, y = 0.5,
      label = "STP percentile:",
      size = txt_size, family = base_font, hjust = 0
    ) +
    ggplot2::annotate(
      "segment", x = 0.34, xend = 0.39,
      y = 0.5, yend = 0.5,
      colour = COL_STP_10, linewidth = seg_size
    ) +
    ggplot2::annotate(
      "text", x = 0.405, y = 0.5,
      label = "10th", size = txt_size, family = base_font, hjust = 0
    ) +
    ggplot2::annotate(
      "segment", x = 0.52, xend = 0.57,
      y = 0.5, yend = 0.5,
      colour = COL_STP_50, linewidth = seg_size
    ) +
    ggplot2::annotate(
      "text", x = 0.585, y = 0.5,
      label = "50th", size = txt_size, family = base_font, hjust = 0
    ) +
    ggplot2::annotate(
      "segment", x = 0.70, xend = 0.75,
      y = 0.5, yend = 0.5,
      colour = COL_STP_90, linewidth = seg_size
    ) +
    ggplot2::annotate(
      "text", x = 0.765, y = 0.5,
      label = "90th", size = txt_size, family = base_font, hjust = 0
    ) +
    ggplot2::coord_cartesian(
      xlim = c(0, 1), ylim = c(0, 1),
      expand = FALSE
    ) +
    void_clean
}


## ============================================================
## 14. Assembly — Option A (3 cols x 2 rows)
##
##   ┌──────────────────────────────────────────┐
##   │          MAOM domain   (banner)          │
##   ├─────────────┬─────────────┬──────────────┤
##   │    (a)      │     (b)     │     (c)      │
##   ├─────────────┴─────────────┴──────────────┤
##   │          POM control   (banner)          │
##   ├─────────────┬─────────────┬──────────────┤
##   │    (d)      │     (e)     │     (f)      │
##   ├─────────────┴─────────────┴──────────────┤
##   │                Legend                    │
##   └──────────────────────────────────────────┘
## ============================================================

maom_strip <- domain_strip("MAOM domain", COL_MAOM)
pom_strip  <- domain_strip("POM control", COL_POM)

maom_row <- plots$A + plots$B + plots$C +
  patchwork::plot_layout(ncol = 3, widths = c(1, 1, 1))

pom_row <- plots$D + plots$E + plots$F +
  patchwork::plot_layout(ncol = 3, widths = c(1, 1, 1))


Fraction_level_model_figure <-
  
  maom_strip /
  
  maom_row /
  
  pom_strip /
  
  pom_row /
  
  shared_stp_legend() +
  
  patchwork::plot_layout(
    heights = c(
      0.22,   # MAOM banner
      3.00,   # MAOM panel row (a, b, c)
      0.22,   # POM banner
      3.00,   # POM panel row (d, e, f)
      0.40    # Legend
    )
  ) +
  
  patchwork::plot_annotation(
    theme = ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin      = ggplot2::margin(2, 2, 2, 2)
    )
  )

print(Fraction_level_model_figure)


## ============================================================
## 15. Statistics tables
## ============================================================

model_overview <- purrr::map_dfr(seq_along(fits), function(i) {
  panel_id <- names(fits)[i]
  spec     <- model_specs[i, ]
  model    <- fits[[i]]$model
  sm <- summary(model)
  gl <- broom::glance(model)
  tibble::tibble(
    panel = panel_id, domain = spec$domain, title = spec$title,
    formula = get_formula_text(model),
    n = stats::nobs(model),
    r_squared = sm$r.squared, adj_r_squared = sm$adj.r.squared,
    sigma = gl$sigma, statistic = gl$statistic,
    model_p = model_p_value(model),
    AIC = stats::AIC(model), BIC = stats::BIC(model)
  )
})

model_coefficients <- purrr::map_dfr(seq_along(fits), function(i) {
  panel_id <- names(fits)[i]
  spec     <- model_specs[i, ]
  broom::tidy(fits[[i]]$model, conf.int = TRUE) %>%
    dplyr::mutate(
      panel = panel_id, domain = spec$domain,
      title = spec$title,
      formula = get_formula_text(fits[[i]]$model),
      .before = 1
    )
})

anova_tables <- purrr::map_dfr(seq_along(fits), function(i) {
  panel_id <- names(fits)[i]
  spec     <- model_specs[i, ]
  broom::tidy(stats::anova(fits[[i]]$model)) %>%
    dplyr::mutate(
      panel = panel_id, domain = spec$domain,
      title = spec$title, .before = 1
    )
})

standardised_coefficients <- purrr::map_dfr(seq_along(fits), function(i) {
  panel_id <- names(fits)[i]
  spec     <- model_specs[i, ]
  d    <- fits[[i]]$data
  form <- fits[[i]]$formula
  broom::tidy(std_fit(form, d), conf.int = TRUE) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::mutate(
      panel = panel_id, domain = spec$domain,
      title = spec$title,
      formula = paste(deparse(form), collapse = " "),
      .before = 1
    )
})


## ============================================================
## 16. Treatment summary
## ============================================================

fraction_vars <- unique(c(required_vars, model_specs$x, model_specs$y))

treatment_summary <- df %>%
  dplyr::select(Treatments, dplyr::all_of(fraction_vars)) %>%
  tidyr::pivot_longer(
    -Treatments,
    names_to  = "variable",
    values_to = "value"
  ) %>%
  dplyr::group_by(Treatments, variable) %>%
  dplyr::summarise(
    n    = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd   = stats::sd(value, na.rm = TRUE),
    se   = se(value),
    min  = min(value, na.rm = TRUE),
    max  = max(value, na.rm = TRUE),
    .groups = "drop"
  )

treatment_mean_sd_wide <- treatment_summary %>%
  dplyr::mutate(
    mean_sd = paste0(round(mean, 3), " ± ", round(sd, 3))
  ) %>%
  dplyr::select(Treatments, variable, mean_sd) %>%
  tidyr::pivot_wider(names_from = Treatments, values_from = mean_sd)


## ============================================================
## 17. Save Excel
## ============================================================

excel_path <- file.path(
  out_dir,
  paste0("Fig_S15_Fraction_Level_STP_6Panel_Statistics_", stamp, ".xlsx")
)

writexl::write_xlsx(
  list(
    input_data_wide           = df,
    model_map                 = model_specs,
    treatment_summary         = treatment_summary,
    treatment_mean_SD         = treatment_mean_sd_wide,
    model_overview            = model_overview,
    model_coefficients        = model_coefficients,
    anova_tables              = anova_tables,
    standardised_coefficients = standardised_coefficients
  ),
  path = excel_path
)


## ============================================================
## 18. Save figure
## ============================================================

fig_base <- file.path(
  out_dir,
  paste0("Fig_S15_Fraction_Level_STP_6Panel_Figure_", stamp)
)

## TIFF
ggplot2::ggsave(
  filename    = paste0(fig_base, ".tiff"),
  plot        = Fraction_level_model_figure,
  width       = fig_w_mm,
  height      = fig_h_mm,
  units       = "mm",
  dpi         = 600,
  bg          = "white",
  device      = "tiff",
  compression = "lzw",
  limitsize   = FALSE
)

## PDF
pdf_device <- function(filename, width, height, ...) {
  if (capabilities("cairo")) {
    grDevices::cairo_pdf(filename, width = width, height = height,
                         family = base_font, ...)
  } else {
    grDevices::pdf(filename, width = width, height = height,
                   family = base_font, useDingbats = FALSE, ...)
  }
}

ggplot2::ggsave(
  filename  = paste0(fig_base, ".pdf"),
  plot      = Fraction_level_model_figure,
  width     = fig_w_mm,
  height    = fig_h_mm,
  units     = "mm",
  bg        = "white",
  device    = pdf_device,
  limitsize = FALSE
)

## PNG
ggplot2::ggsave(
  filename  = paste0(fig_base, ".png"),
  plot      = Fraction_level_model_figure,
  width     = fig_w_mm,
  height    = fig_h_mm,
  units     = "mm",
  dpi       = 600,
  bg        = "white",
  limitsize = FALSE
)


## ============================================================
## 19. Done
## ============================================================

message("")
message("Supplementary Figure S15 completed.")
message("Dimensions: ", fig_w_mm, " x ", fig_h_mm, " mm")
message("")
message("Panels retained (Option A, 3 cols x 2 rows):")
message("(a) MCF  vs RC      (b) MAOC vs MCF     (c) MAOP vs MCF")
message("(d) POC  vs RC      (e) POP  vs RC      (f) POC  vs POP")
message("")
message("Figure S15: ", fig_base, ".tiff/.pdf/.png")
message("Statistics: ", excel_path)
message("")

Fraction_level_model_figure