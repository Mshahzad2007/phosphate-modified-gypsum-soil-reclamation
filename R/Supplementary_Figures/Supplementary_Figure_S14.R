
rm(list = ls())
graphics.off()
cat("\014")

################################################################################
# SUPPLEMENTARY FIGURE S14 — NC portrait page (15.5 × 13.5 cm)
#
# (a) POM: RC decrease × POP percentile -> PCF
# (b) POM: predictors of POC
# (c) Unique RC contribution to C models: MAOM versus POM
#
# Layout:
#
#     (a) POM interaction        (b) POM predictors of POC
#
#     (c) Unique RC contribution to C models — full width
#
# Main-figure MAOM analyses are intentionally not repeated.
################################################################################


## ============================================================
## 0) Packages
## ============================================================

pkgs <- c(
  "readxl", "dplyr", "tidyr", "stringr", "tibble",
  "ggplot2", "patchwork", "broom", "scales",
  "openxlsx", "grid"
)

to_install <- pkgs[!pkgs %in% rownames(installed.packages())]

if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}

suppressPackageStartupMessages({
  invisible(lapply(pkgs, library, character.only = TRUE))
})


## ============================================================
## 1) Input / output
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

out_dir <- file.path("outputs", "FigS14_POM_Control")
dir.create(
  out_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

stamp <- format(
  Sys.time(),
  "%Y%m%d_%H%M%S"
)

word_w <- 15.5  # cm: insert at 100% size in A4 portrait Word
word_h <- 13.5  # cm: three-panel 2-over-1 layout

base_font <- "sans"

## Keep the original 800 bootstrap replicates (no model changes)
BOOT_B <- 800


## ============================================================
## 2) Colours
## ============================================================

col_pos <- "#1565C0"
col_neg <- "#C62828"

domain_cols <- c(
  "MAOM" = "#083C5A",
  "POM"  = "#5E5175"
)

pal_P <- c(
  "10th" = "#C62828",
  "50th" = "#1565C0",
  "90th" = "#2E7D32"
)


## ============================================================
## 3) Helper functions
## ============================================================

norm_key <- function(x) {
  
  x <- as.character(x)
  
  x <- stringr::str_replace_all(
    x,
    "\u00A0",
    " "
  )
  
  x <- stringr::str_squish(x)
  
  tolower(
    gsub(
      "[^a-z0-9]+",
      "",
      x
    )
  )
}


find_col <- function(nm, possible) {
  
  nm_key <- norm_key(nm)
  possible_key <- norm_key(possible)
  
  hit <- which(
    nm_key %in% possible_key
  )
  
  if (length(hit) == 0) {
    return(NA_character_)
  }
  
  nm[hit[1]]
}


safe_numeric <- function(x) {
  
  suppressWarnings(
    as.numeric(x)
  )
}


canon_treatment <- function(x) {
  
  x <- stringr::str_replace_all(
    as.character(x),
    "\u00A0",
    " "
  )
  
  x <- stringr::str_squish(x)
  
  dplyr::case_when(
    
    x %in% c("PGP", "Peg", "peg", "PEG") ~ "PEG",
    
    x %in% c("PG", "Gp", "gp", "GP") ~ "GP",
    
    x %in% c("Ck", "ck", "CK") ~ "CK",
    
    x %in% c("Cl", "cl", "CL") ~ "CL",
    
    TRUE ~ x
  )
}


canon_var <- function(x) {
  
  x_raw <- stringr::str_replace_all(
    as.character(x),
    "\u00A0",
    " "
  )
  
  x_raw <- stringr::str_squish(x_raw)
  
  key <- norm_key(x_raw)
  
  dplyr::case_when(
    
    key %in% c(
      "reactiveca",
      "rc"
    ) ~ "Reactive_Ca",
    
    key %in% c(
      "maomc",
      "maoc",
      "maomcarbon",
      "maomorganiccarbon"
    ) ~ "MAOM_C",
    
    key %in% c(
      "maomp",
      "maop",
      "maomphosphorus"
    ) ~ "MAOM_P",
    
    key %in% c(
      "maomcaf3",
      "maomcf",
      "mcf",
      "maomcafraction3"
    ) ~ "MAOM_Ca_F3",
    
    key %in% c(
      "pomc",
      "poc",
      "pomcarbon",
      "pomorganiccarbon"
    ) ~ "POM_C",
    
    key %in% c(
      "pomp",
      "pop",
      "pomphosphorus"
    ) ~ "POM_P",
    
    key %in% c(
      "pomcaf3",
      "pomcf",
      "pcf",
      "pomcafraction3"
    ) ~ "POM_Ca_F3",
    
    TRUE ~ x_raw
  )
}


std_data <- function(dat, cols) {
  
  d <- dat
  
  d[cols] <- lapply(
    d[cols],
    function(z) as.numeric(scale(z))
  )
  
  d
}


sig_lab <- function(p) {
  
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE ~ ""
  )
}


partial_r2_for_term <- function(fit, term_name) {
  
  if (!inherits(fit, "lm")) {
    return(NA_real_)
  }
  
  s <- summary(fit)
  
  if (!term_name %in% rownames(s$coefficients)) {
    return(NA_real_)
  }
  
  t_val <- s$coefficients[
    term_name,
    "t value"
  ]
  
  df_res <- fit$df.residual
  
  (t_val^2) /
    (t_val^2 + df_res)
}


delta_r2_for_rc <- function(full_model, reduced_model) {
  
  if (
    !inherits(full_model, "lm") ||
    !inherits(reduced_model, "lm")
  ) {
    return(NA_real_)
  }
  
  summary(full_model)$r.squared -
    summary(reduced_model)$r.squared
}


model_fit_stats <- function(fit, model_name) {
  
  if (!inherits(fit, "lm")) {
    
    return(
      tibble::tibble(
        model = as.character(model_name),
        formula = NA_character_,
        r.squared = NA_real_,
        adj.r.squared = NA_real_,
        AIC = NA_real_,
        BIC = NA_real_,
        n = NA_integer_
      )
    )
  }
  
  s <- summary(fit)
  
  tibble::tibble(
    
    model = as.character(model_name),
    
    formula = paste(
      deparse(
        stats::formula(fit)
      ),
      collapse = " "
    ),
    
    r.squared = unname(
      s$r.squared
    ),
    
    adj.r.squared = unname(
      s$adj.r.squared
    ),
    
    AIC = tryCatch(
      as.numeric(
        stats::AIC(fit)
      ),
      error = function(e) NA_real_
    ),
    
    BIC = tryCatch(
      as.numeric(
        stats::BIC(fit)
      ),
      error = function(e) NA_real_
    ),
    
    n = stats::nobs(fit)
  )
}


## ============================================================
## Bootstrap prediction ribbons
## ============================================================

boot_pred_ribbon <- function(
    formula,
    data,
    newdata,
    B = 800,
    seed = 1
) {
  
  set.seed(seed)
  
  n <- nrow(data)
  
  preds <- matrix(
    NA_real_,
    nrow = B,
    ncol = nrow(newdata)
  )
  
  for (b in seq_len(B)) {
    
    idx <- sample.int(
      n,
      n,
      replace = TRUE
    )
    
    fit <- try(
      lm(
        formula,
        data = data[
          idx,
          ,
          drop = FALSE
        ]
      ),
      silent = TRUE
    )
    
    if (inherits(fit, "try-error")) {
      next
    }
    
    preds[b, ] <- as.numeric(
      predict(
        fit,
        newdata = newdata
      )
    )
  }
  
  fit0 <- lm(
    formula,
    data = data
  )
  
  mu <- as.numeric(
    predict(
      fit0,
      newdata = newdata
    )
  )
  
  lwr <- apply(
    preds,
    2,
    function(z) {
      
      z <- z[
        is.finite(z)
      ]
      
      if (length(z) < 30) {
        return(NA_real_)
      }
      
      quantile(
        z,
        0.025,
        na.rm = TRUE
      )
    }
  )
  
  upr <- apply(
    preds,
    2,
    function(z) {
      
      z <- z[
        is.finite(z)
      ]
      
      if (length(z) < 30) {
        return(NA_real_)
      }
      
      quantile(
        z,
        0.975,
        na.rm = TRUE
      )
    }
  )
  
  dplyr::bind_cols(
    newdata,
    tibble::tibble(
      fit = mu,
      lwr = lwr,
      upr = upr
    )
  )
}


## ============================================================
## 4) Shared theme
## ============================================================

theme_s14 <- ggplot2::theme_classic(
  base_size = 9.5,
  base_family = base_font
) +
  ggplot2::theme(
    
    plot.background =
      ggplot2::element_rect(
        fill = "white",
        colour = NA
      ),
    
    panel.background =
      ggplot2::element_rect(
        fill = "white",
        colour = NA
      ),
    
    axis.line =
      ggplot2::element_line(
        linewidth = 0.55,
        colour = "black"
      ),
    
    axis.ticks =
      ggplot2::element_line(
        linewidth = 0.50,
        colour = "black"
      ),
    
    axis.ticks.length =
      grid::unit(
        1.7,
        "mm"
      ),
    
    axis.title.x =
      ggplot2::element_text(
        size = 8.8,
        colour = "black",
        face = "plain",
        margin = ggplot2::margin(t = 4)
      ),
    
    axis.title.y =
      ggplot2::element_text(
        size = 8.8,
        colour = "black",
        face = "plain",
        margin = ggplot2::margin(r = 5)
      ),
    
    axis.text.x =
      ggplot2::element_text(
        size = 8.0,
        colour = "black",
        face = "plain"
      ),
    
    axis.text.y =
      ggplot2::element_text(
        size = 8.0,
        colour = "black",
        face = "plain"
      ),
    
    plot.title =
      ggplot2::element_blank(),
    
    plot.tag =
      ggplot2::element_text(
        size = 11.5,
        colour = "black",
        face = "bold",
        family = base_font
      ),
    
    plot.tag.position =
      c(0.01, 0.99),
    
    legend.title =
      ggplot2::element_text(
        size = 8.0,
        colour = "black",
        face = "plain"
      ),
    
    legend.text =
      ggplot2::element_text(
        size = 7.8,
        colour = "black",
        face = "plain"
      ),
    
    legend.key.width =
      grid::unit(
        6,
        "mm"
      ),
    
    legend.key.height =
      grid::unit(
        3.5,
        "mm"
      ),
    
    plot.margin =
      ggplot2::margin(
        5,
        6,
        5,
        6
      )
  )


## ============================================================
## 5) Read and clean data
## ============================================================

if (!file.exists(file_path)) {
  
  stop(
    paste0(
      "File not found: ",
      file_path,
      "\nPut Conventional_Treatments.xlsx in your working directory."
    ),
    call. = FALSE
  )
}


raw <- readxl::read_excel(
  file_path,
  sheet = sheet_name,
  .name_repair = "minimal"
)


names(raw) <- names(raw) %>%
  
  stringr::str_replace_all(
    "\u00A0",
    " "
  ) %>%
  
  stringr::str_squish()


col_treat <- find_col(
  names(raw),
  c(
    "Treatments",
    "Treatment",
    "Trt"
  )
)


col_rep <- find_col(
  names(raw),
  c(
    "Replicates",
    "Replicate",
    "Rep"
  )
)


col_var <- find_col(
  names(raw),
  c(
    "response.varibale",
    "response.variable",
    "Variable",
    "Response"
  )
)


col_val <- find_col(
  names(raw),
  c(
    "value",
    "Value",
    "Values"
  )
)


if (
  any(
    is.na(
      c(
        col_treat,
        col_rep,
        col_var,
        col_val
      )
    )
  )
) {
  
  stop(
    paste0(
      "Could not detect required columns.\n",
      "Need columns equivalent to:\n",
      "Treatments | Replicates | response.varibale | value\n\n",
      "Available columns:\n",
      paste(
        names(raw),
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


dat_long <- raw %>%
  
  dplyr::transmute(
    
    Treatments =
      canon_treatment(
        .data[[col_treat]]
      ),
    
    Replicates =
      factor(
        .data[[col_rep]]
      ),
    
    Raw_variable =
      stringr::str_squish(
        as.character(
          .data[[col_var]]
        )
      ),
    
    Variable =
      canon_var(
        .data[[col_var]]
      ),
    
    Value =
      safe_numeric(
        .data[[col_val]]
      )
  ) %>%
  
  dplyr::filter(
    !is.na(Treatments),
    !is.na(Replicates),
    !is.na(Variable),
    !is.na(Value)
  )


needed_vars <- c(
  "Reactive_Ca",
  "MAOM_C",
  "MAOM_P",
  "MAOM_Ca_F3",
  "POM_C",
  "POM_P",
  "POM_Ca_F3"
)


missing_vars <- setdiff(
  needed_vars,
  unique(
    dat_long$Variable
  )
)


if (length(missing_vars) > 0) {
  
  stop(
    paste0(
      "Missing required variables after cleaning: ",
      paste(
        missing_vars,
        collapse = ", "
      ),
      "\n\nRaw variables detected:\n",
      paste(
        sort(
          unique(
            dat_long$Raw_variable
          )
        ),
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


wide_df <- dat_long %>%
  
  dplyr::filter(
    Variable %in% needed_vars
  ) %>%
  
  dplyr::group_by(
    Treatments,
    Replicates,
    Variable
  ) %>%
  
  dplyr::summarise(
    Value = mean(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  
  tidyr::pivot_wider(
    names_from = Variable,
    values_from = Value
  )


## ============================================================
## 6) MAOM and POM datasets
## ============================================================

df_maom <- wide_df %>%
  
  dplyr::filter(
    stats::complete.cases(
      Reactive_Ca,
      MAOM_C,
      MAOM_P,
      MAOM_Ca_F3
    )
  )


df_pom <- wide_df %>%
  
  dplyr::filter(
    stats::complete.cases(
      Reactive_Ca,
      POM_C,
      POM_P,
      POM_Ca_F3
    )
  )


if (nrow(df_maom) < 6) {
  
  warning(
    "MAOM model has fewer than 6 complete rows. Interpret cautiously.",
    call. = FALSE
  )
}


if (nrow(df_pom) < 6) {
  
  warning(
    "POM model has fewer than 6 complete rows. Interpret cautiously.",
    call. = FALSE
  )
}


## ============================================================
## 7) Standardized datasets
## ============================================================

df_std_maom <- std_data(
  df_maom,
  c(
    "Reactive_Ca",
    "MAOM_C",
    "MAOM_P",
    "MAOM_Ca_F3"
  )
)


df_std_pom <- std_data(
  df_pom,
  c(
    "Reactive_Ca",
    "POM_C",
    "POM_P",
    "POM_Ca_F3"
  )
)


## ============================================================
## 8) Models
## ============================================================

## ------------------------------------------------------------
## Panel A
## POM Ca allocation interaction model
## ------------------------------------------------------------

m_pom_inter <- lm(
  POM_Ca_F3 ~ Reactive_Ca * POM_P,
  data = df_pom
)


## ------------------------------------------------------------
## Full C models used for Panel C
## ------------------------------------------------------------

m_maom_c <- lm(
  MAOM_C ~ MAOM_Ca_F3 + MAOM_P + Reactive_Ca,
  data = df_std_maom
)


m_pom_c <- lm(
  POM_C ~ POM_Ca_F3 + POM_P + Reactive_Ca,
  data = df_std_pom
)


## ------------------------------------------------------------
## Reduced models without RC
## ------------------------------------------------------------

m_maom_c_red <- lm(
  MAOM_C ~ MAOM_Ca_F3 + MAOM_P,
  data = df_std_maom
)


m_pom_c_red <- lm(
  POM_C ~ POM_Ca_F3 + POM_P,
  data = df_std_pom
)


## ------------------------------------------------------------
## Panel B
## Marginal single-predictor POM models
## ------------------------------------------------------------

m_pom_c_pcf <- lm(
  POM_C ~ POM_Ca_F3,
  data = df_std_pom
)


m_pom_c_pop <- lm(
  POM_C ~ POM_P,
  data = df_std_pom
)


m_pom_c_rc <- lm(
  POM_C ~ Reactive_Ca,
  data = df_std_pom
)


## ============================================================
## 9) PANEL A
## POM: RC decrease × POP percentile -> PCF
## ============================================================

pop_levels <- as.numeric(
  stats::quantile(
    df_pom$POM_P,
    probs = c(
      0.10,
      0.50,
      0.90
    ),
    na.rm = TRUE
  )
)


rc_pom_seq <- seq(
  min(
    df_pom$Reactive_Ca,
    na.rm = TRUE
  ),
  max(
    df_pom$Reactive_Ca,
    na.rm = TRUE
  ),
  length.out = 160
)


nd_pom <- dplyr::bind_rows(
  
  lapply(
    seq_along(pop_levels),
    
    function(i) {
      
      tibble::tibble(
        Reactive_Ca = rc_pom_seq,
        POM_P = pop_levels[i],
        P_level = names(pal_P)[i]
      )
    }
  )
)


pred_pom <- boot_pred_ribbon(
  
  POM_Ca_F3 ~ Reactive_Ca * POM_P,
  
  data = df_pom,
  
  newdata = nd_pom,
    B = BOOT_B,
  
  seed = 102
  
) %>%
  
  dplyr::transmute(
    
    Reactive_Ca = Reactive_Ca,
    
    RC_decrease =
      max(
        df_pom$Reactive_Ca,
        na.rm = TRUE
      ) -
      Reactive_Ca,
    
    P_level =
      factor(
        P_level,
        levels = names(pal_P)
      ),
    
    fit = fit,
    
    lwr = lwr,
    
    upr = upr
  )


pA <- ggplot2::ggplot(
  pred_pom,
  ggplot2::aes(
    x = RC_decrease,
    y = fit,
    colour = P_level,
    fill = P_level
  )
) +
  
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = lwr,
      ymax = upr
    ),
    alpha = 0.22,
    colour = NA
  ) +
  
  ggplot2::geom_line(
    linewidth = 0.80
  ) +
  
  ggplot2::scale_colour_manual(
    values = pal_P,
    name = "POP percentile"
  ) +
  
  ggplot2::scale_fill_manual(
    values = pal_P,
    name = "POP percentile"
  ) +
  
  ggplot2::guides(
    
    colour =
      ggplot2::guide_legend(
        title = "POP percentile",
        nrow = 1,
        byrow = TRUE
      ),
    
    fill =
      ggplot2::guide_legend(
        title = "POP percentile",
        nrow = 1,
        byrow = TRUE
      )
  ) +
  
  ggplot2::labs(
    x = "RC decrease",
    y = expression(
      PCF~(cmol[c]~kg^{-1})
    ),
    tag = "(a)"
  ) +
  
  ggplot2::coord_cartesian(
    clip = "off"
  ) +
  
  theme_s14 +
  
  ggplot2::theme(
    
    legend.position = "bottom",
    
    legend.direction = "horizontal",
    
    legend.box = "horizontal",
    
    legend.margin =
      ggplot2::margin(
        0, 0, 0, 0
      ),
    
    legend.box.margin =
      ggplot2::margin(
        -4, 0, -4, 0
      ),
    
    plot.margin =
      ggplot2::margin(
        5, 6, 4, 5
      )
  )


## ============================================================
## 10) PANEL B
## POM: predictors of POC
## ============================================================

coef_pom <- dplyr::bind_rows(
  
  ## PCF
  broom::tidy(
    m_pom_c_pcf,
    conf.int = TRUE,
    conf.level = 0.95
  ) %>%
    
    dplyr::filter(
      term == "POM_Ca_F3"
    ) %>%
    
    dplyr::transmute(
      Driver = "PCF",
      estimate = estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      p.value = p.value
    ),
  
  
  ## POP
  broom::tidy(
    m_pom_c_pop,
    conf.int = TRUE,
    conf.level = 0.95
  ) %>%
    
    dplyr::filter(
      term == "POM_P"
    ) %>%
    
    dplyr::transmute(
      Driver = "POP",
      estimate = estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      p.value = p.value
    ),
  
  
  ## RC
  broom::tidy(
    m_pom_c_rc,
    conf.int = TRUE,
    conf.level = 0.95
  ) %>%
    
    dplyr::filter(
      term == "Reactive_Ca"
    ) %>%
    
    dplyr::transmute(
      Driver = "RC",
      estimate = estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      p.value = p.value
    )
)


coef_pom <- coef_pom %>%
  
  dplyr::mutate(
    
    ## bottom -> top:
    ## RC, POP, PCF
    Driver = factor(
      Driver,
      levels = c(
        "RC",
        "POP",
        "PCF"
      )
    ),
    
    Driver_y =
      as.numeric(
        Driver
      ),
    
    Sign =
      ifelse(
        estimate >= 0,
        "Positive",
        "Negative"
      ),
    
    Label =
      paste0(
        sprintf(
          "%.2f",
          estimate
        ),
        sig_lab(
          p.value
        )
      ),
    
    Label_y =
      Driver_y +
      0.16
  )


## ------------------------------------------------------------
## Automatic X-range
## ------------------------------------------------------------

x_vals <- c(
  coef_pom$conf.low,
  coef_pom$conf.high,
  coef_pom$estimate
)

x_vals <- x_vals[
  is.finite(x_vals)
]


if (length(x_vals) == 0) {
  
  x_rng <- c(-1, 1)
  
} else {
  
  x_rng <- range(
    x_vals,
    na.rm = TRUE
  )
}


x_span <- diff(x_rng)

if (
  !is.finite(x_span) ||
  x_span == 0
) {
  x_span <- 1
}


x_limits <- c(
  x_rng[1] - 0.30 * x_span,
  x_rng[2] + 0.30 * x_span
)


cap_h <- 0.075


pB <- ggplot2::ggplot(
  coef_pom,
  ggplot2::aes(
    y = Driver_y,
    x = estimate,
    colour = Sign
  )
) +
  
  ggplot2::geom_vline(
    xintercept = 0,
    linewidth = 0.45,
    colour = "grey45"
  ) +
  
  ggplot2::geom_segment(
    ggplot2::aes(
      x = conf.low,
      xend = conf.high,
      y = Driver_y,
      yend = Driver_y
    ),
    linewidth = 0.72
  ) +
  
  ggplot2::geom_segment(
    ggplot2::aes(
      x = conf.low,
      xend = conf.low,
      y = Driver_y - cap_h,
      yend = Driver_y + cap_h
    ),
    linewidth = 0.72
  ) +
  
  ggplot2::geom_segment(
    ggplot2::aes(
      x = conf.high,
      xend = conf.high,
      y = Driver_y - cap_h,
      yend = Driver_y + cap_h
    ),
    linewidth = 0.72
  ) +
  
  ggplot2::geom_point(
    size = 3.0
  ) +
  
  ggplot2::geom_text(
    ggplot2::aes(
      x = estimate,
      y = Label_y,
      label = Label
    ),
    size = 2.7,
    colour = "black",
    family = base_font,
    vjust = 0
  ) +
  
  ggplot2::scale_colour_manual(
    values = c(
      "Positive" = col_pos,
      "Negative" = col_neg
    ),
    guide = "none"
  ) +
  
  ggplot2::scale_y_continuous(
    breaks = c(1, 2, 3),
    labels = c(
      "RC",
      "POP",
      "PCF"
    ),
    expand =
      ggplot2::expansion(
        mult = c(
          0.20,
          0.36
        )
      )
  ) +
  
  ggplot2::scale_x_continuous(
    breaks =
      scales::pretty_breaks(
        n = 5
      ),
    expand =
      ggplot2::expansion(
        mult = c(
          0.02,
          0.02
        )
      )
  ) +
  
  ggplot2::coord_cartesian(
    xlim = x_limits,
    clip = "off"
  ) +
  
  ggplot2::labs(
    x = expression(
      "Marginal standardised " * beta * " (95% CI)"
    ),
    y = "Predictor of POC",
    tag = "(b)"
  ) +
  
  theme_s14 +
  
  ggplot2::theme(
    plot.margin =
      ggplot2::margin(
        5, 6, 4, 5
      )
  )


## ============================================================
## 11) PANEL C
## Unique RC contribution to C models
## MAOM versus POM
##
## Colour now represents contribution metric:
## Delta R2   = blue
## Partial R2 = purple
##
## MAOM/POM are identified directly by text labels.
## ============================================================

metric_cols <- c(
  "Delta R2"   = "#4C78A8",
  "Partial R2" = "#8E6C9F"
)


rc_contrib <- tibble::tibble(
  
  Domain =
    rep(
      c(
        "MAOM",
        "POM"
      ),
      each = 2
    ),
  
  Metric =
    rep(
      c(
        "Delta R2",
        "Partial R2"
      ),
      times = 2
    ),
  
  Value =
    c(
      
      ## MAOM Delta R2
      delta_r2_for_rc(
        m_maom_c,
        m_maom_c_red
      ),
      
      ## MAOM Partial R2
      partial_r2_for_term(
        m_maom_c,
        "Reactive_Ca"
      ),
      
      ## POM Delta R2
      delta_r2_for_rc(
        m_pom_c,
        m_pom_c_red
      ),
      
      ## POM Partial R2
      partial_r2_for_term(
        m_pom_c,
        "Reactive_Ca"
      )
    )
) %>%
  
  dplyr::mutate(
    
    Domain =
      factor(
        Domain,
        levels = c(
          "MAOM",
          "POM"
        )
      ),
    
    Metric =
      factor(
        Metric,
        levels = c(
          "Delta R2",
          "Partial R2"
        )
      ),
    
    Metric_id =
      as.numeric(
        Metric
      ),
    
    ## Small vertical separation of MAOM and POM
    y_pos =
      dplyr::case_when(
        
        Domain == "MAOM" ~
          Metric_id + 0.17,
        
        Domain == "POM" ~
          Metric_id - 0.17,
        
        TRUE ~
          Metric_id
      ),
    
    Label =
      paste0(
        as.character(Domain),
        " = ",
        sprintf(
          "%.3f",
          Value
        )
      )
  )


## ------------------------------------------------------------
## Automatic label positioning
## ------------------------------------------------------------

x_max_c <- max(
  rc_contrib$Value,
  na.rm = TRUE
)

if (
  !is.finite(x_max_c) ||
  x_max_c <= 0
) {
  
  x_max_c <- 0.10
}


rc_contrib <- rc_contrib %>%
  
  dplyr::mutate(
    
    Label_x =
      dplyr::case_when(
        
        Value <=
          0.02 * x_max_c ~
          
          0.022 * x_max_c,
        
        TRUE ~
          
          Value +
          0.025 * x_max_c
      )
  )


## ------------------------------------------------------------
## Plot
## ------------------------------------------------------------

pC <- ggplot2::ggplot(
  
  rc_contrib,
  
  ggplot2::aes(
    x = Value,
    y = y_pos,
    colour = Metric
  )
) +
  
  ## Zero reference
  ggplot2::geom_vline(
    
    xintercept = 0,
    
    linewidth = 0.45,
    
    colour = "grey50"
  ) +
  
  ## Contribution lines
  ggplot2::geom_segment(
    
    ggplot2::aes(
      x = 0,
      xend = Value,
      y = y_pos,
      yend = y_pos,
      colour = Metric
    ),
    
    linewidth = 0.90,
    
    lineend = "round"
  ) +
  
  ## End points
  ggplot2::geom_point(
    
    ggplot2::aes(
      colour = Metric
    ),
    
    size = 3.2,
    
    shape = 21,
    
    fill = "white",
    
    stroke = 0.85
  ) +
  
  ## MAOM / POM value labels
  ggplot2::geom_text(
    
    ggplot2::aes(
      x = Label_x,
      label = Label
    ),
    
    hjust = 0,
    
    size = 2.8,
    
    colour = "black",
    
    family = base_font
  ) +
  
  ## Metric-specific colours
  ggplot2::scale_colour_manual(
    
    values = metric_cols,
    
    guide = "none"
  ) +
  
  ## Y axis
  ggplot2::scale_y_continuous(
    
    breaks = c(
      1,
      2
    ),
    
    labels = c(
      expression(Delta * R^2),
      expression("Partial " * R^2)
    ),
    
    expand =
      ggplot2::expansion(
        mult = c(
          0.18,
          0.18
        )
      )
  ) +
  
  ## X axis
  ggplot2::scale_x_continuous(
    
    breaks =
      scales::pretty_breaks(
        n = 5
      ),
    
    expand =
      ggplot2::expansion(
        mult = c(
          0.03,
          0.18
        )
      )
  ) +
  
  ggplot2::coord_cartesian(
    
    ylim = c(
      0.55,
      2.45
    ),
    
    clip = "off"
  ) +
  
  ggplot2::labs(
    
    x =
      "Unique RC contribution to C models",
    
    y =
      "Contribution metric",
    
    tag =
      "(c)"
  ) +
  
  theme_s14 +
  
  ggplot2::theme(
    
    plot.margin =
      ggplot2::margin(
        5,
        8,
        6,
        6
      )
  )

## ============================================================
## 12) Assemble final figure
##
## A and B equal width
## C full width
## ============================================================

top_row <- pA + pB +
  
  patchwork::plot_layout(
    ncol = 2,
    widths = c(
      1,
      1
    )
  )


FigS14 <- top_row / pC +
  
  patchwork::plot_layout(
    heights = c(
      1.00,
      0.70
    )
  ) &
  
  ggplot2::theme(
    plot.background =
      ggplot2::element_rect(
        fill = "white",
        colour = NA
      )
  )


print(FigS14)


## ============================================================
## 13) Export statistics
## ============================================================

stats_list <- list(
  
  "Raw_clean_all" =
    dat_long,
  
  "Wide_fraction_data" =
    wide_df,
  
  
  ## Panel A
  "POM_interaction_model" =
    broom::tidy(
      m_pom_inter,
      conf.int = TRUE
    ),
  
  "PanelA_predictions" =
    pred_pom,
  
  
  ## Panel B
  "POM_marginal_PCF" =
    broom::tidy(
      m_pom_c_pcf,
      conf.int = TRUE
    ),
  
  "POM_marginal_POP" =
    broom::tidy(
      m_pom_c_pop,
      conf.int = TRUE
    ),
  
  "POM_marginal_RC" =
    broom::tidy(
      m_pom_c_rc,
      conf.int = TRUE
    ),
  
  "PanelB_coefficients" =
    coef_pom,
  
  
  ## Panel C
  "MAOM_full_C_model" =
    broom::tidy(
      m_maom_c,
      conf.int = TRUE
    ),
  
  "POM_full_C_model" =
    broom::tidy(
      m_pom_c,
      conf.int = TRUE
    ),
  
  "MAOM_reduced_C_model" =
    broom::tidy(
      m_maom_c_red,
      conf.int = TRUE
    ),
  
  "POM_reduced_C_model" =
    broom::tidy(
      m_pom_c_red,
      conf.int = TRUE
    ),
  
  "PanelC_RC_contribution" =
    rc_contrib,
  
  
  ## Model fit summary
  "Model_fits" =
    dplyr::bind_rows(
      
      model_fit_stats(
        m_pom_inter,
        "POM: PCF ~ RC x POP"
      ),
      
      model_fit_stats(
        m_pom_c,
        "POM: POC ~ PCF + POP + RC"
      ),
      
      model_fit_stats(
        m_maom_c,
        "MAOM: MAOC ~ MCF + MAOP + RC"
      ),
      
      model_fit_stats(
        m_pom_c_pcf,
        "POM marginal: POC ~ PCF"
      ),
      
      model_fit_stats(
        m_pom_c_pop,
        "POM marginal: POC ~ POP"
      ),
      
      model_fit_stats(
        m_pom_c_rc,
        "POM marginal: POC ~ RC"
      )
    )
)


stats_file <- file.path(
  out_dir,
  paste0(
    "FigS14_POM_Control_statistics_",
    stamp,
    ".xlsx"
  )
)


openxlsx::write.xlsx(
  stats_list,
  file = stats_file,
  overwrite = TRUE
)


## ============================================================
## 14) Save figure — Word portrait at 15.5 × 13.5 cm
## ============================================================

png_out <- file.path(
  out_dir,
  paste0(
    "FigS14_POM_Control_",
    stamp,
    ".png"
  )
)


tiff_out <- file.path(
  out_dir,
  paste0(
    "FigS14_POM_Control_",
    stamp,
    ".tiff"
  )
)


pdf_out <- file.path(
  out_dir,
  paste0(
    "FigS14_POM_Control_",
    stamp,
    ".pdf"
  )
)


ggplot2::ggsave(
  filename = png_out,
  plot = FigS14,
  width = word_w,
  height = word_h,
  units = "cm",
  dpi = 600,
  bg = "white"
)


ggplot2::ggsave(
  filename = tiff_out,
  plot = FigS14,
  width = word_w,
  height = word_h,
  units = "cm",
  dpi = 600,
  bg = "white",
  device = "tiff",
  compression = "lzw"
)


pdf_device <- if (
  capabilities("cairo")
) {
  grDevices::cairo_pdf
} else {
  grDevices::pdf
}


ggplot2::ggsave(
  filename = pdf_out,
  plot = FigS14,
  width = word_w,
  height = word_h,
  units = "cm",
  bg = "white",
  device = pdf_device
)


## ============================================================
## 15) Done
## ============================================================

message("")
message("Clean Supplementary Fig. 14 saved:")
message("PNG:  ", png_out)
message("TIFF: ", tiff_out)
message("PDF:  ", pdf_out)
message("Stats: ", stats_file)
message("")

FigS14