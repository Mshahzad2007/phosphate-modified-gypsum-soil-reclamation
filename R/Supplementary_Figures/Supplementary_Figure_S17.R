# ==============================================================================
# SUPPLEMENTARY FIGURE S17 — CORRELATION / MICROBIAL ASSOCIATION PANEL
# Cleaned directly from the supplied full Figure 6 script.
# Panels A and B, MAOM regression models, and composite-layout code are removed.
# ==============================================================================

## using this code/// 7/8/26

rm(list = ls())
graphics.off()
cat("\014")
## using this code/// 20/5/26

pkgs <- c(
  "readxl", "dplyr", "tidyr", "stringr", "tibble",
  "ggplot2", "scales",
  "vegan", "openxlsx", "cowplot"
)

for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
invisible(lapply(pkgs, library, character.only = TRUE))

################################################################################
# 1. GLOBAL SETTINGS — SUPPLEMENTARY FIG. S17
################################################################################

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
sheet_data <- "data"
sheet_phy  <- "Phylum"
sheet_gen  <- "Genus_repli"

# Original S17 correlation/microbial analysis only includes CK, GP, PEG.
# P+GP is a separate treatment and must not be merged with PEG.
# Add P+GP only if matching microbial profiles are available and models are rerun.
treat_levels <- c("CK", "GP", "PEG")

MANTEL_PERM <- 999

# Microbial correction:
# If Phylum/Genus have only one microbial value per treatment, the script
# automatically switches the microbial Mantel part to treatment-level means.
# This avoids fake replication such as copying CK microbial values into CK_1, CK_2, CK_3.
MIN_SAMPLE_LEVEL_N <- 6

# When microbial data are treatment-level only, Mantel p-values are not interpreted.
# Links are plotted as descriptive treatment-level associations based on |r| and the
# biological block filter below. Statistics are still saved in the Excel output.
PLOT_TREATMENT_LEVEL_LINKS_AS_DESCRIPTIVE <- TRUE

# Final clean rule:
# show only biologically strong and significant Mantel links
MANTEL_R_MIN       <- 0.40
MANTEL_P_MAX       <- 0.05
MAX_LINKS_PER_BIO  <- 4
MAX_MANTEL_LINKS   <- 16

# Heatmap significance:
# dot = p < 0.05; star = p < 0.01
STAR_P_CUTOFF <- 0.01
DOT_P_CUTOFF  <- 0.05

base_font <- "sans"

out_dir <- "Fig_S17_NC_Portrait_Correlation_Microbial"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

################################################################################
# 2. BASIC HELPERS
################################################################################

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
  x <- dplyr::recode(x, "PGP" = "P+GP")
  factor(x, levels = treat_levels)
}

pick_col <- function(df, choices) {
  hit <- choices[choices %in% names(df)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

theme_maom <- ggplot2::theme_classic(base_size = 11.5, base_family = base_font) +
  ggplot2::theme(
    panel.grid        = ggplot2::element_blank(),
    axis.line         = ggplot2::element_line(linewidth = 0.55, colour = "black"),
    axis.ticks        = ggplot2::element_line(linewidth = 0.55, colour = "black"),
    axis.ticks.length = grid::unit(1.7, "mm"),
    axis.title        = ggplot2::element_text(size = 10.8, face = "bold", colour = "black"),
    axis.text         = ggplot2::element_text(size = 9.6, face = "bold", colour = "black"),
    plot.title        = ggplot2::element_blank(),
    plot.margin       = ggplot2::margin(4, 5, 4, 5),
    legend.title      = ggplot2::element_text(size = 8.0, face = "bold"),
    legend.text       = ggplot2::element_text(size = 7.6, face = "bold"),
    panel.background  = ggplot2::element_rect(fill = "white", colour = NA),
    plot.background   = ggplot2::element_rect(fill = "white", colour = NA)
  )

################################################################################
# 3. READ SOIL DATA FOR CORRELATION PANEL
################################################################################

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
    Treatments_raw  = dplyr::all_of(trt_col),
    Replicates_raw  = dplyr::all_of(rep_col),
    response_raw    = dplyr::all_of(resp_col),
    value_raw       = dplyr::all_of(val_col)
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

################################################################################

# 4. CORRELATION PANEL DATA
################################################################################

var_map <- c(
  "pH"          = "pH",
  "ESP"         = "ESP",
  "HCO\u2083\u207b" = "HCO3",
  "EC"          = "EC",
  "RC"          = "Reactive_Ca",
  "CF"          = "Ca_F3",
  "MCF"         = "MAOM_Ca_F3",
  "PCF"         = "POM_Ca_F3",
  "SAP"         = "SAP",
  "MAOP"        = "MAOM_P",
  "POP"         = "POM_P",
  "MAOC"        = "MAOM_C",
  "POC"         = "POM_C",
  "BD"          = "BD",
  "Ksat"        = "Ksat",
  "MWD"         = "MWD",
  "CEC"         = "CEC",
  "Biomass"     = "Biomass"
)

group_def <- tibble::tribble(
  ~Group,           ~Label,
  "Ionic stress",   "pH",
  "Ionic stress",   "ESP",
  "Ionic stress",   "HCO\u2083\u207b",
  "Ionic stress",   "EC",
  "Ca allocation",  "RC",
  "Ca allocation",  "CF",
  "Ca allocation",  "MCF",
  "Ca allocation",  "PCF",
  "P pools",        "SAP",
  "P pools",        "MAOP",
  "P pools",        "POP",
  "C pools",        "MAOC",
  "C pools",        "POC",
  "Soil physics",   "BD",
  "Soil physics",   "Ksat",
  "Soil physics",   "MWD",
  "Exchange",       "CEC",
  "Plant",          "Biomass"
)

present_labels <- names(var_map)[var_map %in% names(soil_wide_all)]
soil_vars <- var_map[present_labels]

# Replicated soil matrix used for the soil-soil Spearman heatmap.
# This keeps the valid soil replication for the heatmap.
soil_for_corr <- soil_wide_all %>%
  dplyr::select(SampleID, Treatments, dplyr::all_of(unname(soil_vars))) %>%
  dplyr::filter(stats::complete.cases(dplyr::across(dplyr::all_of(unname(soil_vars)))))

plot_labels <- names(soil_vars)
names(plot_labels) <- unname(soil_vars)

soil_mat <- soil_for_corr %>%
  dplyr::select(dplyr::all_of(unname(soil_vars))) %>%
  as.data.frame()

names(soil_mat) <- plot_labels
rownames(soil_mat) <- soil_for_corr$SampleID

# Treatment-level soil matrix used only when microbial data are not replicated.
# This is the correct correction for Phylum/Genus n = 1.
soil_treat_for_bio <- soil_wide_all %>%
  dplyr::group_by(Treatments) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(unname(soil_vars)),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  dplyr::filter(stats::complete.cases(dplyr::across(dplyr::all_of(unname(soil_vars)))))

soil_mat_treat <- soil_treat_for_bio %>%
  dplyr::select(dplyr::all_of(unname(soil_vars))) %>%
  as.data.frame()

names(soil_mat_treat) <- plot_labels
rownames(soil_mat_treat) <- as.character(soil_treat_for_bio$Treatments)

################################################################################
# 5. READ MICROBIAL DATA
################################################################################

read_micro_sheet <- function(file, sheet) {
  x <- readxl::read_excel(file, sheet = sheet)
  
  trt_col <- pick_col(x, c("Treatment", "Treatments", "treatment", "treatments"))
  rep_col <- pick_col(x, c("Replicates", "Replicate", "replicates", "replicate"))
  val_col <- pick_col(x, c("Values", "Value", "value", "Abundance", "abundance"))
  
  if (is.na(trt_col) || is.na(val_col)) {
    stop("Sheet ", sheet, " must contain Treatment/Treatments and Values/value columns.")
  }
  
  if (!is.na(rep_col)) {
    out <- x %>%
      dplyr::rename(
        Treatment_raw  = dplyr::all_of(trt_col),
        Replicates_raw = dplyr::all_of(rep_col),
        Values_raw     = dplyr::all_of(val_col)
      )
  } else {
    out <- x %>%
      dplyr::rename(
        Treatment_raw = dplyr::all_of(trt_col),
        Values_raw    = dplyr::all_of(val_col)
      ) %>%
      dplyr::mutate(Replicates_raw = "1")
  }
  
  out %>%
    dplyr::mutate(
      # Preserve every microbial treatment in the sheet, including CL.
      TreatmentID = stringr::str_trim(as.character(Treatment_raw)),
      TreatmentID = dplyr::recode(TreatmentID, "PGP" = "P+GP"),
      Treatment    = TreatmentID,
      Replicates   = as.character(Replicates_raw),
      Values       = suppressWarnings(as.numeric(Values_raw)),
      SampleID     = paste(TreatmentID, Replicates, sep = "_")
    ) %>%
    dplyr::filter(
      !is.na(TreatmentID),
      TreatmentID != "",
      !is.na(Values)
    )
}

phy_raw <- read_micro_sheet(file_path, sheet_phy)
gen_raw <- read_micro_sheet(file_path, sheet_gen)

phy_col <- pick_col(phy_raw, c("Phylum", "phylum"))
gen_col <- pick_col(gen_raw, c("Genus", "genus"))

if (is.na(phy_col)) stop("Phylum sheet must contain column Phylum.")
if (is.na(gen_col)) stop("Genus_repli sheet must contain column Genus.")

# Phylum has one profile per treatment, so it is treatment-level data.
phy_long <- phy_raw %>%
  dplyr::rename(Phylum = dplyr::all_of(phy_col)) %>%
  dplyr::mutate(Phylum = stringr::str_trim(as.character(Phylum))) %>%
  dplyr::filter(!is.na(Phylum), Phylum != "") %>%
  dplyr::group_by(TreatmentID, Phylum) %>%
  dplyr::summarise(
    Values = mean(Values, na.rm = TRUE),
    n_profiles = dplyr::n(),
    .groups = "drop"
  )

# Genus_repli is mixed: CK/GP/PEG can have one profile, while CL can have
# several profiles. Because Phylum is not replicated, the correct common
# analysis level is treatment. Any available genus replicates are therefore
# averaged within Treatment × Genus; they are NOT copied to other treatments.
gen_long <- gen_raw %>%
  dplyr::rename(Genus = dplyr::all_of(gen_col)) %>%
  dplyr::mutate(Genus = stringr::str_trim(as.character(Genus))) %>%
  dplyr::filter(!is.na(Genus), Genus != "") %>%
  dplyr::group_by(TreatmentID, Genus) %>%
  dplyr::summarise(
    Values = mean(Values, na.rm = TRUE),
    n_profiles = dplyr::n(),
    .groups = "drop"
  )

# Use only treatments actually shared by soil + Phylum + Genus.
# Thus CL is retained in the microbial tables, but if soil has no CL it is
# automatically excluded from soil-microbial links rather than being forced in.
common_treatment_ids <- Reduce(
  intersect,
  list(
    rownames(soil_mat_treat),
    unique(phy_long$TreatmentID),
    unique(gen_long$TreatmentID)
  )
)

common_ids <- rownames(soil_mat_treat)[
  rownames(soil_mat_treat) %in% common_treatment_ids
]

if (length(common_ids) < 3) {
  stop(
    "At least three common treatments are required for the treatment-level ",
    "soil-microbial descriptive analysis. Common treatments found: ",
    paste(common_ids, collapse = ", ")
  )
}

microbial_only_ids <- setdiff(
  union(unique(phy_long$TreatmentID), unique(gen_long$TreatmentID)),
  rownames(soil_mat_treat)
)

microbial_level <- "treatment"
micro_id_col <- "TreatmentID"
soil_mat_bio <- soil_mat_treat[common_ids, , drop = FALSE]
descriptive_micro_links <- TRUE

message(
  "\nMicrobial matching mode: treatment-level",
  "\nCommon soil-microbial treatments: ", paste(common_ids, collapse = ", "),
  if (length(microbial_only_ids) > 0) {
    paste0(
      "\nMicrobial-only treatment(s) retained but not used in soil-microbial links: ",
      paste(microbial_only_ids, collapse = ", ")
    )
  } else {
    ""
  },
  "\nPhylum is non-replicated. Genus values are averaged within treatment ",
  "where multiple profiles exist. No pseudo-replicate matching is used.\n"
)

################################################################################
# 6. SPEARMAN CORRELATIONS
################################################################################

calc_corr_long <- function(mat) {
  vars <- colnames(mat)
  out <- expand.grid(var1 = vars, var2 = vars, stringsAsFactors = FALSE)
  
  out$r <- NA_real_
  out$p <- NA_real_
  
  for (k in seq_len(nrow(out))) {
    x <- mat[[out$var1[k]]]
    y <- mat[[out$var2[k]]]
    
    ok <- is.finite(x) & is.finite(y)
    
    if (sum(ok) >= 4) {
      ct <- suppressWarnings(
        stats::cor.test(x[ok], y[ok], method = "spearman", exact = FALSE)
      )
      out$r <- out$r
      out$r[k] <- unname(ct$estimate)
      out$p[k] <- ct$p.value
    }
  }
  
  out %>%
    dplyr::mutate(
      x_id = match(var2, vars),
      y_id = length(vars) - match(var1, vars) + 1,
      i = match(var1, vars),
      j = match(var2, vars),
      keep = j >= i,
      stars = dplyr::case_when(
        var1 == var2 ~ "",
        is.na(p) ~ "",
        p < STAR_P_CUTOFF ~ "*",
        p < DOT_P_CUTOFF  ~ "\u2022",
        TRUE ~ ""
      )
    ) %>%
    dplyr::filter(keep)
}

corr_long <- calc_corr_long(soil_mat)

################################################################################
# 7. BIOLOGICAL DISTANCE MATRICES
################################################################################

find_taxon <- function(available, target, alternatives = character()) {
  candidates <- c(target, alternatives)
  for (cc in candidates) {
    hit <- available[tolower(available) == tolower(cc)]
    if (length(hit) > 0) return(hit[1])
  }
  return(NA_character_)
}

make_abundance_vector <- function(df, tax_col, tax_name, common_ids, id_col) {
  d <- df %>%
    dplyr::filter(.data[[tax_col]] == tax_name) %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::summarise(Abundance = mean(Values, na.rm = TRUE), .groups = "drop") %>%
    dplyr::rename(ID = dplyr::all_of(id_col))
  
  full <- tibble::tibble(ID = common_ids) %>%
    dplyr::left_join(d, by = "ID") %>%
    dplyr::mutate(Abundance = dplyr::if_else(is.na(Abundance), 0, Abundance))
  
  full$Abundance
}

make_vector_dist <- function(v) {
  if (all(!is.finite(v)) || stats::sd(v, na.rm = TRUE) == 0) return(NULL)
  stats::dist(scale(v))
}

genus_mat <- gen_long %>%
  dplyr::filter(.data[[micro_id_col]] %in% common_ids) %>%
  dplyr::group_by(.data[[micro_id_col]], Genus) %>%
  dplyr::summarise(Values = mean(Values, na.rm = TRUE), .groups = "drop") %>%
  dplyr::rename(ID = dplyr::all_of(micro_id_col)) %>%
  tidyr::pivot_wider(
    names_from = Genus,
    values_from = Values,
    values_fill = 0
  ) %>%
  dplyr::arrange(match(ID, common_ids))

genus_community <- genus_mat %>%
  dplyr::select(-ID) %>%
  as.data.frame()

rownames(genus_community) <- genus_mat$ID

bio_dist_list <- list()
bio_dist_list[["Community"]] <- vegan::vegdist(genus_community, method = "bray")

phy_available <- unique(phy_long$Phylum)
gen_available <- unique(gen_long$Genus)

tax_pseudo <- find_taxon(phy_available, "Pseudomonadota", alternatives = c("Proteobacteria"))
tax_bact   <- find_taxon(phy_available, "Bacteroidota", alternatives = c("Bacteroidetes", "Bacteriodota"))
tax_blast  <- find_taxon(gen_available, "Blastomonas")

if (!is.na(tax_blast)) {
  v <- make_abundance_vector(gen_long, "Genus", tax_blast, common_ids, micro_id_col)
  bio_dist_list[["Blastomonas"]] <- make_vector_dist(v)
}

if (!is.na(tax_pseudo)) {
  v <- make_abundance_vector(phy_long, "Phylum", tax_pseudo, common_ids, micro_id_col)
  bio_dist_list[["Pseudomonadota"]] <- make_vector_dist(v)
}

if (!is.na(tax_bact)) {
  v <- make_abundance_vector(phy_long, "Phylum", tax_bact, common_ids, micro_id_col)
  bio_dist_list[["Bacteroidota"]] <- make_vector_dist(v)
}

bio_dist_list <- bio_dist_list[!vapply(bio_dist_list, is.null, logical(1))]

################################################################################
# 8. MICROBIAL–SOIL DISTANCE ASSOCIATIONS
################################################################################

run_mantel_one <- function(bio_name, bio_dist, var_name, x) {
  # Microbial data are treatment-level only (CK, GP, PEG; n = 3 treatments).
  # A permutation Mantel significance test is not appropriate here.
  # Keep only a descriptive Spearman association between the three pairwise
  # microbial distances and the corresponding soil-variable distances.
  if (stats::sd(x, na.rm = TRUE) == 0) {
    return(tibble::tibble(
      BioGroup = bio_name,
      SoilVar = var_name,
      mantel_r = NA_real_,
      mantel_p = NA_real_
    ))
  }
  
  env_dist <- stats::dist(scale(x))
  
  descriptive_r <- suppressWarnings(
    stats::cor(
      as.vector(bio_dist),
      as.vector(env_dist),
      method = "spearman",
      use = "complete.obs"
    )
  )
  
  tibble::tibble(
    BioGroup = bio_name,
    SoilVar = var_name,
    mantel_r = as.numeric(descriptive_r),
    mantel_p = NA_real_
  )
}

mantel_results <- dplyr::bind_rows(lapply(names(bio_dist_list), function(bg) {
  dplyr::bind_rows(lapply(colnames(soil_mat_bio), function(vv) {
    run_mantel_one(bg, bio_dist_list[[bg]], vv, soil_mat_bio[[vv]])
  }))
})) %>%
  dplyr::mutate(
    mantel_p_class = dplyr::case_when(
      is.na(mantel_p) ~ NA_character_,
      mantel_p < 0.01 ~ "< 0.01",
      mantel_p < 0.05 ~ "0.01–0.05",
      TRUE ~ NA_character_
    ),
    mantel_r_abs = abs(mantel_r),
    mantel_r_class = dplyr::case_when(
      is.na(mantel_r_abs) ~ NA_character_,
      mantel_r_abs < 0.70 ~ "0.40–0.70",
      TRUE ~ "> 0.70"
    )
  )

################################################################################
# 9. FIGURE S17 COORDINATES
################################################################################

nvar <- length(colnames(soil_mat))

var_pos <- tibble::tibble(
  SoilVar = colnames(soil_mat),
  var_id  = seq_along(colnames(soil_mat)),
  xend    = var_id,
  yend    = nvar - var_id + 1
)

bio_levels <- c(
  "Community",
  "Blastomonas",
  "Pseudomonadota",
  "Bacteroidota"
)

bio_levels <- bio_levels[bio_levels %in% names(bio_dist_list)]

bio_label_x <- -3.12
bio_point_x <- -0.62

bio_nodes <- tibble::tibble(
  BioGroup = bio_levels,
  label_x  = bio_label_x,
  point_x  = bio_point_x,
  y        = seq(nvar * 0.84, nvar * 0.30, length.out = length(bio_levels))
)

# Block-specific biological interpretation.
# This stops the figure becoming spaghetti.
bio_block_map <- list(
  "Community"      = c("RC", "HCO\u2083\u207b", "MWD", "Biomass"),
  "Blastomonas"    = c("CF", "MCF", "SAP", "MAOP", "MAOC"),
  "Pseudomonadota" = c("RC", "MWD", "Ksat"),
  "Bacteroidota"   = c("ESP", "EC", "HCO\u2083\u207b")
)

is_allowed_mantel_link <- function(bg, sv) {
  if (!bg %in% names(bio_block_map)) return(FALSE)
  sv %in% bio_block_map[[bg]]
}

if (descriptive_micro_links) {
  mantel_plot_links <- mantel_results %>%
    dplyr::filter(!is.na(mantel_r)) %>%
    dplyr::filter(abs(mantel_r) >= MANTEL_R_MIN) %>%
    dplyr::mutate(
      allowed_block = mapply(is_allowed_mantel_link, BioGroup, SoilVar)
    ) %>%
    dplyr::filter(allowed_block) %>%
    dplyr::group_by(BioGroup) %>%
    dplyr::arrange(dplyr::desc(abs(mantel_r)), .by_group = TRUE) %>%
    dplyr::slice_head(n = MAX_LINKS_PER_BIO) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(BioGroup, dplyr::desc(abs(mantel_r))) %>%
    dplyr::slice_head(n = MAX_MANTEL_LINKS) %>%
    dplyr::left_join(var_pos, by = "SoilVar") %>%
    dplyr::left_join(bio_nodes, by = "BioGroup") %>%
    dplyr::filter(!is.na(point_x), !is.na(y), !is.na(xend), !is.na(yend)) %>%
    dplyr::mutate(
      x_start = point_x + 0.18,
      y_start = y,
      x_end   = xend - 0.12,
      y_end   = yend,
      mantel_p_class = "Treatment-level",
      mantel_r_class = dplyr::case_when(
        abs(mantel_r) < 0.70 ~ "0.40–0.70",
        TRUE ~ "> 0.70"
      )
    )
  
  link_colour_values <- c("Treatment-level" = "#1B9E77")
  link_colour_limits <- c("Treatment-level")
  link_legend_title <- "Microbial link"
} else {
  mantel_plot_links <- mantel_results %>%
    dplyr::filter(!is.na(mantel_r), !is.na(mantel_p)) %>%
    dplyr::filter(mantel_p < MANTEL_P_MAX, abs(mantel_r) >= MANTEL_R_MIN) %>%
    dplyr::mutate(
      allowed_block = mapply(is_allowed_mantel_link, BioGroup, SoilVar)
    ) %>%
    dplyr::filter(allowed_block) %>%
    dplyr::group_by(BioGroup) %>%
    dplyr::arrange(mantel_p, dplyr::desc(abs(mantel_r)), .by_group = TRUE) %>%
    dplyr::slice_head(n = MAX_LINKS_PER_BIO) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(BioGroup, mantel_p, dplyr::desc(abs(mantel_r))) %>%
    dplyr::slice_head(n = MAX_MANTEL_LINKS) %>%
    dplyr::left_join(var_pos, by = "SoilVar") %>%
    dplyr::left_join(bio_nodes, by = "BioGroup") %>%
    dplyr::filter(!is.na(point_x), !is.na(y), !is.na(xend), !is.na(yend)) %>%
    dplyr::mutate(
      x_start = point_x + 0.18,
      y_start = y,
      x_end   = xend - 0.12,
      y_end   = yend,
      mantel_p_class = dplyr::case_when(
        mantel_p < 0.01 ~ "< 0.01",
        mantel_p < 0.05 ~ "0.01–0.05",
        TRUE ~ NA_character_
      ),
      mantel_r_class = dplyr::case_when(
        abs(mantel_r) < 0.70 ~ "0.40–0.70",
        TRUE ~ "> 0.70"
      )
    )
  
  link_colour_values <- c(
    "< 0.01"     = "#F57C00",
    "0.01–0.05" = "#1B9E77"
  )
  link_colour_limits <- c("< 0.01", "0.01–0.05")
  link_legend_title <- "p value"
}

group_ranges <- group_def %>%
  dplyr::filter(Label %in% colnames(soil_mat)) %>%
  dplyr::mutate(var_id = match(Label, colnames(soil_mat))) %>%
  dplyr::group_by(Group) %>%
  dplyr::summarise(
    xmin = min(var_id) - 0.48,
    xmax = max(var_id) + 0.48,
    xmid = mean(range(var_id)),    .groups = "drop"
  ) %>%
  dplyr::mutate(
    Group_short = dplyr::recode(
      Group,
      "Ionic stress"  = "Ionic",
      "Ca allocation" = "Ca alloc.",
      "P pools"       = "P",
      "C pools"       = "C",
      "Soil physics"  = "Structure",
      "Exchange"      = "CEC",
      "Plant"         = "Plant"
    ),
    x_lab = dplyr::case_when(
      Group == "Exchange" ~ xmid - 0.18,
      Group == "Plant"    ~ xmid + 0.22,
      TRUE                ~ xmid
    ),
    lab_size = dplyr::case_when(
      Group %in% c("Exchange", "Plant") ~ 2.45,
      TRUE                              ~ 2.85
    ),
    col = dplyr::case_when(
      Group == "Ionic stress"  ~ "#D95F02",
      Group == "Ca allocation" ~ "#1B9E77",
      Group == "P pools"       ~ "#7570B3",
      Group == "C pools"       ~ "#E7298A",
      Group == "Soil physics"  ~ "#66A61E",
      Group == "Exchange"      ~ "#E6AB02",
      Group == "Plant"         ~ "#A6761D",
      TRUE ~ "grey50"
    )
  )

################################################################################
# 10. MINI-BARS
################################################################################

get_treatment_taxon <- function(df, tax_col, tax_name, trt) {
  if (is.na(tax_name)) return(NA_real_)
  
  z <- df %>%
    dplyr::filter(
      TreatmentID == trt,
      .data[[tax_col]] == tax_name
    ) %>%
    dplyr::summarise(v = mean(Values, na.rm = TRUE))
  
  if (nrow(z) == 0 || !is.finite(z$v[1])) return(NA_real_)
  z$v[1]
}

# Named taxa are now read directly from the actual Phylum/Genus sheets.
# Community display values are retained from the existing figure design.
bio_spark_raw <- tibble::tibble(
  BioGroup = c(
    "Community",
    "Blastomonas",
    "Pseudomonadota",
    "Bacteroidota"
  ),
  CK = c(
    0.50,
    get_treatment_taxon(gen_long, "Genus", tax_blast, "CK"),
    get_treatment_taxon(phy_long, "Phylum", tax_pseudo, "CK"),
    get_treatment_taxon(phy_long, "Phylum", tax_bact, "CK")
  ),
  GP = c(
    0.55,
    get_treatment_taxon(gen_long, "Genus", tax_blast, "GP"),
    get_treatment_taxon(phy_long, "Phylum", tax_pseudo, "GP"),
    get_treatment_taxon(phy_long, "Phylum", tax_bact, "GP")
  ),
  PEG = c(
    0.30,
    get_treatment_taxon(gen_long, "Genus", tax_blast, "PEG"),
    get_treatment_taxon(phy_long, "Phylum", tax_pseudo, "PEG"),
    get_treatment_taxon(phy_long, "Phylum", tax_bact, "PEG")
  )
)

bio_spark <- bio_spark_raw %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    mx = max(c(CK, GP, PEG), na.rm = TRUE),
    CK  = CK  / mx,
    GP  = GP  / mx,
    PEG = PEG / mx
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-mx) %>%
  tidyr::pivot_longer(
    cols      = c(CK, GP, PEG),
    names_to  = "Treatment",
    values_to = "val"
  ) %>%
  dplyr::left_join(bio_nodes, by = "BioGroup") %>%
  dplyr::mutate(
    Treatment = factor(Treatment, levels = c("CK", "GP", "PEG")),
    bar_y = y + dplyr::case_when(
      Treatment == "CK"  ~  0.50,
      Treatment == "GP"  ~  0.00,
      TRUE               ~ -0.50
    ),
    bar_xmax = -3.68,
    bar_xmin = bar_xmax - val * 1.35,
    bar_col = dplyr::case_when(
      Treatment == "CK"  ~ "#333333",
      Treatment == "GP"  ~ "#2AA7A1",
      TRUE               ~ "#A8DDD9"
    )
  )

spark_legend_df <- bio_spark %>%
  dplyr::filter(BioGroup == "Community") %>%
  dplyr::mutate(
    lab_x = bar_xmax - 0.05,
    lab_y = bar_y,
    lab   = as.character(Treatment)
  )

################################################################################
# 11. FIGURE S17 MAIN PLOT
################################################################################

pE_main <- ggplot2::ggplot() +
  ggplot2::geom_rect(
    data = bio_nodes,
    ggplot2::aes(
      xmin = -5.25,
      xmax = -3.50,
      ymin = y - 1.00,
      ymax = y + 1.00
    ),
    fill = scales::alpha("grey92", 0.52),
    colour = NA,
    inherit.aes = FALSE
  ) +
  ggplot2::annotate(
    "text",
    x = -5.23,
    y = nvar + 1.16,
    label = "Rel. abundance",
    hjust = 0,
    vjust = 0.5,
    size = 2.45,
    fontface = "bold",
    family = base_font,
    colour = "black"
  ) +
  ggplot2::geom_segment(
    data = bio_spark,
    ggplot2::aes(
      x = bar_xmin,
      xend = bar_xmax,
      y = bar_y,
      yend = bar_y
    ),
    colour = bio_spark$bar_col,
    linewidth = 4.10,
    lineend = "butt",
    inherit.aes = FALSE
  ) +
  ggplot2::geom_text(
    data = spark_legend_df,
    ggplot2::aes(
      x = lab_x,
      y = lab_y,
      label = lab
    ),
    hjust = 1,
    vjust = 0.5,
    size = 2.30,
    fontface = "bold",
    family = base_font,
    colour = "white",
    inherit.aes = FALSE
  ) +
  ggplot2::geom_segment(
    data = mantel_plot_links,
    ggplot2::aes(
      x = x_start,
      y = y_start,
      xend = x_end,
      yend = y_end,
      colour = mantel_p_class,
      linewidth = mantel_r_class
    ),
    alpha = 0.72,
    lineend = "round"
  ) +
  ggplot2::geom_text(
    data = bio_nodes,
    ggplot2::aes(
      x = label_x,
      y = y - 0.30,
      label = BioGroup
    ),
    hjust = 0,
    vjust = 0.5,
    size = 3.05,
    fontface = "bold",
    family = base_font,
    colour = "black"
  ) +
  ggplot2::geom_point(
    data = bio_nodes,
    ggplot2::aes(
      x = point_x,
      y = y
    ),
    size = 2.55,
    colour = "#B0005A"
  ) +
  ggplot2::geom_tile(
    data = corr_long,
    ggplot2::aes(
      x = x_id,
      y = y_id,
      fill = r
    ),
    width = 0.98,
    height = 0.98,
    colour = "grey84",
    linewidth = 0.20
  ) +
  # Significance symbols: make stars larger for visibility, keep dots unchanged.
  ggplot2::geom_text(
    data = corr_long %>%
      dplyr::filter(stars == "*", var1 != var2),
    ggplot2::aes(
      x = x_id,
      y = y_id,
      label = stars
    ),
    size = 3.75,
    fontface = "bold",
    colour = "black",
    family = base_font
  ) +
  ggplot2::geom_text(
    data = corr_long %>%
      dplyr::filter(stars == "•", var1 != var2),
    ggplot2::aes(
      x = x_id,
      y = y_id,
      label = stars
    ),
    size = 2.90,
    fontface = "bold",
    colour = "black",
    family = base_font
  ) +
  ggplot2::geom_segment(
    data = group_ranges,
    ggplot2::aes(
      x = xmin,
      xend = xmax,
      y = nvar + 0.70,
      yend = nvar + 0.70
    ),
    inherit.aes = FALSE,
    colour = group_ranges$col,
    linewidth = 3.90,
    lineend = "butt"
  ) +
  ggplot2::geom_text(
    data = group_ranges,
    ggplot2::aes(
      x = x_lab,
      y = nvar + 1.18,
      label = Group_short,
      size = lab_size
    ),
    inherit.aes = FALSE,
    fontface = "bold",
    family = base_font,
    colour = "black",
    show.legend = FALSE
  ) +
  ggplot2::scale_size_identity() +
  ggplot2::scale_fill_gradient2(
    name = "Spearman's \u03c1",
    low = "#2C7FB8",
    mid = "white",
    high = "#EF3B2C",
    midpoint = 0,
    limits = c(-1, 1),
    oob = scales::squish
  ) +
  ggplot2::scale_colour_manual(
    name   = link_legend_title,
    values = link_colour_values,
    limits = link_colour_limits,
    drop   = FALSE
  ) +
  ggplot2::scale_linewidth_manual(
    name = "Correlation",
    values = c(
      "0.40–0.70" = 0.85,
      "> 0.70"    = 1.45
    ),
    limits = c("0.40–0.70", "> 0.70"),
    drop = FALSE
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq_len(nvar),
    labels = colnames(soil_mat),
    position = "top",
    expand = c(0, 0)
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq_len(nvar),
    labels = rev(colnames(soil_mat)),
    position = "right",
    expand = c(0, 0)
  ) +
  ggplot2::coord_cartesian(
    xlim = c(-5.55, nvar + 0.62),
    ylim = c(0.38, nvar + 1.52),
    clip = "off"
  ) +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::theme_minimal(base_size = 11, base_family = base_font) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      angle = 90,
      hjust = 0,
      vjust = 0.58,
      size = 8.10,
      face = "bold",
      colour = "black",
      margin = ggplot2::margin(b = 20)
    ),
    axis.text.y = ggplot2::element_text(
      size = 8.55,
      face = "bold",
      colour = "black"
    ),
    axis.text.y.right = ggplot2::element_text(
      hjust = 0,
      margin = ggplot2::margin(l = 2, r = 0)
    ),
    axis.ticks = ggplot2::element_blank(),
    legend.position = "none",
    plot.margin = ggplot2::margin(8, 8, 1, 8),
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    plot.background = ggplot2::element_rect(fill = "white", colour = NA)
  )

################################################################################
# 12. FIGURE S17 BOTTOM LEGEND
################################################################################

leg_grad <- tibble::tibble(
  x = seq(5, 23, length.out = 160),
  y = 0.47,
  z = seq(-1, 1, length.out = 160)
)

if (descriptive_micro_links) {
  leg_p_bottom <- tibble::tibble(
    x = 34,
    xend = 38,
    y = 0.45,
    yend = 0.45,
    lab = factor("Treatment-level", levels = link_colour_limits)
  )
} else {
  leg_p_bottom <- tibble::tibble(
    x = c(34, 49),
    xend = c(38, 53),
    y = c(0.45, 0.45),
    yend = c(0.45, 0.45),
    lab = factor(
      c("< 0.01", "0.01–0.05"),
      levels = link_colour_limits
    )
  )
}

leg_r_bottom <- tibble::tibble(
  x = c(73, 87),
  xend = c(77, 91),
  y = c(0.45, 0.45),
  yend = c(0.45, 0.45),
  lab = factor(
    c("0.40–0.70", "> 0.70"),
    levels = c("0.40–0.70", "> 0.70")
  )
)

pE_bottom_legend <- ggplot2::ggplot() +
  ggplot2::geom_tile(
    data = leg_grad,
    ggplot2::aes(x = x, y = y, fill = z),
    width = 0.16,
    height = 0.16,
    inherit.aes = FALSE
  ) +
  ggplot2::annotate(
    "text",
    x = 5,
    y = 0.82,
    label = "Spearman's \u03c1",
    hjust = 0,
    size = 3.0,
    fontface = "bold",
    family = base_font
  ) +
  ggplot2::annotate(
    "text",
    x = c(5, 14, 23),
    y = 0.18,
    label = c("-1", "0", "1"),
    size = 2.45,
    fontface = "bold",
    family = base_font
  ) +
  ggplot2::annotate(
    "text",
    x = 34,
    y = 0.82,
    label = link_legend_title,
    hjust = 0,
    size = 3.0,
    fontface = "bold",
    family = base_font
  ) +
  ggplot2::geom_segment(
    data = leg_p_bottom,
    ggplot2::aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend,
      colour = lab
    ),
    linewidth = 1.1,
    lineend = "round",
    inherit.aes = FALSE
  ) +
  ggplot2::geom_text(
    data = leg_p_bottom,
    ggplot2::aes(
      x = xend + 0.8,
      y = y,
      label = lab
    ),
    hjust = 0,
    vjust = 0.5,
    size = 2.45,
    fontface = "bold",
    family = base_font,
    inherit.aes = FALSE
  ) +
  ggplot2::annotate(
    "text",
    x = 73,
    y = 0.82,
    label = "Correlation",
    hjust = 0,
    size = 3.0,
    fontface = "bold",
    family = base_font
  ) +
  ggplot2::geom_segment(
    data = leg_r_bottom,
    ggplot2::aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend,
      linewidth = lab
    ),
    colour = "grey25",
    lineend = "round",
    inherit.aes = FALSE
  ) +
  ggplot2::geom_text(
    data = leg_r_bottom,
    ggplot2::aes(
      x = xend + 0.6,
      y = y,
      label = lab
    ),
    hjust = 0,
    vjust = 0.5,
    size = 2.45,
    fontface = "bold",
    family = base_font,
    inherit.aes = FALSE
  ) +
  ggplot2::scale_fill_gradient2(
    low = "#2C7FB8",
    mid = "white",
    high = "#EF3B2C",
    midpoint = 0,
    limits = c(-1, 1),
    guide = "none"
  ) +
  ggplot2::scale_colour_manual(
    values = link_colour_values,
    guide = "none"
  ) +
  ggplot2::scale_linewidth_manual(
    values = c(
      "0.40–0.70" = 0.85,
      "> 0.70"    = 1.45
    ),
    guide = "none"
  ) +
  ggplot2::coord_cartesian(
    xlim = c(0, 104),
    ylim = c(0, 1),
    clip = "off"
  ) +
  ggplot2::theme_void(base_family = base_font) +
  ggplot2::theme(
    plot.margin = ggplot2::margin(0, 8, 2, 8),
    plot.background = ggplot2::element_rect(fill = "white", colour = NA)
  )

pE_base <- cowplot::plot_grid(
  pE_main,
  pE_bottom_legend,
  ncol = 1,
  align = "v",
  rel_heights = c(0.90, 0.10)
)

pE <- cowplot::ggdraw(pE_base) +
  cowplot::draw_label(
    "(a)",
    x = 0.010,
    y = 0.988,
    hjust = 0,
    vjust = 1,
    size = 10,
    fontface = "bold",
    colour = "black"
  )

################################################################################


################################################################################
# 13. SAVE SUPPLEMENTARY FIG. S17
################################################################################

# pE is the complete Supplementary Fig. S17 object: correlation heatmap + microbial links +
# relative-abundance mini-bars + bottom legend + panel tag (a).
print(pE)

FIG_C_WIDTH_IN  <- 15.5 / 2.54    # 15.5 cm (portrait Word insertion width)
FIG_C_HEIGHT_IN <- 15.0 / 2.54    # 15.0 cm (fits portrait A4 Word page)

ggplot2::ggsave(
  filename = file.path(out_dir, paste0("Fig_S17_NC_Portrait_Correlation_Microbial_", stamp, ".png")),
  plot = pE,
  width = FIG_C_WIDTH_IN,
  height = FIG_C_HEIGHT_IN,
  units = "in",
  dpi = 600,
  bg = "white",
  limitsize = FALSE
)

ggplot2::ggsave(
  filename = file.path(out_dir, paste0("Fig_S17_NC_Portrait_Correlation_Microbial_", stamp, ".tiff")),
  plot = pE,
  width = FIG_C_WIDTH_IN,
  height = FIG_C_HEIGHT_IN,
  units = "in",
  dpi = 600,
  bg = "white",
  device = "tiff",
  compression = "lzw",
  limitsize = FALSE
)

# Prefer Cairo for fully vector text/shapes. On R builds without Cairo,
# use the built-in Helvetica PDF font instead of a system CID font.
if (capabilities("cairo")) {
  ggplot2::ggsave(
    filename = file.path(out_dir, paste0("Fig_S17_NC_Portrait_Correlation_Microbial_", stamp, ".pdf")),
    plot = pE,
    width = FIG_C_WIDTH_IN,
    height = FIG_C_HEIGHT_IN,
    units = "in",
    bg = "white",
    device = grDevices::cairo_pdf,
    limitsize = FALSE
  )
} else {
  ggplot2::ggsave(
    filename = file.path(out_dir, paste0("Fig_S17_NC_Portrait_Correlation_Microbial_", stamp, ".pdf")),
    plot = pE,
    width = FIG_C_WIDTH_IN,
    height = FIG_C_HEIGHT_IN,
    units = "in",
    bg = "white",
    device = grDevices::pdf,
    family = "Helvetica",
    useDingbats = FALSE,
    limitsize = FALSE
  )
}

################################################################################
# 14. SAVE FIGURE C STATISTICS ONLY
################################################################################

stats_file <- file.path(
  out_dir,
  paste0("Fig_S17_Correlation_Microbial_statistics_", stamp, ".xlsx")
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Spearman_correlations")
openxlsx::writeData(wb, "Spearman_correlations", corr_long)

openxlsx::addWorksheet(wb, "Correlation_tests")
openxlsx::writeData(wb, "Correlation_tests", mantel_results)

openxlsx::addWorksheet(wb, "Correlation_links_plotted")
openxlsx::writeData(wb, "Correlation_links_plotted", mantel_plot_links)

openxlsx::addWorksheet(wb, "Variables_used")
openxlsx::writeData(
  wb,
  "Variables_used",
  tibble::tibble(
    Figure_label = names(soil_vars),
    File_variable = unname(soil_vars)
  )
)

openxlsx::addWorksheet(wb, "Microbial_groups_used")
openxlsx::writeData(
  wb,
  "Microbial_groups_used",
  tibble::tibble(Group = names(bio_dist_list))
)

openxlsx::addWorksheet(wb, "Microbial_matching_mode")
openxlsx::writeData(
  wb,
  "Microbial_matching_mode",
  tibble::tibble(
    microbial_level = microbial_level,
    id_column_used = micro_id_col,
    common_ids_used = paste(common_ids, collapse = ", "),
    descriptive_links = descriptive_micro_links,
    note = dplyr::if_else(
      descriptive_micro_links,
      "Phylum/Genus were handled at treatment level because microbial replication was insufficient; link p-values should not be interpreted.",
      "Microbial data had sufficient sample-level overlap for sample-level Mantel tests."
    )
  )
)

openxlsx::addWorksheet(wb, "Bio_sparklines")
openxlsx::writeData(wb, "Bio_sparklines", bio_spark_raw)

openxlsx::saveWorkbook(wb, stats_file, overwrite = TRUE)

message(
  "\nDONE — SUPPLEMENTARY FIG. S17.",
  "\nFigure saved in: ", out_dir,
  "\nStatistics saved as: ", stats_file,
  "\nNo Panel A or Panel B code is included in this script."
)