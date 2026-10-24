##### 14/10/26
# Inputs in R working directory:
#   Conventional_Treatments.xlsx (sheets: data, Phylum, Genus_repli)
#   EEM_plot_ready.csv
################################################################################

rm(list = ls())
graphics.off()

packages <- c("readxl","readr","dplyr","tidyr","stringr","tibble",
              "ggplot2","patchwork","scales","forcats","viridis",
              "grid","janitor")
for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
invisible(lapply(packages, library, character.only = TRUE))

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

file_micro <- repo_input_path("Conventional_Treatments.xlsx")
file_eem <- repo_input_path("EEM_plot_ready.csv")
out_dir <- "Fig5_NC_FINAL_CLEAN"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
stopifnot(file.exists(file_micro), file.exists(file_eem))

trt <- c("CK", "GP", "PEG")
trt_cols <- c(CK="#000000", GP="#148B8E", PEG="#65C3CD")
font <- "sans"                           # one consistent cross-platform family
TAG_PT <- 11.2

pick_col <- function(d, candidates, sheet) {
  match_i <- match(janitor::make_clean_names(candidates),
                   janitor::make_clean_names(names(d)))
  match_i <- match_i[!is.na(match_i)]
  if (!length(match_i)) stop("Column not found in ", sheet, ": ",
                             paste(candidates, collapse=", "), ". Available: ",
                             paste(names(d), collapse=", "))
  names(d)[match_i[1]]
}

clean_treatment <- function(x) toupper(trimws(as.character(x)))

text_theme <- function(base_size=9) {
  # Apply this theme ONLY to ordinary panels (a)-(d).
  # NEVER apply an axis/text theme to fig_all: that would restore unwanted
  # axes on theme_void() legend bars, tags and the SEM network.
  theme_classic(base_size=base_size, base_family=font) +
    theme(
      text=element_text(family=font,colour="#000000"),
      plot.background=element_rect(fill="white",colour=NA),
      panel.background=element_rect(fill="white",colour=NA),
      axis.title=element_text(size=9.4,face="plain",colour="#000000"),
      axis.text=element_text(size=8.3,face="plain",colour="#000000"),
      axis.ticks=element_line(colour="#000000",linewidth=0.32),
      axis.line=element_line(colour="#000000",linewidth=0.38),
      legend.text=element_text(size=7.6,colour="#000000"),
      plot.margin=margin(2,2,2,3)
    )
}

# Fixed-height tag row: 0.52 cm regardless of panel height.
# The previous relative 0.28:3.72 ratio produced a tag strip narrower
# than its own text, clipping (a)-(e) in the portrait composite.
# Keep all panel tags IN their own white strips above their plots.
tag_strip <- function(tag) {
  ggplot() +
    annotate("text", x=0.022, y=0.50, label=tag,
             fontface="bold", size=3.55, colour="#000000",
             hjust=0, vjust=0.5, family=font) +
    coord_cartesian(xlim=c(0,1), ylim=c(0,1), expand=FALSE, clip="on") +
    theme_void() +
    theme(plot.margin=margin(0,0,0,0),
          plot.background=element_rect(fill="white", colour=NA))
}
tagged <- function(p, tag) {
  (tag_strip(tag) / p) +
    patchwork::plot_layout(heights=grid::unit(c(0.52, 1), c("cm", "null")))
}

# =============== LOAD MAIN TREATMENT DATA ===============
data_raw <- readxl::read_excel(file_micro, sheet="data")
names(data_raw) <- trimws(names(data_raw))
cl_t <- pick_col(data_raw, c("Treatments","Treatment"), "data")
cl_r <- pick_col(data_raw, c("response.varibale","response.variable",
                             "response_varibale","response_variable"), "data")
cl_v <- pick_col(data_raw, c("Value","Values"), "data")
main <- data_raw %>%
  dplyr::transmute(Treatment=clean_treatment(.data[[cl_t]]),
                   variable=trimws(as.character(.data[[cl_r]])),
                   value=suppressWarnings(as.numeric(.data[[cl_v]]))) %>%
  dplyr::filter(Treatment %in% trt, is.finite(value)) %>%
  dplyr::mutate(Treatment=factor(Treatment,levels=trt))

# =============== (a) SHANNON DOT PLOT ===============
# One treatment-level composite value per treatment; no biological replicates.
# Therefore, show the actual values as points only, without error bars or tests.
alpha <- main %>%
  filter(variable == "Shannon") %>%
  group_by(Treatment) %>%
  summarise(value=mean(value), .groups="drop")
stopifnot(nrow(alpha)==3, setequal(as.character(alpha$Treatment), trt))

pA <- ggplot(alpha,aes(x=Treatment,y=value,fill=Treatment)) +
  geom_point(shape=21,size=3.25,stroke=0.55,colour="#000000") +
  geom_text(aes(label=sprintf("%.2f",value)),
            vjust=-0.95,size=2.65,colour="#000000",family=font) +
  scale_fill_manual(values=trt_cols,guide="none") +
  scale_y_continuous(expand=expansion(mult=c(0.10,0.27))) +
  labs(x=NULL,y="Shannon index") + text_theme(8.5) +
  theme(axis.text.x=element_text(size=8.6),
        axis.title.y=element_text(size=9.2),
        plot.margin=margin(2,3,2,4))

# =============== (b) BRAY-CURTIS PCoA ===============
pcoa <- main %>% filter(variable %in% c("PCo1","PCo2")) %>%
  group_by(Treatment,variable) %>% summarise(value=mean(value),.groups="drop") %>%
  pivot_wider(names_from=variable,values_from=value) %>%
  filter(is.finite(PCo1),is.finite(PCo2))
stopifnot(nrow(pcoa)==3)
xpad <- max(diff(range(pcoa$PCo1))*0.14,0.025)
ypad <- max(diff(range(pcoa$PCo2))*0.15,0.014)

# Place point names inside panel: avoid a wasted legend row.
# Placement is chosen for the actual three-treatment orientation.
pcoa_labels <- pcoa %>%
  mutate(
    label_x = PCo1 + case_when(
      Treatment == "CK"  ~ -0.022,
      Treatment == "GP"  ~  0.021,
      Treatment == "PEG" ~  0.021,
      TRUE ~ 0
    ),
    label_y = PCo2 + case_when(
      Treatment == "CK"  ~ -0.013,
      Treatment == "GP"  ~  0.013,
      Treatment == "PEG" ~ -0.013,
      TRUE ~ 0
    ),
    label_hjust = if_else(Treatment == "CK", 1, 0)
  )

pB <- ggplot(pcoa, aes(PCo1, PCo2, shape=Treatment, fill=Treatment)) +
  geom_point(size=3.15, colour="black", stroke=0.55) +
  geom_text(data=pcoa_labels,
            aes(x=label_x, y=label_y, label=Treatment, hjust=label_hjust),
            inherit.aes=FALSE, size=2.8, family=font,
            colour="#000000", fontface="plain", show.legend=FALSE) +
  scale_fill_manual(values=trt_cols, guide="none") +
  scale_shape_manual(values=c(CK=21,GP=23,PEG=24), guide="none") +
  coord_cartesian(xlim=range(pcoa$PCo1)+c(-xpad-0.015,xpad+0.015),
                  ylim=range(pcoa$PCo2)+c(-ypad-0.010,ypad+0.010)) +
  annotate("text", x=Inf, y=Inf, label="Bray–Curtis",
           hjust=1.12, vjust=1.45, size=2.62, family=font) +
  labs(x="PCo1 (35.45%)", y="PCo2 (22.47%)") +
  text_theme(8.8) +
  theme(panel.border=element_rect(colour="#000000",fill=NA,linewidth=0.35),
        legend.position="none", axis.text=element_text(size=8.5),
        axis.title=element_text(size=9.2),
        plot.margin=margin(2,3,2,4))

# =============== (c) PHYLA & COMPACT LEGEND ===============
phy_raw <- readxl::read_excel(file_micro,sheet="Phylum")
names(phy_raw) <- trimws(names(phy_raw))
cp_t <- pick_col(phy_raw,c("Treatment","Treatments"),"Phylum")
cp_n <- pick_col(phy_raw,c("Phylum"),"Phylum")
cp_v <- pick_col(phy_raw,c("Value","Values"),"Phylum")
# IMPORTANT FIX: Some source workbooks already contain an "Other" phylum.
# Merge spelling variants "Other"/"Others" BEFORE ranking and add one
# Other legend level exactly once. No measured abundances are discarded.
phy <- phy_raw %>%
  transmute(
    Treatment=clean_treatment(.data[[cp_t]]),
    Phylum=stringr::str_squish(as.character(.data[[cp_n]])),
    Value=suppressWarnings(as.numeric(.data[[cp_v]]))
  ) %>%
  filter(Treatment %in% trt, !is.na(Phylum), nzchar(Phylum), is.finite(Value)) %>%
  mutate(Phylum=if_else(
    stringr::str_to_lower(Phylum) %in% c("other", "others"),
    "Other", Phylum
  )) %>%
  group_by(Treatment,Phylum) %>%
  summarise(Value=mean(Value), .groups="drop")

# Select the 15 leading NAMED phyla; the pre-existing "Other" is not a phylum.
# Values outside this ranking are combined into the single Other category.
top_phy <- phy %>%
  filter(Phylum != "Other") %>%
  group_by(Phylum) %>%
  summarise(total=sum(Value), .groups="drop") %>%
  arrange(desc(total), Phylum) %>%
  slice_head(n=15) %>%
  pull(Phylum) %>%
  unique()
if (!length(top_phy)) stop("Phylum sheet has no named phyla after cleaning.")
phy_levels <- rev(c(top_phy, "Other"))
stopifnot(!anyDuplicated(phy_levels))

phy_plot <- phy %>%
  mutate(Phylum=if_else(Phylum %in% top_phy, Phylum, "Other")) %>%
  group_by(Treatment,Phylum) %>%
  summarise(Value=sum(Value), .groups="drop") %>%
  group_by(Treatment) %>%
  mutate(Percent=100*Value/sum(Value)) %>%
  ungroup() %>%
  mutate(Treatment=factor(Treatment,levels=trt),
         Phylum=factor(Phylum,levels=phy_levels))

message("Fig. 5(c): ", length(top_phy), " named phyla + one Other category.")
print(phy_plot %>% group_by(Treatment) %>%
        summarise(Total_percent=sum(Percent), .groups="drop"))
phy_palette <- c("#4E79A7","#F28E2B","#E15759","#76B7B2","#59A14F",
                 "#EDC948","#B07AA1","#9C755F","#BAB0AC","#86BCB6",
                 "#FF9DA7","#9C9EDE","#8CD17D","#F1CE63","#D37295")
phy_cols <- c(stats::setNames(phy_palette[seq_along(top_phy)],top_phy),Other="#D9D9D9")

pC <- ggplot(phy_plot,aes(Treatment,Percent,fill=Phylum)) +
  geom_col(width=0.65,colour="white",linewidth=0.17) +
  scale_fill_manual(values=phy_cols,guide="none") +
  scale_y_continuous(breaks=c(0,25,50,75,100),expand=c(0,0)) +
  coord_cartesian(ylim=c(0,100)) +
  labs(x=NULL,y="Relative abundance (%)") + text_theme(9.0) +
  theme(axis.text.x=element_text(size=9.2),
        axis.text.y=element_text(size=8.6),
        axis.title.y=element_text(size=9.8),
        plot.margin=margin(1,3,1,6))

# Manual two-column legend (16 entries at most), with explicit breathing room.
# Its own grid row prevents cropping by panel (e).
phy_key <- c(top_phy, "Other")
n_per_col <- ceiling(length(phy_key)/2)
phy_leg <- data.frame(name=phy_key, i=seq_along(phy_key)) %>%
  mutate(col=(i-1)%/%n_per_col,
         row=n_per_col-(i-1)%%n_per_col,
         x=0.015+col*0.505,
         fill=unname(phy_cols[name]))
pCkey <- ggplot(phy_leg) +
  geom_rect(aes(xmin=x,xmax=x+0.030,
                ymin=row-0.23,ymax=row+0.23),
            fill=phy_leg$fill, colour=NA) +
  geom_text(aes(x=x+0.040,y=row,label=name),
            hjust=0,vjust=0.5,size=2.55,
            family=font,colour="#000000") +
  coord_cartesian(xlim=c(0,1.07),
                  ylim=c(0.20,n_per_col+0.85),
                  clip="off",expand=FALSE) +
  theme_void() + theme(plot.margin=margin(3,3,4,2))

# =============== (d) GENUS HEATMAP & HORIZONTAL Z COLOUR BAR ===============
gen_raw <- readxl::read_excel(file_micro,sheet="Genus_repli")
names(gen_raw) <- trimws(names(gen_raw))
cg_t <- pick_col(gen_raw,c("Treatment","Treatments"),"Genus_repli")
cg_n <- pick_col(gen_raw,c("Genus"),"Genus_repli")
cg_v <- pick_col(gen_raw,c("Value","Values"),"Genus_repli")
gen <- gen_raw %>%
  transmute(Treatment=clean_treatment(.data[[cg_t]]),
            Genus=trimws(as.character(.data[[cg_n]])),
            Value=as.numeric(.data[[cg_v]])) %>%
  filter(Treatment %in% trt,!is.na(Genus),is.finite(Value)) %>%
  group_by(Treatment,Genus) %>% summarise(Value=mean(Value),.groups="drop")
top_gen <- gen %>% group_by(Genus) %>%
  summarise(total=sum(Value),.groups="drop") %>%
  arrange(desc(total)) %>% slice_head(n=20) %>% pull(Genus)
gen_plot <- gen %>% filter(Genus %in% top_gen) %>%
  group_by(Genus) %>%
  mutate(Z=if (sd(Value)==0 || is.na(sd(Value))) rep(0,n())
         else as.numeric(scale(Value))) %>%
  ungroup() %>%
  mutate(Treatment=factor(Treatment,levels=trt),
         Genus=forcats::fct_reorder(Genus,Value,.fun=mean))

# 20 genera require more actual panel height: allocated in final row layout.
# Keep labels readable without packing one line on top of another.
pD <- ggplot(gen_plot,aes(Treatment,Genus,fill=Z)) +
  geom_tile(colour="white",linewidth=0.28,width=0.96,height=0.96) +
  scale_fill_viridis_c(option="magma",limits=c(-1.2,1.2),oob=scales::squish,
                       guide="none") +
  labs(x=NULL,y=NULL) + text_theme(8.8) +
  theme(axis.line=element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(size=9.1,margin=margin(t=4)),
        axis.text.y=element_text(size=7.7,face="italic",colour="#000000",
                                 lineheight=0.92,margin=margin(r=4)),
        plot.margin=margin(2,2,3,4))

# =============== (e) EEM SPECTRA & SHARED INTENSITY BAR ===============
eem_raw <- readr::read_csv(file_eem,show_col_types=FALSE)
em_cols <- grep("^Em_",names(eem_raw),value=TRUE)
if (!length(em_cols)) stop("No Em_ columns found in EEM_plot_ready.csv")
if (!all(c("Treatment","Ex_nm") %in% names(eem_raw)))
  stop("EEM CSV must have Treatment and Ex_nm fields")
eem <- eem_raw %>%
  mutate(Treatment=recode(clean_treatment(Treatment),PGP="PEG"),
         Ex=as.numeric(Ex_nm)) %>%
  filter(Treatment %in% trt) %>%
  pivot_longer(cols=all_of(em_cols),names_to="Em_col",values_to="Intensity") %>%
  mutate(Em=as.numeric(str_remove(Em_col,"^Em_")),
         Intensity=as.numeric(Intensity)) %>%
  filter(between(Ex,250,550),between(Em,250,550),is.finite(Intensity)) %>%
  mutate(Ex=round(Ex),Em=round(Em),Intensity=pmin(pmax(Intensity,0),30000)) %>%
  group_by(Treatment,Ex,Em) %>%
  summarise(Intensity=mean(Intensity),.groups="drop") %>%
  mutate(Treatment=factor(Treatment,levels=trt))

# EEM audit: verify that the finite, in-range source pixels are retained.
# This is a diagnostic only; it does not alter their intensity values.
eem_qc <- eem %>%
  group_by(Treatment) %>%
  summarise(n_pixels=n(), Ex_min=min(Ex), Ex_max=max(Ex),
            Em_min=min(Em), Em_max=max(Em),
            Intensity_min=min(Intensity), Intensity_max=max(Intensity),
            .groups="drop")
message("EEM raster input ranges (after intentional wavelength/intensity filtering):")
print(eem_qc)
if (nrow(eem_qc)!=length(trt) || any(eem_qc$n_pixels==0)) {
  stop("EEM missing one or more treatment maps; check Treatment and wavelength columns.")
}

# Position treatment labels and three regions inside each map.
eem_region <- tidyr::crossing(Treatment=factor(trt,levels=trt),
                              label=c("IV","Other","V")) %>%
  mutate(Ex=case_when(label=="IV"~310,label=="V"~430,TRUE~464),
         Em=case_when(label=="IV"~312,label=="V"~470,TRUE~315))
eem_tags <- data.frame(Treatment=factor(trt,levels=trt),Ex=275,Em=534,label=trt)

pE <- ggplot(eem,aes(Ex,Em,fill=Intensity)) +
  geom_raster(interpolate=TRUE) +
  geom_hline(yintercept=380,colour="white",linewidth=0.56) +
  geom_vline(xintercept=380,colour="white",linewidth=0.56) +
  geom_text(data=eem_region,aes(Ex,Em,label=label),inherit.aes=FALSE,
            colour="white",size=2.5,family=font) +
  geom_text(data=eem_tags,aes(Ex,Em,label=label),inherit.aes=FALSE,
            colour="white",size=2.75,hjust=0,family=font) +
  facet_wrap(~Treatment,nrow=1) +
  scale_fill_viridis_c(option="inferno",limits=c(0,30000),oob=scales::squish,
                       guide="none") +
  # Interior ticks avoid 550 and 250 colliding at adjacent facet edges.
  # IMPORTANT: do not set hard limits on position scales for raster data.
  # ggplot2 censors raster edge pixels when limits crop half-width cells.
  # coord_cartesian zooms the viewport instead of deleting raster observations.
  scale_x_continuous(breaks=c(300,400,500),expand=c(0,0)) +
  scale_y_continuous(breaks=c(250,400,550),expand=c(0,0)) +
  coord_cartesian(xlim=c(250,550),ylim=c(250,550),expand=FALSE) +
  labs(x="Excitation (nm)",y="Emission (nm)") +
  theme_bw(base_size=8.6,base_family=font) +
  theme(text=element_text(family=font,colour="#000000"),
        panel.grid=element_blank(),strip.background=element_blank(),
        strip.text=element_blank(),panel.spacing.x=unit(1.4,"mm"),
        axis.title=element_text(size=9.3,colour="#000000"),
        axis.text=element_text(size=8.0,colour="#000000"),
        axis.ticks=element_line(colour="#000000",linewidth=0.28),
        plot.margin=margin(1,2,3,3))

# Custom shared colour bars: same normalization as panels (d) and (e).
make_scale_strip <- function(which=c("z","intensity")) {
  which <- match.arg(which)
  if (which=="z") {
    vals <- seq(-1.2,1.2,length.out=200)
    lim <- c(-1.2,1.2)
    breaks <- c(-1,0,1)
    title <- "Within-genus z-score"
    cols <- viridisLite::magma(256)
    lab <- c("−1","0","1")
  } else {
    vals <- seq(0,30000,length.out=200)
    lim <- c(0,30000)
    breaks <- c(0,15000,30000)
    title <- "EEM intensity"
    cols <- viridisLite::inferno(256)
    lab <- c("0","15k","30k")
  }
  dd <- data.frame(x=seq(0,1,length.out=length(vals)),v=vals)
  tick_x <- (breaks-lim[1])/diff(lim)
  ggplot(dd,aes(x,0.5,fill=v)) +
    geom_tile(width=1/(length(vals)-1),height=0.28) +
    annotate("text",x=0.5,y=0.96,label=title,size=3.05,
             colour="#000000",fontface="plain",family=font) +
    geom_text(data=data.frame(x=tick_x,label=lab),
              aes(x=x,y=0.10,label=label),inherit.aes=FALSE,size=2.56,
              colour="#000000",family=font) +
    scale_fill_gradientn(colours=cols,limits=lim,guide="none") +
    coord_cartesian(xlim=c(-0.03,1.03),ylim=c(0,1.10),clip="off",expand=FALSE) +
    theme_void() + theme(plot.margin=margin(1,3,1,3))
}
pZkey <- make_scale_strip("z")
pIkey <- make_scale_strip("intensity")
# Two bars grouped below (d), leaving a deliberate margin before (e).
pDkey <- (pZkey / pIkey) + plot_layout(heights=c(1,1))

# =============== (f) SEM: SAME MODELS, CLEANER VISUALIZATION ===============
sem_raw <- readxl::read_excel(file_micro,sheet="data")
names(sem_raw) <- trimws(names(sem_raw))
sc_t <- pick_col(sem_raw,c("Treatments","Treatment"),"data SEM")
sc_rep <- pick_col(sem_raw,c("Replicates","Replicate"),"data SEM")
sc_var <- pick_col(sem_raw,c("response.varibale","response.variable",
                             "response_varibale","response_variable"),"data SEM")
sc_val <- pick_col(sem_raw,c("Value","Values"),"data SEM")
sem_long <- sem_raw %>%
  transmute(treatments=clean_treatment(.data[[sc_t]]),
            replicates=trimws(as.character(.data[[sc_rep]])),
            variable=trimws(as.character(.data[[sc_var]])),
            value=as.numeric(.data[[sc_val]])) %>%
  filter(treatments %in% trt)
sem_wide <- sem_long %>% pivot_wider(id_cols=c(treatments,replicates),
                                     names_from=variable,values_from=value,
                                     values_fn=mean) %>%
  janitor::clean_names()
vars_sem <- c("p_h","hco3","co3","esp","stp","ca_f3",
              "maom_ca_f3","silt_clay","mwd","maom_c","biomass")
missing_sem <- setdiff(vars_sem,names(sem_wide))
if (length(missing_sem)) stop("Missing SEM variables: ",paste(missing_sem,collapse=", "),
                              ". Available: ",paste(names(sem_wide),collapse=", "))
sem_dat <- sem_wide %>%
  select(treatments,replicates,all_of(vars_sem)) %>%
  drop_na(all_of(vars_sem))
cat("SEM complete observations:",nrow(sem_dat),"\n")
if (nrow(sem_dat)<8) stop("Insufficient complete soil records for the requested SEM")
if (nrow(sem_dat)<20) warning("Exploratory SEM: low biological sample size (n = ",
                              nrow(sem_dat),"). Do not interpret paths as causal proof.")
sem_z <- sem_dat
sem_z[vars_sem] <- scale(sem_z[vars_sem])

# Source model equations retained exactly (these are separate regressions).
model_list <- list(
  hco3=c("p_h"), co3=c("p_h"),
  ca_f3=c("esp","co3","hco3","stp"),
  maom_ca_f3=c("stp","ca_f3"),
  silt_clay=c("esp"),
  mwd=c("maom_ca_f3","silt_clay"),
  maom_c=c("maom_ca_f3"),
  biomass=c("maom_c","mwd")
)
fit_one <- function(lhs, rhs) {
  form <- reformulate(rhs,response=lhs)
  fit <- lm(form,data=sem_z)
  co <- summary(fit)$coefficients
  co <- co[setdiff(rownames(co),"(Intercept)"),,drop=FALSE]
  data.frame(lhs=lhs,rhs=rownames(co),est.std=unname(co[,"Estimate"]),
             se=unname(co[,"Std. Error"]),
             pvalue=unname(co[,"Pr(>|t|)"]),
             r2=summary(fit)$r.squared,
             adj_r2=summary(fit)$adj.r.squared,n=nobs(fit),
             row.names=NULL)
}
std_paths <- bind_rows(lapply(names(model_list),function(lhs)
  fit_one(lhs,model_list[[lhs]])))
r2_df <- std_paths %>% group_by(lhs) %>%
  summarise(R2=first(r2),Adj_R2=first(adj_r2),n=first(n),.groups="drop")

# Shannon is not a fitted SEM variable and is therefore not shown in this model.
# MWD is repositioned into the former lower node position to clarify the fitted
# soil-structure branch: MCF and silt+clay predict MWD, which predicts biomass.
# The fitted variables and regression equations remain unchanged.
nodes <- tibble::tribble(
  ~name,          ~label,      ~x,   ~y,   ~fill,
  "p_h",          "pH",        0.65, 4.52, "#F5C6CD",
  "hco3",         "HCO₃⁻",     2.10, 4.50, "#E2E5E8",
  "co3",          "CO₃²⁻",     2.10, 3.69, "#E2E5E8",
  "esp",          "ESP",       0.65, 0.99, "#E88F8F",
  "stp",          "STP",       0.65, 2.10, "#9EC6E6",
  "ca_f3",        "CF",        4.15, 3.14, "#8FB5E4",
  "maom_ca_f3",   "MCF",       5.73, 3.14, "#B8AED9",
  "silt_clay",    "Silt+clay", 5.82, 0.90, "#E4BFC1",
  "mwd",          "MWD",       7.75, 2.24, "#B6DDAF",
  "maom_c",       "MAOC",      9.66, 3.14, "#B7E6DE",
  "biomass",      "Biomass",  11.58, 3.50, "#D9BFE7"
)
# These 14 edges are identical to the source SEM model list.
edge_key <- tibble::tribble(
  ~from,          ~to,          ~curv, ~labx, ~laby,
  "p_h",          "hco3",       0.05,  1.28, 4.87,
  "p_h",          "co3",       -0.13,  1.31, 3.68,
  "esp",          "ca_f3",     -0.18,  2.62, 1.72,
  "co3",          "ca_f3",      0.11,  3.18, 3.68,
  "hco3",         "ca_f3",     -0.20,  3.07, 4.42,
  "stp",          "ca_f3",      0.13,  2.34, 2.69,
  "stp",          "maom_ca_f3",-0.24,  4.12, 1.85,
  "ca_f3",        "maom_ca_f3", 0.00,  4.95, 3.46,
  "esp",          "silt_clay",-0.22,  3.50, 0.51,
  "maom_ca_f3",   "mwd",        0.19,  6.72, 2.70,
  "silt_clay",    "mwd",       -0.30,  7.09, 1.42,
  "maom_ca_f3",   "maom_c",    -0.10,  7.90, 3.29,
  "maom_c",       "biomass",    0.06, 10.67, 3.21,
  "mwd",          "biomass",   -0.16,  9.83, 2.78
)
paths <- edge_key %>%
  left_join(std_paths %>% select(lhs,rhs,est.std,pvalue),
            by=c("from"="rhs","to"="lhs")) %>%
  left_join(nodes %>% select(from=name,x1=x,y1=y),by="from") %>%
  left_join(nodes %>% select(to=name,x2=x,y2=y),by="to")
if (anyNA(paths$est.std)) stop("Path diagram edges do not match SEM equations")
paths <- paths %>%
  mutate(type=case_when(pvalue>=0.05~"Weak / n.s.",
                        est.std<0~"Negative",TRUE~"Positive"),
         width=case_when(pvalue>=0.05~0.45,
                         abs(est.std)>0.80~1.5,
                         abs(est.std)>0.5~1.2,TRUE~0.75),
         lab=if_else(pvalue<0.05,sprintf("%.2f",est.std),""),
         dx=x2-x1,dy=y2-y1,dist=pmax(sqrt(dx^2+dy^2),0.001),
         x1s=x1+0.22*dx/dist,y1s=y1+0.22*dy/dist,
         x2s=x2-0.27*dx/dist,y2s=y2-0.27*dy/dist)

# For path clarity, neutral lines are thin dotted; signed effects are coloured.
line_cols <- c("Positive"="#188B61","Negative"="#CE3D40","Weak / n.s."="#747474")
plot_sem <- ggplot() +
  # Soft grouping bands, without the overlapping headers and six heavy boxes.
  annotate("rect",xmin=3.58,xmax=6.26,ymin=0.48,ymax=5.03,
           fill="#EFF5FD",alpha=0.65) +
  annotate("rect",xmin=6.96,xmax=8.60,ymin=0.48,ymax=5.03,
           fill="#EEF7F1",alpha=0.55) +
  annotate("rect",xmin=9.10,xmax=10.28,ymin=2.60,ymax=3.65,
           fill="#E6F6F3",alpha=0.65) +
  annotate("text",x=1.2,y=5.32,label="Ionic / P inputs",size=2.65,colour="black",family=font) +
  annotate("text",x=4.95,y=5.32,label="Ca association",size=2.65,colour="black",family=font) +
  annotate("text",x=7.73,y=5.32,label="Soil structure",size=2.65,colour="black",family=font) +
  annotate("text",x=9.72,y=5.32,label="MAOM-C",size=2.65,colour="black",family=font) +
  annotate("text",x=11.62,y=5.32,label="Plant",size=2.65,colour="black",family=font)

# Curves are split by curvature because ggplot2 takes curvature per layer.
for (i in seq_len(nrow(paths))) {
  d <- paths[i,,drop=FALSE]
  plot_sem <- plot_sem +
    geom_curve(data=d,aes(x=x1s,y=y1s,xend=x2s,yend=y2s),
               curvature=d$curv,colour=unname(line_cols[d$type]),
               linewidth=d$width,
               linetype=if (d$type=="Weak / n.s.") "dashed" else "solid",
               arrow=grid::arrow(length=unit(1.25,"mm"),type="closed"),
               lineend="round",inherit.aes=FALSE)
}
# No Shannon node or dotted descriptor connector: Shannon is shown only in panel (a).
plot_sem <- plot_sem +
  geom_label(data=paths %>% filter(lab!=""),
             aes(x=labx,y=laby,label=lab),size=2.35,family=font,
             fill="white",colour="#000000",label.size=0.08,
             label.padding=unit(0.45,"mm"),fontface="plain") +
  geom_label(data=nodes,aes(x=x,y=y,label=label,fill=fill),
             colour="#000000",size=2.72,family=font,label.size=0.25,
             label.padding=unit(0.75,"mm")) +
  scale_fill_identity() +
  coord_cartesian(xlim=c(0.12,12.15),ylim=c(0.15,5.62),clip="off") +
  theme_void() +
  theme(plot.margin=margin(3,3,1,3),
        plot.background=element_rect(fill="white",colour=NA))

# SEM legend below the network in a separate strip, never over pathways.
sem_leg <- data.frame(label=c("Positive","Negative","Weak / n.s."),
                      x=c(0.04,0.38,0.71),col=unname(line_cols))
pSEMleg <- ggplot(sem_leg) +
  geom_segment(aes(x=x,y=0.55,xend=x+0.11,yend=0.55,colour=label,
                   linetype=label),linewidth=0.95,
               arrow=grid::arrow(type="closed",length=unit(1.2,"mm"))) +
  geom_text(aes(x=x+0.14,y=0.55,label=label),hjust=0,vjust=0.5,
            size=2.65,colour="#000000",family=font) +
  scale_colour_manual(values=line_cols,guide="none") +
  scale_linetype_manual(values=c("Positive"="solid","Negative"="solid",
                                 "Weak / n.s."="dashed"),guide="none") +
  coord_cartesian(xlim=c(0,1),ylim=c(0,1),clip="off") +
  theme_void() + theme(plot.margin=margin(0,1,0,1))
pF <- (plot_sem / pSEMleg) + plot_layout(heights=c(6,0.58))

# =============== FINAL PORTRAIT ASSEMBLY ===============
# IMPORTANT: NO global "& theme(axis...)" here. It reintroduces x/y axes on
# theme_void() objects and creates the spurious numeric ticks seen previously.
# Fixed 17 x 22.4 cm. Create visibly separated tags in every row.
# 20 genus names require the taxonomy plot row to be taller than before.
# Chart widths prioritize long genus labels; legend row widths are
# INDEPENDENT so long phylum names do not get cut at the column boundary.
row_ab <- patchwork::wrap_plots(tagged(pA,"(a)"),tagged(pB,"(b)"),
                                nrow=1,widths=c(1.05,0.95))
row_cd_charts <- patchwork::wrap_plots(tagged(pC,"(c)"),tagged(pD,"(d)"),
                                       nrow=1,widths=c(0.40,0.60))
row_cd_keys <- patchwork::wrap_plots(pCkey,pDkey,
                                     nrow=1,widths=c(0.50,0.50))
# Within row (c,d): 7.20 cm charts plus 2.85 cm keys = 10.05 cm.
row_cd <- (row_cd_charts / row_cd_keys) +
  patchwork::plot_layout(heights=c(7.20,2.85))

# Total 22.40 cm: upper row 3.40; taxonomy and scales 10.05;
# EEM 2.85; full-width soil/plant path model 6.10.
# Keep the page at 17 cm so Word will not squeeze labels on insertion.
fig_all <- (row_ab / row_cd / tagged(pE,"(e)") / tagged(pF,"(f)")) +
  patchwork::plot_layout(heights=c(3.40,10.05,2.85,6.10)) &
  theme(plot.background=element_rect(fill="white",colour=NA))

WIDTH_CM <- 17.0
HEIGHT_CM <- 22.4

savefig <- function(path, device=NULL, dpi=600, ...) {
  ggplot2::ggsave(path,plot=fig_all,width=WIDTH_CM,height=HEIGHT_CM,
                  units="cm",device=device,dpi=dpi,bg="white",...)
}
savefig(file.path(out_dir,"Fig5_NC_FINAL_CLEAN_600dpi.tiff"),
        device="tiff",compression="lzw")
savefig(file.path(out_dir,"Fig5_NC_FINAL_CLEAN_600dpi.png"),dpi=600)
if (capabilities("cairo")) {
  savefig(file.path(out_dir,"Fig5_NC_FINAL_CLEAN.pdf"),
          device=grDevices::cairo_pdf)
} else {
  message("Cairo unavailable: vector PDF skipped")
}
savefig(file.path(out_dir,"Fig5_NC_FINAL_CLEAN_PREVIEW.png"),dpi=180)

# No separate audit workbook is exported. The single Source_Data_Fig5.xlsx
# workbook below is the only Figure 5 data workbook intended for submission.
# This avoids producing a second workbook with overlapping data tables.
message("Complete. Results folder: ",normalizePath(out_dir))
fig_all

# ============================================================================
# FIGURE 5 — MINIMAL, NON-REPETITIVE SOURCE DATA EXPORT
# One worksheet; three blank rows between panels (a)–(f).
# Panel (a) contains Shannon only; Chao is excluded. Only plotted values and
# the model inputs/results needed to interpret panel (f)
# are included. No duplicate raw/processed tables, plot coordinates, audit tables,
# standardized-copy tables, or repeated summary statistics are exported.
# ============================================================================

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}

sd_file <- file.path(out_dir, "Source_Data_Fig5.xlsx")
sd_tab <- "Fig 5 Source Data"
sd_book <- openxlsx::createWorkbook(creator = "Figure 5 source data")
openxlsx::addWorksheet(sd_book, sd_tab, gridLines = FALSE)

sd_maxcols <- 13L
sd_excel_maxrow <- 1048576L

sd_style_title <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 14, textDecoration = "bold",
  fontColour = "#FFFFFF", fgFill = "#17365D", valign = "center"
)
sd_style_panel <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 11, textDecoration = "bold",
  fontColour = "#FFFFFF", fgFill = "#234A71", valign = "center"
)
sd_style_subhead <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 10, textDecoration = "bold",
  fontColour = "#17365D", fgFill = "#E8F0F7", valign = "center"
)
sd_style_columns <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 9, textDecoration = "bold",
  fontColour = "#243344", fgFill = "#F3F5F7", wrapText = TRUE,
  valign = "center", border = "Bottom", borderColour = "#BECBD8"
)
sd_style_body <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 9, fontColour = "#243344", valign = "center"
)
sd_style_numeric <- openxlsx::createStyle(numFmt = "0.##########")
sd_style_note <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 9, textDecoration = "italic",
  fontColour = "#526579", wrapText = TRUE, valign = "center"
)

sd_write_band <- function(row, title, style, height = 24L) {
  openxlsx::mergeCells(sd_book, sd_tab, cols = 1:sd_maxcols, rows = row)
  openxlsx::writeData(sd_book, sd_tab, title, startRow = row, startCol = 1)
  openxlsx::addStyle(sd_book, sd_tab, style,
                     rows = row, cols = 1:sd_maxcols, gridExpand = TRUE)
  openxlsx::setRowHeights(sd_book, sd_tab, rows = row, heights = height)
}

sd_write_table <- function(data, header_row) {
  data <- as.data.frame(data, check.names = FALSE)
  if (ncol(data) < 1L) stop("Source-data table has no columns.")
  if (ncol(data) > sd_maxcols) {
    stop("Source-data table exceeds the worksheet column limit: ", ncol(data))
  }
  if (nrow(data) < 1L) stop("Source-data table is empty at row ", header_row)
  if (anyDuplicated(names(data))) stop("Duplicate column names in source-data table.")
  end_row <- header_row + nrow(data)
  if (end_row > sd_excel_maxrow) {
    stop("Source Data exceeds Excel's row limit at row ", end_row,
         ". Do not truncate the EEM grid.")
  }
  openxlsx::writeData(
    sd_book, sd_tab, data, startRow = header_row, startCol = 1,
    colNames = TRUE, rowNames = FALSE, keepNA = FALSE,
    headerStyle = sd_style_columns
  )
  openxlsx::setRowHeights(sd_book, sd_tab, rows = header_row, heights = 32L)
  
  # Style small tables only. Avoid thousands of per-cell styles for the EEM grid.
  if (nrow(data) <= 3000L) {
    openxlsx::addStyle(
      sd_book, sd_tab, sd_style_body,
      rows = seq.int(header_row + 1L, end_row),
      cols = seq_len(ncol(data)), gridExpand = TRUE, stack = TRUE
    )
    numeric_cols <- which(vapply(data, is.numeric, logical(1)))
    if (length(numeric_cols)) {
      openxlsx::addStyle(
        sd_book, sd_tab, sd_style_numeric,
        rows = seq.int(header_row + 1L, end_row),
        cols = numeric_cols, gridExpand = TRUE, stack = TRUE
      )
    }
  }
  end_row
}

sd_cursor <- new.env(parent = emptyenv())
sd_cursor$last_row <- 2L
sd_cursor$has_panel <- FALSE
sd_cursor$panels <- character(0)
sd_cursor$panel_heading_rows <- integer(0)

sd_write_band(1L, "FIGURE 5 | SOURCE DATA", sd_style_title, 28L)
sd_write_band(
  2L,
  paste0(
    "One worksheet. Microbiome panels (a)–(d) represent treatment-level composites (n = 1 per treatment); ",
    "panel (f) retains complete-case replicate-level model inputs. Values are not duplicated across raw, processed, and audit tables."
  ),
  sd_style_note, 34L
)

sd_start_panel <- function(letter, label, note) {
  # First panel begins at row 4. Subsequent panels begin after exactly three blank rows.
  rr <- if (isTRUE(sd_cursor$has_panel)) sd_cursor$last_row + 4L else 4L
  sd_write_band(rr, paste0("Fig. 5", letter, " | ", label), sd_style_panel, 25L)
  sd_write_band(rr + 1L, note, sd_style_note, 34L)
  sd_cursor$has_panel <- TRUE
  sd_cursor$panels <- c(sd_cursor$panels, letter)
  sd_cursor$panel_heading_rows <- c(sd_cursor$panel_heading_rows, rr)
  rr
}

sd_add_panel <- function(letter, label, note, data) {
  rr <- sd_start_panel(letter, label, note)
  sd_cursor$last_row <- sd_write_table(data, rr + 2L)
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# (a) Shannon dot plot: only the exact treatment-level values plotted.
# ---------------------------------------------------------------------------
sd_a <- alpha %>%
  dplyr::transmute(
    Treatment = as.character(Treatment),
    Shannon_index = value
  )
sd_add_panel(
  "a", "Shannon alpha diversity",
  "One treatment-level composite Shannon value per treatment (n = 1); no biological replicate-level variation or inferential testing is available.",
  sd_a
)

# ---------------------------------------------------------------------------
# (b) PCoA: only the three plotted treatment coordinates.
# ---------------------------------------------------------------------------
sd_b <- pcoa %>%
  dplyr::transmute(
    Treatment = as.character(Treatment),
    PCo1 = PCo1,
    PCo2 = PCo2
  )
sd_add_panel(
  "b", "Bray–Curtis PCoA coordinates",
  "One composite point per treatment. The plotted axis labels report 35.45% for PCo1 and 22.47% for PCo2.",
  sd_b
)

# ---------------------------------------------------------------------------
# (c) Phylum stacked bars: exact percentages shown in the figure only.
# ---------------------------------------------------------------------------
sd_c <- phy_plot %>%
  dplyr::transmute(
    Treatment = as.character(Treatment),
    Phylum_segment = as.character(Phylum),
    Relative_abundance_percent = Percent
  )
sd_add_panel(
  "c", "Phylum relative abundance",
  "Exact stacked-bar percentages are supplied. The 15 leading named phyla are shown separately; remaining categories are pooled as Other. Percentages sum to 100% within each treatment.",
  sd_c
)

# ---------------------------------------------------------------------------
# (d) Genus heatmap: one row per plotted treatment–genus cell; abundance and
# its plotted within-genus Z-score share the same row, avoiding duplicate tables.
# ---------------------------------------------------------------------------
sd_d <- gen_plot %>%
  dplyr::transmute(
    Treatment = as.character(Treatment),
    Genus = as.character(Genus),
    Treatment_level_abundance = Value,
    Within_genus_Z_score_plotted = Z
  )
sd_add_panel(
  "d", "Genus heatmap",
  "Only the 20 genera displayed are included. Treatment-level abundance and the corresponding within-genus Z-score are provided together; heatmap colours are clipped to the displayed range of −1.2 to 1.2.",
  sd_d
)

# ---------------------------------------------------------------------------
# (e) EEM: exact processed raster values used in the figure, once only.
# The original intensities are not duplicated alongside their transformed copy.
# ---------------------------------------------------------------------------
sd_e <- eem %>%
  dplyr::transmute(
    Treatment = as.character(Treatment),
    Excitation_nm = Ex,
    Emission_nm = Em,
    Intensity_plotted = Intensity
  )
sd_add_panel(
  "e", "Excitation–emission matrix (EEM)",
  "Exact raster inputs are supplied once. Excitation/emission wavelengths were restricted to 250–550 nm and rounded to integer nm; intensities were clipped to 0–30,000 and duplicate grid locations averaged before plotting.",
  sd_e
)

# ---------------------------------------------------------------------------
# (f) Soil–plant path diagram: complete-case replicate inputs plus the fitted
# path estimates needed to interpret the arrows. Shannon is not part of the fit.
# No standardized input copy, node positions, line coordinates, R² audit table,
# or duplicated summaries.
# ---------------------------------------------------------------------------
rr_f <- sd_start_panel(
  "f", "Soil–plant path diagram",
  "The path diagram uses separate linear regressions on standardized variables, not a simultaneous covariance-based SEM. Shannon is shown only in panel (a) and is not included as a predictor or response in the fitted path model."
)

sd_f_input <- sem_dat %>%
  dplyr::transmute(
    Treatment = as.character(treatments),
    Replicate = as.character(replicates),
    pH = p_h,
    HCO3 = hco3,
    CO3 = co3,
    ESP = esp,
    STP = stp,
    Ca_F3 = ca_f3,
    MAOM_Ca_F3 = maom_ca_f3,
    Silt_clay = silt_clay,
    MWD = mwd,
    MAOM_C = maom_c,
    Biomass = biomass
  )

sd_write_band(rr_f + 2L, "Complete-case replicate-level model inputs (original units)",
              sd_style_subhead, 23L)
end_inputs <- sd_write_table(sd_f_input, rr_f + 3L)

sd_f_paths <- paths %>%
  dplyr::left_join(
    nodes %>% dplyr::select(from_code = name, Predictor = label),
    by = c("from" = "from_code")
  ) %>%
  dplyr::left_join(
    nodes %>% dplyr::select(to_code = name, Response = label),
    by = c("to" = "to_code")
  ) %>%
  dplyr::transmute(
    Predictor = Predictor,
    Response = Response,
    Standardized_beta = est.std,
    P_value = pvalue
  )

sd_write_band(end_inputs + 2L, "Fitted path coefficients used in panel (f)",
              sd_style_subhead, 23L)
end_paths <- sd_write_table(sd_f_paths, end_inputs + 3L)
sd_cursor$last_row <- end_paths

# ---------------------------------------------------------------------------
# Export checks: one worksheet, panels a–f in order, exactly three blank rows
# between panel sections. The workbook contains no repeated audit tables.
# ---------------------------------------------------------------------------
if (!identical(sd_cursor$panels, letters[1:6])) {
  stop("Source Data panels are incomplete or out of order: ",
       paste(sd_cursor$panels, collapse = ", "))
}
if (length(sd_cursor$panel_heading_rows) != 6L) {
  stop("Source Data panel headings were not recorded correctly.")
}

openxlsx::setColWidths(
  sd_book, sd_tab, cols = 1:sd_maxcols,
  widths = c(18, 26, 25, 22, 18, 18, 18, 18, 18, 18, 18, 18, 18)
)
openxlsx::pageSetup(sd_book, sd_tab, orientation = "landscape",
                    fitToWidth = 1, fitToHeight = 0)
openxlsx::saveWorkbook(sd_book, file = sd_file, overwrite = TRUE)

if (!file.exists(sd_file) || is.na(file.info(sd_file)$size) || file.info(sd_file)$size <= 0L) {
  stop("Figure 5 Source Data workbook failed to save: ", sd_file)
}
if (!identical(openxlsx::getSheetNames(sd_file), sd_tab)) {
  stop("Source Data workbook must contain exactly one worksheet named '", sd_tab, "'.")
}

status_file <- file.path(out_dir, "FIG5_RUN_STATUS.txt")
writeLines(c(
  "FIGURE 5 — MINIMAL SOURCE DATA SAVED",
  paste("Source Data workbook:", normalizePath(sd_file, winslash = "/")),
  paste("File size (bytes):", file.info(sd_file)$size),
  paste("Worksheet:", sd_tab),
  "Panels: a, b, c, d, e, f",
  "One compact data table per panel, except panel f (model inputs and path estimates)",
  "Three completely blank rows between panel sections: yes",
  "Microbiome panels a–d: treatment-level composite data, n=1 per treatment; panel (a) is Shannon only",
  "Panel f: complete-case replicate-level inputs and fitted path coefficients",
  "No separate audit workbook is generated by this version",
  paste("Figure TIFF exists:", file.exists(file.path(out_dir, "Fig5_NC_FINAL_CLEAN_600dpi.tiff"))),
  paste("Figure PNG exists:", file.exists(file.path(out_dir, "Fig5_NC_FINAL_CLEAN_600dpi.png")))
), status_file)

message("\n==========================================================")
message("FIGURE 5 MINIMAL SOURCE DATA SAVED")
message("FILE: ", normalizePath(sd_file, winslash = "/"))
message("TAB: ", sd_tab)
message("PANELS: a–f; three blank rows between panel sections")
message("SIZE: ", file.info(sd_file)$size, " bytes")
message("Only plotted numerical data and necessary panel (f) model inputs/results are included.")
message("==========================================================\n")

if (.Platform$OS.type == "windows") {
  try(shell.exec(normalizePath(out_dir, winslash = "\\")), silent = TRUE)
}