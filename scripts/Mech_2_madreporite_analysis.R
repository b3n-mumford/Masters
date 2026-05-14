setwd("~/Library/CloudStorage/OneDrive-UniversitedeMontreal/MSc_Thesis")


#read in dataframe 
pisaster_madreporite <- read.csv("Data/processed/pisaster_madreporite.csv")


library(tidyverse)

# Identify element columns
element_cols <-   c("specimen", "element_name", "treatment", "concentration")

# Reshape to for ggplot
#want to rename atomic.concentration.percentage to concentratoin 
pisaster_madreporite <- pisaster_madreporite %>%
  rename(
    concentration = Atomic.concentration.percentage
  ) 


ggplot(pisaster_madreporite, aes(x = treatment, y = concentration, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  facet_wrap(~ element_name, scales = "free_y") +
  theme_bw() +
  labs(title = "Elemental Concentrations by Treatment")





#filter out to just a few key elements for graphical repersetnation 
pisaster_madreporite_filtered <- pisaster_madreporite %>%
  filter(!element_name %in% c("Potassium" ,"Germanium", "Selenium",  "Bromine"  , "Rubidium",  "Tin" ,
                              "Antimony" , "Cesium" ,  
                              "Barium"   , "Lead" ,    "Polonium" , "Astatine" , "Radium" , 
                              "Francium" , "Bismuth" ,"Thallium","Indium", "Gallium", "Tellurium"))
                              

ggplot(pisaster_madreporite_filtered, aes(x = treatment, y = concentration, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  facet_wrap(~ element_name, scales = "free_y") +
  theme_bw() +
  labs(title = "Elemental Concentrations by Treatment", x = "Treatment", y = "Atomic Concentration (%)")
#i need to convert the y axis to an actual percentage and so multiply the concentration by 100 to get a percentage value for the y axis
ggplot(pisaster_madreporite_filtered, aes(x = treatment, y = concentration * 100, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  facet_wrap(~ element_name, scales = "free_y") +
  theme_bw() +
  labs(title = "Elemental Concentrations by Treatment", x = "Treatment", y = "Atomic Concentration (%)")


#given this graph i want to go back and check that all my values are positive as there should be no negative concentratoins 
check_df <- pisaster_madreporite_filtered %>%
  filter(concentration < 0)
#make sure that graphically there are no negative values
ggplot(pisaster_madreporite_filtered, aes(x = treatment, y = concentration * 100, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  facet_wrap(~ element_name, scales = "free_y") +
  theme_bw() +
  labs(title = "Elemental Concentrations by Treatment", x = "Treatment", y = "Atomic Concentration (%)") +
  scale_y_continuous(limits = c(0, NA)) # set lower limit to 0 to ensure no negative values are shown



# just want to analyse the difference in magnsium concentratoins between samples and between treatments 
magnesium_concentration <- pisaster_madreporite %>%
  filter(element_name == "Magnesium") %>%
  group_by(treatment) %>%
  summarise(
    mean_concentration = mean(concentration, na.rm = TRUE),
    sd_concentration = sd(concentration, na.rm = TRUE),
    n = n()
  )








######

#identify which element did have somee difference in elements 
element_stats <- pisaster_madreporite %>%
  group_by(element_name) %>%
  summarise(
    mean_concentration = mean(concentration, na.rm = TRUE),
    sd_concentration = sd(concentration, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(sd_concentration))


#run a test to show largest
dif_elements <- pisaster_madreporite %>%
  group_by(element_name) %>%
  summarise(
    mean_concentration = mean(concentration, na.rm = TRUE),
    sd_concentration = sd(concentration, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(sd_concentration)) %>%
  #filter sd to greater than 0.1 to show only elements with some variation in concentration across samples
  filter(sd_concentration > 0.1)





##runninng a pca 

library(tidyverse)

pisaster_wide <- pisaster_madreporite %>%
  select(specimen, element_name, concentration, treatment) %>%
  pivot_wider(
    names_from = element_name,
    values_from = concentration
  )



# Matrix of element concentrations
element_matrix <- pisaster_wide %>%
  select(-specimen, -treatment) %>%
  as.matrix()

# Row names
row.names(element_matrix) <- pisaster_wide$specimen

# Treatment vector
treatment_factor <- factor(pisaster_wide$treatment)




#have to nmodify some columns before can run 
# Calculate standard deviation per element
constant_elements <- pisaster_wide %>%
  select(-specimen, -treatment) %>%
  summarise(across(everything(), ~ sd(.))) %>%
  pivot_longer(everything(), names_to = "element", values_to = "sd") %>%
  filter(sd == 0)

constant_elements

#remove constant elements 

element_matrix <- pisaster_wide %>%
  select(-specimen, -treatment) %>%
  select(where(~ sd(.) > 0)) %>%  # keep only columns with variance
  as.matrix()




pca_res <- prcomp(element_matrix, scale. = TRUE)

pca_scores <- as_tibble(pca_res$x) %>%
  bind_cols(treatment = pisaster_wide$treatment)

ggplot(pca_scores, aes(x = PC1, y = PC2, color = treatment)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "PCA of Elemental Composition by Treatment")




#run a permanova ?
library(vegan)
treatment_factor <- factor(pisaster_wide$treatment)

adonis2(element_matrix ~ treatment_factor, method = "euclidean")








############################
# SECOND RUN #######
#######################

############################################################
# Pisaster madreporite EDS: T1 (normal) vs T2 (crushed)
# Input: pisaster_madreporite.csv
############################################################

# ---------- Packages ----------
pkgs <- c(
  "tidyverse", "janitor", "stringr",
  "compositions",   # closure(), acomp(), clr()
  "vegan",          # adonis2(), betadisper()
  "pheatmap",
  "ggrepel"
)
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

library(tidyverse)
library(janitor)
library(stringr)
library(compositions)
library(vegan)
library(pheatmap)
library(ggrepel)

# ---------- I/O ----------
infile <- "Data/processed/pisaster_madreporite.csv"  # adjust path if needed
raw <- readr::read_csv(infile, show_col_types = FALSE) %>% janitor::clean_names()

# ---------- 0) Structure checks ----------
required <- c(
  "file_path", "element_symbol",
  "atomic_concentration_percentage", "weight_concentration_percentage",
  "treatment"
)
missing <- setdiff(required, names(raw))
stopifnot(length(missing) == 0)

# Extract tray_id and replicate from file_path: export/T1_P1 etc.
dat <- raw %>%
  mutate(
    tray_id   = str_match(file_path, "export/(T[12])_P(\\d+)")[, 2],
    replicate = str_match(file_path, "export/(T[12])_P(\\d+)")[, 3] %>% as.integer(),
    sample_id = paste0(tray_id, "_P", replicate),
    treatment = tolower(treatment)
  ) %>%
  filter(!is.na(tray_id), !is.na(replicate)) %>%
  mutate(
    # enforce your experimental meaning
    treatment = case_when(
      tray_id == "T1" ~ "Normal diet",
      tray_id == "T2" ~ "Crushed diet",
      TRUE ~ treatment
    ),
    treatment = factor(treatment, levels = c("Normal diet", "Crushed diet"))
  )

# Sanity summary
cat("\n--- Sanity summary ---\n")
cat("Rows:", nrow(dat), " Cols:", ncol(dat), "\n")
cat("Treatments:\n"); print(table(dat$treatment))
cat("Unique replicates per tray:\n"); print(dat %>% distinct(tray_id, replicate) %>% count(tray_id))
cat("Unique elements:", n_distinct(dat$element_symbol), "\n")

# ---------- Choose response variable ----------
# Your file has both; default to atomic (already sums ~1 per sample)
value_col <- "atomic_concentration_percentage"
# value_col <- "weight_concentration_percentage"

dat <- dat %>% rename(value = all_of(value_col)) %>% mutate(value = as.numeric(value))

# ---------- 1) Replicate-level mean (protects against pseudoreplication if you ever have multiple spots) ----------
rep_long <- dat %>%
  group_by(sample_id, tray_id, treatment, replicate, element_symbol) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

# ---------- 2) Wide matrix (samples x elements) ----------
rep_wide <- rep_long %>%
  pivot_wider(names_from = element_symbol, values_from = mean_value, values_fill = 0)

meta <- rep_wide %>% dplyr::select(sample_id, tray_id, treatment, replicate)

X <- rep_wide %>%
  dplyr::select(-sample_id, -tray_id, -treatment, -replicate) %>%
  as.data.frame()

# ---------- 3) Compositional preprocessing (CLR / Aitchison) ----------
# CLR needs strictly positive -> add pseudocount for zeros
pseudocount <- 1e-6
X_pos <- X %>% mutate(across(everything(), ~ .x + pseudocount))

# Manual closure to constant total (1 if atomic proportions; use 100 if desired)
X_mat <- as.matrix(X_pos)
rs <- rowSums(X_mat)
if (any(rs <= 0 | !is.finite(rs))) stop("Row sums are non-positive or non-finite; check your matrix.")

X_closed <- sweep(X_mat, 1, rs, "/")  # closed to total = 1

# CLR transform; Euclidean distance in CLR space == Aitchison distance
X_clr <- compositions::clr(compositions::acomp(X_closed))
rownames(X_clr) <- meta$sample_id

# ---------- 4) Visualisations ----------
# 4a) Stacked bars (compositional proportions)
p_stack <- rep_long %>%
  group_by(sample_id) %>%
  mutate(prop = mean_value / sum(mean_value)) %>%
  ungroup() %>%
  ggplot(aes(x = sample_id, y = prop, fill = element_symbol)) +
  geom_col(width = 0.9) +
  facet_grid(. ~ treatment, scales = "free_x", space = "free_x") +
  labs(title = "Composition by replicate (stacked proportions)", x = "Replicate", y = "Proportion") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
print(p_stack)

# 4b) Heatmap (CLR, z-scored by element for visibility)
mat_heat <- scale(X_clr)
ann <- meta %>%
  tibble::column_to_rownames("sample_id") %>%
  dplyr::select(treatment, replicate, tray_id)
pheatmap(mat_heat, annotation_row = ann, fontsize_row = 8,
         main = "Heatmap: CLR composition (z-scored by element)")

# 4c) Aitchison PCA (PCA on CLR)
pca <- prcomp(X_clr, center = TRUE, scale. = FALSE)
scores <- as.data.frame(pca$x) %>%
  rownames_to_column("sample_id") %>%
  left_join(meta, by = "sample_id")

var_expl <- (pca$sdev^2) / sum(pca$sdev^2)

p_pca <- ggplot(scores, aes(PC1, PC2, shape = treatment)) +
  geom_point(size = 3) +
  ggrepel::geom_text_repel(aes(label = sample_id), size = 3, max.overlaps = 50) +
  labs(
    title = "Aitchison PCA (CLR space)",
    x = paste0("PC1 (", round(100*var_expl[1], 1), "%)"),
    y = paste0("PC2 (", round(100*var_expl[2], 1), "%)")
  ) +
  theme_minimal(base_size = 12)
print(p_pca)

##############
# =========================================================
# INSERT BEFORE SECTION 5: Trace elements + Magnesium quick look
# Requires objects already created earlier:
#   rep_long  (sample_id, tray_id, treatment, replicate, element_symbol, mean_value)
# =========================================================

# --- 4.x Define "major" vs "trace" elements (simple, transparent rule) ---
# Here: "major" = mean atomic fraction >= 5% across all samples.
elem_means <- rep_long %>%
  dplyr::group_by(element_symbol) %>%
  dplyr::summarise(overall_mean = mean(mean_value, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(overall_mean))

major_thresh <- 0.05
major_elements <- elem_means %>% dplyr::filter(overall_mean >= major_thresh) %>% dplyr::pull(element_symbol)
trace_elements <- setdiff(elem_means$element_symbol, major_elements)

cat("\nMajor elements (mean >= ", major_thresh, "): ", paste(major_elements, collapse = ", "), "\n", sep = "")
cat("No. trace elements: ", length(trace_elements), "\n", sep = "")

# --- 4.x Table: trace-element summary by treatment ---
trace_summary <- rep_long %>%
  dplyr::filter(element_symbol %in% trace_elements) %>%
  dplyr::group_by(treatment, element_symbol) %>%
  dplyr::summarise(
    mean = mean(mean_value, na.rm = TRUE),
    sd   = sd(mean_value, na.rm = TRUE),
    min  = min(mean_value, na.rm = TRUE),
    max  = max(mean_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(treatment, dplyr::desc(mean))

print(trace_summary, n = 50)  # adjust n as you like
#readr::write_csv(trace_summary, "trace_elements_summary_by_treatment.csv")

# --- 4.x Plot: top trace elements (by overall mean), compare treatments ---
top_n_trace <- 12
top_trace <- elem_means %>%
  dplyr::filter(element_symbol %in% trace_elements) %>%
  dplyr::slice_head(n = top_n_trace) %>%
  dplyr::pull(element_symbol)

p_trace_top <- trace_summary %>%
  dplyr::filter(element_symbol %in% top_trace) %>%
  ggplot2::ggplot(ggplot2::aes(x = element_symbol, y = mean, shape = treatment)) +
  ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5), size = 3) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = mean - sd, ymax = mean + sd),
    position = ggplot2::position_dodge(width = 0.5),
    width = 0.2
  ) +
  ggplot2::scale_y_continuous(trans = "log10") +
  ggplot2::labs(
    title = paste0("Top ", top_n_trace, " trace elements (mean ± SD), log10 scale"),
    x = "Element", y = "Mean atomic fraction (log10)"
  ) +
  ggplot2::theme_minimal(base_size = 12)
print(p_trace_top)

# --- 4.x Magnesium-only: replicate-level table + simple plots + simple tests ---
mg_df <- rep_long %>%
  dplyr::filter(element_symbol == "Mg") %>%
  dplyr::arrange(treatment, replicate) 

# Table: Mg per replicate (one row per replicate within each treatment)
mg_table_long <- mg_df %>%
  dplyr::select(tray_id, treatment, replicate, mg_atomic = mean_value)
print(mg_table_long, n = 50)
#readr::write_csv(mg_table_long, "magnesium_by_replicate_long.csv")

# Optional wide view (replicate rows, separate columns for treatments)
mg_table_wide <- mg_table_long %>%
  tidyr::pivot_wider(names_from = treatment, values_from = mg_atomic)
print(mg_table_wide, n = 20)
#readr::write_csv(mg_table_wide, "magnesium_by_replicate_wide.csv")

# Summary stats (Mg)
mg_summary <- mg_df %>%
  dplyr::group_by(treatment) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mean = mean(mean_value, na.rm = TRUE),
    sd   = sd(mean_value, na.rm = TRUE),
    median = median(mean_value, na.rm = TRUE),
    iqr  = IQR(mean_value, na.rm = TRUE),
    .groups = "drop"
  )
print(mg_summary)
#readr::write_csv(mg_summary, "magnesium_summary_by_treatment.csv")

# Plot 1: Mg distribution by treatment (quick)
p_mg_box <- ggplot2::ggplot(mg_df, ggplot2::aes(x = treatment, y = mean_value)) +
  ggplot2::geom_boxplot(outlier.shape = NA) +
  ggplot2::geom_jitter(width = 0.08, alpha = 0.7) +
  ggplot2::labs(title = "Magnesium (atomic fraction) by treatment", x = NULL, y = "Mg atomic fraction") +
  ggplot2::theme_minimal(base_size = 12)
print(p_mg_box)

# Plot 2: Mg across replicates within each treatment (pattern check)
p_mg_rep <- ggplot2::ggplot(mg_df, ggplot2::aes(x = factor(replicate), y = mean_value, group = treatment, shape = treatment)) +
  ggplot2::geom_point(size = 3, position = ggplot2::position_dodge(width = 0.25)) +
  ggplot2::geom_line(position = ggplot2::position_dodge(width = 0.25)) +
  ggplot2::labs(title = "Magnesium across replicates (within-treatment pattern)", x = "Replicate", y = "Mg atomic fraction") +
  ggplot2::theme_minimal(base_size = 12)
print(p_mg_rep)

# Simple (exploratory) tests on Mg replicate means
mg_ttest <- t.test(mean_value ~ treatment, data = mg_df)      # Welch t-test
mg_wilcox <- wilcox.test(mean_value ~ treatment, data = mg_df) # Nonparametric
cat("\n--- Mg Welch t-test ---\n"); print(mg_ttest)
cat("\n--- Mg Wilcoxon test ---\n"); print(mg_wilcox)

# =========================================================
# END INSERT
# =========================================================






##############

# ---------- 5) Multivariate inference ----------
dist_aitchison <- dist(X_clr)

set.seed(1)
perm <- vegan::adonis2(dist_aitchison ~ treatment, data = meta, permutations = 9999, by = "margin")
cat("\n--- PERMANOVA (Aitchison) ---\n")
print(perm)

# Dispersion check (important context for PERMANOVA)
bd <- vegan::betadisper(dist_aitchison, group = meta$treatment)
bd_perm <- permutest(bd, permutations = 9999)
cat("\n--- Dispersion test (betadisper) ---\n")
print(bd_perm)

# ---------- 6) Per-element differences (drivers of separation) ----------
# Do inference on CLR (log-ratio) values; adjust p-values (BH/FDR).
clr_long <- as.data.frame(X_clr) %>%
  rownames_to_column("sample_id") %>%
  left_join(meta, by = "sample_id") %>%
  pivot_longer(cols = -c(sample_id, tray_id, treatment, replicate),
               names_to = "element_symbol", values_to = "clr")

per_element <- clr_long %>%
  group_by(element_symbol) %>%
  summarise(
    mean_normal  = mean(clr[treatment == "Normal diet"]),
    mean_crushed = mean(clr[treatment == "Crushed diet"]),
    diff_crushed_minus_normal = mean_crushed - mean_normal,
    p_value = t.test(clr ~ treatment)$p.value,
    .groups = "drop"
  ) %>%
  mutate(p_adj = p.adjust(p_value, method = "BH")) %>%
  arrange(p_adj)

cat("\n--- Per-element Welch t-tests on CLR (BH adjusted) ---\n")
print(per_element)

# Plot the top-shifting elements
top_n <- 12
top_elements <- per_element %>% arrange(desc(abs(diff_crushed_minus_normal))) %>% slice_head(n = top_n) %>% pull(element_symbol)

p_top <- clr_long %>%
  filter(element_symbol %in% top_elements) %>%
  ggplot(aes(x = treatment, y = clr)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.08, alpha = 0.7) +
  facet_wrap(~ element_symbol, scales = "free_y") +
  labs(title = paste0("Top ", top_n, " elements by |mean CLR shift| (Crushed - Normal)"),
       x = NULL, y = "CLR value (log-ratio)") +
  theme_minimal(base_size = 12)
print(p_top)

# # ---------- 7) Save outputs ----------
# readr::write_csv(meta, "pisaster_T1T2_meta.csv")
# readr::write_csv(per_element, "pisaster_T1T2_per_element_CLR_tests.csv")
# readr::write_csv(rep_long, "pisaster_T1T2_replicate_means_long.csv")
# readr::write_csv(rep_wide, "pisaster_T1T2_replicate_means_wide.csv")

