# ==========================================================
# DESCRIPTIVE TABLES -> ONE EXCEL WORKBOOK (ALL + TRACE)
# Input:
#   - tray_elements_phylogeny.csv  (tray means + class + feeding_guild)
# Output:
#   - madreporite_descriptives_tables.xlsx (multiple sheets)
# ==========================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(readr)
  library(openxlsx)   # write multi-sheet Excel
})

setwd("~/Library/CloudStorage/OneDrive-UniversitedeMontreal/MSc_Thesis")

# ---- Paths (edit) ----
infile  <- "Data/processed/tray_elements_phylogeny.csv"
outfile <- "Data/processed/madreporite_descriptives_tables.xlsx"

# ---- Settings ----
exclude_trace <- c("Ca", "C", "O", "Na", "Cl")  # trace-only exclusion set
detect_thresh <- 0                              # present if mean_at > 0 (edit if desired)

# ---- Read ----
df <- readr::read_csv(infile, show_col_types = FALSE) %>%
  janitor::clean_names() 
  #%>%
  # select(-matches("^unnamed"))  # drops Unnamed: 0 etc.

# ---- Minimal checks (adjust req columns if your file differs) ----
req <- c("species", "tray_id", "element_symbol", "mean_at", "class", "feeding_guild_simple")
missing <- setdiff(req, names(df))
if (length(missing) > 0) stop("Missing required columns: ", paste(missing, collapse = ", "))

# Optional fields (only used if present)
has_common <- "common_species" %in% names(df)
has_nscans <- "n_scans" %in% names(df)

# Convenience: atomic %
df <- df %>% mutate(mean_at_pct = mean_at * 100)

df_all   <- df
#now filter out all elements but trace using exclude trace as the selector 
df_trace <- df %>% dplyr::filter(!element_symbol %in% exclude_trace)

# ==========================================================
# Helpers
# ==========================================================
element_stats <- function(d) {
  d %>%
    group_by(element_symbol) %>%
    summarise(
      min_at_pct  = min(mean_at_pct, na.rm = TRUE),
      max_at_pct  = max(mean_at_pct, na.rm = TRUE),
      range_at_pct = max(mean_at_pct, na.rm = TRUE) - min(mean_at_pct, na.rm = TRUE),
      mean_at_pct = mean(mean_at_pct, na.rm = TRUE),
      sd_at_pct   = sd(mean_at_pct, na.rm = TRUE),
      n_species   = n_distinct(species),
      .groups = "drop"
    ) %>%
    arrange(desc(range_at_pct))
}

element_extremes <- function(d) {
  d %>%
    group_by(element_symbol) %>%
    summarise(
      species_max = species[which.max(mean_at_pct)],
      max_at_pct  = max(mean_at_pct, na.rm = TRUE),
      species_min = species[which.min(mean_at_pct)],
      min_at_pct  = min(mean_at_pct, na.rm = TRUE),
      range_at_pct = max_at_pct - min_at_pct,
      .groups = "drop"
    ) %>%
    arrange(desc(range_at_pct))
}

element_prevalence <- function(d, thresh = 0) {
  total_species <- n_distinct(d$species)
  d %>%
    group_by(element_symbol) %>%
    summarise(
      n_species_detected = sum(mean_at > thresh, na.rm = TRUE),
      n_species_total = total_species,
      prevalence = n_species_detected / total_species,
      .groups = "drop"
    ) %>%
    arrange(desc(prevalence), element_symbol)
}

wide_matrix_pct <- function(d) {
  idx <- c("species", "class", "feeding_guild_simple", "tray_id")
  if (has_common) idx <- c(idx, "common_species")
  d %>%
    select(any_of(idx), element_symbol, mean_at_pct) %>%
    pivot_wider(names_from = element_symbol, values_from = mean_at_pct) %>%
    arrange(class, feeding_guild_simple, species)
}

# ==========================================================
# Build tables (ALL)
# ==========================================================
overview <- tibble(
  n_species = n_distinct(df_all$species),
  n_trays = n_distinct(df_all$tray_id),
  n_elements_all = n_distinct(df_all$element_symbol),
  n_elements_trace = n_distinct(df_trace$element_symbol),
  n_rows_all = nrow(df_all),
  n_rows_trace = nrow(df_trace)
)

# Replication (one row per tray/species)
rep_cols <- c("species", "tray_id", "class", "feeding_guild_simple")
if (has_common) rep_cols <- c(rep_cols, "common_species")
if (has_nscans) rep_cols <- c(rep_cols, "n_scans")

tray_replication <- df_all %>%
  distinct(across(all_of(rep_cols))) %>%
  arrange(class, feeding_guild_simple, species)

# Closure check at tray level (should sum to ~1 if built consistently)
tray_totals <- df_all %>%
  group_by(species, tray_id) %>%
  summarise(
    total_mean_at = sum(mean_at, na.rm = TRUE),
    total_mean_at_pct = sum(mean_at_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_mean_at))

# Group counts
counts_by_class <- tray_replication %>%
  count(class, name = "n_species") %>%
  arrange(desc(n_species))

counts_by_guild <- tray_replication %>%
  count(feeding_guild_simple, name = "n_species") %>%
  arrange(desc(n_species))



counts_class_guild <- tray_replication %>%
  count(class, feeding_guild_simple, name = "n_species") %>%
  arrange(class, feeding_guild_simple)



# Element summaries
elem_stats_all <- element_stats(df_all)
elem_ext_all   <- element_extremes(df_all)
elem_prev_all  <- element_prevalence(df_all, thresh = detect_thresh)

elem_stats_trace <- element_stats(df_trace)
elem_ext_trace   <- element_extremes(df_trace)
elem_prev_trace  <- element_prevalence(df_trace, thresh = detect_thresh)

# Species-level summaries (descriptive only; 1 tray per species in your design)
species_summary <- df_all %>%
  group_by(species, tray_id, class, feeding_guild_simple) %>%
  summarise(
    n_elements_all = n_distinct(element_symbol),
    n_elements_detected_all = sum(mean_at > detect_thresh, na.rm = TRUE),
    sum_all_pct = sum(mean_at_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    df_trace %>%
      group_by(species, tray_id) %>%
      summarise(
        n_elements_trace = n_distinct(element_symbol),
        n_elements_detected_trace = sum(mean_at > detect_thresh, na.rm = TRUE),
        sum_trace_pct = sum(mean_at_pct, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("species", "tray_id")
  ) %>%
  arrange(class, feeding_guild_simple, species)

# Top elements per species (by atomic %)
top5_all <- df_all %>%
  arrange(species, desc(mean_at_pct)) %>%
  group_by(species, tray_id, class, feeding_guild_simple) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  select(species, tray_id, class, feeding_guild_simple, element_symbol, mean_at_pct, any_of("n_scans"))

top5_trace <- df_trace %>%
  arrange(species, desc(mean_at_pct)) %>%
  group_by(species, tray_id, class, feeding_guild_simple) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  select(species, tray_id, class, feeding_guild_simple, element_symbol, mean_at_pct, any_of("n_scans"))

# Wide matrices
wide_all_pct   <- wide_matrix_pct(df_all)
wide_trace_pct <- wide_matrix_pct(df_trace)

# ==========================================================
# Write to one Excel workbook (multiple sheets)
# ==========================================================
wb <- createWorkbook()

add_sheet <- function(wb, sheet_name, dat) {
  addWorksheet(wb, sheet_name)
  writeDataTable(wb, sheet_name, dat, tableStyle = "TableStyleMedium9")
  freezePane(wb, sheet_name, firstRow = TRUE)
}

add_sheet(wb, "README", tibble(
  what = "Descriptive summaries of tray-level mean atomic concentrations (mean_at).",
  units = "mean_at is atomic fraction; mean_at_pct is atomic percent (mean_at*100).",
  trace_definition = "Trace excludes: C, O, Na, Cl, Ca.",
  note = "These tables are descriptive (no inferential statistics)."
))

add_sheet(wb, "Overview", overview)
add_sheet(wb, "Counts_by_Class", counts_by_class)
add_sheet(wb, "Counts_by_Guild", counts_by_guild)
add_sheet(wb, "Counts_Class_Guild", counts_class_guild)

add_sheet(wb, "Tray_Replication", tray_replication)
add_sheet(wb, "Tray_Totals", tray_totals)

add_sheet(wb, "ElemStats_All", elem_stats_all)
add_sheet(wb, "ElemExtremes_All", elem_ext_all)
add_sheet(wb, "ElemPrev_All", elem_prev_all)

add_sheet(wb, "ElemStats_Trace", elem_stats_trace)
add_sheet(wb, "ElemExtremes_Trace", elem_ext_trace)
add_sheet(wb, "ElemPrev_Trace", elem_prev_trace)

add_sheet(wb, "Species_Summary", species_summary)
add_sheet(wb, "Top5_All", top5_all)
add_sheet(wb, "Top5_Trace", top5_trace)

add_sheet(wb, "Wide_All_pct", wide_all_pct)
add_sheet(wb, "Wide_Trace_pct", wide_trace_pct)

saveWorkbook(wb, outfile, overwrite = TRUE)

message("Wrote workbook: ", outfile)




# ==========================================================
# CLASS-LEVEL DESCRIPTIVES (console output)
# Assumes you already have:
#   df with columns: species, class, element_symbol, mean_at
# ==========================================================

# Convert to atomic percent for easy reading
df2 <- df %>%
  mutate(mean_at_pct = mean_at * 100)

# Optional: restrict to trace elements only (comment out if you want all)
# exclude_trace <- c("Ca","C","O","Na","Cl")
# df2 <- df2 %>% filter(!element_symbol %in% exclude_trace)

# -----------------------------
# 1) Mean + spread of each element within each class
# -----------------------------
class_element_summary <- df2 %>%
  group_by(class, element_symbol) %>%
  summarise(
    n_species = n_distinct(species),
    mean_pct  = mean(mean_at_pct, na.rm = TRUE),
    sd_pct    = sd(mean_at_pct, na.rm = TRUE),
    min_pct   = min(mean_at_pct, na.rm = TRUE),
    max_pct   = max(mean_at_pct, na.rm = TRUE),
    range_pct = max_pct - min_pct,
    .groups = "drop"
  ) %>%
  arrange(class, desc(range_pct), desc(mean_pct))

print(class_element_summary, n = Inf)



#class wt_pct means 

class_element_summary_wt <- df2 %>% 
  group_by(class, element_symbol) %>%
  summarise(
    n_species = n_distinct(species),
    mean_pct  = mean(mean_wt, na.rm = TRUE),
                     .groups = "drop")%>%
      arrange(class, desc(mean_pct))
    
print(class_element_summary_wt, n = Inf)




# -----------------------------
# 2) Extremes: for each class × element, which species is max/min?
# -----------------------------
class_element_extremes <- df2 %>%
  group_by(class, element_symbol) %>%
  summarise(
    species_max = species[which.max(mean_at_pct)],
    max_pct     = max(mean_at_pct, na.rm = TRUE),
    species_min = species[which.min(mean_at_pct)],
    min_pct     = min(mean_at_pct, na.rm = TRUE),
    range_pct   = max_pct - min_pct,
    .groups = "drop"
  ) %>%
  arrange(class, desc(range_pct))

print(class_element_extremes, n = Inf)

# -----------------------------
# 3) Optional: "top variable elements" per class (largest ranges)
# -----------------------------
top_variable_by_class <- class_element_summary %>%
  group_by(class) %>%
  slice_max(range_pct, n = 5, with_ties = FALSE) %>%
  ungroup()

cat("\nTop 5 most variable elements (by range) within each class:\n")
print(top_variable_by_class, n = Inf)




##
##
##### now can turn to some inferential stats#####
##
##

################
# #I want to alter the feeding class structure within my tray_elements_phylogeny
# #currently have
# unique(df$feeding_guild)
# # 8 different types
# # but want to condensen into tighter groups 
# #make a new column
# #just take first two letter after the first "-"
# df <- df %>%
#   mutate(
#     feeding_guild_simple = str_extract(feeding_guild, "(?<=-)[^-]+")
#   )
# 

# unique(df$feeding_guild_simple)
# 

#list number of species occurences for reach feedingguildsimple
df %>%
  group_by(feeding_guild_simple) %>%
  summarise(
    n_species = n_distinct(species)
  )

write.csv(
  df,
  "Data/processed/tray_elements_phylogeny_simple.csv")



############################################################
############################################################
# THESIS-GRADE INFERENCE (CoDA-correct) for tray-level means
# Data: Data/processed/tray_elements_phylogeny.csv
#
# Strongest defensible inference now:
#  - test class and feeding_guild separately
#  - drop singleton levels (n<2) for the tested factor
#  - PERMANOVA on Aitchison distance + betadisper diagnostics
#  - element-wise clr follow-up with BH-FDR
############################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(readr)
  library(zCompositions)   # cmultRepl
  library(compositions)    # acomp, clr, ilr
  library(vegan)           # adonis2, betadisper
})

set.seed(20260305)

# -------------------------
# USER SETTINGS (EDIT)
# -------------------------
infile <- "Data/processed/tray_elements_phylogeny_simple.csv"

analysis_set <- "trace"  # "trace" or "all"
exclude_trace <- c("Ca","C","O","Na","Cl")

# Keep elements present (>0) in at least this fraction of trays
# (reduces domination by sparse trace elements)
parts_min_prevalence <- 0.1

# Minimum trays per level for inference
min_group_n <- 2

# Permutations for PERMANOVA / dispersion tests
n_perm <- 9999

# Which factors to test (separately)
factors_to_test <- c("class", "feeding_guild_simple")

# -------------------------
# LOAD + CHECK
# -------------------------
df_long <- readr::read_csv(infile, show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  dplyr::select(-matches("^unnamed"))

req <- c("tray_id","species","element_symbol","mean_at","class","feeding_guild_simple")
miss <- setdiff(req, names(df_long))
if (length(miss) > 0) stop("Missing required columns: ", paste(miss, collapse = ", "))

if (any(df_long$mean_at < 0, na.rm = TRUE)) stop("Negative mean_at values detected (invalid for CoDA).")

# Design sanity (tray↔species)
cat("\n=== Tray ↔ Species mapping ===\n")
print(df_long %>% distinct(tray_id, species) %>% summarise(
  n_trays = n_distinct(tray_id),
  n_species = n_distinct(species)
))

# -------------------------
# CORE FUNCTION
# -------------------------
run_coda_inference <- function(df_long, factor_name,
                               analysis_set = c("trace","all"),
                               exclude_trace = c("Ca","C","O","Na","Cl"),
                               parts_min_prevalence = 0.75,
                               min_group_n = 2,
                               n_perm = 9999) {

  analysis_set <- match.arg(analysis_set)

  if (!factor_name %in% names(df_long)) stop("factor_name not found: ", factor_name)

  # ---- 1) subset elements (all vs trace) ----
  df_use <- df_long
  if (analysis_set == "trace") {
    df_use <- df_use %>% dplyr::filter(!element_symbol %in% exclude_trace)
  }

  # ---- 2) build tray-level metadata for the factor ----
  meta <- df_use %>%
    distinct(tray_id, species, class, feeding_guild_simple)

  # drop singleton levels for THIS factor (strongest defensible step)
  lvl_counts <- meta %>% count(.data[[factor_name]], name = "n_trays") %>% arrange(desc(n_trays))
  keep_lvls <- lvl_counts %>% dplyr::filter(n_trays >= min_group_n) %>% pull(.data[[factor_name]])

  cat("\n--------------------------------------------\n")
  cat("Factor:", factor_name, " | analysis_set:", analysis_set, "\n")
  cat("Levels and tray counts:\n")
  print(lvl_counts)

  if (length(keep_lvls) < 2) {
    cat("STOP: After dropping singleton levels, <2 levels remain for inference.\n")
    return(invisible(NULL))
  }

  meta_inf <- meta %>% dplyr::filter(.data[[factor_name]] %in% keep_lvls)

  # Filter long data to retained trays
  df_inf <- df_use %>% dplyr::filter(tray_id %in% meta_inf$tray_id)

  # ---- 3) tray × element matrix (complete missing as 0) ----
  parts_all <- sort(unique(df_inf$element_symbol))
  mat_long <- df_inf %>%
    dplyr::select(tray_id, element_symbol, mean_at) %>%
    tidyr::complete(tray_id, element_symbol = parts_all, fill = list(mean_at = 0))

  # ---- 4) prevalence filter for parts ----
  prev <- mat_long %>%
    group_by(element_symbol) %>%
    summarise(
      n_trays = n_distinct(tray_id),
      n_present = sum(mean_at > 0),
      prevalence = n_present / n_trays,
      .groups = "drop"
    ) %>%
    arrange(desc(prevalence), element_symbol)

  parts_keep <- prev %>% dplyr::filter(prevalence >= parts_min_prevalence) %>% pull(element_symbol)

  cat("\nParts kept after prevalence >= ", parts_min_prevalence, ":\n", sep = "")
  print(parts_keep)

  if (length(parts_keep) < 3) {
    cat("STOP: Too few parts after prevalence filter. Lower parts_min_prevalence.\n")
    return(invisible(NULL))
  }

  mat_long <- mat_long %>% dplyr::filter(element_symbol %in% parts_keep)

  X <- mat_long %>%
    pivot_wider(names_from = element_symbol, values_from = mean_at) %>%
    arrange(tray_id)

  tray_ids <- X$tray_id
  X <- X %>% dplyr::select(-tray_id) %>% as.matrix()
  rownames(X) <- tray_ids

  # ---- 5) closure + zero replacement + clr ----
  rs <- rowSums(X)
  if (any(rs <= 0)) stop("Some trays have non-positive total across kept parts.")

  Xc <- X / rs  # closure to sum=1

  if (any(Xc == 0)) {
    Xc <- zCompositions::cmultRepl(Xc, method = "CZM")
  }
  if (any(Xc <= 0)) stop("Non-positive values remain after zero replacement.")

  X_ac <- compositions::acomp(Xc)
  df <- as.matrix(compositions::clr(X_ac))
  X_ilr <- as.matrix(compositions::ilr(X_ac))
  D_aitch <- dist(df)

  # Align metadata with matrix rows
  meta_inf <- meta_inf %>%
    dplyr::filter(tray_id %in% rownames(df)) %>%
    arrange(match(tray_id, rownames(df)))

  stopifnot(all(meta_inf$tray_id == rownames(df)))
  meta_inf[[factor_name]] <- as.factor(meta_inf[[factor_name]])

  # ---- 6) PERMANOVA (primary inference) ----
  form <- as.formula(paste("D_aitch ~", factor_name))

  permanova <- vegan::adonis2(
    form,
    data = meta_inf,
    permutations = n_perm,
    by = "margin"
  )

  cat("\n=== PERMANOVA (adonis2) on Aitchison distance ===\n")
  print(permanova)

  # ---- 7) Dispersion diagnostic (required) ----
  bd <- vegan::betadisper(D_aitch, group = meta_inf[[factor_name]])
  bd_test <- permutest(bd, permutations = n_perm)

  cat("\n=== betadisper (dispersion) permutation test ===\n")
  print(bd_test)

  # ---- 8) PCA (ilr) for visualization (not a test) ----
  pca <- prcomp(X_ilr, center = TRUE, scale. = FALSE)
  cat("\n=== PCA variance explained (first 5 PCs) ===\n")
  print(summary(pca)$importance[, 1:min(5, ncol(pca$x))])

  # ---- 9) Element-wise follow-up: clr(part) ~ factor ----
  clr_long <- as_tibble(df) %>%
    mutate(tray_id = rownames(df)) %>%
    pivot_longer(-tray_id, names_to = "part", values_to = "clr_value") %>%
    left_join(meta_inf %>% dplyr::select(tray_id, all_of(factor_name)), by = "tray_id")

  pvals <- clr_long %>%
    group_by(part) %>%
    group_modify(~{
      m <- lm(as.formula(paste("clr_value ~", factor_name)), data = .x)
      a <- anova(m)
      tibble(p_value = a$`Pr(>F)`[1])
    }) %>%
    ungroup() %>%
    mutate(p_adj_bh = p.adjust(p_value, method = "BH")) %>%
    arrange(p_adj_bh)

  cat("\n=== Element-wise clr models (BH-adjusted p-values) ===\n")
  print(pvals, n = Inf)

  # Return everything (so you can inspect objects)
  invisible(list(
    factor = factor_name,
    analysis_set = analysis_set,
    levels = lvl_counts,
    parts_prevalence = prev,
    parts_keep = parts_keep,
    permanova = permanova,
    betadisper = bd,
    betadisper_test = bd_test,
    pca = pca,
    elementwise_p = pvals
  ))
}

# -------------------------
# RUN (SEPARATELY) FOR EACH FACTOR
# -------------------------
results <- list()

for (f in factors_to_test) {
  results[[f]] <- run_coda_inference(
    df_long = df_long,
    factor_name = f,
    analysis_set = analysis_set,
    exclude_trace = exclude_trace,
    parts_min_prevalence = parts_min_prevalence,
    min_group_n = min_group_n,
    n_perm = n_perm
  )
}

#After running:
results$class$permanova
results$feeding_guild_simple$permanova
results$class$elementwise_p  #(BH-adjusted follow-up)
results$feeding_guild_simple$elementwise_p





library(dplyr)
library(ggplot2)
library(xlr)



### Create a CLR-transformed variable for Mg:S ratio


clr_df <- df_long %>%
  filter(element_symbol %in% c("Mg", "S")) %>%
  dplyr::select(tray_id, class, feeding_guild_simple, element_symbol, mean_at) %>%
  pivot_wider(names_from = element_symbol, values_from = mean_at) %>%
  mutate(Mg_S_ratio = Mg - S)

ggplot(clr_df, aes(x = class, y = Mg_S_ratio, fill = class)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  geom_jitter(width = 0.15, size = 2, alpha = 0.6) +
  labs(
    x = "Class",
    y = "CLR(Mg:S)",
    title = "Relative Mg:S composition across echinoderm classes"
  ) +
  theme_bw() +
  theme(legend.position = "none")

ggplot(clr_df, aes(x = feeding_guild_simple, y = Mg_S_ratio, fill = feeding_guild_simple)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  geom_jitter(width = 0.15, size = 2, alpha = 0.6) +
  labs(
    x = "Feeding guild",
    y = "CLR(Mg:S)",
    title = "Relative Mg:S composition across feeding guilds"
  ) +
  theme_bw() +
  theme(legend.position = "none")


## now redo above but with C:O ratio 
clr_df <- df_long %>%
  filter(element_symbol %in% c("C", "O")) %>%
  dplyr::select(tray_id, class, feeding_guild_simple, element_symbol, mean_at) %>%
  pivot_wider(names_from = element_symbol, values_from = mean_at) %>%
  mutate(C_O_ratio = C - O)
 
ggplot(clr_df, aes(x = class, y = C_O_ratio, fill = class)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  geom_jitter(width = 0.15, size = 2, alpha = 0.6) +
  labs(
    x = "Class",
    y = "CLR(C:O)",
    title = "Relative C:O composition across echinoderm classes"
  ) +
  theme_bw() +
  theme(legend.position = "none")

ggplot(clr_df, aes(x = feeding_guild_simple, y = C_O_ratio, fill = feeding_guild_simple)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  geom_jitter(width = 0.15, size = 2, alpha = 0.6) +
  labs(
    x = "Feeding guild",
    y = "CLR(C:O)",
    title = "Relative C:O composition across feeding guilds"
  ) +
  theme_bw() +
  theme(legend.position = "none")



## and as above but at species level
### this is the diagnostic plot to acknowledge that C:O ratios are off in Pisaster and Henricia 

clr_df <- df_long %>%
  filter(element_symbol %in% c("C", "O")) %>%
  dplyr::select(tray_id, species, feeding_guild_simple, element_symbol, mean_at) %>%
  pivot_wider(names_from = element_symbol, values_from = mean_at) %>%
  mutate(C_O_ratio = C - O)

ggplot(clr_df, aes(x = species, y = C_O_ratio, fill = species)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  geom_jitter(width = 0.15, size = 2, alpha = 0.6) +
  labs(
    x = "species",
    y = "CLR(C:O)",
    title = "Relative C:O composition across echinoderm species"
  ) +
  theme_bw() +
  theme(legend.position = "none")
#####

