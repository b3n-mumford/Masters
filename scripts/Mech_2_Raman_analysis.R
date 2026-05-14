library(dplyr)
library(ggplot2)

setwd("~/Library/CloudStorage/OneDrive-UniversitedeMontreal/MSc_Thesis")
data <- read.csv("data/processed/biomechanical_raman_data.csv")
biomechanical_raman_data <- read.csv("data/processed/biomechanical_raman_data.csv")
biomechanical_raman_data <- biomechanical_raman_data %>%
  filter(ID != "Box2-S1")

write.csv(biomechanical_raman_data, "data/processed/biomechanical_raman_data.csv", row.names = FALSE)

biomechanical_raman_data <- read.csv("data/processed/biomechanical_raman_data.csv")



#### FIRST LOOK ####


#ggplot for each ID plot a spectra with intenisty on y axis and wavenumber on x axis

ggplot(data, aes(x = wave_number, y = intensity, color = as.factor(ID))) +
  geom_line() +
  labs(title = "Raman Spectra by Sample ID",
       x = "Wavenumber (cm⁻¹)",
       y = "Intensity (a.u.)",
       color = "Sample ID") +
  theme_minimal()


### clearly Box2-S1 is not quality data
data <- data %>%
  filter(ID != "Box2-S1")
#ggplot again without Box2-S1
ggplot(data, aes(x = wave_number, y = intensity, color = as.factor(ID))) +
  geom_line() +
  labs(title = "Raman Spectra by Sample ID (Without Box2-S1)",
       x = "Wavenumber (cm⁻¹)",
       y = "Intensity (a.u.)",
       color = "Sample ID") +
  theme_minimal()


#####now identify where the peaks are in the  spectra for each box
peak_data <- data %>%
  group_by(ID) %>%
  filter(intensity == max(intensity)) %>%
  ungroup()

# give the range of the peak wavenumbers
range(peak_data$wave_number)
# [1] 1084.773 1088.514



#identification of secondary intensity peak (<1000)
secondary_peak_data <- data %>%
  group_by(ID) %>%
  filter(wave_number >= 0 & wave_number <= 1000) %>%
  filter(intensity == max(intensity)) %>%
  ungroup()

# give the range of the secondary peak wavenumbers
range(secondary_peak_data$wave_number)
# [1] 280.3125 284.4238


#### Now plot with vertical lines at peak positions ####
ggplot(data, aes(x = wave_number, y = intensity, color = as.factor(ID))) +
  geom_line() +
  geom_vline(xintercept = c(1086, 282), linetype = "dashed", color = "black") +
  labs(title = "Raman Spectra by Sample ID with Peak Positions",
       x = "Wavenumber (cm⁻¹)",
       y = "Intensity (a.u.)",
       color = "Sample ID") +
  theme_minimal()






##########
#### SECOND TRY ######
##########

# ============================================================
# Raman CaCO3 polymorph analysis from one CSV file
# Input file format:
#   ID, wave_number, intensity
# ============================================================

# -----------------------------
# 1. Packages
# -----------------------------
required_packages <- c(
  "tidyverse",
  "signal",
  "pracma",
  "baseline",
  "readr"
)

new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if(length(new_packages) > 0) install.packages(new_packages)

library(tidyverse)
library(signal)
library(pracma)
library(baseline)
library(readr)

# -----------------------------
# 2. User settings
# -----------------------------
input_file <- "Data/processed/biomechanical_raman_data.csv"   # change path if needed
output_dir <- "Data/processed/raman_polymorph_output"
dir.create(output_dir, showWarnings = FALSE)

# Analysis region focused on calcium carbonate polymorph bands
x_min <- 100
x_max <- 1200

# Savitzky-Golay smoothing parameters
sgolay_p <- 3
sgolay_n <- 11   # must be odd

# Peak detection settings
peak_tolerance <- 8
min_peak_height_quantile <- 0.93
min_peak_distance_pts <- 6

# Diagnostic Raman bands (cm^-1)
diagnostic_bands <- list(
  calcite   = c(156, 281, 711, 1085),
  aragonite = c(153, 206, 705, 1085),
  vaterite  = c(107, 267, 301, 738, 750, 1089)
)

# -----------------------------
# 3. Helper functions
# -----------------------------
normalize01 <- function(y) {
  rng <- range(y, na.rm = TRUE)
  if(diff(rng) == 0) return(rep(0, length(y)))
  (y - rng[1]) / diff(rng)
}

baseline_correct <- function(y) {
  y <- as.numeric(y)
  y <- y - min(y, na.rm = TRUE)
  y[y < 0] <- 0
  return(y)
}


detect_peaks_df <- function(df, sample_name,
                            min_peak_height_quantile = 0.93,
                            min_peak_distance_pts = 6) {
  
  y <- df$intensity_processed
  x <- df$wave_number
  
  mph <- quantile(y, min_peak_height_quantile, na.rm = TRUE)
  
  pk <- pracma::findpeaks(
    y,
    minpeakheight = mph,
    minpeakdistance = min_peak_distance_pts
  )
  
  if(is.null(pk)) {
    return(tibble(
      ID = character(),
      peak_shift = numeric(),
      peak_height = numeric()
    ))
  }
  
  tibble(
    ID = sample_name,
    peak_shift = x[pk[, 2]],
    peak_height = pk[, 1]
  ) %>%
    arrange(peak_shift)
}

match_bands <- function(peaks, target_bands, tolerance = 8) {
  sapply(target_bands, function(band) {
    any(abs(peaks - band) <= tolerance)
  })
}

score_polymorphs <- function(peak_positions, bands_list, tolerance = 8) {
  tibble(
    polymorph = names(bands_list),
    n_matched = sapply(bands_list, function(bands) {
      sum(match_bands(peak_positions, bands, tolerance))
    }),
    n_total = sapply(bands_list, length)
  ) %>%
    mutate(score = n_matched / n_total) %>%
    arrange(desc(score), desc(n_matched))
}

classify_spectrum <- function(peak_positions, bands_list, tolerance = 8) {
  
  scores <- score_polymorphs(peak_positions, bands_list, tolerance)
  
  best_polymorph <- scores$polymorph[1]
  best_score <- scores$score[1]
  second_score <- scores$score[2]
  
  # Extra logic: require the carbonate band near 1085-1089
  has_main_carbonate <- any(abs(peak_positions - 1085) <= tolerance) |
    any(abs(peak_positions - 1089) <= tolerance)
  
  if(!has_main_carbonate) {
    return(list(classification = "uncertain_no_main_carbonate_band", scores = scores))
  }
  
  if(best_score < 0.5) {
    return(list(classification = "uncertain", scores = scores))
  }
  
  if((best_score - second_score) < 0.2 && second_score >= 0.4) {
    return(list(classification = "mixed_or_ambiguous", scores = scores))
  }
  
  return(list(classification = best_polymorph, scores = scores))
}

# -----------------------------
# 4. Read and clean data
# -----------------------------
# -----------------------------
# 4. Read and clean data
# -----------------------------
# -----------------------------
# 4. Read and clean data
# -----------------------------
raman_raw <- read_csv(input_file, show_col_types = FALSE)

# standardize names
names(raman_raw) <- names(raman_raw) |>
  trimws() |>
  tolower() |>
  gsub("[^a-z0-9]+", "_", x = _)

print(names(raman_raw))

# identify columns
id_col <- names(raman_raw)[grepl("^id$|sample", names(raman_raw))]
wave_col <- names(raman_raw)[grepl("wave|wavenumber|raman_shift|shift", names(raman_raw))]
int_col <- names(raman_raw)[grepl("^intensity$|counts|count", names(raman_raw))]

print(id_col)
print(wave_col)
print(int_col)

if (length(id_col) == 0) stop("No ID/sample column found.")
if (length(wave_col) == 0) stop("No wave-number column found.")
if (length(int_col) == 0) stop("No intensity column found.")

# rename outside the main pipeline
names(raman_raw)[names(raman_raw) == id_col[1]] <- "ID"
names(raman_raw)[names(raman_raw) == wave_col[1]] <- "wave_number"
names(raman_raw)[names(raman_raw) == int_col[1]] <- "intensity"

print(names(raman_raw))

# coerce and filter
raman_raw$wave_number <- as.numeric(raman_raw$wave_number)
raman_raw$intensity <- as.numeric(raman_raw$intensity)

raman_raw <- raman_raw[!is.na(raman_raw$ID) &
                         !is.na(raman_raw$wave_number) &
                         !is.na(raman_raw$intensity), ]

raman_raw <- raman_raw[raman_raw$wave_number >= x_min &
                         raman_raw$wave_number <= x_max, ]

# reorder each spectrum low -> high
raman_raw <- raman_raw %>%
  group_by(ID) %>%
  arrange(wave_number, .by_group = TRUE) %>%
  ungroup()


# -----------------------------
# 5. Process each spectrum
# -----------------------------
spectra_processed <- raman_raw %>%
  group_by(ID) %>%
  group_modify(~{
    df <- .x
    
    y_smooth <- signal::sgolayfilt(df$intensity, p = sgolay_p, n = sgolay_n)
    
    # simple baseline correction
    y_corr <- y_smooth - min(y_smooth, na.rm = TRUE)
    y_corr[y_corr < 0] <- 0
    
    y_norm <- normalize01(y_corr)
    
    df$intensity_smooth <- y_smooth
    df$intensity_corrected <- y_corr
    df$intensity_processed <- y_norm
    
    df
  }) %>%
  ungroup()

# -----------------------------
# 6. Detect peaks for each sample
# -----------------------------
peaks_all <- spectra_processed %>%
  group_split(ID) %>%
  purrr::map_dfr(~ detect_peaks_df(
    .x,
    sample_name = unique(.x$ID),
    min_peak_height_quantile = min_peak_height_quantile,
    min_peak_distance_pts = min_peak_distance_pts
  ))

# Add processed intensity at peak positions for plotting
peak_plot_df <- peaks_all %>%
  left_join(
    spectra_processed %>%
      select(ID, wave_number, intensity_processed),
    by = c("ID", "peak_shift" = "wave_number")
  )

# -----------------------------
# 7. Classify each sample
# -----------------------------
sample_ids <- unique(spectra_processed$ID)

classification_results <- purrr::map_dfr(sample_ids, function(samp) {
  
  peak_positions <- peaks_all %>%
    dplyr::filter(ID == samp) %>%
    dplyr::pull(peak_shift)
  
  result <- classify_spectrum(
    peak_positions = peak_positions,
    bands_list = diagnostic_bands,
    tolerance = peak_tolerance
  )
  
  score_tbl <- result$scores %>%
    select(polymorph, score) %>%
    pivot_wider(
      names_from = polymorph,
      values_from = score,
      names_prefix = "score_"
    )
  
  tibble(
    ID = samp,
    classification = result$classification,
    n_detected_peaks = length(peak_positions)
  ) %>%
    bind_cols(score_tbl)
})

# -----------------------------
# 8. Export tables
# -----------------------------
write_csv(spectra_processed, file.path(output_dir, "processed_spectra.csv"))
write_csv(peaks_all, file.path(output_dir, "detected_peaks.csv"))
write_csv(classification_results, file.path(output_dir, "polymorph_classification_summary.csv"))


# -----------------------------
# 9. Plot individual spectra
# -----------------------------
for (samp in sample_ids) {
  
  df_plot <- spectra_processed %>%
    dplyr::filter(ID == samp)
  
  pk_plot <- peak_plot_df %>%
    dplyr::filter(ID == samp)
  
  p <- ggplot(df_plot, aes(x = wave_number, y = intensity_processed)) +
    geom_line(linewidth = 0.5) +
    geom_point(
      data = pk_plot,
      aes(x = peak_shift, y = intensity_processed),
      colour = "red",
      size = 2,
      inherit.aes = FALSE
    ) +
    geom_vline(xintercept = diagnostic_bands$calcite, linetype = "dashed", alpha = 0.3) +
    geom_vline(xintercept = diagnostic_bands$aragonite, linetype = "dotted", alpha = 0.3) +
    geom_vline(xintercept = diagnostic_bands$vaterite, linetype = "dotdash", alpha = 0.3) +
    labs(
      title = paste("Processed Raman spectrum:", samp),
      x = expression("Raman shift (cm"^{-1}*")"),
      y = "Normalized corrected intensity"
    ) +
    theme_bw()
  
  ggsave(
    filename = file.path(output_dir, paste0(samp, "_spectrum.png")),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
}

# -----------------------------
# 10. Overlay plot
# -----------------------------
p_overlay <- ggplot(spectra_processed, aes(x = wave_number, y = intensity_processed, colour = ID)) +
  geom_line(linewidth = 0.5, alpha = 0.8) +
  labs(
    title = "Overlay of processed Raman spectra",
    x = expression("Raman shift (cm"^{-1}*")"),
    y = "Normalized corrected intensity"
  ) +
  theme_bw()

ggsave(
  file.path(output_dir, "overlay_processed_spectra.png"),
  p_overlay,
  width = 10,
  height = 6,
  dpi = 300
)

# -----------------------------
# 11. Faceted plot
# -----------------------------
p_facet <- ggplot(spectra_processed, aes(x = wave_number, y = intensity_processed)) +
  geom_line(linewidth = 0.4) +
  facet_wrap(~ID, scales = "free_y") +
  labs(
    title = "Processed Raman spectra by sample",
    x = expression("Raman shift (cm"^{-1}*")"),
    y = "Normalized corrected intensity"
  ) +
  theme_bw()

ggsave(
  file.path(output_dir, "faceted_processed_spectra.png"),
  p_facet,
  width = 12,
  height = 10,
  dpi = 300
)

# -----------------------------
# 12. Print classification
# -----------------------------
print(classification_results)

cat("\nDone.\n")
cat("Files written to:", output_dir, "\n")
