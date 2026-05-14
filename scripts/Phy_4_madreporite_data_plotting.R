

#### PLOTTING ####

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)

setwd("~/Library/CloudStorage/OneDrive-UniversitedeMontreal/MSc_Thesis")


madreporite_ossicle_means <- read.csv("Data/processed/madreporite_ossicle_element_means.csv")

##very quick and temporary check of my data frames 

#whtin my dataframe i wan t to group by species and look at the mean of each element 
# across species to see if there are any weird outliers or patterns before i do the phylogenetic analyses.
# for each element show me the spread of values across species and the mean value for each species.

madreporite_ossicle_means %>%
  group_by(element) %>%
  summarise(
    min_at = min(mean_at, na.rm = TRUE),
    max_at = max(mean_at, na.rm = TRUE),
    range_at = max(mean_at, na.rm = TRUE) - min(mean_at, na.rm = TRUE),
    mean_at = mean(mean_at, na.rm = TRUE),
    sd_at = sd(mean_at, na.rm = TRUE),
    n_species = n()
  ) %>%
  arrange(desc(range_at))

madreporite_ossicle_means %>%
  group_by(element) %>%
  slice_max(mean_at, n = 1) %>%
  bind_rows(
    madreporite_ossicle_means %>%
      group_by(element) %>%
      slice_min(mean_at, n = 1)
  )

#basic check shows everyhting in order and as expected



########  Start getting ready for plotting ####


# reorder the facetting of species

madreporite_ossicle_means$species <- factor(
  madreporite_ossicle_means$species,
  levels = c(
    "Strongylocentrotus droebachiensis",
    "Strongylocentrotus purpuratus",
    "Mesocentrotus franciscanus",
    "Dendraster excentricus",
    "Ophiopholis aculeata",
    "Florometra serratissima",
    "Pycnopodia helianthoides",
    "Leptasterias hexactis",
    "Pisaster ochraceus",
    "Patiria miniata",
    "Henricia leviuscula",
    "Mediaster aequalis",
    "Stylasterias forreri",
    "Orthasterias koehleri",
    "Evasterias troschelii",
    "Dermasterias imbricata",
    "Chiridota albatrossii",
    "Parastichopus californicus",
    "Cucumaria miniata",
    "Cucumaria pallida",
    "Cucumaria piperata",
    "Psolus chitinoides",
    "Pentamera spp"
  )
)


ggplot(madreporite_ossicle_means,
       aes(x = element, y = mean_at, fill = element)) +
  geom_col(alpha = 0.8) +
  facet_wrap(~ species, scales = "free_y") +
  labs(
    x = "Element",
    y = "Weight percent (%)",
    fill = "Species"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )





##### needs a bit of work below here #####

# Now re-run but with no Ca, C, O to realign the axis 
madreporite_ossicle_means_noCaCO <- madreporite_ossicle_means %>%
  filter(
    !element %in% c("Calcium", "Carbon", "Oxygen")
  )


#running a few quick data checks. later to be commented out 
# madreporite_ossicle_means %>%
#   count(element) %>%
#   arrange(desc(n))
# 
# #see that oxygen is missing in three different species. 
# 
# #identify which species are mising oxygen 
# all_species <- madreporite_ossicle_means %>%
#   distinct(species) %>%
#   pull(species)
# 
# species_with_O <- madreporite_ossicle_means %>%
#   filter(element == "Oxygen") %>%
#   distinct(species) %>%
#   pull(species)
# 
# setdiff(all_species, species_with_O)

# species that are missing idnetified and will be checked again on SEM





#plot of species with no Ca C or O 

ggplot(madreporite_ossicle_means_noCaCO,
       aes(x = element, y = mean_at, fill = element)) +
  geom_col(alpha = 0.8) +
  facet_wrap(~ species, scales = "fixed") +
  labs(
    x = "Element",
    y = "Weight percent (%)",
    fill = "Species"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )


###notice very high Na and Cl in Meso franciscanus 


## may be will want to exclude Chlorine and Sodium from analyses too
madreporite_ossicle_means_noCaCONaCl <- madreporite_ossicle_means_noCaCO %>%
  filter(
    !element %in% c("Chlorine", "Sodium")
  )

ggplot(madreporite_ossicle_means_noCaCONaCl,
       aes(x = element, y = mean_at*100, fill = element)) +
  geom_col(alpha = 0.8) +
  facet_wrap(~ species, ncol=4, nrow =6, scales = "fixed") +
  labs(
    x = "Element",
    y = "Atmoic percent (%)",
    fill = "Species"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )+
  geom_hline(yintercept = 2,
             linetype = 2,
             colour = "red",
             linewidth = 0.3)


#want to shade by class 

phylogeny <- read.csv("Data/metadata/species_phylogeny.csv")
madreporite_ossicle_means_noCaCONaCl <- madreporite_ossicle_means_noCaCONaCl %>%
  left_join(
    phylogeny %>%
      select(species, class, feeding_guild),
    by = "species"
  )



shade_df <- madreporite_ossicle_means_noCaCONaCl %>%
  distinct(species, class)


madreporite_ossicle_means_noCaCONaCl$species <- factor(
  madreporite_ossicle_means_noCaCONaCl$species,
  levels = c(
    "Strongylocentrotus droebachiensis",
    "Strongylocentrotus purpuratus",
    "Mesocentrotus franciscanus",
    "Dendraster excentricus",
    "Ophiopholis aculeata",
    "Florometra serratissima",
    "Pycnopodia helianthoides",
    "Leptasterias hexactis",
    "Pisaster ochraceus",
    "Patiria miniata",
    "Henricia leviuscula",
    "Mediaster aequalis",
    "Stylasterias forreri",
    "Orthasterias koehleri",
    "Evasterias troschelii",
    "Dermasterias imbricata",
    "Chiridota albatrossii",
    "Parastichopus californicus",
    "Cucumaria miniata",
    "Cucumaria pallida",
    "Cucumaria piperata",
    "Psolus chitinoides",
    "Pentamera spp"
  )
)

shade_df$species <- factor(
 shade_df$species,
  levels = c(
    "Strongylocentrotus droebachiensis",
    "Strongylocentrotus purpuratus",
    "Mesocentrotus franciscanus",
    "Dendraster excentricus",
    "Ophiopholis aculeata",
    "Florometra serratissima",
    "Pycnopodia helianthoides",
    "Leptasterias hexactis",
    "Pisaster ochraceus",
    "Patiria miniata",
    "Henricia leviuscula",
    "Mediaster aequalis",
    "Stylasterias forreri",
    "Orthasterias koehleri",
    "Evasterias troschelii",
    "Dermasterias imbricata",
    "Chiridota albatrossii",
    "Parastichopus californicus",
    "Cucumaria miniata",
    "Cucumaria pallida",
    "Cucumaria piperata",
    "Psolus chitinoides",
    "Pentamera spp"
  )
)


# 
# ggplot(madreporite_ossicle_means_noCaCONaCl,
#        aes(x = element, y = mean_at*100)) +
#   geom_rect(data = shade_df,
#             aes(xmin = -Inf, xmax = Inf,
#                 ymin = -Inf, ymax = Inf,
#                 fill = class),
#             inherit.aes = FALSE,
#             alpha = 0.15) +
#   geom_col(aes(fill = element), alpha = 0.8) +
#   facet_wrap(~ species, nrow = 6, ncol = 4) +
#   theme_bw() +
#   theme(
#     axis.text.x = element_text(angle = 90, hjust = 1)
#   )
# 

library(ggnewscale)


element_lookup <- c(
  "Aluminum"  = "Al",
  "Magnesium" = "Mg",
  "Radium"    = "Ra",
  "Rhodium"   = "Rh",
  "Silicon"   = "Si",
  "Sulfur"    = "S",
  "Thorium"   = "Th"
)

madreporite_ossicle_means_noCaCONaCl$element_symbol <-
  element_lookup[madreporite_ossicle_means_noCaCONaCl$element]

ggplot(madreporite_ossicle_means_noCaCONaCl,
       aes(x = element_symbol, y = mean_at*100)) +
  geom_rect(data = shade_df,
            aes(xmin = -Inf, xmax = Inf,
                ymin = -Inf, ymax = Inf,
                fill = class),
            inherit.aes = FALSE,
            alpha = 0.15) +
  scale_fill_brewer(palette = "Dark2", name = "Class") +
  new_scale_fill() +
  geom_col(aes(fill = element), alpha = 0.8, show.legend = FALSE) +
  geom_hline(yintercept = 2,
             linetype = 2,
             colour = "red",
             linewidth = 0.3) +
  facet_wrap(~ species, nrow = 6, ncol = 4) +
  theme_bw()+
  labs(
    x = "Element",
    y = "Atomic percent (%)") 


#and without red line 

ggplot(madreporite_ossicle_means_noCaCONaCl,
       aes(x = element_symbol, y = mean_at*100)) +
  geom_rect(data = shade_df,
            aes(xmin = -Inf, xmax = Inf,
                ymin = -Inf, ymax = Inf,
                fill = class),
            inherit.aes = FALSE,
            alpha = 0.15) +
  scale_fill_brewer(palette = "Dark2", name = "Class") +
  new_scale_fill() +
  geom_col(aes(fill = element), alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ species, nrow = 6, ncol = 4) +
  theme_bw()+
  labs(
    x = "Element",
    y = "Atomic percent (%)") 














##### this is just a quick fix for the poster #####
# 
# 
# #add in pentacrinoid data
# pentacrinoid <-read_csv("Data/raw/SEM/pentacrinoid_element.csv")
# 
# #pivot into long format
# pentacrinoid_long <- pentacrinoid %>%
#   pivot_longer(cols = c( "Calcium", "Carbon", "Phosphorous", "Magnesium",
#                         "Oxygen", "Sodium"
#                         ),
#                names_to = "element",
#                values_to = "mean_at")
# 
# #average each element
# pentacrinoid_means <- pentacrinoid_long %>%
#   group_by(element) %>%
#   summarise(mean_at = mean(mean_at, na.rm = TRUE))
# 
# 
# #add onto bottom of madreporite_ossicle_means_noCaCONaCl
# pentacrinoid_means <- pentacrinoid_means %>%
#   mutate(species = "Pentacrinoid (Juvenile Florometra)",
#          class = "Crinoidea")
# pentacrinoid_noCaCONaCl <- pentacrinoid_means %>%
#   filter(
#     !element %in% c("Calcium", "Carbon", "Oxygen", "Sodium")
#   )
# 
# madreporite_ossicle_means_noCaCONaCl <- madreporite_ossicle_means_noCaCONaCl %>%
#   bind_rows(pentacrinoid_noCaCONaCl)
# 
# #divide the mean_at by 100 to convert to atomic percent only for pentacrinoid
# madreporite_ossicle_means_noCaCONaCl <- madreporite_ossicle_means_noCaCONaCl %>%
#   mutate(mean_at = ifelse(species == "Pentacrinoid (Juvenile Florometra)", mean_at / 100, mean_at))
# 
# 
# 
# #write element symbol column
# 
# element_lookup <- c(
#   "Aluminum"  = "Al",
#   "Magnesium" = "Mg",
#   "Radium"    = "Ra",
#   "Rhodium"   = "Rh",
#   "Silicon"   = "Si",
#   "Sulfur"    = "S",
#   "Thorium"   = "Th",
#   "Phosphorous" = "P"
# )
# 
# 
# madreporite_ossicle_means_noCaCONaCl$element_symbol <-
#   element_lookup[madreporite_ossicle_means_noCaCONaCl$element]
# 
# madreporite_ossicle_means_noCaCONaCl$species <- factor(
#   madreporite_ossicle_means_noCaCONaCl$species,
#   levels = c(
#     "Strongylocentrotus droebachiensis",
#     "Strongylocentrotus purpuratus",
#     "Mesocentrotus franciscanus",
#     "Dendraster excentricus",
#     "Ophiopholis aculeata",
#     "Florometra serratissima",
#     "Pycnopodia helianthoides",
#     "Leptasterias hexactis",
#     "Pisaster ochraceus",
#     "Patiria miniata",
#     "Henricia leviuscula",
#     "Mediaster aequalis",
#     "Stylasterias forreri",
#     "Orthasterias koehleri",
#     "Evasterias troschelii",
#     "Dermasterias imbricata",
#     "Chiridota albatrossii",
#     "Parastichopus californicus",
#     "Cucumaria miniata",
#     "Cucumaria pallida",
#     "Cucumaria piperata",
#     "Psolus spp",
#     "Pentamera spp",
#     "Pentacrinoid (Juvenile Florometra)"
#   )
# )
# 
# 
# shade_df <- madreporite_ossicle_means_noCaCONaCl %>%
#   distinct(species, class)
# 
# shade_df$species <- factor(
#   shade_df$species,
#   levels = c(
#     "Strongylocentrotus droebachiensis",
#     "Strongylocentrotus purpuratus",
#     "Mesocentrotus franciscanus",
#     "Dendraster excentricus",
#     "Ophiopholis aculeata",
#     "Florometra serratissima",
#     "Pycnopodia helianthoides",
#     "Leptasterias hexactis",
#     "Pisaster ochraceus",
#     "Patiria miniata",
#     "Henricia leviuscula",
#     "Mediaster aequalis",
#     "Stylasterias forreri",
#     "Orthasterias koehleri",
#     "Evasterias troschelii",
#     "Dermasterias imbricata",
#     "Chiridota albatrossii",
#     "Parastichopus californicus",
#     "Cucumaria miniata",
#     "Cucumaria pallida",
#     "Cucumaria piperata",
#     "Psolus spp",
#     "Pentamera spp",
#     "Pentacrinoid (Juvenile Florometra)"
# 
#   )
# )
# 
# 
# ggplot(madreporite_ossicle_means_noCaCONaCl,
#        aes(x = element_symbol, y = mean_at*100)) +
#   geom_rect(data = shade_df,
#             aes(xmin = -Inf, xmax = Inf,
#                 ymin = -Inf, ymax = Inf,
#                 fill = class),
#             inherit.aes = FALSE,
#             alpha = 0.15) +
#   scale_fill_brewer(palette = "Dark2", name = "Class") +
#   new_scale_fill() +
#   geom_col(aes(fill = element), alpha = 0.8, show.legend = FALSE)  +
#   facet_wrap(~ species, nrow = 6, ncol = 4) +
#   theme_bw()+
#   labs(
#     x = "Element",
#     y = "Atomic percent (%)") 
# 
# 
# 
