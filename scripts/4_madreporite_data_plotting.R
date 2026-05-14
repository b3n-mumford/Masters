

#### PLOTTING ####

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)


madreporite_ossicle_means <- read.csv("Data/processed/madreporite_ossicle_element_means.csv")


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
    "Psolus spp",
    "Pentamera spp"
  )
)


ggplot(madreporite_ossicle_means,
       aes(x = element, y = mean_wt, fill = element)) +
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
madreporite_ossicle_means %>%
  count(element) %>%
  arrange(desc(n))

#see that oxygen is missing in three different species. 

#identify which species are mising oxygen 
all_species <- madreporite_ossicle_means %>%
  distinct(species) %>%
  pull(species)

species_with_O <- madreporite_ossicle_means %>%
  filter(element == "Oxygen") %>%
  distinct(species) %>%
  pull(species)

setdiff(all_species, species_with_O)

# species that are missing idnetified and will be checked again on SEM





#plot of species with no Ca C or O 

ggplot(madreporite_ossicle_means_noCaCO,
       aes(x = element, y = mean_wt, fill = element)) +
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





## may be will want to exclude Chlorine and Sodium from analyses too
madreporite_ossicle_means_noCaCONaCl <- madreporite_ossicle_means_noCaCO %>%
  filter(
    !element %in% c("Chlorine", "Sodium")
  )

ggplot(madreporite_ossicle_means_noCaCONaCl,
       aes(x = element, y = mean_wt, fill = element)) +
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




#now facet by species
#will also want to do some sort of statistical tests to see if is variation in other elements between species (excl CaCO + NaCl?)

