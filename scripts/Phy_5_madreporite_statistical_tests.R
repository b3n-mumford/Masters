


# now run some statistical test
# will run both univariate and multivariate 
library(tidyverse)

setwd("~/Library/CloudStorage/OneDrive-UniversitedeMontreal/MSc_Thesis")

#want to add a few columns onto madreporite means dataframe so i can have class and feeding guild


madreporite_ossicle_means <- read.csv("Data/processed/madreporite_ossicle_element_means.csv")

#use the relational dataset in metadata called phylogeny to attach class and feeding guilds to species 

phylogeny <- read.csv("Data/metadata/species_phylogeny.csv")

#quick look at phylogeny data frame 
#number of unique feeding_guild codes
unique(phylogeny$feeding_guild)   #8
#number of unique feeding_guild codes
unique(phylogeny$feeding_guild_simple)   #5
#number of unique class codes
unique(phylogeny$class)          #5
# #number of species that contain a Pr in feedinguild
# phylogeny %>%
#   filter(grepl("Pr", feeding_guild)) %>%
#   select(species, feeding_guild)  #8
# #number of species that contain a De in feedinguild
# phylogeny %>%
#   filter(grepl("De", feeding_guild)) %>%
#   select(species, feeding_guild)  #3
# #number of species that contain a Su in feedinguild
# phylogeny %>%
#   filter(grepl("Su", feeding_guild)) %>%
#   select(species, feeding_guild)  #6
# #number of species that contain a EP in feedinguild
# phylogeny %>%
#   filter(grepl("EP", feeding_guild)) %>%
#   select(species, feeding_guild)  #6





# add class and feeding guild to madreporite_ossicle_means with columns from phylogeny, joining by species
madreporite_ossicle_means <- madreporite_ossicle_means %>%
  left_join(
    phylogeny %>%
      select(species, class, feeding_guild_simple),
    by = "species"
  )


write_csv(
  madreporite_ossicle_means ,
"Data/processed/madreporite_means_master.csv")



#### last few final checks

#i want to grou pall element values by species adn then total what all mean_at comes to, for each species
total_at_by_species <- madreporite_ossicle_means %>%
  group_by(species) %>%
  summarise(total_at = sum(mean_at, na.rm = TRUE))

#are some potential issues with totals reaching up to 150%
#identify those with total_at greater thatn 1.2 
total_at_by_species %>%
  filter(total_at > 1.2)
# species                    total_at
# <chr>                         <dbl>
#   1 Henricia leviuscula            1.49
# 2 Mesocentrotus franciscanus     1.22
# 3 Orthasterias koehleri          1.35
# 4 Pisaster ochraceus             1.44





###### Now can start analysis - first Started 21/01 can likey now ignore ####

madreporite_ossicle_means <- read.csv("Data/processed/madreporite_means_master.csv")

#first off univariate 
#one way anova 


#due to some elements only being present in one species have to do some nmanipulation 

testable_elements <- madreporite_ossicle_means %>%
  group_by(element) %>%
  filter(n_distinct(species) >= 2) %>%
  ungroup()



library(broom)

kw_results <- testable_elements %>%
  group_by(element) %>%
  do(
    tidy(
      kruskal.test(mean_wt ~ species, data = .)
    )
  )

kw_results

#for transpraency highlight what elements are excluded 
excluded_elements <- madreporite_ossicle_means %>%
  group_by(element) %>%
  summarise(
    n_species = n_distinct(species),
    .groups = "drop"
  ) %>%
  filter(n_species < 2)
excluded_elements




#now do multivariate 
library(vegan)


madreporite_wide <- read.csv("Data/processed/madreporite_ossicle_element_means_wide.csv")


#first have to check and manipulate the dataframe
colSums(is.na(madreporite_wide))

# due to almost all elements haveing some sort of NA value i will have to conitnue with Bray Curtis and converting NAs to 0 
chem_matrix <- madreporite_wide %>%
  select(-species, -tray_id) %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))



adonis2(
  chem_matrix ~ species,
  data = madreporite_wide,
  method = "bray"
)


#check dispersion 
dist_mat <- vegdist(chem_matrix, method = "bray")
disp <- betadisper(dist_mat, madreporite_wide$species)
anova(disp)









##########################################################################################

##########################################################################################


##########################################################################################

##########################################################################################









##########################################################################################

##########################################################################################


##########################################################################################

##########################################################################################


















##########################################################################################
### now run some statistical test #### starting 29/01 with help from CHATGPT
##########################################################################################
library(lme4)
library(vegan)



df <- madreporite_ossicle_means
df$log_wt <- log10(df$mean_wt + 1e-6)
log_wt ~ class + feeding_guild + (1 | species)


lm(log_wt ~ class + feeding_guild, data = subset(df, element == "Calcium"))






###create wide format matrix ######

mat <- df |>
  select(species, class, feeding_guild, element, log_wt) |>
  pivot_wider(names_from = element, values_from = log_wt)


#creationn of FG feeding guild code ######

mat <- mat %>%
  mutate(FG = str_extract(feeding_guild, "(?<=-)[A-Za-z]{2}"))



#define element columns

element_cols <- c("Aluminum","Calcium","Carbon","Chlorine","Magnesium",
                  "Oxygen","Sodium","Radium","Sulfur",
                  "Rhodium","Thorium")




# not sure why doing here

# Subset element matrix (just takes only elements)
X <- as.matrix(mat[, element_cols])

# Optional: scale the data so all elements are comparable
# X <- scale(X) 
#this just turns everything to an NA so will ignore

# Remove rows with NA values (PERMANOVA cannot handle NAs)
complete_rows <- complete.cases(X)
X <- X[complete_rows, ]
metadata <- mat[complete_rows, c("class","FG")]
    #these two line of code also end up turning it into 0's and nothing
    #to here ^^
















####### start of good stuff #######


# Make species a factor
df2$species <- as.factor(df2$species)
df2$class   <- as.factor(df2$class)
df2$FG      <- as.factor(df2$feeding_guild_simple)

# Summary stats safely
summary_stats <- df2 %>%
  group_by(element) %>%
  filter(n_distinct(species) > 1) %>%  # Only elements present in >1 species
  summarise(
    lm_species = list(summary(lm(mean_wt ~ species))),
    lm_class   = list(summary(lm(mean_wt ~ class))),
    lm_FG      = list(summary(lm(mean_wt ~ FG))),
    .groups = "drop"
  )


summary_stats



extract_stats <- function(lm_obj) {
  # check if lm_obj is a proper summary.lm
  if (inherits(lm_obj, "summary.lm")) {
    r2 <- lm_obj$r.squared
    # take the p-value of the first coefficient (usually the factor)
    pval <- if (nrow(coef(lm_obj)) >= 2) coef(lm_obj)[2, 4] else NA
    return(c(R2 = r2, p = pval))
  } else {
    return(c(R2 = NA, p = NA))
  }
}

summary_stats_clean <- summary_stats %>%
  mutate(
    species_R2 = map_dbl(lm_species, ~ extract_stats(.x)["R2"]),
    species_p  = map_dbl(lm_species, ~ extract_stats(.x)["p"]),
    class_R2   = map_dbl(lm_class,   ~ extract_stats(.x)["R2"]),
    class_p    = map_dbl(lm_class,   ~ extract_stats(.x)["p"]),
    FG_R2      = map_dbl(lm_FG,      ~ extract_stats(.x)["R2"]),
    FG_p       = map_dbl(lm_FG,      ~ extract_stats(.x)["p"])
  )  

summary_stats_clean <- summary_stats_clean %>%
  dplyr::select(
  element, species_R2, species_p, class_R2, class_p, FG_R2, FG_p)

summary_stats_clean


# 
# | Column       | Meaning                                                                       | How to interpret                                                                                                                                        |
#   | ------------ | ----------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------- |
#   | `element`    | The chemical element measured in the stereom                                  | E.g., Calcium, Carbon, Magnesium, etc.                                                                                                                  |
#   | `species_R2` | R² from a linear model predicting that element’s concentration by species     | 1.0 → **all variation in that element is explained by species**. R² = 1 usually happens if each species has only one measurement or perfect separation. |
#   | `species_p`  | p-value for the species effect                                                | NaN → the test is not valid, usually because there is only one observation per species, so p-value cannot be calculated.                                |
#   | `class_R2`   | R² from a linear model predicting element concentration by class              | E.g., Calcium has 0.339 → **34% of the variation in Calcium is explained by class**.                                                                    |
#   | `class_p`    | p-value for the class effect                                                  | Tells you whether the effect of class is statistically significant. E.g., 0.447 → **not significant**.                                                  |
#   | `FG_R2`      | R² from a linear model predicting element concentration by feeding guild (FG) | E.g., Magnesium 0.453 → **45% of variation in Mg is explained by feeding guild**.                                                                       |
#   | `FG_p`       | p-value for the feeding guild effect                                          | Tells you if the effect is significant. E.g., 0.00722 for Magnesium → **significant**.                                                                  |
#   




########### end of good stuff ##########










mat2 <- df2 |>
  select(species, class, FG, element, log_wt) |>
  pivot_wider(names_from = element, values_from = log_wt)








