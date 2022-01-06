#'---------------------------------------
#' Get Species at each site at each year
#'---------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(glue)
keen_dir <- "~/Dropbox (Byrnes Lab)/Byrnes Lab Shared Folder/KEEN Data Processing/Monitoring/cleaned_data/"

# load in quads, swath, fish, pointcount
quads <- read_csv(glue("{keen_dir}/keen_quads.csv"))

get_sp_df_transect <- function(a_df){
  a_df %>% 
    filter(GROUP %in% c("Algae","Fish","Invertebrate")) %>%
    group_by(SITE, YEAR, TRANSECT,
             GROUP,SPECIES) %>%
   slice(1L) %>%
    ungroup() %>%
    select(SITE, YEAR, TRANSECT, GROUP:SPECIES) %>%
    select(-SIZE)
}

files <- paste0(keen_dir, 
               c("keen_quads.csv",
                 "keen_swath.csv",
                 "keen_fish.csv",
                 "keen_cover.csv"
                 ))

sp_by_transect <- map(files, read_csv) %>%
  map_df(get_sp_df_transect) %>%
  group_by(SITE, YEAR, TRANSECT,
           GROUP,SPECIES) %>%
  slice(1L) %>%
  ungroup()

# load species list from web

sp_data <- readRDS("data/sp_data_ne_subtidal_worms_resolves.rds")


# make some changes
sp_by_transect <- sp_by_transect %>%
  mutate(SPECIES = case_when(
    SPECIES=="Boreochiton ruber" ~ "Ischnochiton ruber",
    SPECIES=="Crisularia turrita" ~ "Bugula turrita",
    SPECIES=="Dasysiphonia japonica" ~ "Heterosiphonia japonica",
    SPECIES=="Halichondria (Halichondria) panicea" ~ "Halichondria  panicea",
    SPECIES=="Euspira heros" ~ "Lunatia heros",
    SPECIES=="Schizomavella auriculata biaviculifera" ~ "Schizomavella auriculata",
    SPECIES=="Metridium senile pallidus" ~ "Metridium senile",
    SPECIES=="Pachycerianthus borealis" ~ "Cerianthus borealis",
    SPECIES=="Patinella verrucaria" ~ "Lichenopora verrucaria",
    SPECIES=="Porphyra" ~ "Porphyra spp.",
    SPECIES=="Lithophyllum" ~ "Lithophyllum spp.",
    SPECIES=="Sertularia" ~ "Sertularia sp.",
    SPECIES=="Testudinalia testudinalis" ~ "Tectura testudinalis",
    SPECIES=="Titanoderma" ~ "Titanoderma spp.",
    SPECIES=="Ulvaria" ~ "Ulvaria",
    SPECIES=="Sertularia" ~ "Sertularia sp.",
    SPECIES=="Sabella" ~ "Sabella sp.",
    SPECIES=="Zostera (Zostera) marina" ~ "Zostera marina",
    TRUE ~ SPECIES
  ))

#check
unique(sp_by_transect$SPECIES)[!(unique(sp_by_transect$SPECIES) %in% sp_data$valid_names)] %>% sort()

# expand aggregated species in data
ag_species <- sp_data$`Aggregated taxa` %>% na.omit() %>% unique()

add_expanded_sp <- function(adf){
  #do we need to expand anything?
  if(sum(adf$SPECIES %in% unique(ag_species))==0)
    return(adf$SPECIES)
  
  #make a df of taxa to add
  sp_to_add <- sp_data %>%
    filter(`Aggregated taxa` %in% adf$SPECIES) %>% 
    pull(valid_names) 
  
  c(adf$SPECIES, sp_to_add)
  
}

sp_by_transect_only <- sp_by_transect %>%
  group_by(SITE, YEAR, TRANSECT) %>%
  nest() %>%
  mutate(SPECIES = map(data, add_expanded_sp)) %>%
  select(-data) %>% #don't need
  unnest(SPECIES) %>%
  #get rid of dups
  group_by(SITE, YEAR, TRANSECT, SPECIES) %>%
  slice(1L) %>%
  ungroup()

# write species list by transect 
write_csv(sp_by_transect_only, "data/species_list_by_site_transect_year.csv")