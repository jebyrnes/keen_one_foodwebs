#----------------------------
# Load interaction data
#----------------------------

# get googlesheet info
library(googlesheets4)
library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(rglobi)
library(worrms)

gs4_auth()

int_dat <- read_sheet("https://docs.google.com/spreadsheets/d/1f4a8mYrov0DiRBOoP1lbuEh_kxhQP6t7Ay6fH5q5gME/", 
                      sheet = "Working Interaction Data")
int_dat_2 <- read_sheet("https://docs.google.com/spreadsheets/d/16PwO_TI_YnSktYBolK6MPuuvyhEu7xZy7j0jplmz5sg/edit#gid=1883520352",
                        sheet = "Interaction Data")

int_dat <- bind_rows(int_dat, int_dat_2)
int_dat <- int_dat %>%
  mutate(`Ref ID` = map_chr(`Ref ID`, ~ifelse(is.null(.x), "", .x)),
         `Paper ID` = map_chr(`Paper ID`, ~ifelse(is.null(.x), "", .x)),
         observationDateTime = map_chr(observationDateTime, ~ifelse(is.null(.x), "", .x)),
  )

write_csv(int_dat, "./data/entered_int_data.csv")

# species for interactions
sp_data <- read_sheet("https://docs.google.com/spreadsheets/d/1f4a8mYrov0DiRBOoP1lbuEh_kxhQP6t7Ay6fH5q5gME/", 
                      sheet = "Species List")

sp_data2 <- read_sheet("https://docs.google.com/spreadsheets/d/16PwO_TI_YnSktYBolK6MPuuvyhEu7xZy7j0jplmz5sg/edit#gid=19019573", 
                      sheet = "Species List") %>%
  mutate(`Done = 1` = map_dbl(`Done = 1`, ~ifelse(is.null(.x), NA, .x) %>% as.numeric))

sp_data <- bind_rows(sp_data, sp_data2)

write_csv(sp_data, "./data/entered_species_data.csv")



# get globi data


sp_data_ne_subtidal <- sp_data %>%
  filter(is.na(`Added as new agents`),
         `Taxonomic Level`=="Species")

# get worms synonyms for getting globi data
get_valid_name <- function(.x){
  print(.x)
  
  id <- get_id(.x)
  if(is.na(id)){return(.x)}
  
  wm_record(id) %>%
    pull(valid_name) %>%
    `[`(1)
}

#deprecated
get_ids <- function(.x){
  id <- try(wm_name2id(.x))
  if(class(id) == "try-error"){return(NA)}
  id
}


get_id <- function(.x){
  
  rec <- try(wm_records_name(.x))
  if(class(rec)[1] == "try-error"){return(NA)}
  
  rec %>%
    filter(status=="accepted") %>%
    pull(AphiaID) %>%
    `[`(1)
}

get_synonyms <- function(.x){
  print(.x)
  if(is.na(.x)) return(NA)
  
  syn <- try(wm_synonyms(.x))
  if(class(syn) == "try-error"){return(.x)}
  
  syn %>% pull(scientificname)
  
}
# valid_names <- map_chr(sp_data_ne_subtidal$sourceTaxonName,
#                        get_valid_name)
# 
# ids <- map_int(valid_names, get_ids) %>%
#   na.omit
# synonyms <- map(ids, 
#                 ~wm_synonyms(.x) %>% pull(scientificname)) %>%
#   unlist()

sp_data_ne_subtidal_2 <- sp_data_ne_subtidal %>%
  mutate(valid_names = map_chr(sourceTaxonName, get_valid_name),
         ids = map_int(valid_names, get_id),
         synonyms = map(ids, ~get_synonyms(.x)) )

#save it for the future!
saveRDS(sp_data_ne_subtidal_2, "data/sp_data_ne_subtidal_worms_resolves.rds")
#sp_data_ne_subtidal_2 <- readRDS("data/sp_data_ne_subtidal_worms_resolves.rds")

#make a vector of species to search on
sp_to_search <- c(sp_data_ne_subtidal_2$sourceTaxonName,
                  sp_data_ne_subtidal_2$valid_names#,
                  # sp_data_ne_subtidal_2$synonyms%>%unlist(),
                  # sp_data_ne_subtidal_2$synonyms %>%
                  #   unlist() %>%
                  #   stringr::str_remove(" var\\..*") %>%
                  #   stringr::str_remove(" f\\..*")
                  ) %>%
  unique()

#function to be kind to the API
get_interactions_sleepy <- function(.x, pause = 1, ...){
  Sys.sleep(pause)
  get_interactions(.x, ...)
}

#get interactions for these species from GLOBI
globi_ints <- map_df(sp_to_search, get_interactions_sleepy, 
                     interaction.type = c("eats", "eatenBy", "preysOn", "preyedUponBy"),
                     pause = 0) %>%
  rename(sourceTaxonName = source_taxon_name,
         targetTaxonName = target_taxon_name,
         interactionTypeName = interaction_type)  %>%
  filter(interactionTypeName %in% c("eats", "eatenBy", "preysOn", "preyedUponBy")) %>%
  mutate(interactionTypeName = case_when(
    interactionTypeName == "eatenBy" ~ "eaten by",
    interactionTypeName == "preysOn" ~ "preys on",
    interactionTypeName == "preyedUponBy" ~ "preyed upon by",
    interactionTypeName == "eats" ~ "eats"
  ))
#should this be piped in?
#select(sourceTaxonName, targetTaxonName, interactionTypeName)

write_csv(globi_ints, "./data/globi_ints_unresolved.csv")

#resolve taxonomy on interaction sheet
write_csv(globi_ints, "./data/globi_ints.csv")


