library(readr)
library(dplyr)

#species list
sp_data_ne_subtidal <- readRDS("data/sp_data_ne_subtidal_worms_resolves.rds")

# fix misspellings (i.e., add them back in)
sp_data_ne_subtidal <- sp_data_ne_subtidal%>%
  mutate(sourceTaxonName = ifelse(!is.na(sourceTaxonId), sourceTaxonId, sourceTaxonName))


#load entered data, filter to what we have, and fix taxonomy with valid names
int_dat <- read_csv("./data/entered_int_data.csv") %>%
  filter(sourceTaxonName %in% sp_data_ne_subtidal$sourceTaxonName) %>%
  filter(targetTaxonName %in% sp_data_ne_subtidal$sourceTaxonName) %>%
  dplyr::select(sourceTaxonName, targetTaxonName, interactionTypeName) %>%
  filter(interactionTypeName %in% c("eats", "eaten by", "preys on", "preyed upon by")) 

int_dat <- int_dat %>%
  left_join(sp_data_ne_subtidal %>%
              select(sourceTaxonName, sourceTaxonName_valid = valid_names))%>%
  left_join(sp_data_ne_subtidal %>%
              select(targetTaxonName = sourceTaxonName, targetTaxonName_valid = valid_names)) %>%
  mutate(sourceTaxonName = sourceTaxonName_valid,
         targetTaxonName = targetTaxonName_valid) %>%
  select(-sourceTaxonName_valid, -targetTaxonName_valid)

#load GLobi data
globi_ints <- read_csv("./data/globi_ints.csv")%>%
  filter(sourceTaxonName %in% sp_data_ne_subtidal$sourceTaxonName) %>%
  filter(targetTaxonName %in% sp_data_ne_subtidal$sourceTaxonName)  %>%
  mutate(interactionTypeName = case_when(
    interactionTypeName == "eatenBy" ~ "eaten by",
    interactionTypeName == "preysOn" ~ "preys on",
    interactionTypeName == "preyedUponBy" ~ "preyed upon by",
    interactionTypeName == "eats" ~ "eats"
  ))


int_dat <- bind_rows(int_dat, globi_ints)  %>%
  group_by(sourceTaxonName, targetTaxonName, interactionTypeName) %>%
  slice(1L)

write_csv(int_dat, "./data/cleaned_joined_int_data.csv")
