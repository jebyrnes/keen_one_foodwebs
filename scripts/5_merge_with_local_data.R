#'---------------------------------------
#' Merge FW and Site-level data
#'---------------------------------------

library(dplyr)
library(readr)
library(tidyr)

# Load and process data ####
# load site-year-level data
fw <- readRDS("data/web_transect_year_df.rds") %>%
  filter(SITE != "SW Appledore")

# load site-year-level kelp data
# and get GMC for cover and abundance
keen <- read_csv("https://github.com/jebyrnes/keen_gom_temp/raw/oisst_revision/derived_data/keen_merged_data.csv")

keen_site_trans <- keen %>%
  mutate(SITE = case_when(
           SITE=="HURR" ~ "Hurricane Island",
           SITE=="Pumphouse Beach" ~ "Nahant",
           SITE=="Canoe Beach" ~ "Nahant",
           TRUE ~ SITE
         )) %>%
  # group_by(SITE, YEAR) %>%
  # summarize(across(PERCENT_ROCK:POLLACK_PER_TRANSECT,
  #                  ~mean(.x, na.rm=TRUE))) %>%
  ungroup() %>%
  filter(SITE != "SW Appledore") %>%
  group_by(SITE) %>%
  mutate(s_latissima_mean = mean(S_LATISSIMA_SQ_M, na.rm=TRUE),
         s_latissima_anomaly = S_LATISSIMA_SQ_M - s_latissima_mean,
         kelp_mean = mean(TOTAL_KELP_SQ_M, na.rm=TRUE),
         kelp_anomaly = TOTAL_KELP_SQ_M - kelp_mean,
         kelp_cover_mean = mean(PERCENT_KELP, na.rm=TRUE),
         kelp_cover_anomaly = PERCENT_KELP - kelp_cover_mean) %>%
  ungroup()


keen_site <- keen %>%
  mutate(SITE = case_when(
    SITE=="HURR" ~ "Hurricane Island",
    SITE=="Pumphouse Beach" ~ "Nahant",
    SITE=="Canoe Beach" ~ "Nahant",
    TRUE ~ SITE
  )) %>%
   group_by(SITE, YEAR) %>%
   summarize(across(c(PERCENT_ROCK:POLLACK_PER_TRANSECT, TOTAL_RICHNESS),
                    ~mean(.x, na.rm=TRUE))) %>%
  ungroup() %>%
  filter(SITE != "SW Appledore") %>%
  group_by(SITE) %>%
  mutate(s_latissima_mean = mean(S_LATISSIMA_SQ_M, na.rm=TRUE),
         s_latissima_anomaly = S_LATISSIMA_SQ_M - s_latissima_mean,
         kelp_mean = mean(TOTAL_KELP_SQ_M, na.rm=TRUE),
         kelp_anomaly = TOTAL_KELP_SQ_M - kelp_mean,
         kelp_cover_mean = mean(PERCENT_KELP, na.rm=TRUE),
         kelp_cover_anomaly = PERCENT_KELP - kelp_cover_mean) %>%
  ungroup()

# load site-year-level temp data
# and get GMC on spring and summer mean temp
temps <- read_csv("https://github.com/jebyrnes/keen_gom_temp/raw/oisst_revision/derived_data/keen_sites_with_temps.csv")

temps <- temps %>%
  filter(SITE != "SW Appledore") %>%
  mutate(SITE = 
           case_when(
             SITE=="Canoe Beach" ~ "Nahant",
             SITE=="Canoe Beach, Nahant, MA" ~ "Nahant",
             SITE=="Pumphouse Beach" ~ "Nahant",
             SITE=="Pumphouse Beach, Nahant, MA" ~ "Nahant"
             SITE=="Nubble" ~ "Nubble Lighthouse",
             SITE=="SCHO" ~ "Schoodic",
             SITE=="HURR" ~ "Hurricane Island",
             SITE=="Pemaquid" ~"Pemaquid",
             SITE=="Fort Weatherill"  ~ "Fort Weatherill",
             TRUE ~ SITE
           )) %>%
  group_by(SITE, YEAR) %>%
  summarize(across(FALL_MAX_SEA_SURFACE_TEMPERATURE:WINTER_MIN_SEA_SURFACE_TEMPERATURE,
                   ~mean(.x, na.rm=TRUE))) %>%
  ungroup()

temps <- temps %>%
  #dplyr::select(c(SITE, YEAR, FALL_MAX_SEA_SURFACE_TEMPERATURE:WINTER_MIN_SEA_SURFACE_TEMPERATURE)) %>%
  group_by(SITE) %>%
  mutate(spring_sst_mean = mean(SPRING_MEAN_SEA_SURFACE_TEMPERATURE, na.rm=TRUE),
         spring_sst_anomaly = SPRING_MEAN_SEA_SURFACE_TEMPERATURE-spring_sst_mean,
         winter_sst_mean = mean(WINTER_MEAN_SEA_SURFACE_TEMPERATURE, na.rm=TRUE),
         winter_sst_anomaly = WINTER_MEAN_SEA_SURFACE_TEMPERATURE-winter_sst_mean,
         summer_sst_mean = mean(SUMMER_MEAN_SEA_SURFACE_TEMPERATURE, na.rm=TRUE),
         summer_sst_anomaly = SUMMER_MEAN_SEA_SURFACE_TEMPERATURE-summer_sst_mean
        ) %>%
  ungroup()

# merge
dat <- left_join(fw, keen_site_trans) %>%
  left_join(temps)

visdat::vis_dat(dat)

saveRDS(dat, "data/merged_data.rds")


dat_site <- left_join(fw, keen_site) %>%
  left_join(temps)

saveRDS(dat_site, "data/merged_data_site.rds")


