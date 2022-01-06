library(dplyr)
library(sf)
library(oisstr) #https://github.com/jebyrnes/oisstr
library(lubridate)
library(ggplot2)
library(rnaturalearth)
library(tidyr)
coast <- ne_coastline(scale = 10, returnclass = "sf")


bio_data <- read_csv("https://github.com/jebyrnes/keen_gom_temp/raw/oisst_revision/derived_data/keen_merged_data.csv") %>% 
    as_tibble() %>%
    rename(year = YEAR, region = SITE, latitude = START_LATITUDE, longitude = START_LONGITUDE) %>%
    filter(!is.na(latitude)) %>%
    group_by(year, region, latitude, longitude) %>%
    tally %>%
    ungroup() %>%
    st_as_sf(crs = 4326, coords = c("longitude", "latitude"), 
             remove = FALSE)

st_bbox(bio_data)
coast <- st_crop(coast, bio_data)

#this is big
temp <- OISST_sub_dl(
    start = as.Date("2014-01-01"),
    end = as.Date("2018-12-31"),
    latitude_ext = st_bbox(bio_data)[c(2,4)],
    longitude_ext = st_bbox(bio_data)[c(1,3)]
)




#spring mean, summer mean
summarized_temp <- temp %>%
    mutate(year = year(t),
           month = month(t),
           season = ifelse(month < 6, "spring", "summer"),
           season = ifelse(month %in% 1:2, "winter", month)) %>%
    filter(month <= 9) %>%
    group_by(lat, lon, year, season) %>%
    summarize(mean_temp = mean(temp)) %>%
    ungroup()

ggplot() +
    geom_tile(data = summarized_temp %>% filter(season == "spring"),
              aes(x = lon, y = lat, fill = mean_temp)) +
    geom_sf(data = coast) +
    scale_fill_viridis_c() +
    facet_wrap(vars(year)) +
    labs(x="", y = "", fill = "Mean Spring\nTemp. (C)") +
    theme_void() 

ggsave("figures/oisst_spring.jpg", dpi = 600)

ggplot() +
    geom_tile(data = summarized_temp %>% filter(season == "winter"),
              aes(x = lon, y = lat, fill = mean_temp)) +
    geom_sf(data = coast) +
    scale_fill_viridis_c() +
    facet_wrap(vars(year)) +
    labs(x="", y = "", fill = "Mean Winter\nTemp. (C)") +
    theme_void() 

ggsave("figures/oisst_winter.jpg", dpi = 600)


ggplot() +
    geom_tile(data = summarized_temp %>% filter(season == "summer"),
              aes(x = lon, y = lat, fill = mean_temp)) +
    geom_sf(data = coast) +
    scale_fill_viridis_c() +
    facet_wrap(vars(year)) +
    labs(x="", y = "", fill = "Mean Summer\nTemp. (C)") +
    theme_void() 

ggsave("figures/oisst_summer.jpg", dpi = 600)

ggplot(data = summarized_temp %>% mutate(season = gsub("s", "S", season)),
       aes(x = year, y = mean_temp, 
           group = paste(lat, lon), color = lat)) +
    geom_line() +
    scale_color_distiller(palette = "BrBG") +
    facet_wrap(vars(season)) +
    labs(x="", y = "Mean Temperature (C)", color = "Latitude") +
    theme_bw(base_size = 12) 

ggsave("figures/oisst_summer_trend.jpg", dpi = 600)


# make relevant to data
summarized_temp_wide <- summarized_temp %>%
    pivot_wider(names_from = season, 
                values_from = mean_temp,
                names_glue = "mean_{season}_temp_oisst") 

#add indices for joins
idx_frame <- summarized_temp_wide %>%
    select(lat, lon) %>%
    group_by(lat, lon) %>%
    slice(1L) %>%
    ungroup() %>%
    mutate(oisst_id = 1:n()) %>%
    st_as_sf(crs = 4326, coords = c("lon", "lat"), 
             remove = FALSE)

nearest_index <- st_nearest_feature(bio_data, idx_frame)

bio_data <- bio_data %>%
    mutate(oisst_id = idx_frame$oisst_id[nearest_index])

summarized_temp_wide <- summarized_temp_wide %>%
    left_join(idx_frame%>% select(lon, lat, oisst_id)) %>%
    select(-geometry) %>%
    group_by(lat, lon) %>%
    mutate(mean_spring_temp_oisst_site = mean(mean_spring_temp_oisst, na.rm=TRUE),
           mean_summer_temp_oisst_site = mean(mean_summer_temp_oisst, na.rm=TRUE),
           mean_spring_temp_oisst_dev = mean_spring_temp_oisst - mean_spring_temp_oisst_site,
           mean_summer_temp_oisst_dev = mean_summer_temp_oisst - mean_summer_temp_oisst_site
           
    ) %>%
    arrange(year) %>%
    mutate(lag_mean_spring_temp_oisst_dev = lag(mean_spring_temp_oisst_dev),
           lag_mean_summer_temp_oisst_dev = lag(mean_summer_temp_oisst_dev)) %>%
    ungroup()

#merge the data and add group mean centered terms
joined_dat <- left_join(bio_data, summarized_temp_wide) %>%
    as_tibble() %>%
    select(-geometry, -n, -lat, -lon) 

readr::write_csv(joined_dat, "derived_data/oisst_temp_data.csv")
