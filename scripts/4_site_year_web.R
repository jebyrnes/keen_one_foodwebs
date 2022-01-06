#'---------------------------------------
#' Get Food Web at each site at each year
#' and calculate properties
#'---------------------------------------

library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(glue)
#---
library(tidygraph)
library(ggraph)
library(NetIndices)

#functions
source("scripts/make_web_functions.R")

#get species
sp_by_transect_only<- read_csv("data/species_list_by_site_transect_year.csv")

# get interactions
foodweb <- read_csv("data/cleaned_joined_int_data.csv") %>%
  filter(interactionTypeName %in%
           c("preys on", "preyed upon by",
             "eats", "is eaten by"))



#

# Helpful functions from my ole' GCB paper
consumer_degrees<-function(a.matrix){ 
  cd<-0
  a.matrix<-as.matrix(a.matrix)
  if(!is.na(a.matrix[1])){
    if (length(a.matrix[1,])==1) a.matrix<-t(a.matrix)
    cd<-rowSums(a.matrix)[which(rowSums(a.matrix)!=0)]
  }
  return(cd)
}

mean_degrees<-function(a.matrix){ mean(consumer_degrees(a.matrix))}

sd_degrees<-function(a.matrix){ sd(consumer_degrees(a.matrix))}

degree_info <- function(a_matrix){
  cd <- consumer_degrees(a_matrix)
  tibble(
    avg_cons_degrees = mean(cd),
    sd_cons_degrees = sd(cd)
    
  )
}


###### OK, let's do things to data
# Add web info to data
xy_tab <- NULL

add_web_info <- . %>%
  nest %>%
  mutate(interactions = map(data, make_one_web),
         from_to = map(interactions, make_from_to),
         graph = map(from_to, ~tbl_graph(edges = .x)),
         adj_mat = map(graph, ~igraph::as_adjacency_matrix(.x) %>%
                         as.matrix)) %>%
  #get more detailed info
  mutate(graph = map2(graph, adj_mat, add_info_to_web),
         plot_web = map(graph, make_fw_ggraphs)) %>%
  ungroup()

add_web_metrics <- . %>%
  mutate(map_df(adj_mat, ~GenInd(Tij = .x) %>% as_tibble),
         map_df(adj_mat, ~PathInd(Tij = .x)%>% as_tibble),
         map_df(adj_mat, degree_info),
         diameter = map_dbl(graph, igraph::diameter)
)


# Get the whole enchilada

web_all <- sp_by_transect_only %>%
  group_by(SPECIES) %>%
  slice(1L) %>%
  ungroup() %>%
  group_by(all=1) %>%
  add_web_info %>%
  add_web_metrics


xy_tab <- web_all$graph[[1]] %>% 
  activate("nodes") %>%
  as_tibble() %>% 
  dplyr::select(name, x, y)

web_transect_year_df <- sp_by_transect_only %>%
  group_by(SITE, TRANSECT, YEAR) %>%
  add_web_info %>%
  add_web_metrics

web_site_year_df <- sp_by_transect_only %>%
  group_by(SITE, YEAR, SPECIES) %>%
  slice(1L) %>%
  ungroup %>%
  group_by(SITE, YEAR) %>%
  add_web_info %>%
  add_web_metrics

web_site <- sp_by_transect_only %>%
  group_by(SITE, SPECIES) %>%
  slice(1L) %>%
  ungroup %>%
  group_by(SITE) %>%
  add_web_info %>%
  add_web_metrics

saveRDS(web_all, "data/web_all.rds")
saveRDS(web_site, "data/web_site.rds")
saveRDS(web_site_year_df, "data/web_site_year_df.rds")
saveRDS(web_transect_year_df, "data/web_transect_year_df.rds")


#some plot output
ggsave("figures/web_all.jpg", plot = web_all$plot_web[[1]])

walk2(web_site$SITE, web_site$plot_web,
      ~ggsave(glue("figures/{.x}.jpg"), plot=.y))

pwalk(list(web_site_year_df$SITE,
           web_site_year_df$YEAR,
           web_site_year_df$plot_web),
      ~ggsave(glue("figures/site_year/{..1}_{..2}.jpg"), plot=..3))

