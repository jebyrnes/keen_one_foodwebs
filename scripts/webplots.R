library(ggraph)
library(tidygraph)
library(ggplot2)
library(dplyr)
library(purrr)
library(readr)

source("scripts/make_web_functions.R")

weball <- readRDS("data/web_all.rds")

fw_plot <- weball$plot_web[[1]]
fw <- weball$graph[[1]]
# fun
fw_ggraph <- function(agraph) {
  ggraph(
    agraph,
    layout = "manual",
    circular = FALSE,
    x = agraph %>% activate(nodes) %>% pull(x),
    y = agraph %>% activate(nodes) %>% pull(y)
  ) +
    theme_void()
}

fw_ggraph(fw) +
  geom_edge_link2(alpha = 0.2) +
  
  geom_node_point(aes(color = trophic_level), size = 3) +
  scale_color_viridis_b(option = "C",guide=NULL ) 

#from the total int data
int_dat <- read_csv("./data/cleaned_joined_int_data.csv") %>%
  filter(interactionTypeName %in%
           c("preys on", "preyed upon by",
             "eats", "is eaten by"))
int_dat <- int_dat %>%
  group_by(sourceTaxonName, targetTaxonName, interactionTypeName) %>%
  slice(1L)


web_all_int <- tibble(SPECIES = 
                        unique(int_dat$sourceTaxonName, 
                               int_dat$targetTaxonName)) %>%
  make_one_web(foodweb = int_dat) %>%
  make_from_to %>%
  tbl_graph(edges=.)

adj_mat_all_int <- igraph::as_adjacency_matrix(web_all_int) %>%
  as.matrix

web_all_int <- add_info_to_web(web_all_int, adj_mat_all_int)
  


fw_ggraph(web_all_int) +
  geom_edge_link2(alpha = 0.2) +
  
  geom_node_point(aes(color = trophic_level), size = 3) +
  scale_color_viridis_b(option = "C",guide=NULL ) 


#some queries
web_all_int %>%
  activate("nodes") %>%
  arrange(desc(trophic_level))

web_all_int %>%
  activate("nodes") %>%
  filter(name=="Strongylocentrotus droebachiensis")



web_all_int %>%
  activate("nodes") %>% 
  pull(name) %>% length()

web_all_int %>%
  activate("edges") %>% 
  pull(from) %>% length()
