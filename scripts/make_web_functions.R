require(tidygraph)
require(ggraph)
require(NetIndices)

fw <- read_csv("data/cleaned_joined_int_data.csv") %>%
  filter(interactionTypeName %in%
           c("preys on", "preyed upon by",
             "eats", "is eaten by"))

# function to filter to interactions of just species involved
make_one_web <- function(adf, foodweb = fw){
  sp <- adf$SPECIES
  
  local_web <- foodweb %>%
    filter(sourceTaxonName %in% sp &
             targetTaxonName %in% sp)
  #cleanup
  local_web %>%
    dplyr::select(sourceTaxonName, 
                  interactionTypeName, 
                  targetTaxonName) %>%
    group_by(sourceTaxonName, 
             interactionTypeName, 
             targetTaxonName) %>%
    summarize(n_in_db = n()) %>%
    ungroup()
  
}

# from predator to prey
make_from_to <- function(aweb){
  aweb %>%
    mutate(from = ifelse(interactionTypeName %in% 
                           c("preys on", "eats"),
                         sourceTaxonName,
                         targetTaxonName
    ),
    to = ifelse(interactionTypeName %in% 
                  c("preyed upon by", "is eaten by"),
                sourceTaxonName,
                targetTaxonName
    )
    ) %>%
    dplyr::select(from, to)
  
}

#get xy and things
add_info_to_web <- function(agraph, amat, 
                            seed=NULL, sp_position = NULL){
  set.seed(seed)
  
  troph <- TrophInd(Tij = amat)
  
  agraph <- agraph %>%
    activate(nodes) %>%
    mutate(trophic_level = abs(troph$TL),
           omnivory_ind = troph$OI,
           x = runif(length(name)),
           y = trophic_level)
  
  if(!is.null(sp_position)){
    agraph <- agraph %>% 
      activate(nodes) %>%
      dplyr::select(-x, -y) %>%
      left_join(sp_position)
  }
  
  agraph
  
}


# to make a barebones plot
make_fw_ggraphs <- function(agraph) {
  ggraph(
    agraph,
    layout = "manual",
    circular = FALSE,
    x = agraph %>% activate(nodes) %>% pull(x),
    y = agraph %>% activate(nodes) %>% pull(y)
  )  +
    geom_edge_link0(alpha = 0.4) +
    geom_node_point(aes(x = x, y = trophic_level, 
                        color = trophic_level), size = 3) +
    theme_void() +
    scale_color_viridis_b(option = "C",guide=NULL ) 
}
