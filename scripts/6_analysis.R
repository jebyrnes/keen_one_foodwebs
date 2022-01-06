#'---------------------------------------
#' Analysis!
#'---------------------------------------

library(dplyr)
library(car)
library(lme4)

dat <- readRDS(file = "data/merged_data.rds") %>%
  mutate(rock = scale(PERCENT_ROCK))

#PERCENT_KELP
#S_LATISSIMA_SQ_M
#TOTAL_KELP_SQ_M
kelp_mod <- lmer(TOTAL_KELP_SQ_M ~
                 summer_sst_mean +
                   summer_sst_anomaly +
                 rock +
                (1|SITE),
               data = dat)

keen_sl_mod <-  glmer(TOTAL_KELP_SQ_M+1e-05~ 
                        summer_sst_mean +
                        summer_sst_anomaly +
                        winter_sst_mean +
                        winter_sst_anomaly +
                        rock + (1|SITE),
                      data = dat,
                      family=Gamma(link="log"),
                      control = glmerControl(optimizer = "bobyqa"))

Anova(keen_sl_mod)



library(glmmTMB)
keen_cover_mod <-  glmmTMB((PERCENT_KELP)/100+1e-05 ~ 
                           TOTAL_KELP_SQ_M +
                             summer_sst_mean +
                             summer_sst_anomaly +
                             winter_sst_mean +
                             winter_sst_anomaly +
                           rock + (1|SITE),
                         data = dat,
                         family=beta_family(link = "logit"))

Anova(keen_cover_mod)

# RICHNESS

mod_rich <- lmer(TOTAL_RICHNESS ~ 
                          summer_sst_mean +
                          summer_sst_anomaly +
                          winter_sst_mean +
                          winter_sst_anomaly +
                          TOTAL_KELP_SQ_M +
                          PERCENT_KELP+
                          rock +
                          (1|SITE),
                        data = dat)

Anova(mod_rich)

# FW properties
mod_connectance <- lmer(C ~ 
                          summer_sst_mean +
                          summer_sst_anomaly +
                          winter_sst_mean +
                          winter_sst_anomaly +
            TOTAL_KELP_SQ_M +
            PERCENT_KELP+
              TOTAL_RICHNESS+
            rock +
              (1|SITE),
            data = dat)

Anova(mod_connectance)


#APL

mod_pathlength <- lmer(APL ~ 
                          summer_sst_mean +
                          summer_sst_anomaly +
                        winter_sst_mean +
                         winter_sst_anomaly +
                          TOTAL_KELP_SQ_M +
                          PERCENT_KELP+
                          TOTAL_RICHNESS+
                          rock +
                          (1|SITE),
                        data = dat)

Anova(mod_pathlength)

#avg_cons_degrees
mod_degree <- lmer(avg_cons_degrees ~ 
                     summer_sst_mean +
                     summer_sst_anomaly +
                     winter_sst_mean +
                     winter_sst_anomaly +
                          TOTAL_KELP_SQ_M +
                          PERCENT_KELP+
                          TOTAL_RICHNESS+
                          rock +
                          (1|SITE),
                        data = dat)

Anova(mod_degree)

#sd_cons_degrees

mod_sd_degree <- lmer(sd_cons_degrees ~ 
                        summer_sst_mean +
                        summer_sst_anomaly +
                        winter_sst_mean +
                        winter_sst_anomaly +
                     TOTAL_KELP_SQ_M +
                     PERCENT_KELP+
                     TOTAL_RICHNESS+
                     rock +
                     (1|SITE),
                   data = dat)

Anova(mod_sd_degree)

#diameter

mod_diameter <- lmer(diameter ~ 
                     summer_sst_mean +
                     summer_sst_anomaly +
                     winter_sst_mean +
                     winter_sst_anomaly +
                     TOTAL_KELP_SQ_M +
                     PERCENT_KELP+
                     TOTAL_RICHNESS+
                     rock +
                     (1|SITE),
                   data = dat)


modlist <- list("Connectance" = mod_connectance,
              #  "Average Path Length" = mod_pathlength, #bettter covered by diameter
                "Average Consumer Degree" = mod_degree,
                `SD in Consumer Degree` = mod_sd_degree,
                `Diameter` = mod_diameter)


library(purrr)
library(broom)
library(broom.mixed)


make_ctab_with_aov <- function(term_names, modlist){
  atab <- map(modlist, Anova) %>% 
    map_df(tidy, .id="response") %>%
    filter(term %in% term_names) 
  
  ctab <- map_df(modlist, tidy, .id="response") %>%
    filter(term %in% term_names) %>%
    dplyr::select(-effect, -group) 

  left_join(ctab, 
            atab %>% dplyr::select(response, term, p.value)) %>%
    mutate(term = gsub("_", " ", term),
           term = stringr::str_to_title(term),
           term = gsub("Sst", "SST", term))%>%
    mutate(response = factor(response,
                             levels = rev(c("Connectance", 
                                        "Diameter",
                                        "Average Consumer Degree", 
                                        "SD in Consumer Degree"))
    ))
  
}

plot_coefs <- function(atab){
  
  ggplot(atab, 
         aes(x = response, y = estimate, 
             ymin = estimate-2*std.error, 
             ymax = estimate + 2*std.error)) + 
    geom_pointrange() + 
    facet_wrap(vars(term)) + 
    coord_flip() + 
    geom_hline(yintercept = 0, lty = 2) +
    theme_bw(base_size = 17) +
    labs(x="", y = "Coefficient")
}

temp_eff <- make_ctab_with_aov(c("summer_sst_anomaly", "winter_sst_anomaly"),
                   modlist)

temp_eff
plot_coefs(temp_eff)
ggsave("figures/anomoly_coefs.jpg", dpi = 600,
       width=7.4)

mean_eff <- make_ctab_with_aov(c("summer_sst_mean", "winter_sst_mean"),
                   modlist)
plot_coefs(mean_eff)

other_eff <- make_ctab_with_aov(c("TOTAL_KELP_SQ_M" ,
                     "PERCENT_KELP",
                     "TOTAL_RICHNESS"),
                   modlist)
plot_coefs(other_eff)


rich_eff <- make_ctab_with_aov(c( "TOTAL_RICHNESS"),
                                modlist)
plot_coefs(rich_eff)

ggsave("figures/rich_coefs.jpg", dpi = 600,
       width=7.4)


map(modlist, performance::r2_nakagawa)

eff_on_rich <- make_ctab_with_aov(c("summer_sst_anomaly", "winter_sst_anomaly"),
                                  list(Richness = mod_rich))

ggplot(eff_on_rich, 
       aes(x = term, y = estimate, 
           ymin = estimate-2*std.error, 
           ymax = estimate + 2*std.error)) + 
  geom_pointrange() + 
  coord_flip() + 
  geom_hline(yintercept = 0, lty = 2) +
  theme_bw(base_size = 17) +
  labs(x="", y = "Coefficient",
       subtitle="Effect of Temperature on\nSpecies Richness")

ggsave("figures/eff_on_rich.jpg", dpi = 600,
       width=7.4)
