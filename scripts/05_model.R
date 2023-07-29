#modeling

#://blog.stata.com/2015/10/29/fixed-effects-or-random-effects-the-mundlak-approach/
#below example of mundlak model from
#https://github.com/jebyrnes/ovb_yeah_you_know_me/blob/ce21bf91ceffd700b60d6b207461dc56543b6392/OVB_YEAH_YOU_KNOW_ME.Rmd 

#mundlak_mod <- lmer(snails ~ temp + site_avg_temp + (1|site),
              #      data = sim_df,
              #      REML = FALSE)

#libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(optimx)
library(minqa)
library(dfoptim)

#data
#kelp_data <- readRDS("./data/output/kelp_cover_temp_data_additional_sites_for_analysis.rds")
kelp_data <- readRDS("./data/kelp_data/no_hyper/kelp_cover_temp_data_test.rds") 

kelp_data <- kelp_data |>
  #there is one catalina site that is actual 0 - I believe this to be inaccurate so I am dropping it. 
  filter(summer_mean_biomass_per_station > 0)

#convert year to a character for use as a fixed effect
kelp_data$year <- as.character(kelp_data$year)

#add annual means for 2 way mundlak
kelp_data <- kelp_data |>
  group_by(year) |>
  mutate(annual_mean_summer_temp = mean(summer_mean_temp),
         annual_mean_spring_temp = mean(spring_mean_temp),
         annual_mean_urban = mean(urban_pixel_fraction),
         annual_mean_spring_kelp = mean(spring_mean_biomass_per_station))

#add a site mean summer biomass column so that we can calculate anomaly
kelp_data <- kelp_data |>
  group_by(site) |>
  mutate(site_mean_summer_biomass = mean(summer_mean_biomass_per_station)) |>
  ungroup()

#calculate kelp anomaly, if needed
kelp_data <- kelp_data |>
  mutate(summer_kelp_anomaly = site_mean_summer_biomass - summer_mean_biomass_per_station,
         spring_kelp_anomaly = site_mean_spring_biomass - spring_mean_biomass_per_station)

#calculate site mean spring kelp anomaly for mundlak
kelp_data <- kelp_data |>
  group_by(site) |>
  mutate(site_mean_spring_kelp_anomaly = mean(spring_kelp_anomaly)) |>
  ungroup()

#scratch space for quick viz
ggplot(data = kelp_data , mapping = aes(y = urban_pixel_fraction, x = year)) +
  geom_point() +
  #stat_smooth(method = "lm") +
  facet_wrap(~site,scales = "free")
  

ggplot(data = kelp_data , mapping = aes(y = summer_mean_biomass_per_station, x = urban_pixel_fraction)) +
  geom_point() +
  stat_smooth(method = "lm") +
  facet_wrap(~site,scales = "free")

#data structure
#each row contains the average biomass per station for each temp grid point. 
#grid_index: temp data is at 1/4 degree resolution, all kelp stations were joined to the nearest grid point in this temp dataset. this column contains the grid point ID - see the kelp cropping script for grid index lat/lons
#mean biomass per station: each temp grid cell includes multiple (but not equal) numbers of kelp stations. Stations are joined to the nearest temp. point, and this column is the total biomass/number of stations 
#lc_pixel_count: number of "developed" pixels for each year/site

#basically, each row contains the mean/min/max temp, mean kelp biomass/station, and number of "developed' land cover pixels associated with that year/site for each point in the OISST data.


#post jarrett thoughts:
#start with summer kelp biomass only - done
#consider a lag (previous year's summer?)
#consider a temp lag as well (previous  spring temps)
#mundlak devices need to be for the full time series - not by year - done
#there is proably overdispersion
#I don't have actual 0s :)

#spring temp x urban pixel fraction is significant
#urban areas are more affected by temps during the growth season
#warmer springs see reduced kelp growth in urban areas
visreg::visreg(mundlak_mod_site_year, "spring_mean_temp", by ="urban_pixel_fraction", overlay = T, breaks = 3)
visreg::visreg(mundlak_mod_site_year, "spring_mean_temp", by ="urban_pixel_fraction")
visreg::visreg(mundlak_mod_site_year, "urban_pixel_fraction", by ="spring_mean_temp", overlay = T, breaks = 5)
visreg::visreg(mundlak_mod_site_year, "urban_pixel_fraction", by ="spring_mean_temp", breaks = 4)

visreg::visreg(mundlak_mod_site_year_glm)
visreg::visreg(mundlak_mod_site_year, "urban_pixel_fraction")


visreg(mundlak_mod_site_year, "spring_mean_temp", 
       by ="urban_pixel_fraction", 
       overlay = T, 
       breaks = 3, 
       gg = TRUE) +
  theme_bw() +
  labs(title="Effect of Urbanization on Summer Kelp Biomass", subtitle = "Conditional on Spring SST, Summer SST, and Spring Kelp Biomass",
       x ="Fraction of Pixels Classified as 'Developed'", y = "Summer Kelp Canopy Biomass \n (Wet Kg/pixel)") +
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        title = element_text(size = 20))

plot(visreg_temp)
legend("right")



#model it 
mundlak_mod <- lmer(summer_mean_biomass_per_station ~ urban_pixel_fraction + site_mean_urban + 
                      summer_mean_temp + site_mean_summer_temp + 
                      spring_mean_temp + site_mean_spring_temp + 
                      spring_mean_biomass_per_station + site_mean_spring_biomass + 
                      spring_mean_temp*urban_pixel_fraction +
                      (1|site),
                    data = kelp_data,
                    REML = T)
  
performance::r2(mundlak_mod)
summary(mundlak_mod)

mundlak_mod_site_int <- lmer(summer_mean_biomass_per_station ~ urban_pixel_fraction + site_mean_urban + 
                      summer_mean_temp + site_mean_summer_temp + 
                      spring_mean_temp + site_mean_spring_temp + 
                      spring_mean_biomass_per_station + site_mean_spring_biomass + 
                      spring_mean_temp*urban_pixel_fraction +
                      site_mean_spring_temp*site_mean_urban +
                      (1|site),
                    data = kelp_data,
                    REML = T)

summary(mundlak_mod_site_int)
performance::r2(mundlak_mod_site_int)

mundlak_mod_site_year <- lmer(summer_mean_biomass_per_station ~ urban_pixel_fraction + site_mean_urban + 
                               summer_mean_temp + site_mean_summer_temp + 
                               spring_mean_temp + site_mean_spring_temp + 
                               spring_mean_biomass_per_station + site_mean_spring_biomass + 
                               spring_mean_temp*urban_pixel_fraction +
                               site_mean_spring_temp*site_mean_urban + 
                               year +
                               (1|site),
                             data = kelp_data,
                             REML = T)
summary(mundlak_mod_site_year)
performance::r2(mundlak_mod_site_year)
performance::check_model(mundlak_mod_site_year)

kelp_data <- kelp_data |>
  mutate(summer_temp_anomaly = summer_mean_temp - site_mean_summer_temp,
         spring_temp_anomaly = spring_mean_temp - site_mean_spring_temp,
         urban_pixel_fraction_anomaly = urban_pixel_fraction - site_mean_urban)

kelp_data <- kelp_data |>
  group_by(site) |>
  mutate(site_mean_summer_temp_anomaly = mean(summer_temp_anomaly),
         site_mean_spring_temp_anomaly = mean(spring_temp_anomaly),
         site_mean_urban_anomaly = mean(urban_pixel_fraction_anomaly))|>
  ungroup()


mundlak_mod_site_year_anomaly <-  lmer(summer_mean_biomass_per_station ~ urban_pixel_fraction + site_mean_urban + 
                                         summer_temp_anomaly + site_mean_summer_temp_anomaly + 
                                         spring_temp_anomaly + site_mean_spring_temp_anomaly + 
                                         spring_kelp_anomaly + site_mean_spring_kelp_anomaly + 
                                         spring_temp_anomaly*urban_pixel_fraction +
                                         site_mean_spring_temp_anomaly*site_mean_urban + 
                                         year +
                                         (1|site),
                                       data = kelp_data,
                                       REML = T)
summary(mundlak_mod_site_year_anomaly)
performance::r2(mundlak_mod_site_year_anomaly)
performance::check_model(mundlak_mod_site_year_anomaly)


#check predictions
kelp_data$predicted_values <- predict(mundlak_mod_site_year_glm, re.form=NA)
ggplot(kelp_data, aes(x = urban_pixel_fraction, y = log(summer_mean_biomass_per_station))) +
  geom_point() +  # This will create points for your observed data
  geom_line(aes(y = predicted_values), color = "red") +  # This will create the fitted line
  labs(x = "Urban Pixel Fraction", 
       y = "log Summer Mean Biomass per Station", 
       title = "Observed data with fitted line") 

#just remove the 0 value
kelp_data <- kelp_data |>
  filter(summer_mean_biomass_per_station > 0)
mundlak_mod_site_year_glm <- glmer(summer_mean_biomass_per_station ~ urban_pixel_fraction + site_mean_urban + annual_mean_urban +
                                     summer_mean_temp + site_mean_summer_temp + annual_mean_summer_temp +
                                     spring_mean_temp + site_mean_spring_temp + annual_mean_spring_temp +
                                     spring_mean_biomass_per_station + site_mean_spring_biomass + 
                                     spring_mean_temp*urban_pixel_fraction +
                                     site_mean_spring_temp*site_mean_urban + 
                                     (1|site) + (1|year),
                               data = kelp_data,
                               family = Gamma(link = "log"),
                               control=glmerControl(optimizer="Nelder_Mead",optCtrl=list(maxfun=2e5)))

mundlak_mod_site_year_glm_FE <- glmer(summer_mean_biomass_per_station ~ urban_pixel_fraction + site_mean_urban + 
                                summer_mean_temp + site_mean_summer_temp + 
                                spring_mean_temp + site_mean_spring_temp + 
                                spring_mean_biomass_per_station + site_mean_spring_biomass + 
                                spring_mean_temp*urban_pixel_fraction +
                                site_mean_spring_temp*site_mean_urban + 
                                year +
                                (1|site),
                                data = kelp_data,
                                family = poisson(link = "log"))

diff_optims <- allFit(mundlak_mod_site_year_glm_FE, maxfun = 1e5)
diff_optims_OK <- diff_optims[sapply(diff_optims, is, "merMod")]
lapply(diff_optims_OK, function(x) x@optinfo$conv$lme4$messages)

poistest <- summary(mundlak_mod_site_year_glm_FE)




two_way_mundlak_mod_site_year_anomaly <- lmer(log(summer_mean_biomass_per_station) ~ urban_pixel_fraction_anomaly + site_mean_urban + annual_mean_urban +
                                summer_temp_anomaly + site_mean_summer_temp + annual_mean_summer_temp +
                                spring_temp_anomaly + site_mean_spring_temp + annual_mean_spring_temp +
                                spring_kelp_anomaly + site_mean_spring_kelp_anomaly + annual_mean_spring_kelp +
                                spring_temp_anomaly*urban_pixel_fraction_anomaly +
                                site_mean_spring_temp*site_mean_urban + 
                                annual_mean_spring_temp*annual_mean_urban +  
                                (1|site) + (1|year),
                              data = kelp_data,
                              REML = T)
summary(two_way_mundlak_mod_site_year_anomaly)
performance::r2(two_way_mundlak_mod_site_year_anomaly)
performance::performance(two_way_mundlak_mod_site_year_anomaly)
performance::check_model(two_way_mundlak_mod_site_year_anomaly, which = 5)

qqnorm(residuals(two_way_mundlak_mod_site_year))


mundlak_mod_site_int_confound <- lmer(summer_mean_biomass_per_station ~ urban_pixel_fraction + site_mean_urban + 
                               summer_mean_temp + site_mean_summer_temp + annual_mean_summer_temp +
                               spring_mean_temp + site_mean_spring_temp + annual_mean_spring_temp
                               spring_mean_biomass_per_station + site_mean_spring_biomass + 
                               spring_mean_temp*urban_pixel_fraction +
                               site_mean_spring_temp*site_mean_urban +
                               (1|site),
                             data = kelp_data,
                             REML = T)

performance::r2(mundlak_mod_site_int)

summary(mundlak_mod_site_int)
#if effect of urb varied by site, we'd have urb, site mean urb, and site mean urb X interaction effect
#what if the confounders captured by the site means are interacting

summary(mundlak_mod)

mundlak_mod_no_int <- lmer(summer_mean_biomass_per_station ~ urban_pixel_fraction + site_mean_urban + 
                      summer_mean_temp + site_mean_summer_temp + 
                      spring_mean_temp + site_mean_spring_temp + 
                      spring_mean_biomass_per_station + site_mean_spring_biomass + 
                      (1|site),
                    data = kelp_data,
                    REML = T)

summary(mundlak_mod_no_int)



mundlak_mod_no_spring_kelp <- lmer(summer_mean_biomass_per_station ~ urban_pixel_fraction + site_mean_urban + summer_mean_temp + site_mean_summer_temp + spring_mean_temp + site_mean_spring_temp + (1|site),
                    data = kelp_data,
                    REML = T)
summary(mundlak_mod_no_spring_kelp)

mundlak_mod_no_spring_kelp_int <- lmer(summer_mean_biomass_per_station ~ urban_pixel_fraction + site_mean_urban + summer_mean_temp + site_mean_summer_temp + spring_mean_temp + site_mean_spring_temp  +
                                         summer_mean_temp*urban_pixel_fraction + (1|site),
                                   data = kelp_data,
                                   REML = T)
summary(mundlak_mod_no_spring_kelp_int)




kelp_data_for_cor <- kelp_data
kelp_datas_for_cor$site <- as.numeric(kelp_data_for_cor$site)
cor(kelp_datas_for_cor)
corrplot::corrplot(cor(kelp_datas_for_cor))

#there are a lot of very tight correlations, try the anomoly approach instead




#mundlak_mod <- lmer(mean_biomass_per_station ~ urban_pixel_fraction + site_mean_urban + mean_temp + site_mean_temp + (1|site),
    #                data = site_means,
    #                REML = T)

mundlak_mod_glmer<- glmer(mean_biomass_per_station ~ urban_pixel_fraction + site_mean_urban + mean_temp + site_mean_temp + (1|site),
                                data = kelp_data,
                          family = gaussian(link = "log"))

  
#is this interaction coded correctly?
mundlak_mod_int <- lmer(mean_biomass_per_station ~ urban_pixel_fraction + mean_temp + site_mean_temp + urban_pixel_fraction*mean_temp + (1|site),
      data = kelp_data,
      REML = T)
#singular fit without REML - how do i compare these models if I can't fit without REML?

#check summary
summary(mundlak_mod_no_int)
#anova agrees
anova(mundlak_mod_no_int)



#assumptions
#linearity
plot(resid(mundlak_mod_no_int),kelp_data$mean_biomass_per_station) #resid() calls for the residuals of the model, mean_biomass_per_station was our initial outcome variables - we're plotting the residuals vs observed

#looks terribly

#homogeneity of variance
plot(mundlak_mod_no_int)

#normally distributed residuals
install.packages("DHARMa")
library(DHARMa)

simulationOutput <- simulateResiduals(fittedModel = mundlak_mod, plot = F)


plot(simulationOutput)

####
#predictions

df <- tidyr::crossing(data.frame(site = c("LA", "Catalina"), 
                                 site_mean_urban = c(0.5, 0.002)), 
                      data.frame(urban_pixel_fraction = seq(0,1,length.out = 10)))
library(visreg)

#using visreg to make a conditional plot
#holding all other factors at their median, what is the effect of urban pixel fraction?
visreg(mundlak_mod)


mod <- visreg(mundlak_mod_site_year, 
       gg=TRUE,
       line=list(col="red"),
       points=list(size=3, shape = 16, col="black"))[[1]] +
  theme_bw() +
  labs(title="Effect of Urbanization on Summer Kelp Biomass", subtitle = "Conditional on Spring SST, Summer SST, and Spring Kelp Biomass",
       x ="Fraction of Pixels Classified as 'Developed'", y = "Summer Kelp Canopy Biomass \n (Wet Kg/pixel)") +
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        title = element_text(size = 20))
  
ggsave(mod, path= "./figs/benthics_2023", filename = "mod.jpg", dpi = 600, width = 10, height = 4, units = c("in"))



######################

kelp_data$fit <- predict(mundlak_mod)


# For simple ribbon


ggplot(data = kelp_data, aes(x = urban_pixel_fraction, y = fit))+
  geom_line(colour = "blue")+
  geom_point(data = kelp_data, aes(x=urban_pixel_fraction, y=fit))+
  theme_bw()

getAnywhere(visreg)
