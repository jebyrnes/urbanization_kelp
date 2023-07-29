#working code for urbanization models

#libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(optimx)
library(minqa)
library(dfoptim)
library(DHARMa)


#data
kelp_data <- readRDS("./data/kelp_data/kelp_cover_temp_data_test.rds") 
#this file name sucks but is leftover from tracking down the issue with data processing. 
#I'll be cleaning this up but don't want to break my processing workflow just for this upload

kelp_data <- kelp_data |>
  #there is one catalina site that is actual 0 - I believe this to be inaccurate so I am dropping it. 
  #If more sites had hit 0, or someone disagrees, I am happy to reinstate this, but it makes some modeling approaches more difficult
  filter(summer_mean_biomass_per_station > 0) #this should only ever drop one site - catalina 1993

#data wrangling  ####

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

#calculate anomalies, if needed

#kelp
kelp_data <- kelp_data |>
  mutate(summer_kelp_anomaly = site_mean_summer_biomass - summer_mean_biomass_per_station,
         spring_kelp_anomaly = site_mean_spring_biomass - spring_mean_biomass_per_station)

#environmental conditions
kelp_data <- kelp_data |>
  mutate(summer_temp_anomaly = summer_mean_temp - site_mean_summer_temp,
         spring_temp_anomaly = spring_mean_temp - site_mean_spring_temp,
         urban_pixel_fraction_anomaly = urban_pixel_fraction - site_mean_urban)

#site means
kelp_data <- kelp_data |>
  group_by(site) |>
  mutate(site_mean_summer_temp_anomaly = mean(summer_temp_anomaly),
         site_mean_spring_temp_anomaly = mean(spring_temp_anomaly),
         site_mean_urban_anomaly = mean(urban_pixel_fraction_anomaly))|>
  ungroup()


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

#model vizualations  ####
#i know this isn't very well organized but it was easier for me to have all this code in one place.
#y workflow was to run the models I was checking and then come back up here to tinker with vizualization 



visreg::visreg(mundlak_mod_site_year, "spring_mean_temp", by ="urban_pixel_fraction", overlay = T, breaks = 3)
visreg::visreg(mundlak_mod_site_year, "spring_mean_temp", by ="urban_pixel_fraction")
visreg::visreg(mundlak_mod_site_year, "urban_pixel_fraction", by ="spring_mean_temp", overlay = T, breaks = 5)
visreg::visreg(mundlak_mod_site_year, "urban_pixel_fraction", by ="spring_mean_temp", breaks = 4)

visreg::visreg(mundlak_mod_site_year_glm)

visreg::visreg(mundlak_mod_site_year, "urban_pixel_fraction")

#check predictions
kelp_data$predicted_values <- predict(mundlak_mod_site_year_glm, re.form=NA)
ggplot(kelp_data, aes(x = urban_pixel_fraction, y = log(summer_mean_biomass_per_station))) +
  geom_point() +  
  geom_line(aes(y = predicted_values), color = "red") +  
  labs(x = "Urban Pixel Fraction", 
       y = "log Summer Mean Biomass per Station", 
       title = "Observed data with fitted line") 

#Dharma

simulationOutput <- simulateResiduals(fittedModel = mundlak_mod, plot = F)


plot(simulationOutput)


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

#original attempts for posterity ####
#saving for posterity, but this model does not contain interacting confounders, which makes it inappropriate 
mundlak_mod <- lmer(summer_mean_biomass_per_station ~ urban_pixel_fraction + site_mean_urban + 
                      summer_mean_temp + site_mean_summer_temp + 
                      spring_mean_temp + site_mean_spring_temp + 
                      spring_mean_biomass_per_station + site_mean_spring_biomass + 
                      spring_mean_temp*urban_pixel_fraction +
                      (1|site),
                    data = kelp_data,
                    REML = T)
performance::check_model(mundlak_mod)
performance::r2(mundlak_mod)
summary(mundlak_mod)

#attempt 2
#saving for posterity, but this model does not contain any parameters to account for temporal confounders, so it is inappropriate 
mundlak_mod_site_int <- lmer(summer_mean_biomass_per_station ~ urban_pixel_fraction + site_mean_urban + 
                      summer_mean_temp + site_mean_summer_temp + 
                      spring_mean_temp + site_mean_spring_temp + 
                      spring_mean_biomass_per_station + site_mean_spring_biomass + 
                      spring_mean_temp*urban_pixel_fraction +
                      site_mean_spring_temp*site_mean_urban +
                      (1|site),
                    data = kelp_data,
                    REML = T)
performance::check_model(mundlak_mod_site_int)
summary(mundlak_mod_site_int)
performance::r2(mundlak_mod_site_int)

#1 way mundlak: spatial group means with fixed effect of year ####
#attempt 3: this is the model I presented in the report. it accounts for temporal confounders and interacting spatial confounders 
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
performance::check_model(mundlak_mod_site_year)
summary(mundlak_mod_site_year)
performance::r2(mundlak_mod_site_year)



#two way mundlak #####
#The other way to account for temporal confounders is to calculate yearly group means across sites, aka annual means
#this way we have temp effect, temporal and spatial confounder effects, and then a random effect to capture the remaining variance
two_way_mundlak_mod_site_year <- lmer(summer_mean_biomass_per_station ~ 
                                        urban_pixel_fraction + site_mean_urban + annual_mean_urban +
                                        summer_mean_temp + site_mean_summer_temp + annual_mean_summer_temp +
                                        spring_mean_temp + site_mean_spring_temp + annual_mean_spring_temp +
                                        spring_mean_biomass_per_station + site_mean_spring_biomass + 
                                        spring_mean_temp*urban_pixel_fraction +
                                        site_mean_spring_temp*site_mean_urban + 
                                        (1|site) + (1|year),
                                      data = kelp_data,
                                      REML = T)

performance::check_model(two_way_mundlak_mod_site_year)
summary(two_way_mundlak_mod_site_year)
performance::r2(two_way_mundlak_mod_site_year)

#anomaly model #####
#don't model values, model deviations from means
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
performance::check_model(mundlak_mod_site_year_anomaly)
summary(mundlak_mod_site_year_anomaly)
performance::r2(mundlak_mod_site_year_anomaly)

#This model accounts for interacting temporal confounders. I fear that it over complicates the model structure wihtout adding much real-world relavence,
#but maybe that's just because I am having trouble conceptualizing interacting temporal confounders so my brain discounts their importance
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


#icky GLM stuff #####

two_way_mundlak_glm <- glmer(summer_mean_biomass_per_station ~ urban_pixel_fraction + site_mean_urban + annual_mean_urban +
                                     summer_mean_temp + site_mean_summer_temp + annual_mean_summer_temp +
                                     spring_mean_temp + site_mean_spring_temp + annual_mean_spring_temp +
                                     spring_mean_biomass_per_station + site_mean_spring_biomass + 
                                     spring_mean_temp*urban_pixel_fraction +
                                     site_mean_spring_temp*site_mean_urban + 
                                     (1|site) + (1|year),
                               data = kelp_data,
                               family = Gamma(link = "log"),
                               control=glmerControl(optimizer="Nelder_Mead",optCtrl=list(maxfun=2e5)))

#for some reason R let me fit this with a poisson and it actually converged, despite none of my variables being integers? ?
mundlak_year_FE_glm <- glmer(summer_mean_biomass_per_station ~ urban_pixel_fraction + site_mean_urban + 
                                summer_mean_temp + site_mean_summer_temp + 
                                spring_mean_temp + site_mean_spring_temp + 
                                spring_mean_biomass_per_station + site_mean_spring_biomass + 
                                spring_mean_temp*urban_pixel_fraction +
                                site_mean_spring_temp*site_mean_urban + 
                                year +
                                (1|site),
                                data = kelp_data,
                                family = poisson(link = "log"))
#troubleshooting convergence issues
diff_optims <- allFit(mundlak_mod_site_year_glm_FE, maxfun = 1e5)
diff_optims_OK <- diff_optims[sapply(diff_optims, is, "merMod")]
lapply(diff_optims_OK, function(x) x@optinfo$conv$lme4$messages)

poistest <- summary(mundlak_mod_site_year_glm_FE)

### ### ### ### ### ### ### ### ###
#percent cover and change analysis - this is unchanged from the report I sent over before, so still needs to be corrected. 
#for completions sake I wanted it in this file, just be aware it is still wrong

#load up kelp cover data (wrangled in percent_cover.R)
kelp_cover_data <- readRDS("./data/kelp_data/kelp_cover_full.rds") |>
  #also a terrible file name, sorry
  select(-quarter)

#percent cover #####
cover_mod_site_int <- glmmTMB(summer_kelp_percent_cover ~ urban_pixel_fraction + site_mean_urban + 
                                summer_mean_temp + site_mean_summer_temp + 
                                spring_mean_temp + site_mean_spring_temp + 
                                spring_kelp_percent_cover + site_mean_spring_kelp_percent_cover + 
                                spring_mean_temp*urban_pixel_fraction +
                                site_mean_spring_temp*site_mean_urban +
                                year +
                                (1|site),
                              data = kelp_cover_data,
                              REML = T,
                              family = beta_family(link = "logit"))

performance::check_model(cover_mod_site_int)
summary(cover_mod_site_int)
performance::r2(cover_mod_site_int)


# change analysis #####
#this one is especially wrong

#data wrangling to add lags, na filters are to remove rows that don't have a preceeding row for lag calculation
kelp_data_lags <- kelp_data |>
  group_by(site) |>
  mutate(delta_summer_mean_biomass_per_station = summer_mean_biomass_per_station -lag(summer_mean_biomass_per_station),
         delta_spring_mean_biomass_per_station = spring_mean_biomass_per_station -lag(spring_mean_biomass_per_station)) |>
  filter(!is.na(delta_summer_mean_biomass_per_station)) |>
  mutate(delta_summer_mean_temp = (summer_mean_temp - lag(summer_mean_temp)),
         delta_spring_mean_temp = (spring_mean_temp - lag(spring_mean_temp)),
         delta_urban_pixel_fraction = (urban_pixel_fraction - lag(urban_pixel_fraction)))|>
  filter(!is.na(delta_summer_mean_temp)) |>
  mutate(delta_site_mean_spring_biomass = mean(delta_spring_mean_biomass_per_station),
         delta_site_mean_summer_temp = mean(delta_summer_mean_temp),
         delta_site_mean_spring_temp = mean(delta_spring_mean_temp),
         delta_site_mean_urban_pixel_fraction = mean(delta_urban_pixel_fraction)) |>
  ungroup() 

# model it
mundlak_mod_change <- lmer(delta_summer_mean_biomass_per_station ~ delta_urban_pixel_fraction + delta_site_mean_urban_pixel_fraction + 
                             delta_summer_mean_temp + delta_site_mean_summer_temp + 
                             delta_spring_mean_temp + delta_site_mean_spring_temp + 
                             delta_spring_mean_biomass_per_station +  
                             delta_site_mean_spring_biomass + 
                             delta_spring_mean_temp*delta_urban_pixel_fraction +
                             delta_site_mean_spring_temp*delta_site_mean_urban_pixel_fraction + 
                             year +
                             (1|site),
                           data = kelp_data_lags,
                           REML = T)
performance::check_model(mundlak_mod_change)
summary(mundlak_mod_change)
performance::r2(mundlak_mod_change)

#Change ~ percent cover ####
#calculate lagged percent cover
kelp_cover_data_lags <- kelp_cover_data |>
  group_by(site) |>
  mutate(delta_summer_kelp_percent_cover = summer_kelp_percent_cover -lag(summer_kelp_percent_cover)) |>
  mutate(delta_spring_kelp_percent_cover = spring_kelp_percent_cover -lag(spring_kelp_percent_cover)) |>
  filter(!is.na(delta_summer_kelp_percent_cover)) |>
  mutate(delta_site_mean_spring_kelp_percent_cover = mean(delta_spring_kelp_percent_cover)) |>
  ungroup() 


#join on to the rest of the lagged data

kelp_data_lags_complete <- kelp_cover_data_lags |>
  left_join(kelp_data_lags) |>
  filter(!is.na(annual_mean_summer_temp)) 

#model it
delta_lm <- lmer(delta_summer_kelp_percent_cover ~ delta_urban_pixel_fraction + delta_site_mean_urban_pixel_fraction + 
                   delta_summer_mean_temp + delta_site_mean_summer_temp + 
                   delta_spring_mean_temp + delta_site_mean_spring_temp + 
                   delta_spring_kelp_percent_cover +  
                   delta_site_mean_spring_kelp_percent_cover + 
                   delta_spring_mean_temp*delta_urban_pixel_fraction +
                   delta_site_mean_spring_temp*delta_site_mean_urban_pixel_fraction + 
                   year +
                   (1|site),
                 data = kelp_data_lags_complete, REML = TRUE)

performance::check_model(delta_lm)
summary(delta_lm)
performance::r2(delta_lm)


### ### ### ### ### ### ### ### ###
#figure from benthics, not really relevant if we are including an interaction but saving for formatting 


#holding all other factors at their median, what is the effect of urban pixel fraction?
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




