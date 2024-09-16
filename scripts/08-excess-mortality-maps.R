## ------------------------------------------------------------------------
## 07-excess-mortality-maps.R
## ------------------------------------------------------------------------

library(tidyverse)
library(sf)
library(readstata13)

## ------------------------------------------------------------------------
## Read in and prep data
## ------------------------------------------------------------------------
setwd("/Users/rachelyoung/Dropbox/test/YoungHsiang_replication")
shp <- "data/cb_2016_us_state_20m/cb_2016_us_state_20m.shp"
TC_mort = readstata13::read.dta13("output/mort_TC_state_allmodels_tdths_ttpop.dta") # CREATED IN appendix.do 
mort_age = readstata13::read.dta13("output/FORMAPS_linear_state_mort_age_race.dta") # CREATED IN main_analysis.do ; line 1425


states <- read_sf(shp)
states_other <- st_transform(states, "+init=epsg:26978")
noncont <- c("Hawaii", "Puerto Rico", "Alaska")
states_other <- states_other %>%
  filter(!(NAME %in% noncont))
states_other$adm_name = toupper(states_other$NAME)


tc_mort_shp = left_join(states_other, TC_mort)
tc_mort_shp$prop_total_cubic = tc_mort_shp$mort_m_cubic_adapt_pool24 / tc_mort_shp$dths_tot


## ------------------------------------------------------------------------
## Create Maps
## ------------------------------------------------------------------------
outfile = paste0("figures/figure4_state_TC_mortality_allcause.pdf")
ggplot() +
  geom_sf(data=tc_mort_shp,  aes(fill = prop_total_cubic)) +
  xlab("Longitude") + ylab("Latitude") +
  scale_fill_gradient(low = "white", high = "red", na.value="white") +
  ggtitle("") +
  labs(fill = "Proportion of \nTotal Deaths") +
  theme_void()
ggsave(outfile)



## ---------------------Total Mortality by State---------------------------
#Age<1

tc_mort_shp_age = left_join(tc_mort_shp, mort_age)
outfile = paste0("figures/figure4_state_mortality_0000.pdf")
plot_total_0000 = ggplot() +
  geom_sf(data=tc_mort_shp_age,  aes(fill = total_dths_rate_0000)) +
  xlab("Longitude") + ylab("Latitude") +
  scale_fill_gradient(low = "white", high = "brown", na.value="white") +
  ggtitle("Age <1") +
  labs(fill = "Total Mortality \n(per 100,000)") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5))
ggsave(outfile)



#Age1-44

outfile = paste0("figures/figure4_state_mortality_0144.pdf")
plot_total_0144 = ggplot() +
  geom_sf(data=tc_mort_shp_age,  aes(fill = total_dths_rate_0144)) +
  xlab("Longitude") + ylab("Latitude") +
  scale_fill_gradient(low = "white", high = "darkorange", na.value="white") +
  ggtitle("Age 1-44") +
  labs(fill = "Total Mortality \n(per 100,000)") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5))
ggsave(outfile)


## ---------------------TC Excess Mortality by State---------------------------

#Age<1

tc_mort_shp_age_naomit = na.omit(tc_mort_shp_age)
outfile = paste0("figures/figure4_state_TC_mortality_0000.pdf")
plot_TC_0000 = ggplot() +
  geom_sf(data=tc_mort_shp_age,  aes(fill =  TC_dths_rate_0000)) +
  xlab("Longitude") + ylab("Latitude") +
  scale_fill_gradient(low = "white", high = "brown", na.value="white") +
  ggtitle("Age <1") +
  labs(fill = "Tropical Cylcone \nExcess Mortality \n(per 100,000)") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5))
ggsave(outfile)


#Age1-44
outfile = paste0("figures/figure4_state_TC_mortality_0144.pdf")
plot_TC_0144 = ggplot() +
  geom_sf(data=tc_mort_shp_age,  aes(fill =  TC_dths_rate_0144)) +
  xlab("Longitude") + ylab("Latitude") +
  scale_fill_gradient(low = "white", high = "darkorange", na.value="white") +
  ggtitle("Age 1 - 44") +
  labs(fill = "Tropical Cylcone \nExcess Mortality \n(per 100,000)") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5))
ggsave(outfile)

print("completed making maps")




