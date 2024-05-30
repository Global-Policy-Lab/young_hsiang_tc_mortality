library(data.table)
library(dplyr)
library(magrittr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(fields)
library(akima)
library(sf)
library(RColorBrewer)
library(terra)
library(raster)
library(spatstat)
library(R.matlab)
library(matlib)
library(cowplot)
library(readr)
library(stringr)

shp <- "data/cb_2016_us_state_20m/cb_2016_us_state_20m.shp"
states <- read_sf(shp)

fips <- "https://www2.census.gov/geo/docs/reference/state.txt"
fips_codes <- readr::read_delim(fips, "|")
head(fips_codes, 3)
noncont <- c("Hawaii", "Puerto Rico", "Alaska")
states <- states %>%
  left_join(fips_codes, by = c("STATEFP" = "STATE")) %>%
  filter(!(STATE_NAME %in% noncont))


#----Rain data

data_path <- "https://www.wpc.ncep.noaa.gov/tropical/rain/CONUS_rainfall_obs_1900-2020.csv"

rain_df <- fread(data_path)
rain_df = na.omit(rain_df)
rain_sf = st_as_sf(rain_df, coords = c("Lon", "Lat"))
plot(st_geometry(states), graticule = TRUE)
plot(st_geometry(rain_sf[rain_sf$Storm=="Abby 1964",]), add = TRUE)

stormnames = unique(rain_df$Storm)
grid_res = 0.1
grid_lon <- seq(-140, -60, by = grid_res)
grid_lat <- seq(20, 50, by = grid_res)



#-----windspeed data

f = readMat('data/NA_USA_density_8_yr_1930_2018_storm_specific.mat')

head(f)
str(f)
length(f$storm.specific.fields)

# get longitude and latitude

lat = unlist(f$storm.specific.fields[c(8), , 1])
lon = unlist(f$storm.specific.fields[c(9), , 1])


# get wind speed
maxs = f$storm.specific.fields[c(3), , 1]
storm.name = as.data.frame(f$storm.specific.fields[c(6), , 1])


# Get time
year = unlist(f$storm.specific.fields[c(4), , 1])
month = unlist(f$storm.specific.fields[c(5), , 1])


namelist <- c("DOLLY", "DAVID", "ANDREW", "KATE", "IKE", "IRENE")

for (x in namelist) {
  if (x=="DOLLY") {
    n = 441
    y=1968
  }
  if (x=="DAVID") {
    n = 670
    y=1979
  } 
  if (x=="ANDREW") {
    n = 859
    y=1992
  } 
  if (x=="KATE") {
    n = 768
    y=1985
  } 
  if (x=="IKE") {
    n = 1114
    y=2008
  }
  if (x=="IRENE") {
    n = 1163
    y=2011
  }
  
  print(x)

  nameone = unlist(storm.name)
  index_stormname = nameone[nameone==x]
  yearone = unlist(year[n])
  monthone = unlist(month[n])
  maxsone = t(unlist(maxs$maxs[, , n]))
  maxsone[maxsone==0] = NA
  
  
  swath_vec_long <- as.vector(maxsone)
  nlon = length(lon)
  nlat = length(lat)
  swath_mat <- matrix(swath_vec_long, nrow=nlon*nlat) #, ncol=nt)
  dim(swath_mat)
  
  lonlat <- as.matrix(expand.grid(lon,lat))
  swath_df02 <- data.frame(cbind(lonlat,swath_mat))
  
  swath_df02 = swath_df02 %>%
    rename(lon = Var1, lat = Var2, maxs = V3 )
  swath_df02$name = "DOLLY"
  swath_df02$year = yearone
  swath_df02$month = monthone
  
  swath_sf = st_as_sf(swath_df02, coords = c("lon", "lat"), crs="NAD83")
  
  
  ### DOLLY 1968 rain
  name_rain = paste0(str_to_title(x), " ", y)
  rain_df_1 = rain_df[rain_df$Storm==name_rain]
  rain_df_1_sf =  st_as_sf(rain_df_1, coords=c("Lon", "Lat"))
  rain_df_1_spatial = as_Spatial(rain_df_1_sf)
  obs_window <- owin(rain_df_1_spatial@bbox[1,], rain_df_1_spatial@bbox[2,])
  ppp_rain<-ppp(rain_df_1$Lon,rain_df_1$Lat, marks = rain_df_1$Total,
                window=obs_window)
  
  #------- interpolation
  idw_rain <- idw(ppp_rain, power=2, at="pixels")
  idw.output = as.data.frame(idw_rain)
  names(idw.output)[1:3] <- c("lon", "lat", "rain")
  
  
  idw_sf = st_as_sf(idw.output, coords = c("lon", "lat"), crs="NAD83")
  
  #---- subset states with station
  st_crs(rain_df_1_sf) = "NAD83"
  rain_sf_state = st_intersection(states, rain_df_1_sf)
  statelist = unique(rain_sf_state$STATEFP)
  
  idw_sf_state = st_intersection(states, idw_sf)
  idw_sf_state = idw_sf_state %>%
    filter(STATEFP %in% statelist)
  #idw_sf_state = idw_sf_state %>%
  #  filter(STUSPS !="OH" & STUSPS !="KY")
  swath_sf = na.omit(swath_sf)
  swath_sf_state = st_intersection(states, swath_sf)
  swath_sf_state = swath_sf_state %>%
    filter(STATEFP %in% statelist)
  
  
  #------- side-by-side plot
  rain = ggplot() +
    geom_sf(data=idw_sf_state, aes(color=rain)) +
    geom_sf(data = states, fill = NA) +
    guides(colour=guide_legend(title="Rain (inch)")) +
    theme_void() + coord_sf(xlim = c(-83, -66), ylim = c(24.5, 50), expand = FALSE)
  print(rain + scale_colour_gradient(low="white", high="brown"))
  
  wind = ggplot() +
    geom_sf(data= swath_sf_state, aes(color=maxs))  +
    geom_sf(data = states, fill = NA) +
    guides(colour=guide_legend(title="Wind Speed \n (m/s)")) +
    theme_void() + coord_sf(xlim = c(-83, -66), ylim = c(24.5, 50), expand = FALSE)
  print(wind + scale_colour_gradient(low="white", high="brown"))
  
  
  outname = paste0("./figures/appendix/rain_wind_", x,"_",yearone, ".pdf")
  print(outname)
  pdf(outname, height = 5, width = 7)
  plot_grid(print(rain + scale_colour_gradient(low="white", high="brown")), print(wind + scale_colour_gradient(low="white", high="brown")), labels = "AUTO")
  dev.off()
  
  
}


