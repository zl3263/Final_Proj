library(tidyverse)
library(terra)
library(raster)

library(tgp)
#setwd("Shiny_Map")

load("cleaned_data.RData")

resolution=300

transformed_rental_income = 
  transformed_rental_income %>%
  group_by(boro_block) %>%
  summarize(
    gross_income_per_sq_ft = mean(gross_income_per_sq_ft),
    longitude = mean(longitude),
    latitude = mean(latitude)
  )

long_int = c(min(transformed_rental_income$longitude),max(transformed_rental_income$longitude))
lat_int = c(min(transformed_rental_income$latitude),max(transformed_rental_income$latitude))
counter = matrix(0,nrow=resolution,ncol = resolution)
grid = matrix(0,nrow=resolution,ncol = resolution)

longitude = transformed_rental_income %>% pull(longitude)
latitude = transformed_rental_income %>% pull(latitude)
gross_income_per_sq_ft=transformed_rental_income %>% pull(gross_income_per_sq_ft)
for(i in 1:nrow(transformed_rental_income)){
  x=as.integer((longitude[i]-long_int[1])/(long_int[2]-long_int[1])*(resolution-1))+1
  y=as.integer((latitude[i]-lat_int[1])/(lat_int[2]-lat_int[1])*(resolution-1))+1
  counter[x,y]=counter[x,y]+1
  grid[x,y]=grid[x,y]+gross_income_per_sq_ft[i]
}

grid=grid/counter
grid=t(grid)




heatmap = rast(
  ncol=resolution,
  nrow=resolution,
  xmin=long_int[1],
  xmax=long_int[2],
  ymin=lat_int[1],
  ymax=lat_int[2]
)
values(heatmap) <- grid

heatmap =raster(terra::flip(heatmap,direction="vertical")) 

save(heatmap,file="heatmap.RData")



