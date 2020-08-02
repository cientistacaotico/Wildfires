# Load packages
# https://gist.github.com/stevenworthington/3178163

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(c('ggplot2', 'raster'))

# Load regression data

dado <- read.table("./Data/Regression/data_regression.txt", header = T)
dado <- na.omit(dado) # Remove missing data

reg <- lm(log(dado$areakm) ~ dado$hostspot_km+dado$fence_km+dado$house_km+dado$road_km+dado$river_km+dado$ruralbuild_km)
summary(reg)

# Load density data

hotspot <- raster::raster('./Data/Density_variables/hotspots.tif')
road <- raster::raster('./Data/Density_variables/roads.tif')
river <- raster::raster('./Data/Density_variables/rivers.tif')

# Load shapefile - geographical boundaries

mask <- raster::shapefile('./Shapefile/mask.shp')

# Fitting model

fitting <- hotspot*reg$coefficients[2]+road*reg$coefficients[5]+river*reg$coefficients[6]
clipping <- raster::mask(fitting, mask)

# Function for standarding raster

STANDAR <- function(x) {
  result <-
    (x - raster::cellStats(x, min)) / (raster::cellStats(x, max) - raster::cellStats(x, min))
  return(result)
}

WRM <- STANDAR(clipping)

# Load evaluation data

evaluation <- raster::raster('./Data/Evaluation/occurrences.tif')

# Creating evaluation binary data

WRM_xy <- na.omit(as.data.frame(WRM, xy = TRUE))





