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

regression <- lm(log(dado$areakm) ~ dado$hostspot_m+dado$fence_m+dado$house_m+dado$road_m+dado$river_m+dado$ruralbuild_m)
summary(regression)
