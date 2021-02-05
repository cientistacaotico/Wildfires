# Load Packages -----------------------------------------------------------

# https://gist.github.com/stevenworthington/3178163

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(c('ggplot2', 'raster', 'GGally', 'MASS', 'car', 'spgwr'))

# Load regression data ----------------------------------------------------

dado <- read.table("./Data/Regression/data.txt", header = T)
dado <- na.omit(dado) # Remove missing data

# Correlogram -------------------------------------------------------------

corr <- GGally::ggpairs(data = log(dado[,3:ncol(dado)]))

ggsave(filename = "./Results/corr.tiff", 
       plot = corr, 
       units = "cm", 
       dpi = 600,
       width = 20, 
       height = 12)

# Stepwise Regression -----------------------------------------------------

regression <- lm(log(Area) ~ log(Fences) + log(Houses) + log(Roads) + log(Rivers) + log(Buildings), data = dado)
summary(regression)

step.model <- stepAIC(regression, direction = "backward")

ANOVA <- as.data.frame(step.model$anova)
write.table(x = ANOVA, file = "./Results/anova-table.txt", row.names = FALSE)

# GWR ---------------------------------------------------------------------

# Calculate kernel bandwidth

GWRbandwidth <- gwr.sel(log(Area) ~ log(Houses) + log(Roads) + log(Rivers) + log(Buildings), 
                        data = dado, 
                        coords=cbind(dado$Long, dado$Lat),
                        adapt=T) 
# Run the GWR model

gwr.model <- gwr(log(Area) ~ log(Houses) + log(Roads) + log(Rivers) + log(Buildings), 
                data = dado, coords=cbind(dado$Long, dado$Lat), 
                adapt = GWRbandwidth, 
                hatmatrix = TRUE, 
                se.fit = TRUE) 

gwr.model$SDF$localR2 # R2
gwr.model$SDF$gwr.e # Residuals 
  

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





