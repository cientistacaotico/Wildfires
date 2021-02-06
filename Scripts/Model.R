# Load Packages -----------------------------------------------------------

# https://gist.github.com/stevenworthington/3178163

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(c('ggplot2', 'raster', 'GGally', 'MASS', 'car', 'spgwr', 'spdep'))

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

# Write table - R2

write.table(cbind(dado$Long, dado$Lat, gwr.model$SDF$localR2), 
            file = "./Results/R2.txt", 
            row.names = FALSE,
            col.names = c("Long", "Lat", "R2"))

# Write table - Residuals

write.table(cbind(dado$Long, dado$Lat, gwr.model$SDF$gwr.e), 
            file = "./Results/Residuals.txt", 
            row.names = FALSE,
            col.names = c("Long", "Lat", "Residuals"))

# Moran Index -------------------------------------------------------------

# Generate a spatial weights matrix using nearest neighbours

coord <- as.matrix(dado[,1:2])

M <- knearneigh(coord, k=4)

M <- knn2nb(M)

M <- nb2listw(M,style = 'C')

gwr.morantest(gwr.model, M)




