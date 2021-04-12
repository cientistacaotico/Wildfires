# Load Packages -----------------------------------------------------------

# https://gist.github.com/stevenworthington/3178163

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(c('ggplot2', 'raster', 'GGally', 'MASS', 'car', 'spgwr', 'spdep', 'dplyr'))

# Load regression data ----------------------------------------------------

dado <- read.table("./Data/data.txt", header = T)

# Correlogram -------------------------------------------------------------

corr <- GGally::ggpairs(data = log(dado[,4:8]))

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

# Create a spatial weights matrix object from these weights

M <- nb2listw(M,style = 'C')

# Run moran's I test

gwr.morantest(gwr.model, M)

# Simple Linear Regressions -----------------------------------------------

dado.slr <- na.omit(as.data.frame(dado[,-c(1,2)]))

names <- colnames(dado.slr)

for (i in 2:ncol(dado.slr)) {
  Regression <- lm(formula = log(dado.slr[,1]) ~ log(dado.slr[,i]), data = dado.slr)
  SUMMARY <- summary(Regression)
  Parameters <- SUMMARY$coefficients[2,]
  R2 <- SUMMARY$r.squared
  if(i == 2){
    table <- c(names[i], Parameters, R2)
  } else{
    table2 <- c(names[i], Parameters, R2)
    table <- data.frame(rbind(table, table2))
  }
}

# Organizing the table

row.names(table) <- NULL
names <- colnames(table)
names[1] <- "Variable"
names[6] <- "R2"
table <- `colnames<-`(table, names)
write.table(x = table, 
            file = "./Results/regressions.txt", 
            row.names = FALSE)

# Graph -------------------------------------------------------------------

# Load data

R2 <- raster::raster("./Data/Rasters/R2.tif")

# Extracting

R2 <- extract(R2, dado[,1:2])

dado <- as.data.frame(dado[,ncol(dado)])
colnames(dado) <- "Class"

group <- dado %>% group_by(Class) %>% summarise(no_rows = length(Class))

barras <- ggplot(data = group, aes(x = Class, y = no_rows)) +
  geom_bar(stat = "identity", fill = "#B22222") +
  coord_flip() +
  # ylab("") +
  # xlab("") +
  scale_x_discrete(limits = c("CL","CR","CS","FE","MC","SE","SS","VR"),
                   labels = c("Clean Field",
                              "Rupestrian Field",
                              "Dirty Field",
                              "Seasonal Forest",
                              "Riparian Forest",
                              "Exposed Soil",
                              "Typical Cerrado",
                              "Palm Swamp")) +
  scale_y_continuous(limits = c(0,10), breaks = c(0,2,4,6,8,10)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", size = 12),
        panel.grid = element_blank(), )

ggsave(plot = barras, filename = "./Results/barras.tiff", units = "cm", dpi = 300)

