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

# Forest plot -------------------------------------------------------------

# Function to calculate the confidence interval

ic.m <- function(x, conf = 0.95){
  n <- length(x)
  media <- mean(x)
  variancia <- var(x)
  quantis <- qt(c((1-conf)/2, 1 - (1-conf)/2), df = n-1)
  ic <- media + quantis * sqrt(variancia/n)
  lo<- ic[1]
  up<-ic[2]
  med<- media
  ic<-cbind(med,lo,up)
  return(ic)
}

# Load data

data.for <- read.table("./Data/Forest_plot/coord.txt", header = T)
R2 <- raster::raster("./Data/Rasters/R2.tif")

# Extracting

R2 <- extract(R2, data.for[,1:2])
data.for <- as.data.frame(`colnames<-`(cbind(data.for[,-c(1,2)], R2), c("Class", "R2")))

# Confidence Interval

group <- data.for %>% group_by(Class) %>% summarise(no_rows = length(Class))

dataM <- matrix(nrow = nrow(group), ncol = 5)

for (i in 1:nrow(group)) {
  selection <- data.for[data.for[,1] == group$Class[i],][,2]
  dataM[i,1] <- group$Class[i]
  dataM[i,2] <- length(selection)
  dataM[i,3:5] <- ic.m(as.numeric(selection))
  dataM <- as.data.frame(`colnames<-`(dataM, c("Class", "Replicates", "med", "lo", "up")))
}

write.table(dataM, file = "./Results/forest_data.txt")

# Figure

dataM <- read.table("./Results/forest_data.txt", header = TRUE) 

ggplot(data = dataM, aes(x = med, y = Class, xmax = up, xmin = lo)) +
  geom_errorbarh(alpha = 0.5, color="black",height = 0.4) +
  geom_point(aes(size = factor(Replicates)), 
             shape = 15) +
  labs(size = "On-field\nobservations") +
  xlab("R²") +
  ylab("") +
  theme_bw() + 
  scale_y_discrete(limits = c("CL","CR","CS","FE","MC","SE","SS","VR"),
                   labels = c("Clean Field",
                              "Rupestrian Field",
                              "Dirty Field",
                              "Seasonal Forest",
                              "Riparian Forest",
                              "Exposed Soil",
                              "Typical Cerrado",
                              "Palm Swamp")) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1),
                     labels = c("0", "1")) +
  theme(axis.text.x = element_text(colour = "black", size = 12),
        axis.text.y = element_text(colour = "black", size = 12),
        axis.ticks.x = element_blank(), 
        text = element_text(colour = "black", size = 12),
        legend.position = "none",
        panel.grid.major = element_line(colour = "grey95"))

ggsave(plot = forestplot, filename = "forestplot.tiff", width = 16.5, height = 12, units = "cm", dpi = 300)



