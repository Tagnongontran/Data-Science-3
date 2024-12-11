# Week 08 Activity
library(tidyverse)
library(dplyr)

# Let's download the and get an overview
clim <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv", sep = ";")
str(clim)

# Let's check for missing values 
sum(is.na(clim)) # No missing values

# Let's fix the problem with the variables "altitude" and "p_mean"
# Replace commas with dots and set into numerical plot
clim$altitude <- gsub(",", ".", clim$altitude)
clim$altitude <- as.numeric(clim$altitude)

clim$p_mean <- gsub(",", ".", clim$p_mean)
clim$p_mean <- as.numeric(clim$p_mean)

# Let's plot the map
library(ggplot2) 
install.packages("maps")
library(maps)

states <- map_data("France")

coordinates <- data.frame(
  clim$lon,  # longitudes  
  clim$lat   #  latitudes  
)

ggplot() +  
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "lightblue", color = "black") +   
  geom_point(data = coordinates, aes(x = lon, y = lat), color = "red", size = 3) +  
  coord_fixed(1.3)

 # Exercise 1
# Let's first exclude the two high mountain extremes
climfrar <- clim [1:34,]

# Let's write the model 1 about the Mean Annual Temperature
mdl1 <- lm(t_mean ~ altitude + lat + lon, data = climfrar)
summary(mdl1)
  
# Exercise 2
mdl2 <- predict(model_t_mean, newdata = list("altitude" = c(1212, 2860), "lat" = c(44.2, 42.9)), interval = "p", level = 0.95)
mdl2

# Exercise
scatter_3d <- with(climfrar, scatterplot3d(altitude, lat, t_mean,
                                           pch = 16, highlight.3d = TRUE,
                                           angle = 45,
))
scatter_3d$plane3d(model_t_mean)

summary(model_t_mean)

