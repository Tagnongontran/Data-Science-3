# Week 08 Activity
library(tidyverse)
library(dplyr)

# Let's download the and get an overview
clim <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv", sep = ";")
str(clim)

# Let's check for missing values 
sum(is.na(clim)) # No missing values

# Let's fix the problem with the variables "altitude" and "p_mean"
# Remove commas and set into numerical plot
clim$altitude <- gsub(",", "", clim$altitude)
clim$altitude <- as.numeric(clim$altitude)

clim$p_mean <- gsub(",", "", clim$p_mean)
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

# Exercise 1: Lets test all three spatial attributes, i.e latitude, longitude, and altitude, as explanatorz variables for the mean annual temperature
# Let's first exclude the two high mountain extremes
climfrar <- clim [1:34,]

# Let's write the model 1 about the Mean Annual Temperature
mdl1 <- lm(t_mean ~ altitude + lat + lon, data = climfrar)
mdl1 
#Coefficients:
# (Intercept)     altitude          lat          lon  
#37.265036    -0.006414    -0.533960     0.032101  

# Let's now write the model 1 about the Mean Annual Temperature:
# Mean Annual Temperature = 37.265+(−0.006)×Altitude+(−0.534)×Latitude+0.032×Longitude

summary(mdl1) # Let's look at the test of significance
#Call:
# lm(formula = t_mean ~ altitude + lat + lon, data = climfrar)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.76492 -0.32755  0.04337  0.24787  2.58927 
#Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 37.2650364  2.6220099  14.212 7.29e-15 ***
# altitude    -0.0064139  0.0008688  -7.383 3.17e-08 ***
#  lat         -0.5339603  0.0557546  -9.577 1.24e-10 ***
# lon          0.0321010  0.0395728   0.811    0.424    
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 0.7308 on 30 degrees of freedom
#Multiple R-squared:  0.8329,	Adjusted R-squared:  0.8162 
#F-statistic: 49.84 on 3 and 30 DF,  p-value: 9.112e-12


# Exercise 2:
# Let's exclude the non-significant variable and predict the temperature for Mont-Ventoux and Pic-du-midi. 
mdl2 <- lm(t_mean ~ altitude + lat, data = climfrar)
summary(mdl2)

# Let's predict the temperature for Mont-Ventoux and Pic-du-midi. 
prediction <- predict(mdl2, newdata = list("altitude" = c(1212, 2860), "lat" = c(44.16, 42.93)), interval = "p", level = 0.95)
prediction

summary(prediction) # Let's look at the test of significance

# Let's compare the measured means concerning the prediction confidence bounces.
# We can see that the mean for Mont-Ventoux is 6.19°𝐶 with a confidence interval equals [3.81°𝐶,8.56°𝐶] whereas the measured mean is 3.6°𝐶
# The model is not accurate in predicting the temperature for Mont-Ventoux because the actual value is out of the confidence interval fo 95%
# Furthermore the prediction for Pic-du-midi is -3.46°𝐶  with a confidence interval equals [−8.36°𝐶,1.44°𝐶]
# Since the actual is −1.2°𝐶 and falls into the confidence interval of 95%, we conclude that the model is accurate in predicting the temperature for Pic-du-midi.

# Exercise 3: Model results evaluation by
# 3D scatter plot
install.packages("scatterplot3d")
library(scatterplot3d)
splot <- with(climfrar, scatterplot3d(altitude, lat, t_mean,
                                      pch = 16, highlight.3d = TRUE,
                                      angle = 45,
                                      xlab = "Altitude (m)",     # Label for x-axis  
                                      ylab = "Latitude (°)",     # Label for y-axis  
                                      zlab = "Mean Temperature (°C)", # Label for z-axis  
                                      main = "3D Scatter Plot of Temperature by Altitude and Latitude", # Title of the plot  
))
splot$plane3d(mdl2)

# Summary output
summary(mdl2)
#  lm(formula = t_mean ~ altitude + lat, data = climfrar)
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.79206 -0.27571 -0.00556  0.30536  2.71871 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 37.9147567  2.4828724   15.27 5.68e-16 ***
#  altitude    -0.0062643  0.0008443   -7.42 2.34e-08 ***
#  lat         -0.5465325  0.0532610  -10.26 1.72e-11 ***
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 0.7268 on 31 degrees of freedom
#Multiple R-squared:  0.8292,	Adjusted R-squared:  0.8182 
#F-statistic: 75.26 on 2 and 31 DF,  p-value: 1.268e-12

# The smallest difference between what our model predicts and what we actually measured is −1.79°C, and the largest difference is 2.72°C.
# For a good model, we want the differences (residuals) to be evenly spread around zero, but in this case, they aren't completely even. This means that the model sometimes makes predictions that are quite different from the actual measurements.
# The Residual Standard Error shows how our predictions can deviate on average. Here, it tells us that our predictions can deviate by about 0.73°C from the actual temperatures.
# Since the average temperature in our data is 11.68°C, this average error represents about 6.2%.
# Lastly, the adjusted R² value is 0.82, which means that about 82% of the changes we see in the average temperature can be explained by the factors we considered, like latitude and altitude.
