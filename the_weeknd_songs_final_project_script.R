## Project:  STA 215, Spring 2024, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      4/29/2024
# Who:       Athena Didizian

## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")

##################################################################################
####################### Table 1: Descriptive Statistics    #######################  
##################################################################################
# Streams per song
mean(data$streams)
sd(data$streams)
hist(data$streams)
table(data$streams)
describe(data$streams)
summary(data$streams)
min(data$streams)
max(data$streams)

# Emotional Tone for songs
mean(data$emotional_tone)
sd(data$emotional_tone)
hist(data$emotional_tone)
table(data$emotional_tone)
describe(data$emotional_tone)
summary(data$emotional_tone)
min(data$emotional_tone)
max(data$emotional_tone)

# Energy Level for songs
mean(data$energy_level)
sd(data$energy_level)
hist(data$energy_level)
table(data$energy_level)
describe(data$energy_level)
summary(data$energy_level)
min(data$energy_level)
max(data$energy_level)

#Summary Statistics for Streams
table(data$streams)

#Summary Statistics for Featured Artists
table(data$featured_artists)

#Summary Statistics for Genre
table(data$genre)

##################################################################################
#######################  Table 2: Contingency Table   ############################   
##################################################################################
table(data$featured_artists, data$genre)
chisq.test(data$featured_artists, data$genre)

##################################################################################
##########m################ Figure 1: Boxplot #################################### 
##################################################################################
# Box Plot for Featured Artists by Emotional Tone
ggplot(raw_data, aes(x = data$genre, y = data$emotional_tone)) +
  geom_boxplot() +
  labs(title = "Box Plot of Featured Artists by Emotional Tone",
       x = "Genre",
       y = "Emotional Tone") +
  theme_minimal()

##################################################################################
##########################  Figure 2: Scatter Plot  ##############################   
##################################################################################
#### Scatter Plot for Emotional Tone by Energy Level
linear_plot <- plot(data$emotional_tone, data$energy_level)
print(linear_plot)

# add x line and y line for means
meany <- mean(data$emotional_tone)
meanx <- mean(data$energy_level)

abline(h = meanx, col = "black")
abline(v = meany, col = "black")


# STEP 2: Calculate linear regression line (i.e., slope) and add to scatter plot
linear_relationship <- lm(data$emotional_tone ~ data$energy_level, data = raw_data)
summary(linear_relationship)

# Add the linear regression line to the scatter plot
# NOTE: double check the scatter plot is currently in your utilities window!
abline(linear_relationship, col = "red")

##################################################################################
#########################  Figure 3: Residual Plot  ############################## 
##################################################################################
# Plot the residuals
plot(raw_data$energy_level, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")
