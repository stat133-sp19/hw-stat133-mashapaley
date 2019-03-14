# Title: Workout 1 Data Preparation
# Description: Code for creating new .csv data file from raw data for visualization
# Input(s): Must have access to raw data files 
# Output(s): A new .csv file with modified data formatted for visualization, data summaries

## importing packages

install.packages(dplyr)
library(dplyr)

## reading in raw data

curry <- read.csv("./data/stephen-curry.csv", stringsAsFactors = F)
thompson <- read.csv("./data/klay-thompson.csv", stringsAsFactors = F)
durant <- read.csv("./data/kevin-durant.csv", stringsAsFactors = F)
green <- read.csv("./data/draymond-green.csv", stringsAsFactors = F)
iguodala <- read.csv("./data/andre-iguodala.csv", stringsAsFactors = F)

## adding column for player name

curry <- mutate(curry, name = "Stephen Curry")
thompson <- mutate(thompson, name = "Klay Thompson")
durant <- mutate(durant, name = "Kevin Durant")
green <- mutate(green, name = "Draymond Green")
iguodala <- mutate(iguodala, name = "Andre Iguodala")

## changing values of shot_made_flag

curry$shot_made_flag[curry$shot_made_flag == "y"] <- "shot_yes"
curry$shot_made_flag[curry$shot_made_flag == "n"] <- "shot_no"
thompson$shot_made_flag[thompson$shot_made_flag == "y"] <- "shot_yes"
thompson$shot_made_flag[thompson$shot_made_flag == "n"] <- "shot_no"
durant$shot_made_flag[durant$shot_made_flag == "y"] <- "shot_yes"
durant$shot_made_flag[durant$shot_made_flag == "n"] <- "shot_no"
green$shot_made_flag[green$shot_made_flag == "y"] <- "shot_yes"
green$shot_made_flag[green$shot_made_flag == "n"] <- "shot_no"
iguodala$shot_made_flag[iguodala$shot_made_flag == "y"] <- "shot_yes"
iguodala$shot_made_flag[iguodala$shot_made_flag == "n"] <- "shot_no"

## adding variable for minute number of shot

curry$minute <- (curry$period * 12) - curry$minutes_remaining
thompson$minute <- (thompson$period * 12) - thompson$minutes_remaining
durant$minute <- (durant$period * 12) - durant$minutes_remaining
green$minute <- (green$period * 12) - green$minutes_remaining
iguodala$minute <- (iguodala$period * 12) - iguodala$minutes_remaining

## sending summaries to local repository

sink(file = './output/andre-iguodala-summary.txt')
summary(iguodala)
sink()

sink(file = './output/draymond-green-summary.txt')
summary(green)
sink()

sink(file = './output/kevin-durant-summary.txt')
summary(durant)
sink()

sink(file = './output/klay-thompson-summary.txt')
summary(thompson)
sink()

sink(file = './output/stephen-curry-summary.txt')
summary(curry)
sink()

## combining raw data into single data frame

shots_data <- rbind(curry, thompson, durant, green, iguodala)

## sending data frame to local repository as .csv

write.csv(
  x = shots_data,
  file = './data/shots-data.csv'
)

## sending summary to local repository

sink(file = './output/shots-data-summary.txt')
summary(shots_data)
sink()
