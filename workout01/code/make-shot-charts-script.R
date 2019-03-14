# Title: Workout 1 Shot Charts
# Description: Code for creating player shot charts
# Input(s): Player data frames made using make-shots-data-script.R
# Output(s): Shot chart visualizations for individual players

## importing packages
install.packages("jpeg")
install.packages("grid")
library(jpeg)
library(grid)
library(ggplot2)
library(dplyr)

## importing court image (to be used as background of plots)

court_file <- "./images/nba-court.jpg"

## creating raste object from image

court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc")
)

## creating shot charts for each player

iguodala_chart <- ggplot(data = iguodala) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle("Shot Chart: Andre Iguodala (2016 Season)") +
  theme_minimal()

green_chart <- ggplot(data = green) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle("Shot Chart: Draymond Green (2016 Season)") +
  theme_minimal()

durant_chart <- ggplot(data = durant) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle("Shot Chart: Kevin Durant (2016 Season)") +
  theme_minimal()

thompson_chart <- ggplot(data = thompson) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle("Shot Chart: Klay Thompson (2016 Season)") +
  theme_minimal()

curry_chart <- ggplot(data = curry) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle("Shot Chart: Stephen Curry (2016 Season)") +
  theme_minimal()

## creating faceted shot chart

faceted_charts <- ggplot(data = shots_data, aes(x = x, y = y)) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle("Shot Charts: GSW (2016 season)") +
  facet_wrap(~ name) +
  theme_minimal()

## saving plot as PDF in local repository

ggsave(filename = "./images/gsw-shot-charts.pdf",
       plot = faceted_charts,
       width = 8, height = 7)

png(filename = "./images/gsw-shot-charts.png", res = 75)
faceted_charts
dev.off()
