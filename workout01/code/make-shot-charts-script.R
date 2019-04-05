# Title: 
# Description: 
# Inputs: 
# Outputs: 

library(ggplot2)
library(grid)
library(jpeg)

curry = read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)
thompson = read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)
durant = read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
green = read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
iguodala = read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)
shots = read.csv("../data/shots-data.csv", stringsAsFactors = FALSE)

court_file = "../images/nba-court.jpg"
court_image = rasterGrob(readJPEG(court_file), width = unit(1, "npc"), height = unit(1, "npc"))

curry_shot_chart = ggplot(data = curry) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') + theme_minimal()
ggsave('../images/stephen-curry-shot-chart.pdf', device="pdf", width=6.5, height=5)

thompson_shot_chart = ggplot(data = thompson) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') + theme_minimal()
ggsave('../images/klay-thompson-shot-chart.pdf', device="pdf", width=6.5, height=5)

durant_shot_chart = ggplot(data = durant) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)') + theme_minimal()
ggsave('../images/kevin-durant-shot-chart.pdf', device="pdf", width=6.5, height=5)

green_shot_chart = ggplot(data = green) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') + theme_minimal()
ggsave('../images/draymond-green-shot-chart.pdf', device="pdf", width=6.5, height=5)

iguodala_shot_chart = ggplot(data = iguodala) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') + theme_minimal()
ggsave('../images/andre-iguodala-shot-chart.pdf', device="pdf", width=6.5, height=5)

shot_charts = ggplot(data = shots) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Charts: GSW (2016 season)') + theme(legend.position="top") + facet_wrap(~ name, ncol=3)
ggsave('../images/gsw-shot-charts.pdf', device="pdf", width=8, height=7)
ggsave('../images/gsw-shot-charts.png', device="png", width=8, height=7)

