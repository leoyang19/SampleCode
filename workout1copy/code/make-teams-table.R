# Title: Workout 01- Data Preperation
# Description: This script will prepare the data we will use during our workout
# Input: csv files from github and desktops
# Output: .txt and .csv files regarding NBA and an R script file with a data frame nba, and a new data frame teams 
# Author: Leonard Yang
# Date: 10-3-2018

library(readr) #importing data
library(dplyr) #data wrangling
library(ggplot2) #graphics)
nba = read_csv('/Users/leoyang19/desktop/hw-stat133/workout1/data/nba2018.csv')
View(nba)

nba$experience[nba$experience == "R"]<- 0
nba$experience=as.integer(nba$experience)
nba$salary <- nba$salary/1000000
View(nba)

nba$position <- nba$position <- factor(
  nba$position, labels = c("center", "power_fwd", "point_guard", "small_fwd", "shoot_guard"))

nba = mutate(nba, missed_ft = nba$points1_atts - nba$points1, 
                   missed_fg = nba$field_goals_atts - nba$field_goals, 
                   rebounds = nba$off_rebounds + nba$def_rebounds)

nba = mutate(nba, efficiency = ((nba$points + rebounds + nba$assists + nba$steals + nba$blocks
                                 - missed_fg - missed_ft - nba$turnovers) / nba$games))
                   
View(nba)

sink(file = '../output/efficiency-summary.txt')
summary(nba$efficiency)
sink()

#Creating nba2018-teams.csv

teams=data.frame(summarise(group_by(nba,team), 
                           experience=sum(experience), 
                           salary= round(sum(salary),2),
                           points3=sum(points3),
                           points2=sum(points2),
                           points1=sum(points1),
                           points=sum(points),
                           off_rebounds=sum(off_rebounds),
                           def_rebounds=sum(def_rebounds),
                           assists=sum(assists),
                           steals=sum(steals),
                           blocks=sum(blocks),
                           turnovers=sum(turnovers),
                           fouls=sum(fouls),
                           efficiency=sum(efficiency)
))
View(teams)

sink(file = '../data/teams-summary.txt')
teams
sink()

write_csv(teams, '../data/nba2018-teams.csv')


