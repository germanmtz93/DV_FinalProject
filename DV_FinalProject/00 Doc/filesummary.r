require(tidyr)
require(dplyr)
require(ggplot2)

setwd("~/Documents/Academic/CompSci/Data/FinalProject/DV_FinalProject/01 Data")

file_path <- "Batting.reformatted.csv"
df <- read.csv(file_path, stringsAsFactors = FALSE)
summary(df)

file_path <- "HallOfFame.reformatted.csv"
df2 <- read.csv(file_path, stringsAsFactors = FALSE)
summary(df2)

file_path <- "Master.reformatted.csv"
df3 <- read.csv(file_path, stringsAsFactors = FALSE)
summary(df3)

file_path <- "Salaries.csv"
df4 <- read.csv(file_path, stringsAsFactors = FALSE)
summary(df4)

file_path <- "Teams.reformatted.csv"
df5 <- read.csv(file_path, stringsAsFactors = FALSE)
summary(df5)

file_path <- "TeamsFranchises.reformatted.csv"
df6 <- read.csv(file_path, stringsAsFactors = FALSE)
summary(df6)
