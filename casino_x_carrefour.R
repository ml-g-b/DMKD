library(dplyr)
library("lubridate") 
library(ggplot2) 
library(GGally) 
library(arules)
library(arulesViz)
library(smooth)
library(class)
library("caret")
source("functions.r")

carrefour <- read.csv(file = "carrefour.txt", sep="\t")
casino <- read.csv(file = "casino.txt", sep="\t")

carrefour$short_ma <- SMA(carrefour$ouv, n = 50)
carrefour$long_ma <- SMA(carrefour$ouv, n = 200)
carrefour$date <- as.Date(carrefour$date, format="%d/%m/%Y")
plot(carrefour$date, carrefour$ouv, lty=2, col="#bbbbbb", ylab = "Adjusted closing price (USD)", xlab = "Dates", main = "Carrefour")
lines(carrefour$date, carrefour$short_ma, col = "red")  # Short-term SMA
lines(carrefour$date, carrefour$long_ma, col = "blue") # Long-term SMA
legend("topleft", legend = c("SMA(50)", "SMA(200)"),
       col = c("red", "blue"), lty = c(1, 1))

casino$short_ma <- SMA(casino$ouv, n = 50)
casino$long_ma <- SMA(casino$ouv, n = 200)
casino$date <- as.Date(casino$date, format="%d/%m/%Y")
plot(casino$date, casino$ouv, lty=2, col="#bbbbbb", ylab = "Adjusted closing price (USD)", xlab = "Dates", main = "Casino")
lines(casino$date, casino$short_ma, col = "red")  # Short-term SMA
lines(casino$date, casino$long_ma, col = "blue") # Long-term SMA
legend("topleft", legend = c("SMA(50)", "SMA(200)"),
       col = c("red", "blue"), lty = c(1, 1))
