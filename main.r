#!/usr/bin/env Rscript
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

df <- NULL
sectors_to_see <- c("Telecommunications")
#sectors_to_see <- c("Energy", "Industrials", "Health Care", "Finance")
for(sec in sectors_to_see){
  df <- load_multiple_sectors(c(sec))
  
  df_med <- df %>% group_by(date) %>% summarize(adjclose = median(adjclose), open = median(open), close = median(close), high = median(high), low = median(low))
  df_med$short_ma <- SMA(df_med$adjclose, n = 50)
  df_med$long_ma <- SMA(df_med$adjclose, n = 200)
  df_med$very_long_ma <- SMA(df_med$adjclose, n = 1000)
  
  plot(df_med$date, df_med$adjclose, lty=2, col="#bbbbbb", ylab = "Adjusted closing price (USD)", xlab = "Dates", main = sec)
  lines(df_med$date, df_med$short_ma, col = "red")  # Short-term SMA
  lines(df_med$date, df_med$long_ma, col = "blue") # Long-term SMA
  lines(df_med$date, df_med$very_long_ma, col = "#000000") # Long-term SMA
  legend("topleft", legend = c("SMA(50)", "SMA(200)", "SMA(1000)"),
         col = c("red", "blue", "#000000"), lty = c(1, 1, 1))
  
  df <- classify_df(df)
  filename <- paste0("top_10_buy_",sec,".txt")
  file.create(filename)
  cat("\tFile",filename,"created !\n")
  cat("TOP 10 actions to BUY\n")
  bestbuy <- best_stocks(df, ordre="BUY")
  print(bestbuy)
  write.table(bestbuy, filename, append = TRUE)

  filename <- paste0("top_10_sell_",sec,".txt")
  file.create(filename)
  cat("\tFile",filename,"created !\n")
  cat("TOP 10 actions to SELL\n")
  bestsell <- best_stocks(df, ordre="SELL")
  write.table(bestsell, filename, append = TRUE)
  print(bestsell)
  
  ajd <- stock_of_the_day(df)
  labels_prop <- ajd %>% group_by(label) %>% summarise(total_count=n(), .groups = 'drop')
  labels_prop <- labels_prop[order(labels_prop$total_count, decreasing=TRUE),]
  barplot(height=labels_prop$total_count, names.arg = labels_prop$label,space = 0, col = c("#ff8300","#d03f15","#0077b3","#007254"), main = paste0("Labels for ",sec))
  grid(nx = NA, ny = NULL, lty = 2, col = "#aaaaaa", lwd = 1)
  
  df$label <- as.numeric(CLASSES[df$label])
  echantillon <- chunk(df, 1, 15000)
  
  train_test_set <- train_test_set_split(df[,3:(length(df)-1)], df$label)
  # REGRESSION
  model <- lm(train_test_set$Y_train ~ ., data=train_test_set$X_train)
  predictions <- predict(model, newdata=train_test_set$X_test)
  mse <- mean((predictions - train_test_set$Y_test)^2)
  
  cat("For regression, we have a mean squared error of", mse)
  
  train_test_set <- train_test_set_split(echantillon[,3:(length(echantillon)-1)], echantillon$label)
  # RANDOM FORESTS
  library(randomForest)
  model <- randomForest(x=train_test_set$X_train, y=train_test_set$Y_train)
  predictions <- predict(model, newdata=train_test_set$X_test)
  accuracy <- mean(predictions==train_test_set$Y_test)
  imp <- importance(model)
  cat("For random forest, we have an accuracy of", accuracy)
}

# rules <- apriori(data = df, parameter=list(support=.1, confidence=0.8))
# 
# plot(rules, shading="order")
# 
# subrules <- rules[quality(rules)$confidence > 0.95]
# plot(subrules, method="matrix", measure="lift")
# 
# write(rules, file="rules.out")

#ggpairs(chunk(shuffle(df[,2:length(df)-1]), 1, 10000))

#sorted_df <- df[order(df$date,decreasing = TRUE), ]
#date_count <- aggregate(data=sorted_df, sorted_df$symbol ~date, function(x) length(unique(x)))

#means <- sorted_df %>% group_by(date) %>% summarize(med_open = mean(open),med_close = mean(close))
#means$date <- as_datetime(means$date)

#local_chunck <- chunk(means, 5000, 12740)
#local_chunck[local_chunck$mean_open >= max(local_chunck$mean_open, na.rm = TRUE),]
# 
# df <- load_multiple_sectors(c("Energy"))
# df_med <- df %>% group_by(date) %>% summarize(adjclose = median(adjclose), open = median(open), close = median(close), high = median(high), low = median(low))
#plot(local_chunck$date,local_chunck$med_open,type="l", xlab="Date", ylab="Mean Open Price", main="Mean Open Price Over Time", col="blue", ylim = c(min(local_chunck$med_open)-15,max(local_chunck$med_open)))
#lines(local_chunck$date,local_chunck$med_close, col="red")
#grid(nx = NA,ny = NULL,lty = 2, col = "#aaaaaa", lwd = 1)
#vline(as_datetime("2008-09-15"), y=2, content="Global Financial\nCrisis 2008")
#vline(as_datetime("2020-03-13"), y=2, content="\nCOVID-19")
#vline(as_datetime("2019-12-15"), y=2, content="COVID-19 first sign\n")
#vline(as_datetime("2015-06-24"), y=2, content="Start of Stock crisis\n")
#vline(as_datetime("2016-06-24"), y=2, content="\nEnd of Stock crisis")
# 
# means <- sorted_df %>% group_by(date) %>% summarize(med_open = median(open),med_close = median(close))
# means$date <- as_datetime(means$date)
# 
# local_chunck <- chunk(means, 5000, nrow(means)-1)
#local_chunck[local_chunck$mean_open >= max(local_chunck$mean_open, na.rm = TRUE),]

# plot(df_med$date,df_med$adjclose,type="l", xlab="Date", ylab="Median Open Price", col="#ff8300", ylim = c(min(df_med$adjclose)-10,max(df_med$adjclose)))
# grid(nx = NA,
#      ny = NULL,
#      lty = 2, col = "#aaaaaa", lwd = 1)
# vline(as_datetime("2008-09-15"), y=2, content="Global Financial\nCrisis 2008")
# vline(as_datetime("2014-11-24"), y=2, content="OPEC decides to\nmaintain production")
# vline(as_datetime("2019-12-24"), y=2, content="\nCovid-19 Crisis")
# vline(as_datetime("1991-12-08"), y=2, content="USSR dissolution\n")
# vline(as_datetime("2003-03-20"), y=2, content="Iraq invasion\n")
# 
# stock <- df[df$symbol=="AE",]
# stock <- stock[order(stock$date, decreasing=FALSE), ]
# 
# stock$short_ma <- SMA(stock$adjclose, n = 50)
# stock$long_ma <- SMA(stock$adjclose, n = 200)
# stock$very_long_ma <- SMA(stock$adjclose, n = 1000)
# 
# trading_strategy <- function(row){
#   return (row["short_ma"] > row["long_ma"] && row["short_ma"] > row["very_long_ma"])
# }
# 
# stock$label <- rep(0, nrow(stock))
# stock$label <- apply(stock, 1, trading_strategy)
# 
# train_test_set <- train_test_set_split(stock[,3:length(stock)-1], stock$label)
# 
# # RANDOM FORESTS
# library(randomForest)
# model <- randomForest(x=train_test_set$X_train, y=train_test_set$Y_train)
# predictions <- predict(model, newdata=train_test_set$X_test)
# accuracy <- mean(predictions==train_test_set$Y_test)
# imp <- importance(model)
# 
# # REGRESSION
# model <- lm(train_test_set$Y_train ~ ., data=train_test_set$X_train)
# predictions <- predict(model, newdata=train_test_set$X_test)
# mse <- mean((predictions - train_test_set$Y_test)^2)
# rsquared <- 1 - (sum((train_test_set$Y_test - predictions)^2) / sum((train_test_set$Y_test - mean(train_test_set$Y_test))^2))

#stock$ema <- EMA(stock$adjclose, 50)
# stock$label <- apply(stock, 1, buy_sell)


#plot(local_chunck$date, local_chunck$med_open - local_chunck$med_close , type="l", xlab="Date", ylab="Net difference price", main="Difference between median opening and closing price", col="black")
# 
# df$date <- as_datetime(df$date)
# ggpairs(chunk(shuffle(df[,2:length(df)-1]), 1, 100000))
