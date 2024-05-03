
symbol_list <- read.csv("preprocessing/available_stocks_infos.csv")

load_csv <- function(s){
  datasettmp <- read.csv(paste("actions/full_history/",s,".csv", sep=""))
  dataset <- data.frame(symbol=rep(s, nrow(datasettmp)))
  return (cbind(dataset, datasettmp))
}

load_sector<- function(sector, df=NULL){
  cat("\033[00;30;107mLoading sector:\033[01;40;97m", sector,"\033[00;30;107m\n")
  sectors <- symbol_list[symbol_list$Sector==sector,]
  symbol_only <- sectors$Symbol
  n <- length(symbol_only)
  pb=txtProgressBar(min=0, max=1, initial=0, style=3)
  i <- 1
  
  if(is.null(df)){
    first_element <- symbol_only[1]
    symbol_only <- symbol_only[2:(length(symbol_only)-1)]
    df <- load_csv(first_element)
    setTxtProgressBar(pb, i/n)
    i <- i+1
  }
  for (symbole in symbol_only) {
    df <- rbind(df, load_csv(symbole))
    setTxtProgressBar(pb, i/n)
    i <- i+1
  }
  setTxtProgressBar(pb, 1)
  close(pb)
  cat("\033[0m")
  return (df)
}

load_multiple_sectors <- function(sector_list){
  df <- NULL
  for (sec in sector_list){
    df <- load_sector(sec, df)
  }
  if(is.null(df)){
    return (df)
  }
  df$date <- as_datetime(df$date)
  return (df)
}

shuffle <- function(dataset){
  dataset[sample(1:nrow(dataset)), ]
}

chunk <- function(dataset, start_indice, end_indice){
  longueur <- nrow(dataset)
  head(tail(dataset, n=longueur-start_indice), n=end_indice-start_indice)
}

vline <- function(x, y=0, col="black", content=NULL){
  abline(v=x, col=col, lty=2)
  if(!(is.null(content))){
    text(x=x-2, y=y, labels=content, srt=90)
  }
}

SMA <- function(x, n) {
  ma <- rep(0, length(x))
  if(length(x) < n){
    return (ma)
  }
  for (i in n:length(x)) {
    ma[i] <- mean(x[(i - n + 1):i])
  }
  return(ma)
}

EMA <- function(x,n){
  ma <- rep(0, length(x))
  ma[1:(n-1)] <- NA
  ma[n]<- mean(x[1:n])
  beta <- 2/(n+1)
  for (i in (n+1):length(x)){
    ma[i]<-beta * x[i] + 
      (1-beta) * ma[i-1]
  }
  return(ma)
}

train_test_set_split <- function(X, Y, test_size=0.2){
  train_index <- sample(1:nrow(X), (1-test_size)*nrow(X))
  train_test_set=list(X_train=X[train_index, ], X_test=X[-train_index, ], Y_train=Y[train_index], Y_test=Y[-train_index])
  return (train_test_set)
}

absm <- function(x){
  if(x<0)
    return (-x)
  return (x)
}

percentage_diff_dataset <- function(row){
  return (percentage_diff(as.numeric(row["short_ma"]),as.numeric(row["long_ma"])))
}

percentage_diff <- function(a,b, negatif=FALSE){
  if(a==b && a==0){
    return (0)
  }
  if(negatif){
    return((a-b)/mean(c(a,b)))
  }
  return (absm(a-b)/mean(c(a,b)))
}

SEUIL <- .05
CLASSES <- c("BUY"=0, "SELL"=1, "HOLD"=2,"UNCLASSIFIED"=3)

buy_sell <- function(row){
  if(is.na(row["long_ma"])){
    return(NULL)
  }
  
  coeff <- percentage_diff(as.numeric(row["short_ma"]),as.numeric(row["long_ma"]))
  if(coeff < SEUIL){
    return ("HOLD")
  }
  if(row["short_ma"] >= row["long_ma"]){
    return ("BUY")
  }
  return ("SELL")
}

classify_df <- function(df){
  symboll <- unique(df$symbol)
  n <- nrow(df)
  m <- length(symboll)
  df <- df[order(df$date, decreasing=FALSE), ]
  df$short_ma <- rep(0, n)
  df$long_ma <- rep(0, n)
  df$very_long_ma <- rep(0, n)
  df$label <- rep("UNCLASSIFIED", n)
  cat("\033[01;41m------ Classifying ------\n")
  pb=txtProgressBar(min=0, max=1, initial=0, style=3)
  i <- 1
  for(s in symboll){
    setTxtProgressBar(pb, i/m)
    indices <- which(df$symbol==s)
    if(length(indices)<20){
      next
    }
  
    adjclose_set <- df[indices, "adjclose"]
    df[indices, "short_ma"] <- SMA(adjclose_set, n = 50)
    df[indices, "long_ma"] <- SMA(adjclose_set, n = 200)
    df[indices, "very_long_ma"] <- SMA(adjclose_set, n = 1000)
    
    df[indices, "label"] <- apply(df[indices,], 1, buy_sell)
    
    i <- i+1
  }
  setTxtProgressBar(pb, 1)
  close(pb)
  cat("\033[0m\n")
  df <- df[order(df$date, decreasing=TRUE), ]
  return (df)
}

best_stocks_for_day <- function(df, datum, ordre="HOLD", num_actions=10){
  df <- df[order(df$date, decreasing=TRUE), ]
  df <- df[df$date==datum,]
  df$potentiel <- rep(0, nrow(df))
  df$potentiel <- apply(df, 1, percentage_diff_dataset)
  best_record <- df[order(df$potentiel, decreasing = TRUE),]
  meaningfull_indices <- which(best_record$label==ordre)
  return (head(best_record[meaningfull_indices,], n=num_actions))
}

best_stocks <- function(df, ordre="HOLD",num_actions=10){
  df <- df[order(df$date, decreasing=TRUE), ]
  unique_symbols <- unique(df$symbol)
  indices <- sapply(unique_symbols, function(symbol) {
    which.max(df$symbol == symbol)
  })
  
  recent_record <- df[indices,]
  recent_record$potentiel <- rep(0, nrow(recent_record))
  recent_record$potentiel <- apply(recent_record, 1, percentage_diff_dataset)
  best_record <- recent_record[order(recent_record$potentiel, decreasing = TRUE),]
  meaningfull_indices <- which(best_record$label==ordre)
  return (head(best_record[meaningfull_indices,], n=num_actions))
}

stock_history <- function(df, stock, n=5){
  return (head(df[df$symbol==stock, ], n=n))
}

stock_of_the_day <- function(df, datum=NULL){
  if(is.null(datum)){
    df <- df[order(df$date, decreasing=TRUE), ]
    unique_symbols <- unique(df$symbol)
    indices <- sapply(unique_symbols, function(symbol) {
      which.max(df$symbol == symbol)
    })
    
    return (df[indices,])
  }
  else{
    return (df[df$date==datum, ])
  }
}

