df <- read.csv("preprocessing/available_stocks_infos.csv")
industries <- unique(df$Industry)
industries <- aggregate(data=df, df$X ~Industry, function(x) length(unique(x)))
colnames(industries) <- c("Industry", "Number")

industries$Industry[1] <- "Unknwown"

#write.csv(industries, file = "Industries.csv")
indusSorted <- head(industries[order(industries$Number, decreasing = TRUE), ], n=5)

wrap.it <- function(x, len)
{ 
  sapply(x, function(y) paste(strwrap(y, len), 
                              collapse = "\n"), 
         USE.NAMES = FALSE)
}

wrap.labels <- function(x, len)
{
  if (is.list(x))
  {
    lapply(x, wrap.it, len)
  } else {
    wrap.it(x, len)
  }
}

par(mar=c(11, 3, 0, 0))
barplot(indusSorted$Number, 
        names.arg = wrap.labels(indusSorted$Industry, 10),
        las=2,
        col = c("#003a5d"),
        cex.names = 1,
        plot=TRUE)
grid(nx = NA,
     ny = NULL,
     lty = 2, col = "black", lwd = 1)