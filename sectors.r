library(ggplot2)

df <- read.csv("preprocessing/available_stocks_infos.csv")
sectors <- unique(df$Sector)
sectors <- aggregate(data=df, df$X ~Sector, function(x) length(unique(x)))
colnames(sectors) <- c("Name", "Number")

total <- sum(sectors_number$Number)
#sectors$Number <- sectors$Number/total
sectors$Name[1] <- "Unknwown"

par(mar=c(11, 3, 3, 0))
barplot(sectors$Number, 
        names.arg = sectors$Name,
        las=2,
        col = c("#003a5d"),
        cex.names = 1,
        plot=TRUE)
grid(nx = NA,
     ny = NULL,
     lty = 2, col = "black", lwd = 1)

par(mar=c(0, 0, 0, 0))
pie(sectors$Number, labels=sectors$Name, radius = 0.8, col=rainbow(length(sectors$Number)))


