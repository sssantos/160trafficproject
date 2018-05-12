# TO MAKE FUTURE USAGE EASIER WHEN UPDATED FROM GITHUB, PLEASE YOUR DATA ADD FILE LOCATION HERE

evan   <- ""
yimeng <- ""
colin  <- "/Users/sssantos/Documents/STA160/non_git/d03_text_station_raw_2018_04_25.txt"
# Switch name here
file <- colin

# PLEASE NOTE ANY REQUIRED PACKAGES TO RUN, THIS CODE NEEDS TO BE ABLE TO BE RUN FROM A CLEARED WORKSPACE
require(ggplot2)


df<- read.csv(file, header=F)
df$V2<-as.factor(df$V2) 
df<-df[df$V2 %in% c("314025","318076","318067","318113","316803"),] ##only keep sensors where bottlenecks exist

df$V1 <- as.POSIXct(df$V1, format='%m/%d/%Y %H:%M:%S')

##subset df into 3PM-8PM only
df <- subset(df, V1 >= as.POSIXct('2018-04-25 15:00:00') &
               V1 <= as.POSIXct('2018-04-25 20:00:00'))


df.1 <- df[df$V2 %in% c("314025"),] ##analyze only one station

cars1=df.1$V3 ##isolate vehicle counts from df
cars2=df.1$V6
cars3=df.1$V9
carsdf=data.frame(cars1,cars2,cars3)
df.1$sums=rowSums(carsdf) ##find sum for all 3 lanes by time
sum(df.1$occ_sums)

strt<-as.POSIXct('2018-04-25 15:00:00')
end<-as.POSIXct('2018-04-25 20:00:00')
##create cumulative frequency plot according to time
ggplot(df.1, aes(x=V1, y=cumsum(sums))) + geom_line() + geom_segment(aes(x=strt,y=0,xend=end,yend=sum(sums)), lty="dashed")+
  theme_bw() + xlab("Time") + ylab("Cumulative Vehicle Counts per 30 seconds") +
  scale_x_datetime(breaks=seq(strt,end,"30 min"), labels = strftime(seq(strt,end,"30 min"), "%H:%M")) + 
  ggtitle('Number of Vehicles Driving Through Station 314025') + theme(plot.title = element_text(hjust = 0.5))

df.1$vph_mean <- rep(sum(df.1$sums)/600, length(df.1$V1))
df.1$vph_diff <- df.1$sums-df.1$vph_mean ##calculate residulas
##oblique plot using flow residulas
ggplot(df.1, aes(x=V1, y=cumsum(vph_diff))) + geom_line() + 
  theme_bw() + xlab("Time") + ylab("Cumulative Vehicle Counts per 30 seconds") +
  scale_x_datetime(breaks=seq(strt,end,"30 min"), labels = strftime(seq(strt,end,"30 min"), "%H:%M")) + 
  ggtitle('Number of Vehicles Driving Through Station 314025') + theme(plot.title = element_text(hjust = 0.5))

occ1=df.1$V4 ##isolate occupancy percentages from df
occ2=df.1$V7
occ3=df.1$V10
occdf=data.frame(occ1,occ2,occ3)
df.1$occ_sums=rowSums(occdf) ##find sum for all 3 lanes
ggplot(df.1, aes(x=V1, y=cumsum(occ_sums))) + geom_line() + geom_segment(aes(x=strt,y=0,xend=end,yend=sum(occ_sums)), lty="dashed")+
  theme_bw() + xlab("Time") + ylab("Lane Occupancy Percentage per 30 seconds") +
  scale_x_datetime(breaks=seq(strt,end,"30 min"), labels = strftime(seq(strt,end,"30 min"), "%H:%M")) +
  ggtitle('Lane Occupancy Percentage Through Station 314025') + theme(plot.title = element_text(hjust = 0.5))

df.1$occ_mean <- rep(sum(df.1$occ_sums)/600, length(df.1$V1)) 
df.1$occ_diff <- df.1$occ_mean-df.1$occ_sums ##calculate residuals
##oblique plot using occupancy residuals
ggplot(df.1, aes(x=V1, y=cumsum(occ_diff))) + geom_line() + 
  theme_bw() + xlab("Time") + ylab("Lane Occupancy Percentage per 30 seconds") +
  scale_x_datetime(breaks=seq(strt,end,"30 min"), labels = strftime(seq(strt,end,"30 min"), "%H:%M")) +
  ggtitle('Lane Occupancy Percentage Through Station 314025') + theme(plot.title = element_text(hjust = 0.5))

##scatterplot of occupancy vs. flow
ggplot(df.1, aes(x=occ_sums, y=sums))+geom_point(shape=1)

##attempt at two Y scales... work in progress
library(latticeExtra)
obj1 <- xyplot(occ_sums ~ V1, df.1, type = "l" , lwd=2)
obj2 <- xyplot(sums ~ V1, df.1, type = "l", lwd=2)
doubleYScale(obj1, obj2, add.ylab2 = TRUE)
