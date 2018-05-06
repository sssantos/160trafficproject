df<- read.csv("/Users/evanschulze/Desktop/d03_text_station_raw_2018_04_25.txt", header=F)
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

##create cumulative frequency plot according to time
ggplot(df.1, aes(x=strptime(time, "%H:%M:%S"), y=cumsum(sums))) + geom_line() +
  theme_bw() + xlab("Time") + ylab("Activity (Vehicle Counts per 30 seconds)") +
  scale_x_datetime(breaks=date_breaks("30 min"), labels = date_format("%H:%M"))


occ1=df$V4 ##isolate occupancy percentages from df
occ2=df$V7
occ3=df$V10
occdf=data.frame(occ1,occ2,occ3)
df$occ_sums=rowSums(occdf) ##find sum for all 3 lanes
ggplot(df, aes(x=strptime(time, "%H:%M:%S"), y=cumsum(occ_sums))) + geom_line() +
  theme_bw() + xlab("Time") + ylab("Activity (Lane Occupancy per 30 seconds)") +
  scale_x_datetime(breaks=date_breaks("30 min"), labels = date_format("%H:%M"))
sum(df$occ_sums)
