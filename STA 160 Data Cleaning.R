df<- read.csv("/Users/evanschulze/Desktop/d03_text_station_raw_2018_04_25.txt", header=F)
df$V2<-as.factor(df$V2) 
df<-df[df$V2 %in% c("314025","318076","318067","318113","316803"),] ##only keep sensors where bottlenecks exist

df$Date <- as.POSIXct(df$V1, format='%d/%m/%Y %H:%M:%S')

##subset df into 3PM-8PM only
df <- setDT(df)[,time:=as.ITime(time)][time>=as.ITime('15:00:00') & time<=as.ITime('20:00:00')]

cars1=df$V3 ##isolate vehicle counts from df
cars2=df$V6
cars3=df$V9
carsdf=data.frame(cars1,cars2,cars3)
df$sums=rowSums(carsdf) ##find sum for all 3 lanes by time

##create cumulative frequency plot according to time
ggplot(data = df, aes(x = time, y = cumsum(sums))) + geom_line() + 
  geom_point() + theme(axis.text.x = element_text(angle=90, hjust = 1)) + 
  scale_x_discrete(labels = df$time) + xlab("time")

occ1=df$V4 ##isolate occupancy percentages from df
occ2=df$V7
occ3=df$V10
occdf=data.frame(occ1,occ2,occ3)
df$occ_sums=rowSums(occdf) ##find sum for all 3 lanes
ggplot(data = df, aes(x = time, y = cumsum(occ_sums))) + geom_line() + 
  geom_point() + theme(axis.text.x = element_text(angle=90, hjust = 1)) + 
  scale_x_discrete(labels = df$time) + xlab("time")

