df<- read.csv("/Users/evanschulze/Desktop/d03_text_station_raw_2018_04_25.txt", header=F)
df$V2<-as.factor(df$V2) 
df<-df[df$V2 %in% c("314025","318076","318067","318113","316803"),] ##only keep sensors where bottlenecks exist

date <- sapply(df$V1, function(x) gsub(" \\d{2}:\\d{2}:\\d{2}", "", as.character(x)))
time <- sapply(df$V1, function(x) gsub("\\d{2}/\\d{2}/\\d{4} ", "", as.character(x)))
df <- data.frame(date, time, df[-1])
date <- as.Date(date, "%m/%d/%y")
time <- chron(times=time, format=c(times="h:m:s"))



class(df$time)

strptime(time[1],"%H:%M:%S")
?chron
