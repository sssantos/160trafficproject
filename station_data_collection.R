file_destination <- "/Users/sssantos/Documents/Images/STA160/non_git/d03_text_station_5min_2018_04_25.txt"
df<- read.csv(file_destination, header=F)
df$V2<-as.factor(df$V2) 

# Note from Colin: How do you know a bottleneck exists at sensor?
# df<-df[df$V2 %in% c("314025","318076","318067","318113","316803"),] ##only keep sensors where bottlenecks exist

# Separating first column into date and time
date <- sapply(df$V1, function(x) gsub(" \\d{2}:\\d{2}:\\d{2}", "", as.character(x)))
time <- sapply(df$V1, function(x) gsub("\\d{2}/\\d{2}/\\d{4} ", "", as.character(x)))
df <- data.frame(date_col, time_col, df[-1])

write.csv(df, "station_data_4_25_18")

