install.packages("gdata")
install.packages("forcats")
install.packages("tidyr")
install.packages("DataComputing")
install.packages("plotrix")
install.packageS("corrplot")
library(gdata)
library(forcats)
library(tidyr)
library(DataComputing)
library(plotrix)
library(corrplot)

#data loading
data_24E_4 <- as.data.frame(read.xls("Freeway_flow_24E_20070401.xls"))
data_24E_5 <- as.data.frame(read.xls("Freeway_flow_24E_20070501.xls"))
data_24W_4 <- as.data.frame(read.xls("Freeway_flow_24W_20070401.xls"))
data_24W_5 <- as.data.frame(read.xls("Freeway_flow_24W_20070501.xls"))
data_80E_4 <- as.data.frame(read.xls("Freeway_flow_80E_20070401.xls"))
data_80E_5 <- as.data.frame(read.xls("Freeway_flow_80E_20070501.xls"))
data_80W_4 <- as.data.frame(read.xls("Freeway_flow_80W_20070401.xls"))
data_80W_5 <- as.data.frame(read.xls("Freeway_flow_80W_20070501.xls"))
data_580E_4 <- as.data.frame(read.xls("Freeway_flow_580E_20070401.xls"))
data_580E_5 <- as.data.frame(read.xls("Freeway_flow_580E_20070501.xls"))
data_580W_4 <- as.data.frame(read.xls("Freeway_flow_580W_20070401.xls"))
data_580W_5 <- as.data.frame(read.xls("Freeway_flow_580W_20070501.xls"))
data_880N_4 <- as.data.frame(read.xls("Freeway_flow_880N_20070401.xls"))
data_880N_5 <- as.data.frame(read.xls("Freeway_flow_880N_20070501.xls"))
data_880S_4 <- as.data.frame(read.xls("Freeway_flow_880S_20070401.xls"))
data_880S_5 <- as.data.frame(read.xls("Freeway_flow_880S_20070501.xls"))
data_980E_4 <- as.data.frame(read.xls("Freeway_flow_980E_20070401.xls"))
data_980E_5 <- as.data.frame(read.xls("Freeway_flow_980E_20070501.xls"))
data_980W_4 <- as.data.frame(read.xls("Freeway_flow_980W_20070401.xls"))
data_980W_5 <- as.data.frame(read.xls("Freeway_flow_980W_20070501.xls"))

#data cleaning
instant_data <- as.data.frame(data_880S_4[662:685,c(1,11)])
instant_data <- mutate(instant_data, X400659.ML = gsub(",","",X400659.ML))
instant_data <- mutate(instant_data, date = substr(as.character(Hour),1,4), hour = substr(as.character(Hour),11,12), 
                       X400659.ML = as.numeric(as.character(X400659.ML)))
instant_data <- mutate(instant_data, hour = gsub(":","",hour))

#plot reflects the accident
plot_instant <- ggplot(instant_data, aes(x = hour, y = X400659.ML)) + geom_bar(stat='identity') + aes(x = fct_inorder(hour))
plot_instant + labs(title ="Sudden Drop of Traffic Flow after 2007 I-880 Accident", 
                    y = "Traffic Flow", x = "Time") + annotate("text", x=15, y=1300, label= "accident at 3:41 am", color = "red") + annotate("pointrange", x = 15.5, y = 1200, ymin = 0, ymax = 1200, colour = "red")


#sensors precedes the accident location
data_prec <- rbind(as.data.frame(data_880S_4[,c(1,12,13,14)]), as.data.frame(data_880S_5[,c(1,12,13,14)]))
data_prec <- mutate(data_prec, X400242.ML = gsub(",","",X400242.ML), X401156.ML = gsub(",","",X401156.ML), X401562.ML = gsub(",","",X401562.ML))
data_prec <- mutate(data_prec, date = substr(as.character(Hour),1,4), hour = substr(as.character(Hour),11,12), X400242.ML = as.numeric(as.character(X400242.ML)), X401156.ML = as.numeric(as.character(X401156.ML)), X401562.ML = as.numeric(as.character(X401562.ML)))
data_prec <- mutate(data_prec, hour = gsub(":","",hour))
data_prec$mean = rowMeans(data_prec[2:4])
prec <- filter(data_prec, hour %in% c(15,16,17,18))
ggplot(prec, aes(x = date, y = mean, group = factor(hour))) + geom_point(aes(color = factor(hour))) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Rush Hour Traffic Flow on I-880 Time Series, Ahead") 

#sensors follows the accident location
data_rear <- rbind(as.data.frame(data_880S_4[,c(1,8,9,10)]), as.data.frame(data_880S_5[,c(1,8,9,10)]))
data_rear <- mutate(data_rear, X401708.ML = gsub(",","",X401708.ML), X400454.ML = gsub(",","",X400454.ML), X400093.ML = gsub(",","",X400093.ML))
data_rear <- mutate(data_rear, date = substr(as.character(Hour),1,4), hour = substr(as.character(Hour),11,12), X401708.ML = as.numeric(as.character(X401708.ML)), X400454.ML = as.numeric(as.character(X400454.ML)), X400093.ML = as.numeric(as.character(X400093.ML)))
data_rear <- mutate(data_rear, hour = gsub(":","",hour))
data_rear$mean = rowMeans(data_rear[2:4])
rear <- filter(data_rear, hour %in% c(15,16,17,18))
ggplot(rear, aes(x = date, y = mean, group = factor(hour))) + geom_point(aes(color = factor(hour))) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Rush Hour Traffic Flow on I-880 Time Series, After") 

#immediate reaction, compared with weeks before and after, ahead
immi_prec <- data_prec[c(536:549, 704:717, 872:885, 1040:1053, 1208:1221),]
ggplot(immi_prec, aes(x = fct_inorder(hour), y = mean, group = factor(date))) + geom_point(aes(color = factor(date))) + geom_line() + labs(title ="Traffic Flow Change Following the Accident, ahead", 
                                                                                                                                           y = "Traffic Flow", x = "Hour")

#immediate reaction, compared with weeks before and after, after
immi_after <- data_rear[c(536:549, 704:717, 872:885, 1040:1053, 1208:1221),]
ggplot(immi_after, aes(x = fct_inorder(hour), y = mean, group = factor(date))) + geom_point(aes(color = factor(date))) + geom_line() + labs(title ="Traffic Flow Change Following the Accident, after", 
                                                                                                                                            y = "Traffic Flow", x = "Hour")

#580 accident, ahead
data_580_ahead <- rbind(as.data.frame(data_580E_4[,c(1,8)]), as.data.frame(data_580E_5[,c(1,8)]))
data_580_ahead <- mutate(data_580_ahead, X401218.ML = gsub(",","",X401218.ML))
data_580_ahead <- mutate(data_580_ahead, date = substr(as.character(Hour),1,4), hour = substr(as.character(Hour),11,12), X401218.ML = as.numeric(as.character(X401218.ML)))
data_580_ahead <- mutate(data_580_ahead, hour = gsub(":","",hour))
ahead_580 <- filter(data_580_ahead, hour %in% c(15,16,17,18))
ggplot(ahead_580, aes(x = date, y = X401218.ML, group = factor(hour))) + geom_point(aes(color = factor(hour))) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title ="Rush Hour Traffic Flow on I-580 Time Series, Ahead", 
                                                                                                                                                                                               y = "Traffic Flow", x = "Date")

#580 accident, after
data_580_prec <- rbind(as.data.frame(data_580E_4[,c(1,7)]), as.data.frame(data_580E_5[,c(1,7)]))
data_580_prec <- mutate(data_580_prec, X400075.ML = gsub(",","",X400075.ML))
data_580_prec <- mutate(data_580_prec, date = substr(as.character(Hour),1,4), hour = substr(as.character(Hour),11,12), X400075.ML = as.numeric(as.character(X400075.ML)))
data_580_prec <- mutate(data_580_prec, hour = gsub(":","",hour))
comp_580 <- data_580_prec %>% group_by(date) %>% summarise(route_580 = sum(X400075.ML))
comp_580 <- comp_580[6:61,]
prec_580 <- filter(data_580_prec, hour %in% c(15,16,17,18))
ggplot(prec_580, aes(x = date, y = X400075.ML, group = factor(hour))) + geom_point(aes(color = factor(hour))) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title ="Rush Hour Traffic Flow on I-580 Time Series, After", 
                                                                                                                                                                                              y = "Traffic Flow", x = "Date")
#80E traffic flow change as a result of 880/580 accident
data_80 <- rbind(as.data.frame(data_80E_4[,c(1,4)]), as.data.frame(data_80E_5[,c(1,4)]))
data_80 <- mutate(data_80, X401671.ML = gsub(",","",X401671.ML))
data_80 <- mutate(data_80, date = substr(as.character(Hour),1,4), hour = substr(as.character(Hour),11,12), X401671.ML = as.numeric(as.character(X401671.ML)))
data_80 <- mutate(data_80, hour = gsub(":","",hour))
prec_80 <- data_80 %>% group_by(date) %>% summarise(route_80 = sum(X401671.ML))
prec_80 <- prec_80[6:61,]
day <- rep(c("Weekday","Weekend","Weekend","Weekday","Weekday","Weekday","Weekday"),8)
prec_80_day <- cbind(prec_80,day)
ggplot(prec_80_day, aes(x = date, y = flow_80, group = factor(day))) + geom_point(aes(color = factor(day))) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(prec_80, aes(x = date, y = flow_80, group = 1)) + geom_point() + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#880 with needed sensor point to compare with 580/80
data_880_prec <- rbind(as.data.frame(data_880S_4[,c(1,10)]), as.data.frame(data_880S_5[,c(1,10)]))
data_880_prec <- mutate(data_880_prec, X400093.ML = gsub(",","",X400093.ML))
data_880_prec <- mutate(data_880_prec, date = substr(as.character(Hour),1,4), hour = substr(as.character(Hour),11,12), X400093.ML = as.numeric(as.character(X400093.ML)))
data_880_prec <- mutate(data_880_prec, hour = gsub(":","",hour))
comp_880 <- data_880_prec %>% group_by(date) %>% summarise(route_880 = sum(X400093.ML))
comp_880 <- comp_880[6:61,]

#combine 880/580/80
detour <- merge(prec_80, comp_880)
alt_main <- merge(detour, comp_580)
alt_main_tidy <- alt_main %>% gather(route, flow, route_80:route_580)
alt_main_good <- alt_main[c(1:23,33:48),]
alt_main_tidy_good <- alt_main_good %>% gather(route, flow, route_80:route_580)
ggplot(alt_main_tidy_good, aes(x=date, y=flow, group=route)) + geom_point(aes(color = route)) + geom_line() + geom_smooth(method="lm", fill=NA) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("How Traffic Flow from I-580 Diverges to I-80 & I-880")

#net 580E flow from bay bridge
before_80 <- rbind(as.data.frame(data_80W_4[,c(1,5)]), as.data.frame(data_80W_5[,c(1,5)]))
before_80 <- mutate(before_80, X401698.ML = gsub(",","",X401698.ML))
before_80 <- mutate(before_80, date = substr(as.character(Hour),1,4), hour = substr(as.character(Hour),11,12), X401698.ML = as.numeric(as.character(X401698.ML)))
before_80 <- mutate(before_80, hour = gsub(":","",hour))
before_80 <- before_80 %>% group_by(date) %>% summarise(bef_80 = sum(X401698.ML))
before_80 <- before_80[6:61,]

after_80 <- rbind(as.data.frame(data_80W_4[,c(1,3)]), as.data.frame(data_80W_5[,c(1,3)]))
after_80 <- mutate(after_80, X401535.ML = gsub(",","",X401535.ML))
after_80 <- mutate(after_80, date = substr(as.character(Hour),1,4), hour = substr(as.character(Hour),11,12), X401535.ML = as.numeric(as.character(X401535.ML)))
after_80 <- mutate(after_80, hour = gsub(":","",hour))
after_80 <- after_80 %>% group_by(date) %>% summarise(aft_80 = sum(X401535.ML))
after_80 <- after_80[6:61,]

after_880 <- rbind(as.data.frame(data_880S_4[,c(1,11)]), as.data.frame(data_880S_5[,c(1,11)]))
after_880 <- mutate(after_880, X400659.ML = gsub(",","",X400659.ML))
after_880 <- mutate(after_880, date = substr(as.character(Hour),1,4), hour = substr(as.character(Hour),11,12), X400659.ML = as.numeric(as.character(X400659.ML)))
after_880 <- mutate(after_880, hour = gsub(":","",hour))
after_880 <- after_880 %>% group_by(date) %>% summarise(aft_880 = sum(X400659.ML))
after_880 <- after_880[6:61,]

after_580 <- rbind(as.data.frame(data_580E_4[,c(1,6)]), as.data.frame(data_580E_5[,c(1,6)]))
after_580 <- mutate(after_580, X401899.ML = gsub(",","",X401899.ML))
after_580 <- mutate(after_580, date = substr(as.character(Hour),1,4), hour = substr(as.character(Hour),11,12), X401899.ML = as.numeric(as.character(X401899.ML)))
after_580 <- mutate(after_580, hour = gsub(":","",hour))
after_580 <- after_580 %>% group_by(date) %>% summarise(aft_580 = sum(X401899.ML))
after_580 <- after_580[6:61,]

combine_data <- merge(merge(merge(before_80,after_80),after_880),after_580)
combine_data <- mutate(combine_data, diff = aft_580 - bef_80 + aft_80 + aft_880)
combine_data <- mutate(combine_data, net_580 = pmin(diff,aft_580))
combine_data_des <- combine_data[24:49,]
mean <- colSums(detour[1:23,][,-1])/24
detour_des <- detour[24:49,]
detour_des <- mutate(detour_des, real_80 = round(route_80 - mean[1]), net_880 = round(route_880 - mean[2]))
detour_des <- mutate(detour_des, real_880 = pmax(0, net_880))
net_data <- cbind(combine_data_des[,c(1,7)],detour_des[,c(4,6)])

#plot the comparison between net 580 and 80/880(abandoned)
ggplot(net_data, aes(x=date)) +
  geom_col(aes(y=net_580), fill="light grey") +
  geom_col(aes(y=real_80+real_880), fill="dark grey") +
  geom_col(aes(y=real_80), fill="black") +
  labs(title = "How I-580 Traffic Diverging to I-80 & I-880 After the Accident", y = "flow") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#plot the comparison between net 580 and 80/880
lmao <- net_data %>% gather(key = freeway, value = flow, `net_580`, `real_80`, `real_880`)
ggplot(lmao, aes(x=date, y=flow, group = factor(freeway))) + geom_point(aes(color = factor(freeway))) + geom_line() + labs(title = "How I-580 Traffic Diverging to I-80 & I-880 After the Accident") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#plot correlogram between net 580 and 80/880
lol <- net_data[c("net_580","real_80","real_880")]
M <- cor(lol)
corrplot(M, order = "hclust", 
         tl.col = "black", tl.srt = 45)

#plot the piechart of 580 diverging flow
total <- colSums(net_data[,-1])
slices <- c(total[2], total[3], total[1]-total[2]-total[3]) 
lbls <- c("I-80E (34.0%)", "I-880S (18.5%)", "W Grand Ave, etc (47.5%)")
pie3D(slices,labels=lbls,explode=0.15,theta=1,radius=1.5,col = c("pink","light green","light blue"),border="grey",
      main="Direction of Diverging Flow from I-580")


###################################################


#clean the bridge data
bridge <- read.csv("bridge.csv")
colnames(bridge)[6:7] <- c("Flow","Delay")
bridge <- mutate(bridge, Flow = gsub(",","",Flow))
bridge <- mutate(bridge, date = substr(as.character(Hour),1,5), hour = substr(as.character(Hour),12,13), Flow = as.numeric(as.character(Flow)), Delay = as.numeric(as.character(Delay)))
bridge$freeway = paste(bridge$Freeway,bridge$Direction)
bridge <- bridge[c("date","hour","Station","freeway","Flow","Delay","Direction")]
bridge <- bridge %>% group_by(date,Station,freeway,Direction) %>% summarise(daily_flow = sum(Flow),daily_delay = sum(Delay))
bridge_weekend <- filter(bridge, date %in% c("08/23","08/29","08/30","09/05","09/06","09/12","09/13","09/19","09/20"))
'%ni%' <- Negate('%in%')
bridge_weekday <- filter(bridge, date %ni% c("08/23","08/29","08/30","09/05","09/06","09/12","09/13","09/19","09/20"))
bridge_weekend_WE <- filter(bridge_weekend, Direction %in% c("W","E"))
bridge_weekday_WE <- filter(bridge_weekday, Direction %in% c("W","E"))


#flow & delay on alternate bridges over weekday and weekend
ggplot(bridge_weekday, aes(x = date, y = daily_flow, group = factor(freeway))) + geom_point(aes(color = factor(freeway))) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Traffic Flow on Alternate Bridges, Weekday") 
ggplot(bridge_weekend, aes(x = date, y = daily_flow, group = factor(freeway))) + geom_point(aes(color = factor(freeway))) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Traffic Flow on Alternate Bridges, Weekend") 
ggplot(bridge_weekday, aes(x = date, y = daily_delay, group = factor(freeway))) + geom_point(aes(color = factor(freeway))) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Traffic Delay on Alternate Bridges, Weekday") 
ggplot(bridge_weekend, aes(x = date, y = daily_delay, group = factor(freeway))) + geom_point(aes(color = factor(freeway))) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Traffic Delay on Alternate Bridges, Weekend") 


#flow & delay comparison between E and W bound
ggplot(bridge_weekday_WE, aes(x = date, y = daily_flow, group = factor(freeway))) + geom_point(aes(color = factor(freeway))) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_grid(. ~ Direction) + ggtitle("Comparison of Traffic Flow on East/West Bound, Weekday") 
ggplot(bridge_weekend_WE, aes(x = date, y = daily_flow, group = factor(freeway))) + geom_point(aes(color = factor(freeway))) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_grid(. ~ Direction) + ggtitle("Comparison of Traffic Flow on East/West Bound, Weekend") 
ggplot(bridge_weekday_WE, aes(x = date, y = daily_delay, group = factor(freeway))) + geom_point(aes(color = factor(freeway))) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_grid(. ~ Direction) + ggtitle("Comparison of Traffic Delay on East/West Bound, Weekday") 
ggplot(bridge_weekend_WE, aes(x = date, y = daily_delay, group = factor(freeway))) + geom_point(aes(color = factor(freeway))) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_grid(. ~ Direction) + ggtitle("Comparison of Traffic Delay on East/West Bound, Weekend") 

#clean the bridge_emergency data
bridge_emer <- read.csv("bridge_emergency.csv")
colnames(bridge_emer)[6:7] <- c("Flow","Delay")
bridge_emer <- mutate(bridge_emer, Flow = gsub(",","",Flow))
bridge_emer <- mutate(bridge_emer, date = substr(as.character(Hour),1,5), hour = substr(as.character(Hour),12,13), Flow = as.numeric(as.character(Flow)), Delay = as.numeric(as.character(Delay)))
bridge_emer$freeway = paste(bridge_emer$Freeway,bridge_emer$Direction)
bridge_emer <- bridge_emer[c("date","hour","Station","freeway","Flow","Delay","Direction")]
bridge_emer <- bridge_emer %>% group_by(date,Station,freeway,Direction) %>% summarise(daily_flow = sum(Flow),daily_delay = sum(Delay))
bridge_emer <- bridge_emer[421:637,]
bridge_weekend_emer <- filter(bridge_emer, date %in% c("10/17","10/18","10/24","10/25","10/31","11/01","11/07","11/08"))
'%ni%' <- Negate('%in%')
bridge_weekday_emer <- filter(bridge_emer, date %ni% c("10/17","10/18","10/24","10/25","10/31","11/01","11/07","11/08"))
bridge_weekend_emer_WE <- filter(bridge_weekend_emer, Direction %in% c("W","E"))
bridge_weekday_emer_WE <- filter(bridge_weekday_emer, Direction %in% c("W","E"))


#flow & delay on alternate bridges over weekday and weekend
ggplot(bridge_weekday_emer, aes(x = date, y = daily_flow, group = factor(freeway))) + geom_point(aes(color = factor(freeway))) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Traffic Flow on Alternate Bridges in Oct/Nov, Weekday") 
ggplot(bridge_weekend_emer, aes(x = date, y = daily_flow, group = factor(freeway))) + geom_point(aes(color = factor(freeway))) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Traffic Flow on Alternate Bridges in Oct/Nov, Weekend") 
ggplot(bridge_weekday_emer, aes(x = date, y = daily_delay, group = factor(freeway))) + geom_point(aes(color = factor(freeway))) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Traffic Delay on Alternate Bridges in Oct/Nov, Weekday") 
ggplot(bridge_weekend_emer, aes(x = date, y = daily_delay, group = factor(freeway))) + geom_point(aes(color = factor(freeway))) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Traffic Delay on Alternate Bridges in Oct/Nov, Weekend") 


#flow & delay comparison between E and W bound
ggplot(bridge_weekday_emer_WE, aes(x = date, y = daily_flow, group = factor(freeway))) + geom_point(aes(color = factor(freeway))) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_grid(. ~ Direction) + ggtitle("Comparison of Traffic Flow on East/West Bound in Oct/Nov, Weekday") 
ggplot(bridge_weekend_emer_WE, aes(x = date, y = daily_flow, group = factor(freeway))) + geom_point(aes(color = factor(freeway))) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_grid(. ~ Direction) + ggtitle("Comparison of Traffic Flow on East/West Bound in Oct/Nov, Weekend") 
ggplot(bridge_weekday_emer_WE, aes(x = date, y = daily_delay, group = factor(freeway))) + geom_point(aes(color = factor(freeway))) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_grid(. ~ Direction) + ggtitle("Comparison of Traffic Delay on East/West Bound in Oct/Nov, Weekday") 
ggplot(bridge_weekend_emer_WE, aes(x = date, y = daily_delay, group = factor(freeway))) + geom_point(aes(color = factor(freeway))) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_grid(. ~ Direction) + ggtitle("Comparison of Traffic Delay on East/West Bound in Oct/Nov, Weekend") 


#correlation matrix of each traffic
bridge_trim <- bridge[c("date","freeway","daily_flow")][64:168,]
bridge_wide <- spread(bridge_trim,key=freeway,value=daily_flow)
bridge_wide <- bridge_wide[-1]
A <- cor(bridge_wide)
corrplot(A, type = "upper", order = "hclust", mar=c(0,0,1,0),
         tl.col = "black", tl.srt = 45, title = "Correlogram of Traffic Flow of Each Alternate Bridge Following Planned Bay Bridge Closure")

bridge_trim_emer <- bridge_emer[c("date","freeway","daily_flow")][85:189,]
bridge_wide_emer <- spread(bridge_trim_emer,key=freeway,value=daily_flow)
bridge_wide_emer <- bridge_wide_emer[-1]
B <- cor(bridge_wide_emer)
corrplot(B, type = "upper", order = "hclust", mar=c(0,0,1,0),
         tl.col = "black", tl.srt = 45, title = "Correlogram of Traffic Flow of Each Alternate Bridge Following Unexpected Bay Bridge Closure")

bridge_trim <- bridge[c("date","freeway","daily_delay")][64:168,]
bridge_wide <- spread(bridge_trim,key=freeway,value=daily_delay)
bridge_wide <- bridge_wide[-1]
A <- cor(bridge_wide)
corrplot(A, type = "upper", order = "hclust", mar=c(0,0,1,0),
         tl.col = "black", tl.srt = 45, title = "Correlogram of Traffic Delay of Each Alternate Bridge Following Planned Bay Bridge Closure")

bridge_trim_emer <- bridge_emer[c("date","freeway","daily_delay")][85:189,]
bridge_wide_emer <- spread(bridge_trim_emer,key=freeway,value=daily_delay)
bridge_wide_emer <- bridge_wide_emer[-1]
B <- cor(bridge_wide_emer)
corrplot(B, type = "upper", order = "hclust", mar=c(0,0,1,0),
         tl.col = "black", tl.srt = 45, title = "Correlogram of Traffic Delay of Each Alternate Bridge Following Unexpected Bay Bridge Closure")



