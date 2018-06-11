library(ggplot2)
library(lubridate)
library(scales)
library(plyr)
library(pastecs)

raw13<- read.csv("https://raw.githubusercontent.com/sssantos/160trafficdata/master/df/2013.csv?token=Ak-a7rwDPNE6wCvwx5OTmZ2kQON4bM1gks5bHugowA%3D%3D")
raw12<- read.csv("https://raw.githubusercontent.com/sssantos/160trafficdata/master/df/2012.csv?token=Ak-a7rbYYiMPKJbqdpLcXAOID_QgUNCiks5bHufswA%3D%3D") 

raw12$TimeStamp <- as.POSIXct(raw12$TimeStamp, format='%m/%d/%Y %H:%M')
raw13$TimeStamp <- as.POSIXct(raw13$TimeStamp, format='%m/%d/%Y %H:%M')

raw12 <- raw12[order((df12$TimeStamp)),]
raw13 <- raw13[order((df13$TimeStamp)),]

##subset week before and week after repair in 2013
before13 <- subset(raw13, TimeStamp >= as.POSIXct('2013-08-21 00:00:00') &
                     TimeStamp <= as.POSIXct('2013-08-27 23:00:00'))


after13 <- subset(raw13, TimeStamp >= as.POSIXct('2013-09-04 00:00:00') &
                    TimeStamp <= as.POSIXct('2013-09-10 23:00:00'))


##subset df into labor day weekend only
labor12 <- subset(raw12, TimeStamp >= as.POSIXct('2012-08-29 00:00:00') &
                    TimeStamp <= as.POSIXct('2012-09-04 23:00:00'))
labor12$Year = format(labor12$TimeStamp, "%Y")
labor12$Num = format(labor12$TimeStamp, "%w")
labor12$Day = format(labor12$TimeStamp, "%A")
bad12 <- as.POSIXct('2012-09-01 15:00:00')
bad22 <- as.POSIXct('2012-09-01 17:00:00')
labor12<- subset(labor12, !((TimeStamp >=bad12 & TimeStamp <=bad22)))

labor13 <- subset(raw13, TimeStamp >= as.POSIXct('2013-08-28 00:00:00') &
                    TimeStamp <= as.POSIXct('2013-09-03 23:00:00'))
labor13$Year = format(labor13$TimeStamp, "%Y")
labor13$Num = format(labor13$TimeStamp, "%w")
labor13$Day = format(labor13$TimeStamp, "%A")

bad13 <- as.POSIXct('2013-08-31 15:00:00')
bad23 <- as.POSIXct('2013-08-31 23:00:00')
labor13<- subset(labor13, !((TimeStamp >=bad13 & TimeStamp <=bad23)))

summary(labor12)
##sample subset by freeway
labor12_580E <- NULL
labor13_580E <- NULL
strt12 <- as.POSIXct('2012-08-31 00:00:00')
end12 <- as.POSIXct('2012-08-31 23:00:00')
labor12_580E <- subset(labor12, Freeway == 580 & Direction == 'W')
                         TimeStamp >= strt12 & TimeStamp <= end12)
labor12_580E <- labor12_580E[order(labor12_580E$TimeStamp),]

summary(labor12_580E)
strt13 <- as.POSIXct('2013-08-30 00:00:00')
end13 <- as.POSIXct('2013-08-30 23:00:00')
labor13_580E <- subset(labor13, Freeway == 580 & Direction == 'W')
labor13_580E<- subset(labor13_580E, !((TimeStamp >=bad13 & TimeStamp <=bad23)))

                         TimeStamp >= strt13 & TimeStamp <= end13)
labor13_580E <- labor13_580E[order(labor13_580E$TimeStamp),]

labor_580E <- as.data.frame(rbind(labor12_580E,labor13_580E))
labor_580E$Year = format(labor_580E$TimeStamp, "%Y")
labor_580E$Hour = format(labor_580E$TimeStamp, "%H")
labor_580E$Day = format(labor_580E$TimeStamp, "%A")
labor_580E$Hour <- as.numeric(labor_580E$Hour)
ggplot(labor_580E, aes(Hour, Average.flow, color = Year, shape = Direction, linetype= Direction)) + geom_point() + geom_line() +
  scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M") + ylab("Average Flow") +
  ggtitle('Average Flow on I980-W; 8/30/13 v. 8/31/12') + theme(plot.title = element_text(hjust = 0.5))
 typeof(labor_580E$Hour)

 ##Daily Average Analysis Plots

d12<-aggregate(labor12[, 7], list(labor12$TimeStamp, labor12$Freeway, labor12$Direction, labor12$Year, labor12$Day), mean)
d13<-aggregate(labor13[, 7], list(labor13$Freeway,labor13$Direction, labor13$Year, labor13$Day, labor13$Num), mean)
df_comb_avg <- as.data.frame(rbind(d12,d13))
df_comb_avg$Group.5 <- as.numeric(df_comb_avg$Group.5)
typeof(df_comb_avg$Group.5)

Avg_24E12 <- subset(df_comb_avg, Group.1 == 24 & Group.2 == 'E'
                    & Group.3==2012)
Avg_24E13 <- subset(df_comb_avg, Group.1 == 24 & Group.2 == 'E'
                    & Group.3==2013)

Avg_24W12 <- subset(df_comb_avg, Group.1 == 24 & Group.2 == 'W'
                  & Group.3==2012)
Avg_24W13 <- subset(df_comb_avg, Group.1 == 24 & Group.2 == 'W'
                    & Group.3==2013)

Avg_880N12 <- subset(df_comb_avg, Group.1 == 880 & Group.2 == 'N'
                   & Group.3==2012)
Avg_880N13 <- subset(df_comb_avg, Group.1 == 880 & Group.2 == 'N'
                   & Group.3==2013)
Avg_880S12 <- subset(df_comb_avg, Group.1 == 880 & Group.2 == 'N'
                     & Group.3==2012)

Avg_80W12 <- subset(df_comb_avg, Group.1 == 80 & Group.2 == 'W'
                  & Group.3==2012)
Avg_80E <- subset(df_comb_avg, Group.1 == 80 & Group.2 == 'E')

Avg_980W <- subset(df_comb_avg, Group.1 == 980 & Group.2 == 'W')
Avg_980E <- subset(df_comb_avg, Group.1 == 980 & Group.2 == 'E')

Avg_580W <- subset(df_comb_avg, Group.1 == 580 & Group.2 == 'W')
Avg_580E <- subset(df_comb_avg, Group.1 == 580)


ggplot(Avg_580E, aes(TimeStamp, x, color = Group.3, shape = Group.2, linetype=Group.2)) + geom_point() + geom_line() +
  ylab("Average Flow (veh/h)") + xlab("Day") + scale_x_datetime(date_breaks= "1 day", date_labels = "%A") 
  ggtitle('Average Flow on I580-E') + theme(plot.title = element_text(hjust = 0.5))

##Daily Analysis Hypothesis Testing:
after13_test <- subset(after13, Freeway == 24 & Direction == 'E')
after13_test <- after13_test[order(after13_test$TimeStamp),]

before13_test <- subset(before13, Freeway == 24 & Direction == 'E')
before13_test <- before13_test[order(before13_test$TimeStamp),]
#labor13_580E<- subset(labor13_580E, !((TimeStamp >=bad13 & TimeStamp <=bad23)))

t.test(after13_test$Average.flow, before13_test$Average.flow, alternative = c("two.sided"),
       mu=0, conf.level=.95)

#mean(labor13_580E$Average.speed)

#plot of 2013 before and after
weeks13<-subset(raw13, TimeStamp >= as.POSIXct('2013-08-21 00:00:00') &
         TimeStamp <= as.POSIXct('2013-09-10 23:00:00'))
weeks13$TimeStamp <- format(weeks13$TimeStamp, format = "%Y-%m-%d")
weeks13 <- ddply(weeks13, .(TimeStamp,Freeway, Direction), summarize, daily_flow = mean(Average.del_60))
weeks13$TimeStamp <- as.POSIXct(weeks13$TimeStamp, "%Y-%m-%d")
weeks13 <- subset(weeks13, Freeway == 24) #& Direction == 'E')

ggplot(weeks13, aes(TimeStamp, daily_flow, color=Direction, linetype=Direction)) + geom_line() + geom_point() +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") + ylab("Average Delay (hours)")+ xlab("Day") +
  ggtitle('Average Delay on SR24-E From 8/21/13 to 9/10/13') + theme(plot.title = element_text(hjust = 0.5),
                                                                      axis.text.x = element_text(angle = 90, hjust = 1))
##Descriptive statistics 2013
combine13 <- as.data.frame(rbind(before13, after13))
new13<- ddply(combine13, .(TimeStamp,Freeway, Direction), summarize, daily_flow = mean(Average.flow))
new13<-subset(new13, Freeway== 880 & Direction== 'S')

close13<- ddply(labor13, .(TimeStamp,Freeway, Direction), summarize, daily_flow = mean(Average.flow))
close13<- subset(close13, Freeway== 880 & Direction== 'S')

stat.desc(new13)
stat.desc(close13)

hist(new13$daily_flow)

##Descriptive statistics 2012
before12 <- subset(raw12, TimeStamp >= as.POSIXct('2012-08-22 00:00:00') &
                     TimeStamp <= as.POSIXct('2012-08-28 23:00:00'))

after12 <- subset(raw12, TimeStamp >= as.POSIXct('2012-09-05 00:00:00') &
                    TimeStamp <= as.POSIXct('2012-09-11 23:00:00'))
combine12 <- as.data.frame(rbind(before12, after12))
new12<- ddply(combine12, .(TimeStamp,Freeway, Direction), summarize, daily_flow = mean(Average.flow))
new12<-subset(new12, Freeway== 880 & Direction== 'S')

close12<- ddply(labor12, .(TimeStamp,Freeway, Direction), summarize, daily_flow = mean(Average.flow))
close12<- subset(close12, Freeway== 880 & Direction== 'S')

stat.desc(new12)
stat.desc(close12)

hist(close12$daily_flow)

