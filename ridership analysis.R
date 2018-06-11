closeAllConnections()
rm(list=ls())
setwd("~/Documents/STA160/160trafficdata/df")
library(ggplot2)
require(grDevices)
library(graphics)

# Loading ridership data
ridership_df <- read.csv("~/Documents/STA160/160trafficdata/df/yearly_ridership.csv", row.names = 1)
colnames(ridership_df) <- gsub("X", x = colnames(ridership_df), replacement = "" ) 
x <- rownames(ridership_df) 



# Comparing 2013 and 2012 Ridership
plot(ridership_df$`2013`, type = 'b', pch = 19, col = 'red', xlab = "Month", ylab = 'Ridership', las = 3, xaxt='n')
lines(ridership_df$`2012`, type = 'b', pch = 19, col = 'blue')
title(main="Comparing 2013 and 2012 Ridership")
axis(1, at = 1:12 ,  x, las = 2)
legend("bottomleft", legend=c("2013", "2012"),
       col=c("red", "blue"), lty=2, cex=0.8,
       text.font=4, bg="transparent")
abline(v = 8 + 28/31)
abline(v = 9 + 3/30)
mtext("Vertical lines indicate start and end of closure", 3, cex = 0.74)

# Comparing 2007 and 2006 Ridership
plot(ridership_df$`2007`, type = 'b', pch = 19, col = 'red', xlab = "Month", ylab = 'Ridership', las = 3, xaxt='n')
lines(ridership_df$`2006`, type = 'b', pch = 19, col = 'blue')
title(main="Comparing 2007 and 2006 Ridership")
axis(1, at = 1:12 ,  x, las = 2)
legend("topleft", legend=c("2007", "2006"),
       col=c("red", "blue"), lty=2, cex=0.8,
       text.font=4, bg='transparent')
abline(v = 4 + 29/30)
abline(v = 5 + 7/31)
abline(v = 5 + 24/31)
mtext("Vert. lines are 580 collapse, 880 reopen, 580 reopen", 3, cex = 0.74)

# Comparing 2008 and 2009 Ridership
plot(ridership_df$`2008`, type = 'b', pch = 19, col = 'red', xlab = "Month", ylab = 'Ridership', las = 3, xaxt='n')
lines(ridership_df$`2009`, type = 'b', pch = 19, col = 'blue')
title(main="Comparing 2008 and 2009 Ridership")
axis(1, at = 1:12 ,  x, las = 2)
legend("topleft", legend=c("2008", "2009"),
       col=c("red", "blue"), lty=2, cex=0.8,
       text.font=4, bg='transparent')
abline(v = 9 + 6/30)
abline(v = 9 + 8/30)
abline(v = 10 + 27/31)
abline(v = 11 + 8/30)
mtext("Vertical lines indicate start and end of closure", 3, cex = 0.74)



