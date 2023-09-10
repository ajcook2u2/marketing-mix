library(ggplot2)
library(readr)
library(lubridate)
library(Robyn)

setwd("D:/downloads/kaggle/marketing mix")
data <- read.csv('marketing_mix.csv')
data$Date <- mdy(data$Date)


print(data$Date)
average_line = mean(data$TikTok)

chart <- ggplot(data) +
  geom_area(data=subset(data), aes(x = Date, y = TikTok), fill = 'blue') +
  geom_hline(yintercept = 1500, col='red') +
  geom_hline(yintercept = 1000, col='blue') +
  geom_hline(yintercept = average_line, col='green')
  
  
print(chart)

chart <- ggplot(data) + 
  geom_area(aes(x=Date, y=TikTok))






average_line = mean(data$Facebook)

chart <- ggplot(data) +
  geom_area(data=subset(data), aes(x = Date, y = Facebook), fill = 'blue') +
  geom_hline(yintercept = 1500, col='red') +
  geom_hline(yintercept = 1000, col='blue') +
  geom_hline(yintercept = average_line, col='green')


print(chart)

chart <- ggplot(data) + 
  geom_area(aes(x=Date, y=Facebook))





average_line = mean(data$Google.Ads)

chart <- ggplot(data) +
  geom_area(data=subset(data), aes(x = Date, y = Google.Ads), fill = 'blue') +
  geom_hline(yintercept = 1500, col='red') +
  geom_hline(yintercept = 1000, col='blue') +
  geom_hline(yintercept = average_line, col='green')


print(chart)

chart <- ggplot(data) + 
  geom_area(aes(x=Date, y=Google.Ads))
