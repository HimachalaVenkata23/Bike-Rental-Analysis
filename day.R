rm(list=ls())
setwd("C:/Users/Himachala Venkata/Desktop/edwisor/Projects/Bike rentals")
getwd()
df<-file.choose("day(1)")
day=read.csv(df,header=T)
View(day)
summary(day)
head(day)
nrow(day)
ncol(day)
dim(day)
names(day)
is.na(day)
day<-data.frame(day)
str(day)
library(dplyr)
library(corrgram)
library(corrplot)
library(stats)
library(ggplot2)
day$season <- factor(format(day$season, format="%A"),
                          levels = c("1", "2","3","4") , labels = c("Spring","Summer","Fall","Winter"))
table(day$season)
day$holiday <- factor(format(day$holiday, format="%A"),
                           levels = c("0", "1") , labels = c("Working Day","Holiday"))
table(day$holiday)
day$weathersit <- factor(format(day$weathersit, format="%A"),
                              levels = c("1", "2","3","4") , 
                              labels = c("Good:Clear/Sunny","Moderate:Cloudy/Mist","Bad: Rain/Snow/Fog","Worse: Heavy Rain/Snow/Fog"))
table(day$weathersit)
day$yr <- factor(format(day$yr, format="%A"),
                      levels = c("0", "1") , labels = c("2011","2012"))
table(day$yr)
day$actual_temp <- day$temp*41
day$actual_feel_temp <- day$atemp*50
day$actual_windspeed <- day$windspeed*67
day$actual_humidity <- day$hum*100
day$mean_acttemp_feeltemp <- (day$actual_temp+day$actual_feel_temp)/2
str(day)
summary(day)
################################Exploratary Analysis#############################
h <- hist(day$cnt, breaks = 25, ylab = 'Frequency of Rental', xlab = 'Total Bike Rental Count', main = 'Distribution of Total Bike Rental Count', col = 'white' )

xfit <- seq(min(day$cnt),max(day$cnt), length = 50)
yfit <- dnorm(xfit, mean =mean(day$cnt),sd=sd(day$cnt))
yfit <- yfit*diff(h$mids[1:2])*length(day$cnt)
lines(xfit,yfit, col='red', lwd= 3)
######################Distribution of categorical variables#####################
par(mfcol=c(2,2))
dev.off()

boxplot(day$cnt ~ day$season,
        data = day,
        main = "Total Bike Rentals Vs Season",
        xlab = "Season",
        ylab = "Total Bike Rentals",
        col = c("yellow", "yellow1", "yellow2", "yellow3")) 


boxplot(day$cnt ~ day$holiday,
        data = day,
        main = "Total Bike Rentals Vs Holiday/Working Day",
        xlab = "Holiday/Working Day",
        ylab = "Total Bike Rentals",
        col = c("coral", "coral1", "coral2", "coral3")) 

boxplot(day$cnt ~ day$weathersit,
        data = day,
        main = "Total Bike Rentals Vs Weather Situation",
        xlab = "Weather Situation",
        ylab = "Total Bike Rentals",
        col = c("pink", "pink1", "pink2", "pink3")) 


plot(day$dteday, day$cnt,type = "p",
     main = "Total Bike Rentals Vs DateDay",
     xlab = "Year",
     ylab = "Total Bike Rentals",
     col  = "blue",
     pch  = 19)
#######################Distribution of Numeric variables#########################
par(mfrow=c(2,2))
dev.off()
plot(day$actual_temp, day$cnt ,type = 'h', col= 'purple', xlab = 'Actual Temperature', ylab = 'Total Bike Rentals')

plot(day$actual_feel_temp, day$cnt ,type = 'h', col= 'purple', xlab = 'Actual Feel Temperature', ylab = 'Total Bike Rentals')

plot(day$actual_windspeed, day$cnt ,type = 'h', col= 'purple', xlab = 'Actual Windspeed', ylab = 'Total Bike Rentals')

plot(day$actual_humidity, day$cnt ,type = 'h', col= 'purple', xlab = 'Actual Humidity', ylab = 'Total Bike Rentals')

######################Correlation Plots##########################################
Cor_actual_temp<-cor(x = day$actual_temp, y = day$cnt)
Cor_actual_feel_temp <- cor(x = day$actual_feel_temp, y =day$cnt)
day_cor<- day %>% select (cnt,actual_temp,actual_feel_temp,mean_acttemp_feeltemp,actual_humidity,actual_windspeed)
day_cor<- data.frame(day_cor)

day_cor

colnames(day)[1] <- "Total Number of Bike Rentals"
colnames(day)[2] <- "Temperature"
colnames(day)[3] <- "Feel Temperature"
colnames(day)[4] <- "Mean Actual Temp Feel Temp"
colnames(day)[5] <- "Humidity"
colnames(day)[6] <- "Windspeed"

cor(day_cor)

corplot_day<- cor(day_cor)
corrplot(corplot_day, method="number")

######################Scatter Plot###################################

ggplot_Temp_Rent<- ggplot(day, aes(x=day$actual_temp,y=day$cnt))+geom_point(shape=1)+geom_smooth(method=lm)+ xlab("Actual Temp. in Celcius")+ylab("Bike Rentals")
ggplot_Temp_Rent+scale_y_continuous(breaks=c(0,1100,2345,3500,5000,6000,7000,8000))+labs(title="Total Bike Rentals Vs Actual Temperature | Intercept = 2345")

#####################lm test####################################
dev.off()
lm_test<- lm(day$cnt~day$actual_temp)
summary(lm_test)

plot(lm_test, col = "green")

################################Linear regression model##############################

lm_test1<- lm(sqrt(day$cnt)~day$actual_temp+day$actual_humidity+day$actual_windspeed)

lm_test1
summary(lm_test1)

lm_test2<- lm(((day$cnt)^2)~day$actual_temp+day$actual_humidity+day$actual_windspeed)

lm_test2
summary(lm_test2)

lm_test3<- lm((log(day$cnt))~day$actual_temp+day$actual_humidity+day$actual_windspeed)

lm_test3
summary(lm_test3)

lm_test4<-lm(day$cnt~day$actual_temp+day$actual_humidity+day$actual_windspeed)
lm_test4
summary(lm_test4)

plot(lm_test4,col = "violet", main = "Linear Regression: Bike Rentals, Temp, Windspeed and Humidity")

##################################Time series analysis#############################
library(tidyverse)
library(lubridate)
library(tidyr)
library(reshape2)
library(scales)
library(forecast)
library(zoo)
library(tseries)
library(e1071)
library(pander)
data_ts <-msts(day[,'cnt'], seasonal.periods=c(7))
train_ts <- head(data_ts, round(length(data_ts) * 0.9))
test_ts <- tail(data_ts, round(length(data_ts) * 0.1))
plot(train_ts, xlab="Weeks", ylab="Bike riders")

###############################outliers##################################
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(day$temp, main="temperature", sub=paste("Outlier rows: ", boxplot.stats(day$temp)$out))  # box plot for 'speed'
boxplot(day$cnt, main="ridership", sub=paste("Outlier rows: ", boxplot.stats(day$cnt)$out))

# Correlation
cor(day$cnt, day$temp)

# Build Linear Model only based on temperature
lmfit1<-lm(day$cnt ~day$temp)
panderOptions("digits")
pander(lmfit1, caption = "Linear Model: bike riders ~ temp")


R1=summary(lmfit1)$r.squared
cat("R-Squared = ", R1)

ggplot(day, aes(temp, cnt)) + 
  geom_point(color="firebrick") +
  ggtitle('Ridership vs. temperature') +
  theme(plot.title = element_text(size=19.5, face="bold"))+
  labs(x="temperature", y="ridership")+
  theme(axis.text.x=element_text(angle=90, vjust=.5)) +
  theme(panel.background = element_rect(fill = 'grey75'))+
  stat_smooth(method = "lm",  formula = y ~ x, col = "yellow")

train_df <- day[1:547, ]
test_df <- day[547:nrow(day), ]

# Create a Random Forest model
lm_model <- lm(cnt ~ temp+ workingday + weathersit + atemp + hum + windspeed, data = train_df)
print(lm_model)

# Predicting on test set
predTrain <- predict(lm_model, test_df)

library(randomForest)
train_df <- day[1:547, ]
test_df <- day[547:nrow(day), ]

# Create a Random Forest model
library(randomForest)
rf_model <- randomForest(cnt ~ temp+ workingday + weathersit + atemp + hum + windspeed, data = train_df, ntree = 10)
print(rf_model)

# Predicting on test set
predTrain <- predict(rf_model, test_df, type = "class")

#load libraries
library("ggplot2")
library("scales")
library("psych")
library("gplots")

##################################Visualizations###################################
#loaddata

setwd("C:/Users/Himachala Venkata/Desktop/edwisor/Projects/Bike rentals")
getwd()
df<-file.choose("day(1)")
day=read.csv(df,header=T)

#Univariate 
#Bar plot(categorical data)
#If you want count then stat="bin"
ggplot(day, aes_string(x = day$cnt)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Temp") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Bike Rental Analysis") +  theme(text=element_text(size=15))

#Histogram 
ggplot(day, aes_string(x = day$temp)) + 
  geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("Temp") + ylab("Frequency") + ggtitle("Day: Temp") +
  theme(text=element_text(size=20))

#Box plot
ggplot(day, aes_string(x = day$cnt, y = day$temp, 
                                  fill = day$cnt)) + 
  geom_boxplot(outlier.colour = "red", outlier.size = 3) + 
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  guides(fill=FALSE) + theme_bw() + xlab("Count") + ylab("Temp") +
  ggtitle("Outlier Analysis") +  
  theme(text=element_text(size=20))


