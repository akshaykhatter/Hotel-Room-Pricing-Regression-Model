# Project Title: Hotel Room Pricing
# NAME: AKSHAY KHATTER
# EMAIL: akshaykhatter97@gmail.com
# COLLEGE / COMPANY: MAIT Delhi

setwd("~/Desktop/internship/data")

library("rmarkdown", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
#merging 42 .csv files

library(data.table)
library(plyr)
files <- list.files(path = "/Users/akshaykhatter/Desktop/internship/data",pattern = ".csv")
temp <- lapply(files, fread, sep=",")
data <- rbindlist( temp )
View(data)

#writing the merged data in HotelPricingData.csv file
write.csv(data, file="/Users/akshaykhatter/Desktop/internship/HotelPricingData.csv")

#creating usable data frame
hotmerged.df <- read.csv(paste("/Users/akshaykhatter/Desktop/internship/HotelPricingData.csv", sep=""))
View(hotmerged.df)

#summary of all data
library(psych)
describe(hotmerged.df)

library(car)
library(corrgram)

summary(hotmerged.df)

#corrogram for correlations
corrgram(hotmerged.df, order=TRUE,
main="Hotel Rent - Dependencies",
lower.panel=panel.shade, upper.panel=panel.pie,
diag.panel=panel.minmax, text.panel=panel.txt)
attach(hotmerged.df)

#differences mean Rent in the basis of various parameters
aggregate(RoomRent~ IsMetroCity  ,data= hotmerged.df,mean)
aggregate(RoomRent~ IsTouristDestination  ,data= hotmerged.df,mean)
aggregate(RoomRent~ IsNewYearEve  ,data= hotmerged.df,mean)

#to findout variable data types
sapply(hotmerged.df,class)


#visualising
boxplot(hotmerged.df$StarRating)
boxplot(hotmerged.df$HotelCapacity)
scatterplot(RoomRent ~ StarRating)
scatterplot(RoomRent ~ HotelCapacity)
boxplot(hotmerged.df$RoomRent)
boxplot(hotmerged.df$HasSwimmingPool)

plot(RoomRent, HasSwimmingPool, main="Scatterplot ",
xlab="Rs ", ylab="Yes/No ", pch=19)
abline(lm(HasSwimmingPool~RoomRent), col="red") # regression line (y~x)
lines(lowess(RoomRent,HasSwimmingPool), col="blue") # lowess line (x,y)

#selection to find most significant factors
regmodel1 <-step(lm(RoomRent~1), RoomRent ~  IsNewYearEve+Airport+ FreeWifi+FreeBreakfast+HasSwimmingPool+ IsTouristDestination+ StarRating + HotelCapacity + IsWeekend +IsMetroCity ,direction = "forward")
summary(regmodel1)

#regression model for 3 most significant factors
regmodel2 <-lm(formula = RoomRent ~ StarRating + IsTouristDestination + HasSwimmingPool)
summary(regmodel2)



library(Hmisc)
library(car)
library(corrgram)


#creating a subset of required columns
colhotel <- c("RoomRent","StarRating","HasSwimmingPool","IsTouristDestination")

#correlation matrix
corMatrix <- rcorr(as.matrix(hotmerged.df[,colhotel]))
corMatrix

#corrgram of 3 most significant fcators with RoomRent
corrgram(hotmerged.df[,colhotel], order=TRUE,
main="Hotel Rent dependencies",
lower.panel=panel.shade, upper.panel=panel.pie,
diag.panel=panel.minmax, text.panel=panel.txt)


corrgram(hotmerged.df[,colhotel], order=TRUE,
lower.panel=panel.shade, upper.panel=panel.pie,
diag.panel=panel.minmax, text.panel=panel.txt,
main="Hotel Rent dependencies on other factors")


#Comparing other factors and their pattern using other trends with roomrent

#Analyzing effect of MetroCity on RoomRent
table(hotmerged.df$IsMetroCity)

table1<-table(hotmerged.df$IsMetroCity)
barplot(table1, main="IsMetroCity", xlab="Not Metro city OR 0      Is Metro City OR 1", col="red")



##HYPOTHESIS : Articulating hypothesis and conducting t-test to determine their p value


#Average RoomRent in hotels having swimming pool is more than that which don't have.
t.test(RoomRent~HasSwimmingPool,data = hotmerged.df, alternative="less")


#Average RoomRent in hotels in IsTouristDestination is more than that which don't provide.
t.test(RoomRent~IsTouristDestination, data = hotmerged.df, alternative="less")


#Average RoomRent in hotels with high star rating is high as compared to one which has less star rating.
t.test(hotmerged.df$RoomRent,hotmerged.df$StarRating)


#Regression : Generating Regression models using lm() model and testing hypothesis
#Generating a multiple linear regression model for RoomRent
#1.
fit001<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity+Airport-1, data = hotmerged.df)
summary(fit001)

#Coefficents of the model
fit001$coefficients

#Deviation > 1000, not possible to show in output firl

### salary = b0 + b1*StarRating + b2*HasSwimmingPool+ b3*HotelCapacity +b4*Airport + b5*Date
#   b0 = -1(assumption),  b1 =  1248.426988 , b2=3903.736921, b3= -6.743354, b4= 18.869726
#  Model:    salary = -1 + 1248.426988*StarRating + 3903.736921*HasSwimmingPool -6.743354*HotelCapacity  + 18.869726*Aiport


#2.
fit002<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity-1, data = hotmerged.df)
summary(fit002)

#Coefficents of the model
fit002$coefficients

#Deviation > 1000, not possible to show in output firl

#  salary = b0 + b1*StarRating + b2*HasSwimmingPool+ b3*HotelCapacity
#   b0 = -1(assumption),  b1 =  1396.874562, b2=3719.6943, b3= -7.659814
# Model:    salary = -1 + 1396.874562*StarRating + 3719.6943*HasSwimmingPool -7.659814*HotelCapacity




#3.
fit003<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity+IsWeekend+IsTouristDestination-1, data = hotmerged.df)
summary(fit003)

#Coefficents of the model
fit003$coefficients

#Deviation > 1000, not possible to show in output firl


###   salary = b0 + b1*StarRating + b2*HasSwimmingPool+ b3*HotelCapacity +b4*IsWeekend(0) + b5*IsWeekend(1) + b6*IsTouristDestination
#   b0 = -1(assumption),  b1 =  3635.819, b2=2285.132, b3= -13.965, b4=-8396.67457, b5=-8325.09152,b6=1878.94395
#  Model:    salary = -1 + 3635.819*StarRating + 2285.132*HasSwimmingPool -13.965*HotelCapacity
# -8396.67457*IsWeekend(0) - 8325.09152*IsWeekend(1) + 1878.94395*IsTouristDestination












