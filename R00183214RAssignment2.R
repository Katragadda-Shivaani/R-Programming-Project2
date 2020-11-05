#INTRO TO R FOR DATA SCIENCE ASSIGNMENT2

#SHIVAANI KATRAGADDA
#R00183214

#installing readr package useful to get the data from excel to R easily.

install.packages("readr")

library(readr)#loads and attaches the add-on packages.

#installing DataExplorer package which useful for visualizing and analysing the data

install.packages("DataExplorer")

library(DataExplorer)#loads and attaches the add-on packages.

#installing MICE (Multivariate Imputation via Chained Equations) package which is commonly used for multiple imputations 
#as compared to a single imputation (such as mean),it takes care of  missing values and replaced by predictive values obtained

install.packages("mice")

library(mice)#loads and attaches the add-on packages.


#installing shiny packages used to make  easy to build interactive web apps straight from R

install.packages("shiny")

library(shiny)#loads and attaches the add-on packages.

#installing car(Companion to Applied Regression) package provides many functions
#that are applied to a fitted regression model, perform additional calculations on the model or possibly compute a different model, and then return
#values and graphs.

install.packages("car")

library(car)#loads and attaches the add-on packages.

#installing psych(Procedures for Psychological, Psychometric, and Personality Research) package used for visualizing

install.packages("psych")

library(psych)#loads and attaches the add-on packages.

#installing e1071 package used  for skewness function

install.packages("e1071")

library(e1071)#loads and attaches the add-on packages.

#installing corrplot package used for the graphical display of a correlation matrix, confidence interval or general matrix

install.packages("corrplot")

library(corrplot)#loads and attaches the add-on packages.

#==============================================================================================


#Reading the  Assignmenta.csv file by using read.csv() function and it is named as Assignment_2

Assignment_2 <- read.csv("F:/R2 Assignment/Assignment 2.csv")

View(Assignment_2)#viewing the Asignment_2


#Generating the dataframe for Assignment_2 using as.data.frame() function and that dataframe is named as Assignment_2 

Assignment_2<- as.data.frame(Assignment_2)

View(Assignment_2)#viewing the Assignment_2 dataframe

class(Assignment_2)#checking the class of Assignment_2

str(Assignment_2)#structure of Assignment_2

summary(Assignment_2)#Summary of Assignment_2

head(Assignment_2)#head(displays the first few lines of data) of Assignment_2

#DATA EXPLORER Package provides some good functions to know about the data
#Introduce() function will give the outline of the data 
#it tells about the number of rows,columns,missing values,discrete columns,continous columns,all missing columns,complete rows,total observations and memory usage 

introduce(Assignment_2)

#plot_str() will plot a graph with all the columns present in the data set and also it will tell how many observations and columns present in the dataset

plot_str(Assignment_2)

#plot_intro() will plot a graph which will give the percentage of  discrete columns,continous columns,All missing columns,complete rows and missing observations

plot_intro(Assignment_2)

#plot_missing() function shows the percentage of missing values of each column present in the dataset

plot_missing(Assignment_2)

#The missing plot shows the pm2.5 column have missing values so First fill the missing values using imputation technique

plot(Assignment_2$pm2.5)#plot a graph for pm2.5 column of Assignment_2 dataframe

boxplot(Assignment_2$pm2.5)#boxplot for pm2.5 column of Assignment_2 

#dim() will give the number of rows and columns in the dataset 

dim(Assignment_2) 

#FILLING NA VALUES

#calculating what percentage of data is missing for each column. 

#creating a function and storing that function in variable p

#the function contains the sum of missing values divided by the length of missing value and multiplied by 100 to get the percentage

p<-function(x){sum(is.na(x))/length(x)*100}

#using apply functionwe are calling p,i.e function which will give the percentage of missing value

apply(Assignment_2,2,p)

#md.pattern(md stands for missing data) gives the table as well as a plot for the missing data

md.pattern(Assignment_2)

#Another missing data pattern for finding missing data is is md.pairs
md.pairs(Assignment_2)

#Impute

#storing imputed data in the variable impute and the function used is mice,
#with in the data i am taking from 2 to 13 because the first column is just a row number and it does not have any significance or predictive power so
#i am going to ignore the first column for that i am using square braces and comma which means all the rows but only 2 to 13 columns and specifying how many imputations i want
#so m=3 and used random seed 123(seed is used here when ever to re-run program the impued data will not change)

impute<-mice(Assignment_2[,2:13],m=3,seed=214)

print(impute)#here it specifies number of imputations and imputation methods

#it list all the rows where the pm2.5 values are missing and gives estimates for
#first,second and third imputations
impute$imp$pm2.5

#complete data
#we can get complete dataset by using complete() function in that we can specify which imputation we want 

Assignment_2<-complete(impute,1)

#All the missing data is imputated or not  check again whether any missimg data is present in the datase by using plot_missing()

plot_missing(Assignment_2)


#plot_histogram() is provided by DataExplorer package which will provide histograms of all columns which are containing the  continuous  data

plot_histogram(Assignment_2)

#plot_bar() is provided by DataExplorer package which will provide barchart for discrete data

plot_bar(Assignment_2$cbwd)#plot_bar() for cbwd columnn of Assignment_2 dataframe

sort(table(Assignment_2$cbwd))#sorting the column in order to get the total number of observation for each domain in the column in ascending order

class(Assignment_2$cbwd)#checking the class of Assignmet_2$cbwd

#it is a factor

#NOw i am converting the cbwd to numeric for convience as all the other columns data is numeric  

Assignment_2$cbwd<-as.numeric(Assignment_2$cbwd)

#now if I plot histogram all the columns including cbwd column histograms appears

plot_histogram(Assignment_2)


#===============================================1q=============================================
#QUESTION1
#1) You must apply a linear regression model with TEMP as your y variable and an appropriate combination of other x variables from the dataset.
#You should justify your choice in terms of the number of variables. 2


#LINER REGRESSION

#Model1
par(mfrow=c(1, 2))  # divide graph area in 2 columns

#Using Scatter Plot To Visualise The Relationship

scatter.smooth(x=Assignment_2$year, y=Assignment_2$TEMP, main="Temp~year",col="blue") 

#scatter and histogram plot by using psych  package
scatter.hist(Assignment_2$year,Assignment_2$TEMP)

#Using BoxPlot To Check For Outliers
# box plot for 'year'
boxplot(Assignment_2$year, main="year", sub=paste("Outlier rows: ", boxplot.stats(Assignment_2$year)$out)) 

# box plot for 'temperature'
boxplot(Assignment_2$TEMP, main="Temperature", sub=paste("Outlier rows: ", boxplot.stats(Assignment_2$TEMP)$out))  

#Using Density Plot To Check If Response Variable Is Close To Normal

# density plot for 'year'
plot(density(Assignment_2$year), main="Density Plot: Year", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Assignment_2$year), 2)))  

polygon(density(Assignment_2$year), col="red")

# density plot for 'Temperature'

plot(density(Assignment_2$TEMP), main="Density Plot: TEMPERATURE", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Assignment_2$TEMP), 2)))  

polygon(density(Assignment_2$TEMP), col="red")

#Correlation Analysis

cor(Assignment_2$year, Assignment_2$TEMP)#corelation for year and TEMP columns of Assignment_2 dataframe

#Some  basic tests
t.test(Assignment_2$year,Assignment_2$TEMP)#t_test for year and TEMP columns of Assignment_2 dataframe
wilcox.test(Assignment_2$year,Assignment_2$TEMP)#wilcox test for year and TEMP columns of Assignment_2 dataframe

#creating model for TEMP and year
model1 <- lm(TEMP~year,data = Assignment_2)

model1#printing the intercept and slope of model1

confint(model1)#confint of model1

summary(model1)#summary of model1

#plotting the graph between year and Temperature 
plot(Assignment_2$year,Assignment_2$TEMP,pch = 16, cex = 1.2, col = "blue", main = "Graph of Year and Temperature", xlab = "Year", ylab = "Temperature")

abline(model1,col='red')#drawing the abline for plot using model1

summary(model1$residuals)#summary of model1 residuals

str(model1$residuals)#structure of model1 residuals

plot(model1$residuals)#plot of model1 residuals

abline(h = 0, col = ("blue"))#drawing the abline for plot 

hist(model1$residuals)#histogram of model1 residuals

#qq plot i.e Quantile-Quantile plot is used to visualize the deviation from a specific probability distribution
qqnorm(model1$residuals)#plotting qq plot for residuals of model1

qqline(model1$residuals, col = "purple")#drawing the line for the qqplot

#-------------------------
#model2

#Using Scatter Plot To Visualise The Relationship
scatter.smooth(x=Assignment_2$month, y=Assignment_2$TEMP, main="Temp~month",col="blue")  # scatterplot
scatter.hist(Assignment_2$month,Assignment_2$TEMP)##scatter and histogram plot by using psych  package

#Using BoxPlot To Check For Outliers
boxplot(Assignment_2$month, main="Month", sub=paste("Outlier rows: ", boxplot.stats(Assignment_2$month)$out))  

#Using Density Plot To Check If Response Variable Is Close To Normal

plot(density(Assignment_2$month), main="Density Plot: month", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Assignment_2$month), 2))) 

polygon(density(Assignment_2$month), col="red")

plot(density(Assignment_2$TEMP), main="Density Plot: TEMP", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Assignment_2$TEMP), 2)))  

polygon(density(Assignment_2$TEMP), col="red")

#Correlation Analysis
#corelation for month and TEMP columns of Assignment_2 dataframe
cor(Assignment_2$month, Assignment_2$TEMP)

#some basic tests
t.test(Assignment_2$month,Assignment_2$TEMP)#t_test for month and TEMP columns of Assignment_2 dataframe
#wilcox test for month and TEMP columns of Assignment_2 dataframe
wilcox.test(Assignment_2$month,Assignment_2$TEMP)

#ceating linear regression model for TEMP and month
model2 <- lm(TEMP~month,data = Assignment_2)

model2#printing model2

confint(model2)#confint for model2

summary(model2)#summary of model2

#plotting graph for month and temperaure
plot(Assignment_2$month,Assignment_2$TEMP,pch = 16, cex = 1.2, col = "blue", main = "Graph of month and Temperature", xlab = "month", ylab = "Temperature")

abline(model2)#drawing the abline for plot using model1

summary(model2$residuals)#summary of model2 residuals

str(model2$residuals)#structure of model2 residuals

plot(model2$residuals)#plot of model2 residuals

abline(h = 0, col = ("blue"))#drawing the abline for plot

hist(model2$residuals)#histogram of model2 residuals

qqnorm(model2$residuals)#plotting qq plot for residuals of model2

qqline(model2$residuals, col = "purple")#drawing the line for the qqplot

#------------------
#model3

#Using Scatter Plot To Visualise The Relationship
# scatterplot for day and temperature

scatter.smooth(x=Assignment_2$day, y=Assignment_2$TEMP, main="Temp~day",col="blue")  # scatterplot
scatter.hist(Assignment_2$day,Assignment_2$TEMP)#psych  package

#Using BoxPlot To Check For Outliers
#boxplot of day
boxplot(Assignment_2$day, main="day", sub=paste("Outlier rows: ", boxplot.stats(Assignment_2$day)$out))  # box plot for 'day'

#Using Density Plot To Check If Response Variable Is Close To Normal

#density plot for day
plot(density(Assignment_2$day), main="Density Plot: day", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Assignment_2$day), 2)))  

polygon(density(Assignment_2$day), col="red")
#density plot for temperature
plot(density(Assignment_2$TEMP), main="Density Plot: TEMP", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Assignment_2$TEMP), 2)))

polygon(density(Assignment_2$TEMP), col="red")

#Correlation Analysis

cor(Assignment_2$day, Assignment_2$TEMP)#correlation between day and temp

#some basic tests
t.test(Assignment_2$day,Assignment_2$TEMP)#t_test for day and TEMP columns of Assignment_2
wilcox.test(Assignment_2$day,Assignment_2$TEMP)#twilcox test for day and TEMP columns of Assignment_2

#Creating the model for TEMP~day
model3 <- lm(TEMP~day,data = Assignment_2)
model3#printing model3
confint(model3)#confict for model3
summary(model3)#summary of model3
#plot for day and temp
plot(Assignment_2$day,Assignment_2$TEMP,pch = 16, cex = 1.2, col = "blue", main = "Graph of day and Temperature", xlab = "day", ylab = "Temperature")
abline(model3)#drawing abline 
summary(model3$residuals)#summary of residuals
str(model3$residuals)#structure of residuals
plot(model3$residuals)#plot for residuals
abline(h = 0, col = ("blue"))#drawing the abline
hist(model3$residuals)#histogram for residuals
qqnorm(model3$residuals)#qqplot for residuals
qqline(model3$residuals, col = "purple")#line for the qqplot
#-------------------------------

#model4

#Using Scatter Plot To Visualise The Relationship

scatter.smooth(x=Assignment_2$hour, y=Assignment_2$TEMP, main="Temp~hour",col="blue")  # scatterplot
scatter.hist(Assignment_2$hour,Assignment_2$TEMP)#psych  package

#Using BoxPlot To Check For Outliers

boxplot(Assignment_2$hour, main="year", sub=paste("Outlier rows: ", boxplot.stats(Assignment_2$hour)$out))  # box plot for 'hour'

boxplot(Assignment_2$TEMP, main="Temperature", sub=paste("Outlier rows: ", boxplot.stats(Assignment_2$TEMP)$out))  # box plot for 'temperature'

#Using Density Plot To Check If Response Variable Is Close To Normal


plot(density(Assignment_2$hour), main="Density Plot: hour", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Assignment_2$hour), 2)))  

polygon(density(Assignment_2$hour), col="red")

plot(density(Assignment_2$TEMP), main="Density Plot: TEMP", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Assignment_2$TEMP), 2)))  

polygon(density(Assignment_2$TEMP), col="red")

#Correlation Analysis

cor(Assignment_2$hour, Assignment_2$TEMP)
#some basic tests
t.test(Assignment_2$hour,Assignment_2$TEMP)
wilcox.test(Assignment_2$hour,Assignment_2$TEMP)

#creating model TEMP~hour
model4<- lm(TEMP~hour,data = Assignment_2)
model4#printing the model
confint(model4)#confint of model
summary(model4)#summary of model
#plot for hour and TEMP
plot(Assignment_2$hour,Assignment_2$TEMP,pch = 16, cex = 1.2, col = "blue", main = "Graph of hour and Temperature", xlab = "hour", ylab = "Temperature")
abline(model4)#abline of model
summary(model4$residuals)#summary of model residuas
str(model4$residuals)#structure of model residuals
plot(model4$residuals)#plotting the model residuals
abline(h = 0, col = ("blue"))#drawing the abline
hist(model4$residuals)#histogram for residuals
qqnorm(model4$residuals)#qqplot for residuals
qqline(model4$residuals, col = "purple")#line for the qqplot

#------------------------
#MOdel5

#Using Scatter Plot To Visualise The Relationship

scatter.smooth(x=Assignment_2$pm2.5, y=Assignment_2$TEMP, main="Temp~pm2.5",col="blue")  # scatterplot
scatter.hist(Assignment_2$pm2.5,Assignment_2$TEMP)#psych  package

#Using BoxPlot To Check For Outliers

boxplot(Assignment_2$pm2.5, main="pm2.5", sub=paste("Outlier rows: ", boxplot.stats(Assignment_2$pm2.5)$out))  

boxplot(Assignment_2$TEMP, main="Temperature", sub=paste("Outlier rows: ", boxplot.stats(Assignment_2$TEMP)$out))

#Using Density Plot To Check If Response Variable Is Close To Normal



plot(density(Assignment_2$pm2.5), main="Density Plot: pm2.5", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Assignment_2$pm2.5), 2)))  
polygon(density(Assignment_2$pm2.5), col="red")

plot(density(Assignment_2$TEMP), main="Density Plot: Temperature", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Assignment_2$TEMP), 2))) 

polygon(density(Assignment_2$TEMP), col="red")

#Correlation Analysis

cor(Assignment_2$pm2.5, Assignment_2$TEMP)

#some basic test
t.test(Assignment_2$pm2.5,Assignment_2$TEMP)
wilcox.test(Assignment_2$pm2.5,Assignment_2$TEMP)

model5 <- lm(TEMP~pm2.5,data = Assignment_2)
model5#printing the  model 
confint(model5)#confint of model 
summary(model5)#summary of model 
#plot of pm2.5 and TEMP
plot(Assignment_2$pm2.5,Assignment_2$TEMP,pch = 16, cex = 1.2, col = "blue", main = "Graph of pm2.5 and Temperature", xlab = "pm2.5", ylab = "Temperature")
abline(model5)#abline of model 
summary(model5$residuals)#summary of model residuals
str(model5$residuals)#structure of model residuals
plot(model5$residuals)#plot of model residuals
abline(h = 0, col = ("blue"))#drawing the abline
hist(model5$residuals)#histogram for residuals
qqnorm(model5$residuals)#qqplot for residuals
qqline(model5$residuals, col = "purple")#line for the qqplot

#------------------------------
#Model6
#Using Scatter Plot To Visualise The Relationship

scatter.smooth(x=Assignment_2$DEWP, y=Assignment_2$TEMP, main="Temp~DEWP",col="blue")  # scatterplot
scatter.hist(Assignment_2$DEWP,Assignment_2$TEMP)#psych  package

#Using BoxPlot To Check For Outliers

#boxplot for DEWP
boxplot(Assignment_2$DEWP, main="year", sub=paste("Outlier rows: ", boxplot.stats(Assignment_2$DEWP)$out))  


#Using Density Plot To Check If Response Variable Is Close To Normal

plot(density(Assignment_2$DEWP), main="Density Plot: DEWP", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Assignment_2$DEWP), 2)))  

polygon(density(Assignment_2$DEWP), col="red")

plot(density(Assignment_2$TEMP), main="Density Plot: TEMP", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Assignment_2$TEMP), 2)))  

polygon(density(Assignment_2$TEMP), col="red")

#Correlation Analysis

cor(Assignment_2$DEWP, Assignment_2$TEMP)#correlation of dewp and temp

#some basic tests
t.test(Assignment_2$DEWP,Assignment_2$TEMP)#t-test for dewp and temp
wilcox.test(Assignment_2$DEWP,Assignment_2$TEMP)#wilcox test for dewp and temp

#creating the model for TEMP~DEWP
model6 <- lm(TEMP~DEWP,data = Assignment_2)
model6#printing the model
confint(model6)#confint of model
summary(model6)#summary of model
#plot of DEWP and temp
plot(Assignment_2$DEWP,Assignment_2$TEMP,pch = 16, cex = 1.2, col = "blue", main = "Graph of DEWP and Temperature", xlab = "DEWP", ylab = "Temperature")
abline(model6)#abline of model
summary(model6$residuals)#summary of model residuals
str(model6$residuals)#structure of model residuals
plot(model6$residuals)#plot of model residuals
abline(h = 0, col = ("blue"))#drawing the abline
hist(model6$residuals)#histogram for residuals
qqnorm(model6$residuals)#qqplot for residuals
qqline(model6$residuals, col = "purple")#line for the qqplot
#------------------------------------
#Model7

#Using Scatter Plot To Visualise The Relationship

scatter.smooth(x=Assignment_2$PRES, y=Assignment_2$TEMP, main="Temp~PRES",col="blue")  # scatterplot
scatter.hist(Assignment_2$PRES,Assignment_2$TEMP)#psych  package

#Using BoxPlot To Check For Outliers

boxplot(Assignment_2$PRES, main="year", sub=paste("Outlier rows: ", boxplot.stats(Assignment_2$PRES)$out))  

boxplot(Assignment_2$TEMP, main="Temperature", sub=paste("Outlier rows: ", boxplot.stats(Assignment_2$TEMP)$out))  # box plot for 'temperature'

#Using Density Plot To Check If Response Variable Is Close To Normal



plot(density(Assignment_2$PRES), main="Density Plot: PRES", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Assignment_2$PRES), 2)))  

polygon(density(Assignment_2$PRES), col="red")

plot(density(Assignment_2$TEMP), main="Density Plot: Temperature", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Assignment_2$TEMP), 2))) 

polygon(density(Assignment_2$TEMP), col="red")

#Correlation Analysis

cor(Assignment_2$PRES, Assignment_2$TEMP)
t.test(Assignment_2$PRES,Assignment_2$TEMP)
wilcox.test(Assignment_2$PRES,Assignment_2$TEMP)

#creating model TEMP~PRES
model7 <- lm(TEMP~PRES,data = Assignment_2)
model7#printing the model
confint(model7)#confint of model
summary(model7)#summary of model
#plot of PRES and temp
plot(Assignment_2$PRES,Assignment_2$TEMP,pch = 16, cex = 1.2, col = "blue", main = "Graph of PRES and Temperature", xlab = "PRES", ylab = "Temperature")
abline(model7)#abline of model
summary(model7$residuals)#summary of model
str(model7$residuals)#str of model residuals
plot(model7$residuals)#plot of model residuals
abline(h = 0, col = ("blue"))#drawing the abline
hist(model7$residuals)#histogram for residuals
qqnorm(model7$residuals)#qqplot for residuals
qqline(model7$residuals, col = "purple")#line for the qqplot

#--------------------------
#MOdel8

#creating model with year+month+hour+day+pm2.5
model8<-lm(TEMP~year+month+hour+day+pm2.5,data=Assignment_2)
model8#printinh the model
confint(model8)#confint of model
summary(model8)#summary of model
summary(model8$residuals)#summary of model residuals
str(model8$residuals)#str of model residuals
plot(model8$residuals)#plot of model residuals
abline(h = 0, col = ("blue"))#drawing the abline
hist(model8$residuals)#histogram for residuals
qqnorm(model8$residuals)#qqplot for residuals
qqline(model8$residuals, col = "purple")#line for the qqplot
#-------------------------------------------

#Model9

#creating the model with combination year+month+hour+day+DEWP
model9<-lm(TEMP~year+month+hour+day+DEWP,data=Assignment_2)
model9#printing model
confint(model9)#confint of model
summary(model9)#summary of model
summary(model9$residuals)#summary of residuals
str(model9$residuals)#str of model residuals
plot(model9$residuals)#plotting residuals
abline(h = 0, col = ("blue"))#drawing the abline
hist(model9$residuals)#histogram for residuals
qqnorm(model9$residuals)#qqplot for residuals
qqline(model9$residuals, col = "purple")#line for the qqplot
#-----------------------------

#MOdel10

#creating the model with combination year+month+hour+day+PRES
model10<-lm(TEMP~year+month+hour+day+PRES,data=Assignment_2)
model10#printing the model
confint(model10)#confint the model
summary(model10)#summary of model
summary(model10$residuals)#summary of model residuals
str(model10$residuals)#structure of residuals
plot(model10$residuals)#plot for residuals
abline(h = 0, col = ("blue"))#drawing the abline
hist(model10$residuals)#histogram for residuals
qqnorm(model10$residuals)#qqplot for residuals
qqline(model10$residuals, col = "purple")#line for the qqplot
#----------------------

#model11

#creating the model with combination year+month+hour+day+cbwd
model11<-lm(TEMP~year+month+hour+day+cbwd,data=Assignment_2)
model11#printing the model
confint(model11)#confint of model
summary(model11)#summary of model
summary(model11$residuals)#summary of residuals
str(model11$residuals)#structure of residuals
plot(model11$residuals)#plot for residuals
abline(h = 0, col = ("blue"))#drawing the abline
hist(model11$residuals)#histogram for residuals
qqnorm(model11$residuals)#qqplot for residuals
qqline(model11$residuals, col = "purple")#line for the qqplot

#------------------------------

#MOdel 12

#creating the model with combination year+month+hour+day+pm2.5+DEWP
model12<-lm(TEMP~year+month+hour+day+pm2.5+DEWP,data=Assignment_2)
model12#printing the model
confint(model12)#confint of model
summary(model2)#summary of model
summary(model12$residuals)#summary of residuals
str(model12$residuals)#structure of residuals
plot(model12$residuals)#plot for residuals
abline(h = 0, col = ("blue"))#drawing the abline
hist(model12$residuals)#histogram for residuals
qqnorm(model12$residuals)#qqplot for residuals
qqline(model12$residuals, col = "purple")#line for the qqplot
#-------------------------

#model13

#creating the model with combination year+month+hour+day+pm2.5+PRES
model13<-lm(TEMP~year+month+hour+day+pm2.5+PRES,data=Assignment_2)
model13#printing the model
confint(model13)#confint of model
summary(model13)#summary of model13
summary(model13$residuals)#summary of residuals
str(model13$residuals)#structure of residuals
plot(model13$residuals)#plot for residuals
abline(h = 0, col = ("blue"))#drawing the abline
hist(model13$residuals)#histogram for residuals
qqnorm(model13$residuals)#qqplot for residuals
qqline(model13$residuals, col = "purple")#line for the qqplot
#-------------------------------

#Model14

#creating the model with combination year+month+hour+day+pm2.5+cbwd
model14<-lm(TEMP~year+month+hour+day+pm2.5+cbwd,data=Assignment_2)
model14#printing the model
confint(model14)#confint of model
summary(model4)#summary of model
summary(model14$residuals)#summary of residuals of model
str(model14$residuals)#structure of model
plot(model14$residuals)#plot for residuals
abline(h = 0, col = ("blue"))#drawing the abline 
hist(model14$residuals)#histogram for residuals
qqnorm(model14$residuals)#qqplot for residuals
qqline(model14$residuals, col = "purple")#line for the qqplot

#performing AIC test to know the best model,lower the AIC value is the best model
AIC(model1)
AIC(model2)
AIC(model3)
AIC(model4)
AIC(model5)
AIC(model6)
AIC(model7)
AIC(model8)
AIC(model9)
AIC(model10)
AIC(model11)
AIC(model12)
AIC(model13)
AIC(model14)
#12,13 models are the best among all the models
#==============================================1q====================================

#=============================================2q================================================
#QUESTION2
# You should build a Shiny app or dashboard allowing a scatterplot for any combination of variables to be displayed.  
#Additionally,you should be able to generate histograms, boxplots etc. of your data in this app. 
# Defining the UI 
ui <- fluidPage(
   
   # The main title for the App
   titlePanel("Data Visualisation"),
   
   # Sidebar layout with the input and output definitions 
   sidebarLayout(
      
      # Sidebar panel for the inputs 
      sidebarPanel(
         
         # Input:To  Select the plot type with radio buttons
         #Created radiobutton for selecting the options  scatterplot,Histogram of x,Histogram of y,Boxplot
         radioButtons("type", "Plot type:",
                      c("Scatter" = "scat",
                        "Histogram of X"="Histogram of x",
                        "Histogram of Y"="Histogram of y",
                        "Boxplot" = "box")),
         
         
         # Input: Dropdown list for the variables to plot
         selectInput('x', 'X', names(Assignment_2), names(Assignment_2)[[2]]),#selecting the input for x variable
         selectInput('y', 'Y', names(Assignment_2), names(Assignment_2)[[1]]),#selecting the input for y variable
         
      ),
      
      # Main panel for displaying outputs
      mainPanel(
         
         # Output: Tabset with plots and table 
         #creating tab by name Plot all the plots will be generated in this tab
         tabsetPanel(type = "tabs",
                     tabPanel("Plot", plotOutput("plotxy"))
                     
         )
         
      )
   )
)


# Definining the server logic for app 
server <- function(input, output) {
   
   
   #Here the generation of plots process takes place and when ever one option is selected in radiobuttion the output will be generated based on the selected option  
   output$plotxy <- renderPlot({
      
      

      #if the scatterplot is  selected
      if(input$type=="scat"){
        #plotting thre scatterplot 
         plot(Assignment_2[input$x][[1]],Assignment_2[input$y][[1]],
              xlab = input$x, ylab=input$y,
              main = paste("Scatterplot of", input$y, "vs", input$x),col="blue")
      } 
      #if the Histogram of x is selected
      if(input$type=="Histogram of x"){
         #plotting the histogram for x
         hist(Assignment_2[input$x][[1]], 
              xlab = input$x, 
              main = paste("Histogram of", input$x),col="blue")
      }
      #if the Histogram of y is selected
      if(input$type=="Histogram of y"){
         #plotting the histogram for y
         hist(Assignment_2[input$y][[1]], 
              xlab = input$y, 
              main = paste("Histogram of", input$y),col="red")
      }
      
      
      # if boxplot is selected
      if(input$type=="box"){
         #plotting the boxplot
         boxplot(
            as.formula(
               paste(Assignment_2[input$y]," ~ ",Assignment_2[input$x])),
            xlab = input$x, ylab=input$y,
            main = paste("Boxplot of", input$y, "vs", input$x),col="blue" )
      }
      
   })
}

#To generate the app
shinyApp(ui, server)

#===============================================2q===============================================

#===============================================3q==============================================
#QUESTION3
#3) You should include the ability to fit a linear regression model to the scatterplots generated in (2). 
#The chart should include the fitted line and a table with the slope and intercept should be
#present within the Shiny App or dashboard. 


# Defining the UI 
ui <- fluidPage(
   # The main title for the App
   titlePanel("Regression Model "),
   # Sidebar layout with the input and output definitions 
   sidebarLayout(
      # Sidebar panel for the inputs 
      sidebarPanel(
         # Input:To  Select the plot type with radio buttons
         #Created radiobutton for selecting the options  scatterplot,Histogram of x,Histogram of y,Boxplot
         
         radioButtons("type", "Plot type:",
                      c("Scatter" = "scat",
                        "Histogram of X"="Histogram of x",
                        "Histogram of Y"="Histogram of y",
                        "Boxplot" = "box")),
         # Input: Dropdown list for the variables to plot
         #selecting the input for x variable
         selectInput("x", label = h3("x"),
                     choices = names(Assignment_2), selected = 1),
         #selecting the input for y variable
         selectInput("y", label = h3("y"),
                     choices = names(Assignment_2), selected = 1)
         
      ),
      # Main panel for displaying outputs
      
      mainPanel(
         # Output: Tabset with plots and table 
         #creating tab by name Plot all the plots will be generated in this tab
         tabsetPanel(type = "tabs",
                     tabPanel("Plot", plotOutput("plotxy")),
                     tabPanel("Intercept and slope", verbatimTextOutput("summary"))
                     
         )
      )
   ))



# SERVER
# Definining the server logic for app 

server <- function(input, output) {
   
   # Intercept and slope output
   output$summary <- renderPrint({
      fit <- lm(Assignment_2[,input$x] ~ Assignment_2[,input$y])
      names(fit$coefficients) <- c("Intercept", input$x)
      print(fit)
   })
   
  
   
   
   
   #Here the generation of plots process takes place and when ever one option is selected in radiobuttion the output will be generated based on the selected option  
   
   output$plotxy <- renderPlot({
      #if scatterplot selected
      if(input$type=="scat"){
         #plotting thre scatterplot 
            plot(Assignment_2[,input$x],Assignment_2[,input$y],  main="Scatterplot",
                 xlab=input$x, ylab=input$y, pch=19)
            abline(lm( Assignment_2[,input$y]~Assignment_2[,input$x]), col="red")
            lines(lowess(Assignment_2[,input$x],Assignment_2[,input$y]), col="blue")
         }
      #if the Histogram of x is selected
      if(input$type=="Histogram of x"){
         
         #plotting the histogram for x
         
         hist(Assignment_2[input$x][[1]], 
              xlab = input$x, 
              main = paste("Histogram of", input$x),col="blue")
      }
      #if the Histogram of y is selected
      
      if(input$type=="Histogram of y"){
         #plotting the histogram for y
         
         hist(Assignment_2[input$y][[1]], 
              xlab = input$y, 
              main = paste("Histogram of", input$y),col="red")
      }
      
      
      # if boxplot is selected
      if(input$type=="box"){
         # plotting the boxplot
         boxplot(
            as.formula(
               paste(Assignment_2[input$y]," ~ ",Assignment_2[input$x])),
            xlab = input$x, ylab=input$y,
            main = paste("Boxplot of", input$y, "vs", input$x),col="blue" )
      }
      
      

   })
}
#To generate the app

shinyApp(ui = ui, server = server)

#===============================================3q=============================================

#================================================4q=============================================

#MONTE CARLO STIMULATION

#Using Monte Carlo simulations, you should attempt to predict the temperature in subsequent years.
#This should be done using at least two different models (i.e. different collections of variables in part 1). 
#You should clearly state which performs best. 

#corrplot is used for the graphical display of a correlation matrix, confidence interval or general matrix
corrplot(cor(Assignment_2))#corrplot for Assignment_2 dataframe

par(mfrow=c(1, 2))  # divide graph area in 2 columns

#For Model1

#histogram of TEMP column of Assignment_2 dataframe
hist(Assignment_2$TEMP, breaks = 10)
stem(Assignment_2$TEMP)#stem for TEMP column of Assignment_2 dataframe

#finding mean absolute deviation (mad) for the model TEMP~year+month+hour+day+pm2.5+DEWP and storing in disp variable
disp=mad(summary(lm(TEMP~year+month+hour+day+pm2.5+DEWP , data = Assignment_2))$resid)

disp#printing disp value

#model coefficients for the model EMP~year+month+hour+day+pm2.5+DEWP and stored in  model

model=(summary(lm(TEMP~year+month+hour+day+pm2.5+DEWP, data = Assignment_2))$coef)

model#printing the model

#considering Ysim variable and assigned NULL to it
ysim=NULL
#performing the monte carlo stimulation
for(i in 1:500){
   ysim=cbind(ysim ,model[1] +
                 model[2]*mean(Assignment_2$year) +
                 model[3]*mean(Assignment_2$month) +  
                 model[4]*mean(Assignment_2$hour) + 
                 model[5]*mean(Assignment_2$day)+
                 model[6]*mean(Assignment_2$pm2.5)+
                 model[7]*mean(Assignment_2$DEWP) +runif(1,-4*disp,4*disp) ) 
   
}

hist(ysim)#histogram of stimulated variable i.e ysim

abline(v=mean(Assignment_2$TEMP),col="red")#drawing the abline by using the mean of temp column of Assignment_2


#For Model2
#model2

#histogram of TEMP column of Assignment_2 dataframe

hist(Assignment_2$TEMP, breaks = 10)
stem(Assignment_2$TEMP)#stem for TEMP column of Assignment_2 dataframe


#finding mean absolute deviation (mad) for the model TEMP~year+month+hour+day+pm2.5+PRES and storing in disp1 variable
disp1=mad(summary(lm(TEMP~year+month+hour+day+pm2.5+PRES , data = Assignment_2))$resid)

disp1#printing disp1 value

#model coefficients for the model EMP~year+month+hour+day+pm2.5+PRES and stored in model1

model1=(summary(lm(TEMP~year+month+hour+day+pm2.5+PRES, data = Assignment_2))$coef)

model1#printing the model1 value

#considering Ysim1 variable and assigned NULL to it

ysim1=NULL
#performing the monte carlo stimulation

for(i in 1:500){
   ysim1=cbind(ysim1 ,model1[1] +
                 model1[2]*mean(Assignment_2$year) +
                 model1[3]*mean(Assignment_2$month) +  
                 model1[4]*mean(Assignment_2$hour) + 
                 model1[5]*mean(Assignment_2$day)+
                 model1[6]*mean(Assignment_2$pm2.5)+
                 model1[7]*mean(Assignment_2$PRES) +runif(1,-4*disp,4*disp) ) 
   
}

hist(ysim1)#histogram of stimulated variable i.e ysim1

#drawing the abline by using the mean of temp column of Assignment_2

abline(v=mean(Assignment_2$TEMP),col="red")


#============================================4q==================================================

#============================================5q==================================================
#QUESTION5
#We now consider two linear models: one where we consider y as temperature and x as pressure and a second with y as 
#temperature and both pressure and wind speed as x variables.  You should apply both models and calculate the ESS statistics on the data
#(some notes on this statistic can be found here: https://www.graphpad.com/guides/prism/7/curvefitting/reg_howtheftestworks.htm?toc=0&printWindow ). 
#You are required to generate a distribution for this test statistic by simulation; you may assume the errors and residuals are normal. 
#You should clearly state whether this model is an appropriate fit to the data based on your simulations.  


#Assigning the Assignment_2$TEMP to TEMP

TEMP<-Assignment_2$TEMP

#Assigning the Assignment_2$PRES to PRES

PRES<-Assignment_2$PRES

#Assigning the Assignment_2$Iws to Iws

Iws<-Assignment_2$Iws

#creating a model coefficient for TEMP~PRES and assigining it to model1
model1=summary(lm(TEMP~PRES))$coef
model1#printing the model1

#finding mean absolute deviation (mad) for the model TEMP~PRES and storing in disp variable

disp=mad((summary(lm(TEMP~PRES))$resi))
disp#printing disp value


#creating a model coefficient for TEMP~PRES+Iws and assigining it to model2

model2=summary(lm(TEMP~PRES+Iws))$coef
model2#printing the model2
#finding mean absolute deviation (mad) for the model TEMP~PRES+Iws and storing in disp1 variable
disp1=mad((summary(lm(TEMP~PRES+Iws))$resi))
disp1#printing disp1

#finding the sum of square for model1
ss1=sum((lm(TEMP~PRES)$resi)^2)
#finding the sum of square for model2

ss2=sum((lm(TEMP~PRES+Iws)$resi)^2)
#degree of freedom for model1,degree of freedom is defined as  the number of data points minus the number of parameters 
#so in model1 number of parameters are 2 
df1<-nrow(Assignment_2)-2
df1#printing df1 value

#degree of freedom for model2,degree of freedom is defined as  the number of data points minus the number of parameters 
#so in model2 number of parameters are 3 
df2<-nrow(Assignment_2)-3
df2#printing df2 value
#Formula for the extra sum of squares of f_test
F=((ss1-ss2)/ss2)/((df1-df2)/df2)#finding the F value
F#printing the F value

#set up MC simulation
#considering rsss1,rsss2,ysim1,ysim2 and assining NULL to them
rsss1=NULL
ysim1=NULL
rsss2=NULL
ysim2=NULL

#considering a  empty vector and assigning it to variable a
a <- c()

#Doing the stimulation
for(i in 1:500){#simulate dataset 500 times, calcuate rss on each dataset
   ysim1=model1[1]+model1[2]*PRES+rnorm(length(PRES),0,disp)#stimulation for model1
   rsss1=sum((lm(ysim1~PRES)$resi)^2)#sum of square for model1
   
   ysim2=model2[1]+model2[2]*PRES+model2[3]*Iws+rnorm(length(PRES+Iws),0,disp1)#stimulation for model2
   rsss2=cbind(rsss2,sum((lm(ysim2~PRES+Iws)$resi)^2))#sum of square for model2
   
   df1<-nrow(Assignment_2)-2#degree of freedom for model1
   df2<-nrow(Assignment_2)-3#degree of freedom for model2
   F=((rsss1-rsss2)/rsss2)/((df1-df2)/df2)#calculating f value
   a[i]<-F#assigining f value to a vector
   
}
cat(a)#printing out the vetcor a value
#generating histograms for ysim2 and rsss2
hist(rsss2)
hist(ysim2)

#=============================================5q===============================================




