#IST 687 Project

#CW comment
#https://archive.ics.uci.edu/ml/datasets/Adult
#How can we determien msot effectively if someone makes over 50,000.
#What are indicators that we can use to best predict this? Education, relationship, native country, etc?
#If I am a certain job title can we predict a base of pay?
#Formula to make prediction

#cleaning the dataset

#basic functions: mean, mode, regression

#histograms

#ggplot2

#world heat map

#randomForest

####################################################################
#################### Load Data and Name Columns ####################
####################################################################
#URL to dataset
url <-"https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"

#Loading data without a header (dataset doesn't contain a header)
adultData <- read.csv(url, header = FALSE)
nrow(adultData)
#-- 32561

colnames(adultData)
head(adultData)

#Renaming the columns
colnames(adultData) <- c("age", "workclass", "fnlwgt", "education", "education-num", "marital-status", 
                         "occupation", "relationship", "race", "sex", "capital-gain", "capital-loss", 
                         "hours-per-week", "native-country", "income")

colnames(adultData)
head(adultData)
str(adultData)

####################################################################
####################### Cleaning the dataset #######################
####################################################################

#Removing 'capital-gain' and 'capital-loss'
adultData <- adultData[,-11:-12]
colnames(adultData)  

#Columns with NAs
colnames(adultData)[colSums(is.na(adultData)) > 0]
#-- Zero initially                     

#Finding question marks/periods and making NA
idx <- adultData == " ?"
is.na(adultData) <- idx

#Columns with NAs
colnames(adultData)[colSums(is.na(adultData)) > 0]
#-- [1] "workclass"      "occupation"     "native-country"

#Number of rows before
nrow(adultData) 
#-- 32561

#Count of total NAs
sum(is.na(adultData))
#-- 4262

#Omit the NAs
adultData <- na.omit(adultData)

#Count of NAs after
sum(is.na(adultData))
#-- 0

#New number of rows
nrow(adultData) 
#-- 30162





###JT###

#useful binary variable
adultData$income.num <-ifelse(adultData$income==' >50K',1,0)
adultData$sex.num <- ifelse(adultData$sex ==' Male',1,0) #made male =1 female0
head(adultData)
View(adultData)
#basic histogram
numvar = adultData$age
adultHistogram <- ggplot(adultData) + aes(x=as.numeric(numvar)) + geom_histogram(binwidth=1,color="black")
adultHistogram

adultHistDens <- ggplot(adulData) + aes(x=as.numeric(numvar))+geom_density()
adultHistDens

#density outline of the histogram shows that there are two modes in the age spectrum, but looking closer,
#there is one true mode
skewness(numvar)
#this equals 0.5586919, showing skewness to the right


##(this one is a work in progress)
#representation of number of people with income over 50
#education and age are used as explanatory variables

adultData2 = adultData %>%
  group_by(education,age)%>%
  summarise(
    over50k = sum(income.num, na.rm=TRUE)/n()
  )
num1=adultData2$age
num2=adultData2$over50k
num1
num2

educationAge <- ggplot(adultData2, aes(num1,num2))
educationAge + geom_point(aes(color=education))
                          
                          
#lm for education and income, then also do regression analysis 
sexHours <- lm(adultData$sex.num ~ adultData$`hours-per-week`)                          
predict(sexHours, adultData, type = "response")
summary(sexHours)

educationIncome <- lm(adultData$income.num ~ adultData$`education-num`)
predict(educationIncome, adultData, type = "response")
summary(educationIncome)

incomeSexEdHours <- lm(adultData$income.num ~ adultData$`education-num` + adultData$sex.num +adultData$`hours-per-week`)
predict(incomeSexEdHours, adultData, type = "response")
summary(incomeSexEdHours)                          

incomeAgeHoursEd <- lm(adultData$income.num ~ adultData$age + adultData$`hours-per-week` +adultData$`education-num`)
predict(incomeAgeHoursEd, adultData, type = "response")
summary(incomeAgeHoursEd)

library(tidyverse)
library(rworldmap)
#world map
#Getting countries
map.world <- map_data("world")

#https://blog.learningtree.com/how-to-display-data-on-a-world-map-in-r/
#cleaning data to match countries in map.world
adultData$`native-country` <- recode(adultData$`native-country`,' United-States' = 'USA',' England' = ' UK',' Columbia' = 'Colombia', ' Puerto-Rico' = 'Puerto Rico',' Cambodia' = 'Cambodia',' Canada' = 'Canada',' Germany' = 'Germany')

mapped_data <- joinCountryData2Map(adultData, joinCode = "ISO3", nameJoinColumn = "native-country")

par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapCountryData(mapped_data, nameColumnToPlot = "income")
View(adultData)
