---
title: "IST_707_Project"
author: "Trevor Kahl"
date: "5/4/2020"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## load the libraries

```{r}
library(readxl)
library(arules)
library(arulesViz)
library(datasets)
library(randomForest)
```
## load the data


```{r cars}
asylumTrends1 <-read_excel("C:/Users/trev9/OneDrive - Syracuse University/IST 707/IST_707_Project/2014AsylumTrends/2014_Asylum_Trends_annex_tables_v3_with_Freedom.xls", sheet = 2, col_names = FALSE, col_types = NULL)
asylumTrends2 <- read_excel("C:/Users/trev9/OneDrive - Syracuse University/IST 707/IST_707_Project/2014AsylumTrends/2014_Asylum_Trends_annex_tables_v3_with_Freedom.xls", sheet = 3, col_names = FALSE, col_types = NULL)
asylumTrends3 <- read_excel("C:/Users/trev9/OneDrive - Syracuse University/IST 707/IST_707_Project/2014AsylumTrends/2014_Asylum_Trends_annex_tables_v3_with_Freedom.xls", sheet = 4, col_names = FALSE, col_types = NULL)

# (JEAN) adding my own desktop to load from too
asylumTrends1 <- read_excel("/Users/jeanthompson/Desktop/Syracuse/IST\ 707/Project/2014_Asylum_Trends_annex_tables_v3_with_Freedom.xls", sheet = 2, col_names=FALSE,col_types = NULL)
asylumTrends2 <-read_excel("/Users/jeanthompson/Desktop/Syracuse/IST\ 707/Project/2014_Asylum_Trends_annex_tables_v3_with_Freedom.xls",sheet = 3, col_names = FALSE, col_types = NULL)
asylumTrends3 <- read_excel("/Users/jeanthompson/Desktop/Syracuse/IST\ 707/Project/2014_Asylum_Trends_annex_tables_v3_with_Freedom.xls", sheet = 4, col_names = FALSE, col_types = NULL)

View(asylumTrends3)
str(asylumTrends1)
str(asylumTrends2)
str(asylumTrends3)
```

## cleaning rows and columns for asylumTrends1


```{r}
#removoing irrelevant rows in begginning and end
asylumTrends1 <- asylumTrends1[-1:-6,] #take first rows so we can add headers below
asylumTrends1 <- asylumTrends1 [-51:-91,] #take out rows with na in new added cols
#add col names to make data readable as it was in the file
colnames(asylumTrends1) <- c("Country", "2010", "2011", "2012", "2013", "2014", "Total", "AnnualChange14-13", "Share2014", "Share10-14", "Rank2014", "Rank10-14", "1000inhabitantsTotal2014" ,"1000inhabitantsTotal10-14", "1000inhabitantsRank2014", "1000inhabitantsRank10-14", "1GDPperCapitaTotal2014", "1GDPperCapitaTotal10-14", "1GDPperCapitaRank2014", "1GDPperCapitaRank10-14", "Region", "Free", "Political Rights", "Civil Liberties", "Electoral","PolPluralism", "Governance","PolRights", "Expression", "Organization","Rule of Law", "IndRights", "Liberties Score" )
str(asylumTrends1)
#make all cols num except free and politcal rights
asylumTrends1[,2:20]<- lapply(asylumTrends1[,2:20], FUN = as.numeric)
asylumTrends1[,24:33]<- lapply(asylumTrends1[,24:33], FUN = as.numeric)
#make factors for free and political rights
asylumTrends1$Country<- as.factor(asylumTrends1$Country)
asylumTrends1$Region<- as.factor(asylumTrends1$Region)
asylumTrends1$Free<- as.factor(asylumTrends1$Free)
asylumTrends1$`Political Rights` <- as.factor(asylumTrends1$`Political Rights`)
asylumTrends1$`Civil Liberties` <- as.factor(asylumTrends1$`Civil Liberties`)
str(asylumTrends1)
View(asylumTrends1)
#make df removing totals in the rows
asylumTrends1NoTotals <- asylumTrends1[-42:-50,] # remove Totals
asylumTrends1NoTotals <- asylumTrends1NoTotals[-22,] #remove Lie
View(asylumTrends1NoTotals)

#asylumTrends1NoTotals <- asylumTrends1NoTotals[-36,] # remove 'of Kosovo' breakdown
str(asylumTrends1NoTotals)
# Make new df so have cleaned with int and not cuts
asylumTrends1Cuts <- asylumTrends1NoTotals
asylumTrends1Cuts$`2010` <- cut(asylumTrends1NoTotals$`2010`, breaks = c(0,15000,30000,45000,60000,75000,90000,Inf),labels = c("small", "small-high", "mid-low", "mid" ,"mid-high", "high", "highest"))
asylumTrends1Cuts$`2011` <- cut(asylumTrends1NoTotals$`2011`, breaks = c(0,15000,30000,45000,60000,75000,90000,Inf),labels = c("small", "small-high", "mid-low", "mid" ,"mid-high", "high", "highest"))
asylumTrends1Cuts$`2012` <- cut(asylumTrends1NoTotals$`2012`, breaks = c(0,15000,30000,45000,60000,75000,90000,Inf),labels = c("small", "small-high", "mid-low", "mid" ,"mid-high", "high", "highest"))
asylumTrends1Cuts$`2013` <- cut(asylumTrends1NoTotals$`2013`, breaks = c(0,15000,30000,45000,60000,75000,90000,Inf),labels = c("small", "small-high", "mid-low", "mid" ,"mid-high", "high", "highest"))
asylumTrends1Cuts$`2014` <- cut(asylumTrends1NoTotals$`2014`, breaks = c(0,15000,30000,45000,60000,75000,90000,Inf),labels = c("small", "small-high", "mid-low", "mid" ,"mid-high", "high", "highest"))
asylumTrends1Cuts$Electoral < cut(asylumTrends1NoTotals$Electoral, breaks = c(5,9,12,Inf), labels = c("low", "mid", "high"))# 3 cuts
asylumTrends1Cuts$PolPluralism <- cut(asylumTrends1NoTotals$PolPluralism, breaks = c(8,11,14,Inf), labels = c("low", "mid", "high"))#3 cuts
asylumTrends1Cuts$Governance <- cut(asylumTrends1NoTotals$Governance, breaks = c(3,5,8,10,Inf), labels = c("low","mid-low", "mid-high", "high")) #4 cuts
asylumTrends1Cuts$PolRights <- cut(asylumTrends1NoTotals$PolRights, breaks = c(16,22,33,37,Inf), labels = c("low","mid-low", "mid-high", "high")) #4 cuts
asylumTrends1Cuts$Expression <- cut(asylumTrends1NoTotals$Expression, breaks = c(5,10,14,Inf), labels = c("low", "mid", "high")) #3 cuts
asylumTrends1Cuts$Organization <- cut(asylumTrends1NoTotals$Organization, breaks = c(3,7,10,Inf), labels = c("low", "mid", "high")) #3 cuts
asylumTrends1Cuts$`Rule of Law` <- cut(asylumTrends1NoTotals$`Rule of Law`, breaks = c(3,9,12,14,Inf), labels = c("low","mid-low", "mid-high", "high")) #4 cuts
asylumTrends1Cuts$IndRights <- cut(asylumTrends1NoTotals$IndRights, breaks = c(5,9,12,14,Inf), labels = c("low","mid-low", "mid-high", "high")) #4 cuts
asylumTrends1Cuts$`Liberties Score` <- cut(asylumTrends1NoTotals$`Liberties Score`, breaks = c(16,34,49,Inf), labels = c("NF","PF","F")) # 3 cuts

str(asylumTrends1Cuts)
View(asylumTrends1NoTotals)
View(asylumTrends1)
View(asylumTrends1Cuts)
```

## cleaning rows and columns for asylumTrends2

```{r}
asylumTrends2 <- asylumTrends2[-1:-6,]
asylumTrends2 <- asylumTrends2[-8:-16,] #take out total and rest of cols
View(asylumTrends2)
str(asylumTrends2)
colnames(asylumTrends2) <- c("Country", "2010", "2011", "2012", "2013", "2014", "Total", "AnnualChange14-13", "Share2014", "Share10-14","Rank2002","Rank2003","Rank2004","Rank2005", "Rank2014", "Rank10-14", "1000inhabitantsTotal2014" ,"1000inhabitantsTotal10-14", "1000inhabitantsRank2014", "1000inhabitantsRank10-14", "1GDPperCapitaTotal2014", "1GDPperCapitaTotal10-14", "1GDPperCapitaRank2014", "1GDPperCapitaRank10-14","Region", "Free", "Political Rights", "Civil Liberties", "Electoral","PolPluralism", "Governance","PolRights", "Expression", "Organization","Rule of Law", "IndRights", "Liberties Score")
asylumTrends2[,6:24] <- lapply(asylumTrends2[,6:24], FUN = as.numeric)
asylumTrends2[,27:37] <- lapply(asylumTrends2[,27:37], FUN = as.numeric)
View(asylumTrends2)
asylumTrends2$Country <- as.factor(asylumTrends2$Country)
asylumTrends2$Region <- as.factor(asylumTrends2$Region)
asylumTrends2$Free <- as.factor(asylumTrends2$Free)
asylumTrends2$`Political Rights` <- as.factor(asylumTrends2$`Political Rights`)
asylumTrends2$`Civil Liberties` <- as.factor(asylumTrends2$`Civil Liberties`)

asylumTrends2Cuts <- asylumTrends2
asylumTrends2Cuts$`2010` <- cut(asylumTrends2$`2010`, breaks = c(0,500,1000,1500,2000,2500,3000,Inf),labels = c("small", "small-high", "mid-low", "mid" ,"mid-high", "high", "highest"))
asylumTrends2Cuts$`2011` <- cut(asylumTrends2$`2011`, breaks = c(0,500,1000,1500,2000,2500,3000,Inf),labels = c("small", "small-high", "mid-low", "mid" ,"mid-high", "high", "highest"))
asylumTrends2Cuts$`2012` <- cut(asylumTrends2$`2012`, breaks = c(0,500,1000,1500,2000,2500,3000,Inf),labels = c("small", "small-high", "mid-low", "mid" ,"mid-high", "high", "highest"))
asylumTrends2Cuts$`2013` <- cut(asylumTrends2$`2013`, breaks = c(0,500,1000,1500,2000,2500,3000,Inf),labels = c("small", "small-high", "mid-low", "mid" ,"mid-high", "high", "highest"))
asylumTrends2Cuts$`2014` <- cut(asylumTrends2$`2014`, breaks = c(0,500,1000,1500,2000,2500,3000,Inf),labels = c("small", "small-high", "mid-low", "mid" ,"mid-high", "high", "highest"))
asylumTrends2Cuts$Electoral < cut(asylumTrends2$Electoral, breaks = c(-1,3,8,Inf), labels = c("low", "mid", "high"))# 3 cuts
asylumTrends2Cuts$PolPluralism <- cut(asylumTrends2$PolPluralism, breaks = c(1,8,14,Inf), labels = c("low", "mid", "high"))#3 cuts
asylumTrends2Cuts$Governance <- cut(asylumTrends2$Governance, breaks = c(-1,3,5,8,Inf), labels = c("low","mid-low", "mid-high", "high")) #4 cuts
asylumTrends2Cuts$PolRights <- cut(asylumTrends2$PolRights, breaks = c(1,10,20,26,Inf), labels = c("low","mid-low", "mid-high", "high")) #4 cuts
asylumTrends2Cuts$Expression <- cut(asylumTrends2$Expression, breaks = c(1,9,14,Inf), labels = c("low", "mid", "high")) #3 cuts
asylumTrends2Cuts$Organization <- cut(asylumTrends2$Organization, breaks = c(0,7,10,Inf), labels = c("low", "mid", "high")) #3 cuts
asylumTrends2Cuts$`Rule of Law` <- cut(asylumTrends2$`Rule of Law`, breaks = c(-1,9,12,14,Inf), labels = c("low","mid-low", "mid-high", "high")) #4 cuts
asylumTrends2Cuts$IndRights <- cut(asylumTrends2$IndRights, breaks = c(-1,9,12,14,Inf), labels = c("low","mid-low", "mid-high", "high")) #4 cuts
asylumTrends2Cuts$`Liberties Score` <- cut(asylumTrends2$`Liberties Score`, breaks = c(7,34,49,Inf), labels = c("NF","PF","F")) # 3 cuts
View(asylumTrends2Cuts)
str(asylumTrends2)
View(asylumTrends2)
```


#clean aslyumTrends3

```{r}
asylumTrends3 <- asylumTrends3[-1:-5,] #take first rows so we can add headers below
asylumTrends3 <- asylumTrends3 [-40:-43,] #take out end rows of other total and blanks
View(asylumTrends3)
asylumTrends3 <- asylumTrends3[-12,]#remove stateless
asylumTrends3 <- asylumTrends3[-17,]#remove various/unknown
asylumTrends3 <- asylumTrends3[-22,]#remove Congo
colnames(asylumTrends3) <- c("Country","2013", "2014","Total","Share2013", "Share2014","AvgTotalShare","Rank2014", "Rank10-14", "AnnualChange","Region", "Free", "Political Rights", "Civil Liberties", "Electoral","PolPluralism", "Governance","PolRights", "Expression", "Organization","Rule of Law", "IndRights", "Liberties Score")

asylumTrends3[,2:10] <- lapply(asylumTrends3[,2:10], FUN = as.numeric)
asylumTrends3[,13:23] <- lapply(asylumTrends3[,13:23], FUN = as.numeric)

asylumTrends3$Country <- as.factor(asylumTrends3$Country)
asylumTrends3$Region <- as.factor(asylumTrends3$Region)
asylumTrends3$Free <- as.factor(asylumTrends3$Free)
asylumTrends3 <- asylumTrends3[-12,]
asylumTrends3 <- asylumTrends3[-21,]
asylumTrends3 <- asylumTrends3[-35,]
asylumTrends3 <- asylumTrends3 [-16,]
str(asylumTrends3)
asylumTrends3Cuts <- asylumTrends3

View(asylumTrends3)
asylumTrends3Cuts$`2013` <- cut(asylumTrends3$`2013`, breaks = c(0,500,1000,1500,2000,2500,3000,Inf),labels = c("small", "small-high", "mid-low", "mid" ,"mid-high", "high", "highest"))
asylumTrends3Cuts$`2014` <- cut(asylumTrends3$`2014`, breaks = c(0,500,1000,1500,2000,2500,3000,Inf),labels = c("small", "small-high", "mid-low", "mid" ,"mid-high", "high", "highest"))
asylumTrends3Cuts$`Political Rights` < cut(asylumTrends3$`Political Rights`, breaks = c(1,4,9,Inf), labels = c("low", "mid", "high"))# 3 cuts
asylumTrends3Cuts$`Civil Liberties` <- cut(asylumTrends3$`Civil Liberties`, breaks = c(1,4,9,Inf), labels = c("low", "mid", "high"))#3 cuts
asylumTrends3Cuts$Electoral < cut(asylumTrends3$Electoral, breaks = c(-1,6,11,Inf), labels = c("low", "mid", "high"))# 3 cuts
asylumTrends3Cuts$PolPluralism <- cut(asylumTrends3$PolPluralism, breaks = c(-1,6,11,Inf), labels = c("low", "mid", "high"))#3 cuts
asylumTrends3Cuts$Governance <- cut(asylumTrends3$Governance, breaks = c(-1,3,6,10,Inf), labels = c("low","mid-low", "mid-high", "high")) #4 cuts
asylumTrends3Cuts$PolRights <- cut(asylumTrends3$PolRights, breaks = c(-4,10,22,35,Inf), labels = c("low","mid-low", "mid-high", "high")) #4 cuts
asylumTrends3Cuts$Expression <- cut(asylumTrends3$Expression, breaks = c(-1,6,11,Inf), labels = c("low", "mid", "high")) #3 cuts
asylumTrends3Cuts$Organization <- cut(asylumTrends3$Organization, breaks = c(-1,5,10,Inf), labels = c("low", "mid", "high")) #3 cuts
asylumTrends3Cuts$`Rule of Law` <- cut(asylumTrends3$`Rule of Law`, breaks = c(-1,3,8,12,Inf), labels = c("low","mid-low", "mid-high", "high")) #4 cuts
asylumTrends3Cuts$IndRights <- cut(asylumTrends3$IndRights, breaks = c(-1,4,9,13,Inf), labels = c("low","mid-low", "mid-high", "high")) #4 cuts
asylumTrends3Cuts$`Liberties Score` <- cut(asylumTrends3$`Liberties Score`, breaks = c(0,26,42,Inf), labels = c("NF","PF","F")) # 3 cuts

View(asylumTrends3Cuts)
str(asylumTrends3Cuts)

```

# summaries of frequency for table 1 (europe and selected non-europe)
```{r}

# count the appearances for size name by year
asylumTrends1_sizes2010 <- as.data.frame(table(asylumTrends1Cuts$'2010'))
asylumTrends1_sizes2011 <- as.data.frame(table(asylumTrends1Cuts$'2011'))
asylumTrends1_sizes2012 <- as.data.frame(table(asylumTrends1Cuts$'2012'))
asylumTrends1_sizes2013 <- as.data.frame(table(asylumTrends1Cuts$'2013'))
asylumTrends1_sizes2014 <- as.data.frame(table(asylumTrends1Cuts$'2014'))

asylumTrends1_sizes2010
asylumTrends1_sizes2011
asylumTrends1_sizes2012
asylumTrends1_sizes2013
asylumTrends1_sizes2014

# creating a clean set with the raw numbers

View(asylumTrends1)


#histograms
hist(asylumTrends1NoTotals$'2010', main="2010 Trends")
hist(asylumTrends1NoTotals$'2011', main="2011 Trends")
hist(asylumTrends1NoTotals$'2012', main="201 Trends")
hist(asylumTrends1NoTotals$'2013', main="2013 Trends")
hist(asylumTrends1NoTotals$'2014', main="2014 Trends")
hist(asylumTrends1NoTotals$Total, main="Overall Trends")

#df ordered by rank
rank2014 <- asylumTrends1NoTotals[order(asylumTrends1NoTotals$Rank2014),]
rankAll <- asylumTrends1NoTotals[order(asylumTrends1NoTotals$'Rank10-14'),]
rank2014 <- data.frame(rank2014)
rankAll <- data.frame(rankAll)
colnames(rank2014) <- c("Country", "2010", "2011", "2012", "2013", "2014", "Total", "AnnualChange 14-13", "Share2014", "Share10-14", "Rank2014", "Rank10-14", "1000inhabitantsTotal2014" ,"1000inhabitantsTotal10-14", "1000inhabitantsRank2014", "1000inhabitantsRank10-14", "1GDPperCapitaTotal2014", "1GDPperCapitaTotal10-14", "1GDPperCapitaRank2014", "1GDPperCapitaRank10-14")
colnames(rankAll) <- c("Country", "2010", "2011", "2012", "2013", "2014", "Total", "AnnualChange 14-13", "Share2014", "Share10-14", "Rank2014", "Rank10-14", "1000inhabitantsTotal2014" ,"1000inhabitantsTotal10-14", "1000inhabitantsRank2014", "1000inhabitantsRank10-14", "1GDPperCapitaTotal2014", "1GDPperCapitaTotal10-14", "1GDPperCapitaRank2014", "1GDPperCapitaRank10-14")


############# this is giving me errors? ###########
#bar plots for top 10 by rank
rank2014 <- asylumTrends1NoTotals[order(asylumTrends1NoTotals$Rank2014),]
toprank2014 <- rank2014[-(11:44),]
ggplot(data=toprank2014, aes(x=Country, y=Rank2014) + geom_bar())

```


## arm for 1


```{r}
#make df taking out num cols to run arm
asylumTrends1CutsRules <- asylumTrends1Cuts
asylumTrends1CutsRules <- asylumTrends1CutsRules[,-7:-20]
asylumTrends1CutsRules <- asylumTrends1CutsRules[,-11]

#itemFrequencyPlot(asylumTrends1CutsRules,topN=20,type="absolute")
#itemFrequencyPlot(asylumTrends2,topN=20,type="absolute")

#rules run for asylumTrends1
rulesAsylumTrends1 <- apriori(asylumTrends1CutsRules, parameter = list(supp = 0.2, conf = .8,minlen=3))
rulesAsylumTrends1<-sort(rulesAsylumTrends1, decreasing=FALSE,by="confidence")
options(digits = 2)
inspect(rulesAsylumTrends1[1:30])

rulesAsylumTrends1 <- apriori(asylumTrends1CutsRules, parameter = list(supp = 0.2, conf = .85,minlen=3))
rulesAsylumTrends1<-sort(rulesAsylumTrends1, decreasing=FALSE,by="confidence")
options(digits = 2)
inspect(rulesAsylumTrends1[1:30])

#run rules with rhs as year and type
rules2010<-apriori(data=asylumTrends1CutsRules, parameter=list(supp=0.001,conf = 0.08,minlen=2),
               appearance = list(default="lhs",rhs="2010=small"),
               control = list(verbose=F))
rules2010<-sort(rules2010, decreasing=TRUE,by="confidence")
inspect(rules2010[1:10])
```

##arm for 2

```{r}
#make df taking out num cols to run arm
str(asylumTrends2CutsRules)
asylumTrends2CutsRules <- asylumTrends2Cuts
asylumTrends2CutsRules <- asylumTrends2CutsRules[,-7:-24]
asylumTrends2CutsRules <- asylumTrends2CutsRules[,-11]

#itemFrequencyPlot(asylumTrends1CutsRules,topN=20,type="absolute")
#itemFrequencyPlot(asylumTrends2,topN=20,type="absolute")

#rules run for asylumTrends1
rulesAsylumTrends2 <- apriori(asylumTrends2CutsRules, parameter = list(supp = 0.2, conf = .8,minlen=3))
rulesAsylumTrends2<-sort(rulesAsylumTrends2, decreasing=FALSE,by="confidence")
options(digits = 2)
inspect(rulesAsylumTrends2[1:30])

#run rules with rhs as year and type
asylum2rules2010<-apriori(data=asylumTrends2CutsRules, parameter=list(supp=0.001,conf = 0.08,minlen=2),
               appearance = list(default="lhs",rhs="2010=small"),
               control = list(verbose=F))
asylum2rules2010<-sort(asylum2rules2010, decreasing=TRUE,by="confidence")
inspect(asylum2rules2010[1:10])
```
#arm for 3

```{r}
View(asylumTrends3CutsRules)
str(asylumTrends3CutsRules)
asylumTrends3CutsRules <- asylumTrends3Cuts
asylumTrends3CutsRules <- asylumTrends3CutsRules[,-4:-10]
asylumTrends3CutsRules <- asylumTrends3CutsRules[,-6]
asylumTrends3CutsRules <- asylumTrends3CutsRules[,-7]

#itemFrequencyPlot(asylumTrends1CutsRules,topN=20,type="absolute")
#itemFrequencyPlot(asylumTrends2,topN=20,type="absolute")

#rules run for asylumTrends1
rulesAsylumTrends3 <- apriori(asylumTrends3CutsRules, parameter = list(supp = 0.2, conf = .8,minlen=3))
rulesAsylumTrends3<-sort(rulesAsylumTrends3, decreasing=FALSE,by="confidence")
options(digits = 2)
inspect(rulesAsylumTrends3[1:30])

rulesAsylumTrends3 <- apriori(asylumTrends3CutsRules, parameter = list(supp = 0.2, conf = .85,minlen=3))
rulesAsylumTrends3<-sort(rulesAsylumTrends3, decreasing=FALSE,by="confidence")
options(digits = 2)
inspect(rulesAsylumTrends3[1:30])

rulesAsylumTrends3 <- apriori(asylumTrends3CutsRules, parameter = list(supp = 0.2, conf = .9,minlen=3))
rulesAsylumTrends3<-sort(rulesAsylumTrends3, decreasing=FALSE,by="confidence")
options(digits = 2)
inspect(rulesAsylumTrends3[1:30])


```


##random forest models 1
```{r}
asylumTrends1RF <- data.frame(asylumTrends1NoTotals)
asylumTrends1RF <- asylumTrends1RF[,-8:-12]
asylumTrends1RF <- asylumTrends1RF[,-18:-28]
asylumTrends1RF <- asylumTrends1RF[,-1]
asylumTrends1RF
str(asylumTrends1RF)
ATRF1 <- randomForest(asylumTrends1RF[,-16], asylumTrends1RF[,16], prox=TRUE)
ATRF1
importance(ATRF1)
View(asylumTrends1RF)
```

##random forest models 3

```{r}
View(asylumTrends3RF)
asylumTrends3RF <- data.frame(asylumTrends3)
asylumTrends3RF <- asylumTrends3RF[,-5:-10]
asylumTrends3RF <- asylumTrends3RF[,-1]
asylumTrends3RF
str(asylumTrends1RF)
ATRF3 <- randomForest(asylumTrends3RF[,-5], asylumTrends3RF[,5], prox=TRUE)
ATRF3
importance(ATRF3)
View(asylumTrends1RF)
```

install.packages("ggdendro")
library(readxl)
library(arules)
library(arulesViz)
library(datasets)
library(cluster)
library(mclust)
library(factoextra)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(rattle)
library(ggdendro)
## load the data

#### start IAN CODE ####
####                ####
####                ####

####K means, HAC, NB and DT for 1 - will follow up with other tables this week

which(is.na(asylumTrends1NoTotals)) ## Na's must be removed for Kmeans
asylumTrends1NoTotals_kmeans <- na.omit(asylumTrends1NoTotals) ## remove Na for Kmeans
which(is.na(asylumTrends1NoTotals_kmeans)) ## should be 0
str(asylumTrends1NoTotals_kmeans) ## prep to remove non-int columns for kmeans
asylumTrends1NoTotals_kmeans_label <- asylumTrends1NoTotals_kmeans[,1] ## store labels
asylumTrends1NoTotals_kmeans <- asylumTrends1NoTotals_kmeans[,-1] ## remove country names
asylumTrends1NoTotals_kmeans <- asylumTrends1NoTotals_kmeans[,-20:-23] ## remove factor columns

### Data for table 1 should be prepared for kmeans

###
fviz_nbclust(asylumTrends1NoTotals_kmeans, FUNcluster = kmeans) ## function suggests 2 clusters
asylumtrendsNT_KM_1  <- kmeans(asylumTrends1NoTotals_kmeans, 3) ## works to store into the K1 as list of 9, now need to get dist
head(asylumtrendsNT_KM_1)
asylumtrendsNT_KM_2  <- kmeans(asylumTrends1NoTotals_kmeans, 5) ## 5
head(asylumtrendsNT_KM_2)
asylumtrendsNT_KM_3  <- kmeans(asylumTrends1NoTotals_kmeans, 7) ## 7
head(asylumtrendsNT_KM_3)
asylumtrendsNT_KM_4  <- kmeans(asylumTrends1NoTotals_kmeans, 2) ## the "optimal" 2
head(asylumtrendsNT_KM_4)

### K means complete - creating tables to store the results and visualize

asylumTrends1NoTotals_clusts <- asylumTrends1NoTotals
which(is.na(asylumTrends1NoTotals_clusts))
asylumTrends1NoTotals_clusts <- asylumTrends1NoTotals_clusts[-22,] ## remove liechtenstein
asylumTrends1NoTotals_clusts$cluster2k <- asylumtrendsNT_KM_4$cluster ## assign cluster results to the table
asylumTrends1NoTotals_clusts[asylumTrends1NoTotals_clusts$cluster2k == "1", 1] ## cluster 1
asylumTrends1NoTotals_clusts[asylumTrends1NoTotals_clusts$cluster2k == "2", 1] ## cluster 2
asylumTrends1NoTotals_clusts$cluster3k <- asylumtrendsNT_KM_1$cluster ## assign cluster results to the table for 3k
asylumTrends1NoTotals_clusts$cluster5k <- asylumtrendsNT_KM_2$cluster ## assign cluster results to the table for 5k
asylumTrends1NoTotals_clusts$cluster7k <- asylumtrendsNT_KM_3$cluster ## assign cluster results to the table for 7k
asylumTrends1NoTotals_clusts[asylumTrends1NoTotals_clusts$cluster5k == "1", 1] ## cluster 1 for 5k
asylumTrends1NoTotals_clusts[asylumTrends1NoTotals_clusts$cluster5k == "2", 1] ## cluster 2
asylumTrends1NoTotals_clusts[asylumTrends1NoTotals_clusts$cluster5k == "3", 1] ## cluster 3
asylumTrends1NoTotals_clusts[asylumTrends1NoTotals_clusts$cluster5k == "4", 1] ## cluster 4
asylumTrends1NoTotals_clusts[asylumTrends1NoTotals_clusts$cluster5k == "5", 1] ## cluster 5

### Visualize the cluster with ggplot

ggplot(asylumTrends1NoTotals_clusts, aes(x = cluster2k)) + geom_bar() ## visualizing counts by which cluster 2k
ggplot(asylumTrends1NoTotals_clusts, aes(x = cluster3k)) + geom_bar() ## visualizing counts by which cluster 3k
ggplot(asylumTrends1NoTotals_clusts, aes(x = cluster5k)) + geom_bar() ## visualizing counts by which cluster 5k
ggplot(asylumTrends1NoTotals_clusts, aes(x = cluster7k)) + geom_bar() ## visualizing counts by which cluster 7k
#
ggplot(asylumTrends1NoTotals_clusts, aes(x = cluster2k, y = Country)) + geom_point() ## scatter assigning countries to specific clusters - this is 2 clusters
ggplot(asylumTrends1NoTotals_clusts, aes(x = cluster3k, y = Country)) + geom_point() ## 3 clusters
ggplot(asylumTrends1NoTotals_clusts, aes(x = cluster5k, y = Country)) + geom_point() ## 5 clusters
ggplot(asylumTrends1NoTotals_clusts, aes(x = cluster7k, y = Country)) + geom_point() ## 7 clusters

### Distance measurements using asylumtrendsNoTotals_kmeans results, need to find out how to label these by country.  
asylumTrends1NoTotals_kmeans$Country <- asylumTrends1NoTotals_kmeans_label$Country ## add the country names back to kmeans
rownames(asylumTrends1NoTotals_kmeans) <- asylumTrends1NoTotals_kmeans$Country
dist_manhattan <- get_dist(asylumTrends1NoTotals_kmeans, method = "manhattan") ## manhattan dist
fviz_dist(dist_manhattan, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle(
  label = "Manhattan Distance for Countries")## manhattan vis
dist_euc <- get_dist(asylumTrends1NoTotals_kmeans, method = "euclidean") ## euclidean dist
fviz_dist(dist_euc, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle(
  label = "Euclidean Distance for Countries")## euclidean vis

#dist_pear <- get_dist(asylumTrends1NoTotals_kmeans, method = "pearson") ## pearson dist
#fviz_dist(dist_pear, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle(
# label = "Pearson Distance for Countries")## pearson vis

### Same test but with removing refugee number and assessing the civil and political freedom, need to label

asylumTrends1NoTotals_kmeans_civpol <- asylumTrends1NoTotals_kmeans[, 20:28]
rownames(asylumTrends1NoTotals_kmeans_civpol) <- rownames(asylumTrends1NoTotals_kmeans)
dist_civpol_man <- get_dist(asylumTrends1NoTotals_kmeans_civpol, method = "manhattan")
fviz_dist(dist_civpol_man, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle(
  label = "Manhattan Distance for Civil / Political Freedom")  ## manhattan vis
dist_civpol_euc <- get_dist(asylumTrends1NoTotals_kmeans_civpol, method = "euclidean")
fviz_dist(dist_civpol_euc, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle(
  label = "Euclidean Distance for Civil / Political Freedom") ## euclidean vis
#dist_civpol_pear <- get_dist(asylumTrends1NoTotals_kmeans_civpol, method = "pearson")
#fviz_dist(dist_civpol_pear, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) ## pearson vis

### CONTINUE ON WITH SHEET 2 through 5 FOR KMEANS AND VIZ ###
###                                                       ###
###                                                       ###

which(is.na(asylumTrends2)) ## Na's must be removed for Kmeans
asylumTrends2_Kmeans <- na.omit(asylumTrends2) ## remove Na for Kmeans
which(is.na(asylumTrends2_Kmeans)) ## should be 0
str(asylumTrends2_Kmeans) ## prep to remove non-int columns for kmeans
asylumTrends2_Kmeans_label <- asylumTrends2_Kmeans[,1] ## store labels
asylumTrends2_Kmeans <- asylumTrends2_Kmeans[,-1] ## remove country names
asylumTrends2_Kmeans <- asylumTrends2_Kmeans[,-24:-27] ## remove factor columns
str(asylumTrends2_Kmeans)
### Data for table 2 should be prepared for kmeans
###
##fviz_nbclust(asylumTrends2_Kmeans, FUNcluster = kmeans) 
## not enough observations to suggest clusters?
asylumtrends2_KM_1  <- kmeans(asylumTrends2_Kmeans, 2) ## k = 2works to store into the K1 as list of 9, now need to get dist
head(asylumtrendsNT_KM_1)
asylumtrends2_KM_2   <- kmeans(asylumTrends2_Kmeans, 3) ## k = 3
head(asylumtrends2_KM_2)
asylumtrends2_KM_3  <- kmeans(asylumTrends2_Kmeans, 4) ## k = 4 for fun
head(asylumtrends2_KM_3)

### K means complete - creating tables to store the results and visualize

asylumtrends2_clusts <- asylumTrends2 ## create new df to adjust clusters
which(is.na(asylumtrends2_clusts)) ## check for NAs that might ruin data
##asylumTrends1NoTotals_clusts <- asylumTrends1NoTotals_clusts[-22,] ## remove liechtenstein
asylumtrends2_clusts$Cluster2k <- asylumtrends2_KM_1$cluster ## assign cluster results to the table
asylumtrends2_clusts$Cluster2k ## what are the clusters
asylumtrends2_clusts[asylumtrends2_clusts$Cluster2k == "1", 1 ] ## cluster 1
asylumtrends2_clusts[asylumtrends2_clusts$Cluster2k == "2", 1 ] ## cluster 2
asylumtrends2_clusts$Cluster3k <- asylumtrends2_KM_2$cluster ## assign cluster results to the table
asylumtrends2_clusts$Cluster4k <- asylumtrends2_KM_3$cluster ## assign cluster results to the table
asylumtrends2_clusts$Cluster3k ## what are the clusters
asylumtrends2_clusts$Cluster4k ## what are the clusters
asylumtrends2_clusts[asylumtrends2_clusts$Cluster3k == "1", 1 ] ## cluster 1 for 3k
asylumtrends2_clusts[asylumtrends2_clusts$Cluster3k == "2", 1 ] ## cluster 2 for 3k
asylumtrends2_clusts[asylumtrends2_clusts$Cluster3k == "3", 1 ] ## cluster 3 for 3k
asylumtrends2_clusts[asylumtrends2_clusts$Cluster4k == "1", 1 ] ## cluster 1 for 4k
asylumtrends2_clusts[asylumtrends2_clusts$Cluster4k == "2", 1 ] ## cluster 2 for 4k
asylumtrends2_clusts[asylumtrends2_clusts$Cluster4k == "3", 1 ] ## cluster 3 for 4k
asylumtrends2_clusts[asylumtrends2_clusts$Cluster4k == "4", 1 ] ## cluster 4 for 4k

## visualize the clusters
ggplot(asylumtrends2_clusts, aes(x = Cluster3k, y = Country)) + geom_point() ## 3 clusters
ggplot(asylumtrends2_clusts, aes(x = Cluster2k, y = Country)) + geom_point() ## 2 clusters. which is better?

## distance measurements
### Distance measurements using asylumtrends2_kmeans  
rownames(asylumTrends2_Kmeans) <- asylumTrends2$Country
view(asylumTrends2_Kmeans)
asylumtrends2_kmeans_man <- get_dist(asylumTrends2_Kmeans, method = "manhattan") ## manhattan dist
fviz_dist(asylumtrends2_kmeans_man, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle(
  label = "Manhattan Distance for Countries")## manhattan vis
asylumtrends2_kmeans_Euc <- get_dist(asylumTrends2_Kmeans, method = "euclidean") ## euclidean dist
fviz_dist(asylumtrends2_kmeans_Euc, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle(
  label = "Euclidean Distance for Countries")## euclidean vis

### Same test but with removing refugee number and assessing the civil and political freedom, need to label

asylumTrends2_Kmeans_civpol <- asylumTrends2_Kmeans[, 24:32]
view(asylumTrends2_Kmeans_civpol)
rownames(asylumTrends2_Kmeans_civpol) <- rownames(asylumTrends2_Kmeans)
dist_asylumtrends2_kmeans_civpol_man <- get_dist(asylumTrends2_Kmeans_civpol, method = "manhattan")
fviz_dist(dist_asylumtrends2_kmeans_civpol_man, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle(
  label = "Manhattan Distance for Civil / Political Freedom")  ## manhattan vis
dist_asylumtrends2_kmeans_civpol_euc <- get_dist(asylumTrends2_Kmeans_civpol, method = "euclidean")
fviz_dist(dist_asylumtrends2_kmeans_civpol_euc, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle(
  label = "Euclidean Distance for Civil / Political Freedom") ## euclidean vis

### CONTINUE ON WITH SHEET 3 through 5 FOR KMEANS AND VIZ ###
###                                                       ###
###                                                       ###
view(asylumTrends3)
which(is.na(asylumTrends3)) ## Na's must be removed for Kmeans
asylumTrends3_Kmeans <- na.omit(asylumTrends3) ## remove Na for Kmeans
which(is.na(asylumTrends2_Kmeans)) ## should be 0
str(asylumTrends3_Kmeans) ## prep to remove non-int columns for kmeans
asylumTrends3_Kmeans_label <- asylumTrends3_Kmeans[,1] ## store labels
asylumTrends3_Kmeans <- asylumTrends3_Kmeans[,-1] ## remove country names
asylumTrends3_Kmeans <- asylumTrends3_Kmeans[,-10:-11] ## remove factor columns
str(asylumTrends3_Kmeans)
### Data for table 3 should be prepared for kmeans

###

fviz_nbclust(asylumTrends3_Kmeans, FUNcluster = kmeans) ## suggest 2 optimal clusters
asylumtrends3_KM_1  <- kmeans(asylumTrends3_Kmeans, 2) ## k = 2
head(asylumtrends3_KM_1)
asylumtrends3_KM_2   <- kmeans(asylumTrends3_Kmeans, 3) ## k = 3
head(asylumtrends3_KM_2)
asylumtrends3_KM_3  <- kmeans(asylumTrends3_Kmeans, 5) ## k = 5 for fun
head(asylumtrends3_KM_3)

asylumtrends3_clusts <- asylumTrends3 ## create new df to adjust clusters
which(is.na(asylumtrends3_clusts)) ## check for NAs that might ruin data
##asylumTrends1NoTotals_clusts <- asylumTrends1NoTotals_clusts[-22,] ## remove liechtenstein
asylumtrends3_clusts$Cluster2k <- asylumtrends3_KM_1$cluster ## assign cluster results to the table
asylumtrends3_clusts$Cluster2k ## what are the clusters
asylumtrends3_clusts[asylumtrends3_clusts$Cluster2k == "1", 1 ] ## cluster 1
asylumtrends3_clusts[asylumtrends3_clusts$Cluster2k == "2", 1 ] ## cluster 2
asylumtrends3_clusts$Cluster3k <- asylumtrends3_KM_2$cluster ## assign cluster results to the table
asylumtrends3_clusts$Cluster3k ## what are the clusters
asylumtrends3_clusts[asylumtrends3_clusts$Cluster3k == "1", 1 ] ## cluster 1 for 3k
asylumtrends3_clusts[asylumtrends3_clusts$Cluster3k == "2", 1 ] ## cluster 2 for 3k
asylumtrends3_clusts[asylumtrends3_clusts$Cluster3k == "3", 1 ] ## cluster 3 for 3k
asylumtrends3_clusts$Cluster5k <- asylumtrends3_KM_3$cluster ## assign cluster results to the table
asylumtrends3_clusts$Cluster5k ## what are the clusters
asylumtrends3_clusts[asylumtrends3_clusts$Cluster5k == "1", 1 ] ## cluster 1 for 5k
asylumtrends3_clusts[asylumtrends3_clusts$Cluster5k == "2", 1 ] ## cluster 2 for 5k
asylumtrends3_clusts[asylumtrends3_clusts$Cluster5k == "3", 1 ] ## cluster 3 for 5k
asylumtrends3_clusts[asylumtrends3_clusts$Cluster5k == "4", 1 ] ## cluster 4 for 5k
asylumtrends3_clusts[asylumtrends3_clusts$Cluster5k == "5", 1 ] ## cluster 4 for 5k

## visualize the clusters
ggplot(asylumtrends3_clusts, aes(x = Cluster2k, y = Country)) + geom_point() ## 2 clusters
ggplot(asylumtrends3_clusts, aes(x = Cluster3k, y = Country)) + geom_point() ## 3 clusters
ggplot(asylumtrends3_clusts, aes(x = Cluster5k, y = Country)) + geom_point() ## 5 clusters

## distance measurements
### Distance measurements using asylumtrends2_kmeans  
rownames(asylumTrends3_Kmeans) <- asylumTrends3$Country
view(asylumTrends3_Kmeans)
asylumtrends3_kmeans_man <- get_dist(asylumTrends3_Kmeans, method = "manhattan") ## manhattan dist
fviz_dist(asylumtrends3_kmeans_man, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle(
  label = "Manhattan Distance for Countries")## manhattan vis
asylumtrends3_kmeans_Euc <- get_dist(asylumTrends3_Kmeans, method = "euclidean") ## euclidean dist

### Same test but with removing refugee number and assessing the civil and political freedom

asylumTrends3_Kmeans_civpol <- asylumTrends3_Kmeans[, 10:20]
view(asylumTrends3_Kmeans_civpol)
rownames(asylumTrends3_Kmeans_civpol) <- rownames(asylumTrends3_Kmeans)
dist_asylumtrends3_kmeans_civpol_man <- get_dist(asylumTrends3_Kmeans_civpol, method = "manhattan")
fviz_dist(dist_asylumtrends3_kmeans_civpol_man, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle(
  label = "Manhattan Distance for Civil / Political Freedom")  ## manhattan vis
dist_asylumtrends3_kmeans_civpol_euc <- get_dist(asylumTrends3_Kmeans_civpol, method = "euclidean")
fviz_dist(dist_asylumtrends3_kmeans_civpol_euc, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle(
  label = "Euclidean Distance for Civil / Political Freedom") ## euclidean vis

### CONTINUE ON WITH SHEET 4 through 5 FOR KMEANS AND VIZ ###
###                                                       ###
###                                                       ###


## cleaning
view(asylumTrends4)
asylumTrends4 <- asylumTrends4[-1:-5,] ## remove header rows
asylumTrends4 <- asylumTrends4 [-41:-42,] ## remove tail rows
asylumTrends4 <- asylumTrends4 [-11,] ## remove stateless
colnames(asylumTrends4) <- c("Origin","2013", "2014","Total","Annual Change",
                             "Share2013","Share2014","Total", "Rank2013", "Rank2014","Region", "Free",
                             "Political Rights", "Civil Liberties", "Electoral","PolPluralism", "Governance",
                             "PolRights", "Expression", "Organization","Rule of Law", "IndRights", "Liberties Score")
str(asylumTrends4)
asylumTrends4[,2:8] <- lapply(asylumTrends4[,2:8], FUN = as.numeric) ## change numeric
asylumTrends4 <- asylumTrends4[,-9:-10]
asylumTrends4$Origin <- as.factor(asylumTrends4$Origin)
asylumTrends4[,9:10] <- lappl
y((asylumTrends4[,9:10]), FUN = as.factor)
## prepare for Kmeans
which(is.na(asylumTrends4)) ## Na's must be removed for Kmeans
asylumTrends4[331,] ## weird?
asylumTrends4 <- na.omit(asylumTrends4) ## remove NAs
which(is.na(asylumTrends4)) ## NAs removed.  
asylumTrends4_Kmeans <- asylumTrends4 ## create kmeans df
str(asylumTrends4_Kmeans) ## prep to remove non-int columns for kmeans
## there are still non numeric columns
asylumTrends4_Kmeans[,11:21] <- lapply(asylumTrends4_Kmeans[,11:21], FUN = as.numeric) ## as numeric for remaining
asylumTrends4[,11:21] <- lapply(asylumTrends4[,11:21], FUN = as.numeric) ## as numeric for remaining
str(asylumTrends4_Kmeans) ## yes
str(asylumTrends4) ## yes
asylumTrends4_Kmeans_label <- asylumTrends4_Kmeans$Origin ## store labels
asylumTrends4_Kmeans <- asylumTrends4_Kmeans[,-1] ## remove country names
asylumTrends4_Kmeans <- asylumTrends4_Kmeans[,-8:-9] ## remove factor columns
str(asylumTrends4_Kmeans)
### Data for table 4 should be prepared for kmeans

###

fviz_nbclust(asylumTrends4_Kmeans, FUNcluster = kmeans) ## suggest 2 optimal clusters
asylumtrends4_KM_1  <- kmeans(asylumTrends4_Kmeans, 2) ## k = 2
head(asylumtrends4_KM_1)
asylumtrends4_KM_2   <- kmeans(asylumTrends4_Kmeans, 3) ## k = 3
head(asylumtrends4_KM_2)
asylumtrends4_KM_3  <- kmeans(asylumTrends4_Kmeans, 5) ## k = 5 for fun
head(asylumtrends4_KM_3)

asylumtrends4_clusts <- asylumTrends4 ## create new df to adjust clusters
which(is.na(asylumtrends4_clusts)) ## check for NAs that might ruin data
##asylumTrends1NoTotals_clusts <- asylumTrends1NoTotals_clusts[-22,] ## remove liechtenstein
asylumtrends4_clusts$Cluster2k <- asylumtrends4_KM_1$cluster ## assign cluster results to the table
asylumtrends4_clusts$Cluster2k ## what are the clusters
asylumtrends4_clusts[asylumtrends4_clusts$Cluster2k == "1", 1 ] ## cluster 1
asylumtrends4_clusts[asylumtrends4_clusts$Cluster2k == "2", 1 ] ## cluster 2
asylumtrends4_clusts$Cluster3k <- asylumtrends4_KM_2$cluster ## assign cluster results to the table
asylumtrends4_clusts$Cluster3k ## what are the clusters
asylumtrends4_clusts[asylumtrends4_clusts$Cluster3k == "1", 1 ] ## cluster 1 for 3k
asylumtrends4_clusts[asylumtrends4_clusts$Cluster3k == "2", 1 ] ## cluster 2 for 3k
asylumtrends4_clusts[asylumtrends4_clusts$Cluster3k == "3", 1 ] ## cluster 3 for 3k
asylumtrends4_clusts$Cluster5k <- asylumtrends4_KM_3$cluster ## assign cluster results to the table
asylumtrends4_clusts$Cluster5k ## what are the clusters
asylumtrends4_clusts[asylumtrends4_clusts$Cluster5k == "1", 1 ] ## cluster 1 for 5k
asylumtrends4_clusts[asylumtrends4_clusts$Cluster5k == "2", 1 ] ## cluster 2 for 5k
asylumtrends4_clusts[asylumtrends4_clusts$Cluster5k == "3", 1 ] ## cluster 3 for 5k
asylumtrends4_clusts[asylumtrends4_clusts$Cluster5k == "4", 1 ] ## cluster 4 for 5k
asylumtrends4_clusts[asylumtrends4_clusts$Cluster5k == "5", 1 ] ## cluster 4 for 5k

## visualize the clusters
## error with duplicate columns
colnames(asylumtrends4_clusts) <- make.unique(names(asylumtrends4_clusts)) ## makes column names unique
ggplot(asylumtrends4_clusts, aes(x = Cluster2k, y = Origin)) + geom_point() ## 2 clusters
ggplot(asylumtrends4_clusts, aes(x = Cluster3k, y = Origin)) + geom_point() ## 3 clusters
ggplot(asylumtrends4_clusts, aes(x = Cluster5k, y = Origin)) + geom_point() ## 5 clusters

## distance measurements
### Distance measurements using asylumtrends2_kmeans  
rownames(asylumTrends4_Kmeans) <- asylumTrends4$Origin  
view(asylumTrends4_Kmeans)
asylumtrends4_kmeans_man <- get_dist(asylumTrends4_Kmeans, method = "manhattan") ## manhattan dist
fviz_dist(asylumtrends4_kmeans_man, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle(
  label = "Manhattan Distance for Refugee Origin Countries")## manhattan vis
asylumtrends4_kmeans_Euc <- get_dist(asylumTrends4_Kmeans, method = "euclidean") ## euclidean dist
fviz_dist(asylumtrends4_kmeans_Euc, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle(
  label = "Euclidean Distance for Refugee Origin Countries")## Euclidean vis

### Same test but with removing refugee number and assessing the civil and political freedom

asylumTrends4_Kmeans_civpol <- asylumTrends4_Kmeans[, 8:18]
view(asylumTrends4_Kmeans_civpol)
rownames(asylumTrends4_Kmeans_civpol) <- rownames(asylumTrends4_Kmeans)
dist_asylumtrends4_kmeans_civpol_man <- get_dist(asylumTrends4_Kmeans_civpol, method = "manhattan")
fviz_dist(dist_asylumtrends4_kmeans_civpol_man, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle(
  label = "Manhattan Distance for Civil / Political Freedom in Origin Countries")  ## manhattan vis
dist_asylumtrends4_kmeans_civpol_euc <- get_dist(asylumTrends4_Kmeans_civpol, method = "euclidean")
fviz_dist(dist_asylumtrends4_kmeans_civpol_euc, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle(
  label = "Euclidean Distance for Civil / Political Freedom in Origin Countries") ## euclidean vis

### CONTINUE ON WITH SHEET 5 FOR KMEANS AND VIZ ###
###                                                       ###
###                                                       ###


## cleaning
view(asylumTrends5)
asylumTrends5 <- asylumTrends5[-1:-5,] ## remove header rows
asylumTrends5 <- asylumTrends5 [-41:-42,] ## remove tail rows
asylumTrends5 <- asylumTrends5 [-10,] ## remove stateless
colnames(asylumTrends5) <- c("Origin","2013", "2014","Total","Annual Change",
                             "Share2013","Share2014","Total", "Rank2013", "Rank2014","Region", "Free",
                             "Political Rights", "Civil Liberties", "Electoral","PolPluralism", "Governance",
                             "PolRights", "Expression", "Organization","Rule of Law", "IndRights", "Liberties Score")
str(asylumTrends5)
asylumTrends5[,2:8] <- lapply(asylumTrends5[,2:8], FUN = as.numeric) ## change numeric
asylumTrends5 <- asylumTrends5[,-9:-10] ## remove rank
asylumTrends5$Origin <- as.factor(asylumTrends5$Origin)
asylumTrends5[,9:10] <- lapply((asylumTrends5[,9:10]), FUN = as.factor)
## prepare for Kmeans
which(is.na(asylumTrends5)) ## Na's must be removed for Kmeans
asylumTrends5[330,] ## weird?
asylumTrends5 <- na.omit(asylumTrends5) ## remove NAs
which(is.na(asylumTrends5)) ## NAs removed.  
asylumTrends5_Kmeans <- asylumTrends5 ## create kmeans df
str(asylumTrends5_Kmeans) ## prep to remove non-int columns for kmeans
## there are still non numeric columns
asylumTrends5_Kmeans[,11:21] <- lapply(asylumTrends5_Kmeans[,11:21], FUN = as.numeric) ## as numeric for remaining
asylumTrends5[,11:21] <- lapply(asylumTrends5[,11:21], FUN = as.numeric) ## as numeric for remaining
str(asylumTrends5_Kmeans) ## yes
str(asylumTrends5) ## yes
asylumTrends5_Kmeans_label <- asylumTrends5_Kmeans$Origin ## store labels
asylumTrends5_Kmeans <- asylumTrends5_Kmeans[,-1] ## remove country names
asylumTrends5_Kmeans <- asylumTrends5_Kmeans[,-8:-9] ## remove factor columns
str(asylumTrends5_Kmeans)
### Data for table 5 should be prepared for kmeans

###

fviz_nbclust(asylumTrends5_Kmeans, FUNcluster = kmeans) ## suggest 2 optimal clusters
asylumtrends5_KM_1  <- kmeans(asylumTrends5_Kmeans, 2) ## k = 2
head(asylumtrends5_KM_1)
asylumtrends5_KM_2   <- kmeans(asylumTrends5_Kmeans, 3) ## k = 3
head(asylumtrends5_KM_2)
asylumtrends5_KM_3  <- kmeans(asylumTrends5_Kmeans, 5) ## k = 5 for fun
head(asylumtrends5_KM_3)

asylumtrends5_clusts <- asylumTrends5 ## create new df to adjust clusters
which(is.na(asylumtrends5_clusts)) ## check for NAs that might ruin data
##asylumTrends1NoTotals_clusts <- asylumTrends1NoTotals_clusts[-22,] ## remove liechtenstein
asylumtrends5_clusts$Cluster2k <- asylumtrends5_KM_1$cluster ## assign cluster results to the table
asylumtrends5_clusts$Cluster2k ## what are the clusters
asylumtrends5_clusts[asylumtrends5_clusts$Cluster2k == "1", 1 ] ## cluster 1
asylumtrends5_clusts[asylumtrends5_clusts$Cluster2k == "2", 1 ] ## cluster 2
asylumtrends5_clusts$Cluster3k <- asylumtrends5_KM_2$cluster ## assign cluster results to the table
asylumtrends5_clusts$Cluster3k ## what are the clusters
asylumtrends5_clusts[asylumtrends5_clusts$Cluster3k == "1", 1 ] ## cluster 1 for 3k
asylumtrends5_clusts[asylumtrends5_clusts$Cluster3k == "2", 1 ] ## cluster 2 for 3k
asylumtrends5_clusts[asylumtrends5_clusts$Cluster3k == "3", 1 ] ## cluster 3 for 3k
asylumtrends5_clusts$Cluster5k <- asylumtrends5_KM_3$cluster ## assign cluster results to the table
asylumtrends5_clusts$Cluster5k ## what are the clusters
asylumtrends5_clusts[asylumtrends5_clusts$Cluster5k == "1", 1 ] ## cluster 1 for 5k
asylumtrends5_clusts[asylumtrends5_clusts$Cluster5k == "2", 1 ] ## cluster 2 for 5k
asylumtrends5_clusts[asylumtrends5_clusts$Cluster5k == "3", 1 ] ## cluster 3 for 5k
asylumtrends5_clusts[asylumtrends5_clusts$Cluster5k == "4", 1 ] ## cluster 4 for 5k
asylumtrends5_clusts[asylumtrends5_clusts$Cluster5k == "5", 1 ] ## cluster 4 for 5k

## visualize the clusters
## error with duplicate columns
colnames(asylumtrends5_clusts) <- make.unique(names(asylumtrends5_clusts)) ## makes column names unique
ggplot(asylumtrends5_clusts, aes(x = Cluster2k, y = Origin)) + geom_point() ## 2 clusters
ggplot(asylumtrends5_clusts, aes(x = Cluster3k, y = Origin)) + geom_point() ## 3 clusters
ggplot(asylumtrends5_clusts, aes(x = Cluster5k, y = Origin)) + geom_point() ## 5 clusters

## distance measurements
### Distance measurements using asylumtrends2_kmeans  
rownames(asylumTrends5_Kmeans) <- asylumTrends5$Origin  
view(asylumTrends5_Kmeans)
asylumtrends5_kmeans_man <- get_dist(asylumTrends5_Kmeans, method = "manhattan") ## manhattan dist
fviz_dist(asylumtrends5_kmeans_man, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle(
  label = "Manhattan Distance for Refugee Origin Countries")## manhattan vis
asylumtrends5_kmeans_Euc <- get_dist(asylumTrends5_Kmeans, method = "euclidean") ## euclidean dist
fviz_dist(asylumtrends5_kmeans_Euc, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle(
  label = "Euclidean Distance for Refugee Origin Countries")## Euclidean vis

### Same test but with removing refugee number and assessing the civil and political freedom

asylumTrends5_Kmeans_civpol <- asylumTrends5_Kmeans[, 8:18]
view(asylumTrends5_Kmeans_civpol)
rownames(asylumTrends5_Kmeans_civpol) <- rownames(asylumTrends5_Kmeans)
dist_asylumtrends5_kmeans_civpol_man <- get_dist(asylumTrends5_Kmeans_civpol, method = "manhattan")
fviz_dist(dist_asylumtrends5_kmeans_civpol_man, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle(
  label = "Manhattan Distance for Civil / Political Freedom in Origin Countries")  ## manhattan vis
dist_asylumtrends5_kmeans_civpol_euc <- get_dist(asylumTrends5_Kmeans_civpol, method = "euclidean")
fviz_dist(dist_asylumtrends5_kmeans_civpol_euc, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle(
  label = "Euclidean Distance for Civil / Political Freedom in Origin Countries") ## euclidean vis

###
### K means complete, HAC BELOW.
###

### HAC FOR Asylum Trends 1
view(asylumTrends1NoTotals_kmeans_label)
rownames(asylumTrends1NoTotals_clusts) <- asylumTrends1NoTotals_kmeans_label$Country
AT1_dist <- dist(asylumTrends1NoTotals_clusts, method = "euclidean") ## convert to dissimiliarity object
AT1_HAC <- hclust(AT1_dist, method = "complete") ## assign
ggdendrogram(AT1_HAC, rotate = TRUE, cex = 5) + ggtitle(label = "HAC of Refugee Destination Countries") 

### freedom index HAC for asylum trends 1

AT1_dist_civpol <- dist(asylumTrends1NoTotals_kmeans_civpol, method = "euclidean") ## convert to dissimiliarity object
AT1_HAC_civpol <- hclust(AT1_dist_civpol, method = "complete") ## assign
ggdendrogram(AT1_HAC_civpol, rotate = TRUE, cex = 5) + ggtitle(label = "HAC of Refugee Destination Countries w/ Freedom Index Only ")  ##plot

### HAC FOR Asylum Trends 2
view(asylumTrends2_Kmeans_label)
rownames(asylumtrends2_clusts) <- asylumTrends2_Kmeans_label$Country
AT2_dist <- dist(asylumtrends2_clusts, method = "euclidean") ## convert to dissimiliarity object
AT2_HAC <- hclust(AT2_dist, method = "complete") ## assign
ggdendrogram(AT2_HAC, rotate = TRUE, cex = 5) + ggtitle(label = "HAC of Refugee Destination Countries") 

### freedom index HAC for asylum trends 2

AT2_dist_civpol <- dist(asylumTrends2_Kmeans_civpol, method = "euclidean") ## convert to dissimiliarity object
AT2_HAC_civpol <- hclust(AT2_dist_civpol, method = "complete") ## assign
ggdendrogram(AT2_HAC_civpol, rotate = TRUE, cex = 5) + ggtitle(label = "HAC of Refugee Destination Countries w/ Freedom Index Only")  ##plot

### HAC FOR Asylum Trends 3
view(asylumTrends3_Kmeans_label)
rownames(asylumtrends3_clusts) <- asylumTrends3_Kmeans_label$Country
AT3_dist <- dist(asylumtrends3_clusts, method = "euclidean") ## convert to dissimiliarity object
AT3_HAC <- hclust(AT3_dist, method = "complete") ## assign
ggdendrogram(AT3_HAC, rotate = TRUE, cex = 5) + ggtitle(label = "HAC of Refugee Origination Countries") 

### freedom index HAC for asylum trends 3

AT3_dist_civpol <- dist(asylumTrends3_Kmeans_civpol, method = "euclidean") ## convert to dissimiliarity object
AT3_HAC_civpol <- hclust(AT3_dist_civpol, method = "complete") ## assign
ggdendrogram(AT3_HAC_civpol, rotate = TRUE, cex = 5) + ggtitle(label = "HAC of Refugee Origination Countries w/ Freedom Index Only")  ##plot

### HAC FOR Asylum Trends 4
view(asylumTrends4_Kmeans_label)
rownames(asylumtrends4_clusts) <- asylumTrends4_Kmeans_label
AT4_dist <- dist(asylumtrends4_clusts, method = "euclidean") ## convert to dissimiliarity object
AT4_HAC <- hclust(AT4_dist, method = "complete") ## assign
ggdendrogram(AT4_HAC, rotate = TRUE, cex = 5) + ggtitle(label = "HAC of Refugee Origination Countries") 

### freedom index HAC for asylum trends 4

AT4_dist_civpol <- dist(asylumTrends4_Kmeans_civpol, method = "euclidean") ## convert to dissimiliarity object
AT4_HAC_civpol <- hclust(AT4_dist_civpol, method = "complete") ## assign
ggdendrogram(AT4_HAC_civpol, rotate = TRUE, cex = 5) + ggtitle(label = "HAC of Refugee Origination Countries w/ Freedom Index Only")  ##plot

### HAC FOR Asylum Trends 5
view(asylumTrends5_Kmeans_label)
rownames(asylumtrends5_clusts) <- asylumTrends5_Kmeans_label
AT5_dist <- dist(asylumtrends5_clusts, method = "euclidean") ## convert to dissimiliarity object
AT5_HAC <- hclust(AT5_dist, method = "complete") ## assign
ggdendrogram(AT5_HAC, rotate = TRUE, cex = 5) + ggtitle(label = "HAC of Refugee Origination Countries") 

### freedom index HAC for asylum trends 5

AT5_dist_civpol <- dist(asylumTrends5_Kmeans_civpol, method = "euclidean") ## convert to dissimiliarity object
AT5_HAC_civpol <- hclust(AT5_dist_civpol, method = "complete") ## assign
ggdendrogram(AT5_HAC_civpol, rotate = TRUE, cex = 5) + ggtitle(label = "HAC of Refugee Origination Countries w/ Freedom Index Only")  ##plot

### HAC COMPELTE, DECISION TREES

## Plan is to predict "free" dependent on all other values.  Using function from HW8
#N is number of rows in the dataset
#kfolds is number of folds we want for dt model
#holdout is split train/test, 
#all labels and all results are where we are storing the results for the confusion matrix
## develop a function for model evaluation

get_accuracy_rate <- function(results_table, total_cases) { 
  diagonal_sum <- sum(c(results_table[[1]], results_table[[5]],
                        results_table[[9]]))
  (diagonal_sum / total_cases)*100 
}



AT1_N <- nrow(asylumTrends1NoTotals)
AT1_kfolds <- 4
AT1_holdout <- split(sample(1:N), 1:AT1_kfolds)
AT1_AllResults_dt <- list()
AT1_AllLabels_dt <- list()
# The Process
for (k in 1:AT1_kfolds) { 
  AT1_DT_Test <- asylumTrends1NoTotals[AT1_holdout[[k]],]
  AT1_DT_Train <- asylumTrends1NoTotals[-AT1_holdout[[k]],]
  #
  AT1_DT_Test_NLabel <- AT1_DT_Test[-c(22)] ## remove freedom classifier
  AT1_DT_Test_JLabel <- AT1_DT_Test[c(22)] ## isolate the freedom classifier to run matrix later
  AT1_train_DT <- rpart(AT1_DT_Train$Free~., data = AT1_DT_Train, method="class", control=rpart.control(cp = 0, minsplit = 2)) # the model to train
  AT1_DT_pred <- predict(AT1_train_DT, AT1_DT_Test_NLabel, type = "class")
  (confusionMatrix(AT1_DT_pred, AT1_DT_Test$Free)) #the confusion matrix.  
  AT1_AllResults_dt<- c(AT1_AllResults_dt, AT1_DT_pred)
  AT1_AllLabels_dt <- c(AT1_AllLabels_dt, AT1_DT_Test_JLabel)
  plot(AT1_DT_pred, ylab = "Density", main = "Decision Tree Plot") 
}
## table and confusion matrix
(table(unlist(AT1_AllResults_dt),unlist(AT1_AllLabels_dt)))  
get_accuracy_rate(table(unlist(AT1_AllResults_dt), unlist(AT1_AllLabels_dt)), length(unlist(AT1_AllLabels_dt))) ## 95% 
## plot the DT model
fancyRpartPlot(AT1_train_DT) ## kind of weird labeling??? 


### asylumtrends 2



## asylum trends 2 decision tree //// we need to remove the rownames
view(asylumTrends2)
AT2_N <- nrow(asylumTrends2)
AT2_kfolds <- 2
AT2_holdout <- split(sample(1:AT2_N), 1:AT2_kfolds)
AT2_AllResults_dt <- list()
AT2_AllLabels_dt <- list()
## error with eval /// remove rownames
##rownames(asylumtrends2) <- c()
view(asylumtrends2)
## remove the country names from the dataset, because it makes the plot dumb
asylumtrends2 <- asylumtrends2[,-1]
view(asylumtrends2)

## The Process
#for (k in 1:AT2_kfolds) { 
AT2_DT_Test <- asylumtrends2[AT2_holdout[[k]],]
AT2_DT_Train <- asylumtrends2[-AT2_holdout[[k]],]
#
AT2_DT_Test_NLabel <- AT2_DT_Test[-c(21)] ## remove freedom classifier MATCH TARGET COLUMN
AT2_DT_Test_JLabel <- AT2_DT_Test[c(21)] ## isolate the freedom classifier to run matrix later
AT2_train_DT <- rpart(AT2_DT_Train$Free~., data = AT2_DT_Train, method="class", control=rpart.control(cp = 0, minsplit = 2)) # the model to train
AT2_DT_pred <- predict(AT2_train_DT, AT2_DT_Test_NLabel, type = "class")
(confusionMatrix(AT2_DT_pred, AT2_DT_Test$Free)) #the confusion matrix.  
AT2_AllResults_dt<- c(AT2_AllResults_dt, AT2_DT_pred)
AT2_AllLabels_dt <- c(AT2_AllLabels_dt, AT2_DT_Test_JLabel)
plot(AT2_DT_pred, ylab = "Density", main = "Decision Tree Plot") 
#}

## table and confusion matrix
(table(unlist(AT2_AllResults_dt),unlist(AT2_AllLabels_dt)))  
get_accuracy_rate(table(unlist(AT2_AllResults_dt), unlist(AT2_AllLabels_dt)), length(unlist(AT2_AllLabels_dt))) ## 5/7 
## plot the DT model
fancyRpartPlot(AT2_train_DT) ## kind of weird labeling??? 

###
### Asylumtrends3
###

view(asylumTrends3) 
asylumtrends3a <- asylumTrends3[,-1]
AT3_N <- nrow(asylumTrends3a)
AT3_kfolds <- 2
AT3_holdout <- split(sample(1:AT3_N), 1:AT3_kfolds)
AT3_AllResults_dt <- list()
AT3_AllLabels_dt <- list()
view(asylumtrends3a)
# The Process
for (k in 1:AT3_kfolds) { 
  AT3_DT_Test <- asylumtrends3a[AT3_holdout[[k]],]
  AT3_DT_Train <- asylumtrends3a[-AT3_holdout[[k]],]
  #
  AT3_DT_Test_NLabel <- AT3_DT_Test[-c(11)] ## remove freedom classifier
  AT3_DT_Test_JLabel <- AT3_DT_Test[c(11)] ## isolate the freedom classifier to run matrix later
  AT3_train_DT <- rpart(AT3_DT_Train$Free~., data = AT3_DT_Train, method="class", control=rpart.control(cp = 0, minsplit = 2)) # the model to train
  AT3_DT_pred <- predict(AT3_train_DT, AT3_DT_Test_NLabel, type = "class")
  (confusionMatrix(AT3_DT_pred, AT3_DT_Test$Free)) #the confusion matrix.  
  AT3_AllResults_dt<- c(AT3_AllResults_dt, AT3_DT_pred)
  AT3_AllLabels_dt <- c(AT3_AllLabels_dt, AT3_DT_Test_JLabel)
  plot(AT3_DT_pred, ylab = "Density", main = "Decision Tree Plot") 
}
## table and confusion matrix
(table(unlist(AT3_AllResults_dt),unlist(AT3_AllLabels_dt)))  
get_accuracy_rate(table(unlist(AT3_AllResults_dt), unlist(AT3_AllLabels_dt)), length(unlist(AT3_AllLabels_dt))) ##28/36
## plot the DT model
fancyRpartPlot(AT3_train_DT) ## kind of weird labeling??? 

