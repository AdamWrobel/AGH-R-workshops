rm(list=ls())

#optional - install packages
# install.packages("dplyr")
# install.packages("ROCR")

#load libraries
library(dplyr)
library(ROCR)

#set working directory - please select the folder where you saved script and data files
setwd("C:/UBS/Dev/Students/Working folder/")

############## DATA CHECK #####################

# C1: read the data
Data <- read.delim("DataPD.txt", stringsAsFactors = F)
CriteriaSummary <- read.delim("Description.txt", stringsAsFactors = F)

# C2: set useful variable
VarNames <- CriteriaSummary[, "Criteria"]

# C3 look at the data description - criteria have economic interpretation, grouped into four categories
CriteriaSummary

# C4 the model developer has submitted following model to validation:

Regress <- glm(formula= "deflag ~ var5_LF + var14_LF + var2_AQ + var11_AQ + var8_PE + var18_CL + var4_CL",na.action=na.exclude,family=binomial("probit"), data = Data)


#questions:
# 1. What can we say about the coefficents? What is the model deviance, null deviance?

summary(Regress)

# 2. How to assess the model performance? ROC curve/AUC. Why AIC or analysis of deviance may be not appropriate?

FittedScores <- predict.glm(Regress)
Pred <- prediction(FittedScores,Data$deflag)
AUC <- as.numeric(performance(Pred,"auc")@y.values)
plot(performance(Pred, "tpr", "fpr"))
lines(seq(0,1,by = 0.01), seq(0,1,by = 0.01), col = "red")

#compare the default rate with estimated average PD
# that's task for you


# 3. What is the size of estimation sample? The default coverage?
CriteriaCodes <- c("var5_LF" , "var14_LF", "var2_AQ" , "var11_AQ" , "var8_PE" , "var18_CL",  "var4_CL")
DataCoverageBigModel <-  nrow(na.omit(Data[,CriteriaCodes]))/nrow(Data) 
DefaultCoverageBigModel <-  sum(Data[, "deflag"] *  !is.na(rowSums(Data[, CriteriaCodes]) )) / sum(Data[, "deflag"])

#any ideas how to fill data gaps in production?
#small hint:

CorrelationMatrix <- cor(Data[, VarNames], use = "pairwise.complete.obs" )


# 4. Does estimation data contain strange entires? Did the model owner address their existance for estimation?
View(Data)
# or
head(Data)

#check data histograms
par(mfrow = c(3,4))
for(i in 1:length(VarNames)){
  Data[, VarNames[i]] %>% density(na.rm = T) %>% plot(main = VarNames[i])
}

#data quantiles
QuantileSummary <- sapply(VarNames, function(x) quantile(Data[, x], seq(0, 1, by = 0.1), na.rm = T))
# what do we see?

#more transparent histograms
par(mfrow = c(3,4))
for(i in 1:length(VarNames)){
  LowerBound <- quantile(Data[, VarNames[i]], 0.05, na.rm = T)
  UpperBound <- quantile(Data[, VarNames[i]], 0.95, na.rm = T)
  Data[Data[, VarNames[i]] >= LowerBound & Data[, VarNames[i]] <= UpperBound , VarNames[i]] %>% density(na.rm = T) %>% plot(main = VarNames[i])
}

# 5. How does the model behave in response to margin observations?
pnorm(max(FittedScores, na.rm = T))
pnorm(min(FittedScores, na.rm = T))

# 6. any suggestions for other variable selection? hint: univariate analysis


AUCsummary <- sapply(VarNames, function(x) as.numeric(performance(prediction(Data[,x],Data[,"deflag"]),"auc")@y.values))

# TASK: use command sapply to summarize data coverage (proportion of complete observations to all observations)

DataCoverage <- 

#what is the default coverage for particular varialbes?

DefaultCoverage <- sapply(VarNames, function(x)  sum(Data[, "deflag"] * !is.na(Data[, x])) / sum(Data[, "deflag"]) )

#let's gather everything in one data frame

VariablesSummary <- data.frame( VarNames, AUCsummary, DataCoverage, DefaultCoverage)


# 
# #different approach to Regression coding
# CriteriaCodes <- VariablesSummary[1:3, "VarNames"]
# Formula <- as.formula(paste("deflag ~ ", gsub(", "," + ",toString(CriteriaCodes))))# The regression formula
# Regress <- glm(formula=Formula,na.action=na.exclude,family=binomial("probit"), data = Data)
# Regress$coefficients
# summary(Regress)


