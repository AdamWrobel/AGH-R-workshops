## Valuation of a property - multivariate linear model ##

adress <- 'https://raw.githubusercontent.com/AdamWrobel/AGH-R-workshops/master/4_data_processing_and_modelling/house_prices.csv'
house_prices <- read.csv(adress, stringsAsFactors = F) %>% data.frame

# description for all the variable could be found at:
# https://raw.githubusercontent.com/AdamWrobel/AGH-R-workshops/master/4_data_processing_and_modelling/data_description.txt
# or https://www.kaggle.com/c/house-prices-advanced-regression-techniques

# first look at the data set
str(house_prices)
head(house_prices,2)
summary(house_prices)

# Exercise 2

# initial data processing

# variables with mostly NAs
logicalNAs <- is.na(house_prices) 
countedNAs <- apply(logicalNAs,2,sum)
countedNAs[countedNAs >= 100]
variables_to_keep <- countedNAs[countedNAs < 100] %>% names

house_prices2 <- house_prices %>% select(variables_to_keep)


# drop variables with same value for almost all observations
categorigal_variables <- apply(house_prices2, 2, is.character)


# select 
house_prices_small <- house_prices %>% 
  select(-PoolQC,-Fence,-MiscFeature,-FireplaceQu,-Alley) %>% # drop variables with mostly NAs
  select(-Exterior2nd, -Exterior1st, -Neighborhood) %>% # drop with many levels
  select(-RoofMatl,-Condition2,-Condition1,-LandSlope,-Utilities,-Street,-BsmtCond,-Heating,-Electrical,-Functional,-GarageQual,-GarageCond,-PavedDrive) %>% # drop not informative
  na.omit  # drop observation with NAs

fctr <- house_prices_small[sapply(house_prices_small, is.character)]
sapply(fctr, table)


house_prices_small <- house_prices %>% 
  select(SalePrice,OverallQual,GrLivArea, GarageCars, TotalBsmtSF, FullBath, TotRmsAbvGrd)


initial_model <- lm(SalePrice~.,house_prices_small)
summary(initial_model)

model_stepwise <- step(initial_model)
summary(model_stepwise)

initial_model <- lm(SalePrice~OverallQual+GrLivArea+GarageCars+TotalBsmtSF+FullBath+TotRmsAbvGrd,house_prices)
summary(initial_model)
step(initial_model, direction = 'both')

initial_model

MSSubClass

house_prices %>% lapply(table)
