## Valuation of a property - multivariate linear model ##
library(dplyr)
library(ggplot2)
options(scipen = 10)

adress <- 'https://raw.githubusercontent.com/AdamWrobel/AGH-R-workshops/master/4_data_processing_and_modelling/house_prices.csv'
house_prices <- read.csv(adress, stringsAsFactors = F) %>% data.frame

# description for all the variable could be found at:
# https://raw.githubusercontent.com/AdamWrobel/AGH-R-workshops/master/4_data_processing_and_modelling/data_description.txt
# or https://www.kaggle.com/c/house-prices-advanced-regression-techniques

# first look at the data set
str(house_prices)
head(house_prices,2)
summary(house_prices)



## initial data processing ##

# variables with mostly NAs
logicalNAs <- is.na(house_prices) 
countedNAs <- apply(logicalNAs,2,sum)
countedNAs[countedNAs >= 100]
variables_to_keep <- countedNAs[countedNAs < 100] %>% names

house_prices2 <- house_prices %>% select(variables_to_keep)


# drop variables with same value for almost all observations
categorigal_variables <- sapply(house_prices2, is.character)
subset_with_cat_var <- house_prices2[,categorigal_variables]

subset_with_cat_var %>% sapply(table)

house_prices3 <- house_prices2 %>% 
  select(-Street,-Utilities,-LandSlope,-Condition1,Condition2,-RoofMatl,-BsmtCond,
         -Heating,-Electrical,-Functional,-GarageQual,-GarageCond,-PavedDrive)


# drop variables with to many levels
house_prices4 <- house_prices3 %>% 
  select(-Neighborhood,-Exterior1st,-Exterior2nd,-MSSubClass)

# drop observations with missing value and drop id variable
house_prices5 <- house_prices4 %>% na.omit %>% select(-Id)


## investigate variables ##

# numeric variables
numeric_variables <- sapply(house_prices5, is.numeric)
subset_with_num_var <- house_prices5[,numeric_variables]
dim(subset_with_num_var)
correlation <- cor(subset_with_num_var)[,35]

corelated <- correlation[abs(correlation) > 0.25]

# categorical variables
categorigal_variables <- sapply(house_prices5, is.character) 
categorigal_variables[length(categorigal_variables)] <- T
subset_with_cat_var <- house_prices5[,c(categorigal_variables)]

subset_with_cat_var %>% group_by(MSZoning) %>% summarize(mean_price = mean(SalePrice))
subset_with_cat_var %>% group_by(CentralAir) %>% summarize(mean_price = mean(SalePrice))

subset_with_cat_var %>% ggplot(aes(SalePrice, fill = CentralAir)) + geom_density(alpha = 0.5)

# exercise 2
# a) calculate standard deviation for each numerical variable using
# apply function and subset_with_num_var object


# b) calculate mean LotArea using dplyr functions


# c) calculate mean LotArea for each level of MSZoning variable (using dplyr functions)



# specify the model with one categorical variable
one_factor_model <- lm(SalePrice~CentralAir,house_prices5)
summary(one_factor_model)

# add one more categorical variable
two_factor_model <- lm(SalePrice~CentralAir + MSZoning,house_prices5)
summary(two_factor_model)


# model with all the numeric variable with correlation with SalePrice higher than 0.25
cor_var <- corelated[-20] %>% names
Formula <- as.formula(paste("SalePrice ~ ", gsub(", "," + ",toString(cor_var))))
numeric_variables_model <- lm(Formula,house_prices5)
summary(numeric_variables_model)


# specify the model with all variable
initial_model <- lm(SalePrice~.,house_prices5)
summary(initial_model)


# apply a stepwise optimization algorithm
model_stepwise <- step(initial_model, direction = 'both')
summary(model_stepwise)


# plot the fitted vs observed price
fitted <- model_stepwise %>% fitted
observed <- house_prices5$SalePrice
plot(fitted,observed, pch = 1, col = 'firebrick1', xlim = c(0,5*10^5), ylim = c(0,5*10^5))
grid()

# plot the fitted vs residuals 
residuals_step <- model_stepwise %>% residuals
plot(fitted,residuals_step, pch = 1, col = 'cadetblue3')
grid()


# Exercise 3 
# a) plot the fitted vs observed price for numeric_variables_model



# plot the fitted vs residuals for numeric_variables_model







# other elements that could be checked/improved
plot(model_stepwise)

# outliers

# combining some levels in the categorical variables

# droping some variables to get simplier model