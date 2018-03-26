### przypomnienie rzeczy z poprzednich zajec ###

mortgages <- read.csv('https://raw.githubusercontent.com/AdamWrobel/AGH-R-workshops/master/3_modelling/mortgages.csv')

## data processing - dplyr ##
library(dplyr)

# %>% - pipe operator 
# input %>% function
mortgages %>% head

# equivalent to function(input)
head(mortgages)

# select columns/variables
mortgages %>% select(city,loan_size) %>% head

# filter row/observations
mortgages %>% filter(LTV < 1) %>% head

# create new variable - mutate
mortgages %>% mutate(collateral_value_stress = collateral_value * (1-0.18)) %>% head

# calculate aggregate - summarize
mortgages %>% summarise(mean_loan_size = mean(loan_size))

# calculate aggregate per group - group_by & summarize
mortgages %>% group_by(city) %>%
  summarize(mean_loan_size = mean(loan_size), 
            mean_LTV = mean(LTV),
            mean_PD = mean(PD))


## data vizualization - ggplot2 ##
HPI_long <- read.csv('https://raw.githubusercontent.com/AdamWrobel/AGH-R-workshops/master/4_data_processing_and_modelling/HPI_long_format.csv',
                     stringsAsFactors = F)
HPI_long <- HPI_long %>% mutate(Date = as.Date(Date))
library(ggplot2)

# initialize plot #
ggplot()

# specify input data # 
ggplot(HPI_long)

# specfiy aesthetic of the plot
HPI_long %>% head
ggplot(HPI_long, aes(x = Date, y = Value))

# add a plot of a given type
ggplot(HPI_long, aes(x = Date, y = Value)) + geom_line()

# maybe geom_point?
ggplot(HPI_long, aes(x = Date, y = Value)) + geom_point()

# maybe adding colour dimension?
ggplot(HPI_long, aes(x = Date, y = Value, colour = Index)) + 
  geom_line()
