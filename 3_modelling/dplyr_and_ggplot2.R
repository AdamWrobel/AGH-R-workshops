### przypomnienie rzeczy z poprzednich dwoch zajec ###

## data processing - dplyr ##
library(dplyr)

# %>% - pipe operator
iris %>% head()
head(iris)

# select columns/variables
iris %>% select(Species,Sepal.Length) %>% head

# filter row/observations
iris %>% filter(Sepal.Length > 2) %>% head

# create new variable - mutate
iris %>% mutate(Petal.Area = Petal.Length * Petal.Width) %>% head

# calculate aggregate - summarize
iris %>% summarise(mean_Sepal_Length = mean(Sepal.Length))

# calculate aggregate per group - group_by & summarize
iris %>% group_by(Species) %>% summarize(mean_Sepal_Length = mean(Sepal.Length))


## data vizualization - ggplot2 ##
library(ggplot2)

# initialize plot #
ggplot()

# specify input data # 
ggplot(iris)

# specfiy aesthetic of the plot
iris %>% head
ggplot(iris, aes(x = Sepal.Length, y = Petal.Width))

# add a plot of a given type
ggplot(iris, aes(x = Sepal.Length, y = Petal.Width)) + geom_point()

# add colour dimension
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, colour = Species)) + geom_point()
