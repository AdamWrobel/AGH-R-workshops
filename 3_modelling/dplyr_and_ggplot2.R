# przypomnienie rzeczy z poprzednich dwoch zajec

# dplyr
library(dplyr)
iris %>% head()

iris %>% group_by(Species) %>% summarize(mean_Sepal_Length = mean(Sepal.Length))


# ggplot2
library(ggplot2)
ggplot(iris, aes(x = Sepal.Length,y = Petal.Width, colour = Species)) + geom_point()
