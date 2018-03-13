# instal the ggplot2 package
install.packages('ggplot2')

# Load the ggplot2 package using library().
library(ggplot2)

# Exploring ggplot2, part 1
# To get a first feel for ggplot2, let's try to run some basic
# ggplot2 commands. Together, they build a plot of the mtcars 
# dataset that contains information about 32 cars from a 1973 
# Motor Trend magazine. This dataset is small, intuitive, 
# and contains a variety of continuous and categorical variables.

# Use str() to explore the structure of the mtcars dataset.
str(mtcars)
# attach(mtcars)

# Execute the following command
ggplot(mtcars, aes(x = cyl, y = mpg)) +
  geom_point()

# The plot from the previous exercise wasn't really satisfying.
# Although cyl (the number of cylinders) is categorical, 
# it is classified as numeric in mtcars. You'll have to 
# explicitly tell ggplot2 that cyl is a categorical variable.

# Change the command below so that cyl is treated as factor
# by wrapping factor() around cyl
ggplot(mtcars, aes(x = ---, y = mpg)) +
  geom_point()

ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_point()


# We'll use several datasets throughout the courses to
# showcase the discussed concepts.
# In the previous exercises, you already got to know mtcars. 
# Let's dive a little deeper to explore the three main topics 
# in this course: The data, aesthetics, and geom layers.


# You're encouraged to think about how the examples and 
# concepts we discuss throughout these data viz courses 
# apply to your own data-sets

# A scatter plot has been made for you
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

# In the second call of ggplot() change the color argument 
# in aes() (which stands for aesthetics). The color should 
# be dependent on the displacement of the car engine, found
# in disp.

# Replace ___ with the correct column
ggplot(mtcars, aes(x = wt, y = mpg, color = ___)) +
  geom_point()

ggplot(mtcars, aes(x = wt, y = mpg, color = disp)) +
  geom_point()

# In the third call of ggplot() change the size argument 
# in aes() (which stands for aesthetics). 
# The size should be dependent on the displacement of the 
# car engine, found in disp.

# Replace ___ with the correct column
ggplot(mtcars, aes(x = wt, y = mpg, size = ___ )) +
  geom_point()

ggplot(mtcars, aes(x = wt, y = mpg, size = disp)) +
  geom_point()
# In the previous exercise you saw that disp can be mapped onto a 
# color gradient or onto a continuous size scale.

# Another argument of aes() is the shape of the points. 
# There are a finite number of shapes which ggplot() can 
# automatically assign to the points. However, if you try 
# this command in the console 


ggplot(mtcars, aes(x = wt, y = mpg, shape = disp)) +
  geom_point()
# Why does R returns error?

# Possible Answers
# shape is not a defined argument.
# shape only makes sense with categorical data, and disp is continuous.
# shape only makes sense with continuous data, and disp is categorical.
# shape is not a variable in your dataset.
# shape has to be defined as a function.

# Anserw:
# shape only makes sense with categorical data, and disp is continuous.

# Pierwsza warstwa to 
# 1 Data - dane, oczywiœcie potrzebujemy jakiœ danych aby je (plot) zwizualizowaæ
# Jednym z klasycznych zbiorów danych jest IRIS, który zosta³ dodane przez 
# Edgara Anderson'a w 1940 i spopularyzowany przez Fishera'a. 

# Druga baza danych
head(iris)

# Zbiór danych zawiera indormacje o trzech kontretnych gatunkach. 
# - Setosa 
# - versicolor 
# - virginica

# 4 miary zosta³y sprawdzone dla ka¿dego z gatunku:
#  - lenght, width, sepal.length, sepal.width (to na górze kwiatka), 
# w zbiorze znajduje siê 5 zmiennych, 4 o których ju¿ wspomnia³am oraz 
# jedna odpowiadaj¹ca dla jakiego gatunku obserwacja pomiarów mia³a miejsce

# Druga wartswa to 
# AESTHETICS,  mówi nam jak¹ skalê powinniœmy zmapowaæ na nasze dane, 
# tutaj gdy drugi z g³ównych elementów gramatyki grafiki odgrywa rolê; 
# na pocz¹rku gramtycznych warstw to tutaj ustanawiamy nasze aesthetics mapowanie

# W tym wypadku gdzy chcemy stworzyæ scater plot 
# bêdziemy mapowaæ sepal width jako argument x (aesthetics) 
# na wartosci y (aethetics) sepal width 


# Trzecia, równie wa¿na warstwa, to 
# Geometrics, która pozwala na wybór geomterii, to znaczy jak 
# wykres bêdzie dzia³a³. 
# Po tym jak ustanowimy nasze trzy warstwy mamy wystarczaj¹co wiêdzê to stworzenia 
# podstawowego scater plot

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6)


# Nastêna, czwarta, warstwa której u¿ywamy to facets, mówi nam jak podzieliæ 
# nasze wykresy. W tym pryzpadku bedziemy chcieli storzyæ trzy rózne scater 
# plots ze wzglêdu na gatunek.

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6) + 
  facet_grid(.~Species) 

# Pi¹ta, statystyczna warstwa moze byæ u¿yta do policzenia wielu ró¿nych 
# parametrów.Na przyk³ad w naszym przypadku zdecydowaliœmy siê dodaæ liniê 
# regresji liniowej do ka¿dego ze zbioru danych. 

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6) + 
  facet_grid(.~Species) +
  stat_smooth(method = 'lm', se = F, col = 'red')

# Nastêpna, szósta jest  warstwa coordinates, która pozwala nam wyspecyfikowaæ
# wymiary wykresu. 

levels(iris$Species) <- c("Setosa", "Versicolor","Virginica")

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6) + 
  facet_grid(.~Species) +
  stat_smooth(method = 'lm', se = F, col = 'red') +
  scale_y_continuous("Sepal Width (cm)", 
                     limits = c(2,5),
                     expand = c(0,0)) +
  scale_x_continuous("Sepal Lenght (cm)", 
                     limits = c(4,8),
                     expand = c(0,0)) + 
  coord_equal()

# Wkoñcu ostatnia, siódma warstwa, theme
# która daje nam ³adnie wygl¹daj¹cy, znaczacy i mo¿liwy do publikacji wykres
# bezpoœrednio z jêzyka R

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6) + 
  facet_grid(.~Species) +
  stat_smooth(method = 'lm', se = F, col = 'red') +
  scale_y_continuous("Sepal Width (cm)", 
                     limits = c(2,5),
                     expand = c(0,0)) +
  scale_x_continuous("Sepal Lenght (cm)", 
                     limits = c(4,8),
                     expand = c(0,0)) + 
  coord_equal() +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        axis.text = element_text(colour = 'black'),
        axis.ticks = element_line(colour = 'black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        strip.text = element_blank(),
        panel.spacing = unit(1,'lines')
        )
# Jeœli zawsze tworzymy te same ustawienia na theme mo¿na je zamkn¹c w funkcji 

theme_Ania <- function() {
  return(theme_bw() + theme(panel.background = element_blank(),
                     plot.background = element_blank(),
                     legend.background = element_blank(),
                     legend.key = element_blank(),
                     strip.background = element_blank(),
                     axis.text = element_text(colour = 'black'),
                     axis.ticks = element_line(colour = 'black'),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.line = element_line(color = 'black'),
                     strip.text = element_blank(),
                     panel.spacing = unit(1,'lines'))
  )
                     
}

# ggplot obiekt z ustawieniami Theme stworzonymi stworzony przez Ciebie

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6) + 
  facet_grid(.~Species) +
  stat_smooth(method = 'lm', se = F, col = 'red') +
  scale_y_continuous("Sepal Width (cm)", 
                     limits = c(2,5),
                     expand = c(0,0)) +
  scale_x_continuous("Sepal Lenght (cm)", 
                     limits = c(4,8),
                     expand = c(0,0)) + 
  coord_equal() +
  theme_Ania()

# Rozwa¿my koncept warstw dok³adniej w kolejnych æwiczeniach. 
# The diamonds data frame contains 
# information on the prices and various metrics of 50,000 diamonds. 
# Among the variables included are carat (a measurement of the size of the 
# diamond) and price.
# For the next exercises, you'll be using a subset of 1,000 diamonds.

# Here you'll use two common geom layer functions: geom_point() and geom_smooth()
# We already saw in the earlier exercises how these are added using the + 
# operator.

# Explore the diamonds data frame with str()
str(diamonds)
# Create a scatter plot of the diamonds dataset, with carat on the x-axis 
# and price on the y-axis. 
# Add geom_point() with + This will tell ggplot2 to draw points on the plot.
ggplot(diamonds, aes(x = ----, y = -----)) + ----

ggplot(diamonds, aes(x = carat, y = price)) + geom_point()

# Add geom_point() and geom_smooth() with +
# These just stack on each other! geom_smooth() will draw a smoothed line over
# the points.

ggplot(diamonds, aes(x = carat, y = price)) + geom_point() + ------

ggplot(diamonds, aes(x = carat, y = price)) + geom_point() + geom_smooth()

# With this plot as a starting point, let's explore some more possibilities of
# combining geoms.
# Plot 2 - Copy and paste previous plot, but show only the smooth line, 
# no points

ggplot(diamonds, aes(x = carat, y = price)) + -----

ggplot(diamonds, aes(x = carat, y = price)) + geom_smooth()

# Show only the smooth line, but color according to clarity by placing the 
# argument color = clarity in the aes() function of your ggplot() call.

ggplot(diamonds, aes(x = carat, y = price, color = clarity)) + ----
  
ggplot(diamonds, aes(x = carat, y = price, color = clarity)) + geom_smooth()

#  Draw translucent colored points.
# Copy the ggplot() previus command (with clarity mapped to color).
# Remove the smooth layer.
# Add the points layer back in.
# Set alpha = 0.4 inside geom_point(). This will make the points 40% transparent.

ggplot(diamonds, aes(x = carat, y = price, color = clarity)) + -----

ggplot(diamonds, aes(x = carat, y = price, color = clarity)) +
  geom_point(alpha = 0.4)

# Understanding the grammar, part 1
# Here you'll explore some of the different grammatical elements. 
# Throughout this course, you'll discover how they can be combined in all 
# sorts of ways to develop unique plots.

# In the following instructions, you'll start by creating a ggplot object 
# from the diamonds dataset. Next, you'll add layers onto this object to 
# build beautiful & informative plots.

# 1 Define the data (diamonds) and aesthetics layers. 
# Map carat on the x axis and price on the y axis. 
# Assign it to an object: dia_plot

# Create the object containing the data and aes layers: dia_plot
dia_plot <- ggplot(----, aes(x = ----, y = ----))

dia_plot <- ggplot(diamonds, aes(x = carat, y = price))

# Add a geom layer with + and geom_point()
dia_plot + ----

dia_plot + geom_point()

# Add the same geom layer, but with aes() inside

dia_plot + geom_point(aes(color = ----))

dia_plot + geom_point(aes(color = clarity))

# Understanding the grammar, part 2
# Continuing with the previous exercise, here you'll explore mixing 
# arguments and aesthetics in a single geometry.

# You're still working on the diamonds dataset.

# Expand dia_plot by adding geom_point() with alpha set to 0.2
dia_plot <- dia_plot + geom_point(----)

dia_plot <- dia_plot + geom_point(alpha = 0.2)

# Plot dia_plot with additional geom_smooth() with se set to FALSE
dia_plot + ----

dia_plot + geom_smooth(se = FALSE)

# Modify the geom_smooth() function from the previous instruction 
# so that it contains aes() and map clarity to the col argument.
dia_plot + geom_smooth(aes(col = ----), se = FALSE)

dia_plot + geom_smooth(aes(col = clarity), se = FALSE)

# Cw Wysymulowac jedna sciezke procesu Wienera
## petle w symulacjach ##

number_of_steps <- 10
N <- rnorm(n = number_of_steps+1, sd = 0.5)
path <- vector(mode="numeric", length = number_of_steps)
dt <- 1/number_of_steps
# jedna sciezka - 10 krokow 
for(i in 1:(number_of_steps+1)){
  path[i+1] <- path[i] + sqrt(dt)*N[i]
}
df <- data.frame(s=1:12,sim = path)
ggplot(df, aes(x=s,y=sim)) + geom_line()


library(dplyr)
# 10 paths
N3 <- rnorm(n = 260*10, sd = sqrt(dt))
tabela <- data.frame(N3, scenario = rep(1:10, each= 260), step = seq(1,260))
tabela <- tabela %>% group_by(scenario) %>% mutate(path = cumsum(N3))


# cwiczenie
# zrob wykres realizacji sciezek, gdzie kazda ze sciezek bedzie zaznaczona
# innym kolorem
# uzyj tabela i funkcji ggplot

# omega - scenariusz
# step to krok czasowy !
ggplot(tabela, aes(x = step, y = path, col = scenario)) + geom_line()

ggplot(tabela, aes(x = step, y = path, col = as.factor(scenario)) + geom_line()
       