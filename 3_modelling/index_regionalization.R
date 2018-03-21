# regionalizacja indexu 

# wczytanie danych o Home Price Index
HPI <- read.csv("HPI.csv")

# przejrzenie danych
str(HPI)
View(HPI)
head(HPI)

# przetworzenie danych na poprzeby wizualizacji

# zamiana typu zmiennej Date
str(HPI$Date)
HPI$Date <- HPI$Date %>% as.Date()
str(HPI$Date)

# dodajmy zmienna miesiac
HPI2 <- HPI %>% mutate(month = substr(Date,6,7))
head(HPI2)

# pakiet tidyr
install.packages('tidyr')
library(tidyr)

# przejscie na dlugi format tabeli
head(HPI2[,1:5])
HPI_long <- HPI2 %>% gather('Index','Value', AZ.Phoenix:National.US)
head(HPI_long)

HPI_long <- HPI_long %>% filter(is.na(Value) == F)
head(HPI_long)

# wizualizacja danych
HPI_long %>% ggplot() + 
  geom_line(aes(x = Date, y = Value, group = Index, colour = Index))

# tylko kilka wybranych miast i index dla US
HPI_long %>% filter(Index %in% c('CA.Los.Angeles','MI.Detroit','IL.Chicago','National.US')) %>% 
  ggplot() + geom_line(aes(x = Date, y = Value, Group = Index, colour = Index))

# policzenie stop zwrotu
HPI_long_2<- HPI_long %>% group_by(Index) %>% 
  mutate(simple_quarterly_return = (Value-lag(Value,3))/lag(Value,3)) %>%
  filter(month %in% c('03','06','09','12'))

# przejscie z dlugiego na szeroki format
HPI_wide <- HPI_long_2 %>% select(-Value) %>% spread(key = Index, value = simple_quarterly_return)
                                
# model dla trzech wybranych stanow (w naszym przykladzie to w tych stanach mamy skoncentrowany nasz portfel)
HPI_wide <- HPI_wide %>% select(Date,National.US,CA.Los.Angeles,MI.Detroit,IL.Chicago)
HPI_wide <- HPI_wide[complete.cases(HPI_wide),]

model_Los_Angeles <- lm(CA.Los.Angeles ~ National.US,data = HPI_wide)
summary(model_Los_Angeles)

summary(model_Los_Angeles)
plot(y = HPI_wide$CA.Los.Angeles, x = HPI_wide$National.US, 
     xlim = c(-0.1,0.1), ylim = c(-0.1,0.1),pch = 19,
     main = "Los Angeles")
abline(model_Los_Angeles$coefficients, lwd = 2)
grid()

## cwiczenie 3 ##
# a) stworz analogiczne modele dla Detroit i Chicago
model_Detroit <- 
model_Chicago <- 

# b) stworzyc wykres dla Detroit (taki sam jak dla Los Angeles)
summary(model_Detroit)



## cwiczenie 4 ##
# a) zaaplikuj parametry beta do wyznaczania wartosci zabezpieczenia w stresie 
#    oraz wyznaczy EL po tej zmianie
betas_df <- data.frame(beta = c(model_Los_Angeles$coefficients[2],
                                 model_Detroit$coefficients[2],
                                 model_Chicago$coefficients[2]),
                       city = c('CA.Los.Angeles','MI.Detroit','IL.Chicago'))

mortgages_results_2 <-
mortgages %>% left_join(betas_df) %>%
  mutate(collateral_stress = ... * ...,
         LTV_stress = .../...,
         LGD = pnorm(...),
         EAD = loan_size,
         EL = PD * LGD * EAD)

# b) porownaj wyniki modeli
mortgages_results$LGD %>% quantile(c(0.25, 0.5, 0.75, 0.99))
mortgages_results_2$LGD %>% quantile(c(0.25, 0.5, 0.75, 0.99))

mortgages_results$EL %>% quantile(c(0.25, 0.5, 0.75, 0.99))
mortgages_results_2$EL %>% quantile(c(0.25, 0.5, 0.75, 0.99))

# c) stworz wykres funckji gestosci EL dla obu wersji (z parametrami beta i z samym indeksem narodowamy)


# Zadanie domowe #
# Ogranicz probkê, na ktorej jest zbudowany model do danych po 2003 i podaj wp³yw na model
# mozna na przyklad wyznaczyc rok dla ka¿dej obserwacji i wyfiltrowac obserwaje po 2003
# mozna rowniez wyfiltrowac po dacie porownujac siê na przyklad z as.Date('2003-01-01')

model_Los_Angeles_new <- 
  lm())


summary(model_Los_Angeles)
summary(model_Los_Angeles_new)
