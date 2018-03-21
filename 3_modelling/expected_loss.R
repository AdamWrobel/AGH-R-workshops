## ryzyko kredytowe - wyznaczenie oczekiwanej straty przy spadku cen nieruchomosci ##


# wczytaj pakiet dplyr i ggplot2
library(dplyr)
library(ggplot2)

# zdefiniuj katalog roboczy
setwd('D:/R_projects/AGH_workshops/3_modelling')

# wczytaj plik mortgages.csv i zapisz jako mortgages (wykorzystujac funkcje read.csv)
mortgages <- read.csv('mortgages.csv')
head(mortgages)
# income - przychod
# loan_size - kwota zadluzenia
# collateral_value - wartosc zabezpieczenia (w tym przypadku nieruchomosci)
# city - kod stanu USA i nazwa miast
# LTV - loan to value: kwata zadluzenia podzielona przez wartosc zabezpieczenia
# PD - probability of default


# Cwiczenie 1 #
# a ) ocen wartosci zmiennych wykorzystujac funkcje summary na obiekcie mortgages



# b) wyznacz srednia dla zmiennych: loan_size, LTV, PD dla ka¿dego miasta



# c) stworz histogramy dla zmiennych LTV i PD wykorzystujac funkcje hist
#    jesli grupa jeszcze pracuje, a Ty juz skonczyles to sprobuj zmodyfikowac
#    histogramy tak aby wygladaly lepiej
hist(mortgages$loan_size, col = 'red')
hist(...)
hist(...)


# rozklad funkcji gestosci dla loan_size (inny kolor dla kazdego miasta)
ggplot(mortgages, aes(loan_size, fill = city)) + geom_density(alpha = 0.5)


# Cwiczenie 2 #
# a) wyznacz oczekiwana strate dla kazdego klienta wypelniajac luki w kodzie:
#    zakladajac spadek wartosci nieruchomosci zgodnie z spadkiem indeksu
#    cen nieruchomosci w USA o 18%
mortgages_results <-
mortgages %>% mutate(collateral_value_stress = ... * ...,
                     LTV_stress = .../...,
                     LGD = pnorm(...),
                     EAD = loan_size,
                     EL = PD * LGD * EAD)

# b) policz kwantyle 0, 0.25, 0.5, 0.75, 0.99 dla zmiennych LGD i EL
#    uzywajac funkcji quantile



# c) stworz histogram dla zmiennych LGD i EL (funkcja hist)

