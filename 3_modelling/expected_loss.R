# ryzyko kredytowe - wyznaczenie oczekiwanej straty przy spadku cen nieruchomosci

# wczytaj pakiet dplyr i ggplot2
library(dplyr)
library(ggplot2)

# zdefiniuje katalog roboczy
setwd('D:/R_projects/AGH_workshops/3_modelling')

# wczytaj plik mortgages.csv i zapisz jako mortgages
mortgages <- read.csv("mortgages.csv")

# ocen wartosci zmiennych wykorzystujac funkcje summary na obiekcie mortgages
summary(mortgages)

# a) wyznacz srednia dla zmiennych: loan_size, LTV, PD dla ka¿dego miasta
mortgages %>% group_by(city) %>%
  summarize(mean_loan_size = mean(loan_size), 
            mean_LTV = mean(LTV),
            mean_PD = mean(PD))

# b) stworz histogramy dla tych trzech zmiennych wykorzystujac funkcje hist
hist(mortgages$loan_size)
hist(mortgages$LTV)
hist(mortgages$PD)

# c) stworz rozklad funkcji gestosci dla loan_size (inny kolor dla kazdego miasta)
#    wykorzystujac funkcje ggplot i geom_density
ggplot(mortgages) + geom_density(aes(loan_size, fill = city), alpha = 0.5)

# d) wyznacz oczekiwana strate dla kazdego klienta wypelniajac luki w kodzie:
# zakladajc spadek wartosci nieruchomosci zgodnie z spadkiem indeksu
# cen nieruchomosci w USA o 18%
mortgages_results <-
mortgages %>% mutate(collateral_stress = collateral * (1-0.18),
                     LTV_stress = loan_size/collateral_stress,
                     LGD = pnorm(-2.2+1.3*LTV_stress),
                     EAD = loan_size,
                     EL = PD * LGD * loan_size)
mortgages_results <-
mortgages %>% mutate(collateral_stress = ...,
                     LTV_stress = ...,
                     LGD = pnorm(...),
                     EAD = loan_size,
                     EL = ... * ... * ...)

# e) policz kwantyle 0.25, 0.5, 0.75, 0.99 dla zmiennych LGD i EL
# funkcja quantile
mortgages_results$LGD %>% quantile(c(0.25, 0.5, 0.75, 0.99))
mortgages_results$EL %>% quantile(c(0.25, 0.5, 0.75, 0.99))

# f) stworz histogram dla zmiennych LGD i EL
hist(mortgages_results$LGD, col = 'blue')
hist(mortgages_results$EL, col = 'green')

