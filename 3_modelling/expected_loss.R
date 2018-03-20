## ryzyko kredytowe - wyznaczenie oczekiwanej straty przy spadku cen nieruchomosci ##


# wczytaj pakiet dplyr i ggplot2


# zdefiniuj katalog roboczy


# wczytaj plik mortgages.csv i zapisz jako mortgages (wykorzystujac funkcje read.csv)
mortgages <- 

# ocen wartosci zmiennych wykorzystujac funkcje summary na obiekcie mortgages


# a) wyznacz srednia dla zmiennych: loan_size, LTV, PD dla ka¿dego miasta

# b) stworz histogramy dla tych trzech zmiennych wykorzystujac funkcje hist


# c) stworz rozklad funkcji gestosci dla loan_size (inny kolor dla kazdego miasta)
#    wykorzystujac funkcje ggplot i geom_density


# d) wyznacz oczekiwana strate dla kazdego klienta wypelniajac luki w kodzie:
# zakladajc spadek wartosci nieruchomosci zgodnie z spadkiem indeksu
# cen nieruchomosci w USA o 18%
mortgages_results <-
mortgages %>% mutate(collateral_stress = ...,
                     LTV_stress = ...,
                     LGD = pnorm(...),
                     EAD = loan_size,
                     EL = ... * ... * ...)

# e) policz kwantyle 0.25, 0.5, 0.75, 0.99 dla zmiennych LGD i EL
# funkcja quantile


# f) stworz histogram dla zmiennych LGD i EL (funkcja hist)

