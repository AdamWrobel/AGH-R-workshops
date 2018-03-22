# Cwiczenie 1 #
# a ) ocen wartosci zmiennych wykorzystujac funkcje summary na obiekcie mortgages
summary(mortgages)


# b) wyznacz srednia dla zmiennych: loan_size, LTV, PD dla ka¿dego miasta
mortgages %>% group_by(city) %>%
  summarize(mean_loan_size = mean(loan_size), 
            mean_LTV = mean(LTV),
            mean_PD = mean(PD))

# c) stworz histogramy dla zmiennych LTV i PD wykorzystujac funkcje hist
#    jesli grupa jeszcze pracuje, a Ty juz skonczyles to sprobuj zmodyfikowac
#    histogramy tak aby wygladaly lepiej
hist(mortgages$loan_size, col = 'chartreuse1')
hist(mortgages$LTV, col = 'chocolate1')
hist(mortgages$PD, col = 'firebrick1')



# Cwiczenie 2 #
# a) wyznacz oczekiwana strate dla kazdego klienta wypelniajac luki w kodzie:
#    zakladajac spadek wartosci nieruchomosci zgodnie z spadkiem indeksu
#    cen nieruchomosci w USA o 18%
mortgages_results <-
  mortgages %>% mutate(collateral_stress = collateral * (1-0.18),
                       LTV_stress = loan_size/collateral_stress,
                       LGD = pnorm(-2.2+1.3*LTV_stress),
                       EAD = loan_size,
                       EL = PD * LGD * loan_size)

# b) policz kwantyle 0, 0.25, 0.5, 0.75, 0.99 dla zmiennych LGD i EL
#    uzywajac funkcji quantile
mortgages_results$LGD %>% quantile(c(0.25, 0.5, 0.75, 0.99))
mortgages_results$EL %>% quantile(c(0.25, 0.5, 0.75, 0.99))


# c) stworz histogram dla zmiennych LGD i EL (funkcja hist)
hist(mortgages_results$LGD, col = 'blue')
hist(mortgages_results$EL, col = 'green')


mortgages_results %>% select(LGD) %>% as.matrix() %>% hist