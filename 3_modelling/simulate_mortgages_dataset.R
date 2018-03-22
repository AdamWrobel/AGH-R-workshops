N <- 3000

income <- rpois(N, 5) * 1000 %>% pmax(2000)
loan_size <- abs(round(rnorm(N, 20,7) * 15000 + abs(round(rnorm(N,0.2,0.1) * income/3)*10))*100)/100

loan_size <- round(abs(round(rnorm(N, 30,10) * 15000))/100)*100
collateral <- round((abs(round(rnorm(N,1,0.3) * loan_size)) + abs(round(rnorm(N, 3,2) * 10000)))/100)*100
city <- c(rep('CA.Los.Angeles',900),rep('MI.Detroit',600),rep('IL.Chicago',1500))

            
            
cor(loan_size,collateral)
plot(loan_size,collateral)

(collateral/loan_size) %>% hist

mortgages <- data.frame(income,loan_size,collateral,city) %>% 
  mutate(loan_size = ifelse(city == 'CA.Los.Angeles', loan_size + 200000 * round(abs(rnorm(1)),3),loan_size),
         loan_size = ifelse(city == 'MI.Detroit', loan_size - 100000 * round(abs(rnorm(1)),3),loan_size),
         collateral = ifelse(city == 'CA.Los.Angeles', collateral + 100000 * round(abs(rnorm(1)),3),collateral),
         collateral = ifelse(city == 'MI.Detroit', collateral - 50000 * round(abs(rnorm(1)),3),collateral)) %>%
         mutate(collateral = ifelse(collateral < 0 , -collateral, collateral),
                loan_size = ifelse(loan_size < 0 , -loan_size, loan_size),
                loan_size = pmin(loan_size,1.5 * collateral),
                LTV = loan_size/collateral)
mortgages <- data.frame(mortgages, ran = runif(N)) %>% arrange(ran) %>% select(-ran) %>%
  mutate(PD = runif(N,max = LTV/20),
         PD = ifelse(city == 'IL.Chicago', PD + rnorm(1500,mean = 0.15, sd = 0.02)/20,PD))
colnames(mortgages)[3] <- 'collateral_value'
mortgages 

write.csv(mortgages, 'mortgages.csv',row.names = F)
