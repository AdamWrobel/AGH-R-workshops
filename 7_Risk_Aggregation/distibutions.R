
library(dplyr)
library(ggplot2)

## distibutions ##

# normal distibution
N1 <- rnorm(10000, mean = 0, sd = 1)
N1 %>% hist

# plot density
N1 %>% density %>% plot(xlim = c(-7,7), main = 'Normal distibutions density')
grid()

# change the mean
N2 <- rnorm(10000, mean = 2, sd = 1)
N2 %>% density %>% lines(col = 'blue')

# change the standard deviation
N3 <- rnorm(10000, mean = 0, sd = 2)
N3 %>% density %>% lines(col = 'red')

legend('topleft',c('mean = 0, sd = 1', 'increase mean by 2', 'increase standard deviation by 1'),
       col = c('black','blue','red'), lty = 1, cex = 0.8)


# uniform distibution
U1 <- runif(10000)
U1 %>% hist

# binomial distibution
B1 <- rbinom(10000, size = 10, prob = 0.2)
B1 %>% table

# poisson distibution
P1 <- rpois(10000, lambda = 5)
P1 %>% table


# exercise 1 
# create a date.frame that will contain three variables (each variable with 1000 realizations):
# P2 defined as poisson distibution with lambda equal to 20
# B2 defined as binomial distibution with probability of success of 0.8 and number of trials equal to 20
# N4 defined as normal distibution with a mean of 10 and sd of 6





# normal inverse gaussian distibution
# install.packages('ghyp')
library(ghyp)
N1 <- rnorm(10000, mean = 0, sd = 1)
fit <- fit.NIGuv(N1, silent = T)
N1 %>% density %>% plot
qghyp(seq(from=0, to =1, by = 0.005),fit) %>% density %>% lines(col = 'blue')
grid()

NIG <- NIG(chi = 2, psi = 2, mu = 0, sigma = diag(1), gamma = 0)
NIG1<- qghyp(seq(from=0, to =1, by = 0.005),NIG) 
NIG1 %>% density %>% plot

NIG <- NIG(chi = 2, psi = 2, mu = 0, sigma = diag(1), gamma = -2)
NIG1<- qghyp(seq(from=0, to =1, by = 0.005),NIG) 
NIG1 %>% density %>% plot

NIG <- NIG(chi = 2, psi = 2, mu = 0, sigma = diag(1), gamma = 2)
NIG1<- qghyp(seq(from=0, to =1, by = 0.005),NIG) 
NIG1 %>% density %>% plot


# reading the data
WIG <- read.csv('https://raw.githubusercontent.com/AdamWrobel/AGH-R-workshops/master/7_Risk_Aggregation/EQ_WIG.csv')

WIG %>% head
WIG %>% tail

WIG <-
WIG %>% mutate(year_month = substr(Date,1,7)) %>% group_by(year_month) %>% 
  filter(substr(Date,9,10) == max(substr(Date,9,10))) %>% data.frame
plot(WIG$EQ_WIG, type = 'l')
grid()


IR_PLN <-read.csv('https://raw.githubusercontent.com/AdamWrobel/AGH-R-workshops/master/7_Risk_Aggregation/IR_PLN.csv') %>% 
  mutate(year_month = substr(Date,1,7))
plot(IR_PLN$YR1, type = 'l')
matplot(IR_PLN[,2:4], type = 'l') # for other tenors
grid()

## bind both
risk_drivers <- IR_PLN %>% left_join(WIG, by = 'year_month') %>% select(-contains('Date'))

# transofrm data
risk_drivers_trans <- risk_drivers %>% 
  mutate(WIG_log = log(EQ_WIG)) %>% 
  mutate(WIG_log_return = WIG_log - lag(WIG_log)) %>%
  mutate(YR1_log_return = log(YR1) - lag(log(YR1))) %>%
  filter(is.na(WIG_log_return) ==F) %>%
  mutate(YR1_log_return_N = scale(YR1_log_return),WIG_log_return_N = scale(WIG_log_return))

# plot data
risk_drivers_trans %>% select(YR1_log_return) %>% as.matrix %>% density %>% plot(main = 'YR1 log return & EQ_WIG log return PDF')
risk_drivers_trans %>% select(WIG_log_return) %>% as.matrix %>% density %>% lines(col = 'red')
risk_drivers_trans %>% select(YR1_log_return_N) %>% as.matrix %>% density %>% plot(main = 'Normalized log return PDF')
risk_drivers_trans %>% select(WIG_log_return_N) %>% as.matrix %>% density %>% lines(col = 'red')
grid()
legend('topright', c('YR1_normalized','EQ_WIG_normalized'), col = c('red','black'), lty=1)

# fit NIG distribution
YR1_NIG <- fit.NIGuv(risk_drivers_trans$YR1_log_return, silent = T)
EQ_WIG_NIG <- fit.NIGuv(risk_drivers_trans$WIG_log_return, silent = T)


# check the fit quality - YR1
risk_drivers_trans %>% select(YR1_log_return) %>% as.matrix %>% density %>% plot(main = 'YR1 log return probability density function',lwd = 2)
qghyp(seq(from=0, to =1, by = 0.005),YR1_NIG) %>% density %>% lines(col = 'cadetblue3', lwd = 2, lty = 2)
grid()
legend('topleft',c('empirical distibution', 'fitted NIG distibution'), lty = c(1,2),lwd = 2, col = c('black','cadetblue3'))

# normal instead of NIG
risk_drivers_trans %>% select(YR1_log_return) %>% as.matrix %>% density %>% plot(main = 'YR1 log return probability density function',lwd = 2)
qnorm(seq(from=0, to =1, by = 0.005), 
      mean = mean(risk_drivers_trans$YR1_log_return),
      sd = sd(risk_drivers_trans$YR1_log_return)) %>% 
  density %>% lines(col = 'cadetblue3', lwd = 2, lty = 2)
grid()
legend('topleft',c('empirical distibution', 'fitted normal distibution'), lty = c(1,2),lwd = 2, col = c('black','cadetblue3'))


# check the fit quality - WIG
risk_drivers_trans %>% select(WIG_log_return) %>% as.matrix %>% density %>% plot(main = 'EQ WIG log return probability density function',lwd = 2, ylim = c(0,7))
qghyp(seq(from=0, to =1, by = 0.005),EQ_WIG_NIG) %>% density %>% lines(col = 'cadetblue3', lwd = 2, lty = 2)
grid()
legend('topleft',c('empirical distibution', 'fitted NIG distibution'), lty = c(1,2), lwd = 2, col = c('black','cadetblue3'))

# normal instead of NIG
risk_drivers_trans %>% select(WIG_log_return) %>% as.matrix %>% density %>% plot(main = 'EQ WIG log return probability density function',lwd = 2, ylim = c(0,7))
qnorm(seq(from=0, to =1, by = 0.005), 
      mean = mean(risk_drivers_trans$WIG_log_return),
      sd = sd(risk_drivers_trans$WIG_log_return)) %>% 
  density %>% lines(col = 'cadetblue3', lwd = 2, lty = 2)
grid()
legend('topleft',c('empirical distibution', 'fitted normal distibution'), lty = c(1,2),lwd = 2, col = c('black','cadetblue3'))


