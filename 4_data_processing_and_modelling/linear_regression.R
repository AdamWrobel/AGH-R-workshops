## linear regression ##

# read data
HPI_wide <- read.csv('https://raw.githubusercontent.com/AdamWrobel/AGH-R-workshops/master/4_data_processing_and_modelling/HPI_wide_format.csv',
                     stringsAsFactors = F)
HPI_wide %>% head

# fit linear regression
model_Los_Angeles <- lm(CA.Los.Angeles ~ National.US,data = HPI_wide)
summary(model_Los_Angeles)

# plot the relation between Los Angeles and National index
plot(y = HPI_wide$CA.Los.Angeles, x = HPI_wide$National.US, 
     xlim = c(-0.1,0.1), ylim = c(-0.1,0.1),pch = 19,
     main = "Los Angeles", col = 'firebrick1')
abline(model_Los_Angeles$coefficients, lwd = 2)
grid()

# plot the residuals
residuals <- model_Los_Angeles %>% residuals %>% scale
fitted <- model_Los_Angeles %>% fitted
plot(fitted,residuals, pch = 19, col = 'firebrick1')
grid()

# residuals density
residuals %>% density %>% plot
x <- seq(from = 0.001, to = 0.999, by  = 0.001)
qnorm(x) %>% density %>% lines(col = 'chartreuse1')
grid()

# test for normality
shapiro.test(residuals)

shapiro.test(rnorm(100)) # test for random draws from normal distibution
shapiro.test(qnorm(x)) # test for teoretical distibution

# much more normality tests in nortest package
# nortest::ad.test(residuals)

# exercise 1
model_Detroit <- lm(MI.Detroit ~ National.US,data = HPI_wide)
summary(model_Detroit)

# a) save normalized residuals of model_Detroit as residuals_D
residuals_D <- 

  
# b) plot density of the residuals of a model for Detroit   

  
  
# c) test normality of the residuals of a model for Detroit

  
# if group is still working and you are already done then 
# implement the same steps model_Chicago
model_Chicago <- lm(IL.Chicago ~ National.US,data = HPI_wide)
summary(model_Chicago)
  