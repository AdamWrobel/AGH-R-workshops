# exercise 1
model_Detroit <- lm(MI.Detroit ~ National.US,data = HPI_wide)
summary(model_Detroit)

# a) save normalized residuals of model_Detroit as residuals_D
residuals_D <- model_Detroit %>% residuals %>% scale

# b) plot density of the residuals of a model for Detroit   
residuals_D %>% density %>% plot
x <- seq(from = 0.001, to = 0.999, by  = 0.001)
qnorm(x) %>% density %>% lines(col = 'chartreuse1')
grid()

# c) test normality of the residuals of a model for Detroit
shapiro.test(residuals_D)