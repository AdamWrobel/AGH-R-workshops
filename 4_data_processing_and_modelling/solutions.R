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

# exercise 2
# a) calculate standard deviation for each numerical variable using
# apply function and subset_with_num_var object
apply(subset_with_num_var,2,sd)

# b) calculate mean LotArea using dplyr functions


# c) calculate mean LotArea for each level of MSZoning variable (using dplyr functions)



# Exercise 3 
# a) plot the fitted vs observed price for numeric_variables_model
fitted <- numeric_variables_model %>% fitted
observed <- house_prices5$SalePrice
plot(fitted,observed, pch = 1, col = 'firebrick1', xlim = c(0,5*10^5), ylim = c(0,5*10^5))
grid()
# plot the fitted vs residuals for numeric_variables_model
residuals_step <- numeric_variables_model %>% residuals
plot(fitted,residuals_step, pch = 1, col = 'cadetblue3')
grid()


# other elements that could be checked/improved
plot(model_stepwise)