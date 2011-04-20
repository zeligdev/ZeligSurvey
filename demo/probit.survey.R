# Attach data-frame

data(api)

# Fit the model to the data

z.out1 <- zelig(
                yr.rnd ~ meals + mobility,
                model = "probit.survey",
                weights=~pw,
                data = apistrat
                )

# Set the meals explanatory variable to its 20th and 80th quantile

x.low <- setx(z.out1, meals = quantile(apistrat$meals, 0.2))
x.high <- setx(z.out1, meals = quantile(apistrat$meals, 0.8))

# Simulate quantities of interest

s.out1 <- sim(z.out1, x=x.low, x1=x.high)

# Summary of fitted model

summary(z.out1)

# Summary of simulated quantities of interest

summary(s.out1)

# Plot of simulated quantities of interest
plot(s.out1)
