# Attach data-frame

data(api)

# Fit the statistical model

z.out1 <- zelig(api00 ~ meals + yr.rnd,
                model   = "gamma.survey",  
                weights = ~pw,
                data    = apistrat
                )



# Set explanatory variables to their default (mean/mode) values, and set
# a high (80th percentile) and low (20th percentile) value for "meals,"
# the percentage of students who receive subsidized meals:

x.low <- setx(z.out1, meals= quantile(apistrat$meals, 0.2))
x.high <- setx(z.out1, meals= quantile(apistrat$meals, 0.8))


# Generate first differences for the effect of high versus low "meals" 
# on academic performance:

s.out1 <- sim(z.out1, x=x.high, x1=x.low)


# Summary of fitted model

summary(z.out1)

# Summary of simulated quantities of interest

summary(s.out1)

# Plot of simulated quantities of interest

plot(s.out1)
