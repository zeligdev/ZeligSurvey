data(api)

# In this example, we will estimate a model using 
# the percentages of students who receive subsidized 
# lunch and an indicator for whether schooling is 
# year-round to predict California public schools' 
# academic performance index scores:

z.out1 <- zelig(api00 ~ meals + yr.rnd, model = "normal.survey",  
  weights=~pw, data = apistrat)

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

# Generate a second set of fitted values and a plot:

plot(s.out1)
