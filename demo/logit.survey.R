data(api)

# In this example, we will estimate a model using 
# the percentages of students who receive subsidized 
# lunch and the percentage who are new to a school
# to predict whether each California public school 
# attends classes year round.

z.out1 <- zelig(yr.rnd ~ meals + mobility, model = "logit.survey", weights=~pw, data = apistrat)

# Set explanatory variables to their default (mean/mode) values, and set
# a high (80th percentile) and low (20th percentile) value for "meals,"
# the percentage of students who receive subsidized meals:

x.low <- setx(z.out1, meals= quantile(apistrat$meals, 0.2))
x.high <- setx(z.out1, meals= quantile(apistrat$meals, 0.8))

# Generate first differences for the effect of high versus low "meals" 
# on the probability that a school will hold classes year round:

s.out1 <- sim(z.out1, x=x.low, x1=x.high)

# Summary of fitted model

summary(z.out1)

# Summary of simulated quantities of interest

summary(s.out1)

# Generate a second set of fitted values and a plot:

plot(s.out1)
