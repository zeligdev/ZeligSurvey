# Attach data-frame

data(api)

# In this example, we will estimate a model using 
# each school's academic performance in 2000 and an
# indicator for year-round schools to predict the 
# number of students who enrolled in each California school.

z.out1 <- zelig(enroll ~ api99 + yr.rnd , model = "poisson.survey", data = apistrat)

# Set explanatory variables to their default (mean/mode) values, and set
# a high (80th percentile) and low (20th percentile) value for the
# measure of academic performance, "api00":

x.low <- setx(z.out1, api00= quantile(apistrat$api00, 0.2))
x.high <- setx(z.out1, api00= quantile(apistrat$api00, 0.8))

# Generate first differences for the effect of high versus low "meals" 
# on the probability that a school will hold classes year round:

s.out1 <- sim(z.out1, x=x.low, x1=x.high)

# Summary of fitted model

summary(z.out1)

# Summary of simulated quantities of interest

summary(s.out1)

# Generate a second set of fitted values and a plot:

plot(s.out1)
