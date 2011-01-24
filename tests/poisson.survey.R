library(survey.Zelig)

data(api, package="survey")

# TEST 1
z.out1 <- zelig(enroll ~ api99 + yr.rnd , model = "poisson.survey", data = apistrat)
summary(z.out1)

x.low <- setx(z.out1, api00= quantile(apistrat$api00, 0.2))
x.high <- setx(z.out1, api00= quantile(apistrat$api00, 0.8))

x.low
x.high

s.out1 <- sim(z.out1, x=x.low, x1=x.high)

summary(s.out1)

plot(s.out1)


# TEST 2
z.out2 <- zelig(
                enroll ~ api99 + yr.rnd,
                model = "poisson.survey",
                data = apistrat, 
                strata=~stype,
                fpc=~fpc
                )

summary(z.out2)

data(scd, package="survey")

BRRrep<-2*cbind(
                c(1,0,1,0,1,0),
                c(1,0,0,1,0,1),
                c(0,1,1,0,0,1),
                c(0,1,0,1,1,0)
                )

z.out3 <- zelig(
                alive ~ arrests,
                model = "poisson.survey", 
                repweights=BRRrep,
                type="BRR",
                data=scd
                )

summary(z.out3)

x.low <- setx(z.out3, arrests = quantile(scd$arrests, .2))
x.high <- setx(z.out3, arrests = quantile(scd$arrests,.8))

x.low
x.high

s.out3 <- sim(z.out3, x=x.high, x1=x.low)

plot(s.out3)
