library(survey.zelig)

data(api, package="survey")

z.out1 <- zelig(
                yr.rnd ~ meals + mobility,
                model = "probit.survey",
                weights=~pw,
                data = apistrat
                )

summary(z.out1)

x.low <- setx(z.out1, meals= quantile(apistrat$meals, 0.2))
x.high <- setx(z.out1, meals= quantile(apistrat$meals, 0.8))

x.low
x.high

s.out1 <- sim(z.out1, x=x.low, x1=x.high)


plot(s.out1)


# TEST 2
z.out2 <- zelig(
                yr.rnd ~ meals + mobility,
                model = "probit.survey",
                strata=~stype,
                fpc=~fpc,
                data = apistrat
                )

summary(z.out2)


data(scd)

scd$sued <- as.vector(c(0,0,0,1,1,1))

BRRrep<-2*cbind(
                c(1,0,1,0,1,0),
                c(1,0,0,1,0,1),
                c(0,1,1,0,0,1),
                c(0,1,0,1,1,0)
                )

z.out3 <- zelig(
                formula=sued ~ arrests + alive,
                model = "probit.survey", 
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
