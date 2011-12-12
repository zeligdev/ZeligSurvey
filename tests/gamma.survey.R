library(ZeligSurvey)

data(api)

# TEST 1
z.out1 <- zelig(api00 ~ meals + yr.rnd,
                model   = "gamma.survey",  
                weights = ~pw,
                data    = apistrat
                )

summary(z.out1)

x.low <- setx(z.out1, meals= quantile(apistrat$meals, 0.2))
x.high <- setx(z.out1, meals= quantile(apistrat$meals, 0.8))

x.low
x.high

s.out1 <- sim(z.out1, x=x.high, x1=x.low)

plot(s.out1)

# TEST 2
z.out2 <- zelig(
                api00 ~ meals + yr.rnd,
                model = "gamma.survey",  
                strata=~stype,
                fpc=~fpc,
                data = apistrat
                )

summary(z.out2)

jk1reps <- jk1weights(psu=apistrat$dnum)

z.out3 <- zelig(
                api00 ~ meals + yr.rnd,
                model = "gamma.survey", 
		data = apistrat,
                repweights=jk1reps$weights,
		type="JK1"
                )

summary(z.out3)

x.low <- setx(z.out3, meals= quantile(apistrat$meals, 0.2))
x.high <- setx(z.out3, meals= quantile(apistrat$meals, 0.8))

x.low
x.high

s.out3 <- sim(z.out3, x=x.high, x1=x.low)


plot(s.out3)
