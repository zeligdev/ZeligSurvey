library(survey.zelig)

data(api)

# TEST 1
z.out1 <- zelig(
                api00 ~ meals + yr.rnd,
                model = "normal.survey",  
                weights=~pw,
                data = apistrat
                )

summary(z.out1)

x.low <- setx(z.out1, meals= quantile(apistrat$meals, 0.2))
x.high <- setx(z.out1, meals= quantile(apistrat$meals, 0.8))

x.low
x.high

s.out1 <- sim(z.out1, x=x.high, x1=x.low)

plot(s.out1)


z.out2 <- zelig(
                api00 ~ meals + yr.rnd,
                model = "normal.survey",  
                strata=~stype,
                fpc=~fpc,
                data = apistrat
                )

summary(z.out2)

# TEST 2
data(scd)

BRRrep<-2 * cbind(
                  c(1,0,1,0,1,0),
                  c(1,0,0,1,0,1),
                  c(0,1,1,0,0,1),
                  c(0,1,0,1,1,0)
                  )

z.out3 <- zelig(
                formula=alive ~ arrests,
                model = "normal.survey", 
                repweights=BRRrep,
                type="BRR",
                data=scd,
                na.action=NULL
                )

summary(z.out3)

x.min <- setx(z.out3, arrests = min(scd$alive))
x.max <- setx(z.out3, arrests = max(scd$alive))

x.min
x.max

s.out3 <- sim(z.out3, x=x.max, x1=x.min)

plot(s.out3)
