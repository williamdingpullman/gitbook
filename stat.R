library(bild)
head(locust,40)
locust2 <- bild(move ~ (time + I(time^2)) * feed, data = locust,start = NULL, aggregate = feed, dependence = "MC2")

muscatine$time1 <- c(-1, 0, 1)
muscatine$time2 <- c(1, -2, 1)

Integ <- bildIntegrate(lig = -3.95, lsg = 3.95)
musc2r <- bild(obese ~ (time1 + time2) * sex, data = muscatine,time = "time1", start = c(1, 1, 1), dependence = "MC2R",integrate = Integ)
summary(musc2r)


musc1r <- bild(obese ~ time1, data = muscatine, time = "time1",start = c(1, 1), dependence = "MC1R")
summary(musc1r)

apply(mtcars,2,mean)


library(splines)
## Data from Alvarez et. al. (1996)
data(aclp)
newdat <- btscs(aclp, "democ", "year", "country")
# Estimate Model with and without spell
full.mod <- glm(democ ~ log(gdpw) + popg + bs(spell, df=4), data=newdat, family=binomial)
restricted.mod <- glm(democ ~ log(gdpw) + popg, data=newdat, family=binomial)
# Incremental F-test of time dependence
anova(restricted.mod, full.mod, test='Chisq')
