set.seed(123)
n.groups <- 4 # number of groups
n.repeats <- 2 # samples per group
#Generating index for observations belong to the same group
groups <- as.factor(rep(1:n.groups, each=n.repeats))
n <- length(groups)
#Generating 4 random numbers, assuming normal distribution
z0 <- rnorm(n.groups, 0, 10) 
z <- z0[as.numeric(groups)] # generate and inspect random group effects
z

epsilon <- rnorm(n,0,1) # generate measurement error
beta0 <- 2 # this is the actual parameter of interest! The global mean.
y <- beta0 + z + epsilon # sample from an LMM

lm.5 <- lm(y~1)

library(lme4)

lme.5.a <- lmer(y~1+(1|groups)) 
summary(lm.5)
summary(lme.5.a)


str(sleepstudy)
fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
summary(fm1)

fm2 <- lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
summary(fm2)
