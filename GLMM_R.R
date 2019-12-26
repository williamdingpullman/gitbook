n = 1e3; p = 3; K = 4; sig = 10
set.seed(123)
## time varying covariates
Xl = vector('list', K)
for(k in 1:K) Xl[[k]] = matrix(rnorm(n*p), n,p)
## constant covariate
Z = rbinom(n, 2,0.2)
## random effects
U = rnorm(n)*sig
## fixed effects
etaX = sapply(Xl, rowSums)
## random errors
eps = matrix(rnorm(n*K), n,K)
## logit model
eta = etaX + U + eps
prb = 1/(1+exp(-eta))
D = 1*(matrix(runif(n*K),n,K)<prb)
Xs = Xl[[1]]
for(k in 2:K) Xs = rbind(Xs, Xl[[k]])
## GLMM model
library(lme4)
sid = rep(1:n, K)
## model fit with GLMMM (default to Laplace approximation)
a1 = glmer(c(D) ~ Xs + Z[sid] + (1|sid), family=binomial)