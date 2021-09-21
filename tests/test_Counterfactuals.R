library(prosjektoppgave)
n = 1000
a = rnorm(n, 5, 5)
b = runif(n, 0, 10)
c = runif(n, 0, 10)
d = sample(c("fac1", "fac2"), n, replace = TRUE)
epsilon = rnorm(n)
y = 5*a + b + epsilon + 10 * (d == "fac1")
df = data.table(y = y, a = a, b = b, c = c, d = factor(d))

fit = lm(y ~ a + b + c + d)
mod = Predictor$new(fit, data = df, predict.function = predict.lm)


cf = Counterfactuals$new(predictor = mod,
                         x.interest = mod$data$X[2,],
                         target = 21,
                         fixed.features = c(1,2))

cond = prosjektoppgave::fit_conditionals(cf$predictor$data$get.x())
cond$a

params = make_paramlist(df)
params
param.set = ParamHelpers::makeParamSet(params = params)
param.set

ParamHelpers::getLower(param.set)
ParamHelpers::getUpper(param.set)
ParamHelpers::getParamIds(param.set)
ParamHelpers::getParamTypes(param.set)

apply(Filter(is.numeric, cf$predictor$data$get.x()), 2, sd)

cf$predictor$data$get.x()
cf$x.interest

# Testing functions for conditionals
conditionals = fit_conditionals(df)
conditionals$y$csample(as.data.table(df[1,]), size = 1, type = "data")

conditionals$a$cdens(df[1,], seq(0,10,length.out = 100))

conditionals$a$csample(as.data.table(df[,]), size = 1, type = "parametric")
df$a
