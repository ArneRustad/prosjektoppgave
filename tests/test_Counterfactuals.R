library(prosjektoppgave)
n = 200
e = sample(1:5, n, replace = TRUE, prob = 1:5)
a = rnorm(n, e, 1)
b = rnorm(n, e*2, 5)
c = runif(n, 0, 10)
d = sample(c("fac1", "fac2"), n, replace = TRUE)
epsilon = rnorm(n)
y = 5*a + b + epsilon + 10 * (d == "fac1") + 2*e

df = data.table(y = y, a = a, b = b, c = c, d = factor(d), e = e)
df.x = df[, colnames(df) != "y"]

fit = lm(y ~ a + b + c + d)
mod = Predictor$new(fit, data = df, predict.function = predict.lm)


cf = Counterfactuals$new(predictor = mod,
                         fixed.features = c(1,2))
cf$explain(x.interest = mod$data$X[2,],
           target = 21)

cond = prosjektoppgave::fit_conditionals(cf$predictor$data$get.x())
cond$a

params = make_paramlist(df.x)
params

params.wo.orig = make_paramlist(df.x, use.orig = FALSE)

param.set = ParamHelpers::makeParamSet(params = params.wo.orig)
param.set

ParamHelpers::getLower(param.set)
ParamHelpers::getUpper(param.set)
range = rep(NA, ncol(df.x))
names(range) = colnames(df.x)
range[ParamHelpers::getParamTypes(param.set) %in% c("numeric", "integer")] = ParamHelpers::getUpper(param.set) - ParamHelpers::getLower(param.set)
ParamHelpers::getParamIds(param.set)
ParamHelpers::getParamTypes(param.set)
range
apply(Filter(is.numeric, cf$predictor$data$get.x()), 2, sd)

cf$predictor$data$get.x()
cf$x.interest

# Testing functions for conditionals
conditionals = fit_conditionals(df)
conditionals$e$model
conditionals$e$cnode(as.data.table(df[1,]))
conditionals$d$csample(as.data.table(df[1,]), size = 1, type = "data")

conditionals$a$cdens(df[1,], seq(0,10,length.out = 100))

conditionals$e$csample(as.data.table(df[,]), size = 1, type = "parametric")
df$a

# Testing fitness function
x.try = data.frame(a = 4, b = 5, c = 6, d = factor("fac1", levels = c("fac1", "fac2")), e = 5L)
fitness_fun(x.try, df.x[2,], predictor = mod, target = 21, train.data =df.x, range = range)

print(x.try)
print(df.x[2,])

x.try.multiple = data.frame(a = c(4,5), b = c(5,5), c = c(2,3), d = factor(c("fac1", "fac2")), e = c(5L, 2L))
fitness_fun(x.try.multiple, df.x[2,], predictor = mod, target = 21, train.data =df.x, range = range)

StatMatch::gower.dist(data.x = df.x, data.y = x.try, rngs = range,
                      KR.corr = FALSE)
apply(StatMatch::gower.dist(data.x = df.x, data.y = x.try, rngs = range, KR.corr = FALSE),
      MARGIN = 2, FUN = function(dist, k = 1, weights = NULL) {
        d = sort(dist)[1:k]
        if (length(d) == k) {
          if (!is.null(weights)) {
            d = weighted.mean(d, w = weights)
          } else {
            d = mean(d)
          }
        }
        return(d)
      })

?apply
