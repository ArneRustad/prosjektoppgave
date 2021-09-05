library(data.table)
n = 1000
a = rnorm(n, 5, 5)
b = runif(n, 0, 10)
c = runif(n, 0, 10)

y = 5*a + b + 0.01*c
df = data.table(y = y, a = a, b = b, c = c)
df

fit = lm(y ~ a + b + c)

library(iml)

mod = Predictor$new(fit, data = df, predict.function = predict.lm)

mod$print()

View(mod$data)

mod$data$X
