library(data.table)
n = 1000
a = rnorm(n, 5, 5)
b = runif(n, 0, 10)
c = runif(n, 0, 10)
epsilon = rnorm(n)
y = 5*a + b + epsilon
df = data.table(y = y, a = a, b = b, c = c)
df

fit = lm(y ~ a + b + c)
fit
library(iml)
library(R6)
mod = Predictor$new(fit, data = df, predict.function = predict.lm)

simpleCounterfactuals = R6::R6Class(
  classname = "simpleCounterfactuals",
  inherit = InterpretationMethod,
  public = list(
    predictor = NULL,
    x.interest = NULL,
    target = NULL,
    result = NULL,
    initialize = function(predictor, x.interest = NULL, target = NULL) {
      super$initialize(predictor = predictor)
      # sanitize features
      checkmate::assert_data_frame(x.interest, null.ok = TRUE)
      checkmate::assert_numeric(target, null.ok = TRUE, min.len = 1, max.len = 2, any.missing = FALSE)
      self$x.interest = x.interest
      self$target = target
    },
    evaluate = function(x.interest = self$x.interest) {
      if (missing(x.interest)) {
        self$x.interest = x.interest
      } else {
        x.interest = self$x.interest
      }
      result = optim(x.interest,
                     function(x) return(abs(self$predictor$predict(x) - self$target)))
      result
    }
  )
)

cf.simple = simpleCounterfactuals$new(mod, mod$data$X[1,])
cf.simple$x.interest
cf.simple$evaluate()
mod$predict.function(mod$data$X[1,])


?R6Class
mod$print()

View(mod$data)

mod$data$X

?arrange

?remove
?cut

?dplyr::
