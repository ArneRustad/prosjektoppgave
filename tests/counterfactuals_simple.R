library(data.table)
library()
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
    evaluate = function(x.interest = self$x.interest, target = self$target) {
      print(self$x.interest)
      print(self$predictor$predict(x.interest))
      print(target)
      cols = colnames(x.interest)
      fr = function(x, cols) {
        x.dt = as.data.table(matrix(x, nrow = 1, ncol = 3, dimnames = list(NULL, cols) ))
        return(abs(self$predictor$predict(x.dt) - self$target))
      }
      result = optim(as.vector(x.interest),
                     fr, NULL, cols)
      result$par
    }
  )
)

cf.simple = simpleCounterfactuals$new(mod, mod$data$X[2,], 21)
cf.simple$evaluate()


