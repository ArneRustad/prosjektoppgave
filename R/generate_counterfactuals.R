#### Functions as input for slickEcr ####

#' Compute fitness values
#'
#' Returns a matrix of three rows, one for the distance of the prediction to
#' the desired target, one for the distance to x.interest and
#' one for the number of changed features.
#'
#' @section Arguments:
#' \describe{
#' \item{x: }{(data.frame)\cr Data frame of candidates.}
#' \item{x.interest: }{(data.frame)\cr Data frame with one row as instance to
#' be explained. }
#' \item{target: }{numeric(1)|numeric(2) \cr Desired outcome either a single numeric or
#' a vector of two numerics in order to define an interval as desired outcome.}
#' \item{predictor: }{(Predictor)\cr
#' The object that holds the machine learning model and the data.}
#' \item{range: }{(numeric)\cr Vector of ranges for numeric features.
#' Must have same ordering as columns in x and x.interest.}
#' }
#' @return (matrix)
#' #export (remove later?)
fitness_fun = function(x, x.interest, target, predictor, train.data, range = NULL,
                       track.infeas = TRUE, identical.strategy = FALSE, k = 1, weights = NULL) {
  checkmate::assertIntegerish(k, null.ok = FALSE)
  checkmate::assertDataFrame(x)
  checkmate::assertDataFrame(x.interest, nrows = 1, any.missing = FALSE)
  checkmate::assertNumeric(target, min.len = 1, max.len = 2)
  checkmate::assertNumeric(range, lower = 0, finite = TRUE, any.missing = TRUE,
                len = ncol(x.interest), null.ok = TRUE)
  checkmate::assertNumeric(weights, null.ok = TRUE, finite = TRUE, any.missing = FALSE,
                len = k)
  if (!(is.null(range)) && !(all(names(x.interest) == names(range)))) {
    stop("range for gower distance needs same order and feature names as
      'x.interest'")
  }

  if (any(grepl("use.orig", names(x)))) {
    x = x[, -grep("use.orig", x = names(x))]
  }
  checkmate::assert_data_frame(x, ncols = ncol(x.interest))

  if(!all(names(x) == names(x.interest))) {
    stop("x.interest and x need same column ordering and names")
  }
  equal.type = all(mapply(x, x.interest,
                          FUN = function(x1, x2) {class(x1) == class(x2)}))
  if (!equal.type) {
    stop("x.interest and x need same feature types")
  }

  # Objective Functions
  pred = predictor$predict(newdata = x)[[1]]
  q1 = vapply(pred, numeric(1), FUN =  function(x) min(abs(x - target)))
  q1 = ifelse(length(target) == 2 & (pred >= target[1]) & (pred <= target[2]),
              0, q1)
  q2 = StatMatch::gower.dist(data.x = x.interest, data.y = x, rngs = range,
                             KR.corr = FALSE)[1,]

  if (identical.strategy) {
    feature.types = predictor$data$feature.types
    x.interest.rep = x.interest[rep(row.names(x.interest), nrow(x)),]
    notid = x != x.interest.rep
    id.num = predictor$data$feature.types == "numerical"
    if (sum(id.num) > 0) {
      notid[, id.num] =  abs(x[, id.num] - x.interest.rep[, id.num]) >= sqrt(.Machine$double.eps)
    }
    q3 = rowSums(notid)
  } else {
    q3 = rowSums(x != x.interest[rep(row.names(x.interest), nrow(x)),])
  }
  if (track.infeas) {
    q4 = apply(StatMatch::gower.dist(data.x = train.data, data.y = x, rngs = range,
                                     KR.corr = FALSE), MARGIN = 2, FUN = function(dist) {
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
    fitness = mapply(function(a, b, c, d) {
      c(a, b, c, d)
    }, q1, q2, q3, q4)
  } else {
    fitness = mapply(function(a, b, c) {
      c(a, b, c)
    }, q1, q2, q3)
  }
  return(fitness)
}
