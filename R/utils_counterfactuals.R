###### Utils Counterfactual #####

#' Extract information about each feature from input dataset
#'
#' Results in a list as input for ParamHelpers::makeParamSet.
#'
#' @section Arguments:
#' \describe{
#' \item{input.data:}{(data.frame)\cr Training data}
#' \item{lower: }{numeric\cr Vector of minimal values for numeric features.
#' If NULL lower is extracted from input data specified in field 'data' of
#' 'predictor'.}
#' \item{upper: }{numeric\cr Vector of maximal values for numeric features.
#' If NULL upper is extracted from input data specified in field 'data' of
#' 'predictor'.}
#' }
#' @return (list)
#' @export
make_paramlist = function(input.data, lower = NULL, upper = NULL, use.orig = TRUE) {
  checkmate::assert_data_frame(input.data)
  checkmate::assert_numeric(lower, null.ok = TRUE)
  checkmate::assert_numeric(upper, null.ok = TRUE)

  checkmate::assert_true(all(names(lower) %in% names(input.data)))
  checkmate::assert_true(all(names(upper) %in% names(input.data)))

  ncol = ncol(input.data)

  l = lapply(colnames(input.data), function(colnam) {
    col = input.data[[colnam]]
    if (colnam %in% names(lower)) {
      l = lower[[colnam]]
    } else {
      l =  tryCatch(min(col, na.rm = TRUE), error = function(err) NA)
    }
    if (colnam %in% names(upper)) {
      u = upper[[colnam]]
    }
    else {
      u = tryCatch(max(col, na.rm = TRUE), error = function(err) NA)
    }

    if (is.double(col)) {
      ParamHelpers::makeNumericParam(colnam, lower = l, upper = u)
    }

    else if (is.integer(col)) {
      ParamHelpers::makeIntegerParam(colnam, lower = l, upper = u)
    }

    else {
      if (is.character(col)) {
        values = unique(col)
      } else {
        values = char_to_factor(levels(col))
      }
      ParamHelpers::makeDiscreteParam(colnam, values = values)
    }
  })
  if (use.orig) {
    l[[length(l)+1]] = ParamHelpers::makeLogicalVectorParam("use.orig", len = ncol)
  }
  return(l)
}

#' Transform features to x.interest
#'
#' Replace features of x by value of x.interest
#' where use.orig is set to TRUE.
#'
#' @section Arguments:
#' \describe{
#' \item{x: }{(list)\cr List of features, must have list element
#' use.origin.}
#' \item{x.interest: }{(data.frame)\cr Data frame with one row.}
#' \item{delete.use.orig: }{(logical(1))\cr Whether to
#' delete list element use.orig of x.}
#' \item{fixed.features: }{(character)\cr
#' Indicate fixed features by feature name.}
#' \item{max.changed: }{numeric(1)\cr Number indicating
#' how many feature are allowed to maximially differ from the original data point.}
#' }
#'
#' @return (list)
transform_to_orig = function(x, x.interest, delete.use.orig = FALSE,
                             fixed.features = NULL, max.changed = NULL) {
  checkmate::assert_list(x, len = ncol(x.interest) + 1)
  checkmate::assert_data_frame(x.interest, any.missing = FALSE, nrows = 1)
  checkmate::assert(
    check_character(fixed.features, null.ok = TRUE),
    check_true(fixed.features %in% names(x))
  )
  checkmate::assert_integerish(max.changed, null.ok = TRUE)
  types = lapply(x[names(x)!="use.orig"], class)

  if (!is.null(fixed.features)) {
    pos = which(names(x) %in% fixed.features)
    x$use.orig[pos] = TRUE
  }

  use.orig = x$use.orig
  if (!is.null(max.changed)) {
    n.changed = sum(!use.orig)
    if (n.changed > max.changed) {
      n = n.changed - max.changed
      mut.idx = sample(which(!use.orig), n)
      use.orig[mut.idx] = TRUE
    }
  }
  x$use.orig = NULL
  x[use.orig] = x.interest[use.orig]
  types.after.trans = lapply(x, class)
  if (!identical(types, types.after.trans)) {
    stop("setting values to x.interest values introduced a type shift")
  }
  if (!delete.use.orig) {
    x$use.orig = use.orig
  }
  return(x)
}


#' Transmit levels of factor variable to parameter set
#'
#' @section Arguments:
#' \describe{
#' \item{levels: }{(character)\cr Character vector of feature class labels.}
#' }
char_to_factor= function(levels){
  sapply(as.character(levels), function(x)
    factor(x, levels=levels),
    simplify = FALSE)
}
