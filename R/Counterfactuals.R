#'@export
Counterfactuals = R6::R6Class(
  "Counterfactuals",
  inherit = InterpretationMethod,
  public = list(
    predictor = NULL,
    x.interest = NULL,
    y.hat.interest = NULL,
    target = NULL,
    epsilon = NULL,
    fixed.features = NULL,
    max.changed = NULL,
    mu = NULL,
    generations  = NULL,
    p.mut  = NULL,
    p.rec  = NULL,
    p.mut.gen  = NULL,
    p.mut.use.orig = NULL,
    p.rec.gen  = NULL,
    p.rec.use.orig = NULL,
    k = NULL,
    weights = NULL,
    track.infeas = NULL,
    initialization = NULL,
    lower = NULL,
    upper = NULL,
    log = NULL,
    conditionals = NULL,
    results = NULL,
    verbal = NULL, #added for increased functionality and easier debugging
    initialize = function(predictor, x.interest = NULL, target = NULL,
                          epsilon = NULL, fixed.features = NULL, max.changed = NULL,
                          mu = 20, generations = 175, p.rec = 0.57, p.rec.gen = 0.85, p.rec.use.orig = 0.88,
                          p.mut = 0.79, p.mut.gen = 0.56, p.mut.use.orig = 0.32, k = 1L, weights = NULL,
                          lower = NULL, upper = NULL, conditionals = TRUE, initialization = "icecurve",
                          track.infeas = TRUE, verbal = 1) {

      super$initialize(predictor = predictor)
      print(fixed.features)
      fixed.features = private$sanitize_feature(fixed.features, predictor$data$feature.names)
      print(fixed.features)
      # can be missing
      checkmate::assert_data_frame(x.interest, null.ok = TRUE)
      checkmate::assert_numeric(target, null.ok = TRUE, min.len = 1,
                                max.len = 2, any.missing = FALSE)
      if (!is.null(target) && all(sapply(target, is.infinite))) {
        stop("One element of target must be finite")
      }
      checkmate::assert_number(epsilon, null.ok = TRUE)
      checkmate::assert_integerish(max.changed, null.ok = TRUE, len = 1)
      checkmate::assert_numeric(lower, null.ok = TRUE, any.missing = FALSE)
      checkmate::assert_numeric(upper, null.ok = TRUE, any.missing = FALSE)

      # should exist
      checkmate::assert_integerish(mu, lower = 1)
      checkmate::assert(
        checkmate::checkInt(generations, lower = 0),
        checkmate::checkList(generations, types = "function")
      )
      checkmate::assert_number(p.mut, lower = 0, upper = 1)
      checkmate::assert_number(p.rec, lower = 0, upper = 1)
      checkmate::assert_number(p.mut.gen, lower = 0, upper = 1)
      checkmate::assert_number(p.rec.gen, lower = 0, upper = 1)
      checkmate::assert_number(p.mut.use.orig, lower = 0, upper = 1)
      checkmate::assert_number(p.rec.use.orig, lower = 0, upper = 1)
      checkmate::assert_integerish(k, lower = 1)
      checkmate::assert_numeric(weights, null.ok = TRUE, lower = 0, upper = 1, len = k)
      checkmate::assert_logical(track.infeas)
      checkmate::assert_character(initialization)
      checkmate::assert_true(initialization %in% c("random", "sd", "icecurve", "traindata"))

      checkmate::assert_integer(verbal, lower = 0, upper = 2)


      # assign
      self$target = target
      self$epsilon = epsilon
      self$max.changed = max.changed
      self$fixed.features = fixed.features
      self$mu = mu
      self$generations = generations
      self$p.mut = p.mut
      self$p.rec = p.rec
      self$p.mut.gen = p.mut.gen
      self$p.mut.use.orig = p.mut.use.orig
      self$p.rec.gen = p.rec.gen
      self$p.rec.use.orig = p.rec.use.orig
      self$k = k
      self$lower = lower
      self$upper = upper
      self$track.infeas = track.infeas
      self$initialization = initialization

      self$verbal = verbal

      print(paste("Conditionals =", conditionals))

      # fit conditionals if conditionals != false
      if (is.logical(conditionals) && conditionals) {
        # TODO: Investigate this function further
        if (verbal >= 2) print("Fitting conditional distribution for each feature")
        conditionals = fit_conditionals(self$predictor$data$get.x(),
                                        ctrl = partykit::ctree_control(maxdepth = 5L))
        if (verbal >= 2) print("Finished fitting conditional distributions") # Perhaps change this into a bar
      }
      print("Conditionals fitted if needed")
      self$conditionals = conditionals
      # Check if column names of x.interest and observed data are identical
      if(!is.null(x.interest) && any(!(self$predictor$data$feature.names %in% colnames(x.interest)))) {
        stop("colnames of x.interest must be identical to observed data")
      }


      # Define parameterset
      private$param.set= ParamHelpers::makeParamSet(
        params = make_paramlist(rbind(predictor$data$get.x(), x.interest[predictor$data$feature.names]),
                                lower = lower, upper = upper))
      print(make_paramlist(rbind(predictor$data$get.x(), x.interest[predictor$data$feature.names]),
                           lower = lower, upper = upper))
      print(private$param.set)

      # Extract info from input.data
      private$range = ParamHelpers::getUpper(private$param.set) -
        ParamHelpers::getLower(private$param.set)
      private$range[ParamHelpers::getParamIds(private$param.set)
                    [ParamHelpers::getParamTypes(private$param.set) == "discrete"]]  = NA
      private$range = private$range[predictor$data$feature.names]
      private$sdev = apply(Filter(is.numeric, predictor$data$get.x()), 2, sd)

      # Set x.interest
      if (!is.null(x.interest) & !is.null(target)) {
        private$set_x_interest(x.interest) # Remove later, unneeded
        # Not yet added function self$explain(x.interest = self$x.interest, target = self$target)
      }
    }
  ),
  private = list(
    #featurenames = NULL,
    dataDesign = NULL,
    range = NULL,
    ref.point = NULL,
    sdev = NULL,
    param.set= NULL,
    param.set.init = NULL,
    ecrresults = NULL,
    obj.names = NULL,
    finished = NULL,
    qResults = NULL,
    set_x_interest = function(x.interest) {
      checkmate::assert_data_frame(x.interest, any.missing = FALSE, all.missing = FALSE,
                        nrows = 1, null.ok = FALSE)
      x.interest = x.interest[setdiff(colnames(x.interest), self$predictor$data$y.names)]
      if (any(colnames(x.interest) != self$predictor$data$feature.names)) {
        warning("columns of x.interest were reordered according to predictor$data$feature.names")
        x.interest = x.interest[, self$predictor$data$feature.names]
      }
      x.interest.list = as.list(x.interest)
      x.interest.list$use.orig = rep(TRUE, ncol(x.interest))
      if (!ParamHelpers::isFeasible(private$param.set, x.interest.list)) {
        stop(paste("Feature values of x.interest outside range of observed data",
                   "of predictor or given arguments lower or upper. Please modify arguments",
                   "lower or upper accordingly."))
      }
      self$y.hat.interest = self$predictor$predict(x.interest)[1,]
      self$x.interest = x.interest
    },
    sanitize_feature = function(fixed.features, feature.names) {
      if (is.numeric(fixed.features)) {
        checkmate::assert_integerish(fixed.features, lower = 1, upper = length(feature.names),
                          null.ok = TRUE)
        fixed.features = feature.names[fixed.features]
      }
      checkmate::assert_character(fixed.features, null.ok = TRUE, unique = TRUE)
      stopifnot(all(fixed.features %in% feature.names))
      fixed.features
    }
  )
)

