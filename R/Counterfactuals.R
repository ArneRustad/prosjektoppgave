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
                          track.infeas = TRUE, verbal = 1L) {

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
        private$set_x_interest(x.interest) # Remove later, unneeded, don't need to set x_interest twice
        # Not yet added function self$explain(x.interest = self$x.interest, target = self$target)
      }
    },
    explain = function(x.interest, target) {
      checkmate::assert_numeric(target, min.len = 1,
                                max.len = 2, any.missing = FALSE, null.ok = FALSE)
      if (all(sapply(target, is.infinite))) {
        stop("One element of target must be finite")
      }
      checkmate::assert_true
      self$target = target
      private$set_x_interest(x.interest)
      private$flush()
      private$run()
      return(self)
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
    flush = function() {
      private$ecrresults = NULL
      self$results = NULL
      private$finished = FALSE
    },
    # based on InterpretationMethod::run()
    # but own functionality necessary since results does not expect a list()
    run = function(force = FALSE, ...) {
      if (force) private$flush()
      if (!private$finished) {
        # Observed data
        private$dataSample <- private$getData()
        print(private$getData())
        # Get counterfactuals
        private$dataDesign = private$intervene()
        # Get predictions for counterfactuals
        private$qResults = self$predictor$predict(data.frame(private$dataDesign))
        # Get results list
        self$results = private$aggregate()
        private$finished <- TRUE
      }
    },
    intervene = function() {

      # Define reference point for hypervolume computation
      private$ref.point = c(min(abs(self$y.hat.interest - self$target)),
                            1, ncol(self$x.interest))
      private$obj.names = c("dist.target", "dist.x.interest", "nr.changed")
      n.objectives = 3L
      if (self$track.infeas) {
        private$ref.point = c(private$ref.point, 1)
        private$obj.names = c(private$obj.names, "dist.train")
        n.objectives = n.objectives + 1L
      }
      if (is.infinite(private$ref.point[1])) {
        pred = self$predictor$predict(self$predictor$data$get.x())
        private$ref.point[1] = diff(c(min(pred), max(pred)))
      }

      # Create fitness function with package smoof
      fn = smoof::makeMultiObjectiveFunction(
        has.simple.signature = FALSE, par.set = private$param.set,
        n.objectives = n.objectives,
        noisy = TRUE, ref.point = private$ref.point, # Question - why classify fn as noisy?
        fn = function(x, fidelity = NULL) {
          fitness_fun(x, x.interest = self$x.interest, target = self$target,
                      predictor = self$predictor, train.data = self$predictor$data$get.x(),
                      range = private$range, track.infeas = self$track.infeas,
                      k = self$k, weights = self$weights)
        })

      fn = mosmafs::setMosmafsVectorized(fn)

      # Initialize population based on x.interest, param.set

      # Strategy 1: Use sd to initialize numeric features (default FALSE)
      if (self$initialization == "sd" && length(private$sdev) > 0) {
        lower = self$x.interest[names(private$sdev)] - private$sdev
        upper = self$x.interest[names(private$sdev)] + private$sdev
        if (nrow(lower)>0 && nrow(upper)>0) {
          lower.ps = pmax(ParamHelpers::getLower(private$param.set), lower)
          upper.ps = pmin(ParamHelpers::getUpper(private$param.set), upper)
          lower.ps[names(self$lower)] = self$lower
          upper.ps[names(self$upper)] = self$upper
          private$param.set.init = ParamHelpers::makeParamSet(params = make_paramlist(
            self$predictor$data$get.x(),
            lower = lower.ps,
            upper = upper.ps))
        }
      } else {
        # Strategy 2: Randomly sample
        private$param.set.init = private$param.set
      }
      # Strategy 3: Use training data as first population
      if (self$initialization == "traindata") {
        train.data = as.data.frame(private$dataSample, stringsAsFactors = FALSE)

        train.data = train.data[head(sample.int(nrow(train.data)), 200), ]
        for (rx in seq_len(nrow(train.data))) {
          use.orig.feats = sample.int(length(self$x.interest), 1) - 1
          use.orig = seq_along(self$x.interest) <= use.orig.feats
          use.orig = sample(use.orig)
          train.data[rx, use.orig] = self$x.interest[use.orig]
        }
        fitness.train = fn(as.data.frame(train.data))
        if (nrow(train.data) > self$mu) {
          idx = select_nondom(fitness.train, self$mu, train.data , epsilon = self$epsilon,
                              extract.duplicates = TRUE)
          train.sub = train.data[idx,]
        } else {
          train.sub = train.data
        }
        # Initialize also a use.orig vector
        init.df = cbind(train.sub, use.orig = train.sub == self$x.interest[rep(row.names(self$x.interest),
                                                                               nrow(train.sub)),])
        init.df = BBmisc::convertDataFrameCols(init.df, factors.as.char = TRUE)
        initial.pop = ParamHelpers::dfRowsToList(init.df, par.set = private$param.set.init)
        # necessity to manually set factors to characters
        initial.pop = lapply(initial.pop, function(ipop) {
          new.ipop = lapply(ipop, function(i) {
            if(is.factor(i)) i = as.character(i)
            return(i)
          })
          return(new.ipop)
        })
        # Add random samples if number of training data obs < mu
        n.missing = self$mu - length(initial.pop)
        if (n.missing > 0) {
          additional.pop = ParamHelpers::sampleValues(private$param.set.init, n.missing,
                                                      discrete.names = TRUE)
          initial.pop = c(initial.pop, additional.pop)
        }
      } else {
        initial.pop = ParamHelpers::sampleValues(private$param.set.init, self$mu,
                                                 discrete.names = TRUE)
      }

      # Strategy 4: Use ice curve variance to initialize use.original vector
      # while features are initialized randomly
      if (self$initialization == "icecurve") {
        ice.var = get_ICE_var(self$x.interest, self$predictor, private$param.set)
        prob.use.orig = 1 - mlr::normalizeFeatures(as.data.frame(ice.var),
                                                   method = "range", range = c(0.01, 0.99))
        ilen = length(initial.pop[[1]]$use.orig)
        distribution = function() rbinom(n = ilen, size = ilen,
                                         prob = t(prob.use.orig))
        initial.pop = initSelector(initial.pop, vector.name = "use.orig",
                                   distribution = distribution)
      }
      i = sapply(self$x.interest, is.factor)
      x.interest = self$x.interest
      x.interest[i] = lapply(self$x.interest[i], as.character)

      initial.pop = lapply(initial.pop, function(x) {
        x = transform_to_orig(x, x.interest, delete.use.orig = FALSE,
                              fixed.features = self$fixed.features, max.changed = self$max.changed)
      })


      # Define operators based on parameterset private$param.set
      # Messages can be ignored (only hint that operator was initialized, although
      # no feature of the corresponding type exists)
      sdev.l = sdev_to_list(private$sdev, private$param.set)
      # Use mutator based on conditional distributions if functions are given
      # it is the case if self$predictor$conditionals is NOT logical
      if (is.logical(self$conditionals)) {
        single.mutator = suppressMessages(mosmafs::combine.operators(private$param.set,
                                                                     numeric = ecr::setup(mosmafs::mutGaussScaled, p = self$p.mut.gen, sdev = sdev.l$numeric),
                                                                     integer = ecr::setup(mosmafs::mutGaussIntScaled, p = self$p.mut.gen, sdev = sdev.l$integer),
                                                                     discrete = ecr::setup(mosmafs::mutRandomChoice, p = self$p.mut.gen),
                                                                     logical = ecr::setup(ecr::mutBitflip, p = self$p.mut.gen),
                                                                     use.orig = ecr::setup(mosmafs::mutBitflipCHW, p = self$p.mut.use.orig),
                                                                     .binary.discrete.as.logical = TRUE))
        mutator = ecr::makeMutator(function(ind) {
          transform_to_orig(single.mutator(ind), x.interest, delete.use.orig = FALSE,
                            fixed.features = self$fixed.features, max.changed = self$max.changed)
        }, supported = "custom")
      } else {
        single.mutator = suppressMessages(mosmafs::combine.operators(private$param.set,
                                                                     numeric = ecr::setup(mosmafs::mutGaussScaled, p = self$p.mut.gen, sdev = sdev.l$numeric),
                                                                     integer = ecr::setup(mosmafs::mutGaussIntScaled, p = self$p.mut.gen, sdev = sdev.l$integer),
                                                                     discrete = ecr::setup(mosmafs::mutRandomChoice, p = self$p.mut.gen),
                                                                     logical = ecr::setup(ecr::mutBitflip, p = self$p.mut.gen),
                                                                     use.orig = ecr::setup(mosmafs::mutBitflipCHW, p = self$p.mut.use.orig),
                                                                     .binary.discrete.as.logical = TRUE))

        mutator = ecr::makeMutator(function(ind) {
          # # Transform use.original
          ind$use.orig = as.logical(mosmafs::mutBitflipCHW(as.integer(ind$use.orig), p = self$p.mut.use.orig)) #SD
          # Transform as before
          ind = transform_to_orig(ind, x.interest, delete.use.orig = FALSE, #SD single.mutator(ind)
                                  fixed.features = self$fixed.features, max.changed = self$max.changed)
          ind.short = ind
          ind.short$use.orig = NULL
          # Select features to mutate:
          affect = NA
          affect = runif(length(ind.short)) < self$p.mut.gen
          cols.nams = names(ind.short)
          affected.cols = cols.nams[affect & !ind$use.orig]
          # Shuffle mutation order
          affected.cols = sample(affected.cols)
          if (length(affected.cols != 0)) {
            for (a in affected.cols) {
              X = data.table::as.data.table(data.frame(ind.short, stringsAsFactors = FALSE))
              single.mutator = suppressMessages(mosmafs::combine.operators(private$param.set,
                                                                           .params.group = c(a),
                                                                           group = ecr::setup(mutConDens, conditionals = self$conditionals,
                                                                                              X = X, pred = self$predictor, param.set = private$param.set),
                                                                           use.orig = mutInd, numeric = mutInd, integer = mutInd, discrete = mutInd,
                                                                           logical = mutInd, .binary.discrete.as.logical = FALSE))
              ind = single.mutator(ind)
            }
          }
          return(ind)
        }, supported = "custom")
      }

      recombinator = suppressMessages(mosmafs::combine.operators(private$param.set,
                                                                 numeric = ecr::setup(ecr::recSBX, p = self$p.rec.gen),
                                                                 integer = ecr::setup(mosmafs::recIntSBX, p = self$p.rec.gen),
                                                                 discrete = ecr::setup(mosmafs::recPCrossover, p = self$p.rec.gen),
                                                                 logical = ecr::setup(mosmafs::recPCrossover, p = self$p.rec.gen),
                                                                 use.orig = ecr::setup(mosmafs::recPCrossover, p = self$p.rec.use.orig),
                                                                 .binary.discrete.as.logical = TRUE))

      overall.recombinator <- ecr::makeRecombinator(function(inds, ...) {
        inds <- recombinator(inds)
        do.call(ecr::wrapChildren, lapply(inds, function(x) {
          transform_to_orig(x, x.interest, delete.use.orig = FALSE,
                            fixed.features = self$fixed.features, max.changed = self$max.changed)
        }))
      }, n.parents = 2, n.children = 2)

      parent.selector = mosmafs::selTournamentMO

      survival.selector = ecr::setup(select_nondom,
                                     epsilon = self$epsilon,
                                     extract.duplicates = TRUE)

      # Extract algorithm information with a log object
      log.stats = list(fitness = lapply(
        seq_len(n.objectives),
        function(idx) {
          list(min = function(x) min(x[idx, ]), mean = function(x) mean(x[idx, ]))
        }))
      names(log.stats$fitness) = sprintf("obj.%s", seq_len(n.objectives))
      log.stats$fitness = unlist(log.stats$fitness, recursive = FALSE)
      log.stats$fitness = c(log.stats$fitness,
                            list(
                              n.row = function(x) sum(ecr::nondominated(x))
                            ))

      # Compute counterfactuals
      ecrresults = mosmafs::slickEcr(fn, lambda = self$mu, population = initial.pop,
                                     mutator = mutator,
                                     recombinator = overall.recombinator, generations = self$generations,
                                     parent.selector = parent.selector,
                                     survival.strategy = select_diverse,
                                     survival.selector = survival.selector,
                                     p.recomb = self$p.rec,
                                     p.mut = self$p.mut, log.stats = log.stats)
      private$ecrresults = ecrresults
      return(private$evaluate(ecrresults))
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

