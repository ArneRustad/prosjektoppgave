### Example: Stratified Medicine Using Partitioned Cox-Models
### A combination of <DOI:10.1515/ijb-2015-0032> and <arXiv:1701.02110>
### based on infrastructure in the mlt R add-on package described in
### https://cran.r-project.org/web/packages/mlt.docreg/vignettes/mlt.pdf

library("trtf")
library("survival")
### German Breast Cancer Study Group 2 data set
data("GBSG2", package = "TH.data")

### set-up Cox model with overall treatment effect in hormonal therapy
yvar <- numeric_var("y", support = c(100, 2000), bounds = c(0, Inf))
By <- Bernstein_basis(yvar, order = 5, ui = "incre")
m <- ctm(response = By, shifting = ~ horTh, todistr = "MinExt", data = GBSG2)
GBSG2$y <- with(GBSG2, Surv(time, cens))

### overall log-hazard ratio
coef(cmod <- mlt(m, data = GBSG2))["horThyes"]
### roughly the same as
coef(coxph(y ~ horTh, data = GBSG2))

### partition the model, ie both the baseline hazard function AND the
### treatment effect
(part_cmod <- trafotree(m, formula = y ~ horTh | age + menostat + tsize +
                          tgrade + pnodes + progrec + estrec, data = GBSG2))

predict(part_cmod, newdata = GBSG2[1:5,], type = "logdensity", prob = c(0.1, 0.9))

### compare the log-likelihoods
logLik(cmod)
logLik(part_cmod)

### stronger effects in nodes 2 and 4 and no effect in node 5
coef(part_cmod)[, "horThyes"]

### plot the conditional survivor functions; blue is untreated
### and green is hormonal therapy
nd <- data.frame(horTh = sort(unique(GBSG2$horTh)))
plot(part_cmod, newdata = nd,
     tp_args = list(type = "survivor", col = c("cadetblue3", "chartreuse4")))

### same model, but with explicit intercept term and max-type statistic
### for _variable_ selection
K <- diag(length(coef(m)) - 1)
K[upper.tri(K)] <- 1
K <- cbind(rbind(K, 0), 0)
K[nrow(K), nrow(K)] <- 1
### horThyes is not touched, 6th parameter is intercept
coef(cmod)
(part_cmod_max <- trafotree(m, formula = y ~ horTh | age + menostat + tsize +
                              tgrade + pnodes + progrec + estrec, data = GBSG2, reparm = K,
                            control = ctree_control(teststat = "max")))
logLik(part_cmod_max)

### the trees (and log-likelihoods are the same) but the
### p-values are sometines much smaller in the latter tree
cbind(format.pval(info_node(node_party(part_cmod))$criterion["p.value",]),
      format.pval(info_node(node_party(part_cmod_max))$criterion["p.value",]))


n = 1000
a = rnorm(n, 5, 5)
b = runif(n, 0, 10)
c = runif(n, 0, 10)
d = sample(c("fac1", "fac2"), n, replace = TRUE)
epsilon = rnorm(n)
y = 5*a + b + epsilon + 10 * (d == "fac1")
df = data.table(y = y, a = a, b = b, c = c, d = factor(d))

yvar <- numeric_var("y", support = c(-10, 10), bounds = c(-Inf, Inf))
By <- Bernstein_basis(yvar, order = 5, ui = "incre")
m <- ctm(response = By, todistr = "Normal", data = df)
form = as.formula(sprintf("%s ~ 1 | .", "y"))
part_cmod = trafotree(m, formula = form,  data = df)
predict(part_cmod, newdata = df[1:5,], type = "quantile", prob = 0.1)
