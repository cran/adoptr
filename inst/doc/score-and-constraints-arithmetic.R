## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse   = TRUE,
  comment    = "#>",
  fig.width  = 7,
  fig.height = 5
)

## ----setup---------------------------------------------------------------
library(adoptr)

## ----define-design-------------------------------------------------------
design <- TwoStageDesign(
    n1  = 100,
    c1f = .0,
    c1e = 2.0,
    n2_pivots = rep(150, 5),
    c2_pivots = sapply(1 + adoptr:::GaussLegendreRule(5)$nodes, function(x) -x + 2)
)

plot(design)

## ------------------------------------------------------------------------
uniform_prior <- ContinuousPrior(
  function(x) numeric(length(x)) + 1/.2,
  support = c(.3, .5)
)

cp <- ConditionalPower(Normal(), uniform_prior)

evaluate(cp, design, x1 = 0.5) 

## ------------------------------------------------------------------------
plot(design, "Conditional Power" = cp)

## ------------------------------------------------------------------------
ep <- expected(cp)

evaluate(ep, design)

## ------------------------------------------------------------------------
power <- expected(ConditionalPower(Normal(), PointMassPrior(.4, 1.0)))

## ------------------------------------------------------------------------
ess <- expected(ConditionalSampleSize(Normal(), uniform_prior))

## ----define-new-score----------------------------------------------------
# Define the class
setClass("ConditionalProbabilityEarlyFutility", contains = "ConditionalScore")

# Define constructor
ConditionalProbabilityEarlyFutility <- function(dist, prior) {
  new("ConditionalProbabilityEarlyFutility", distribution = dist, prior = prior)
} 

# Define corresponding evaluate method
setMethod("evaluate", signature("ConditionalProbabilityEarlyFutility", "TwoStageDesign"),
          function(s, design, x1, optimization = FALSE, ...) ifelse(x1 < design@c1f, 1, 0)
)

## ----p-cont--------------------------------------------------------------
prob_early_fut <- expected(ConditionalProbabilityEarlyFutility(
  Normal(), 
  PointMassPrior(.0, 1)
))

## ----evaluate-p-cont-----------------------------------------------------
evaluate(prob_early_fut, design)

## ----check---------------------------------------------------------------
pnorm(design@c1f)

## ----sum-----------------------------------------------------------------
evaluate(ess + 50*power, design)

## ----constraints---------------------------------------------------------
power >= .8

power <= ess

0.7 <= cp

