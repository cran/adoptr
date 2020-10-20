## ---- include = FALSE-----------------------------------------------
knitr::opts_chunk$set(
  collapse   = TRUE,
  comment    = "#>",
  fig.width  = 7,
  fig.height = 5
)

## ----setup----------------------------------------------------------
library(adoptr)

## ----define-design--------------------------------------------------
design <- TwoStageDesign(
    n1        = 100,
    c1f       = .0,
    c1e       = 2.0,
    n2_pivots = rep(150, 5),
    c2_pivots = sapply(1 + adoptr:::GaussLegendreRule(5)$nodes, function(x) -x + 2)
)
plot(design)

## -------------------------------------------------------------------
uniform_prior <- ContinuousPrior(
  function(x) numeric(length(x)) + 1/.2,
  support = c(.3, .5)
)

cp  <- ConditionalPower(Normal(), uniform_prior)
css <- ConditionalSampleSize()

x1  <- c(0, .5, 1)
evaluate(cp, design, x1)
evaluate(css, design, x1)

## -------------------------------------------------------------------
plot(design, "Conditional Power" = cp)

## -------------------------------------------------------------------
ep <- expected(cp, Normal(), uniform_prior)
evaluate(ep, design)

## -------------------------------------------------------------------
power1 <- expected(
  ConditionalPower(Normal(), PointMassPrior(.4, 1.0)),
  Normal(), PointMassPrior(.4, 1.0)
)
power2 <- Power(Normal(), PointMassPrior(.4, 1.0))

evaluate(power1, design)
evaluate(power2, design)

## -------------------------------------------------------------------
ess1 <- expected(ConditionalSampleSize(), Normal(), uniform_prior)
ess2 <- ExpectedSampleSize(Normal(), uniform_prior)

evaluate(ess1, design)
evaluate(ess2, design)

## -------------------------------------------------------------------
cp >= 0.7

## -------------------------------------------------------------------
cp >= ConditionalPower(Normal(), PointMassPrior(0, 1))

