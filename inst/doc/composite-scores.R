## ----include = FALSE------------------------------------------------
knitr::opts_chunk$set(
  collapse   = TRUE,
  comment    = "#>",
  fig.width  = 7,
  fig.height = 5
)

## ----setup----------------------------------------------------------
library(adoptr)

## -------------------------------------------------------------------
H_0      <- PointMassPrior(.0, 1)
H_1      <- PointMassPrior(.2, 1)
datadist <- Binomial(.1, two_armed = FALSE)

ess   <- ExpectedSampleSize(datadist, H_1)
power <- Power(datadist, H_1)
toer  <- Power(datadist, H_0)

## ----sum------------------------------------------------------------
objective <- composite({ess - 50*power})

## -------------------------------------------------------------------
design <- TwoStageDesign(
    n1  = 100,
    c1f = .0,
    c1e = 2.0,
    n2_pivots = rep(150, 5),
    c2_pivots = sapply(1 + adoptr:::GaussLegendreRule(5)$nodes, function(x) -x + 2)
)

evaluate(objective, design)

## -------------------------------------------------------------------
cp  <- ConditionalPower(datadist, H_1)
css <- ConditionalSampleSize()

cs  <- composite({css - 50*cp})

## -------------------------------------------------------------------
evaluate(cs, design, c(0, .5, 1))

## -------------------------------------------------------------------
evaluate(expected(cs, datadist, H_1), design)

## -------------------------------------------------------------------
cs <- composite({log(css) - 50*sin(cp)})
evaluate(cs, design, c(0, .5, 1))

## -------------------------------------------------------------------
cs <- composite({
  res <- 0
  for (i in 1:3) {
    res <- res + css
  }
  res
})
evaluate(cs, design, c(0, .5, 1))

