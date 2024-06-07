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
setClass("FutilityStopping", contains = "ConditionalScore")

# constructor
FutilityStopping <- function() new("FutilityStopping")

## -------------------------------------------------------------------
setMethod("evaluate", signature("FutilityStopping", "TwoStageDesign"),
          function(s, design, x1, optimization = FALSE, ...) 
              ifelse(x1 < design@c1f, 1, 0)
)

## ----p-cont---------------------------------------------------------
pr_early_futility <- expected(
  FutilityStopping(), 
  Normal(), PointMassPrior(.0, 1)
)

## ----define-design--------------------------------------------------
design <- TwoStageDesign(
    n1  = 100,
    c1f = .0,
    c1e = 2.0,
    n2_pivots = rep(150, 5),
    c2_pivots = sapply(1 + adoptr:::GaussLegendreRule(5)$nodes, function(x) -x + 2)
)

plot(design)

## ----evaluate-p-cont------------------------------------------------
evaluate(pr_early_futility, design)

## ----check----------------------------------------------------------
pnorm(design@c1f)

