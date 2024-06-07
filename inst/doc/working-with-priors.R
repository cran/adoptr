## ----include = FALSE------------------------------------------------
knitr::opts_chunk$set(
  collapse   = TRUE,
  comment    = "#>",
  fig.width  = 7,
  fig.height = 5
)

## ----setup----------------------------------------------------------
library(adoptr)

datadist <- Normal()

## ----discrete-prior-------------------------------------------------
disc_prior <- PointMassPrior(c(0.1, 0.25), c(0.4, 0.6))

## -------------------------------------------------------------------
cont_prior <- ContinuousPrior(
  pdf     = function(x) dnorm(x, mean = 0.3, sd = 0.2), 
  support = c(-2, 3)
)

## -------------------------------------------------------------------
Power(Normal(), PointMassPrior(.4, 1)) >= 0.8

## -------------------------------------------------------------------
Power(Normal(), condition(cont_prior, c(0, 3))) >= 0.8

