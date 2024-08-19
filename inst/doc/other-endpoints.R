## ----include = FALSE------------------------------------------------
knitr::opts_chunk$set(
  collapse   = TRUE,
  comment    = "#>",
  fig.width  = 7,
  fig.height = 5
)

## ----setup, include=FALSE-------------------------------------------
library(adoptr)

## -------------------------------------------------------------------
datadist <- Binomial(0.3, two_armed = TRUE)

## -------------------------------------------------------------------
H_0        <- PointMassPrior(.0, 1)
prior      <- ContinuousPrior(function(x) 1 / (pnorm(0.69, 0.2, 0.2) - 
                                                   pnorm(-0.29, 0.2, 0.2)) * 
                                  dnorm(x, 0.2, 0.2),
                              support = c(-0.29,0.69),
                              tighten_support = TRUE)

## -------------------------------------------------------------------
alpha      <- 0.025
min_epower <- 0.8
toer_cnstr <- Power(datadist, H_0) <= alpha
epow_cnstr <- Power(datadist, condition(prior, c(0.0,0.69))) >= min_epower


## -------------------------------------------------------------------
ess <- ExpectedSampleSize(datadist,prior)

init <- get_initial_design(0.2,0.025,0.2)

opt_design <- minimize(ess,subject_to(toer_cnstr,epow_cnstr), 
                       initial_design = init, check_constraints = TRUE)

plot(opt_design$design)

## -------------------------------------------------------------------
datadist <- Survival(0.7, two_armed = TRUE)

## -------------------------------------------------------------------
H_0 <- PointMassPrior(1, 1)
H_1 <- PointMassPrior(1.7, 1)

## -------------------------------------------------------------------
alpha <- 0.025
min_power <- 0.8
toer_con <- Power(datadist,H_0) <= alpha
pow_con <- Power(datadist,H_1) >= min_power

## -------------------------------------------------------------------
exp_no_events <- ExpectedNumberOfEvents(datadist, H_1)
init <- get_initial_design(1.7, 0.025, 0.2, dist=datadist)
opt_survival <- minimize(exp_no_events, subject_to(toer_con,pow_con),
                         initial_design = init, check_constraints=TRUE)

summary(opt_survival$design)

