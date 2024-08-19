## ----setup-knitr, include=FALSE-----------------------------------------------
knitr::opts_chunk$set(
  collapse   = TRUE,
  comment    = "#>",
  fig.width  = 7,
  fig.height = 5
)

## ----load-adoptr--------------------------------------------------------------
library(adoptr)

## ----define-hypotheses--------------------------------------------------------
H_0 <- PointMassPrior(.0, 1)
H_1 <- PointMassPrior(.4, 1)

## ----define data-gernerating-mechanism----------------------------------------
datadist <- Normal(two_armed = FALSE)

## ----define-ess---------------------------------------------------------------
ess <- ExpectedSampleSize(datadist, H_1)

## ----define-power-toer--------------------------------------------------------
power <- Power(datadist, H_1)
toer  <- Power(datadist, H_0)

## ----create-design------------------------------------------------------------
initial_design <- get_initial_design(
  theta = .4,
  alpha = .025,
  beta  = .2,
  type_design  = "two-stage",
  dist  = datadist,
  order = 7L
)

## ----check-power--------------------------------------------------------------
evaluate(power, initial_design)

## ----check-toer---------------------------------------------------------------
evaluate(toer, initial_design)

## ----optimize-----------------------------------------------------------------
opt_res <- minimize(
  
  ess,
  
  subject_to(
    power >= 0.8,
    toer  <= .025
  ),
  
  initial_design
)

## ----nloptr-output, eval = FALSE----------------------------------------------
#  opt_res$nloptr_return$iterations

## ----plot-optimal-design------------------------------------------------------
plot(
  opt_res$design, 
  "Conditional power" = ConditionalPower(datadist, H_1)
)

## -----------------------------------------------------------------------------
evaluate(ess, opt_res$design)

## -----------------------------------------------------------------------------
df_sim <- simulate(
  opt_res$design, 
  nsim = 10^6, 
  dist = datadist, 
  theta = .4, 
  seed = 42
)

n <- df_sim$n1 + df_sim$n2
mean(n)
sd(n) / sqrt(length(n))

## -----------------------------------------------------------------------------
evaluate(toer, opt_res$design)
evaluate(power, opt_res$design)

