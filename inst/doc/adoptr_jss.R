## ----setup, include=FALSE-------------------------------------------
library(adoptr)
library(dplyr)
library(tidyr)
library(ggplot2)

buildvignette <- as.logical(Sys.getenv("NOTCRAN", unset = FALSE))

knitr::opts_chunk$set(
    engine='R', 
    highlight=FALSE, 
    prompt=TRUE, 
    tidy=FALSE,
    eval=buildvignette
)
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)

## ----plot-opts, include=FALSE---------------------------------------
add_opts <- function(p) {
  p + theme_bw() +
  theme(
    panel.grid   = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  )
}


# extract legend
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


## ----class-diagram, out.width='100%', echo=FALSE, fig.cap='Overview of the most important classes and methods (in italic) in the R-package adoptr. A subclassing relationship is indicated by a connecting line to the  corresponding super class above it. The most important methods for each class are listed under the respective class name in italic font.'----
knitr::include_graphics("structure.png")

## ----case-1-data-distribution---------------------------------------
datadist <- Normal(two_armed = TRUE)

## ----case-1-hypotheses----------------------------------------------
null        <- PointMassPrior(theta = .0, mass = 1.0)
alternative <- PointMassPrior(theta = .3, mass = 1.0)
power       <- Power(dist = datadist, prior = alternative)
toer        <- Power(dist = datadist, prior = null)
mss         <- MaximumSampleSize()

## ----case-1-ess-----------------------------------------------------
ess <- ExpectedSampleSize(dist = datadist, prior = alternative)

## -------------------------------------------------------------------
initial_design <- get_initial_design(theta = 0.3, alpha = 0.025,
                                     beta = 0.1, type = "two-stage",
                                     dist = datadist, order = 7)

## ---- collapse=FALSE------------------------------------------------
evaluate(toer, initial_design)
evaluate(power, initial_design)

## ---- collapse=FALSE------------------------------------------------
evaluate(toer  <= .025, initial_design)
evaluate(power >= .9, initial_design)

## ----case-1-optimization--------------------------------------------
opt1 <- minimize(ess, subject_to(power >= 0.9, toer  <= 0.025),
                 initial_design)

## -------------------------------------------------------------------
cp <- ConditionalPower(dist = datadist, prior = alternative)
summary(opt1$design, "Power" = power, "ESS" = ess, "CP" = cp)

## ---- standard-case, fig.height=2.25*1.25, fig.width=6*1.25, out.width='100%', fig.cap="Optimal sample size, critical value, and conditional power plotted against the interim test statistic (built-in plot method)."----
plot(opt1$design, `Conditional power` = cp)

## -------------------------------------------------------------------
prior <- ContinuousPrior(
  pdf     = function(theta) dnorm(theta, mean = .3, sd = .1),
  support = c(-1, 1),
  order   = 25)

## ----case-2-ess-----------------------------------------------------
ess <- ExpectedSampleSize(dist = datadist, prior = prior)

## ----case-2-pwr-mcr-------------------------------------------------
epower <- Power(dist = datadist, prior = condition(prior, c(.1, 1)))

## ----case-2-evaluate-epwr-opt1--------------------------------------
evaluate(epower, opt1$design)

## ----case-2-optimization, warning=FALSE-----------------------------
opt2 <- minimize(ess, subject_to(epower >= 0.9, toer <= 0.025),
                 initial_design,
                 opts = list(algorithm = "NLOPT_LN_COBYLA",
                             xtol_rel = 1e-5, maxeval = 20000))

## -------------------------------------------------------------------
`n(X_1)`      <- ConditionalSampleSize()
`E[n(X_1)^2]` <- expected(composite({`n(X_1)`^2}),
                          data_distribution = datadist,
                          prior = prior)

## ----case-3-optimization--------------------------------------------
opt3 <- minimize(composite({`E[n(X_1)^2]` - 200000*epower}),
                 subject_to(toer <= 0.025), initial_design)

## ----power-utility--------------------------------------------------
evaluate(epower, opt3$design)

## ---- comparison, echo=FALSE, warning=FALSE, fig.height=2.25*1.25, fig.width=6*1.25, out.width='100%', fig.cap="Comparison of optimal designs under a point prior, a continuous prior, and a utility maximization approach."----

x1    <- seq(0, 3, by = .01)
theta <- seq(0, .4, by = .01)

plot_data_1 <- tibble(
    type   = c("Point Prior", "Continuous Prior", "Utility Maximization"),
    design = c(opt1$design, opt2$design, opt3$design)
  ) %>%
  group_by(type) %>%
  do(
    x1 = x1,
    n  = adoptr::n(.$design[[1]], x1),
    c2 = c2(.$design[[1]], x1)
  )  %>%
  unnest(., cols = c(x1, n, c2)) %>%
  mutate(
    section = ifelse(
      is.finite(c2),
      "continuation",
      ifelse(c2 == -Inf, "efficacy", "futility")
    )
  ) %>%
  gather(variable, value, n, c2)

plot_data_2 <- tibble(
    type   = c("Point Prior", "Continuous Prior", "Utility Maximization"),
    design = c(opt1$design, opt2$design, opt3$design)
  ) %>%
  group_by(type) %>%
  do(
    theta = theta,
    pow   = sapply(theta,
                   function(d) {
                       evaluate(Power(datadist, PointMassPrior(d, 1)), .$design[[1]])
                     })
  )  %>%
  unnest(., cols = c(theta, pow)) %>%
  gather(variable, value, pow)





p1 <- {
  ggplot(filter(plot_data_1, variable == "n"), aes(x1, value, color = type)) +
    xlab(expression(x[1])) +
    geom_line(aes(group = interaction(section, type))) +
    scale_y_continuous("n", limits = c(0, 550), breaks = seq(0, 500, 100))
  } %>%
  add_opts

p2 <- {
  ggplot(filter(plot_data_1, variable == "c2"), aes(x1, value, color = type)) +
    xlab(expression(x[1])) +
    geom_line(aes(group = interaction(section, type))) +
    scale_y_continuous(expression(c[2]), breaks = seq(0, 5, by = .5))
} %>%
  add_opts


p3 <- {
  ggplot(filter(plot_data_2, variable == "pow"), aes(theta, value, color = type)) +
    xlab(expression(theta)) +
    geom_line(aes(group = type)) +
    scale_y_continuous("Power", breaks = seq(0, 1, .1))
} %>%
  add_opts


gridExtra::grid.arrange(
  gridExtra::arrangeGrob(
    p1 + theme(legend.position = "none"),
    p2 + theme(legend.position = "none"),
    p3 + theme(legend.position = "none"),
    nrow = 1
  ),
  g_legend(p1),
  nrow = 2,
  heights = c(10, 1)
)

## ----case-4-hypotheses----------------------------------------------
prior <- PointMassPrior(theta = .3, mass = 1.0)
ess   <- ExpectedSampleSize(dist = datadist, prior = prior)
cp    <- ConditionalPower(dist = datadist, prior = prior)
power <- expected(cp, data_distribution = datadist, prior = prior)

## -------------------------------------------------------------------
opt4 <- minimize(ess, subject_to(toer <= 0.025, power >= 0.9, cp >= 0.8),
                 initial_design)

## ---- cp-constraint, echo=FALSE, warning=FALSE, fig.height=2.25*1.25, fig.width=6*1.25, out.width="100%", fig.cap="Optimal designs with and without conditional power constraint."----
x1 <- seq(0, 3, by = .01)

plot_data_2 <- tibble(
  type   = c("No CP constraint", "With CP constraint"),
  design = c(opt1$design, opt4$design)
) %>%
    group_by(type) %>%
    do(
        x1 = x1,
        n  = adoptr::n(.$design[[1]], x1),
        c2 = c2(.$design[[1]], x1),
        cp = evaluate(cp, .$design[[1]], x1)
    )  %>%
    unnest(., c(x1, n, c2, cp)) %>%
    mutate(
        section = ifelse(
            is.finite(c2),
            "continuation",
            ifelse(c2 == -Inf, "efficacy", "futility")
        )
    ) %>%
    gather(variable, value, n, c2, cp)

add_opts <- function(p) {
  p + xlab(expression(x[1])) +
  theme_bw() +
  theme(
    panel.grid   = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  )
}


p1_2 <- {
  ggplot(filter(plot_data_2, variable == "n"), aes(x1, value, color = type)) +
      geom_line(aes(group = interaction(section, type))) +
      scale_y_continuous("n", limits = c(0, 500), breaks = seq(0, 500, 100))
} %>%
  add_opts

p2_2 <- {
  ggplot(filter(plot_data_2, variable == "c2"), aes(x1, value, color = type)) +
      geom_line(aes(group = interaction(section, type))) +
      scale_y_continuous(expression(c[2]), breaks = seq(0, 5, by = .5))
} %>%
  add_opts

p3_2 <- {
  ggplot(filter(plot_data_2, variable == "cp"), aes(x1, value, color = type)) +
      geom_line(aes(group = interaction(section, type))) +
      scale_y_continuous("Conditional Power", breaks = seq(0, 1, by = .1))
} %>%
  add_opts



gridExtra::grid.arrange(
  gridExtra::arrangeGrob(
    p1_2 + theme(legend.position = "none"),
    p2_2 + theme(legend.position = "none"),
    p3_2 + theme(legend.position = "none"),
    nrow = 1
  ),
  g_legend(p1_2),
  nrow = 2,
  heights = c(10, 1)
)

## -------------------------------------------------------------------
initial_design@n1  <- 80
initial_design@c1f <- 0
initial_design     <- make_fixed(initial_design, n1, c1f)

## ---- message=FALSE-------------------------------------------------
opt5 <- minimize(ess, subject_to(toer <= 0.025, power >= 0.9),
                 initial_design)

## ---- tunable, echo=FALSE, warning=FALSE, fig.height=2.25*1.25, fig.width=6*1.25, out.width="100%", fig.cap="Comparison of fully optimal design and optimal design with fixed first-stage sample size."----

x1    <- seq(-.5, 3, by = .01)
theta <- seq(-.2, .5, by = .01)

plot_data_1 <- tibble(
    type   = c("Optimal", "Optimal with fixed n1"),
    design = c(opt1$design, opt5$design)
  ) %>%
  group_by(type) %>%
  do(
      x1 = x1,
      n  = adoptr::n(.$design[[1]], x1),
      c2 = c2(.$design[[1]], x1)
  )  %>%
  unnest(., c(x1, n, c2)) %>%
  mutate(
      section = ifelse(
          is.finite(c2),
          "continuation",
          ifelse(c2 == -Inf, "efficacy", "futility")
      )
  ) %>%
  gather(variable, value, n, c2)



plot_data_2 <- tibble(
    type   = c("Optimal", "Optimal with fixed n1"),
    design = c(opt1$design, opt5$design)
  ) %>%
  group_by(type) %>%
  do(
      theta = theta,
      pow   = sapply(theta,
                     function(d) {
                       evaluate(ExpectedSampleSize(datadist, PointMassPrior(d, 1)), .$design[[1]])
                     })
  )  %>%
  unnest(., cols  = c(theta, pow)) %>%
  gather(variable, value, pow)



p1 <- {
  ggplot(filter(plot_data_1, variable == "n"), aes(x1, value, color = type)) +
      xlab(expression(x[1])) +
      geom_line(aes(group = interaction(section, type))) +
      scale_y_continuous("n", limits = c(0, 500), breaks = seq(0, 500, 100))
} %>%
  add_opts

p2 <- {
  ggplot(filter(plot_data_1, variable == "c2"), aes(x1, value, color = type)) +
      xlab(expression(x[1])) +
      geom_line(aes(group = interaction(section, type))) +
      scale_y_continuous(expression(c[2]), breaks = seq(0, 5, by = .5))
} %>%
  add_opts


p3 <- {
  ggplot(filter(plot_data_2, variable == "pow"), aes(theta, value, color = type)) +
      xlab(expression(theta)) +
      geom_line(aes(group = type)) +
      scale_y_continuous("Expected Sample Size") +
      geom_vline(xintercept = 0.3, colour = "grey", size=0.3)
} %>%
  add_opts


gridExtra::grid.arrange(
  gridExtra::arrangeGrob(
    p1 + theme(legend.position = "none"),
    p2 + theme(legend.position = "none"),
    p3 + theme(legend.position = "none"),
    nrow = 1
  ),
  g_legend(p1),
  nrow = 2,
  heights = c(10, 1)
)

