context("TwoStageDesign")

n1           <-  49.6
c1f          <-   0.7
c1e          <-   2.5
number_knots <-   5L
n2_piv       <- rep(49.6, number_knots)
c2_piv       <- rep(1.96, number_knots)
design       <- TwoStageDesign(n1, c1f, c1e, n2_piv, c2_piv, number_knots)
cp           <- ConditionalPower(Normal(), PointMassPrior(.4, 1))
pow          <- Power(Normal(), PointMassPrior(.4, 1))

test_that("gaussian quadrature constructor", {

    expect_equal(
        c(n1, c1f, c1e),
        c(design@n1, design@c1f, design@c1e),
        tolerance = sqrt(.Machine$double.eps), scale = 1)

    x1 <- seq(c1f, c1e, length.out = 11)

    expect_equal(
        n2(design, x1, round = FALSE),
        rep(49.6, length(x1)),
        tolerance = sqrt(.Machine$double.eps), scale = 1)

    expect_equal(
        n2(design, x1),
        rep(50, length(x1)),
        tolerance = sqrt(.Machine$double.eps), scale = 1)

    expect_equal(
        n(design, x1),
        rep(100, length(x1)),
        tolerance = sqrt(.Machine$double.eps), scale = 1)

}) # end 'gaussian quadrature constructor'

test_that("TwoStageDesignSurvival can be constructed",{
  n1           <-  49.6
  c1f          <-   0.7
  c1e          <-   2.5
  number_knots <-   5L
  n2_piv       <- rep(49.6, number_knots)
  c2_piv       <- rep(1.96, number_knots)
  design       <- TwoStageDesign(n1, c1f, c1e, n2_piv, c2_piv, number_knots, 0.7)
  
  expect_equal(
    c(n1, c1f, c1e),
    c(design@n1, design@c1f, design@c1e),
    tolerance = sqrt(.Machine$double.eps), scale = 1)
  
  x1 <- seq(c1f, c1e, length.out = 11)
  
  expect_equal(
    n2(design, x1, round = FALSE),
    rep(49.6, length(x1)),
    tolerance = sqrt(.Machine$double.eps), scale = 1)
  
  expect_equal(
    n2(design, x1),
    rep(50, length(x1)),
    tolerance = sqrt(.Machine$double.eps), scale = 1)
  
  expect_equal(
    n(design, x1),
    rep(100, length(x1)),
    tolerance = sqrt(.Machine$double.eps), scale = 1)
  
  #summary uses one extra parameter for event rate
  
  des <- TwoStageDesign(n1, c1f, c1e, n2_piv, c2_piv, number_knots, 0.8)
  
  expect_equal(
    length(summary(des)),
    9
  )
  
})# end 'TwoStageDesignSurvival can be constructed"

test_that("simulate works (as last time)", {

    design@n1      <- 50

    expect_known_value(
        adoptr::simulate(design, nsim = 50, dist = Normal(), theta = .5, seed = 42),
        file = "known_values/simulate.rds")

}) # end 'simulate works'



test_that("errors are returned correctly", {

    # pivots length must fit
    expect_error(
        TwoStageDesign(50, 0, 2, rep(50, 3), c(2, 2)))

    design  <- TwoStageDesign(50.1, 0, 2, rep(50, number_knots), rep(2, number_knots))

    # unconditional scores are not plotted
    expect_error(
        plot(design, rounded = TRUE, "Power" = pow))

    # only scores can be summarized
    expect_error(
        summary(design, rounded = TRUE, "Alternative" = PointMassPrior(.4, 1)))

}) # end 'errors are returned correctly'



test_that("plot produces correct number of columns", {

    pic1 <- plot(design, "ConditionalPower" = cp, lwd = 1.5, col = "green")
    pic2 <- plot(design, "ConditionalPower" = cp)
    pic3 <- plot(design, cex = 2)
    pic4 <- plot(design)

    expect_true(pic1$mfrow[2] == 3)
    expect_true(pic2$mfrow[2] == 3)
    expect_true(pic3$mfrow[2] == 2)
    expect_true(pic4$mfrow[2] == 2)

}) # end 'plot produces correct number of columns'



test_that("show method", {
  
  expect_equal(
    paste0(capture.output(show(design)), collapse = "\n\r"),
    "TwoStageDesign<n1=50;0.7<=x1<=2.5:n2=50> "
  )
  
  design <- TwoStageDesign(50, 0.7, 2.5, rep(49.6, 7), rep(1.96, 7), 7, 0.7)
  expect_equal(
    paste0(capture.output(show(design)), collapse = "\n\r"),
    "TwoStageDesignSurvival<n_events1=50;0.7<=x1<=2.5;n_events2=50> "
  )
  
})


test_that("print method", {
  
  design_summary <- summary(design, "CP"=cp, "Power"=pow)
  
  expect_equal(
    length(paste0(capture.output(print(design_summary)))),
    8
  )
  
  design <- TwoStageDesign(50, 0.7, 2.5, rep(49.6,7), rep(1.96,7), 7, 0.7)
  design_summary <- summary(design, "CP"=cp, "Power"=pow)
  expect_equal(
    length(paste0(capture.output(print(design_summary)))),
    11
  )
  
})



test_that("defining order does not destroy pivots", {

    n2 <- seq(100, 40, length.out = number_knots)
    c2 <- seq(2.0, 0.0, length.out = number_knots)
    d  <- TwoStageDesign(n1, c1f, c1e, n2, c2, number_knots)

    expect_equal(
        d@n2_pivots,
        n2,
        tolerance = sqrt(.Machine$double.eps), scale = 1)

    expect_equal(
        d@c2_pivots,
        c2,
        tolerance = sqrt(.Machine$double.eps), scale = 1)

}) # end 'defining order does not destroy pivots'



test_that("boundary designs keep monotonicity", {

    n2   <- seq(100, 40, length.out = number_knots)
    c2   <- seq(2.0, 0.0, length.out = number_knots)
    d    <- TwoStageDesign(n1, c1f, c1e, n2, c2, number_knots)
    d_lb <- get_lower_boundary_design(d)
    d_ub <- get_upper_boundary_design(d)

    expect_true(all(
        sign(diff(d_lb@c2_pivots)) == sign(diff(d@c2_pivots))))

    expect_true(all(
        sign(diff(d_ub@n2_pivots)) == sign(diff(d@n2_pivots))))

    expect_true(all(
        sign(diff(d_ub@c2_pivots)) == sign(diff(d@c2_pivots))))

}) # end 'boundary designs keep monotonicity'

test_that("design is correctly converted",{
  des_survival <- TwoStageDesign(design, 0.7)
  
  expect_true(
    is(des_survival,"TwoStageDesignSurvival")
  )
  
  expect_equal(
    c(n1, c1f, c1e),
    c(des_survival@n1, des_survival@c1f, des_survival@c1e),
    tolerance = sqrt(.Machine$double.eps), scale = 1)
  
  x1 <- seq(c1f, c1e, length.out = 11)
  
  expect_equal(
    n2(des_survival, x1, round = FALSE),
    rep(49.6, length(x1)),
    tolerance = sqrt(.Machine$double.eps), scale = 1)
  
  expect_equal(
    n2(des_survival, x1),
    rep(50, length(x1)),
    tolerance = sqrt(.Machine$double.eps), scale = 1)
  
  expect_equal(
    n(des_survival, x1),
    rep(100, length(x1)),
    tolerance = sqrt(.Machine$double.eps), scale = 1)
  
  expect_equal(
    des_survival, SurvivalDesign(design, 0.7)
  )
  
  expect_equal(
    design, TwoStageDesign(design)
  )
  
})
