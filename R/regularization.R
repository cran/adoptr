#' Regularization via L1 norm
#'
#' Implements the L1-norm of the design's stage-two sample size function.
#' The average of the stage-two sample size without weighting with
#' the data distribution is computed.
#' This can be interpreted as integration over a unifrom prior on
#' the continuation region.
#'
#' @template s
#' @template design
#' @template optimization
#' @template label
#' @template dotdotdot
#' @param subdivisions number of subdivisions to use for adaptive integration
#'   (only affects non-optimization code)
#'
#' @seealso \code{\link{N1}} for penalizing n1 values
#'
#' @aliases AverageN2
#' @exportClass AverageN2
setClass("AverageN2", representation(
    dummy = 'logical'
    ),
    contains = "UnconditionalScore")


#' @examples
#' avn2 <- AverageN2()
#'
#' @return an object of class \code{\link{AverageN2}}
#'
#' @rdname AverageN2-class
#' @export
AverageN2 <- function(label = NA_character_) new("AverageN2", label = label, dummy = FALSE)



#' @examples
#' evaluate(
#'    AverageN2(),
#'    TwoStageDesign(100, 0.5, 1.5, 60.0, 1.96, order = 5L)
#' ) # 60
#'
#' @rdname AverageN2-class
#' @export
setMethod("evaluate", signature("AverageN2", "TwoStageDesign"),
          function(s, design, optimization = FALSE, subdivisions = 10000L, ...) {
              if (optimization) {
                  res <- gauss_quad(design@n2_pivots,
                                    design@c1f,
                                    design@c1e,
                                    design@weights)
              } else {
                  res <- stats::integrate(
                      function(x) n2(design, x, round = TRUE),
                      design@c1f,
                      design@c1e,
                      subdivisions = subdivisions,
                      ...
                  )$value
              }
              res <- res / (design@c1e - design@c1f)
              return(res)
          }
)





#' Regularize n1
#'
#' \code{N1} is a class that computes the \code{n1} value of a design.
#' This can be used as a score in \code{\link{minimize}}.
#'
#' @template s
#' @template design
#' @template optimization
#' @template dotdotdot
#' @template label
#'
#' @seealso See \code{\link{AverageN2}} for a regularization of
#'  the second-stage sample size.
#'
#' @aliases N1
#' @exportClass N1
setClass("N1", representation(dummy = 'logical'),
contains = "UnconditionalScore")

#' @examples
#' n1_score <- N1()
#'
#' @return an object of class \code{\link{N1}}
#'
#' @rdname N1-class
#' @export
N1 <- function(label = NA_character_) new("N1", label = label, dummy = FALSE)


#' @examples
#' evaluate(
#'    N1(),
#'    TwoStageDesign(70, 0, 2, rep(60, 6), rep(1.7, 6))
#' ) # 70
#'
#' @rdname N1-class
#' @export
setMethod("evaluate", signature("N1", "TwoStageDesign"),
          function(s, design, optimization = FALSE, ...)
              if(optimization) design@n1
              else n1(design, round = !optimization)
          )

