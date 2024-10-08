#' Group-sequential two-stage designs
#'
#' Group-sequential designs are a sub-class of the \code{TwoStageDesign}
#' class with constant stage-two sample size.
#' See \code{\link{TwoStageDesign}} for slot details.
#' Any group-sequential design can be converted to a fully flexible
#' \code{TwoStageDesign} (see examples section).
#'
#' @seealso \code{\link{TwoStageDesign}} for superclass and inherited methods
#'
#' @exportClass GroupSequentialDesign
setClass("GroupSequentialDesign",  contains = "TwoStageDesign")


#' Group-sequential two-stage designs for time-to-event-endpoints
#'
#' Group-sequential designs for time-to-event-endpoints are a subclass of both
#' \code{TwoStageDesignSurvival} and \code{GroupSequentialDesign}.
#'
#' @seealso \code{\link{TwoStageDesignSurvival-class}} and \code{\link{GroupSequentialDesign-class}}
#' for superclasses and inherited methods.
#'
#' @exportClass GroupSequentialDesignSurvival
setClass("GroupSequentialDesignSurvival", 
         contains = c("GroupSequentialDesign", "TwoStageDesignSurvival"))

#' @export
#' @rdname GroupSequentialDesign-class
setGeneric("GroupSequentialDesign", function(n1,...) standardGeneric("GroupSequentialDesign"))


#' @template c1f
#' @template c1e
#' @param n2_pivots numeric of length one, stage-two sample size
#' @param c2_pivots numeric vector, stage-two critical values on the integration
#' pivot points
#' @param order of the Gaussian quadrature rule to use for integration, set to
#' length(c2_pivots) if NULL, otherwise first value of c2_pivots is repeated
#' 'order'-times.
#' @param event_rate probability that a subject in either group will eventually have an event,
#' only needs to be specified for time-to-event endpoints.
#' @template dotdotdot
#'
#' @examples
#' design <- GroupSequentialDesign(25, 0, 2, 25, c(1, 1.5, 2.5))
#' summary(design)
#' 
#' design_survival <- GroupSequentialDesign(25, 0, 2, 25, c(1, 1.5, 2.5), event_rate = 0.7)
#'
#' @rdname GroupSequentialDesign-class
#' @export

setMethod(GroupSequentialDesign, signature = "numeric",
          function(n1, c1f, c1e, n2_pivots, c2_pivots, order = NULL, event_rate, ...) {
              if (is.null(order)) {
                  order <- length(c2_pivots)
                  } else if (length(c2_pivots) != order) {
                      c2_pivots <- rep(c2_pivots[1], order)
                  }
          
               rule <- GaussLegendreRule(as.integer(order))
          
               tunable <- logical(8) # initialize to all false
               tunable[1:5] <- TRUE
               names(tunable) <- c("n1", "c1f", "c1e", "n2_pivots", "c2_pivots", "x1_norm_pivots", "weights", "tunable")
          
               if (missing(event_rate)) {
                 new("GroupSequentialDesign", n1 = n1, c1f = c1f, c1e = c1e,
                     n2_pivots = n2_pivots, c2_pivots = c2_pivots,
                     x1_norm_pivots = rule$nodes, weights = rule$weights, tunable = tunable)
                 } else {
                   new("GroupSequentialDesignSurvival", n1 = n1, c1f = c1f, c1e = c1e,
                     n2_pivots = n2_pivots, c2_pivots = c2_pivots,
                     x1_norm_pivots = rule$nodes, weights = rule$weights, tunable = tunable, 
                     event_rate = event_rate)
               }
})


#' @param n1 design object to convert (overloaded from \code{TwoStageDesign})
#' @rdname SurvivalDesign
#'
#' @examples
#' GroupSequentialDesign(design_gs, 0.8)
#'
#' @export
setMethod("GroupSequentialDesign", signature("GroupSequentialDesign"),
          function(n1,event_rate){
            if(!missing(event_rate)) SurvivalDesign(n1, event_rate)
            else n1
          })


#' @rdname n
#' @export
setMethod("n2", signature("GroupSequentialDesign", "numeric"),
          function(d, x1, round = TRUE, ...) {
              n2 <- ifelse(x1 < d@c1f | x1 > d@c1e, 0, d@n2_pivots)
              if (round)
                  n2 <- round(n2)
              return(n2)
          }
)

#' @param n1 stage one sample size or \code{GroupSequentialDesign} object to convert
#'   (overloaded from \code{\link{TwoStageDesign}})
#'
#' @examples
#' TwoStageDesign(design)
#'
#' @rdname GroupSequentialDesign-class
#' @export
setMethod("TwoStageDesign", signature("GroupSequentialDesign"),
     function(n1, event_rate, ...){
         tunable <- logical(8) # initialize to all false
         tunable[1:5] <- TRUE
         names(tunable) <- c("n1", "c1f", "c1e", "n2_pivots", "c2_pivots", "x1_norm_pivots", "weights", "tunable")
         if (!missing(event_rate)) {
           new("TwoStageDesignSurvival", n1 = n1@n1, c1f = n1@c1f, c1e = n1@c1e,
               n2_pivots = rep(n1@n2_pivots, length(n1@weights)),
               c2_pivots = n1@c2_pivots,
               x1_norm_pivots = n1@x1_norm_pivots, weights = n1@weights,
               tunable = tunable, event_rate=event_rate)
           } else {
             new("TwoStageDesign", n1 = n1@n1, c1f = n1@c1f, c1e = n1@c1e,
               n2_pivots = rep(n1@n2_pivots, length(n1@weights)),
               c2_pivots = n1@c2_pivots,
               x1_norm_pivots = n1@x1_norm_pivots, weights = n1@weights,
               tunable = tunable)
             }
})

#' @param n1 stage one sample size or \code{GroupSequentialDesign} object to convert
#'   (overloaded from \code{\link{TwoStageDesign}})
#'
#' @examples
#' TwoStageDesign(design_survival)
#'
#' @rdname GroupSequentialDesign-class
#' @export
setMethod("TwoStageDesign", signature("GroupSequentialDesignSurvival"),
          function(n1, ...){
            tunable <- logical(8) # initialize to all false
            tunable[1:5] <- TRUE
            names(tunable) <- c("n1", "c1f", "c1e", "n2_pivots", "c2_pivots", "x1_norm_pivots", "weights", "tunable")
            return(TwoStageDesign(n1 = n1@n1, c1f = n1@c1f, c1e = n1@c1e,
                                  n2_pivots = rep(n1@n2_pivots, length(n1@weights)),
                                  c2_pivots = n1@c2_pivots,
                                  x1_norm_pivots = n1@x1_norm_pivots, weights = n1@weights,
                                  tunable = tunable,event_rate=n1@event_rate))})

#' @rdname SurvivalDesign
#' @export
setMethod("SurvivalDesign", signature("GroupSequentialDesign"),
          function(design,event_rate){
            tunable <- logical(8) # initialize to all false
            tunable[1:5] <- TRUE
            names(tunable) <- c("n1", "c1f", "c1e", "n2_pivots", "c2_pivots", "x1_norm_pivots", "weights", "tunable")
            new("GroupSequentialDesignSurvival",
                n1=design@n1,c1f=design@c1f,c1e=design@c1e,n2_pivots=design@n2_pivots,
                c2_pivots=design@c2_pivots,
                x1_norm_pivots = design@x1_norm_pivots, weights = design@weights,
                tunable = tunable, event_rate=event_rate)
          })
