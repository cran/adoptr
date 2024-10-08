#' One-stage designs
#'
#' \code{OneStageDesign} implements a one-stage design as special case of
#' a two-stage design, i.e. as sub-class of \code{\link{TwoStageDesign}}.
#' This is possible by defining \ifelse{html}{\out{n<sub>2</sub> = 0}}{\eqn{n_2=0}},
#' \ifelse{html}{\out{c = c<sub>1</sub><sup>f</sup> = c<sub>1</sub><sup>e</sup>}}{\eqn{c = c_1^f = c_1^e}},
#' \ifelse{html}{\out{c<sub>2</sub>(x<sub>1</sub>) = ifelse(x<sub>1</sub> < c, Inf, -Inf)}}{\eqn{c_2(x_1) = ifelse(x_1 < c, Inf, -Inf)}}.
#' No integration pivots etc are required (set to \code{NaN}).
#'
#' Note that the default \code{\link{plot,TwoStageDesign-method}} method
#' is not supported for \code{OneStageDesign} objects.
#'
#' @seealso \code{\link{TwoStageDesign}}, \code{\link{GroupSequentialDesign-class}}
#'
#' @exportClass OneStageDesign
setClass("OneStageDesign",  contains = "TwoStageDesign")

#' One-stage designs for time-to-event endpoints
#'
#' \code{OneStageDesignSurvival} is a subclass of both \code{OneStageDesign} and
#' \code{TwoStageDesignSurvival}.
#'
#' @seealso \code{\link{TwoStageDesignSurvival-class}} and \code{\link{OneStageDesign-class}}
#' for superclasses and inherited methods.
#'
#' @exportClass OneStageDesignSurvival
setClass("OneStageDesignSurvival", contains = c("OneStageDesign", "TwoStageDesignSurvival"))

#' @export
#' @rdname OneStageDesign-class
setGeneric("OneStageDesign", function(n, ...) standardGeneric("OneStageDesign"))

#' @param n sample size (stage-one sample size)
#' @param c rejection boundary (\ifelse{html}{\out{c = c<sub>1</sub><sup>f</sup> = c<sub>1</sub><sup>e</sup>}}{\eqn{c = c_1^f = c_1^e}})
#' @param event_rate probability that a subject in either group will eventually have an event,
#' only needs to be specified for time-to-event endpoints.
#' @examples
#' design <- OneStageDesign(30, 1.96)
#' summary(design)
#' design_twostage <- TwoStageDesign(design)
#' summary(design_twostage)
#' design_survival <- OneStageDesign(30, 1.96, 0.7)
#'
#' @include TwoStageDesign.R
#'
#' @rdname OneStageDesign-class
#' @export
setMethod("OneStageDesign", signature = "numeric",
          function(n, c, event_rate) {
            tunable <- logical(8)
            tunable[1:2] <- TRUE
            names(tunable) <- c("n1", "c1f", "c1e", "n2_pivots", "c2_pivots", "x1_norm_pivots", "weights", "tunable")
            if(missing(event_rate)){
              new("OneStageDesign", n1 = n, c1f = c, c1e = c, n2_pivots = 0,
                  c2_pivots = NaN, x1_norm_pivots = NaN, weights = NaN,
                  tunable = tunable)}
            else{
              new("OneStageDesignSurvival", n1 = n, c1f = c, c1e = c, n2_pivots = 0,
                  c2_pivots = NaN, x1_norm_pivots = NaN, weights = NaN,
                  tunable = tunable, event_rate=event_rate)
            }
          })

#' @param n design object to convert (overloaded from \code{TwoStageDesign})
#'
#' @rdname SurvivalDesign
#'
#' @examples
#' OneStageDesign(design_os, 0.7)
#'
#' @export
setMethod("OneStageDesign", signature("OneStageDesign"),
          function(n,event_rate) {
            if(!missing(event_rate)) SurvivalDesign(n, event_rate)
            else n
          })


#' @rdname tunable_parameters
#' @export
setMethod("update", signature("OneStageDesign"),
          function(object, params, ...) {
            tunable_names <- names(object@tunable)[object@tunable]
            res <- object
            idx <- 1
            for (i in 1:length(tunable_names)) {
              slotname <- tunable_names[i]
              k <- length(slot(object, name = slotname))
              slot(res, name = slotname) <- params[idx:(idx + k - 1)]
              idx <- idx + k
            }
            res@c1e <- res@c1f
            return(res)
          })





#' @rdname n
#' @export
setMethod("n2", signature("OneStageDesign", "numeric"),
          function(d, x1, ...) 0 )





#' @rdname critical-values
#' @export
setMethod("c2", signature("OneStageDesign", "numeric"),
          function(d, x1, ...) ifelse(x1 <= d@c1f, Inf, -Inf) )





#' @param n1 \code{OneStageDesign} object to convert, overloaded from
#'   \code{\link{TwoStageDesign}}
#' @param order integer >= 2, default is 5; order of Gaussian quadrature
#'   integration rule to use for new TwoStageDesign.
#' @param eps numeric > 0, default = .01; the single critical value c must be
#'   split in a continuation interval \[c1f, c1e\]; this is given by c +/- eps.
#' @template dotdotdot
#'
#' @rdname OneStageDesign-class
#' @export
setMethod("TwoStageDesign", signature("OneStageDesign"),
          function(n1, event_rate,order = 5L, eps = .01, ...){
            
            c2 <- numeric(order)
            c2[1:floor(order / 2)] <- rep(3, floor(order / 2))
            c2[(ceiling(order / 2) + 1):order] <- rep(-3, floor(order / 2))
            if(!missing(event_rate)){
              return(
                TwoStageDesign(
                  n1             = n1@n1,
                  c1f            = n1@c1f - eps, # needs to be done for interpolation
                  c1e            = n1@c1f + eps, # needs to be done for interpolation
                  n2_pivots      = rep(0, order),
                  c2_pivots      = c2, # acceptance left/rejection right from c
                  event_rate     = event_rate
                )
              )}
            else{
              return(
                TwoStageDesign(
                  n1             = n1@n1,
                  c1f            = n1@c1f - eps, # needs to be done for interpolation
                  c1e            = n1@c1f + eps, # needs to be done for interpolation
                  n2_pivots      = rep(0, order),
                  c2_pivots      = c2 # acceptance left/rejection right from c
                  
                ))}
            
          })

#' @param n1 \code{OneStageDesign} object to convert, overloaded from
#'   \code{\link{TwoStageDesign}}
#' @param order integer >= 2, default is 5; order of Gaussian quadrature
#'   integration rule to use for new TwoStageDesign.
#' @param eps numeric > 0, default = .01; the single critical value c must be
#'   split in a continuation interval \[c1f, c1e\]; this is given by c +/- eps.
#' @examples
#' TwoStageDesign(design_survival)
#' @template dotdotdot
#'
#' @rdname OneStageDesign-class
#' @export
setMethod("TwoStageDesign", signature("OneStageDesignSurvival"),
          function(n1, order = 5L, eps = .01, ...){
            
            c2 <- numeric(order)
            c2[1:floor(order / 2)] <- rep(3, floor(order / 2))
            c2[(ceiling(order / 2) + 1):order] <- rep(-3, floor(order / 2))
            
            return(
              TwoStageDesign(
                n1             = n1@n1,
                c1f            = n1@c1f - eps, # needs to be done for interpolation
                c1e            = n1@c1f + eps, # needs to be done for interpolation
                n2_pivots      = rep(0, order),
                c2_pivots      = c2, # acceptance left/rejection right from c
                event_rate     = n1@event_rate
              )
            )
            
          })

#' plot() is not defined for one stage designs
#'
#' @template plot
#'
#' @rdname OneStageDesign-class
#' @export
setMethod("plot", signature("OneStageDesign"),
          function(x, y, ...)
            stop("plot method is only defined for two-stage designs!")
)
#' @rdname SurvivalDesign
#' @export
setMethod("SurvivalDesign", signature("OneStageDesign"),
          function(design, event_rate){
            tunable <- logical(8) # initialize to all false
            tunable[1:2] <- TRUE
            names(tunable) <- c("n1", "c1f", "c1e", "n2_pivots", "c2_pivots", "x1_norm_pivots", "weights", "tunable")
            new("OneStageDesignSurvival",
                n1=design@n1,c1f=design@c1f,c1e=design@c1e,n2_pivots=design@n2_pivots,
                c2_pivots=design@c2_pivots,
                x1_norm_pivots = design@x1_norm_pivots, weights = design@weights,
                tunable = tunable, event_rate=event_rate)
          })
