% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ContinuousPrior.R
\docType{class}
\name{ContinuousPrior-class}
\alias{ContinuousPrior-class}
\alias{ContinuousPrior}
\title{Continuous univariate prior distributions}
\usage{
ContinuousPrior(
  pdf,
  support,
  order = 10,
  label = NA_character_,
  tighten_support = FALSE,
  check_normalization = TRUE
)
}
\arguments{
\item{pdf}{vectorized univariate PDF function}

\item{support}{numeric vector of length two with the bounds of
the compact interval on which the pdf is positive.}

\item{order}{\code{integer}, integration order of the employed Gaussian quadrature
integration rule to evaluate scores. Automatically set to \code{length(n2_pivots)} if \cr
\code{length(n2_pivots) == length(c2_pivots) > 1}, otherwise c2 and n2
are taken to be constant in stage-two and replicated to match the number of
pivots specified by \code{order}}

\item{label}{object label (string)}

\item{tighten_support}{logical indicating if the support should be tightened}

\item{check_normalization}{logical indicating if it should be checked
that \code{pdf} defines a density.}
}
\description{
\code{ContinuousPrior} is a sub-class of \code{\link{Prior}} implementing
a generic representation of continuous prior distributions over a compact
interval on the real line.
}
\section{Slots}{

\describe{
\item{\code{pdf}}{cf. parameter 'pdf'}

\item{\code{support}}{cf. parameter 'support'}

\item{\code{pivots}}{normalized pivots for integration rule (in [-1, 1])
the actual pivots are scaled to the support of the prior}

\item{\code{weights}}{weights of of integration rule at \code{pivots} for
approximating integrals over \code{delta}}
}}

\examples{
ContinuousPrior(function(x) 2*x, c(0, 1))

}
\seealso{
Discrete priors are supported via \code{\link{PointMassPrior}}
}
