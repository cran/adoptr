% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regularization.R
\docType{class}
\name{AverageN2-class}
\alias{AverageN2-class}
\alias{AverageN2}
\alias{evaluate,AverageN2,TwoStageDesign-method}
\title{Regularization via L1 norm}
\usage{
AverageN2(label = NA_character_)

\S4method{evaluate}{AverageN2,TwoStageDesign}(s, design, optimization = FALSE, subdivisions = 10000L, ...)
}
\arguments{
\item{label}{object label (string)}

\item{s}{\code{\link[=Scores]{Score}} object}

\item{design}{object}

\item{optimization}{logical, if \code{TRUE} uses a relaxation to real
parameters of the underlying design; used for smooth optimization.}

\item{subdivisions}{number of subdivisions to use for adaptive integration
(only affects non-optimization code)}

\item{...}{further optional arguments}
}
\value{
an object of class \code{\link{AverageN2}}
}
\description{
Implements the L1-norm of the design's stage-two sample size function.
The average of the stage-two sample size without weighting with
the data distribution is computed.
This can be interpreted as integration over a unifrom prior on
the continuation region.
}
\examples{
avn2 <- AverageN2()

evaluate(
   AverageN2(),
   TwoStageDesign(100, 0.5, 1.5, 60.0, 1.96, order = 5L)
) # 60

}
\seealso{
\code{\link{N1}} for penalizing n1 values
}
