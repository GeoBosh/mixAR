\name{mixSARfit}
\alias{mixSARfit}
\title{
  Fit mixture autoregressive models with seasonal AR parameters
}
\description{
  Provides estimation via EM-Algorithm for Mixture Autoregressive Models
  including seasonal AR parameters.
}
\usage{
mixSARfit(y, model, est_shift = FALSE, tol = 10^-14)
}
\arguments{
  \item{y}{A time series (currently a numeric vector).}
  \item{model}{
    An object of class "mixAR" including seasonality components.
  }
  \item{est_shift}{
    If missing of \code{FALSE}, fix the intercepts to zero, estimate
    them otherwise.
  }
  \item{tol}{Threshold for stopping criterion.}
}
\details{

  This function only works for "mixAR" objects in which slot
  \code{arcoef} is of class \code{raggedCoefS}.

}
\value{A list of 2:
  \item{model}{An object of class "mixAR". The estimated model.}
  \item{vallogf}{
    The value of the loglikelihood function for the returned model.
  }
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{Davide Ravagli and Georgi N. Boshnakov}
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
ar1 <- list(c(0.5, -0.5), c(1.1, 0, -0.5))
ar12 <- list(0, c(-0.3, 0.1))
s = 12

rag <- new("raggedCoefS", a = ar1, as = ar12, s = s)

model <- new("mixARGaussian", prob = exampleModels$WL_A@prob, # c(0.5, 0.5)
             scale = exampleModels$WL_A@scale,                # c(5, 1)
             arcoef = rag)

#set.seed(1234)
y <- mixAR_sim(model, n = 500, init = rep(0, 24))

mixSARfit(y, model)
## fix the intercepts to zero
mixSARfit(y, model, est_shift = FALSE)
}
%\keyword{ }% use one of  RShowDoc("KEYWORDS")
