\name{lik_params_bounds}
\alias{lik_params_bounds}
\title{
  Give natural limits for parameters of a MixAR model.
}
\description{
  Give the natural lower and upper limits for the parameters of a MixAR
  model.
}
\usage{
lik_params_bounds(model)
}
\arguments{
  \item{model}{a \code{MixAR} model
}
}
\details{
  The function gives crude limits:
  the probabilities of the components are between 0 and 1,
  the standard deviations of the components are non-negative.
  For the other parameters the limits are
  \eqn{(-\infty,\infty)}{(-Inf,Inf)}.

  There are further restrictions, e.g.
  the sum of the probabilities should be less than or equal to 1
  and
  the autoregression coefficients normally are restricted to a
  particular region, but these are not indicated in the returned value.
}
\value{
  A list with two components describing the limits on the parameters.
  The order of the parameters is as the one returned by \code{\link{lik_params}}.
  \item{lower}{lower limits, a numeric vector}
  \item{upper}{upper limits, a numeric vector}
}
% \references{
% %% ~put references to the literature/web site here ~
% }
% \author{
% %%  ~~who you are~~
% }
% \note{
% %%  ~~further notes~~
% }
%
% %% ~Make other sections like Warning with \section{Warning }{....} ~
%
\seealso{
  \code{\link{lik_params}}
}
%\examples{
%##
%}
\keyword{MixAR}
\keyword{internal}
