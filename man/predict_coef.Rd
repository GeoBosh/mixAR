\name{predict_coef}
\alias{predict_coef}
\title{Exact predictive parameters for multi-step mixAR prediction}
\description{Exact predictive parameters for multi-step mixAR prediction.}
\usage{
predict_coef(model, maxh)
}
\arguments{
  \item{model}{a mixAR model.}
  \item{maxh}{maximal horizon.}
}
\details{

  \code{predict_coef()} implements the method of
  \insertCite{boshnakov2009mar;textual}{mixAR} for the h-step prediction
  of mixAR processes. The h-step predictive distribution has a mixAR
  distribution and this function computes its parameters.

  \code{predict_coef()} is mostly a helper function, use
  \code{\link{multiStep_dist}} for prediction/forecasting. 
  
}
\value{
  a list with components:
  \item{arcoefs}{}
  \item{sigmas}{}
  \item{probs}{}
  \item{sStable}{}
}
\references{
  \insertAllCited{}
}
\author{Georgi N. Boshnakov}
\seealso{
  \code{\link{multiStep_dist}}
}
%\examples{
%}
\keyword{mixAR}
\keyword{prediction}
