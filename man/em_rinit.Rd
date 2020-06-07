\name{em_rinit}
\alias{em_rinit}
\alias{etk2tau}
\title{Gaussian EM-step with random initialisation}
\description{Gaussian EM-step with random initialisation.}
\usage{
em_rinit(y, order, partempl)

etk2tau(etk)
}
\arguments{
  \item{y}{time series.}
  \item{order}{MixAR order, vector of length the number of components.}
  \item{partempl}{ TODO: describe \code{partempl}.}
  \item{etk}{MixAR component residuals, a matrix.}
}
\details{

  \code{em_rinit} generates random MAR residuals, performs a non-distributional
  E-step, and a Gaussian M-step.
  % todo: Can this be generalised to non-Gaussian models?

  \code{etk2tau} estimates \code{tau} from component residuals
  only. Note that this is unlike \code{\link{em_tau}}, which also needs
  the noise pdf's, as well as estimates of the mixture probabilities.

  \code{em_rinit} uses \code{etk2tau} to start the EM algorithm.

}
\value{
  for \code{em_rinit}, an object from class \code{"MixARGaussian"}

  for \code{etk2tau}, a matrix representing \code{tau} (i-th row
  contains probabilities corresponding to the i-th observation) 
}
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{Georgi N. Boshnakov}
% \note{
% %%  ~~further notes~~
% }
% 
% %% ~Make other sections like Warning with \section{Warning }{....} ~
% 
% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
%\examples{
%}
\keyword{ em }
