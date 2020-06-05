\name{dist_norm}
\alias{dist_norm}
\docType{data}
\title{
  Functions for the standard normal distribution
}
\description{
  The noise distributions are specified by a list of functions for the
  density, quantiles, etc. This object demonstrates this for the
  standard normal distribution.
}
\usage{dist_norm}
\format{
  This is a list of functions or names of functions for calculations
  related to the standard normal distribution. Currently it has
  elements with the following names:
      \Sexpr[stage=build]{paste("\"", names(mixAR::dist_norm), "\"", collapse=", ", sep="")}.
}
\details{
  \code{dist_norm} may be used to specify the noise distribution for
  \code{MixAR} models. It can be used as a template if other
  distributions are needed, see also \code{fdist_stdnorm}.
}
% \source{
% %%  ~~ reference to a publication or URL from which the data were obtained ~~
% }
% \references{
% %%  ~~ possibly secondary sources and usages ~~
% }
\seealso{
  \code{\link{fdist_stdnorm}}
}
\examples{
dist_norm
dist_norm$pdf
dist_norm$cdf
}
\keyword{distribution}
