\name{err_k}
\alias{err_k}
\title{Utility function for mixAR}
\description{
  Calculates residuals under a certain component of the mixture.
}
\usage{
err_k(AR, mu, y, z, p, pk)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{AR}{a \code{list} with autoregressive parameters.}
  \item{mu}{component means.}
  \item{y}{a time series (currently a numeric vector).}
  \item{z}{a vector of allocation to a specific component.}
  \item{p}{maximum autoregressive order}
  \item{pk}{autoregressive order of the component.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  \item{e}{a vector containing component specific residuals.}
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Davide Ravagli}
\note{This is built as a utility function.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%}
\keyword{ residual }% use one of  RShowDoc("KEYWORDS")
\keyword{ component }
\keyword{ internal }
