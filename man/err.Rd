\name{err}
\alias{err}
\title{
  Calculate component specific error terms under mixAR model
}
\description{
  Returns a matrix which columns correspond to the error terms. Each
  column's row will assume value 0 if the observation was not "assigned"
  to that component.
}
\usage{
err(AR, mu, y, z, pk)
}
\arguments{
  \item{AR}{a \code{list} with autoregressive components.}
  \item{mu}{Component means.}
  \item{y}{a time series (currently a numeric vector).}
  \item{z}{
    a \code{matrix} which columns contain observations
    "assignment". Hence each row will contain one 1 and (g-1) 0's
  }
  \item{pk}{autoregressive orders.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  \item{e}{
    a matrix which columns correspond to component specific "residuals",
    which are equal to 0 for observations not assigned to such
    component.
  }
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Davide Ravagli}
\note{This function is built for use within sampling functions.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%}
\keyword{ sample }% use one of  RShowDoc("KEYWORDS")
\keyword{ residuals }
\keyword{ internal }
