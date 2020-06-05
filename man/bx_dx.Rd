\name{bx_dx}
\alias{bx_dx}
\title{
  RJMCMC move for AR order selection of mixture autoregressive models
}
\description{
  Computes probabilities for deciding whether the AR order should be
  increased or decreased by 1 at each iteration in Bayesian analysis of
  mixture autoregressive models.
}
\usage{
bx_dx(method = c("Ratio", "Poisson", "NULL"), par, pk)
}
\arguments{
  \item{method}{
    the method used for updating probabilities. If \code{"Ratio"} or
    \code{"Poisson"}, probabilities depend on \code{par} and decrease
    as \code{pk} increases. If \code{"NULL"}, probabilities are set to
    0.5. See Examples for details.
  }
  \item{par}{
    tuning parameter for calculating updating probabilities.
  }
  \item{pk}{
    autoregressive order of the selected component.
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{A list of \code{2} elements:
  \item{bx}{The probability of increasing the autoregressive order by 1.}
  \item{dx}{
    The probability of decreasing the autoregressive order by 1,
    calculated as \code{1 - bx}.
  }
}
\references{
  \insertRef{Ravagliboshnakov2019bayesmar}{mixAR}
}
\author{Davide Ravagli}
\note{
  This function is for use within \code{\link{Choose_pk}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%  \code{\link{Choose_pk}}
%}
%\examples{
%## for internal use only
%}
\keyword{internal} % use one of  RShowDoc("KEYWORDS")
