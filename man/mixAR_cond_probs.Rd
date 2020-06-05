\name{mixAR_cond_probs}
\alias{mixAR_cond_probs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The E-step of the EM algorithm for MixAR models}
\description{Compute conditional probabilities for the E-step of the EM
  algorithm for MixAR models. Internal function.}
\usage{
mixAR_cond_probs(model, y, indx = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{an object from a sub-class of "MixAR".}
  \item{y}{the time series, a numeric vector.}
  \item{indx}{indices of elements for which to compute residuals.}
}
\details{
  This is essentially the E-step for the MixAR models.
}
\value{
  the conditional probabilities, an object from class "MixComp".
}
%\examples{
%}
\keyword{ em }
