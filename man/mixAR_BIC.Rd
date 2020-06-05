\name{mixAR_BIC}
\alias{mixAR_BIC}
\alias{BIC_comp}
\title{
  BIC based model selection for MixAR models
}
\description{
  BIC calculations for mixture autoregressive models.
}
\usage{
mixAR_BIC(y, model, fix = NULL, comp_loglik = TRUE, index)
BIC_comp(x, y)
}
\arguments{
  \item{y}{a time series.}
  \item{model}{
    An object of class \code{MixAR}. Alternatively an output list from
    \code{fit_mixAR}. The model for which to calculate BIC.
  }  \item{fix}{
  If \code{fix = "shift"} shift parameters are not included
  in calculation of BIC. Default is \code{NULL}, i.e. shift parameters
  are included.
  }
  \item{
    comp_loglik}{Should the loglikelihood be calculated?  Default is
    \code{TRUE}. If \code{FALSE} and model is output of
    \code{fit_mixAR}, then the loglikelihood is not recalculated.
  }
  \item{index}{
    Discard the first \code{1:index} observations. If \code{missing},
    \code{index} is set to the largest AR order.
  }
  \item{x}{
    a list containing a combination of \code{MixAR} objects and/or
    output lists from \code{fit_mixAR}.
  }
}
\details{

  \code{mixAR_BIC} calculates the BIC criterion of a given \code{MixAR}
  object with respect to a specified time series.
  
  If \code{index} is specified, it has to be at least equal to the
  largest autoregressive order. The function calculates BIC on the last
  \code{(index + 1):n} data points.


  \code{BIC_comp} calculates the value of BIC for the models listed in
  \code{x} with respect to the specified time series \code{y}.

}
\value{
  
  If \code{comp_loglik = TRUE}, the function calculates BIC based on the
  given model, data and \code{index}.

  If \code{comp_loglik = FALSE} and model is output from
  \code{fit_mixAR}, it returns object \code{vallogf} from that list.

}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Davide Ravagli}
\examples{
model1 <- new("MixARGaussian", prob = c(0.5, 0.5), scale = c(1, 2),
              arcoef = list(-0.5, 1.1))

model2 <- new("MixARGaussian", prob = c(0.5, 0.3, 0.2), scale = c(1, 3, 8),
              arcoef = list(c(-0.5, 0.5), 1, 0.4))

set.seed(123)
y <- mixAR_sim(model1, 400, c(0, 0, 0), nskip = 100)

mixAR_BIC(y, model1)

model_fit1 <- fit_mixAR(y, model1)
model_fit2 <- fit_mixAR(y, model2)

mixAR_BIC(y, model_fit1)
mixAR_BIC(y, model_fit2)

BIC_comp(list(model1, model2, model_fit1, model_fit2), y)

mixAR_BIC(y, model_fit1, index = 20)
mixAR_BIC(y, model_fit2, index = 20)
}
%\keyword{ts}
\keyword{MixAR}
