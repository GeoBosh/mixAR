\name{marg_loglik}
\alias{marg_loglik}
\title{
  Calculate marginal loglikelihood at high density points of a MAR model.
}
\description{
  
  The function implements the method by
  \insertCite{Chib1995;textual}{mixAR} and
  \insertCite{ChibJeliazkov2001;textual}{mixAR} for calculation of the
  marginal loglikelihood of a mixture autoregressive model. It
  automatically finds high density values for model parameters, and
  evaluates the likelihood at such points.

}
\usage{
marg_loglik(y, model, tau, nsim, prob_mod)
}
\arguments{
  \item{y}{a time series (currently a numeric vector).}
  \item{model}{
    object of formal class \code{MixAR}, containing initial values for
    the parameters. Currently available for \code{MixARGaussian} objects
    only.
  }
  \item{tau}{
    tuning parameter for Metropolis-Hasting move to update
    autoregressive parameters.
  }
  \item{nsim}{
    sample size on which to evaluate highest density values.
  }
  \item{prob_mod}{
    this is currently the output from \code{Choose_pk}: the proportion
    of times the "best model" was chosen.
  }
}
\details{

  \code{nsim} is the sample size on which to evaluate highest density
  values for each set of parameters. For example, choosing
  \code{nsim=1000} results in \code{1000*(g+3)} (1000 iterations for
  each autoregressive component, plus 1000 for mean and scale parameters
  and mixing weights).

}
\value{A list containing the following elements:
  \item{marg_loglik}{value of the marginal loglikelihood.}
  \item{phi_hd}{set of highest density autoregressive parameters.}
  \item{prec_hd}{set of highest density precision parameters.}
  \item{mu_hd}{set of highest density mean parameters.}
  \item{weig_hd}{set of highest density mixing weights.}
}
\references{
\insertRef{Chib1995}{mixAR}

\insertRef{ChibJeliazkov2001}{mixAR}

}
\author{Davide Ravagli}
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
prob <- c(0.5, 0.5)
sigma <- c(1, 2)
arco <- list(-0.5, 1)

model <- new("MixARGaussian", prob = prob, scale = sigma, arcoef = arco)

y <- mixAR_sim(model, 250, rep(0, max(model@order)), nskip = 100)  # data

marg_loglik(y, model, tau = c(.15, .25), nsim = 50, 0.5)
}
\keyword{ estimation }
\keyword{ selection }
