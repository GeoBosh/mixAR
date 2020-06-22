\name{bayes_mixAR}
\alias{bayes_mixAR}
\title{Bayesian sampling of mixture autoregressive models}
\description{
  Samples parameters of a mixture autoregressive model from respective
  posterior distributions.
}
\usage{
bayes_mixAR(y, model, fix_shift = FALSE, a = .2, c = 2, tau, nsim, burnin)
}
\arguments{
  \item{y}{a time series (currently a numeric vector).}
  \item{model}{
    an object of class \code{MixAR}. Currently only handles
    \code{MixARGaussian} objects.
  }
  \item{fix_shift}{
    should \code{shift} be kept fixed? If \code{FALSE} (default)
    \code{shift} is sampled.
  }
  \item{a, c}{
    numeric hyperparameters, default values are from
    \insertCite{RichardsonGreen1997;textual}{mixAR}.
  }
  \item{tau}{
    \code{numeric} vector of length g, the number of components in the
    mixture. Tuning parameter for M-H move in updating AR parameters. If
    \code{length(tau)} is 1, same tuning parameter is taken for all
    components.
  }
  \item{nsim}{
    \code{numeric}, the number of iterations.
  }
  \item{burnin}{
    \code{numeric}, the number of iterations taken as burn-in period.
  }
}

\details{
  
  For details see \insertCite{ravagli2020bayesian;textual}{mixAR}.

}
\value{
  a list with following elements:
  \item{mix_weights}{
    a \code{g} columns matrix with samples from the posterior distributions of
    the mixing weights.
  }
  \item{scale}{
    a \code{g} columns matrix with samples from posterior distributions of
    scale parameters.
  }
  \item{precision}{
    a \code{g} columns matrix with samples from posterior distributions of
    precision parameters, defined as \code{1 / (scale ^ 2)}.
  }
  \item{shift}{
    a \code{g} columns matrix with samples from posterior distributions of
    shift parameters, namely phi_k0.
  }
  \item{mu}{
    a \code{g} columns matrix with samples from posterior distributions
    of component means, calculated as \code{phi_k0 / (1 - phi_k1 - phi_k2 - ...)}.
  }
  \item{ARcoeff}{
    a list which elements are matrices, one for each AR component in the
    mixture.
  }
  \item{acc_rate}{
    \code{numeric} vector, the acceptance rate for M-H moves.
  }
  \item{n_samp}{
    the sample size, calculated as \code{nsim - burnin}.
  }
  \item{LatentZ}{
    the latest Z variables drawn (for utility only).
  }
  \item{n_comp}{
    the number of components in the mixture.
  }
  \item{fix_shift}{
    same as input, whether the shift parameter was kept fixed or not.
  }
}
\references{
  \insertRef{RichardsonGreen1997}{mixAR}

  \insertRef{ravagli2020bayesian}{mixAR}
}
\author{
  Davide Ravagli
}
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
ar <- list(-0.5, 1)

model <- new("MixARGaussian", prob = prob, scale = sigma, arcoef = ar)

## MAR(1,1) model
y <- mixAR_sim(model, 300, rep(0, max(model@order)))

bayes_mixAR(y, model, fix_shift = FALSE, tau = c(.15,.25), nsim = 20, burnin = 10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ samp }% use one of  RShowDoc("KEYWORDS")
