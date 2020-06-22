\name{Choose_pk}
\alias{Choose_pk}
\title{Choose the autoregressive order of MixAR components}
\description{
  Reversible Jump MCMC algorithm to choose the optimal
  autoregressive order of each component of a mixture autoregressive
  model.
}
\usage{
Choose_pk(y, model, fix_shift = FALSE, tau, pmax, method, par = NULL, nsim)
}
\arguments{
  \item{y}{
    a time series. Currently a \code{numeric} vector.
  }
  \item{model}{
    an object inheriting from class \code{"MixAR"}.
  }
  \item{fix_shift}{
    whether the shift/mean parameter should be kept fixed to its
    starting value or not. Default is \code{FALSE}.
  }
  \item{tau}{
    tuning parameters for Metropolis-Hastings algorithm in sampling AR
    coefficients.
  }
  \item{pmax}{
    the largest autoregressive order allowed for each component.
  }
  \item{method}{
    character vector of length 1. Method for calculating probability of
    new AR order to be increased/decreased by 1 unit. Currently
    available \code{"Ratio"}, \code{"Poisson"} and
    \code{"NULL"}. Default is \code{"NULL"}.
  }
  \item{par}{
    \code{numeric}, parameter for tuning probabilities according to
    \code{method}. Ignored if \code{method} is "NULL".
  }
  \item{nsim}{
    \code{numeric}, the number of iterations.
  }

}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  \item{out}{
    a dataframe with \code{g+1} columns. The first \code{g} columns
    contain the autoregressive orders of the components, the last column
    how often a model is preferred, divided by \code{nsim}.
  }
  \item{fix_shift}{the choice made for the shift/mean parameters.}
  \item{method}{the method used to increase/decrease AR orders.}
}
\references{
  \insertRef{Ravagliboshnakov2019bayesmar}{mixAR}
}
\author{Davide Ravagli}
\note{
   \code{Choose_pk} currenlty supports class \code{"MixARGaussian"}
   only.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{bx_dx}} for more details on the \code{method}
}
\examples{
model <- new("MixARGaussian", 
             prob   = exampleModels$WL_At@prob,      # c(0.5, 0.5)
             scale  = exampleModels$WL_At@scale,     # c(1, 2)        
             arcoef = list(-0.5, 1) )
             # note: arcoef != list(-0.5, 1.1) == exampleModels$WL_At@arcoef@a

set.seed(1234)
n <- 50 # 200
y <- mixAR_sim(model, n, rep(0, max(model@order)), nskip = 100)

nsim <- 25 # 100
pk_star <- Choose_pk(y, model, tau = c(.15, .25), pmax = 5, method = "NULL", nsim = nsim)
}
\keyword{MixAR}
