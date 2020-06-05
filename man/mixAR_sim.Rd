\name{mixAR_sim}
\alias{mixAR_sim}
\alias{mixAny_sim}
\title{Simulate from mixAR models}
\description{Simulate from mixAR models}
\usage{
mixAR_sim(model, n, init, nskip = 100, flag = FALSE)

mixAny_sim(model, n, init, nskip=100, flag = FALSE,
                  theta, galpha0, galpha, gbeta)

}
\arguments{
  \item{model}{model from which to simulate, an object inheriting from class \code{mixAR}.}
  \item{init}{initial values, numeric vector.}
  \item{n}{size of the simulated series.}
  \item{nskip}{number of burn-in values, see Details.}
  \item{flag}{if \code{TRUE} return also the regimes.}
  \item{theta}{ma coef, a list.}
  \item{galpha0}{alpha0[k], k=1,...,g.}
  \item{galpha}{garch alpha.}
  \item{gbeta}{garch beta.}
}
\details{

  \code{mixAR_sim} simulates a series of length \code{nskip+n} and
  returns the last \code{n} values.

  \code{mixAny_sim} simulates from a mixAR model with GARCH
  innovations. \code{mixAny_sim} was a quick fix for Shahadat and needs
  consolidation. 
  
  The vector \code{init} provides the initial values for
  \eqn{t=...,-1,0}. Its length must be at least equal to the maximal AR
  order. If it is longer, only the last \code{max(model@order)} elements
  are used.

}
\value{
  a numeric vector of length \code{n}. If \code{flag = TRUE} it has
  attribute \code{regimes} containing \code{z}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Todo}{
  Todo: Currently \code{init} has no default value but I am thinking
  about introducing such.

  Todo:

  (1) The default value of \code{nskip} may be better if set to zero.

  (2) Work with and/or return \code{ts} objects.

}
\examples{
exampleModels$WL_ibm
## simulate a continuation of BJ ibm data
ts1 <- mixAR_sim(exampleModels$WL_ibm, n = 30, init = c(346, 352, 357), nskip = 0)

## ts1 <- mixAR_sim(exampleModels$WL_ibm, n = 400, init = c(346, 352, 357), nskip = 0)
## plot(diff(ts(ts1)))

## plot(diff(mixAR_sim(exampleModels$WL_ibm_gen, n = 100, init = c(346, 352, 357), nskip = 0)),
##      type = "l")

# a simulation based estimate of the 1-step predictive distribution
# for the first date after the data.
s1 <- replicate(1000, mixAR_sim(exampleModels$WL_ibm, n = 1, init = c(346, 352, 357), nskip = 0))
plot(density(s1))

# load ibm data from BJ
## data(ibmclose, package = "fma")

# overlay the 'true' predictive density.
pdf1 <- mix_pdf(exampleModels$WL_ibm, xcond = as.numeric(fma::ibmclose))
curve(pdf1, add = TRUE, col = 'blue')

# estimate of 5\% quantile of predictive distribution
quantile(s1, 0.05)

# Monte Carlo estimate of "expected shortfall"
# (but the data has not been converted into returns...)
mean(s1[ s1 <= quantile(s1, 0.05) ])
}
\keyword{ts}
\keyword{datagen}
