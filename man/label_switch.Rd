\name{label_switch}
\alias{label_switch}
\title{A posteriori relabelling of a Markov chain}
\description{
  Takes the output from a MCMC simulation of parameters of a mixture,
  and detects whether labels switch has occured while sampling, using
  the method by Celeux (2000).
}
\usage{
label_switch(x, m)
}
\arguments{
  \item{x}{
    output from an MCMC sampling of a mixture. A \code{matrix}, each
    column corresponds to one component of the mixture.
  }
  \item{m}{
    the number of observations in the sample that will be used to
    initialise the algorithm. \code{m~100} is recommended.
  }
}
\details{

  Function can be directly executed when \code{x} is one of
  \code{mix_weights}, \code{scale}, \code{precision}, \code{shift} or
  \code{mu} from \code{bayes_mixAR} output.  \code{ARcoeff} cannot be input
  as it is, but element from the list may be used.

}
\value{A list of 2:
  \item{x}{The input matrix, with adjusted labels}
  \item{true_perm}{The "true" permutation at each iteration.}
}
\references{
  \insertRef{Celeux2000}{mixAR}

}
\author{Davide Ravagli}
\note{
  There is no absolute choice on what \code{x} should be to obtain the
  "true" permutation at any given point. User is subject to make the
  most suitable choice, given output of their MCMC.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{bayes_mixAR}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
model <- new("MixARGaussian",
             prob   = exampleModels$WL_At@prob,      # c(0.5, 0.5)
             scale  = exampleModels$WL_At@scale,     # c(1, 2)        
             arcoef = exampleModels$WL_At@arcoef@a ) # list(-0.5, 1.1)

y <- mixAR_sim(model, n = 300, init = rep(0, which.max(model@order)))

## just examples, use larger numbers in practice
nsim <- 30   # 200
burnin <- 10  # 100
x <- bayes_mixAR(y, model, fix_shift = FALSE, tau = c(.15, .25),
              nsim = nsim, burnin = burnin)

label_switch(x$mix_weights, m = 5)
}
%\keyword{ labels }% use one of  RShowDoc("KEYWORDS")


