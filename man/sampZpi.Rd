\name{sampZpi}
\alias{sampZpi}
\alias{sampMuShift}
\alias{sampSigmaTau}
\title{
  Sampling functions for Bayesian analysis of mixture autoregressive
  models
}
\description{
  Sampling functions for Bayesian analysis of mixture autoregressive
  models. Draws observations from posterior distributions of the latent
  variables \code{Z_t}s and the parameters of mixture autoregressive
  models.
}
\usage{
sampZpi(y, pk, prob, mu, AR, sigma, nsim, d)
sampMuShift(y, pk, prec, nk, shift, z, AR, nsim)
sampSigmaTau(y, pk, prec, nk, AR, mu, z, a, c, nsim)
}
\arguments{
  \item{y}{a time series (currently a  \code{numeric} vector).}
  \item{pk}{
    \code{numeric} vector of length \code{g}. The autoregressive order
    of each component.
  }
  \item{prob}{
    \code{numeric} vector of length \code{g}. Current mixing weights.
  }
  \item{mu}{
    \code{numeric} vector of length \code{g}. Current mean parameters.
  }
    \item{shift}{
    \code{numeric} vector of length \code{g}. Current shift parameters.
  }
  \item{AR}{
    \code{list} of \code{g} elements. Autoregressive coefficients for
    each component.
  }
  \item{sigma}{
    \code{numeric} vector of length \code{g}. Current scale parameters
  }
  \item{nsim}{dessired sample size.}
  \item{d}{
    \code{numeric} vector of length \code{g}. Hyperparameters for
    Dirichlet prior. If missing, a vector of \code{1}s is generated. If
    length is \code{1}, creates a vector of length \code{g} with given
    number.
  }

  \item{prec}{
    \code{numeric} vector of length \code{g}. Current precision
    parameters.
  }
  \item{nk}{
    output from \code{sampZpi}. Component sum of latent variables Z_t.
  }
  \item{z}{
    output \code{latentZ} from \code{sampZpi}. A matrix containing the
    simulated latent variables.
  }

  \item{a,c}{hyperparameters.}
  
}
\details{

  \code{sampZpi} draws observations from posterior distributions of the
  latent variables \code{Z_t}s and mixing weights of a Mixture
  autoregressive model.

  \code{sampSigmaTau} draws observations from posterior distributions of
  the precisions \code{tau_k} of a Mixture autoregressive model, and
  obtains scales \code{sigma_k} by transformation.

  \code{sampMuShift} Draws observations from posterior distributions of
  the means \code{mu_k} of a Mixture autoregressive model, and obtains
  shifts \code{phi_k0} by transformation.
  
}
\value{
    
    \strong{TODO: \code{@Davide}:}
    Please give the full dimensions of the matrices. If more explanation
    is needed, do it in Details section. Running the examples below I get:
    \preformatted{
        > sapply(x, dim)
        $mix_weights
        [1] 100   2
        
        $latentZ
        [1] 499   2
        
        $nk
        NULL
    }


  for \code{sampZpi}, a list containing the following elements:
  \item{mix_weights}{
    \code{g} column matrix with sampled mixing weights.
  }
  \item{latentZ}{
    a matrix containing the simulated latent variables (functional).
  }
  \item{nk}{
    vector of length \code{g}. Component sum of latent variables \code{Z_t}.
  }

  for \code{sampMuShift}, a list containing the following elements:
  \item{shift}{
    \code{g} column matrix of shifts, obtained by transformation of the
    means.
  }
  \item{mu}{\code{g} column matrix of means.}

  for \code{sampSigmaTau}, a list containing the following elements:
  \item{scale}{
    \code{g} column matrix of scale parameters, obtained by transformation
    of precisions.
  }
  \item{precision}{
    \code{g} column matrix of precision parameters.
  }
  \item{lambda}{
    a vector of simulated values of hyperparameter lambda, due to
    hierarchical setup.
  }
}
%\references{
%%% ~put references to the literature/web site here ~
%}
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
model <- new("mixARGaussian",
             prob   = exampleModels$WL_At@prob,      # c(0.5, 0.5)
             scale  = exampleModels$WL_At@scale,     # c(1, 2)        
             arcoef = exampleModels$WL_At@arcoef@a ) # list(-0.5, 1.1)
             
prob  <- model@prob
sigma <- model@scale
prec  <- 1 / sigma ^ 2
g     <- length(model@prob)
d     <- rep(1, g)
pk    <- model@arcoef@p
p     <- max(pk)
shift <- mu <- model@shift
         
AR <- model@arcoef@a       

model

y <- mixAR_sim(model, n = 500, init = 0) # or, say, n = 250
x  <- sampZpi(y, pk, prob, shift, AR, sigma, nsim = 100, d)
x1 <- sampMuShift(y, pk, prec, nk = x$nk, shift, z = x$latentZ, AR, nsim = 100)
x2 <- sampSigmaTau(y, pk, prec, nk = x$nk, AR, mu = x1$mu, z = x$latentZ,
                   a = 0.2, c = 2, nsim = 100)
}
\keyword{ samp }% use one of  RShowDoc("KEYWORDS")
\keyword{ weights }
\keyword{ tau }% use one of  RShowDoc("KEYWORDS")
\keyword{ sigma }
\keyword{ mean }% use one of  RShowDoc("KEYWORDS")
\keyword{ shift }
