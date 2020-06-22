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

  for \code{sampZpi}, a list containing the following elements:
  
  \item{mix_weights}{
   \code{matrix} with \code{nrow = nsim} and \code{ncol = g}: 
   sampled mixing weights.
  }
  
  \item{latentZ}{
    \code{matrix} with \code{nrow = n - p} and \code{ncol = g}, \code{n} equal to
    \code{length(y)} and \code{p} equal to \code{max(pk}: 
    the simulated latent variables \code{Z_t} at the last of \code{nsim} iterations 
    (functional). Specifically, each row contains \code{1} for exactly 
    one component, and is filled with \code{0}. 
    
  }
  
  \item{nk}{
    Vector of length \code{g}. Column sums of \code{latentZ}.
  }

  for \code{sampMuShift}, a list containing the following elements:
  
  \item{shift}{
    \code{matrix} with \code{nrow = nsim} and \code{ncol = g}: 
    simulated shift parameters, obtained by transformation of the
    means.
  }
  
  \item{mu}{
  \code{matrix} with \code{nrow = nsim} and \code{ncol = g}:
  simulated mean parameters.}

  for \code{sampSigmaTau}, a list containing the following elements:
  
  \item{scale}{
    \code{matrix} with \code{nrow = nsim} and \code{ncol = g}: 
    scale parameters, obtained by transformation
    of precisions.
  }
  
  \item{precision}{
    \code{matrix} with \code{nrow = nsim} and \code{ncol = g}: 
    precision parameters.
  }
  
  \item{lambda}{
    \code{numeric} vector of length \code{nsim} simulated values 
    of hyperparameter lambda, due to hierarchical setup.
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
model <- new("MixARGaussian",
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

set.seed(1234)
n <- 50 # 500
nsim <- 50

y <- mixAR_sim(model, n = n, init = 0)
x  <- sampZpi(y, pk, prob, shift, AR, sigma, nsim = nsim, d)
x1 <- sampMuShift(y, pk, prec, nk = x$nk, shift, z = x$latentZ, AR, nsim = nsim)
x2 <- sampSigmaTau(y, pk, prec, nk = x$nk, AR, mu = x1$mu, z = x$latentZ,
                   a = 0.2, c = 2, nsim = nsim)
}
%\keyword{ samp }% use one of  RShowDoc("KEYWORDS")
%\keyword{ weights }
%\keyword{ tau }% use one of  RShowDoc("KEYWORDS")
%\keyword{ sigma }
%\keyword{ mean }% use one of  RShowDoc("KEYWORDS")
%\keyword{ shift }
