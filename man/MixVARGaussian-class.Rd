\name{MixVARGaussian-class}
\Rdversion{1.1}
\docType{class}
\alias{MixVARGaussian-class}
\alias{MixVARGaussian}
% \alias{mix_cdf,MixARGaussian,numeric,numeric,missing-method}
% \alias{mix_cdf,MixARGaussian,numeric,missing,numeric-method}
% \alias{mix_cdf,MixARGaussian,missing,missing,numeric-method}
% \alias{mix_pdf,MixARGaussian,numeric,numeric,missing-method}
% \alias{mix_pdf,MixARGaussian,numeric,missing,numeric-method}
% \alias{mix_pdf,MixARGaussian,missing,missing,numeric-method}

\title{
  MixVAR models with multivariate Gaussian noise components
}
\description{
  Class MixVARGaussian represents MixAR models with multivariate
  Gaussian noise components.
}
\section{Objects from the Class}{

  Objects can be created by calls of the form
  \code{new("MixVARGaussian", ...)}, giving the elements of the model as
  named arguments, see the examples below.

  \code{arcoef} contains the AR coefficients, one numeric array for
  each mixture component. It can be given as a
  \code{\linkS4class{raggedCoefV}} object or as a list of numeric
  arrays.

  The AR order of the model is inferred from \code{arcoef}
  argument. If argument \code{order} is given, it is checked for
  consistency with \code{arcoef}.  The \code{shift} slot defaults to a
  matrix of zeroes and the \code{vcov} slot to an array of 
  identity matrices, one for each component.

  The distribution of the noise components is standard multivariate
  Gaussian, N(0,1).

}
\section{Slots}{
  All slots except \code{arcoef} are numeric vectors of length
  equal to the number of components in the model.
  \describe{
    \item{\code{prob}:}{
      probabilities of the mixture components,
    }
    \item{\code{order}:}{
      AR orders of the components,
    }
    \item{\code{shift}:}{
      the shift (intercept) terms of the AR components,
    }
    \item{\code{vcov}:}{
      covariance matrices of the noise terms of the AR components,
    }
    \item{\code{arcoef}:}{
      The AR components, object of class \code{"raggedCoefV"}.
    }
  }
}
\section{Extends}{
Class \code{"\linkS4class{MixAR}"}, directly.
}
\section{Methods}{
  \describe{

    \item{fit_mixAR}{\code{signature(x = "ANY", model =	"MixARGaussian")}:
      ... 
    }
  }
}


\author{Davide Ravagli}
% \note{
% }

%%  ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  %	~~objects to See Also as \code{\link{~~fun~~}}, ~~~
  \code{\linkS4class{MixAR}}
}
\examples{
showClass("MixVARGaussian")

## Create array of covariance matrices
Sigma1 <- cbind(c(0.0013, 0.0011), c(0.0011, 0.0012))
Sigma2 <- cbind(c(0.0072, 0.0047), c(0.0047, 0.0039))
Sigma  <- array(c(Sigma1, Sigma2), dim=c(2,2,2))

## Create list of AR coefficients
AR <- list()
AR[[1]] <- array(c(0.0973, -0.0499,  0.2927,  0.4256,  ## VAR(2;4)
                  -0.0429,  0.0229, -0.1515, -0.1795,
                  -0.0837, -0.1060, -0.1530,  0.1947,
                  -0.1690, -0.0903,  0.1959,  0.0955), dim=c(2,2,4))
AR[[2]] <- array(c(0.3243,  0.2648,  0.4956,  0.2870,  ## VAR(2;3)
                  -0.1488,  0.0454, -0.0593, -0.3629,
                   0.1314,  0.0274,  0.0637,  0.0485), dim=c(2,2,3))

## Create vector of mixing weights
prob <- c(0.6376, 0.3624)

## Create matrix of shift parameters
shift <- cbind(c(0.0044, 0.0020), c(-0.0039, -0.0014))

## Build "MixVARGaussian" model
new("MixVARGaussian", prob=prob, vcov=Sigma, arcoef=AR, shift=shift)
}
\keyword{classes}
\keyword{MixVAR}
