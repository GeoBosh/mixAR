\name{MixARGaussian-class}
\Rdversion{1.1}
\docType{class}
\alias{MixARGaussian-class}
\alias{MixARGaussian}
% \alias{mix_cdf,MixARGaussian,numeric,numeric,missing-method}
% \alias{mix_cdf,MixARGaussian,numeric,missing,numeric-method}
% \alias{mix_cdf,MixARGaussian,missing,missing,numeric-method}
% \alias{mix_pdf,MixARGaussian,numeric,numeric,missing-method}
% \alias{mix_pdf,MixARGaussian,numeric,missing,numeric-method}
% \alias{mix_pdf,MixARGaussian,missing,missing,numeric-method}

\title{mixAR models with Gaussian noise components}
\description{
  Class \code{"MixARGaussian"} represents MixAR models with Gaussian
  noise components.
}
\section{Objects from the Class}{

  Objects can be created by calls of the form \code{new("MixARGaussian",
  ...)}, giving the elements of the model as named arguments, see the
  examples below. All elements of the model, except \code{arcoef}, are
  simple numeric vectors. From version 0.19-15 of package MixAR it is
  possible to create objects using \code{MixARGaussian(...)}. The two
  forms are completely equivalent.

  \code{arcoef} contains the AR coefficients, one numeric vector for
  each mixture component. It can be given as a
  \code{"\linkS4class{raggedCoef}"} object or as a list of numeric
  vectors.

  To input a model with seasonal AR coefficients, argument passed to \code{arcoef} can be passed as a \code{raggedCoefS} object, or as a \code{list} of three elements.
  For the latter, seasonality \code{s} must be explicitly indicated.
  AR coefficients can be given as \code{list} or \code{matrix} within the main list (one for main AR coefficients, named \code{a}, and one for seasonal AR coefficients, \code{as}). Each row of a input matrix/element of the list denotes one component of the mixture.
  If not named, initialisation takes the first passed element to be \code{a} and the second to be \code{as}.

  The AR order of the model is inferred from \code{arcoef}
  argument. If argument \code{order} is given, it is checked for
  consistency with \code{arcoef}.  The \code{shift} slot defaults to a
  vector of zeroes and the \code{scale} slot to a vector of ones.

  The distribution of the noise components is standard Gaussian, N(0,1).
}
\section{Slots}{
  All slots except \code{arcoef} are numeric vectors of length
  equal to the number of components in the model.
  \describe{
    \item{\code{prob}:}{probabilities of the mixture components}
    \item{\code{order}:}{AR orders of the components}
    \item{\code{shift}:}{the shift (intercept) terms of the AR components}
    \item{\code{scale}:}{the standard deviations of the noise terms of the AR components}
    \item{\code{arcoef}:}{The AR components, object of class \code{"raggedCoef"}}
  }
}
\section{Extends}{
Class \code{"\linkS4class{MixAR}"}, directly.
}
\section{Methods}{
  \describe{
    \item{mix_cdf}{%
      \code{signature(model = "MixARGaussian", x = "numeric", index = "numeric", xcond = "missing")}:
      ... }
    \item{mix_cdf}{%
      \code{signature(model = "MixARGaussian", x = "numeric", index = "missing", xcond = "numeric")}:
      ... }

    \item{fit_mixAR}{\code{signature(x = "ANY", model = "MixARGaussian", init = "MixAR")}: ... }
    \item{get_edist}{\code{signature(model = "MixARGaussian")}: ... }
    \item{mix_cdf}{\code{signature(model = "MixARGaussian", x = "missing", index = "missing", xcond = "numeric")}: ... }
    \item{mix_pdf}{\code{signature(model = "MixARGaussian", x = "missing", index = "missing", xcond = "numeric")}: ... }
    \item{mix_pdf}{\code{signature(model = "MixARGaussian", x = "numeric", index = "missing", xcond = "numeric")}: ... }
    \item{mix_pdf}{\code{signature(model = "MixARGaussian", x = "numeric", index = "numeric", xcond = "missing")}: ... }
    \item{noise_dist}{\code{signature(model = "MixARGaussian")}: ... }
    \item{noise_rand}{\code{signature(model = "MixARGaussian")}: ... }}
}

\author{Georgi N. Boshnakov}
% \note{
% }

%%  ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  classes
  \code{"\linkS4class{MixARgen}"},
  \code{"\linkS4class{MixAR}"}
}
\examples{
showClass("MixARGaussian")

## load ibm data from BJ
## data(ibmclose, package = "fma")

## compute a predictive density, assuming exampleModels$WL_ibm model
## for the first date after the end of the data
pdf1 <- mix_pdf(exampleModels$WL_ibm, xcond = as.numeric(fma::ibmclose))

## plot the predictive density
## (cdf is used to determine limits on the x-axis)
cdf1 <- mix_cdf(exampleModels$WL_ibm, xcond = as.numeric(fma::ibmclose))
gbutils::plotpdf(pdf1, cdf = cdf1, lq = 0.001, uq = 0.999)
}
\keyword{classes}
\keyword{MixAR}
