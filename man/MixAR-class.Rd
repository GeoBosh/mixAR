\name{MixAR-class}
\Rdversion{1.1}
\docType{class}
\alias{MixAR-class}
% \alias{mix_ek,MixAR,numeric,numeric,missing,missing-method}
% \alias{mix_ek,MixAR,numeric,numeric,missing,logical-method}
% \alias{mix_ek,MixAR,numeric,missing,numeric,missing-method}
% \alias{mix_ek,MixAR,numeric,missing,numeric,logical-method}
% \alias{mix_hatk,MixAR,numeric,numeric,missing-method}

\title{Class \code{"MixAR"} --- mixture autoregressive models }
\description{Mixture autoregressive models}
\section{Objects from the Class}{
  A virtual Class: No objects can be created from it.

  Derived classes add distribution properties, e.g. use
  \code{\linkS4class{MixARGaussian}} for MixAR models with Gaussian
  error components.
}
\section{Slots}{
  \describe{
    \item{\code{prob}:}{
      the mixing probabilities,
      an object of class \code{"numeric"}
    }
    \item{\code{order}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{shift}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{scale}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{arcoef}:}{Object of class \code{"raggedCoef"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{fit_mixAR}{\code{signature(x = "ANY", model = "MixAR", init = "list")}: ... }
    \item{fit_mixAR}{\code{signature(x = "ANY", model = "MixAR", init = "missing")}: ... }
    \item{fit_mixAR}{\code{signature(x = "ANY", model = "MixAR", init = "MixAR")}: ... }
    \item{fit_mixAR}{\code{signature(x = "ANY", model = "MixAR", init = "numeric")}: ... }
    \item{fit_mixAR}{\code{signature(x = "ANY", model = "MixARGaussian", init = "MixAR")}: ... }
    \item{get_edist}{\code{signature(model = "MixAR")}: ... }
    \item{initialize}{\code{signature(.Object = "MixAR")}: ... }
    \item{lik_params}{\code{signature(model = "MixAR")}: ... }
    \item{make_fcond_lik}{\code{signature(model = "MixAR", ts = "numeric")}: ... }
    \item{mix_ek}{\code{signature(model = "MixAR", x = "numeric", index = "numeric", xcond = "missing", scale = "missing")}: ... }
    \item{mix_ek}{\code{signature(model = "MixAR", x = "numeric", index = "numeric", xcond = "missing", scale = "logical")}: ... }
    \item{mix_ek}{\code{signature(model = "MixAR", x = "numeric", index = "missing", xcond = "numeric", scale = "missing")}: ... }
    \item{mix_ek}{\code{signature(model = "MixAR", x = "numeric", index = "missing", xcond = "numeric", scale = "logical")}: ... }
    \item{mix_hatk}{\code{signature(model = "MixAR", x = "numeric", index = "numeric", xcond = "missing")}: ... }
    \item{mix_ncomp}{\code{signature(x = "MixAR")}: ... }
    \item{mixAR}{\code{signature(template = "MixAR")}: ... }
    \item{noise_dist}{\code{signature(model = "MixAR")}: ... }
    \item{noise_params}{\code{signature(model = "MixAR")}: ... }
    \item{noise_rand}{\code{signature(model = "MixAR")}: ... }
    \item{parameters}{\code{signature(model = "MixAR")}: ... }
    \item{row_lengths}{\code{signature(x = "MixAR")}: ... }
  }
}
%\references{ ~put references to the literature/web site here ~ }
\author{Georgi N. Boshnakov}
% \note{ ~~further notes~~ }

%%  ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\linkS4class{MixARGaussian}},
  \code{\linkS4class{MixARgen}}
}
%\examples{
%showClass("MixAR")
%}
\keyword{classes}
\keyword{MixAR}
