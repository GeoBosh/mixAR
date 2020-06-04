\name{mixAR-class}
\Rdversion{1.1}
\docType{class}
\alias{mixAR-class}
% \alias{mix_ek,mixAR,numeric,numeric,missing,missing-method}
% \alias{mix_ek,mixAR,numeric,numeric,missing,logical-method}
% \alias{mix_ek,mixAR,numeric,missing,numeric,missing-method}
% \alias{mix_ek,mixAR,numeric,missing,numeric,logical-method}
% \alias{mix_hatk,mixAR,numeric,numeric,missing-method}

\title{Class "mixAR" --- mixture autoregressive models }
\description{Mixture autoregressive models}
\section{Objects from the Class}{
  A virtual Class: No objects can be created from it.

  Derived classes add distribution properties, e.g. use
  \code{\linkS4class{mixARGaussian}} for MixAR models with Gaussian
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
    \item{fit_mixAR}{\code{signature(x = "ANY", model = "mixAR", init = "list")}: ... }
    \item{fit_mixAR}{\code{signature(x = "ANY", model = "mixAR", init = "missing")}: ... }
    \item{fit_mixAR}{\code{signature(x = "ANY", model = "mixAR", init = "mixAR")}: ... }
    \item{fit_mixAR}{\code{signature(x = "ANY", model = "mixAR", init = "numeric")}: ... }
    \item{fit_mixAR}{\code{signature(x = "ANY", model = "mixARGaussian", init = "mixAR")}: ... }
    \item{get_edist}{\code{signature(model = "mixAR")}: ... }
    \item{initialize}{\code{signature(.Object = "mixAR")}: ... }
    \item{lik_params}{\code{signature(model = "mixAR")}: ... }
    \item{make_fcond_lik}{\code{signature(model = "mixAR", ts = "numeric")}: ... }
    \item{mix_ek}{\code{signature(model = "mixAR", x = "numeric", index = "numeric", xcond = "missing", scale = "missing")}: ... }
    \item{mix_ek}{\code{signature(model = "mixAR", x = "numeric", index = "numeric", xcond = "missing", scale = "logical")}: ... }
    \item{mix_ek}{\code{signature(model = "mixAR", x = "numeric", index = "missing", xcond = "numeric", scale = "missing")}: ... }
    \item{mix_ek}{\code{signature(model = "mixAR", x = "numeric", index = "missing", xcond = "numeric", scale = "logical")}: ... }
    \item{mix_hatk}{\code{signature(model = "mixAR", x = "numeric", index = "numeric", xcond = "missing")}: ... }
    \item{mix_ncomp}{\code{signature(x = "mixAR")}: ... }
    \item{mixAR}{\code{signature(template = "mixAR")}: ... }
    \item{noise_dist}{\code{signature(model = "mixAR")}: ... }
    \item{noise_params}{\code{signature(model = "mixAR")}: ... }
    \item{noise_rand}{\code{signature(model = "mixAR")}: ... }
    \item{parameters}{\code{signature(model = "mixAR")}: ... }
    \item{row_lengths}{\code{signature(x = "mixAR")}: ... }
  }
}
%\references{ ~put references to the literature/web site here ~ }
\author{Georgi N. Boshnakov}
% \note{ ~~further notes~~ }

%%  ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	% ~~objects to See Also as \code{\link{}}, ~~~
	\code{\linkS4class{mixARGaussian}}
}
%\examples{
%showClass("mixAR")
%}
\keyword{classes}
\keyword{mixAR}
