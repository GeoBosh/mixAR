\name{MixVAR-class}
\Rdversion{1.1}
\docType{class}
\alias{MixVAR-class}
% \alias{mix_ek,MixAR,numeric,numeric,missing,missing-method}
% \alias{mix_ek,MixAR,numeric,numeric,missing,logical-method}
% \alias{mix_ek,MixAR,numeric,missing,numeric,missing-method}
% \alias{mix_ek,MixAR,numeric,missing,numeric,logical-method}
% \alias{mix_hatk,MixAR,numeric,numeric,missing-method}

\title{Class \code{"MixVAR"} --- mixture vector autoregressive models }
\description{Mixture vector autoregressive models}
\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.

  Derived classes add distribution properties, e.g. use
  \code{\linkS4class{MixVARGaussian}} for MixVAR models with Gaussian
  error components.
}
\section{Slots}{
  \describe{
    \item{\code{prob}:}{
      the mixing probabilities,
      an object of class \code{"numeric"}
    }
    \item{\code{order}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{shift}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{vcov}:}{Object of class \code{"array"} ~~ }
    \item{\code{arcoef}:}{Object of class \code{"raggedCoefV"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{fit_mixVAR}{\code{signature(x = "ANY", model = "MixAR")}: ... }
  }
}
%\references{ ~put references to the literature/web site here ~ }
\author{Davide Ravagli}
% \note{ ~~further notes~~ }

%%  ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	% ~~objects to See Also as \code{\link{}}, ~~~
	\code{\linkS4class{MixVARGaussian}}
}
%\examples{
%showClass("MixAR")
%}
\keyword{classes}
\keyword{MixVAR}

