\name{mix_pdf-methods}
\docType{methods}
%  "pdf" and "cdf" have methods with the same signatures and the same
%  meaning of the arguments. They are described only ones for each
%  signature.
%
% reprompt() will set only aliases for "pdf".
% The aliases for "cdf" are added manually by copying and pasting.
%
\alias{mix_pdf}
\alias{mix_pdf-methods}
\alias{mix_pdf,MixARGaussian,missing,missing,numeric-method}
\alias{mix_pdf,MixARGaussian,numeric,missing,numeric-method}
\alias{mix_pdf,MixARGaussian,numeric,numeric,missing-method}
\alias{mix_pdf,MixARgen,missing,missing,numeric-method}
\alias{mix_pdf,MixARgen,numeric,missing,numeric-method}
\alias{mix_pdf,MixARgen,numeric,numeric,missing-method}

\alias{mix_cdf}
\alias{mix_cdf-methods}
\alias{mix_cdf,MixARGaussian,missing,missing,numeric-method}
\alias{mix_cdf,MixARGaussian,numeric,missing,numeric-method}
\alias{mix_cdf,MixARGaussian,numeric,numeric,missing-method}
\alias{mix_cdf,MixARgen,missing,missing,numeric-method}
\alias{mix_cdf,MixARgen,numeric,missing,numeric-method}
\alias{mix_cdf,MixARgen,numeric,numeric,missing-method}

\title{Conditional pdf's and cdf's of MixAR models}
\description{
 Gives conditional probability densities and distribution functions of
 mixture autoregressive models. 
}
\section{Methods}{

  \code{mix_pdf} gives a probability density, \code{mix_cdf} a
  distribution function. If argument \code{x} is supplied, the functions
  are evaluated for the specified values of \code{x}, otherwise function
  objects are returned and can be used for further computations, eg for
  graphs. 
  
  
  \code{mix_pdf} and \code{mix_cdf} have methods with the following
  signatures. 

  \describe{
    \item{\code{signature(model = "MixARGaussian", x = "missing", index = "missing", xcond = "numeric")}}{

      Return (as a function of one argument) the conditional density
      (respectively cdf), \eqn{f(x|xcond)}, of \eqn{X_{t+1}}{X[t+1]} given
      the past values \code{xcond}. The values in \code{xcond} are in
      natural time order, e.g. the last value in \code{xcond} is
      \eqn{x_{t}}{x[t]}. \code{xcond} must contain enough values for the
      computation of the conditional density (cdf) but if more are given,
      only the necessary ones are used.

    }
    \item{\code{signature(model = "MixARGaussian", x = "numeric", index = "missing", xcond = "numeric")}}{
      Compute the conditional density (respectively cdf) at the values given
      by \code{x}.
    }
    \item{\code{signature(model = "MixARGaussian", x = "numeric", index = "numeric", xcond = "missing")}}{

      Compute conditional densities (respectively cdf) for times
      specified in \code{index}. For each \eqn{t\in{}}\code{index} the
      past values needed for the computation of the pdf (cdf) are
      \code{\ldots,x[t-2],x[t-1]}.

    }
  \item{\code{signature(model = "MixARgen", x = "missing", index = "missing",
          xcond = "numeric")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(model = "MixARgen", x = "numeric", index = "missing",
          xcond = "numeric")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(model = "MixARgen", x = "numeric", index = "numeric",
          xcond = "missing")}}{{
%%  ~~describe this method here~~
}
}}
}
\author{Georgi N. Boshnakov}
\seealso{
  \code{\link{mix_moment}} for examples and computation of summary statistics of the
  predictive distributions 
}
\keyword{methods}
\keyword{MixAR}
\keyword{prediction}

       % \eqn{\;\;\;\;\;\;}{ }       % \verb{ }
       % \eqn{\;}{ }
       %\eqn{\;\;\;\;\;\;\;\;\;\;\;\;\;}{ }
       % this  works, but R puts it on its own line
       % \eqn{\;\;\;\;\;\;\;\;\;\;\;\;\;}{ }
       % \eqn{\;}{ }
       % this  works, but R puts quotes around the spaces
       % \verb{ }\cr \samp{       }
