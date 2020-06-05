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
\alias{mix_pdf,mixARGaussian,missing,missing,numeric-method}
\alias{mix_pdf,mixARGaussian,numeric,missing,numeric-method}
\alias{mix_pdf,mixARGaussian,numeric,numeric,missing-method}
\alias{mix_pdf,mixARgen,missing,missing,numeric-method}
\alias{mix_pdf,mixARgen,numeric,missing,numeric-method}
\alias{mix_pdf,mixARgen,numeric,numeric,missing-method}

\alias{mix_cdf}
\alias{mix_cdf-methods}
\alias{mix_cdf,mixARGaussian,missing,missing,numeric-method}
\alias{mix_cdf,mixARGaussian,numeric,missing,numeric-method}
\alias{mix_cdf,mixARGaussian,numeric,numeric,missing-method}
\alias{mix_cdf,mixARgen,missing,missing,numeric-method}
\alias{mix_cdf,mixARgen,numeric,missing,numeric-method}
\alias{mix_cdf,mixARgen,numeric,numeric,missing-method}

\title{Conditional pdf's and cdf's of mixAR models}
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
    \item{\code{signature(model = "mixARGaussian", x = "missing", index = "missing", xcond = "numeric")}}{

      Return (as a function of one argument) the conditional density
      (respectively cdf), \eqn{f(x|xcond)}, of \eqn{X_{t+1}}{X[t+1]} given
      the past values \code{xcond}. The values in \code{xcond} are in
      natural time order, e.g. the last value in \code{xcond} is
      \eqn{x_{t}}{x[t]}. \code{xcond} must contain enough values for the
      computation of the conditional density (cdf) but if more are given,
      only the necessary ones are used.

    }
    \item{\code{signature(model = "mixARGaussian", x = "numeric", index = "missing", xcond = "numeric")}}{
      Compute the conditional density (respectively cdf) at the values given
      by \code{x}.
    }
    \item{\code{signature(model = "mixARGaussian", x = "numeric", index = "numeric", xcond = "missing")}}{

      Compute conditional densities (respectively cdf) for times
      specified in \code{index}. For each \eqn{t\in{}}\code{index} the
      past values needed for the computation of the pdf (cdf) are
      \code{\ldots,x[t-2],x[t-1]}.

    }
  \item{\code{signature(model = "mixARgen", x = "missing", index = "missing",
          xcond = "numeric")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(model = "mixARgen", x = "numeric", index = "missing",
          xcond = "numeric")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(model = "mixARgen", x = "numeric", index = "numeric",
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
\keyword{mixAR}

       % \eqn{\;\;\;\;\;\;}{ }       % \verb{ }
       % \eqn{\;}{ }
       %\eqn{\;\;\;\;\;\;\;\;\;\;\;\;\;}{ }
       % this  works, but R puts it on its own line
       % \eqn{\;\;\;\;\;\;\;\;\;\;\;\;\;}{ }
       % \eqn{\;}{ }
       % this  works, but R puts quotes around the spaces
       % \verb{ }\cr \samp{       }
