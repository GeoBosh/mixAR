\name{mix_ek}
\docType{methods}
\alias{mix_ek}
\alias{mix_ek-methods}

\alias{mix_ek,MixAR,numeric,missing,numeric,logical-method}
\alias{mix_ek,MixAR,numeric,missing,numeric,missing-method}
\alias{mix_ek,MixAR,numeric,numeric,missing,logical-method}
\alias{mix_ek,MixAR,numeric,numeric,missing,missing-method}
\title{
  Function and methods to compute component residuals for MixAR models
}
\description{
  Compute component residuals for MixAR models. 
}
\usage{
mix_ek(model, x, index, xcond, scale)
}
\arguments{
  \item{model}{a model.}
  \item{x}{time series.}
  \item{index}{
    a vector of positive integer specifying the indices for which to
    compute the residuls, has a natural default.
  }
  \item{xcond}{

    the past values needed for the conditional distribution, a numeric
    vector of length at least the maximal AR order of the components.

  }
  \item{scale}{
    logical or missing, if \code{TRUE} standardise the residuals.
  } 
}
\details{
  
  \code{mix_ek} computes component residuals from MixAR models.

  It is highly desirable to use it along with \code{\link{mix_hatk}} and
  the underlying function \code{\link{mixFilter}}. Doing this ensures
  transparent code and easy maintenance. Also, more efficient
  implementation can be introduced without changing other code.

}
\author{Georgi N. Boshnakov}
\section{Methods}{
  \describe{
    \item{\code{signature(model = "MixAR", x = "numeric", index = "missing",
	xcond = "numeric", scale = "logical")}}{
      %: ~~describe this method here
    }

    \item{\code{signature(model = "MixAR", x = "numeric", index = "missing",
	xcond = "numeric", scale = "missing")}}{
      %: ~~describe this method here
    }

    \item{\code{signature(model = "MixAR", x = "numeric", index = "numeric",
	xcond = "missing", scale = "logical")}}{
      %: ~~describe this method here
    }

    \item{\code{signature(model = "MixAR", x = "numeric", index = "numeric",
	xcond = "missing", scale = "missing")}}{
      %: ~~describe this method here
    }
}}
\seealso{
  \code{\link{mixFilter}} which is used by \code{mix_ek} to do the job,
  \code{\link{MixComp-class}} for easy manipulation of the returned
  object.
  
  class \code{"\linkS4class{MixAR}"}
}
\keyword{methods}
\keyword{MixAR}
