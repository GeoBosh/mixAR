\name{mixAR-methods}
\docType{methods}
\alias{mixAR}
\alias{mixAR-methods}
\alias{mixAR,ANY-method}
\alias{mixAR,mixAR-method}

\title{Create mixAR objects}
\description{
  Generic function with methods for creating mixAR objects.
}
\usage{
mixAR(template, coef, ..., filler = NA_real_)
}

\arguments{
  \item{template}{
    
    an object to be used as a template for the new object, typically
    inheriting from \code{"mixAR"}. Alternatively, missing or a numeric
    vector specifying the order of the mixAR model, see Details.

  }
  \item{coef}{
    
    parameters for the new object a list with components
    \code{"arcoef"}, \code{"order"}, \code{"prob"},
    \code{"shift"}, and \code{"scale"}.

  }
  \item{\dots}{further arguments for methods.}
  \item{filler}{
    value for unspecified parameters, default is \code{NA_real_}.
  }
}
\details{

  \code{mixAR} provides an alternative to the function \code{new} for
  specifying mixAR models.

  If \code{template} is numeric vector, it is taken to specify the AR order
  of the model and the number of mixture components. A Gaussian mixAR
  model is created with parameters filled initially with NA's and then
  updated with values given by \code{coef}. \code{coef} does not need to
  have values for all parameters and may be missing altogether. If NA's
  are not suitable for initialisation, a suitable value can be specified
  with \code{filler}.

  If \code{template} is a mixAR object, then the new object will
  have the class of \code{template}. The new object is set
  initially to a copy of \code{template} and then updated with
  parameters specified by \code{coef} (and maybe others for some
  methods). 

  In principle, the numeric parameters are vectors of length the number of
  components of the mixAR model. For convenience, single values are
  replicated to the number of components. For this to work, at least one
  component must be specified completely, for example the order. It is
  an error for the parameters to imply conflicting number of components.  
  

  
}

\section{Methods}{
\describe{

\item{\code{signature(template = "ANY")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(template = "mixAR")}}{
%%  ~~describe this method here~~
}
}}
\seealso{
	class \code{\linkS4class{mixARGaussian}},
	class \code{\linkS4class{mixARgen}}
}
\examples{
mixAR(coef = list(prob = c(.5,.5), scale = c(1,2), 
                  arcoef = list(.5, 1.1), shift = c(0,0), order = c(1,1)))

mixAR(template = c(1,1))
mixAR(coef = list(order = c(1,1))) # same

m2 <- new("mixARGaussian", order = c(3, 2, 1),
          arcoef = matrix(c(1:3, c(1:2, 0), c(1, 0, 0)), nrow = 3, byrow = TRUE))
m2a <- mixAR(m2, list(prob = c(0.5, 0.25, 0.25)))
show_diff(m2, m2a)
}
\keyword{methods}
\keyword{mixAR}
