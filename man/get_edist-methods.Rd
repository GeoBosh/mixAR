\name{get_edist-methods}
\docType{methods}
\alias{get_edist-methods}
\alias{get_edist,mixAR-method}
\alias{get_edist,mixARGaussian-method}
\alias{get_edist,mixARgen-method}
\title{ ~~ Methods for Function \code{get_edist} in Package \pkg{mixAR} ~~}
\description{
 ~~ Methods for function \code{get_edist} in package \pkg{mixAR} ~~
}
\section{Methods}{
  \code{get_edist} gives the error (or noise) distribution of mixAR
  objects.

  Currently the distribution is returned as a list of functions. The
  list contains one element for each component. If the error
  distributions of all components are the same, then the list may
  contain a single element representing the common error distribution.

  Note that the distribution is not necessarily stored in slot
  \code{dist} in this format, see the description of this slot in class
  \code{\linkS4class{mixARgen}}.
  Such a slot may even not exist if the distribution of the error
  components is fixed as is the case for class \code{mixARGaussian}.

  Each subclass of \code{mixAR} needs to define a method for
  \code{get_edist}.


  \describe{

    \item{\code{signature(model = "mixAR")}}{
      Issue an error message and stop.

      This method is invoked for subclasses of \code{mixAR} that have
      not defined their own method for \code{get_edist}. This is an
      error.

    }

    \item{\code{signature(model = "mixARGaussian")}}{
      Return an object representing the fact that the error
      distributions of the components of \code{mixARGaussian} objects
      are standard normal.
    }

    \item{\code{signature(model = "mixARgen")}}{
      Return an object representing the error distributions of the
      components of \code{mixARgen} objects. The distributions are not
      necessarilly the same for such objects.
    }
}}
\keyword{methods}
