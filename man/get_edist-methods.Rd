\name{get_edist-methods}
\docType{methods}
\alias{get_edist-methods}
\alias{get_edist,MixAR-method}
\alias{get_edist,MixARGaussian-method}
\alias{get_edist,MixARgen-method}
\title{ Methods for function \code{get_edist} in package \pkg{mixAR} }
\description{
  Methods for function \code{get_edist} in package \pkg{mixAR}
}
\section{Methods}{
  \code{get_edist} gives the error (or noise) distribution of MixAR
  objects.

  Currently the distribution is returned as a list of functions. The
  list contains one element for each component. If the error
  distributions of all components are the same, then the list may
  contain a single element representing the common error distribution.

  Note that the distribution is not necessarily stored in slot
  \code{dist} in this format, see the description of this slot in class
  \code{"\linkS4class{MixARgen}"}.
  Such a slot may even not exist if the distribution of the error
  components is fixed as is the case for class \code{MixARGaussian}.

  Each subclass of \code{MixAR} needs to define a method for
  \code{get_edist}.


  \describe{

    \item{\code{signature(model = "MixAR")}}{
      Issue an error message and stop.

      This method is invoked for subclasses of \code{MixAR} that have
      not defined their own method for \code{get_edist}. This is an
      error.

    }

    \item{\code{signature(model = "MixARGaussian")}}{
      Return an object representing the fact that the error
      distributions of the components of \code{MixARGaussian} objects
      are standard normal.
    }

    \item{\code{signature(model = "MixARgen")}}{
      Return an object representing the error distributions of the
      components of \code{MixARgen} objects. The distributions are not
      necessarilly the same for such objects.
    }
}}
\keyword{methods}
