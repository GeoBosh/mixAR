\name{initialize-methods}
\docType{methods}
\alias{initialize-methods}
\alias{initialize,MixAR-method}
\alias{initialize,raggedCoef-method}
\alias{initialize,raggedCoefS-method}
\alias{initialize,MixARgen-method}
\alias{initialize,raggedCoefV-method}
\alias{initialize,MixVAR-method}
\title{ Methods for function \code{initialize} in package \pkg{mixAR} }
\description{
 Methods for function \code{initialize} in package \pkg{mixAR}.
}
\section{Methods}{

  These methods are for internal use by the user-level function
  \code{new()} to create objects from the corresponding classes.  The
  creation of objects from a class and examples can be found in the
  description of the corresponding class.

\describe{

\item{\code{signature(.Object = "MixAR")}}{
  Objects from class \code{MixAR} cannot be created since it is virtual.
  This method is called by initialisation methods of non-virtual
  subclasses of \code{MixAR} to set up its slots.
}

\item{\code{signature(.Object = "raggedCoef")}}{
  Creates objects from class \code{"\linkS4class{raggedCoef}"}.
}
\item{\code{signature(.Object = "MixARgen")}}{{
  Creates objects from class \code{MixARgen}.
}
}}}
\keyword{methods}
\keyword{internal}
