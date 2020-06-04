\name{initialize-methods}
\docType{methods}
\alias{initialize-methods}
\alias{initialize,mixAR-method}
\alias{initialize,raggedCoef-method}
\alias{initialize,raggedCoefS-method}
\alias{initialize,mixARgen-method}
\alias{initialize,raggedCoefV-method}
\alias{initialize,mixVAR-method}
\title{ Methods for function \code{initialize} in package mixAR }
\description{
 Methods for function \code{initialize} in package mixAR.
}
\section{Methods}{

  These methods are for internal use by the user-level function
  \code{new()} to create objects from the corresponding classes.  The
  creation of objects from a class and examples can be found in the
  description of the corresponding class.

\describe{

\item{\code{signature(.Object = "mixAR")}}{
  Objects from class \code{mixAR} cannot be created since it is virtual.
  This method is called by initialisation methods of non-virtual
  subclasses of \code{mixAR} to set up its slots.
}

\item{\code{signature(.Object = "raggedCoef")}}{
  Creates objects from class \code{\linkS4class{raggedCoef}}.
}
\item{\code{signature(.Object = "mixARgen")}}{{
  Creates objects from class \code{mixARgen}.
}
}}}
\keyword{methods}
\keyword{internal}
