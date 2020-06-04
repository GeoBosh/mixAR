\name{row_lengths-methods}
\docType{methods}
\alias{row_lengths-methods}
\alias{row_lengths}

\alias{row_lengths,ANY-method}
\alias{row_lengths,mixAR-method}
\alias{row_lengths,raggedCoef-method}
\title{ Methods for Function row_lengths in Package `mixAR'}
\description{
  Determine the lengths of the `rows' of a ragged object.
}
\section{Methods}{
  Some objects in this package contain (effectively) lists of vectors.
  These vectors are considered `rows' and this function returns their
  lengths (as a vector).

\describe{

\item{\code{signature(x = "ANY")}}{
  The default method. Applies \code{length} to the elements of the
  argument (2020-03-28: now using \code{lengths(x)}).
}

\item{\code{signature(x = "raggedCoef")}}{
  Returns the lengths of the rows of the components, a numeric vector.
}

\item{\code{signature(x = "mixAR")}}{
  Returns the AR orders of the model components, a numeric vector.
}

}}
\keyword{methods}
