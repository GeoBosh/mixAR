\name{mix_ncomp-methods}
\docType{methods}
\alias{mix_ncomp}
\alias{mix_ncomp-methods}
\alias{mix_ncomp,mixAR-method}
\alias{mix_ncomp,mixComp-method}
\title{
  Number of rows or columns of a mixComp object
}
\description{
  
  Function and methods to get the number of component in a mixture
  object. For \code{"mixComp"} objects this is equivalent to
  \code{ncol}.

}
\usage{
mix_ncomp(x)
}
\arguments{
  \item{x}{an object, such as \code{"mixComp"} or  \code{"mixAR"}.}
}

\value{
  a number
}

\author{Georgi N. Boshnakov}

\section{Methods}{
\describe{

\item{\code{signature(x = "mixAR")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "mixComp")}}{
%%  ~~describe this method here~~
}
}}
\seealso{
  \code{\link{mixComp-class}},
  \code{\link{mixAR-class}}
}
\keyword{methods}
\keyword{mixComp}
