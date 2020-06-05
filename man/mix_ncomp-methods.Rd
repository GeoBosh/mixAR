\name{mix_ncomp-methods}
\docType{methods}
\alias{mix_ncomp}
\alias{mix_ncomp-methods}
\alias{mix_ncomp,MixAR-method}
\alias{mix_ncomp,MixComp-method}
\title{
  Number of rows or columns of a MixComp object
}
\description{
  
  Function and methods to get the number of component in a mixture
  object. For \code{"MixComp"} objects this is equivalent to
  \code{ncol}.

}
\usage{
mix_ncomp(x)
}
\arguments{
  \item{x}{an object, such as \code{"MixComp"} or  \code{"MixAR"}.}
}

\value{
  a number
}

\author{Georgi N. Boshnakov}

\section{Methods}{
\describe{

\item{\code{signature(x = "MixAR")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "MixComp")}}{
%%  ~~describe this method here~~
}
}}
\seealso{
  \code{\link{MixComp-class}},
  \code{\link{MixAR-class}}
}
\keyword{methods}
\keyword{MixComp}
