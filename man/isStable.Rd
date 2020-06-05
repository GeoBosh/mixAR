\name{isStable}
\alias{isStable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Check if a MixAR model is stable}
\description{Checks if a MixAR model is stable. This is also the second
  order stationarity condition.}
\usage{
isStable(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the model}
}
\details{
  If each component of a MixAR model corresponds to a stable
  autoregression model, then the MixAR model is also stable. However,
  the MixAR model may be stable also when some of its components
  correspond to integrated or explosive AR models, see the references.
}
\value{
  True if the model is stable (second order stationary), FALSE otherwise.
}
\references{
  \insertRef{Boshnakov2011on1st2nd}{mixAR}

  \insertRef{WongLi2000}{mixAR}
}



%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
isStable(exampleModels$WL_I)
isStable(exampleModels$WL_II)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{MixAR}
