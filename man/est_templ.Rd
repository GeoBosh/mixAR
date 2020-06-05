\name{est_templ}
\alias{est_templ}
\title{Create estimation templates from MixAR model objects}
\description{Create estimation templates from MixAR model objects.}
\usage{
est_templ(model, shift = TRUE, ...)
}
\arguments{
  \item{model}{a \code{"MixAR"} object.}
  \item{shift}{logical, see Details.}
  \item{\dots}{currently not used.}
}
\details{

  Argument \code{model} is used as a template to specify values of
  parameters and/or which parameters to estimate or fix. In general,
  If a value of a parameter in \code{model} is \code{NA}, then it is to
  be estimated. Otherwise the parameter is taken as is.
  
  The current implementation is incomplete. In particular, the AR
  parameters are always designated for estimation.

  Argument \code{shift} can be used to overwrite some or values
  component \code{shift} in \code{model}.  If \code{shift} has length
  one, it is replicated to the number of MixAR components. If
  \code{shift[k]} is \code{TRUE}, then the shift coefficient for the
  k-th component is set to \code{NA} to request its
  estimation. Otherwise, the value of the shift for the k-th component
  in \code{model} is taken.

  Argument \code{shift} has a default of \code{TRUE} which causes the
  shift coefficients to be estimated irrespectively of their values in
  model. 

  \code{est_templ} returns a list with as many components as there are
  MixAR components in the model. The k-th component of the list is itself a list
  specifing which parameters of the i-th MixAR component to estimate or fix. 
}
\value{
  a list, as described in Details. 
}
\examples{
exampleModels$WL_A
est_templ(exampleModels$WL_A)
est_templ(exampleModels$WL_A, shift = FALSE)

exampleModels$WL_I
est_templ(exampleModels$WL_I)
}
%\keyword{}
