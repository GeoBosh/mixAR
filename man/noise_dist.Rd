\name{noise_dist}
\alias{get_edist}
\alias{noise_dist}
\alias{noise_rand}
\alias{noise_params}
\alias{set_noise_params}

\title{Internal mixAR Functions}
\description{
  Functions for the distributions of the components of mixAR models
}
\usage{
get_edist(model)
noise_dist(model, what, expand = FALSE)
noise_rand(model, expand = FALSE)
noise_params(model)
set_noise_params(model, nu)
}
\arguments{
  \item{model}{a model.}
  \item{what}{the property, a character string.}
  \item{expand}{if TRUE, expand the list to length equal to the number
    of components, see Details.}
  \item{nu}{degrees of freedom.}
}
\details{

  \code{get_edist} gives the distributions of the noise components of
  \code{model}.
  \code{noise_dist} gives property \code{what} of the noise
  distribution.
  \code{noise_rand} gives a list of functions for simulation from the
  component distributions.

  In each case, the list contains one element for each component but if
  it is of length one, then the only element is common for all
  components.  To force a complete list even in this case, use
  \code{expand = TRUE}.

  \code{noise_params} gives the parameters of the model as a numeric
  vector. (todo: currently returns \code{list{}} when there are no such
  parameters (e.g. if the components are standard normal) but shouldn't
  this be \code{numeric(0)}?)

  The distribution is specified as a list. Element "dist" contain the
  distribution. Element "generator" is a function that generates a
  distribution like the one specified. If "dist" is absent or NULL, the
  generator is called to generate a distribution object.

  Initially the distribution itself was used for slot \code{dist}. For
  compatibility with old code using that format, this is still
  supported.

  todo: this needs consolidation.
}
\seealso{
  \code{\link{fdist_stdnorm}},
  \code{\link{fdist_stdt}},
  \code{\link{fn_stdt}}.
}
\keyword{mixAR}
