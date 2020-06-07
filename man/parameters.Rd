\name{parameters}
\docType{methods}
\alias{parameters}
\alias{parameters-methods}
\alias{parameters,ANY-method}
\alias{parameters,MixAR-method}

\alias{parameters<-}
\alias{parameters<--methods}
\alias{parameters<-,ANY-method}
\alias{parameters<-,MixAR-method}

% deprecated:
\alias{set_parameters}
\alias{set_parameters-methods}
\alias{set_parameters,ANY-method}
\alias{set_parameters,MixAR-method}


\title{Set or extract the parameters of MixAR objects}
\description{Set or extract the parameters of MixAR objects.}
\usage{
parameters(model, namesflag = FALSE, drop = character(0))

parameters(model) <- value
\S4method{parameters}{MixAR}(model) <- value
\S4method{parameters}{ANY}(model) <- value
}
\arguments{
  \item{model}{a model.}
  \item{namesflag}{if TRUE, generate names.}
  \item{drop}{
    names of parameters not to include in the returned value, a
    character vector. The default is to return all parameters, see
    Details.
  }
  \item{value}{values of the parameters, numeric.}
}
\details{
  This is a generic function. The dispatch is on argument \code{model}.
  The default calls \code{coef}.

  \code{parameters} extracts the parameters of a MixAR object. It
  returns a numeric vector. If \code{namesflag} is \code{TRUE} the
  returned vector is named, so that the parameters can be referred to by
  names. Argument \code{drop} is a character vector giving names of
  parameters not to be included in the returned value.

  This function can be useful for setting parameters from optimisation
  routines.
  
  \code{set_parameters} is deprecated,
  use \code{parameters(model) <- value} instead.
  
  

%  todo: (1) currently there is no specific method for class \code{"MixARgen"}.
%
%        (2) related to (1) above - what is the intention? are parameters
%        of the noise distributions parameters in the sense of this
%        function?
%
%        (3) clarify the difference with \code{lik_params}. Is it only in
%        that one of the component probabilities is omitted in
%        \code{lik_params}?
}

\value{
  a vector of parameters, maybe with names.
}

\section{Methods}{
  \describe{

    \item{\code{signature(model = "ANY")}}{
      %%  ~~describe this method here~~
    }

    \item{\code{signature(model = "MixAR")}}{
      %%  ~~describe this method here~~
    }
}}
\examples{
parameters(exampleModels$WL_ibm)
parameters(exampleModels$WL_ibm, namesflag = TRUE)
## drop orders
parameters(exampleModels$WL_ibm, namesflag = TRUE, drop = "order")
## drop orders and mixing weights
parameters(exampleModels$WL_ibm, namesflag = TRUE, drop = c("order", "prob"))

parameters(exampleModels$WL_I,  namesflag = TRUE)
parameters(exampleModels$WL_II, namesflag = TRUE)
}
\keyword{methods}
\keyword{MixAR}
