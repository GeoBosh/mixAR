\name{make_fcond_lik-methods}
\docType{methods}
\alias{make_fcond_lik}
\alias{make_fcond_lik-methods}
\alias{make_fcond_lik,mixAR,numeric-method}
\title{
  Create a function for computation of conditional likelihood
}
\description{
  
  Create a function for the computation of the conditional likelihood of
  mixAR models for a given time series.  The methods for this generic
  function defined in package \pkg{mixAR} are described here.

}
\usage{
make_fcond_lik(model, ts)
}
\arguments{
  \item{model}{a \code{mixAR} model}
  \item{ts}{the time series}
}
\details{
  The returned value is a function, say \code{f(x)}, whose only argument
  is a numeric vector of parameters with the arrangement of
  \code{\link{lik_params}}, for which it computes the conditional
  loglikelihood.
  \code{f} can be given to optimisation routines.

  Argument \code{model} is an object inheriting from \code{mixAR} and
  determines the structure of the mixAR model for the function,
  \code{f}, that it creates. So, properties of the model,
  such as number of components, AR order, and distribution of the noise
  components are fixed when \code{f} is created and only the numeric
  values of the parameters are changed by calls to it.


}
\value{
  a function of one argument, the parameters of a mixAR model as a
  numeric vector with the arrangement of \code{\link{lik_params}}, for which
  it computes the conditional loglikelihood
}




%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Todo}{
  The environment of the returned function contains the time series and
  the model object (initially argument \code{model}, later the model
  used in the last call to \code{f}). So, these things can be extracted
  from \code{f}.
  Is it necessary to create convenience functions?
}


\section{Methods}{
\describe{

\item{\code{signature(model = "mixAR", ts = "numeric")}}{
%%  ~~describe this method here~~
}
}}
\seealso{
  \code{\link{mix_pdf}},
  \code{\link{mix_cdf}}
}
\keyword{fixme}
\keyword{methods}
