\name{mixARExperiment}

\alias{mixARExperiment}

\title{Simulation experiments with MixAR models}

\description{Perform simulation experiments with MixAR models.}

\usage{
mixARExperiment(model, imodel = NULL, simargs = NULL, estargs = NULL, fix, ...)
}

\arguments{
  \item{model}{the underlying model, an object inheriting from "MixAR",
  see 'Details'.}
  \item{imodel}{initial model,  an object inheriting from "MixAR", see 'Details'.}
  \item{simargs}{additional arguments for the simulation function, a
    list, see 'Details'.}
  \item{estargs}{additional arguments for the estimation function, a
    list, see 'Details'.}
  \item{fix}{which arguments to keep fixed during estimation; currently
    if \code{fix = "shift"} the shift parameters are kept fixed, otherwise
    all parameters are estimated.}
  \item{\dots}{additional arguments to pass on to \code{\link{simuExperiment}}.}
}

\details{
  \code{mixARExperiment} is a wrapper for \code{simuExperiment}.

  The simulation function is \code{mixARsim} and the estimation function
  is \code{fit_mixAR}. The simulation function gets \code{model} as
  first argument. The first argument of the estimation function is
  \code{imodel}, if supplied, or \code{model} otherwise. Some
  appropriate defaults for other arguments are also supplied. Arguments
  \code{simargs} and \code{estargs} are needed only to override
  defaults.

  If \code{fix = "shift"}, the shifts components are not estimated and
  are kept fixed. This may be useful when fitting the model to
  differenced series, for example.

  \code{mixARExperiment} sets \code{keep} and \code{summary_fun} to some
  defaults (currently \code{mixAR:::.fsumA} for the latter).
}

\value{
  A list with one or more elements, depending on the arguments
  \code{\dots} passed on to \code{\link{simuExperiment}}.

  \item{Summary}{a summary of the experiment, by default sample means
    and standard deviations of the estimates. This is a list of MixAR models.}

  \item{Raw}{A list of the estimated models.}
}

%\references{
%%% ~put references to the literature/web site here ~
%}

\author{Georgi N. Boshnakov}

\note{
  This is an initial version of this function, some of its arguments may
  change.  Additional elements may be made available in the returned
  value, so refer to those by name, not with a numeric index.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\seealso{
  \code{\link{simuExperiment}}
}

\examples{
exampleModels$WL_II

set.seed(1234)
n = 20 # toy size
N = 5 # toy number of simulations
mixARExperiment(exampleModels$WL_II, N = N, estargs = list(crit = 1e-4))
mixARExperiment(exampleModels$WL_II, N = N, raw = TRUE, estargs = list(crit = 1e-4))
mixARExperiment(exampleModels$WL_II, N = N, raw = TRUE,
    simargs = list(n = n), estargs = list(crit = 1e-4) )
}
\keyword{MixAR}
\keyword{simulation}
\keyword{internal}
