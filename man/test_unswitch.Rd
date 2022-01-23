\name{test_unswitch}
\alias{test_unswitch}

\title{A test for 'unswitch'}
\description{A test for 'unswitch'.}
\usage{
test_unswitch(models, true_model, allperm, ...)
}
\arguments{
  \item{models}{a list of MixAR models to process.}
  \item{true_model}{the MixAR model used in the experiment.}
  \item{allperm}{which permutations to try.}
  \item{\dots}{additional arguments to pass on to \code{\link{unswitch}}.}
}
\details{
  This function permutes ('switches') randomly each model in \code{models} and calls
  \code{unswitch} to 'unswitch' the switched models. It calculates
  summary statistics for the three sets of models.

  The summary statistics for the 'switched' models will often be
  meaningless. Those for the 'unswitched' ones should be good.

  The quality of the statistics for argument \code{models} itself
  depend on how they are obtained and on the specifics of the model.
  They are good if, as is often done is mixture experiments, estimation
  is initialised with the true values of the parameters, since there is
  little scope for switching in that case. If initial values are chosen
  randomly, then there may still be little switching, largely dependent
  on the probabilities of the components.
}

\value{
  A list with the summaries for the 3 sets of models.
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{Georgi N. Boshnakov}
%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
## N is small for to make the examples run instantly.
set.seed(1234)
aII10data <- mixARExperiment(exampleModels$WL_II, N = 5, raw = TRUE,
                 estargs = list(crit = 1e-4))
aII10 <- test_unswitch(aII10data$Raw, exampleModels$WL_II)
aII10

aII10adata <- mixARExperiment(exampleModels$WL_II, N = 5, raw = TRUE,
                  simargs = list(n = 100), estargs = list(crit = 1e-4))
aII10a <- test_unswitch(aII10adata$Raw, exampleModels$WL_II)
aII10a
}
\keyword{MixAR}
\keyword{simulation}
\keyword{internal}
