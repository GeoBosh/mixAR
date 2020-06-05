\name{test_unswitch}

\alias{test_unswitch}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{A test for 'unswitch'}

\description{A test for 'unswitch'.}

\usage{
test_unswitch(models, true_model, allperm, ...)
}

%- maybe also 'usage' for other objects documented here.

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

\author{Georgin N. Boshnakov}


%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{
## all examples run succesfully, commenting out some for CRAN

aII10data <- mixARExperiment(exampleModels$WL_II, N=10, raw=TRUE)
aII10 <- test_unswitch(aII10data$Raw, exampleModels$WL_II)
aII10

aII10adata <- mixARExperiment(exampleModels$WL_II, N=10, raw=TRUE, simargs=list(n=500))
aII10a <- test_unswitch(aII10adata$Raw, exampleModels$WL_II)
aII10a

## aII1000adata <- mixARExperiment(exampleModels$WL_II, N=1000, raw=TRUE, simargs=list(n=500))
## aII1000a <- test_unswitch(aII1000adata$Raw, exampleModels$WL_II)
## aII1000a
## 
## aII1000bdata <- mixARExperiment(exampleModels$WL_II, N=1000, raw=TRUE, simargs=list(n=50))
## aII1000b <- test_unswitch(aII1000bdata$Raw, exampleModels$WL_II)
## aII1000b
## 
## aII1000b2data <- mixARExperiment(exampleModels$WL_II, N=1000, raw=TRUE, simargs=list(n=50))
## aII1000b2 <- test_unswitch(aII1000b2data$Raw, exampleModels$WL_II, Nref = 300)
## aII1000b2
## 
## aII1000b3data <- mixARExperiment(exampleModels$WL_II, N=1000, raw=TRUE,
##                     simargs=list(n=50), estargs=list(maxniter=500))
## aII1000b3 <- test_unswitch(aII1000b3data$Raw, exampleModels$WL_II, Nref = 300)
## 
## aII1000b4data <- mixARExperiment(exampleModels$WL_II, N=1000, raw=TRUE,
##                     simargs=list(n=50), estargs=list(init=1,maxniter=500))
## aII1000b4 <- test_unswitch(aII1000b4data$Raw, exampleModels$WL_II, Nref = 300)
## 
## aII1000b5data <- mixARExperiment(exampleModels$WL_II, N=1000, raw=TRUE,
##                     simargs=list(n=100), estargs=list(init=1,maxniter=500))
## aII1000b5 <- test_unswitch(aII1000b5data$Raw, exampleModels$WL_II, Nref = 300)
## aII1000b5
## 
## aII1000a5data <- mixARExperiment(exampleModels$WL_II, N=1000, raw=TRUE,
##                     simargs=list(n=100), estargs=list(init=5,maxniter=500))
## aII1000b2a5 <- test_unswitch(aII1000a5data$Raw, exampleModels$WL_II, Nref = 300)
## aII1000b2a5
## 
## aII1000b2a10data <- mixARExperiment(exampleModels$WL_II, N=1000, raw=TRUE,
##                        simargs=list(n=100), estargs=list(init=10,maxniter=500))
## aII1000b2a10 <- test_unswitch(aII1000b2a10data$Raw, exampleModels$WL_II, Nref = 300)
## aII1000b2a10
}

% Add one or more standard keywords, see file 'KEYWORDS' in the

% R documentation directory.

\keyword{MixAR}
\keyword{simulation}
