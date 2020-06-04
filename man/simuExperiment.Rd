\name{simuExperiment}

\alias{simuExperiment}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{Perform simulation experiments}

\description{Perform simulation experiments}

\usage{
simuExperiment(model, simu, est, N = 100, use_true = FALSE,
               raw = FALSE, init_name = "init", keep = identity,
               summary_fun = .fsummary, \dots)
}

%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{model}{the model, see 'Details'.}
  \item{simu}{arguments for the simulation function, a list, see 'Details'.}
  \item{est}{arguments for the estimation function, a list, see 'Details'.}
  \item{N}{number of simulations.}
  \item{use_true}{if TRUE, use also the "true" coefficients as initial
    values, see 'Details'.}
  \item{raw}{if TRUE, include the list of estimated models in the
  returned value.}
  \item{init_name}{name of the argument of the estimation function
    which specifies the initial values for estimation, not always used,
    see `Details'.
  }
  \item{keep}{what values to keep from each simulation run, a function,
  see 'Details'.}
  \item{summary_fun}{A function to apply at the end of the experiment to
  obtain a summary, see 'Details'.}
  \item{\dots}{additional arguments to pass on to the summary
  function. NOTE: this may change.}
}

\details{
  Argument \code{model} specifies the underlying model and is not always
  needed, see the examples.
  Argument \code{simu} specifies how to simulate the data.
  Argument \code{est} specifies the estimation procedure.
  Argument \code{N} specifies the number of simulation runs.
  The remaining arguments control details of the simulations, mostly
  what is returned.

  Basically, \code{simuExperiment} does \code{N} simulation-estimation
  runs.
  The \code{keep} function is applied to the value obtained from each
  run.
  The results from \code{keep} are assembled in a list (these are the
  'raw' results).
  Finally, the summary function (argument \code{summary_fun}) is applied
  to the raw list.

  \code{simu} and \code{est} are lists with two elements: \code{fun} and
  \code{args}. \code{fun} is a function or the name of a
  function. \code{args} is a list of arguments to that function. The
  first argument of the estimation function, \code{est$fun}, is the
  simulated data. This argument is inserted by \code{simuExperiment} and
  should not be put in \code{est$args}.


  The value returned by the summary function is the main part of the
  result. If \code{raw = TRUE}, then the raw list is returned, as well.
  Further fields may be made possible through additional arguments but
  'Summary' and 'Raw' are guaranteed to be as described here.

  \code{simuExperiment} uses \code{init_name} only if \code{use_true} is
  TRUE to arrange a call of the estimation function with initial value
  \code{model}.  Obviously, \code{simuExperiment} does not know how (or
  if) the estimation function does with its arguments.

  The function specified by argument \code{keep} is called with one
  argument when \code{use_true} is FALSE and two arguments otherwise.

}

\value{
  A list with one or more elements, depending on the arguments.

  \item{Summary}{a summary of the experiment, by default sample means
    and standard deviations of the estimates.}

  \item{Raw}{A list of the estimated models.}
}

%\references{
%%% ~put references to the literature/web site here ~
%}

\author{
  Georgi N. Boshnakov
}

% \note{
% }


%% ~Make other sections like Warning with \section{Warning }{....} ~


%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{
## explore dist. of the mean of a random sample of length 5.
## (only illustration, such simple cases hardly need simuExperiment)
sim1 <- list(fun="rnorm", args = list(n=5, mean=3, sd = 2))
est1 <- list(fun=mean, args = list())

# a basic report function
fsum1 <-  function(x){ wrk <- do.call("c",x)
                       c(n = length(wrk), mean = mean(wrk), sd = sd(wrk))}

a1 <- simuExperiment(TRUE, simu = sim1, est = est1, N = 1000, summary_fun = fsum1)

# explore also the dist. of the sample s.d.
est2 <- est1
est2$fun <- function(x) c(xbar = mean(x), s = sd(x))

a2  <- simuExperiment(TRUE, simu = sim1, est = est2, N = 1000)

# keep the raw sample means and s.d.'s for further use
a2a <- simuExperiment(TRUE, simu = sim1, est = est2, N = 1000, raw = TRUE)
a2a$Summary

# replicate a2a$Summary
s5 <- sapply(a2a$Raw, identity)
apply(s5, 1, mean)
apply(s5, 1, sd)

hist(s5[1,], prob=TRUE)
lines(density(s5[1,]))
curve(dnorm(x, mean(s5[1,]), sd(s5[1,])), add = TRUE, col = "red")

mixAR:::.fsummary(a2a$Raw)
mixAR:::.fsummary(a2a$Raw, merge = TRUE)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the

% R documentation directory.

\keyword{simulation}
