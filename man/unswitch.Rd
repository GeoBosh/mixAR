\name{unswitch}
\alias{unswitch}

\title{
  Dealing with label switching in MixAR experiments
}

\description{
  Deal with label switching in MixAR experiments.
}

\usage{
unswitch(models, true_model, Nref = 100, simargs = NULL)
}
\arguments{
  \item{models}{a list of MixAR models to process.}
  \item{true_model}{the MixAR model used in the experiment.}
  \item{Nref}{length of the reference series}
  \item{simargs}{arguments for simulation of the reference series, see 'Details'.}
}

%\details{
%  todo: give details!
%}

\value{
  a list of the unswitched models.
}

\references{
  Boshnakov, Georgi N., (2012) Label switching in MixAR models (in preparation).
}

\author{
  Georgi N. Boshnakov
}

%\note{
%%%  ~~further notes~~
%}
%
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
%
%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

%\examples{
%}

% Add one or more standard keywords, see file 'KEYWORDS' in the

% R documentation directory.

\keyword{simulation}
\keyword{MixAR}
\keyword{internal}
