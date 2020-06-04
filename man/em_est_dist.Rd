\name{em_est_dist}
\alias{em_est_dist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Optimise scale parameters in mixARgen models }
\description{
  Optimise the scale parameters in mixAR models from class mixARgen.
  Internal function.
}
\usage{
em_est_dist(tau, etk, parscore, sigma, nu, logpdf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tau}{conditional probabilities, an object of class "mixComp",
    see 'Details'.}
  \item{etk}{component residuals, see 'Details'.}
  \item{parscore}{the score function(s), see 'Details'.}
  \item{sigma}{current values of the scale parameters, a numeric vector.}
  \item{nu}{current values of the parameters. w.r.t. which optimisation
    is done. (todo: check if this is used)}
  \item{logpdf}{the log of pdf as a function of the parameters.}
}
\details{

  One or more of the error distributions of a mixAR model may have
  parameters that are considered unknown. In that \code{em_est_dist} can
  be used to optimise to them.

  The representation of the error distributions in "mixARgen" models
  carries all the necessary information about
  parameters. \code{em_est_dist} works by extracting their current
  values from \code{logpdf}, passes them to the optimisation function
  (or equation solver) and stores the result back into \code{logpdf}.
  \code{em_est_dist} is quite general, as long as \code{logpdf} is
  prepared according to the conventions it expects (this is so if they
  arevalid elements of the \code{dist} slot of "mixARgen" objects).

}
\value{
  the new values of the parameters.
}
% \references{
% %% ~put references to the literature/web site here ~
% }
% \author{
% %%  ~~who you are~~
% }
% \note{
% %%  ~~further notes~~
% }
% 
% %% ~Make other sections like Warning with \section{Warning }{....} ~
% 
% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{
##---- Should be DIRECTLY executable !! ----
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ em }% __ONLY ONE__ keyword per line
