\name{.canonic_coef}
\alias{.canonic_coef}
\title{Put core mixAR coefficients into a canonical form, internal function}
\description{Put core mixAR coefficients into a canonical form, internal
  function.}
\usage{
.canonic_coef(coef, filler)
}
\arguments{
  \item{coef}{the core coefficients of a mixAR model, a list.}
  \item{filler}{a value for filling unspecified entries, such as
    \code{NA_real_}.}
}
\details{
  This is an internal function called by \code{mixAR} to put mixAR
  parameters into a canonical form. \code{"shift"}, \code{"scale"} and
  \code{"prob"} are normally vectors with one element for each
  component but they may also be given as a single number, in which case
  it is taken to be the common value for all parameters and
  \code{.canonic_coef} extends them correspondingly. Also, the AR
  coefficients may be specified in a number of ways and this function
  converts them to the format used by the mixAR classes.
}
\value{
  a list with the folowing components:
  \item{order}{mixAR order, a vector of integers}
  \item{prob}{mixAR probabilities, vector of length\code{length(order)}}
  \item{shift}{mixAR shift parameters, vector of length\code{length(order)}}
  \item{scale}{mixAR scle parameters, vector of length\code{length(order)}}
  \item{arcoef}{AR coefficients as a \code{"raggedCoef"} object}
}
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{Georgi N. Boshnakov}
% \note{
% %%  ~~further notes~~
% }
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
  \code{\link{mixAR}}
}
\examples{
## missing components are filled with 'filler', extended accordingly
mixAR:::.canonic_coef(list(order = c(2,3)), filler = NA)

# here 'scale' is replicated, the missing 'shift' is inserted
mo <- list(order = c(2,3), prob = c(0.4, 0.6), scale = 1,
           arcoef = list(c(0.5, -0.5), c(1.1,  0.0, -0.5)) )
mixAR:::.canonic_coef(mo, filler = NA)
}
\keyword{mixAR}
\keyword{internal}
