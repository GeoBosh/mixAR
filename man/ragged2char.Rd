\name{ragged2char}
\alias{ragged2char}
\title{Convert a ragged list into a matrix of characters}
\description{
  The function transforms a ragged list into a matrix of characters. It
  is used in \pkg{mixAR} for output visualisation purposes.
}
\usage{
ragged2char(raglist, filler = NA_character_)
}
\arguments{
  \item{raglist}{A ragged list (from \code{raggedCoef}).}
  \item{filler}{The character filling order mismatches.}
}
\details{

  \code{ragged2char} converts a ragged list into a character matrix with
  as many columns as the longest component of the list, filling the
  missing entries with the values of argument \code{filler}. The latter
  defaults to \code{NA}.

  In MixAR context the \code{i}-th row represents the AR coefficients
  for the \code{i}-th component.

}
\value{
    a character matrix
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Georgi N. Boshnakov}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{raggedCoef}}
}
\examples{
li1 <- list(1.5, c(1.51, 1.522), c(10.311, 1.31, 1.313))
ragged2char(li1)
ragged2char(li1, "")
}
\keyword{ ragged }% use one of  RShowDoc("KEYWORDS")
\keyword{ internal }

