\name{permn_cols}

\alias{permn_cols}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{All permutations of the columns of a matrix}

\description{All permutations of the columns of a matrix}

\usage{
permn_cols(m)
}

%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{m}{a matrix}
}

\details{
  This function is a wrapper for   \code{permn} from package `combinat'.
}

\value{
  a list with one element for each permutation of the columns. Each
  element of the list is an unnamed list with two components:
  \enumerate{
    \item the permutation, a vector of positive integers,
    \item a matrix obtained by permuting the columns of \code{m}.
  }
}

%\references{
%%% ~put references to the literature/web site here ~
%}

\author{Georgi N. Boshnakov}

\examples{
m <- matrix(c(11:14,21:24,31:34), ncol=3)
pm <- permn_cols(m)
pm[[2]]
}

% Add one or more standard keywords, see file 'KEYWORDS' in the

% R documentation directory.

\keyword{array}
