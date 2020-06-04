\name{companion_matrix}

\alias{companion_matrix}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{Create a companion matrix from a vector}

\description{Create a companion matrix from a vector.}

\usage{
companion_matrix(v, ncol = length(v), nrow = ncol)
}

%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{v}{the first row of the matrix, a numeric vector or a matrix
    with one row.}
  \item{ncol}{number of columns, a number.}
  \item{nrow}{number of rows, a number.}
}

\details{
  With the default settings, a square \eqn{m\times m}{m x m} matrix is
  returned, where \eqn{m} is the length of \code{v}. If \code{ncol>}\eqn{m},
  the vector is amended with 0's. It is an error for \code{ncol} to be
  smaller than \eqn{m}.

  Argument \code{nrow} may be used to get a rectangular matrix, although
  the term "companion" is normally used only for square matrices.
}

\value{
  a matrix
}

% \references{
% %% ~put references to the literature/web site here ~
% }
% 
% \author{
% %%  ~~who you are~~
% }
% 
% \note{
% %%  ~~further notes~~
% }
% 
% 
% %% ~Make other sections like Warning with \section{Warning }{....} ~
% 
% 
% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }

\examples{
companion_matrix(4:1)
companion_matrix(4:1, ncol=6)
companion_matrix(4:1, ncol=6, nrow=3)
}
\keyword{math}
\keyword{internal}
