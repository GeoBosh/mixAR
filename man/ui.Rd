\name{ui}
\alias{ui}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Utility function for mixAR
%%  ~~function to do ... ~~
}
\description{Extracts the (t-i)th component from a vector.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ui(x, t, i)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A vector.
%%     ~~Describe \code{x} here~~
}
  \item{t}{The present "time".
%%     ~~Describe \code{t} here~~
}
  \item{i}{The lag. Can be a vector.
%%     ~~Describe \code{i} here~~
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns the (t-i)th element of the vector. If i=0, returns 1.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Davide Ravagli
%%  ~~who you are~~
}
\note{This is a utility function for mixAR.
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ lag }% use one of  RShowDoc("KEYWORDS")
\keyword{ ts }% __ONLY ONE__ keyword per line
\keyword{ internal }
