\name{adjustLengths}
\alias{adjustLengths}
\title{
  Adjust the length of the second argument to be the same as that of the
  first one
}
\description{
  Adjust the length of the second argument to be the same as that of the
  first one. Appends 0's if the second argument is shorter and drops
  excess elements if it is longer.
}
\usage{
adjustLengths(x, y)
}
\arguments{
  \item{x}{the template vector}
  \item{y}{the vector to be adjusted}
}

\value{A vector of the same length as \code{x}}



%% ~Make other sections like Warning with \section{Warning }{....} ~


% \examples{
% ##---- Should be DIRECTLY executable !! ----
% ##-- ==>  Define data, use random,
% ##--	or do  help(data=index)  for the standard data sets.
%
% ## The function is currently defined as
% "adjustLengths"
% }
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
\keyword{internal}
