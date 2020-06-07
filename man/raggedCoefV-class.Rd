\name{raggedCoefV-class}
\Rdversion{1.1}
\docType{class}
\alias{raggedCoefV-class}
\alias{raggedCoefV}
\alias{[,raggedCoefV,numeric,ANY-method}
\alias{[,raggedCoefV,numeric,ANY,ANY-method}
\alias{[,raggedCoefV,numeric,missing,ANY-method}
\alias{[,raggedCoefV,missing,numeric,ANY-method}
\alias{[,raggedCoefV,numeric,numeric,ANY-method}
\alias{[[,raggedCoefV,missing,ANY-method}
\alias{[[,raggedCoefV,numeric,ANY-method}
%\alias{initialize, raggedCoefV-method}

\alias{[,raggedCoefV,missing,ANY,ANY-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Class \code{"raggedCoefV"} --- ragged list
}
\description{
Ragged list used to hold coefficients of MixVAR models.
}
\section{Objects from the Class}{
  Objects are created by calls of the form
  \code{new("raggedCoefV", a = list(v1, v2 ,...)}.
}
%- maybe also 'usage' for other objects documented here.
\section{Slots}{
\describe{
  \item{\code{a}:}{

    Object of class \code{"list"} containing AR values. Each element of
    the list must be \code{"array"}.

  }
  \item{\code{p}:}{

    Object of class \code{"numeric"} containing the length of arrays in
    \code{a} (AR orders). If missing, it is generated based on lengths
    of elements of \code{a}.

  }
}
}

\section{Methods}{

   Indexing with "[" and "[[" works on slot \code{a}.

   "[" and "[[" can be use alternatively. Specifically, "[]" and "[[]]"
   produce the same result, the complete list of AR coefficients.
   Similarly, \code{[i,]}, \code{[i]} and \code{[[i]]} all return the
   i^th element of the list, the array for i^th component.  \code{[,j]}
   returns an array with j^th lag autoregressive parameters for each
   component.

  \describe{
    \item{[}{\code{signature(x = "raggedCoefV", i = "missing", j = "ANY", drop = "ANY")}: ... }
    \item{[}{\code{signature(x = "raggedCoefV", i = "missing", j = "numeric", drop = "ANY")}: ... }
    \item{[}{\code{signature(x = "raggedCoefV", i = "numeric", j = "missing", drop = "ANY")}: ... }
    \item{[}{\code{signature(x = "raggedCoefV", i = "numeric", j = "numeric", drop = "ANY")}: ... }
    \item{[[}{\code{signature(x = "raggedCoefV", i = "missing", j = "ANY")}: ... }
    \item{[[}{\code{signature(x = "raggedCoefV", i = "numeric", j = "ANY")}: ... }
    \item{initialize}{\code{signature(.Object = "raggedCoefV")}: ... }
    \item{show}{\code{signature(object = "raggedCoefV")}: ... }
  }
}
\author{Davide Ravagli}

\seealso{
        class \code{"\linkS4class{MixVAR}"}
%	~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%	or \code{"\linkS4class{CLASSNAME}"} for links to other classes
}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%\references{
%%% ~put references to the literature/web site here ~
%}
%
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
showClass("raggedCoefV")

AR <- list()
AR[[1]] <- array(c(0.0973, -0.0499,  0.2927,  0.4256,  ## VAR(2;4)
                  -0.0429,  0.0229, -0.1515, -0.1795,
                  -0.0837, -0.1060, -0.1530,  0.1947,
                  -0.1690, -0.0903,  0.1959,  0.0955), dim=c(2,2,4))
AR[[2]] <- array(c(0.3243,  0.2648,  0.4956,  0.2870,  ## VAR(2;3)
                  -0.1488,  0.0454, -0.0593, -0.3629,
                   0.1314,  0.0274,  0.0637,  0.0485), dim=c(2,2,3))
                   
new("raggedCoefV", AR)
new("raggedCoefV", a=AR, p=c(4,3))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ragged }
\keyword{ classes }
