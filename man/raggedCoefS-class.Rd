\name{raggedCoefS-class}
\Rdversion{1.1}
\docType{class}
\alias{raggedCoefS-class}
\alias{raggedCoefS}
%\alias{[-methods}
\alias{[,raggedCoefS,missing,missing,ANY-method}
\alias{[,raggedCoefS,numeric,missing,ANY-method}
\alias{[,raggedCoefS,missing,numeric,ANY-method}
\alias{[,raggedCoefS,numeric,numeric,ANY-method}
\alias{[[,raggedCoefS,ANY,missing-method}
\alias{[[,raggedCoefS,ANY,ANY-method}
\alias{[[<-,raggedCoefS,ANY,ANY,numeric-method}
\alias{[[<-,raggedCoefS,ANY,missing,list-method}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Class \code{"raggedCoefS"} --- ragged list
}
\description{
Ragged list used to hold coefficients of mixAR models with seasonal AR parameters.
}
\section{Objects from the Class}{
Objects are created by calls of the form \code{new("raggedCoefS",a=list(v1, v2 , ...),as=list(vs1, vs2, ...), s)}. If orders \code{p} and \code{ps} are specified, a consistency check is made.
}
%- maybe also 'usage' for other objects documented here.
\section{Slots}{
\describe{
\item{a}{Object of class "\code{list}" containing AR values. Each element of the list must be "\code{numeric}"}
\item{p}{Object of class "\code{numeric}" containing the lengths of components in \code{a}. If missing, it is generated based on lengths of elements of \code{a}.}
\item{as}{Object of class "\code{list}" containing seasonal AR values. Each element of the list must be "\code{numeric}"}
\item{ps}{Object of class "\code{numeric}" containing the lengths of elements of \code{as}. If missing, it is generated based on lengths of elements of \code{as}.}
\item{s}{A single element "\code{numeric}" vector
determining the seasonality in the model(monthly, quarterly, etc..).}
}
}
\section{Methods}{

   Indexing with "[" treats a \code{raggedCoef} object as a matrix
   (one row for each component), while
  "[[" treats the object as list (it works on slot \code{a}). Specifically,
  "[[1]]" picks the systematic AR parameters, "[[2]]" picks seasonal AR
  parameters.
  
  The replacement variants of "[" and "[[" do not change the structure
  of the object. 
  
  Replacement methods only work for subsets
  \code{x[[i]]}, \code{x[[i]][[j]]}, \code{x[[i]][[j]][k]} for suitable \code{i}, 
  \code{j} and \code{k}.
  
  \code{i} must be equal to 1 for \code{x@a} and 2 for \code{x@as}.
  
\describe{
    \item{[}{\code{signature(x = "raggedCoefS", i = "missing", j = "missing")}:}
    returns the complete matrix of coefficients, one row corresponding to one component,
    with '0's to match different orders
    \item{[}{\code{signature(x = "raggedCoefS", i = "missing", j =
        "missing")}:}
    \item{[}{\code{signature(x = "raggedCoefS", i = "numeric", j =
        "missing")}:}
    \item{[}{\code{signature(x = "raggedCoefS", i = "numeric", j = "numeric")}:
      Indexing with "[" treats a \code{raggedCoef} object as a matrix
      with one row for each component and number of columns equal to
      \code{max(p) + max(ps)} in increasing lag. 
      However, \code{x[2]} is equivalent to \code{x[2,]}
      which is different from the treatment of \code{matrix} objects in
      base \R.}

    \item{[[}{\code{signature(x = "raggedCoefS"), i = "numeric"}:}
    if i=1 selects the list of systematic AR parameters;
    if i=2 selects the list of seasonal AR parameters.
    \item{[[}{\code{signature(x = "raggedCoefS"), i = "numeric", j = "numeric"}:}
    \item{[[}{\code{signature(x = "raggedCoefS"), i = "numeric", j = "numeric", k = "numeric"}:}
    j and k are used to select specific elements from the listt of interest.
}
}
\author{Davide Ravagli}

\seealso{
        \code{\linkS4class{raggedCoef}}
%	~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%	or \code{\linkS4class{CLASSNAME}} for links to other classes
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
showClass("raggedCoefS")

ragA <- new("raggedCoefS", a = list( c(0.5, -0.5), 1),
            as = list(0, c(0.3, -0.1) ), s = 12)
ragB <- new("raggedCoefS", a = list( c(0.5, -0.5), 1), p = c(2, 1),
            as = list(0, c(0.3, -0.1) ), ps = c(1, 2), s = 12)  # same

## Elements selection examples

ragA[]             ## matrix of coefficients
ragA[1]; ragA[1, ] ## vector of coefficients from first component
ragA[[2]]          ## list of seasonal AR parameters
ragA[[2]][[1]]      ## vector of seasonal AR parameters from first component

## Replacement of values in 'raggedCoefS' objects


ragB[[2]] <- list(1, c(-0.5,0.5))
ragB[[2]][[2]] <- c(20, 22)
ragB[[1]][[1]][1] <- 0


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ragged }
\keyword{ classes }
