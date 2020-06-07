% I removed the individual files for *-methods, /-methods, etc, to avoid
% repetition. A consequence is that \code{methods ? "*"}, etc, do not
% give info.
%
% On the other hand, it is probably inappropriate to put
% aliases here for "*-methods", etc., since a user who is not interested
% in this may be bemused. (I am not sure if this is a valid reason not to
% include aliases though.)
\name{MixComp-class}
\docType{class}
\alias{MixComp-class}
\alias{dim,MixComp-method}
\alias{-,MixComp,missing-method}
\alias{-,MixComp,numeric-method}
\alias{-,numeric,MixComp-method}
\alias{*,character,MixComp-method}
\alias{*,function,MixComp-method}
\alias{*,MixComp,MixComp-method}
\alias{*,MixComp,numeric-method}
\alias{*,numeric,MixComp-method}
\alias{/,MixComp,numeric-method}
\alias{/,numeric,MixComp-method}
\alias{+,MixComp,numeric-method}
\alias{+,numeric,MixComp-method}
\alias{^,MixComp,numeric-method}

%\alias{\%of\%,character,MixComp-method}
%\alias{\%of\%,function,MixComp-method}
%\alias{\%of\%,list,MixComp-method}

%\alias{inner,MixComp,missing,missing,missing-method}
%\alias{inner,MixComp,numeric,missing,missing-method}
%\alias{inner,numeric,MixComp,missing,missing-method}
%\alias{inner,MixComp,numeric,ANY,ANY-method}
%\alias{inner,MixComp,numeric,ANY,missing-method}

%\alias{mix_ncomp,MixComp-method}

\title{Class \code{"MixComp"} --- manipulation of MixAR time series}
\description{
  
  Class \code{"MixComp"} represents components of mixture autoregressive
  time series and their transformations obtained by arithmetic and
  related operations. Methods are provided to allow convenient
  computation with such time series.

}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("MixComp", ...)}. 
  It is more usual however to obtain such objects initially
  from functions such as \code{mix_ek}. Methods are defined to allow for
  convenient and intuitive further manipulation of such objects.

  Internally, an object of class \code{MixComp} is a matrix with one column
  for each component. However, methods for arithmetic operations
  involving \code{MixComp} objects  are defined to perform natural
  operations for mixture objects. For example, multiplication by
  vectors is commutative and ``does the right thing''.
}
\section{Slots}{
  \describe{
    \item{\code{m}:}{
      Object of class \code{"matrix"} with one column correponding to
      each component of the mixture AR model.
    }
  }
}
\section{Methods}{
    Arithmetic operations involving \code{MixComp} objects are defined
    to allow for convenient execution of computations for mixture
    autoregressive models, see class \code{"\linkS4class{MixComp}"}.

  \describe{
    \item{-}{\code{signature(e1 = "MixComp", e2 = "missing")}:
      unary minus for "MixComp" objects.
    }
    \item{-}{\code{signature(e1 = "numeric", e2 = "MixComp")}:

      If \code{e2} is thought of as a matrix, \eqn{m}, then the number
      of elements of \code{e1} must be the same as the number of rows
      of \eqn{m} and each column of \eqn{m} is subtracted from
      \code{e1}, see also \code{"mix_ek"}, \code{"mix_hatk"}.

      As a special case, if \eqn{m} has only one row, then it is
      subtracted from each element of \code{e1}, i.e. that row is
      replicated to obtain a matrix with as many rows as the length of
      \code{e1} and then subtracted from \code{e1} as above.

      The result is a \code{MixComp} object.
    }
    \item{-}{\code{signature(e1 = "MixComp", e2 = "numeric")}:
      This is analogous to the above method. (\bold{FIXME:} the code
      of this function does not deal with the special case as in the
      above method. Is this an omission or I have done it on purpose?)
    }

    \item{\%of\%}{\code{signature(e1 = "function", e2 = "MixComp")}:
      This applies the function \code{e1} to each element of
      \code{e2}. Together with the arithmetic operations this allows for
      easy computation with MixComp objects (e.g. pdfs, likelihoods).
    }
    \item{\%of\%}{\code{signature(e1 = "character", e2 = "MixComp")}:}
    \item{\%of\%}{\code{signature(e1 = "list", e2 = "MixComp")}:
      If \code{e1} is of length one it specifies a function to be
      applied to each element of \code{e2}, otherwise it is a list of
      functions, such that the \eqn{i}{i-}th function is applied to the
      \eqn{i}{i-}th column of \code{e2@m}.
    }


    \item{*}{\code{signature(e1 = "MixComp", e2 = "MixComp")}: ... }

    \item{*}{\code{signature(e1 = "MixComp", e2 = "numeric")}:
      see the following.
    }
    \item{*}{\code{signature(e1 = "numeric", e2 = "MixComp")}:
      ``Column'' \eqn{i} of the \code{MixComp} object is multiplied by
      the \eqn{i}{i-}th element of the numeric vector, i.e. each ``row''
      of the \code{MixComp} object is multiplied by the vector (or, the
      vector is replicated to a matrix to be multiplied by the
      \code{MixComp} object).
    }
    \item{*}{\code{signature(e1 = "function", e2 = "MixComp")}:
      Multiplying a function by  a \code{MixComp} object actually
      applies the function to each element of the object.
      This is a misuse of methods, prefer operator
      \code{\%of\%} which does the same.
    }
    \item{*}{\code{signature(e1 = "character", e2 = "MixComp")}:
      The first argument is a name of a function which is
      applied to each element of the \code{MixComp} object. This is a
      misuse of methods, see operator \code{\link[=percent_of]{\%of\%}}
      which does the same.  
      % (\bold{TODO:} but the latter does not have a method with this
      % signature.) 
    }
    \item{/}{\code{signature(e1 = "MixComp", e2 = "numeric")}:
    }
    \item{/}{\code{signature(e1 = "numeric", e2 = "MixComp")}:
      Division works analogously to \code{"*"}.
    }

    \item{^}{\code{signature(e1 = "MixComp", e2 = "numeric")}:
      If \code{k} is a scalar, raise each element of \code{e1@m} to
      power \code{k}.

      % \bold{TODO:}
      (For consistency this operation should have the
      semantics of "*" and "/" but this operator probably makes sense
      only for scalar 'e2', where the semantics doesn't matter. So,
      don't bother for now.)
    }


    \item{+}{\code{signature(e1 = "numeric", e2 = "MixComp")}:
    }
    \item{+}{\code{signature(e1 = "MixComp", e2 = "numeric")}:
      Addition involving \code{MixComp} objects works analogously to subtraction.
    }

    \item{inner}{\code{signature(x = "MixComp", y = "missing", star = "missing", plus = "missing")}:
      With one argument \code{inner} computes the sum of the columns of
      the argument. This is conceptually equivalent to \code{y} being a
      vector of ones.
    }
    \item{inner}{\code{signature(x = "MixComp", y = "numeric", star = "missing", plus = "missing")}:
    }
    \item{inner}{\code{signature(x = "numeric", y = "MixComp", star = "missing", plus = "missing")}:
      The number of elements of the numeric argument should be equal to
      the number of rows of the \code{MixComp} object. Effectively,
      computes the inner product of the two arguments. The order of the
      arguments does not matter.
      % (\bold{TODO:} \bold{FIXME:} make sure that this is true!)

      Returns a numeric vector.
    }

    \item{inner}{\code{signature(x = "MixComp", y = "numeric", star = "ANY", plus = "ANY")}:
      Computes a generalised inner product of \code{x} with \code{y} using the
      specified functions in place of the usual "*" and "+"
      operations. The defaults for \code{star} and \code{+} are
      equivalent to multiplication and addition, respectively.

      Note that "+" is a binary operation (not \eqn{n}-ary) in \R. So
      technically the correct way to specify the default operation here
      is "sum" or \code{sum}. Since it is easy to make this mistake, if
      \code{plus == "+"}, it is replaced by "sum".
      (In fact, \code{plus} is given a single argument, the vector of
      values to work on. Since "+" works as a unary operator on one
      argument, it would give surprising results if left as is.)
    }
    \item{inner}{\code{signature(x = "MixComp", y = "numeric", star = "ANY", plus = "missing")}:
      This is a more efficient implementation for the case when
      \code{plus = sum}.
    }

    \item{mix_ncomp}{\code{signature(x = "MixComp")}:
      Number of components.
    }

    \item{\code{signature(x = "MixComp")}}{
      A \code{"MixComp"} object is essentially a matrix. This method
      gives the dimension of the underlying matrix. This method
      indirectly ensures that \code{nrow()} and \code{ncol()} work
      naturally for \code{"MixComp"} objects.
    }
    
  }
}
%\references{
%}  
\author{Georgi N. Boshnakov}
% \note{
% %  ~~further notes~~
% }
%
% %%  ~Make other sections like Warning with \section{Warning }{....} ~
%
% \seealso{
% %	~~objects to See Also as \code{\link{~~fun~~}}, ~~~
% %	or \code{"\linkS4class{CLASSNAME}"} for links to other classes
% }
%\examples{
%showClass("MixComp")
%}
\examples{

## dim, nrow, ncol
a <- new("MixComp", m = matrix(c(1:7, 11:17, 21:27), ncol = 3))
a
dim(a)
nrow(a)
ncol(a)
mix_ncomp(a)

-a
a - 1:7 
1:7 + a
2*a


b <- new("MixComp", m = matrix(rnorm(18), ncol = 3))

## apply a function to the columns of a MixComp object
pnorm \%of\% b

## apply a separate function to to each column
flist <- list(function(x) pnorm(x),
              function(x) pt(x, df = 5),
              function(x) pt(x, df = 4) )
flist \%of\% b
}
\keyword{classes}
\keyword{MixComp}
