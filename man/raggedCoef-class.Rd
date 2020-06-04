\name{raggedCoef-class}
\Rdversion{1.1}
\docType{class}
\alias{raggedCoef}
\alias{raggedCoef-class}

\alias{[-methods}
\alias{[,raggedCoef,missing,missing,ANY-method}
\alias{[,raggedCoef,numeric,missing,ANY-method}
\alias{[,raggedCoef,missing,numeric,ANY-method}
\alias{[,raggedCoef,numeric,numeric,ANY-method}

\alias{[[-methods}
\alias{[[,raggedCoef,ANY,missing-method}
\alias{[[,raggedCoef,ANY,ANY-method}

\alias{[<-,raggedCoef,ANY,ANY,numeric-method}
\alias{[<-,raggedCoef,ANY,missing,numeric-method}
\alias{[<-,raggedCoef,ANY,missing,list-method}
\alias{[<-,raggedCoef,ANY,missing,matrix-method}

\alias{[<-,raggedCoef,missing,missing,list-method}
\alias{[<-,raggedCoef,missing,missing,matrix-method}
\alias{[<-,raggedCoef,missing,missing,numeric-method}

\alias{[<-,raggedCoef,missing,missing,ANY-method}
\alias{[<-,raggedCoef,numeric,missing,ANY-method}
\alias{[<-,raggedCoef,numeric,numeric,ANY-method}

\alias{[[<--methods}
\alias{[[<-,raggedCoef,ANY,ANY,numeric-method}
\alias{[[<-,raggedCoef,ANY,missing,numeric-method}

% \alias{initialize,raggedCoef-method}
% \alias{mixFilter,numeric,raggedCoef,numeric-method}

\alias{anyNA,raggedCoef-method}
\alias{dim,raggedCoef-method}
\alias{length,raggedCoef-method}

\title{
  Class "raggedCoef" --- ragged list objects
}
\description{

  Some models have several several vectors of parameters, possibly of
  different lengths, such that in some circumstances they are thought of
  as lists, in others as matrices after suitable padding with zeroes.
  Class \code{"raggedCoef"} represents such ragged lists.  In package
  \code{"mixAR"} it is used to hold the autoregressive coefficients of
  mixAR models.

}
\usage{
raggedCoef(p, value = NA_real_)
}
\arguments{
  \item{p}{orders, vector of integers.}
  \item{value}{typically, a list, but see Details.}
}
\details{

  Class \code{"raggedCoef"} is for objects that can be considered as
  both, lists and matrices. The elements of the list are vectors,
  possibly of different lengths.  When the object is viewed as a matrix,
  each element of the list (suitably padded with zeroes or \code{NA}s)
  represents a row of a matrix.

  The recommended way to create objects from class \code{"raggedCoef"}
  is with the function \code{raggedCoef}. 

  If \code{value} is a "raggedCoef" object it is returned.
  If value is a list, it is converted to "raggedCoef" using
  \code{new()}.
  If argument \code{p} is missing, it is inferred from the
  lengths of the elements of the list.  
  If argument \code{p} is not missing, a consistency check
  is made to ensure that the order of the object is as specified by
  \code{p}.

  Otherwise, if \code{value} is of length one, it is replicated to form
  a ragged list with i-th element a vector of length
  \code{p[i]}. Although not checked, the intention here is that
  \code{value} is from some atomic class.  The default for \code{value}
  is \code{NA_real_} to give a convenient way to create a ragged list.

  Finally, if none of the above applies, \code{value} is effectively assumed to
  be a vector of length \code{sum(p)}, although other cases are
  admissible (but I don't remember if this was intended). In this case,
  \code{value} is reshaped into a ragged list to match \code{p}. This is
  convenient when, for example, the elements of a ragged array are
  obtained from an optimisation routine which expects plain vector.
  (\strong{TODO:} the description of this case is from cursory glance at
  the code and guesswork - check.)

}

\section{Objects from the Class}{

  Below we describe the \code{"initialize"} method that underlies
  \code{new("raggedCoef", ...)}.  The recommended way to create
  \code{"raggedCoef"} objects is with the function \code{raggedCoef},
  see section Details.

  Objects can also be created by calls of the form
  \code{new("raggedCoef", v)}, where \code{v} is a list whose elements
  are numeric vectors, or \code{new("raggedCoef", v1, v2, ...)}, where
  \code{v1, v2, ...} are numeric vectors.  The two forms are equivalent
  if \code{v = list(v1, v2, ...)}.

  The elements of the list \code{v} may be named.
  Similarly, named arguments can be used in the second form, say
  \code{new("raggedCoef", name1 = v1, name2 = v2, ...)}.
  In both cases the names are preserved in the internal representaion,
  but not used.

  If the arguments are not as specified above the result should be
  considered undefined. 
  Currently, if there are other arguments after the list \code{v}, they
  are ignored with a warning. If the first argument is not a list then
  all arguments must be \code{numeric} and an error is raised if this is
  not the case. For completeness, we mention that exactly two arguments named
  \code{a}, and \code{p} are also accepted by \code{new()}, eg
  \code{new("raggedCoef", p = c(1, 2), a =  list(3, 4:5))}, but these
  are assigned to the slots without any checking. so it is
  most flexible (and recommended) to use \code{raggedCoef()} instead.

}
\section{Slots}{
  \describe{
    \item{\code{a}:}{Object of class \code{"list"} containing the values. }
    \item{\code{p}:}{Object of class \code{"numeric"} containing the
      lengths of the components of \code{a}.}
  }
}
\section{Methods}{
  Indexing with "[" treats a \code{raggedCoef} object as a matrix, while
  "[[" treats the object as list (it works on slot \code{a}).

  Note that there is a difference between \code{x[2,]} (or the
  equivalent \code{x[2]}) and \code{x[[2]]}---the former gives a vector
  of length \code{max(p)}, so potentially padded with zeroes, while the
  latter gives the component with its ``natural'' length.

  The replacement variants of "[" and "[[" do not change the structure
  of the object and issue errors if the replacement value would result
  in that. In situations where the checks are deemed redundant, direct
  assignments to the corresponding slots may be used.

  \describe{
    \item{[}{\code{signature(x = "raggedCoef", i = "missing", j = "missing", drop = "ANY")}: }
    \item{[}{\code{signature(x = "raggedCoef", i = "missing", j = "numeric", drop = "ANY")}: }
    \item{[}{\code{signature(x = "raggedCoef", i = "numeric", j = "missing", drop = "ANY")}: }
    \item{[}{\code{signature(x = "raggedCoef", i = "numeric", j = "numeric", drop = "ANY")}:
      Indexing with "[" treats a \code{raggedCoef} object as a matrix
      with one row for each component and number of columns equal to
      \code{max(p)}. However, \code{x[2]} is equivalent to \code{x[2,]}
      which is different from the treatment of \code{matrix} objects in
      base \R.
    }

    \item{[[}{\code{signature(x = "raggedCoef", i = "ANY", j = "missing")}:}
    \item{[[}{\code{signature(x = "raggedCoef", i = "ANY", j = "ANY")}:
      \code{"[["} extracts the corresponding element of slot \code{a}.}
    
    \item{[[<-}{\code{signature(x = "raggedCoef", i = "ANY", j = "ANY", value = "numeric")}:
      Replace the j-th element of i-th row with \code{value}.
      All arguments must be scalars.
    }
    \item{[[<-}{\code{signature(x = "raggedCoef", i = "ANY", j = "missing", value = "numeric")}:
    }

    \item{[<-}{\code{signature(x = "raggedCoef", i = "ANY", j = "missing", value = "numeric")}:
      Replace the i-th row with \code{value}. Argument \code{i} must be
      a scalar while the length of \code{value} must be the same as that
      of \code{x@a[[i]]}. The methods for "[" and "[[" with this
      signature coinside.
    }
    \item{[<-}{\code{signature(x = "raggedCoef", i = "ANY", j = "missing", value = "list")}:
      The elements of \code{value} must have the same lengths as the
      elements they are replacing.
    }
    \item{[<-}{\code{signature(x = "raggedCoef", i = "ANY", j = "missing", value = "matrix")}:
       This is essentially the reverse od the corresponding
       non-replacement operator. \code{value} must have at least as many
       columns as the longest element of \code{x} that is replaced.
    }

    \item{[<-}{\code{signature(x = "raggedCoef", i = "ANY", j = "ANY", value = "numeric")}: ... }
    \item{[<-}{\code{signature(x = "raggedCoef", i = "missing", j = "missing", value = "list")}: ... }
    \item{[<-}{\code{signature(x = "raggedCoef", i = "missing", j = "missing", value = "matrix")}: ... }

    \item{[<-}{\code{signature(x = "raggedCoef", i = "missing", j = "missing", value = "numeric")}: ... }

    \item{initialize}{\code{signature(.Object = "raggedCoef")}:
      Creates objects of class \code{raggedCoef}. This method is used
      internally by \code{new()}. Users should use \code{new()} for
      creation of objects from this class, see the examples.
    }
    \item{show}{\code{signature(object = "raggedCoef")}: ... }

    \item{mixFilter}{\code{signature(x = "numeric", coef = "raggedCoef", index = "numeric")}:
      Apply a mixture filter to a time series.
      TODO: describe...
    }
    \item{row_lengths}{\code{signature(x = "raggedCoef")}:
      Gives \code{x@p}, which is the same as \code{lengths(x@a)}.
    }
    \item{length}{\code{signature(x = "raggedCoef")}:
      Gives the total number of coefficients (\code{sum(x@p)}).
    }
    
    \item{anyNA}{\code{signature(x = "raggedCoef")}:
      Are there \code{NA}'s in \code{x@a}?
    }
    \item{dim}{\code{signature(x = "raggedCoef")}:
      The dimension of the object, when viewed as a matrix.
      The presence of this method also ensures that \code{nrow()}
      and related functions give the expected result.
    }
  }
}

\author{Georgi N. Boshnakov}
\note{
  Slot \code{p} is redundant but convenient.

  todo: allow a matrix as first argument in the initialization
  function.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\linkS4class{mixARGaussian}}
%       \code{\link{initialize,raggedCoef-method}}
%	~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%	or \code{\linkS4class{CLASSNAME}} for links to other classes
}
\examples{
ragged1 <- list(1, 2:3, 4:6)
ragged2 <- list(a = 1, b = 2:3, c = 4:6)

raggedCoef(1:3)          # only order given, fill with NA's
raggedCoef(1:3, 0)       # fill with a number (zero in this case)

## init with a list
raggedCoef(ragged1)
raggedCoef(value = ragged1) 

## error, since the shape of ragged1 is not c(2, 2, 3):
## raggedCoef(c(2, 2, 3), value = ragged1)

## init with a flattened list
raggedCoef(p = 1:3, value = 1:6)

## specify each component separately
ragA <- new("raggedCoef", 1, 2:3, 4:6)
ragB <- new("raggedCoef", list(1, 2:3, 4:6))  # same
identical(ragA, ragB) #TRUE

## extract as a matrix
ragA[]

## extract the 2nd component
ragA[2]      # c(2, 3, 0) ("[" pads with 0's)
ragA[[2]]    # c(2, 3)    ("[[" does not pad)

## get the 2nd and 3rd components as a matrix
ragA[2:3, ]    # "[" treats object (almost) as matrix
ragA[2:3]      # same (though not as for "matrix")

## names are kept in the list but currently not used 
ragC  <- new("raggedCoef", list(a = 1, b = 2:3, c = 4:6))
ragC1 <- new("raggedCoef", a = 1, b = 2:3, c = 4:6)
identical(ragC, ragC1) # TRUE
names(ragC@a) # [1] "a" "b" "c"

length(ragA)
dim(ragA)
c(nrow(ragA), ncol(ragA))
c(NROW(ragA), NCOL(ragA))
}
\keyword{classes}
\keyword{ragged}
