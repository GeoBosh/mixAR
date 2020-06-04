\name{raghat1}
\alias{raghat1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Filter a time series with options to shift and scale
}
\description{
  Filter a time series with options to shift and scale. This function is
  used by mixFilter.
}
\usage{
raghat1(filter, x, index, shift = 0, residual = FALSE, scale = 1)
}
\arguments{
  \item{filter}{The coefficients of the filter, numeric, see Details.}
  \item{x}{time series, numeric.}
  \item{index}{
    indices for which to compute the filtered values, numeric.
  } 
  \item{shift}{
    a constant to be added to each filtered element, a number.
  }
  \item{residual}{
    if TRUE calculate a `residual', otherwise calculate a `hat' value.
  }
  \item{scale}{
    if \code{scale != 1} calculate scaled residuals by divividing by
    this value. Probably meaningful only if \code{residual=TRUE}.
  }
}
\details{
  This function is used by \code{mixFilter}. Applies an autoregressive
  filter to a time series for indices specified by \code{index}.

  Note that `filter' here is equivalent to calculating one-step
  predictions (or residuals if \code{residual=TRUE}) from
  autoregressions.

  \code{index} should not specify indices smaller than
  \code{length(filter)+1} or larger than \code{length(x)+1}.  The value
  \code{length(x)+1} can legitimately be used to calculate a prediction
  (but not a residual of course) for the first value after the end of the
  series.
}
\value{
  A numeric vector of length equal to \code{length(index)}.
}


\note{
  This should probably use \code{filter} but for the purposes of this
  package \code{filter} is usually short and the calculation is
  vectorised w.r.t. \code{index}, so should not be terribly slow.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ragged}
