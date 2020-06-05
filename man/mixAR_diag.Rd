\name{mixAR_diag}
\alias{mixAR_diag}
\alias{tsdiag}
\alias{tsdiag.MixARGaussian}

\title{
  Diagnostic checks for mixture autoregressive models
}
\description{
  Perform diagnostic checks and tests on given \code{MixARGaussian}
  object with respect to a given time series.
}
\usage{
\method{tsdiag}{MixARGaussian}(object, gof.lag, y, ask = interactive(), ...)

mixAR_diag(model, y, ask = interactive())
}
\arguments{
  \item{model,object}{
    the model on which to perform the checks, an object from class
    \code{MixAR}. \code{model} can also be the output list from
    \code{fit_mixAR}.
  }
  \item{gof.lag}{
    how many lags to compute for acf and pacf? The default is as that of
    \code{lag.max} for \code{acf}.
  }
  \item{y}{A time series, currently a \code{numeric} vector.}
  \item{ask}{if \code{TRUE} ask before presenting a new plot.}
  \item{...}{currently not used.}
}
\details{

  It is recommended to use \code{tsdiag}.  \code{mixAR_diag} is
  essentially deprecated and is still here for compatibility with old
  code. Moreover, the \code{tsdiag} method is more flexible. The only
  advantage of \code{mixAR_diag} is that it accepts also a list for
  argument \code{model} but this is equivalent to calling \code{tsdiag}
  with \code{object = model$model}.

  The function calculates two types of residuals. The first set
  calculates residuals as of canonical definition.  ACF and Partial ACF
  plots are given.

  Transformations on the data are performed, as described in Smith
  (1985).

  The following tests are available:

  \describe{

    \item{Ljung-Box test}{
      is applied to \code{residuals}, checking for residual correlation
      up to lag 20.
    }

    \item{Kolmogorov-Smirnov test}{
      is applied to \code{U} to assess the hypothesis of uniform
      distribution.
    }

    \item{
      Shapiro-Wilk test of Normality}{ is applied to \code{V}, where
      \code{V = qnorm(U)}.
    }
  }

  The second set is calculated as the component specific residual
  \code{e_tk} divided by its corresponding scale \code{sigma_k},
  according to under which component y_t has largest density.  Under
  correct model specification, these should be jointly
  Normal. Shapiro-Wilk test is performed on this set of residual to
  assess the hypothesis.

}
\value{
  \item{residuals}{the first set of residuals,}
  \item{U}{CDF of the one step ahead predictors of \code{y_t},}
  \item{V}{set of transformed \code{U} by use of \code{qnorm},}
  \item{tau_residuals}{The second set of residuals, as described,}
  \item{tests}{
    a \code{list} containing the results of the tests deescribed in Details.
  }
}
\references{
  \insertRef{Smith1985}{mixAR}
  
  \insertRef{WongLi2000}{mixAR}
}
\author{Davide Ravagli and Georgi N. Boshnakov}
\note{
  This function should be used for diagnostic checking of \code{MixARGaussian}
  objects only.
}
\seealso{
  \code{\link{mixAR_BIC}}
}
\examples{
model1 <- new("MixARGaussian", prob = c(0.5, 0.5), scale = c(1, 2),
              arcoef = list(-0.5, 1.1))
set.seed(123)
y <- mixAR_sim(model1, 400, c(0,0,0), nskip = 100) 

fit1 <- fit_mixAR(y, model1)
mixAR_diag(fit1, y)
}
\keyword{ MixAR }% use one of  RShowDoc("KEYWORDS")
%\keyword{}
