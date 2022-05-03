\name{mixAR_diag}
\alias{mixAR_diag}
\alias{tsdiag}
\alias{tsdiag.MixAR}

\title{
  Diagnostic checks for mixture autoregressive models
}
\description{
  Carry out diagnostic checks and tests on fitted mixAR models.
}
\usage{
\method{tsdiag}{MixAR}(object, gof.lag = NULL, y, ask = interactive(), \dots, 
       plot = interactive(), std.resid = FALSE)

mixAR_diag(model, y, \dots)
}
\arguments{
  \item{model,object}{
    the model on which to perform the checks, an object from class
    \code{MixAR}. \code{model} can also be the output list from
    \code{fit_mixAR}.
  }
  \item{gof.lag}{

    Goodness of fit lag(s) for the Ljung-Box tests. Vector containing
    one or more positive integers. \code{max(gof.lag)} is the maximal
    lag in the acf and pacf plots.
    
    how many lags to compute for acf and pacf? The default is as that of
    \code{lag.max} for \code{acf}.

  }
  \item{y}{a time series, currently a \code{numeric} vector.}
  \item{ask}{
    if \code{TRUE}, ask (using a menu) which plot to present. Otherwise
    just plot the selected plots. \code{ask} is ignored if only one plot 
    is selected with argument \code{plot}. 
  }
  \item{plot}{
    if \code{TRUE}, the default, produce diagnostic plots. If
    \code{FALSE} don't produce plots. Otherwise, a numeric vector of
    integers defining a subset of plots to consider, see Details.
  }
  \item{std.resid}{

    if \code{TRUE} standardise the ordinary residuals using
    the conditional standard deviations. \strong{NOTE:}  the default is
    currently \code{FALSE} but it may soon be changed to \code{TRUE}. 

  }
  \item{...}{for \code{mixAR_diag}, passed on to \code{tsdiag}.}
}
\details{

  It is recommended to use \code{tsdiag}.  \code{mixAR_diag} is
  essentially deprecated and is still here for compatibility with old
  code. Moreover, the \code{tsdiag} method is more flexible. The only
  advantage of \code{mixAR_diag} is that it accepts also a list for
  argument \code{model} but this is equivalent to calling \code{tsdiag}
  with \code{object = model$model}.

  
  The function calculates several types of residuals, provides
  diagnostic plots for each of them, and returns numerical results.
  The following choices are currently available:
  \enumerate{
    \item ACF/PACF of residuals,
    \item ACF/PACF of U_residuals,
    \item ACF/PACF of tau_residuals,
    \item ACF/Histogram of tau_residuals.
  }


  In interactive sessions the user is presented with a menu to select
  plot(s) from the available ones. The choice can be restricted to a
  subset of them by giving argument \code{plot} a vector of integers.
  This is most useful to select a particular plot, with somethinng like
  \code{plot = 2} in the call to \code{tsdiag}.  \code{plot} is used as
  an index vector, so \code{plot = -1} would remove the first item
  listed above from the offered alternatives.

  Transformations on the data are performed, as described in Smith
  (1985).

  Four types of residuals are computed:
  \describe{

    \item{ordinary residuals}{
      difference (possibly scaled) between observed values and point
      predictions.
    }
    \item{U_residuals/PIT residuals}{

      probability integral transform of the data using the CDF of the
      conditional distributions implied by the fitted model.  For a good
      model these should resemble an IID sequence uniformly distributed
      on (0,1).
      % equivalently, the one step ahead predictive distributions of
      % \code{y_t}. 
    }
    \item{V_residuals}{
      
      set of transformed \code{U_residuals} with the quantile function
      of the standard normal distribution (\code{qnorm}).  For a good
      model these should resemble an IID sequence from N(0,1).

    }
    \item{tau_residuals}{
      
      These residuals are calculated as the component specific residual
      \code{e_tk} divided by its corresponding scale \code{sigma_k},
      according to under which component y_t has largest density.  Under
      correct model specification, these should be jointly
      Normal. Shapiro-Wilk test is performed on this set of residual to
      assess the hypothesis.

    }


    For all types of residual results for the Ljung-Box test are
    provided. This test is particularly relevant for the V- and
    tau-residuals.
    
    Kolmogorov-Smirnov test is carried out for the U_residuals to assess
    the hypothesis of uniform distribution.
    
    Shapiro-Wilk test of normality} is applied to V- and tau-residuals.

}
\value{
  
  returns invisibly a list with class \code{"tsdiagMixAR"}, currently
  containing the following components:

  \item{residuals}{ordinary residuals,}
  \item{U_residuals}{see Details,}
  \item{V_residuals}{see Details,}
  \item{tau_residuals}{see Details,}
  \item{BIC}{the value of the BIC criterion, a number.}

  Each component, except \code{BIC}, is a list containing the residuals
  in component \code{value}, Ljung-Box test in \code{"Ljung-Box"} and
  possibly other tests suitable for the corresponding type of
  residuals. 
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
  \code{\link{mixAR_BIC}},
  \code{\link[stats]{tsdiag}}
}
\examples{
model1 <- new("MixARGaussian", prob = c(0.5, 0.5), scale = c(1, 2),
              arcoef = list(-0.5, 1.1))
set.seed(123)
y <- mixAR_sim(model1, 400, c(0,0,0), nskip = 100) 

fit1 <- fit_mixAR(y, model1)
d <- tsdiag(fit1$model, c(10, 20, 50), y)
d
## This will put each plot in a separate file (mydiag01.pdf, ..., mydiag04.pdf)
## pdf("mydiag%02d.pdf", onefile = FALSE)
## d <- tsdiag(fit1$model, c(10, 20, 50), y,  ask = FALSE)
## dev.off()
}
\keyword{ MixAR }% use one of  RShowDoc("KEYWORDS")

