\name{MixARgen-class}
\Rdversion{1.1}
\docType{class}
\alias{MixARgen-class}
\alias{mixARgen}
%\alias{initialize,MixARgen-method}
% \alias{lik_params,MixARgen-method}
%\alias{mix_cdf,MixARgen,missing,missing,numeric-method}
%\alias{mix_cdf,MixARgen,numeric,missing,numeric-method}
%\alias{mix_cdf,MixARgen,numeric,numeric,missing-method}
%\alias{mix_pdf,MixARgen,missing,missing,numeric-method}
%\alias{mix_pdf,MixARgen,numeric,missing,numeric-method}
%\alias{mix_pdf,MixARgen,numeric,numeric,missing-method}
% \alias{noise_dist,MixARgen-method}
% \alias{noise_params,MixARgen-method}
% \alias{noise_rand,MixARgen-method}

\title{Class \code{"MixARgen"}}
\description{
  A class for MixAR models with arbitrary noise distributions. \code{"MixARgen"}
  inherits from \code{"MixAR"}.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("MixARgen",
  dist, ...)}  or \code{mixARgen(...)}. The two forms are completely
  equivalent. The latter is available from version 0.19-15 of package
  MixAR.
}
\section{Slots}{
  Most slots are inherited from class \code{"\linkS4class{MixAR}"}.
  \describe{
    \item{\code{prob}:}{the mixing probabilities, \code{"numeric"}.}
    \item{\code{order}:}{the AR orders, \code{"numeric"}.}
    \item{\code{shift}:}{intercept terms, \code{"numeric"}.}
    \item{\code{scale}:}{scaling factor, \code{"numeric"}.}
    \item{\code{arcoef}:}{
      autoregressive coefficients, an object from class
      \code{"raggedCoef"} containing one row for each mixture component.
    }
    \item{\code{dist}:}{Object of class \code{"list"}, representing the
      noise distributions. The list contains one element for each
      component of the MixAR model or a single element if the
      noise distribution is the same for all components.

      If the distributions do not contain parameters (e.g. Gaussian or
      \eqn{t_4}) it is sufficient to give the list of functions in the
      element \code{dist} of the list.

      If the distributions do contain parameters the recommended
      arrangement is to give a list with components \code{generator} and
      \code{param}, such that a call \code{generator(param)} should
      produce the required list of distributions.

      This is not finalised but if changed, backward compatibility
      with existing objects will be maintained.
    }
  }
}
\section{Extends}{
Class \code{"\linkS4class{MixAR}"}, directly.
}
\section{Methods}{
  \describe{
    
    \item{get_edist}{\code{signature(model = "MixARgen")}: ... }
    \item{initialize}{\code{signature(.Object = "MixARgen")}: ... }
    \item{lik_params}{\code{signature(model = "MixARgen")}: ... }
    
    \item{mix_cdf}{\code{signature(model = "MixARgen", x = "missing", index = "missing", xcond = "numeric")}: ... } 

    \item{mix_cdf}{\code{signature(model = "MixARgen", x = "numeric", index = "missing", xcond = "numeric")}: ... }

    \item{mix_cdf}{\code{signature(model = "MixARgen", x = "numeric", index = "numeric", xcond = "missing")}: ... }

    \item{mix_pdf}{\code{signature(model = "MixARgen", x = "missing", index = "missing", xcond = "numeric")}: ... }

    \item{mix_pdf}{\code{signature(model = "MixARgen", x = "numeric", index = "missing", xcond = "numeric")}: ... }

    \item{mix_pdf}{\code{signature(model = "MixARgen", x = "numeric", index = "numeric", xcond = "missing")}: ... }

    \item{noise_dist}{\code{signature(model = "MixARgen")}: ... }
    \item{noise_params}{\code{signature(model = "MixARgen")}: ... }
    \item{noise_rand}{\code{signature(model = "MixARgen")}: ... }}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
showClass("MixARgen")

exampleModels$WL_ibm_gen@dist
noise_dist(exampleModels$WL_ibm_gen, "cdf")
noise_dist(exampleModels$WL_ibm_gen, "pdf")
noise_dist(exampleModels$WL_ibm_gen, "pdf", expand = TRUE)
noise_dist(exampleModels$WL_ibm_gen, "cdf", expand = TRUE)

## data(ibmclose, package = "fma")  # for `ibmclose'

pdf1 <- mix_pdf(exampleModels$WL_ibm, xcond = as.numeric(fma::ibmclose))
cdf1 <- mix_cdf(exampleModels$WL_ibm, xcond = as.numeric(fma::ibmclose))
gbutils::plotpdf(pdf1, cdf = cdf1, lq = 0.001, uq = 0.999)

pdf1gen <- mix_pdf(exampleModels$WL_ibm_gen, xcond = as.numeric(fma::ibmclose))
cdf1gen <- mix_cdf(exampleModels$WL_ibm_gen, xcond = as.numeric(fma::ibmclose))
gbutils::plotpdf(pdf1gen, cdf = cdf1gen, lq = 0.001, uq = 0.999)

length(fma::ibmclose)
cdf1gena <- mix_cdf(exampleModels$WL_ibm_gen, xcond = as.numeric(fma::ibmclose)[-(369:369)])
pdf1gena <- mix_pdf(exampleModels$WL_ibm_gen, xcond = as.numeric(fma::ibmclose)[-(369:369)])
gbutils::plotpdf(pdf1gena, cdf = cdf1gena, lq = 0.001, uq = 0.999)

pdf1a <- mix_pdf(exampleModels$WL_ibm, xcond = as.numeric(fma::ibmclose)[-(369:369)])
cdf1a <- mix_cdf(exampleModels$WL_ibm, xcond = as.numeric(fma::ibmclose)[-(369:369)])
gbutils::plotpdf(pdf1a, cdf = cdf1a, lq = 0.001, uq = 0.999)


cdf1gena <- mix_cdf(exampleModels$WL_ibm_gen, xcond = as.numeric(fma::ibmclose)[-(369:369)])

cond_loglik(exampleModels$WL_ibm, as.numeric(fma::ibmclose))
cond_loglik(exampleModels$WL_ibm_gen, as.numeric(fma::ibmclose))

ts1gen <- mixAR_sim(exampleModels$WL_ibm_gen, n = 30, init = c(346, 352, 357), nskip = 0)
plot(ts1gen)

plot(mixAR_sim(exampleModels$WL_ibm_gen, n = 100, init = c(346, 352, 357), nskip = 0),
     type = "l")

plot(diff(mixAR_sim(exampleModels$WL_ibm_gen, n = 100, init = c(346, 352, 357), nskip = 0)),
     type = "l")

noise_dist(exampleModels$WL_ibm_gen, "Fscore")

prob   <- exampleModels$WL_ibm@prob
scale  <- exampleModels$WL_ibm@scale
arcoef <- exampleModels$WL_ibm@arcoef@a

mo_WLt3  <- new("MixARgen", prob = prob, scale = scale, arcoef = arcoef,
                dist = list(fdist_stdt(3)))
mo_WLt30 <- new("MixARgen", prob = prob, scale = scale, arcoef = arcoef,
                dist = list(fdist_stdt(30)))
}
\keyword{classes}
