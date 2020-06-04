\name{show_diff}
\docType{methods}
\alias{show_diff}
\alias{show_diff-methods}
\alias{show_diff,mixAR,mixAR-method}
\alias{show_diff,mixARGaussian,mixARgen-method}
\alias{show_diff,mixARgen,mixARGaussian-method}
\alias{show_diff,mixARgen,mixARgen-method}
\title{Show differences between two models}
\description{

  Show differences between two mixAR models in a way that enables quick
  comparison between them. This is a generic function, package
  \pkg{mixAR} defines methods for \code{mixAR} models.

}
\usage{
show_diff(model1, model2)
}
\arguments{
  \item{model1,model2}{
    the mixAR models to be compared.
  }
}
\details{

  \code{show_diff()} is a generic function with dispatch on both
  arguments.

  \code{show_diff()} prints the differences between two
  models in convenient form for comparison. The methods for mixAR models
  allow to see differences between similar models at a glance.
  
}
\value{
  The function is called for the side effect of printing the differences
  between the two models and has no useful return value.
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{Georgi N. Boshnakov}
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\section{Methods}{
  \describe{

    \item{\code{signature(model1 = "mixAR", model2 = "mixAR")}}{
      %%  ~~describe this method here~~
    }
    \item{\code{signature(model1 = "mixARGaussian", model2 = "mixARgen")}}{
      %%  ~~describe this method here~~
    }
    \item{\code{signature(model1 = "mixARgen", model2 = "mixARGaussian")}}{
      %%  ~~describe this method here~~
    }
    \item{\code{signature(model1 = "mixARgen", model2 = "mixARgen")}}{
      %%  ~~describe this method here~~
    }
}}
\examples{
## the examples reveal that the models below
##        differ only in the noise distributions
show_diff(exampleModels$WL_Ct_3, exampleModels$WL_Bt_1)
show_diff(exampleModels$WL_Bt_1, exampleModels$WL_Ct_3)
show_diff(exampleModels$WL_Ct_2, exampleModels$WL_Bt_3)
}
\keyword{methods}
\keyword{ mixAR }
