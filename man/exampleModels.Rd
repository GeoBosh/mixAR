\name{exampleModels}
\alias{exampleModels}
\alias{moWLprob}  % old objects, superceded by exampleModels$xxx
\alias{moWLsigma}
\alias{moWLar}
\alias{moWL_I}
\alias{moWL_II}
\alias{moWL_A}
\alias{moWL_B}
\alias{moWL}
\alias{moWLgen}
\alias{moWLt3v}
\alias{moWLtf}
\alias{moT_A}
\alias{moT_B}
\alias{moT_B2}
\alias{moT_B3}
\alias{moT_C1}
\alias{moT_C2}
\alias{moT_C3}

\title{
  MixAR models for examples and testing
}
\description{
  MixAR models for examples and testing.
}
\usage{
exampleModels
}
\details{

  Coefficients of models from the examples in
  \insertCite{WongLi2000;textual}{mixAR}. Variations on these with different
  noise distributions are used throughout the examples in \pkg{mixAR}.
  The models are from classes inheriting from class \code{"MixAR"}.

  \code{exampleModels} is a list with the following components:

  %% TODO: Initially replaced this since pkgdown::build_site() gives error
  %%       (at the time of trying this) when 'results=verbatim'
  %%
  %%    \Sexpr[stage=build,results=verbatim]{names(exampleModels)}
  %%
  %% However, using tabular and results=rd seems better anyway.
  %%
  %% NOTE: Without the pair of braces after \cr, there is an wanted
  %%       space in the pdf rendering before each row, except the first,
  %%       resulting in:
  %%
  %%         WL_I
  %%          WL_II
  %%          WL_A
  %%          ...
  %% TODO: Take note somewhere since I recall wrestling with this in
  %%       similar circumstances elsewhere.
  %%
  %% TODO: Eventually, each entry needs to be documented.
  \tabular{l}{
  \Sexpr[stage=build,results=rd]{paste0(names(mixAR::exampleModels), collapse = " \\\\cr{}")}
  }
  
  Each component is a MixAR model, i.e. an object inheriting from class
  \code{"MixAR"}. 
  
}
\source{
  \insertAllCited{}
}
%\references{
%}
\examples{
## use these instead of moWL, moWL_A, moWL_B, etc.
exampleModels$WL_ibm

exampleModels$WL_A
exampleModels$WL_B
# what is the difference between A and B?
show_diff(exampleModels$WL_A, exampleModels$WL_B)

exampleModels$WL_I
exampleModels$WL_II
#show_diff(exampleModels$WL_I, exampleModels$WL_II)

exampleModels$WL_ibm_gen
exampleModels$WL_ibm_t3v
exampleModels$WL_ibm_tf
#show_diff(exampleModels$WL_ibm_gen, exampleModels$WL_ibm_t3v)

exampleModels$WL_At

exampleModels$WL_Bt_1
exampleModels$WL_Bt_2
exampleModels$WL_Bt_3
## what is different between Bt_2 and Bt_1? (df of component 2)
show_diff(exampleModels$WL_Bt_2, exampleModels$WL_Bt_1)

exampleModels$WL_Ct_1
exampleModels$WL_Ct_2
exampleModels$WL_Ct_3

## The models were created with something like:
moWLprob <- c(0.5439,0.4176,0.0385)
moWLsigma <- c(4.8227,6.0082,18.1716)
moWLar <- list(c(0.6792,0.3208), c(1.6711,-0.6711), 1)

moWL <- new("MixARGaussian", prob = moWLprob, scale = moWLsigma,
            arcoef = moWLar)
moWLgen <- new("MixARgen", prob = moWLprob, scale = moWLsigma,
               arcoef = moWLar, dist = list(dist_norm))
## clean up a bit
rm(moWLprob, moWLsigma, moWLar, moWL, moWLgen)
}
\keyword{MixAR}
