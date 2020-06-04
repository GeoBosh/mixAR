\name{tomarparambyComp}
\alias{tomarparambyComp}
\alias{tomarparambyType}
\alias{permuteArpar}
\title{
Translations of my old mixAR Mathematica functions
}
\description{
  Translations of some of my mixAR Mathematica functions.
  Not sure if these are still used.
}
\usage{
tomarparambyComp(params)
tomarparambyType(params)
permuteArpar(params)
}
\arguments{
  \item{params}{the parameters of the mixAR model, a list, see Details.}
}
\details{

  % These functions are translations of some of my Mathematica programs
  % (todo: check if in them the last component is sigma or sigma^2,
  % probably sigma^2, given the output of the functions here). I think
  % that 'permuteArpar' here corresponds to 'permuteMarComponents' in
  % Mathematica. Also, 'tomarparambyComp' here is for completeness, my
  % Mathematica programs do not have this currently.

  \code{tomarparambyComp} is for completeness, my Mathematica programs
  do not have this currently.
  
  The arrangement of the parameters of mixAR models in package
  \code{"mixAR"} is ``by type'': slot \code{prob} contains the mixture
  probabilities (weights), \code{shift} contains intercepts, and so on.

  An alternative representation is ``by component'': a list whose k-th
  elements contains all parameters associated with the k-th mixture
  component. The functions described here use the following order for
  the parameter of the k-th component: prob_k, shift_k, arcoeff_k,
  sigma2_k.

  \code{tomarparambyType} takes an argument, \code{params}, arranged
  ``by component'' and converts it to ``by type''.
  \code{tomarparambyComp} does the inverse operation, from ``by type''
  to ``by component''.

  \code{permuteArpar} creates all permutaions of the components of a
  mixAR model. It takes a ``by component'' argument. The autoregressive
  orders are not permuted, in that if the input model has AR orders
  \code{c(2, 1, 3)}, all permuted models are also \code{c(2, 1, 3)}.
  The AR coefficients of shorter or longer components are padded with
  zeroes or truncated, respectively, see the unexported
  \code{adjustLengths()}.

}
\value{

  For \code{tomarparambyComp}, a list containing the parameters of
  the model arranged ``by component'', see Details.

  For \code{tomarparambyType}, a list containing the parameters of
  the model arranged ``by type''. It contains the following elements.
  \item{prob}{mixture probabilities, a numeric vector,}
  \item{shift}{shifts, a numeric vector,}
  \item{arcoef}{autoregressive coefficients,}
  \item{s2}{noise variances, a numeric vector.}

  For \code{permuteArpar}, a list with one element (arranged ``by
  type'') for each possible permutation of the AR parameters. (todo:
  make clear!)
}
\author{Georgi N. Boshnakov}
\seealso{
  \code{\link{randomArCoefficients}}
}
\examples{
bycomp <- list(list(0.1, 10,  0.11,                1),
               list(0.2,  20, c(0.11, 0.22),       2),
               list(0.3,  30, c(0.11, 0.22, 0.33), 3) )
bytype <- tomarparambyType(bycomp)
identical(bycomp, tomarparambyComp(bytype)) # TRUE
               
permuteArpar(bycomp)
}
\keyword{mixAR}
