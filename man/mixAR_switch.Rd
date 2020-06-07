\name{mixAR_switch}

\alias{mixAR_switch}
\alias{mixAR_permute}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{Relabel the components of a MixAR model}

\description{Relabel the components of a MixAR model.}

\usage{
mixAR_switch(model, perm)
mixAR_permute(model, perm)
}

%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{model}{a MixAR model}
  \item{perm}{a permutation for relabeling}
}

\details{
  If the permutation is the identity permutation the model is returned
  as is. Otherwise the order of the components is changed according to
  \code{perm}.  Basically, \code{perm} is used as index,
  e.g. \code{prob[perm]}, etc.

  % TODO: There is something strange here---is \code{mixAR_switch} an
  % older (and wrong, it does not permute \code{order}) variant and
  % \code{mixAR_permute} the real thing?

}


\note{
  Currently the function only reorders the "usual"
  components. Subclasses of "MixAR" may contain other parameters
  (e.g. different error distributions). So this function may not be
  appropriate for them.

  % todo: make it generic.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}


% Add one or more standard keywords, see file 'KEYWORDS' in the

% R documentation directory.

\keyword{MixAR}
