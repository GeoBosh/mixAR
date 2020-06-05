\name{noise_moment-methods}
\docType{methods}
\alias{noise_moment}
\alias{noise_moment-methods}
\alias{noise_moment,ANY,ANY-method}
\alias{noise_moment,MixARGaussian,numeric-method}
\alias{noise_moment,MixARgen,numeric-method}
\title{Compute moments of the noise components}
\description{Compute moments of the noise components.}
\usage{
noise_moment(model, k)
}
\arguments{
  \item{model}{a MixAR model.}
  \item{k}{which moment to compute?}
}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%%  ~Describe the value returned
%%%  If it is a LIST, use
%%%  \item{comp1 }{Description of 'comp1'}
%%%  \item{comp2 }{Description of 'comp2'}
%%% ...
%}
\section{Methods}{
\describe{

\item{\code{signature(model = "ANY", k = "ANY")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(model = "MixARGaussian", k = "numeric")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(model = "MixARgen", k = "numeric")}}{
%%  ~~describe this method here~~
}
}}
%\examples{
%# todo:
%}
\keyword{methods}
\keyword{MixAR}
\keyword{internal}
