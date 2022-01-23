\name{PortfolioData1}
\alias{PortfolioData1}
\docType{data}
\title{Closing prices of four stocks}
\description{
  Closing prices of four stocks.
}
\usage{
data("PortfolioData1")
}
\format{
  A data frame with 867 observations on the following 4 variables.
  \describe{
    \item{\code{DELL}}{numeric, Dell Technologies Inc.}
    \item{\code{MSFT}}{numeric, Microsoft Corporation.}
    \item{\code{INTC}}{numeric, Intel Corporation.}
    \item{\code{IBM}}{numericr, International Business Machine Corporation.}
  }
}
\details{

  Time series of daily adjusted close prices of the above stocks from 2
  January 2016 to 29 January 2020 (867 observations).

}
\source{
  \samp{https://finance.yahoo.com/}
}
% \references{
%    TODO: put here a reference to the portfolio paper
% %%  ~~ possibly secondary sources and usages ~~
% }
\examples{
data(PortfolioData1)
dim(PortfolioData1)
head(PortfolioData1)
}
\keyword{datasets}
