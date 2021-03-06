% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.r
\name{pull_equity_market}
\alias{pull_equity_market}
\title{Retrieves equity historical market data.}
\usage{
pull_equity_market(
  source = "Bloomberg",
  tickers,
  start,
  end,
  verbose = T,
  file = NULL,
  ...
)
}
\arguments{
\item{source}{a scalar character vector. Specifies the data source for the query:
"Bloomberg" or "storethat". Defaults to "Bloomberg".}

\item{tickers}{a chatacter vector. Specifies the equity Bloomberg tickers to query
data for.}

\item{start}{a scalar character vector. Specifies the starting date for the query
in the following format: 'yyyy-mm-dd'.}

\item{end}{a scalar character vector. Specifies the end date for the query in the
following format: 'yyyy-mm-dd'.}

\item{verbose}{a logical scalar vector. Should progression messages be printed?
Defaults to TRUE.}

\item{file}{a scalar chatacter vector. Optional parameter that specifies the
target \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite
database file to retrieve data from.}

\item{...}{optional parameters to pass to the \link[Rblpapi]{bdh} function from the
\href{http://dirk.eddelbuettel.com/code/rblpapi.html}{\pkg{Rblpapi}} package used
  for the query (\code{options} parameter).}
}
\value{
An S4 object of class \linkS4class{EquityMarket}.
}
\description{
Provided with a set of equity Bloomberg tickers and a time period,
  queries Bloomberg for the corresponding equity historical market data or
  retrieves it from an existing
  \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database..
}
\examples{
\dontrun{

    BBG_mkt <- pull_equity_market(source = "Bloomberg",
      tickers = c("BP/ LN Equity", "WEIR LN Equity"),
      start = "2000-01-01", end = as.character(Sys.Date()))

    storethat_mkt <- pull_equity_market(source = "storethat",
      tickers = c("BP/ LN Equity", "WEIR LN Equity"),
      start = "2000-01-01", end = as.character(Sys.Date()))

  }


}
\seealso{
\itemize{

    \item{The \link[BBGsymbols]{fields} dataset in the
    \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite)
     for details on the Bloomnerg fields used here.}

    \item{Helper datasets in the
    \href{https://github.com/bautheac/fewISOs/}{\pkg{fewISOs}} and
    \href{https://github.com/bautheac/GICS/}{\pkg{GICS}} packages
    (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite).}

  }
}
