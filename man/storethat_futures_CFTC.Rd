% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.r
\name{storethat_futures_CFTC}
\alias{storethat_futures_CFTC}
\title{Retrieves futures CFTC position historical data from from a
  \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.}
\usage{
storethat_futures_CFTC(
  active_contract_tickers,
  start,
  end,
  file = NULL,
  verbose = T
)
}
\arguments{
\item{active_contract_tickers}{a chatacter vector. Specifies the futures active
contract Bloomberg tickers to query data for.}

\item{start}{a scalar character vector. Specifies the starting date for the query in
the following format: 'yyyy-mm-dd'.}

\item{end}{a scalar character vector. Specifies the end date for the query in the
following format: 'yyyy-mm-dd'.}

\item{file}{a scalar chatacter vector. Specifies the target \pkg{storethat}
SQLite database file.}

\item{verbose}{a logical scalar vector. Should progression messages be printed?
Defaults to TRUE.}
}
\value{
An S4 object of class \linkS4class{FuturesCFTC}.
}
\description{
Provided with a set of Bloomberg futures active contract tickers
  and a time period, retrieves the corresponding futures CFTC position historical
  data previously stored in a
  \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
}
\examples{
\dontrun{

    storethat_futures_CFTC(active_contract_tickers = c("W A Comdty", "KWA Comdty"),
      start = "2000-01-01", end = as.character(Sys.Date()))

  }


}
\seealso{
\itemize{

    \item{"GFUT <GO>" on a Bloomberg terminal.}

    \item{The \link[BBGsymbols]{tickers_cftc} dataset in the
    \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
    (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite) for details
    on the Bloomnerg position tickers used here.}

    \item{Helper datasets in the
    \href{https://github.com/bautheac/fewISOs/}{\pkg{fewISOs}} package
    (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite).}

    \item{The \href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite,
    in particular the \href{https://github.com/bautheac/storethat/}{\pkg{storethat}}
    package.}

  }
}
