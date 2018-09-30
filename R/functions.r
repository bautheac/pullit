# Bloomberg ####

## tickers ####

#' Bloomberg futures term structure ticker
#'
#'
#' @description Provided with a futures active contract Bloomberg ticker,
#'   a term structure position and a set of roll parameters, constructs the
#'   corresponding futures term structure Bloomberg ticker according to
#'   Bloomberg construction method.
#'
#'
#' @param active_contract_ticker a scalar chatacter vector. Specifies the
#'   futures active contract Bloomberg ticker to use for term structure
#'   ticker construction.
#'
#' @param TS_position a scalar integer vector. Specifies the term structure
#'   position desired.
#'
#' @param roll_type a scalar chatacter vector. Specifies roll type to use
#'   for term structure ticker construction. Must be one of 'A', 'B', 'D',
#'   'F', 'N', 'O' or 'R'.
#'
#' @param roll_days a scalar integer vector. Specifies the day the roll
#'   should be done. Refers to the day of the month (\code{roll_type} = 'F')
#'   or the number of days before a reference date (\code{roll_type} = 'D',
#'   \code{roll_type} = 'N', \code{roll_type} = 'O', \code{roll_type} = 'R').
#'   Works in tandem with `roll_months` below.
#'
#' @param roll_months a scalar integer vector. Specifies the month the roll
#'   should be done. Refers to the number of months before a reference date
#'   (\code{roll_type} = 'D', \code{roll_type} = 'N', \code{roll_type} = 'O',
#'   \code{roll_type} = 'R'). Works in tandem with `roll_days` above.
#'
#' @param roll_adjustment a scalar chatacter vector. Specifies roll adjustment
#'   method to use for term structure ticker construction. Must be one of 'D',
#'   'N', 'R', or 'W'.
#'
#'
#' @return A scalar character vector containing the corresponding term
#'   structure ticker.
#'
#'
#' @seealso
#'
#'   \itemize{
#'
#'     \item{"GFUT <GO>" &/or "DOCS #2072138 <GO>" on a Bloomberg terminal to
#'     learn more about the Bloomberg rolling conventions.}
#'
#'     \item{The \link[BBGsymbols]{rolls} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'      (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}}.}
#'
#'   }
#'
#'
#' @examples futures_ticker(active_contract_ticker = "C A Comdty", TS_position = 5L,
#'   roll_type = "A", roll_days = 0L, roll_months = 0L, roll_adjustment = "N")
#'
#'
#' @export
futures_ticker <- function(active_contract_ticker = "C A Comdty", TS_position = 1L,
                           roll_type = "A", roll_days = 0L, roll_months = 0L,
                           roll_adjustment = "N"){

  if (! rlang::is_scalar_character(active_contract_ticker))
    stop("The parameter 'active_contract_ticker' must be supplied as a scalar
         character vector")

  if (! rlang::is_scalar_integer(TS_position))
    stop("The parameter 'TS_position' must be supplied as a scalar integer vector")

  if (! all(rlang::is_scalar_character(roll_type),
            roll_type %in% c("A", "B", "D", "F", "N", "O", "R")))
    stop("The parameter 'roll_type' must be one of 'A', 'B', 'D', 'F', 'N', 'O' or 'R'")

  if (! all(rlang::is_scalar_integer(roll_days), roll_days <= 31L))
    stop("The parameter 'roll_days' must be supplied as a scalar integer
         vector between 0 and 31")

  if (! all(rlang::is_scalar_integer(roll_months), roll_months <= 12L))
    stop("The parameter 'roll_months' must be supplied as a scalar integer
         vector between 0 and 12")

  if (! all(rlang::is_scalar_character(roll_adjustment),
            roll_adjustment %in% c("D", "N", "R", "W")))
    stop("The parameter 'roll_adjustment' must be one of 'D', 'N', 'R' or 'W'")


  split <- stringr::str_split(string = active_contract_ticker, pattern = "A ",
                              simplify = FALSE) %>% purrr::flatten_chr()

  paste0(split[NROW(split) - 1L], TS_position, " ", roll_type, ":",
         dplyr::if_else(roll_days < 10L, paste0(0L, roll_days), paste0(roll_days)), "_",
         roll_months, "_", roll_adjustment, " ", split[NROW(split)])

}


## futures ####

### market ####

#### global ####

#' Futures market historical data from Bloomberg
#'
#'
#' @description Provided with a set of futures active contract Bloomberg tickers,
#'   term structure positions, roll parameters and a time period, queries Bloomberg
#'   for the corresponding futures historical market data.
#'
#'
#' @param type a scalar character vector, 'term sructure' or 'aggregate'.
#'   'term structure' returns individual futures chain data for a selected portion
#'   of the term structure (specify desired positions in \code{TS_positions}) while
#'   'aggregate' returns aggregated data over the whole term structure for the
#'   corresponding names (\code{active_contract_tickers}).
#'
#' @param active_contract_tickers a chatacter vector. Specifies the futures active
#'   contract Bloomberg tickers to query data for.
#'
#' @param start a scalar character vector. Specifies the starting date for the query
#'   in the following format: 'yyyy-mm-dd'.
#'
#' @param end a scalar character vector. Specifies the end date for the query in the
#'   following format: 'yyyy-mm-dd'.
#'
#' @param TS_positions An integer vector. Specifies the term structure positions to
#'   query data for.
#'
#' @param roll_type a scalar chatacter vector. Specifies roll type to use for term
#'   structure ticker construction. Must be one of 'A', 'B', 'D', 'F', 'N', 'O' or 'R'.
#'
#' @param roll_days a scalar integer vector. Specifies the day the roll should be done.
#'   Refers to the day of the month (\code{roll_type} = 'F') or the number of days
#'   before a reference date (\code{roll_type} = 'D', \code{roll_type} = 'N',
#'   \code{roll_type} = 'O', \code{roll_type} = 'R'). Works in tandem with
#'   \code{roll_months} below.
#'
#' @param roll_months a scalar integer vector. Specifies the month the roll should
#'   be done. Refers to the number of months before a reference date
#'   (\code{roll_type} = 'D', \code{roll_type} = 'N', \code{roll_type} = 'O',
#'   \code{roll_type} = 'R'). Works in tandem with \code{roll_days} above.
#'
#' @param roll_adjustment a scalar chatacter vector. Specifies roll adjustment method
#'   to use for term structure ticker construction. Must be one of 'D', 'N', 'R', or 'W'.
#'
#' @param verbose a logical scalar vector. Should progression messages be printed?
#'   Defaults to TRUE.
#'
#' @param ... optional parameters to pass to the \link[Rblpapi]{bdh} function from the
#' \href{http://dirk.eddelbuettel.com/code/rblpapi.html}{\pkg{Rblpapi}} package used
#'   for the query (\code{options} parameter).
#'
#'
#' @return An S4 object of class \linkS4class{FuturesTS} (\code{type = 'term structure'})
#'   or \linkS4class{FuturesAggregate} (\code{type = 'aggregate'}).
#'
#'
#' @seealso
#'
#'   \itemize{
#'
#'     \item{"GFUT <GO>" on a Bloomberg terminal.}
#'
#'     \item{The \link[BBGsymbols]{rolls} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite)
#'     for details regarding the \code{roll_type} & \code{roll_adjustment} parameters.}
#'
#'     \item{The \link[BBGsymbols]{fields} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'     for details on the Bloomnerg fields used here.}
#'
#'     \item{Helper datasets in the
#'     \href{https://github.com/bautheac/fewISOs/}{\pkg{fewISOs}} package
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite).}
#'
#'   }
#'
#'
#' @examples \dontrun{
#'
#'     BBG_futures_market(type = 'term structure',
#'       active_contract_tickers = c("W A Comdty", "KWA Comdty"),
#'       start = "2000-01-01", end = as.character(Sys.Date()),
#'       TS_positions = 1L:5L, roll_type = "A", roll_days = 0L,
#'       roll_months = 0L, roll_adjustment = "N")
#'
#'     BBG_futures_market(type = "aggregate",
#'       active_contract_tickers = c("W A Comdty", "KWA Comdty"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'   }
#'
#'
#' @import BBGsymbols
#' @importFrom magrittr "%<>%"
#'
#'
#' @export
BBG_futures_market <- function(type, active_contract_tickers, start, end, TS_positions = NULL, roll_type = NULL,
                               roll_days = NULL, roll_months = NULL, roll_adjustment = NULL, verbose = TRUE, ...){

  utils::data(list = c("fields", "rolls"), package = "BBGsymbols", envir = environment())

  if (! is.character(active_contract_tickers))
    stop("The parameter 'active_contract_tickers' must be supplied as a character vector of futures active contract Bloomberg tickers")

  if (! all(rlang::is_scalar_character(type),
            type %in% (dplyr::distinct(dplyr::filter(fields, instrument == "futures", book == "market"), type) %>% purrr::flatten_chr())))
    stop("The parameter 'type' must be supplied as a scalar character vector; one of '",
         paste((dplyr::distinct(dplyr::filter(fields, instrument == "futures", book == "market"), type) %>% purrr::flatten_chr()), collapse = "', '"), "'")

  if (! all(stringr::str_detect(c(start, end), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))
    stop("The parameters 'start' and 'end' must be supplied as scalar character vectors of dates (yyyy-mm-dd)")

  if (! is.null(TS_positions))
    if (! all(is.integer(TS_positions))) stop("The parameters 'TS_positions' must be supplied as a vector of integers")

  if (! all(is.null(roll_days), is.null(roll_months)))
    if (! all(rlang::is_scalar_integer(roll_days), rlang::is_scalar_integer(roll_months)))
      stop("The parameters 'roll_days' and 'roll_months' must be supplied as scalar integer vectors")

  if (! all(is.null(roll_type), is.null(roll_adjustment)))
    if (! all(rlang::is_scalar_character(roll_type), rlang::is_scalar_character(roll_adjustment),
              roll_type %in% dplyr::filter(rolls, roll == "type")$symbol, roll_adjustment %in% dplyr::filter(rolls, roll == "adjustment")$symbol))
      stop("The parameters 'roll_type' and 'roll_adjustment' must be supplied as scalar character vectors; one of '",
           paste(dplyr::filter(rolls, roll == "type")$symbol, collapse = "', '"), "' ('roll_type') or '",
           paste(dplyr::filter(rolls, roll == "adjustment")$symbol, collapse = "', '"), "' ('roll_adjustment')")

  if (! rlang::is_scalar_logical(verbose)) stop("The parameter 'verbose' must be supplied as a scalar logical vector")

  switch(type,
         `term structure` = BBG_futures_TS(active_contract_tickers = active_contract_tickers, start = start, end = end, TS_positions = TS_positions,
                                           roll_type = roll_type, roll_days = roll_days, roll_months = roll_months, roll_adjustment = roll_adjustment,
                                           verbose = verbose, ...),
         `aggregate` = BBG_futures_aggregate(active_contract_tickers = active_contract_tickers, start = start, end = end, verbose = verbose, ...)
  )
}


#### term structure ####

#' Futures term structure historical data from Bloomberg
#'
#'
#' @description Provided with a set of futures active contract Bloomberg tickers,
#'   term structure positions, roll parameters and a time period, queries Bloomberg
#'   for the corresponding term structure historical data.
#'
#'
#' @param active_contract_tickers a chatacter vector. Specifies the futures active
#'   contract Bloomberg tickers to query data for.
#'
#' @param start a scalar character vector. Specifies the starting date for the query
#'   in the following format: 'yyyy-mm-dd'.
#'
#' @param end a scalar character vector. Specifies the end date for the query in the
#'   following format: 'yyyy-mm-dd'.
#'
#' @param TS_positions an integer vector. Specifies the term structure positions to
#'   query data for.
#'
#' @param roll_type a scalar chatacter vector. Specifies roll type to use for term
#'   structure ticker construction. Must be one of 'A', 'B', 'D', 'F', 'N', 'O' or 'R'.
#'
#' @param roll_days a scalar integer vector. Specifies the day the roll should be
#'   done. Refers to the day of the month (\code{roll_type} = 'F') or the number of
#'   days before a reference date (\code{roll_type} = 'D', \code{roll_type} = 'N',
#'   \code{roll_type} = 'O', \code{roll_type} = 'R'). Works in tandem with
#'   \code{roll_months} below.
#'
#' @param roll_months a scalar integer vector. Specifies the month the roll should be
#'   done. Refers to the number of months before a reference date (\code{roll_type} = 'D',
#'   \code{roll_type} = 'N', \code{roll_type} = 'O', \code{roll_type} = 'R'). Works in
#'   tandem with \code{roll_days} above.
#'
#' @param roll_adjustment a scalar chatacter vector. Specifies roll adjustment method
#'   to use for term structure ticker construction. Must be one of 'D', 'N', 'R', or 'W'.
#'
#' @param verbose a logical scalar vector. Should progression messages be printed?
#'  Defaults to TRUE.
#'
#' @param ... optional parameters to pass to the \link[Rblpapi]{bdh} function from the
#' \href{http://dirk.eddelbuettel.com/code/rblpapi.html}{\pkg{Rblpapi}} package used
#'   for the query (\code{options} parameter).
#'
#'
#' @return An S4 object of class \linkS4class{FuturesTS}.
#'
#'
#' @seealso
#'   \itemize{
#'
#'     \item{"GFUT <GO>" on a Bloomberg terminal.}
#'
#'     \item{The \link[BBGsymbols]{rolls} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite) for details
#'     regarding the \code{roll_type} & \code{roll_adjustment} parameters.}
#'     \item{The \link[BBGsymbols]{fields} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'     for details on the Bloomnerg fields used here.}
#'
#'     \item{Helper datasets in the
#'     \href{https://github.com/bautheac/fewISOs/}{\pkg{fewISOs}} package
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite).}
#'
#'   }
#'
#'
#' @examples \dontrun{
#'
#'     BBG_futures_market(type = 'term structure',
#'       active_contract_tickers = c("W A Comdty", "KWA Comdty"),
#'       start = "2000-01-01", end = as.character(Sys.Date()),
#'       TS_positions = 1L:5L, roll_type = "A", roll_days = 0L,
#'       roll_months = 0L, roll_adjustment = "N")
#'
#'   }
#'
#'
#' @import BBGsymbols
#' @importFrom magrittr "%<>%"
BBG_futures_TS <- function(active_contract_tickers, start, end, TS_positions, roll_type, roll_days,
                           roll_months, roll_adjustment, verbose = TRUE, ...){

  utils::data(list = c("fields", "rolls"), package = "BBGsymbols", envir = environment())

  data <- lapply(active_contract_tickers, function(y){
    tickers <- sapply(TS_positions, function(x) futures_ticker(y, TS_position = x, roll_type, roll_days,
                                                               roll_months, roll_adjustment))
    data <- BBG_pull_historical_market(tickers,
                                       fields = dplyr::filter(fields, instrument == "futures", book == "market",
                                                              type == "term structure") %>% dplyr::select(symbol) %>%
                                         purrr::flatten_chr(),
                                       start, end, ...)

    data <- if (! is.data.frame(data)) {
      lapply(names(data), function(x) { if (nrow(data[[x]]) > 0L) { data[[x]]$ticker <- x; data[[x]] } else NULL }) %>%
        data.table::rbindlist(use.names = TRUE)
    } else { if(nrow(data) < 1L) NULL else { data$ticker <- tickers; data } }
    data$`active contract ticker` <- y
    if (verbose) done(y); data
  }) %>%
    data.table::rbindlist(use.names = TRUE)

  data %<>%
    tidyr::gather(field, value, -c(`active contract ticker`, ticker, date)) %>%
    dplyr::select(`active contract ticker`, ticker, field, date, value) %>%
    dplyr::arrange(`active contract ticker`, ticker, field, date) %>% dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01"))

  if (nrow(data) == 0L) warning("No term structure data found.")

  active_contract_tickers <- dplyr::distinct(data, ticker = `active contract ticker`)

  term_structure_tickers <- dplyr::distinct(data, `active contract ticker`, ticker) %>%
    dplyr::mutate(`TS position` = stringr::str_extract(ticker, pattern = "(?<=^.{0,10})\\d(?=\\s[A-Z]:)"),
                  `roll type symbol` = stringr::str_extract(ticker, pattern = "(?<= )[A-Z](?=:)"),
                  `roll days` = stringr::str_extract(ticker, pattern = "(?<=:)\\d{2}(?=_)") %>% as.numeric(),
                  `roll months` = stringr::str_extract(ticker, pattern = "(?<=_)\\d(?=_)") %>% as.numeric(),
                  `roll adjustment symbol` = stringr::str_extract(ticker, pattern = "(?<=_)[A-Z](?= )"))

  fields <- dplyr::filter(fields, instrument == "futures", book == "market", type == "term structure")

  fields <- dplyr::distinct(data, `active contract ticker`, ticker, field) %>%
    dplyr::left_join(dplyr::select(fields, instrument, book, type, symbol), by = c("field" = "symbol")) %>%
    dplyr::select(`active contract ticker`, ticker, instrument, book, type, symbol = field) %>%
    dplyr::arrange(`active contract ticker`, ticker, instrument, book, type)

  methods::new("FuturesTS", active_contract_tickers = data.table::as.data.table(dplyr::arrange(active_contract_tickers)),
               term_structure_tickers = data.table::as.data.table(term_structure_tickers),
               fields = data.table::as.data.table(fields),
               data = data.table::as.data.table(dplyr::select(data, ticker, field, date, value)), call = match.call())
}


#### aggregate ####

#' Futures aggregate historical data from Bloomberg
#'
#'
#' @description Provided with a set of futures active contract Bloomberg tickers and a time period,
#'   queries Bloomberg for the corresponding futures historical aggregate data. For each individual field,
#'   futures aggregate data represents the aggregation of the corresponding field values over all the
#'   corresponding term structure contracts.
#'
#'
#' @param active_contract_tickers a chatacter vector. Specifies the futures active contract
#'   Bloomberg tickers to query data for.
#'
#' @param start a scalar character vector. Specifies the starting date for the query in the
#'   following format: 'yyyy-mm-dd'.
#'
#' @param end a scalar character vector. Specifies the end date for the query in the following
#'   format: 'yyyy-mm-dd'.
#'
#' @param verbose a logical scalar vector. Should progression messages be printed? Defaults to TRUE.
#'
#' @param ... optional parameters to pass to the \link[Rblpapi]{bdh} function from the
#' \href{http://dirk.eddelbuettel.com/code/rblpapi.html}{\pkg{Rblpapi}} package used
#'   for the query (\code{options} parameter).
#'
#'
#' @return An S4 object of class \linkS4class{FuturesAggregate}.
#'
#'
#' @seealso
#'   \itemize{
#'
#'     \item{"GFUT <GO>" on a Bloomberg terminal.}
#'
#'     \item{The \link[BBGsymbols]{rolls} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite) for details
#'     regarding the \code{roll_type} & \code{roll_adjustment} parameters.}
#'     \item{The \link[BBGsymbols]{fields} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'     for details on the Bloomnerg fields used here.}
#'
#'     \item{Helper datasets in the
#'     \href{https://github.com/bautheac/fewISOs/}{\pkg{fewISOs}} package
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite).}
#'
#'   }
#'
#'
#' @examples \dontrun{
#'     BBG_futures_market(type = "aggregate",
#'       active_contract_tickers = c("W A Comdty", "KWA Comdty"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'   }
#'
#'
#' @import BBGsymbols
#' @importFrom magrittr "%<>%"
BBG_futures_aggregate <- function(active_contract_tickers, start, end, verbose, ...){

  utils::data(list = c("fields"), package = "BBGsymbols", envir = environment())

  data <- lapply(active_contract_tickers, function(x) {
    data <- BBG_pull_historical_market(x, fields = dplyr::filter(fields, instrument == "futures",
                                                                 book == "market", type == "aggregate") %>%
                                         dplyr::select(symbol) %>% purrr::flatten_chr(), start, end, ...) %>%
      dplyr::mutate(`active contract ticker` = x)
    if (verbose) done(x); data
  }) %>%
    data.table::rbindlist(use.names = TRUE)

  data %<>%
    tidyr::gather(field, value, -c(`active contract ticker`, date)) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::arrange(`active contract ticker`, field, date) %>%
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01")) %>%
    dplyr::select(ticker = `active contract ticker`, field, date, value)

  active_contract_tickers <- dplyr::distinct(data, ticker)

  fields <- dplyr::filter(fields, instrument == "futures", book == "market", type == "aggregate")

  fields <- dplyr::distinct(data, ticker, field) %>%
    dplyr::left_join(dplyr::select(fields, instrument, book, type, symbol),
                     by = c("field" = "symbol")) %>%
    dplyr::select(ticker, instrument, book, type, symbol = field) %>%
    dplyr::arrange(ticker, instrument, book, type)

  if (nrow(data) == 0L) warning("No aggregate data found.")

  methods::new("FuturesAggregate",
               active_contract_tickers = data.table::as.data.table(dplyr::arrange(active_contract_tickers)),
               fields = data.table::as.data.table(fields), data = data.table::as.data.table(data),
               call = match.call())
}


### CFTC ####

#' Futures CFTC reports historical data from Bloomberg
#'
#'
#' @description Provided with a set of futures active contract Bloomberg tickers and a
#'   time period, queries Bloomberg for the corresponding futures CFTC reports
#'   historical data.
#'
#'
#' @param active_contract_tickers a chatacter vector. Specifies the futures active
#'   contract Bloomberg tickers to query data for.
#'
#' @param start a scalar character vector. Specifies the starting date for the query
#'   in the following format: 'yyyy-mm-dd'.
#'
#' @param end a scalar character vector. Specifies the end date for the query in the
#'   following format: 'yyyy-mm-dd'.
#'
#' @param verbose a logical scalar vector. Should progression messages be printed?
#'   Defaults to TRUE.
#'
#' @param ... optional parameters to pass to the \link[Rblpapi]{bdh} function from the
#' \href{http://dirk.eddelbuettel.com/code/rblpapi.html}{\pkg{Rblpapi}} package used
#'   for the query (\code{options} parameter).
#'
#'
#' @return An S4 object of class \linkS4class{FuturesCFTC}.
#'
#'
#' @seealso
#'
#'   \itemize{
#'
#'     \item{"GFUT <GO>" on a Bloomberg terminal.}
#'
#'     \item{The \link[BBGsymbols]{tickers_CFTC} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite)
#'     for details on the Bloomnerg position tickers used here.}
#'
#'     \item{Helper datasets in the
#'     \href{https://github.com/bautheac/fewISOs/}{\pkg{fewISOs}} package
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite).}
#'
#'   }
#'
#'
#' @examples \dontrun{
#'     BBG_futures_CFTC(active_contract_tickers = c("W A Comdty", "KWA Comdty"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'   }
#'
#'
#' @import BBGsymbols
#' @importFrom magrittr "%<>%"
#'
#'
#' @export
BBG_futures_CFTC <- function(active_contract_tickers, start, end, verbose = TRUE, ...){

  utils::data(list = c("fields", "tickers_cftc"), package = "BBGsymbols", envir = environment())

  if (! is.character(active_contract_tickers))
    stop("The parameter 'active_contract_tickers' must be supplied as a character vector of
         futures active contract Bloomberg tickers")

  if (! all(stringr::str_detect(c(start, end), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))
    stop("The parameters 'start' and 'end' must be supplied as scalar character vectors of
         dates (yyyy-mm-dd)")

  if (! rlang::is_scalar_logical(verbose))
    stop("The parameter 'verbose' must be supplied as a scalar logical vector")

  data <- lapply(active_contract_tickers, function(x) {
    tickers <- dplyr::filter(tickers_cftc, `active contract ticker` == x) %>%
      dplyr::select(ticker) %>% purrr::flatten_chr()
    if (NROW(tickers) == 0L) stop(paste0("No CFTC data for ", x, "."))
    data <- BBG_pull_historical_market(tickers, fields = "PX_LAST", start, end, ...)

    data <- lapply(names(data), function(y) {
      if(nrow(data[[y]]) == 0L) NULL else dplyr::mutate(data[[y]], ticker = y)
    }) %>%
      data.table::rbindlist(use.names = TRUE)

    data %<>%
      dplyr::mutate(`active contract ticker` = x) %>%
      dplyr::left_join(dplyr::select(tickers_cftc, format, underlying, `unit` = unit,
                                     participant, position, ticker), by = "ticker")
    if (verbose) done(x); data
  }) %>%
    data.table::rbindlist(use.names = TRUE) %>%
    dplyr::select(`active contract ticker`, ticker, format, underlying, `unit`,
                  participant, position, date, value = PX_LAST) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::mutate(field = "PX_LAST", date = as.Date(date, origin = "1970-01-01"))

  if (nrow(data) == 0L) warning("No CFTC data found.")

  active_contract_tickers <- dplyr::distinct(data, `active contract ticker`)

  tickers <- dplyr::distinct(data, `active contract ticker`, ticker)

  methods::new("FuturesCFTC",
               active_contract_tickers = data.table::as.data.table(dplyr::arrange(active_contract_tickers)),
               cftc_tickers = data.table::as.data.table(tickers),
               data = data.table::as.data.table(dplyr::select(data, ticker, field, date, value)),
               call = match.call())
}



### info ####

#' Futures series qualitative information from Bloomberg.
#'
#'
#' @description Provided with a set of futures active contract Bloomberg tickers
#'   retrieves qualitative information on the corresponding futures series.
#'
#'
#' @param active_contract_tickers a chatacter vector. Specifies the futures active
#'   contract Bloomberg tickers to query data for.
#'
#' @param ... optional parameters to pass to the \link[Rblpapi]{bdp} function from the
#' \href{http://dirk.eddelbuettel.com/code/rblpapi.html}{\pkg{Rblpapi}} package used
#'   for the query (\code{options} parameter).
#'
#'
#' @return An S4 object of class \linkS4class{FuturesInfo}.
#'
#'
#' @seealso
#'
#'   \itemize{
#'
#'     \item{The \link[BBGsymbols]{fields} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'      (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite) for
#'      details on the Bloomnerg fields used here.}
#'
#'     \item{Helper datasets in the
#'     \href{https://github.com/bautheac/fewISOs/}{\pkg{fewISOs}} and
#'     \href{https://github.com/bautheac/GICS/}{\pkg{GICS}} packages
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite).}
#'
#'   }
#'
#'
#' @examples \dontrun{
#'
#'     BBG_futures_info(active_contract_tickers = c("W A Comdty", "KWA Comdty"))
#'
#'   }
#'
#'
#' @export
BBG_futures_info <- function(active_contract_tickers, ...){

  utils::data(list = c("fields"), package = "BBGsymbols", envir = environment())

  fields <- dplyr::filter(fields, instrument == "futures", book == "info")

  con <- tryCatch({
    Rblpapi::blpConnect()
  }, error = function(e)
    stop("Unable to connect Bloomberg. Please open a Bloomberg session on this terminal",
         call. = FALSE))

  query <- Rblpapi::bdp(securities = active_contract_tickers,
                        fields = dplyr::select(fields, symbol) %>% purrr::flatten_chr(), con = con) %>%
    dplyr::mutate(ticker = row.names(.)) %>% dplyr::mutate_all(dplyr::funs(as.character)) %>%
    tidyr::gather(field, value, -ticker) %>% dplyr::mutate(field = forcats::as_factor(field)) %>%
    dplyr::arrange(ticker, field) %>% dplyr::mutate(field = as.character(field))

  fields <- dplyr::left_join(dplyr::distinct(query, ticker, symbol = field),
                             dplyr::select(fields, instrument, book, symbol),
                             by = "symbol") %>%
    dplyr::select(ticker, instrument, book, symbol)

  Rblpapi::blpDisconnect(con)

  methods::new("FuturesInfo", info = tibble::as.tibble(query),
               fields = data.table::as.data.table(fields), call = match.call())
}



## equity ####

### market ####

#' Equity historical market data from Bloomberg
#'
#'
#' @description Provided with a set of equity Bloomberg tickers and a time period,
#'   queries Bloomberg for the corresponding equity historical market data.
#'
#'
#' @param tickers a chatacter vector. Specifies the equity Bloomberg tickers to query
#'   data for.
#'
#' @param start a scalar character vector. Specifies the starting date for the query
#'   in the following format: 'yyyy-mm-dd'.
#'
#' @param end a scalar character vector. Specifies the end date for the query in the
#'   following format: 'yyyy-mm-dd'.
#'
#' @param verbose a logical scalar vector. Should progression messages be printed?
#'   Defaults to TRUE.
#'
#' @param ... optional parameters to pass to the \link[Rblpapi]{bdh} function from the
#' \href{http://dirk.eddelbuettel.com/code/rblpapi.html}{\pkg{Rblpapi}} package used
#'   for the query (\code{options} parameter).
#'
#'
#' @return An S4 object of class \linkS4class{EquityMarket}.
#'
#'
#' @seealso
#'
#'   \itemize{
#'
#'     \item{The \link[BBGsymbols]{fields} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'      (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite)
#'      for details on the Bloomnerg fields used here.}
#'
#'     \item{Helper datasets in the
#'     \href{https://github.com/bautheac/fewISOs/}{\pkg{fewISOs}} and
#'     \href{https://github.com/bautheac/GICS/}{\pkg{GICS}} packages
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite).}
#'
#'   }
#'
#'
#' @examples \dontrun{
#'
#'     BBG_equity_market(tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'   }
#'
#'
#' @import BBGsymbols
#' @importFrom magrittr "%<>%"
#'
#'
#' @export
BBG_equity_market <- function(tickers, start, end, verbose = TRUE, ...){

  utils::data(list = c("fields"), package = "BBGsymbols", envir = environment())

  if (! is.character(tickers))
    stop("The parameter 'tickers' must be supplied as a character vector of
         equity Bloomberg tickers")

  if (! all(stringr::str_detect(c(start, end), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))
    stop("The parameters 'start' and 'end' must be supplied as scalar character
         vectors of dates (yyyy-mm-dd)")

  if (! rlang::is_scalar_logical(verbose))
    stop("The parameter 'verbose' must be supplied as a scalar logical vector")

  data <- lapply(tickers, function(x) {
    data <- BBG_pull_historical_market(x, fields = dplyr::filter(fields, instrument == "equity", book == "market") %>%
                                         dplyr::select(symbol) %>% purrr::flatten_chr(),
                                       start, end, ...) %>%
      dplyr::mutate(ticker = x)
    if (verbose) done(x); data
  }) %>%
    data.table::rbindlist(use.names = TRUE)

  data %<>%
    tidyr::gather(field, value, -c(ticker, date)) %>% dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::select(ticker, field, date, value) %>%
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01")) %>% dplyr::arrange(ticker, field, date)

  if (nrow(data) == 0L) warning("No market data found.")

  tickers <- dplyr::distinct(data, ticker)

  fields <- dplyr::filter(fields, instrument == "equity", book == "market")

  fields <- dplyr::distinct(data, ticker, field) %>%
    dplyr::left_join(dplyr::select(fields, instrument, book, symbol), by = c("field" = "symbol")) %>%
    dplyr::select(ticker, instrument, book, symbol = field) %>% dplyr::arrange(ticker, instrument, book)

  methods::new("EquityMarket", tickers = data.table::as.data.table(dplyr::arrange(tickers)),
               fields = data.table::as.data.table(fields), data = data.table::as.data.table(data), call = match.call())
}


### book ####

#' Equity historical book data from Bloomberg
#'
#'
#' @description Retrieves equity historical book data from Bloomberg. Books include
#'   'key stats', 'income statement', 'balance sheet', 'cash flow statement' and 'ratios'.
#'
#'
#' @param book a scalar chatacter vector, 'key stats', 'income statement', 'balance sheet',
#'   'cash flow statement' or 'ratios'.
#'
#' @param tickers a chatacter vector. Specifies the Bloomberg equity tickers to query data for.
#'
#' @param start a scalar character vector. Specifies the starting date for the query in the
#'   following format: 'yyyy-mm-dd'.
#'
#' @param end a scalar character vector. Specifies the end date for the query in the
#'   following format: 'yyyy-mm-dd'.
#'
#' @param verbose a logical scalar vector. Should progression messages be printed?
#'   Defaults to TRUE.
#'
#' @param ... optional parameters to pass to the \link[Rblpapi]{bdh} function from the
#' \href{http://dirk.eddelbuettel.com/code/rblpapi.html}{\pkg{Rblpapi}} package used
#'   for the query (\code{options} parameter).
#'
#'
#' @return An S4 object of class \linkS4class{EquityKS} (\code{book = 'key stats'}),
#'   \linkS4class{EquityIS} (\code{book = 'income statement'}), \\linkS4class{EquityBS}
#'   (\code{book = 'balance sheet'}), \linkS4class{EquityCF}
#'   (\code{book = 'cash flow statement'}), \linkS4class{EquityRatios} (\code{book = 'ratios'}).
#'
#'
#' @seealso
#'
#'   \itemize{
#'
#'     \item{The \link[BBGsymbols]{fields} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'      (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite) for
#'      details on the Bloomnerg fields used here.}
#'
#'     \item{Helper datasets in the
#'     \href{https://github.com/bautheac/fewISOs/}{\pkg{fewISOs}} and
#'     \href{https://github.com/bautheac/GICS/}{\pkg{GICS}} packages
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite).}
#'
#'   }
#'
#'
#' @examples \dontrun{
#'
#'     BBG_equity_books(book = 'key stats',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'     BBG_equity_books(book = 'income statement',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'     BBG_equity_books(book = 'balance sheet',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'     BBG_equity_books(book = 'cash flow statement',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'     BBG_equity_books(book = 'ratios',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'   }
#'
#'
#' @import BBGsymbols
#'
#'
#' @export
BBG_equity_book <- function(book, tickers, start, end, verbose = TRUE, ...){


  utils::data(list = c("fields"), package = "BBGsymbols", envir = environment())


  if (! all(rlang::is_scalar_character(book),
            book %in% (dplyr::filter(fields, instrument == "equity",
                                     ! book %in% c("info", "market")) %>%
                       dplyr::distinct(book) %>% purrr::flatten_chr())))
    stop("The parameter book' must be supplied as a scalar character vector; one of '",
         paste(dplyr::filter(fields, instrument == "equity", book != "market") %>%
                 dplyr::distinct(book) %>% purrr::flatten_chr(), collapse = "', '"), "'")

  if (! is.character(tickers))
    stop("The parameter 'tickers' must be supplied as a character vector of equity
         Bloomberg tickers")

  if (! all(stringr::str_detect(c(start, end), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))
    stop("The parameters 'start' and 'end' must be supplied as scalar character
         vectors of dates (yyyy-mm-dd)")

  if (! rlang::is_scalar_logical(verbose))
    stop("The parameter 'verbose' must be supplied as a scalar logical vector (TRUE or FALSE)")

  if (! "periodicitySelection" %in% names(list(...)))
    utils::modifyList(list(...), list(periodicitySelection = "MONTHLY"))


  symbols <- dplyr::filter(fields, instrument == "equity", book == !! book)


  data <- lapply(unique(symbols$symbol), function(x) {
    data <- BBG_pull_historical_books(tickers = tickers, field = x, start, end, ...)
    if (verbose) done(x); data
  }) %>%
    data.table::rbindlist(use.names = TRUE) %>% dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::arrange(ticker, field, date) %>%
    dplyr::select(ticker, field, date, value)

  if (nrow(data) == 0L) warning(paste("No", book, "data found.", sep = " "))

  tickers <- dplyr::distinct(data, ticker)

  fields <- dplyr::filter(fields, instrument == "equity", book == !! book)

  fields <- dplyr::distinct(data, ticker, field) %>%
    dplyr::left_join(dplyr::select(fields, instrument, book, type, section, subsection, symbol),
                     by = c("field" = "symbol")) %>%
    dplyr::select(ticker, instrument, book, type, section, subsection, symbol = field) %>%
    dplyr::arrange(ticker, instrument, book, type, section, subsection)

  methods::new(dplyr::case_when(book == "key stats" ~ "EquityKS", book == "balance sheet" ~ "EquityBS",
                                book == "cash flow statement" ~ "EquityCF",
                                book == "income statement" ~ "EquityIS", book == "ratios" ~ "EquityRatios"),
               tickers = data.table::as.data.table(dplyr::arrange(tickers)), fields = data.table::as.data.table(fields),
               data = data.table::as.data.table(data), call = match.call())
}


### info ####

#' Equity qualitative information from Bloomberg.
#'
#'
#' @description Provided with a set of equity Bloomberg tickers retrieves qualitative
#'   information on the corresponding corporations.
#'
#'
#' @param tickers a chatacter vector. Specifies the equity Bloomberg tickers to query
#'   data for.
#'
#' @param ... optional parameters to pass to the \link[Rblpapi]{bdp} function from the
#' \href{http://dirk.eddelbuettel.com/code/rblpapi.html}{\pkg{Rblpapi}} package used
#'   for the query (\code{options} parameter).
#'
#'
#' @return An S4 object of class \linkS4class{EquityInfo}.
#'
#'
#' @seealso
#'
#'   \itemize{
#'
#'     \item{The \link[BBGsymbols]{fields} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'      (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite) for details
#'      on the Bloomnerg fields used here.}
#'
#'     \item{Helper datasets in the
#'     \href{https://github.com/bautheac/fewISOs/}{\pkg{fewISOs}} and
#'     \href{https://github.com/bautheac/GICS/}{\pkg{GICS}} packages
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite).}
#'
#'
#'   }
#'
#'
#' @examples \dontrun{
#'     BBG_equity_info(tickers = c("BP/ LN Equity", "WEIR LN Equity"))
#'   }
#'
#'
#' @import BBGsymbols
#' @importFrom magrittr "%<>%"
#'
#'
#' @export
BBG_equity_info <- function(tickers, ...){

  utils::data(list = c("fields"), package = "BBGsymbols", envir = environment())

  fields <- dplyr::filter(fields, instrument == "equity", book == "info")

  con <- tryCatch({
    Rblpapi::blpConnect()
  }, error = function(e)
    stop("Unable to connect Bloomberg. Please open a Bloomberg session on this terminal",
         call. = FALSE))

  query <- Rblpapi::bdp(securities = tickers,
                        fields = dplyr::select(fields, symbol) %>% purrr::flatten_chr(),
                        con = con) %>%
    dplyr::mutate(ticker = row.names(.)) %>%
    dplyr::mutate_all(dplyr::funs(as.character)) %>%
    tidyr::gather(field, value, -ticker) %>%
    dplyr::mutate(field = forcats::as_factor(field)) %>%
    dplyr::arrange(ticker, field) %>% dplyr::mutate(field = as.character(field))

  fields <- dplyr::left_join(dplyr::distinct(query, ticker, symbol = field),
                             dplyr::select(fields, instrument, book, symbol),
                             by = "symbol") %>%
    dplyr::select(ticker, instrument, book, symbol)

  Rblpapi::blpDisconnect(con)

  methods::new("EquityInfo", info = tibble::as.tibble(query),
               fields = data.table::as.data.table(fields), call = match.call())
}


## fund ####

### market ####

#' Fund historical market data from Bloomberg
#'
#'
#' @description Provided with a set of Bloomberg fund tickers and a time period,
#'   queries Bloomberg for the corresponding fund historical market data.
#'
#'
#' @param tickers a chatacter vector. Specifies the Bloomberg fund tickers to
#'   query data for.
#'
#' @param start a scalar character vector. Specifies the starting date for the
#'   query in the following format: 'yyyy-mm-dd'.
#'
#' @param end a scalar character vector. Specifies the end date for the query
#'   in the following format: 'yyyy-mm-dd'.
#'
#' @param verbose a logical scalar vector. Should progression messages be printed?
#'   Defaults to TRUE.
#'
#' @param ... optional parameters to pass to the \link[Rblpapi]{bdh} function from the
#' \href{http://dirk.eddelbuettel.com/code/rblpapi.html}{\pkg{Rblpapi}} package used
#'   for the query (\code{options} parameter).
#'
#'
#' @return An S4 object of class \linkS4class{FundMarket}.
#'
#'
#' @seealso
#'
#'   \itemize{
#'
#'     \item{The \link[BBGsymbols]{fields} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'      (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite) for details
#'      on the Bloomnerg fields used here.}
#'
#'     \item{Helper datasets in the
#'     \href{https://github.com/bautheac/fewISOs/}{\pkg{fewISOs}} and
#'     \href{https://github.com/bautheac/GICS/}{\pkg{GICS}} packages
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite).}
#'
#'   }
#'
#'
#' @examples \dontrun{
#'     BBG_fund_market(tickers = c("SPY US Equity", "IVV US Equity"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'   }
#'
#'
#' @import BBGsymbols
#' @importFrom magrittr "%<>%"
#'
#'
#' @export
BBG_fund_market <- function(tickers, start, end, verbose = TRUE, ...){

  utils::data(list = c("fields"), package = "BBGsymbols", envir = environment())

  if (! is.character(tickers))
    stop("The parameter 'tickers' must be supplied as a character vector of
         equity Bloomberg tickers")

  if (! all(stringr::str_detect(c(start, end), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))
    stop("The parameters 'start' and 'end' must be supplied as scalar character
         vectors of dates (yyyy-mm-dd)")

  if (! rlang::is_scalar_logical(verbose))
    stop("The parameter 'verbose' must be supplied as a scalar logical vector")

  data <- lapply(tickers, function(x) {
    data <- BBG_pull_historical_market(x,
                                       fields = dplyr::filter(fields,
                                                              instrument == "fund",
                                                              book == "market") %>%
                                         dplyr::select(symbol) %>% purrr::flatten_chr(),
                                       start, end, ...) %>% dplyr::mutate(ticker = x)
    if (verbose) done(x); data
  }) %>%
    data.table::rbindlist(use.names = TRUE)

  data %<>%
    tidyr::gather(field, value, -c(ticker, date)) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::select(ticker, field, date, value) %>%
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01")) %>%
    dplyr::arrange(ticker, field, date)

  if (nrow(data) == 0L) warning("No market data found")

  tickers <- dplyr::distinct(data, ticker)

  fields <- dplyr::filter(fields, instrument == "fund", book == "market")

  fields <- dplyr::distinct(data, ticker, field) %>%
    dplyr::left_join(dplyr::select(fields, instrument, book, symbol),
                     by = c("field" = "symbol")) %>%
    dplyr::select(ticker, instrument, book, symbol = field) %>%
    dplyr::arrange(ticker, instrument, book)

  methods::new("FundMarket", tickers = data.table::as.data.table(dplyr::arrange(tickers)),
               fields = data.table::as.data.table(fields),
               data = data.table::as.data.table(data), call = match.call())
}


### info ####

#' fund qualitative information from Bloomberg.
#'
#'
#' @description Provided with a set of fund Bloomberg tickers retrieves qualitative
#'   information on the corresponding fund(s).
#'
#'
#' @param tickers a chatacter vector. Specifies the futures active contract Bloomberg
#'   tickers to query data for.
#'
#' @param ... optional parameters to pass to the \link[Rblpapi]{bdp} function from the
#' \href{http://dirk.eddelbuettel.com/code/rblpapi.html}{\pkg{Rblpapi}} package used
#'   for the query (\code{options} parameter).
#'
#'
#' @return An S4 object of class \linkS4class{FundInfo}.
#'
#'
#' @seealso
#'
#'   \itemize{
#'
#'     \item{The \link[BBGsymbols]{fields} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'      (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite) for details
#'      on the Bloomnerg fields used here.}
#'
#'     \item{Helper datasets in the
#'     \href{https://github.com/bautheac/fewISOs/}{\pkg{fewISOs}} and
#'     \href{https://github.com/bautheac/GICS/}{\pkg{GICS}} packages
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite).}
#'
#'
#'   }
#'
#'
#' @examples \dontrun{
#'     BBG_fund_info(tickers = c("SPY US Equity", "IVV US Equity"))
#'   }
#'
#'
#' @importFrom magrittr "%<>%"
#'
#'
#' @export
BBG_fund_info <- function(tickers, ...){


  utils::data(list = c("fields"), package = "BBGsymbols", envir = environment())


  fields <- dplyr::filter(fields, instrument == "fund", book == "info")


  con <- tryCatch({
    Rblpapi::blpConnect()
  }, error = function(e)
    stop("Unable to connect Bloomberg. Please open a Bloomberg session on this terminal",
         call. = FALSE))


  query <- Rblpapi::bdp(securities = tickers,
                        fields = dplyr::select(fields, symbol) %>% purrr::flatten_chr(),
                        con = con) %>%
    dplyr::mutate(ticker = row.names(.)) %>% dplyr::mutate_all(dplyr::funs(as.character)) %>%
    tidyr::gather(field, value, -ticker) %>% dplyr::mutate(field = forcats::as_factor(field)) %>%
    dplyr::arrange(ticker, field) %>% dplyr::mutate(field = as.character(field))


  fields <- dplyr::left_join(dplyr::distinct(query, ticker, symbol = field),
                             dplyr::select(fields, instrument, book, symbol),
                             by = "symbol") %>% dplyr::select(ticker, instrument, book, symbol)


  Rblpapi::blpDisconnect(con)


  methods::new("FundInfo", info = tibble::as.tibble(query),
               fields = data.table::as.data.table(fields), call = match.call())
}


# storethat ####

## futures ####

### market ####

#### global ####

#' Futures historical market data from a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database
#'
#'
#' @description Provided with a set of Bloomberg futures active contract tickers,
#'   term structure positions, roll parameters and a time period, retrieves the corresponding
#'   futures market historical data previously stored in a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
#'
#'
#' @param file a scalar chatacter vector. Specifies the target
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database file.
#'
#' @param type a scalar character vector, 'term sructure' or 'aggregate'. 'term structure'
#'   returns individual futures chain data for a selected portion of the term structure
#'   (specify desired positions in \code{TS_positions}) while 'aggregate' returns aggregated data
#'   over the whole term structure for the corresponding names (\code{active_contract_tickers}).
#'
#' @param active_contract_tickers a chatacter vector. Specifies the futures active contract
#'   Bloomberg tickers to query data for.
#'
#' @param start a scalar character vector. Specifies the starting date for the query in the
#'   following format: 'yyyy-mm-dd'.
#'
#' @param end a scalar character vector. Specifies the end date for the query in the
#'   following format: 'yyyy-mm-dd'.
#'
#' @param TS_positions an integer vector. Specifies the term structure positions to
#'   query data for.
#'
#' @param roll_type a scalar chatacter vector. Specifies roll type to use for term structure
#'   ticker construction. Must be one of 'A', 'B', 'D', 'F', 'N', 'O' or 'R'.
#'
#' @param roll_days a scalar integer vector. Specifies the day the roll should be done.
#'   Refers to the day of the month (\code{roll_type} = 'F') or the number of days before a
#'   reference date (\code{roll_type} = 'D', \code{roll_type} = 'N', \code{roll_type} = 'O',
#'   \code{roll_type} = 'R'). Works in tandem with \code{roll_months} below.
#'
#' @param roll_months a scalar integer vector. Specifies the month the roll should be done.
#'   Refers to the number of months before a reference date (\code{roll_type} = 'D',
#'   \code{roll_type} = 'N', \code{roll_type} = 'O', \code{roll_type} = 'R'). Works in tandem
#'   with \code{roll_days} above.
#'
#' @param roll_adjustment a scalar chatacter vector. Specifies roll adjustment method to use for
#'   term structure ticker construction. Must be one of 'D', 'N', 'R', or 'W'.
#'
#' @param verbose a logical scalar vector. Should progression messages be printed? Defaults to TRUE.
#'
#'
#' @return An S4 object of class \linkS4class{FuturesTS} (\code{type = 'term structure'}) or
#'   \linkS4class{FuturesAggregate} (\code{type = 'aggregate'}).
#'
#'
#' @seealso
#'
#'   \itemize{
#'
#'     \item{"GFUT <GO>" on a Bloomberg terminal.}
#'
#'     \item{The \link[BBGsymbols]{rolls} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite) for details
#'     regarding the \code{roll_type} & \code{roll_adjustment} parameters.}
#'
#'     \item{The \link[BBGsymbols]{fields} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'     for details on the Bloomnerg fields used here.}
#'
#'     \item{Helper datasets in the
#'     \href{https://github.com/bautheac/fewISOs/}{\pkg{fewISOs}} package
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite).}
#'
#'     \item{The \href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite,
#'     in particular the \href{https://github.com/bautheac/storethat/}{\pkg{storethat}}
#'     package.}
#'
#'   }
#'
#' @examples \dontrun{
#'
#'     storethat_futures_market(type = 'term structure',
#'       active_contract_tickers = c("W A Comdty", "KWA Comdty"),
#'       start = "2000-01-01", end = as.character(Sys.Date()),
#'       TS_positions = 1L:5L, roll_type = "A", roll_days = 0L,
#'       roll_months = 0L, roll_adjustment = "N")
#'
#'     storethat_futures_market(type = "aggregate",
#'       active_contract_tickers = c("W A Comdty", "KWA Comdty"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'   }
#'
#'
#' @export
storethat_futures_market <- function(file = NULL, type, active_contract_tickers,
                                     start, end, TS_positions = NULL, roll_type = NULL,
                                     roll_days = NULL, roll_months = NULL,
                                     roll_adjustment = NULL, verbose = TRUE){


  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file),
              stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite
           database file (ie. ~/storethat.sqlite)")


  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)
  fields <- RSQLite::dbReadTable(con, "support_fields")

  tickers <- "SELECT ticker FROM tickers_futures;"
  tickers <- RSQLite::dbGetQuery(con, tickers)

  roll_types <- RSQLite::dbReadTable(con, "support_futures_roll_types")$symbol

  roll_adjustments <- RSQLite::dbReadTable(con, "support_futures_roll_adjustments")$symbol

  RSQLite::dbDisconnect(con)


  if (! all(rlang::is_scalar_character(type),
            type %in% unique(dplyr::filter(fields, instrument == "futures",
                                           book == "market")$type)))
    stop("The parameter 'type' must be supplied as a scalar character vector; one of '",
         paste(unique(dplyr::filter(fields, instrument == "futures",
                                    book == "market")$type),
               collapse = "', '"), "'")

  if (! all(active_contract_tickers %in% tickers$ticker))
    stop("The parameter 'active_contract_tickers' must be supplied as a character
         vector; one or more of '",
         paste(tickers$ticker, collapse = "', '"), "'")

  if (! all(stringr::str_detect(c(start, end), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))
    stop("The parameters 'start' and 'end' must be supplied as scalar character
         vectors of dates (yyyy-mm-dd)")

  if (! is.null(TS_positions))
    if (! all(is.integer(TS_positions)))
      stop("The parameters 'TS_positions' must be supplied as a vector of integers")

  if (! all(is.null(roll_days), is.null(roll_months)))
    if (! all(rlang::is_scalar_integer(roll_days), rlang::is_scalar_integer(roll_months)))
      stop("The parameters 'roll_days' and 'roll_months' must be supplied as
           scalar integer vectors")

  if (! all(is.null(roll_type), is.null(roll_adjustment)))
    if (! all(rlang::is_scalar_character(roll_type),
              rlang::is_scalar_character(roll_adjustment),
              roll_type %in% roll_types, roll_adjustment %in% roll_adjustments))
      stop("The parameters 'roll_type' and 'roll_adjustment' must be supplied as
           scalar character vectors; one of '", paste(roll_types, collapse = "', '"),
           "' ('roll_type') or '", paste(roll_adjustments, collapse = "', '"),
           "' ('roll_adjustment')")

  if (! rlang::is_scalar_logical(verbose))
    stop("The parameter 'verbose' must be supplied as a scalar logical vector")


  switch(type,
         `term structure` = storethat_futures_TS(active_contract_tickers = active_contract_tickers,
                                                 start = start, end = end,
                                                 TS_positions = TS_positions,
                                                 roll_type = roll_type, roll_days = roll_days,
                                                 roll_months = roll_months,
                                                 roll_adjustment = roll_adjustment,
                                                 verbose = verbose, file = file),
         `aggregate` = storethat_futures_aggregate(active_contract_tickers = active_contract_tickers,
                                                   start = start, end = end, verbose = verbose,
                                                   file = file)
  )
}


#### term structure ####

#' Futures term structure historical data from a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}}
#'   SQLite database
#'
#'
#' @description Provided with a set of Bloomberg futures active contract tickers,
#' term structure positions, roll parameters and a time period,
#'   retrieves the corresponding term structure historical data previously stored
#'   in a \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite
#'   database.
#'
#'
#' @param file a scalar chatacter vector. Specifies the target
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}}
#'   SQLite database file.
#'
#' @param active_contract_tickers a chatacter vector. Specifies the futures active
#'   contract Bloomberg tickers to query data for.
#'
#' @param start a scalar character vector. Specifies the starting date for the query
#'   in the following format: 'yyyy-mm-dd'.
#'
#' @param end a scalar character vector. Specifies the end date for the query in the
#'   following format: 'yyyy-mm-dd'.
#'
#' @param TS_positions An integer vector. Specifies the term structure positions to
#'   query data for.
#'
#' @param roll_type a scalar chatacter vector. Specifies roll type to use for term
#'   structure ticker construction. Must be one of 'A', 'B', 'D', 'F', 'N', 'O' or 'R'.
#'
#' @param roll_days a scalar integer vector. Specifies the day the roll should be done.
#'   Refers to the day of the month (\code{roll_type} = 'F') or the number of days
#'   before a reference date (\code{roll_type} = 'D', \code{roll_type} = 'N',
#'   \code{roll_type} = 'O', \code{roll_type} = 'R'). Works in tandem with
#'   \code{roll_months} below.
#'
#' @param roll_months a scalar integer vector. Specifies the month the roll should be
#'   done. Refers to the number of months before a reference date (\code{roll_type} = 'D',
#'   \code{roll_type} = 'N', \code{roll_type} = 'O', \code{roll_type} = 'R'). Works in
#'   tandem with \code{roll_days} above.
#'
#' @param roll_adjustment a scalar chatacter vector. Specifies roll adjustment method
#'   to use for term structure ticker construction. Must be one of 'D', 'N', 'R', or 'W'.
#'
#' @param verbose a logical scalar vector. Should progression messages be printed?
#'   Defaults to TRUE.
#'
#'
#' @return An S4 object of class \linkS4class{FuturesTS}.
#'
#'
#' @seealso
#'
#'   \itemize{
#'
#'     \item{"GFUT <GO>" on a Bloomberg terminal.}
#'
#'     \item{The \link[BBGsymbols]{rolls} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite) for details
#'     regarding the \code{roll_type} & \code{roll_adjustment} parameters.}
#'
#'     \item{The \link[BBGsymbols]{fields} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'     for details on the Bloomnerg fields used here.}
#'
#'     \item{Helper datasets in the
#'     \href{https://github.com/bautheac/fewISOs/}{\pkg{fewISOs}} package
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite).}
#'
#'     \item{The \href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite,
#'     in particular the \href{https://github.com/bautheac/storethat/}{\pkg{storethat}}
#'     package.}
#'
#'   }
#'
#' @examples \dontrun{
#'
#'     storethat_futures_market(type = 'term structure',
#'       active_contract_tickers = c("W A Comdty", "KWA Comdty"),
#'       start = "2000-01-01", end = as.character(Sys.Date()),
#'       TS_positions = 1L:5L, roll_type = "A", roll_days = 0L,
#'       roll_months = 0L, roll_adjustment = "N")
#'
#'   }
#'
#' @importFrom magrittr "%<>%"
storethat_futures_TS <- function(file, active_contract_tickers, start, end, TS_positions,
                                 roll_type, roll_days, roll_months, roll_adjustment, verbose){


  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)


  term_structure_tickers <- sapply(active_contract_tickers,
                                   function(y) sapply(TS_positions,
                                                      function(x) {

                                                        futures_ticker(y, TS_position = x,
                                                                       roll_type, roll_days,
                                                                       roll_months,
                                                                       roll_adjustment)
                                                      }))


  term_structure_tickers <- paste0("SELECT * FROM tickers_support_futures_ts WHERE ticker IN ('",
                                   paste(term_structure_tickers, collapse = "', '"), "');")
  term_structure_tickers <- RSQLite::dbGetQuery(con, term_structure_tickers)

  query <- paste0("SELECT id, ticker AS 'active contract ticker' FROM tickers_futures WHERE id IN (",
                  paste(term_structure_tickers$active_contract_ticker_id, collapse = ", "), ");")
  term_structure_tickers %<>% dplyr::left_join(RSQLite::dbGetQuery(con, query),
                                               by = c("active_contract_ticker_id" = "id")) %>%
    dplyr::select(id, active_contract_ticker_id, `active contract ticker`, ticker,
                  `TS position` = position, `roll type symbol` = roll_type_symbol,
                  `roll days` = roll_days, `roll months` = roll_months,
                  `roll adjustment symbol` = roll_adjustment_symbol)


  dates <- paste0("SELECT * FROM support_dates WHERE date >= '", start, "' AND date <= '", end, "';")
  dates <- RSQLite::dbGetQuery(con, dates)


  data <- lapply(unique(dates$period), function(z){

    query <- paste0("SELECT * FROM data_futures_ts_", z, " WHERE ticker_id IN (",
                    paste(term_structure_tickers$id, collapse = ", "),
                    ") AND date_id >= ", min(dates$id),
                    " AND date_id <= ", max(dates$id), ";")
    query <- RSQLite::dbGetQuery(con, query)

    if (verbose) done(paste0("Period ", data.table::first(dplyr::filter(dates, period == z)$date), "/",
                             data.table::last(dplyr::filter(dates, period == z)$date)))

    query

  }) %>% data.table::rbindlist()


  fields <- paste0("SELECT id, instrument, book, type, symbol FROM support_fields WHERE
                   instrument = 'futures' AND book = 'market' AND type = 'term structure';")
  fields <- RSQLite::dbGetQuery(con, fields)


  RSQLite::dbDisconnect(con)


  data %<>% dplyr::left_join(dplyr::select(term_structure_tickers, id, ticker),
                             by = c("ticker_id" = "id")) %>%
    dplyr::left_join(dplyr::select(dates, id, date), by = c("date_id" = "id")) %>%
    dplyr::left_join(fields, by = c("field_id" = "id")) %>%
    dplyr::select(ticker, field = symbol, date, value) %>% dplyr::arrange(ticker, field, date)


  fields <- dplyr::left_join(dplyr::distinct(data, ticker, symbol = field),
                             dplyr::select(fields, instrument, book, type, symbol),
                             by = "symbol") %>%
    dplyr::left_join(dplyr::select(term_structure_tickers, `active contract ticker`, ticker),
                     by = "ticker") %>%
    dplyr::select(`active contract ticker`, ticker, instrument, book, type, symbol)


  active_contract_tickers <- dplyr::distinct(term_structure_tickers, `active contract ticker`)


  methods::new("FuturesTS",
               active_contract_tickers = data.table::as.data.table(active_contract_tickers),
               term_structure_tickers = dplyr::select(term_structure_tickers,
                                                      -c(id, active_contract_ticker_id)) %>%
                 data.table::as.data.table(),
               fields = data.table::as.data.table(fields),
               data = data.table::as.data.table(data), call = match.call())
}


#### aggregate ####

#' Futures aggregate historical data from from a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database
#'
#'
#' @description Provided with a set of Bloomberg futures active contract tickers and
#'   a time period, retrieves the corresponding futures historical aggregate data previously
#'   stored in a \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite
#'   database. For each individual field, futures aggregate data represents the aggregation
#'   of the corresponding field values over all the corresponding term structure contracts.
#'
#'
#' @param file a scalar chatacter vector. Specifies the target
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database file.
#'
#' @param active_contract_tickers a chatacter vector. Specifies the futures active contract
#'   Bloomberg tickers to query data for.
#'
#' @param start a scalar character vector. Specifies the starting date for the query in the
#'   following format: 'yyyy-mm-dd'.
#'
#' @param end a scalar character vector. Specifies the end date for the query in the
#'   following format: 'yyyy-mm-dd'.
#'
#' @param verbose a logical scalar vector. Should progression messages be printed?
#'   Defaults to TRUE.
#'
#'
#' @return An S4 object of class \linkS4class{FuturesAggregate}.
#'
#'
#' @seealso
#'
#'   \itemize{
#'
#'     \item{"GFUT <GO>" on a Bloomberg terminal.}
#'
#'     \item{The \link[BBGsymbols]{rolls} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite) for details
#'     regarding the \code{roll_type} & \code{roll_adjustment} parameters.}
#'
#'     \item{The \link[BBGsymbols]{fields} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'     for details on the Bloomnerg fields used here.}
#'
#'     \item{Helper datasets in the
#'     \href{https://github.com/bautheac/fewISOs/}{\pkg{fewISOs}} package
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite).}
#'
#'     \item{The \href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite,
#'     in particular the \href{https://github.com/bautheac/storethat/}{\pkg{storethat}}
#'     package.}
#'
#'   }
#'
#'
#' @examples \dontrun{
#'
#'     storethat_futures_market(type = "aggregate",
#'       active_contract_tickers = c("W A Comdty", "KWA Comdty"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'   }
#'
#'
#' @importFrom magrittr "%<>%"
storethat_futures_aggregate <- function(file, active_contract_tickers, start, end, verbose){


  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)


  active_contract_tickers <- paste0("SELECT * FROM tickers_futures WHERE ticker IN ('",
                                    paste(active_contract_tickers, collapse = "', '"), "');")
  active_contract_tickers <- RSQLite::dbGetQuery(con, active_contract_tickers)


  dates <- paste0("SELECT * FROM support_dates WHERE date >= '", start, "' AND date <= '",
                  end, "';")
  dates <- RSQLite::dbGetQuery(con, dates)


  data <- lapply(unique(dates$period), function(z){

    query <- paste0("SELECT * FROM data_futures_aggregate_", z, " WHERE ticker_id IN (",
                    paste(active_contract_tickers$id, collapse = ", "),
                    ") AND date_id >= ", min(dates$id), " AND date_id <= ", max(dates$id), ";")
    query <- RSQLite::dbGetQuery(con, query)

    if (verbose) done(paste0("Period ", data.table::first(dplyr::filter(dates, period == z)$date),
                             "/", data.table::last(dplyr::filter(dates, period == z)$date)))

    query

  }) %>% data.table::rbindlist()


  fields <- paste0("SELECT id, instrument, book, type, symbol FROM support_fields WHERE id IN (",
                   paste(unique(data$field_id), collapse = ", "), ");")
  fields <- RSQLite::dbGetQuery(con, fields)


  RSQLite::dbDisconnect(con)


  data %<>% dplyr::left_join(dplyr::select(active_contract_tickers, id, ticker),
                             by = c("ticker_id" = "id")) %>%
    dplyr::left_join(dplyr::select(dates, id, date), by = c("date_id" = "id")) %>%
    dplyr::left_join(fields, by = c("field_id" = "id")) %>%
    dplyr::select(ticker, field = symbol, date, value) %>%
    dplyr::arrange(ticker, field, date)


  fields <- dplyr::left_join(dplyr::distinct(data, ticker, symbol = field),
                             dplyr::select(fields, instrument, book, type, symbol),
                             by = "symbol") %>%
    dplyr::left_join(dplyr::select(active_contract_tickers, ticker), by = "ticker") %>%
    dplyr::select(ticker, instrument, book, type, symbol)


  methods::new("FuturesAggregate", active_contract_tickers = data.table::as.data.table(tickers),
               fields = data.table::as.data.table(fields), data = data.table::as.data.table(data),
               call = match.call())
}


### CFTC ####

#' Futures CFTC position historical data from from a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database
#'
#'
#' @description Provided with a set of Bloomberg futures active contract tickers
#'   and a time period, retrieves the corresponding futures CFTC position historical
#'   data previously stored in a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
#'
#'
#' @param file a scalar chatacter vector. Specifies the target \pkg{storethat}
#'   SQLite database file.
#'
#' @param active_contract_tickers a chatacter vector. Specifies the futures active
#'   contract Bloomberg tickers to query data for.
#'
#' @param start a scalar character vector. Specifies the starting date for the query in
#'   the following format: 'yyyy-mm-dd'.
#'
#' @param end a scalar character vector. Specifies the end date for the query in the
#'   following format: 'yyyy-mm-dd'.
#'
#' @param verbose a logical scalar vector. Should progression messages be printed?
#'   Defaults to TRUE.
#'
#'
#' @return An S4 object of class \linkS4class{FuturesCFTC}.
#'
#'
#' @seealso
#'
#'   \itemize{
#'
#'     \item{"GFUT <GO>" on a Bloomberg terminal.}
#'
#'     \item{The \link[BBGsymbols]{tickers_CFTC} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite) for details
#'     on the Bloomnerg position tickers used here.}
#'
#'     \item{Helper datasets in the
#'     \href{https://github.com/bautheac/fewISOs/}{\pkg{fewISOs}} package
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite).}
#'
#'     \item{The \href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite,
#'     in particular the \href{https://github.com/bautheac/storethat/}{\pkg{storethat}}
#'     package.}
#'
#'   }
#'
#' @examples \dontrun{
#'
#'     storethat_futures_CFTC(active_contract_tickers = c("W A Comdty", "KWA Comdty"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'   }
#'
#'
#' @importFrom magrittr "%<>%"
#'
#'
#' @export
storethat_futures_CFTC <- function(file = NULL, active_contract_tickers, start, end, verbose = TRUE){


  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file),
              stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database
           file (ie. ~/storethat.sqlite)")


  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)


  tickers <- "SELECT id, ticker FROM tickers_futures;"
  tickers <- RSQLite::dbGetQuery(con, tickers)
  if (! all(active_contract_tickers %in% tickers$ticker))
    stop("The parameter 'active_contract_tickers' must be supplied as a character
         vector; one or more of '",
         paste(tickers$ticker, collapse = "', '"), "'")

  if (! all(stringr::str_detect(c(start, end), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))
    stop("The parameters 'start' and 'end' must be supplied as scalar character vectors
         of dates (yyyy-mm-dd)")

  if (! rlang::is_scalar_logical(verbose))
    stop("The parameter 'verbose' must be supplied as a scalar logical vector")


  cftc_tickers <- paste0("SELECT * FROM tickers_support_futures_cftc WHERE
                         active_contract_ticker_id IN (",
                         paste(tickers$id, collapse = ", "), ");")
  cftc_tickers <- RSQLite::dbGetQuery(con, cftc_tickers) %>%
    dplyr::left_join(dplyr::select(tickers, id, `active contract ticker` = ticker),
                     by = c("active_contract_ticker_id" = "id")) %>%
    dplyr::select(id, active_contract_ticker_id, `active contract ticker`, ticker)


  dates <- paste0("SELECT * FROM support_dates WHERE date >= '", start, "' AND date <= '",
                  end, "';")
  dates <- RSQLite::dbGetQuery(con, dates)


  data <- lapply(unique(dates$period), function(z){

    query <- paste0("SELECT * FROM data_futures_cftc_", z, " WHERE ticker_id IN (",
                    paste(cftc_tickers$id, collapse = ", "),
                    ") AND date_id >= ", min(dates$id), " AND date_id <= ",
                    max(dates$id), ";")
    query <- RSQLite::dbGetQuery(con, query)

    if (verbose) done(paste0("Period ",
                             data.table::first(dplyr::filter(dates, period == z)$date), "/",
                             data.table::last(dplyr::filter(dates, period == z)$date)))

    query

  }) %>% data.table::rbindlist() %>%
    dplyr::mutate(field = "PX_LAST") %>%
    dplyr::left_join(dplyr::select(cftc_tickers, id, `active contract ticker`, ticker),
                     by = c("ticker_id" = "id")) %>%
    dplyr::left_join(dplyr::select(dates, id, date), by = c("date_id" = "id")) %>%
    dplyr::select(`active contract ticker`, ticker, field, date, value) %>%
    dplyr::arrange(`active contract ticker`, ticker, field, date)


  RSQLite::dbDisconnect(con)


  methods::new("FuturesCFTC",
               active_contract_tickers = dplyr::distinct(cftc_tickers, ticker = `active contract ticker`) %>%
                 data.table::as.data.table(),
               cftc_tickers = dplyr::select(cftc_tickers, -c(id, active_contract_ticker_id)) %>%
                 data.table::as.data.table(),
               data = data.table::as.data.table(data), call = match.call())
}



### info ####

#' Futures series qualitative information from a
#' \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database
#'
#'
#' @description Provided with a set of futures active contract Bloomberg tickers retrieves
#'   qualitative information on the corresponding futures series previously stored in a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
#'
#'
#' @param file a scalar chatacter vector. Specifies the target \pkg{storethat} SQLite
#'   database file.
#'
#' @param active_contract_tickers a chatacter vector. Specifies the futures active contract
#'   Bloomberg tickers to query data for.
#'
#'
#' @return An S4 object of class \linkS4class{FuturesInfo}.
#'
#'
#' @seealso
#'
#'   \itemize{
#'
#'     \item{The \link[BBGsymbols]{fields} (\pkg{finRes} suite) dataset in the \pkg{BBGsymbols}
#'     package for details on the Bloomnerg fields used here.}
#'
#'     \item{Helper datasets in the \pkg{fewISOs} and \pkg{GICS} packages (\pkg{finRes} suite).}
#'
#'     \item{The \href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite,
#'     in particular the \href{https://github.com/bautheac/storethat/}{\pkg{storethat}}
#'     package.}
#'
#'   }
#'
#' @examples \dontrun{
#'     storethat_futures_info(active_contract_tickers = c("W A Comdty", "KWA Comdty"))
#'   }
#'
#' @export
storethat_futures_info <- function(file = NULL, active_contract_tickers){


  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file), stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database file (ie. ~/storethat.sqlite)")


  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)


  tickers <- "SELECT id, ticker FROM tickers_futures;"; tickers <- RSQLite::dbGetQuery(con, tickers)
  if (! all(active_contract_tickers %in% tickers$ticker))
    stop("The parameter 'active_contract_tickers' must be supplied as a character vector; one or more of '",
         paste(tickers$ticker, collapse = "', '"), "'")


  data <- paste0("SELECT ticker_id, field_id, value FROM data_futures_info WHERE ticker_id IN (",
                 paste(tickers$id, collapse = ", "), ");")
  data <- RSQLite::dbGetQuery(con, data)


  fields <- paste0("SELECT id, instrument, book, symbol FROM support_fields WHERE instrument = 'futures'
                   AND book = 'info' AND id IN (",
                   paste(unique(data$field_id), collapse = ", "), ");")
  fields <- RSQLite::dbGetQuery(con, fields)


  RSQLite::dbDisconnect(con)


  data <- dplyr::left_join(data, tickers, by = c("ticker_id" = "id")) %>%
    dplyr::left_join(fields, by = c("field_id" = "id")) %>%
    dplyr::select(ticker, symbol, value)


  fields <- dplyr::left_join(dplyr::distinct(data, ticker, symbol), fields, by = "symbol") %>%
    dplyr::select(ticker, instrument, book, symbol)


  methods::new("FuturesInfo", info = tibble::as.tibble(data),
               fields = data.table::as.data.table(fields), call = match.call())
}




## equity ####

### market ####

#' Equity historical market data from a \href{https://github.com/bautheac/storethat/}{\pkg{storethat}}
#'   SQLite database
#'
#'
#' @description Provided with a set of Bloomberg equity tickers and a time period, retrieves the
#'   corresponding equity historical market data previously stored in
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database SQLite database.
#'
#'
#' @param file a scalar chatacter vector. Specifies the target \pkg{storethat} SQLite database file.
#'
#' @param tickers a chatacter vector. Specifies the equity Bloomberg tickers to query data for.
#'
#' @param start a scalar character vector. Specifies the starting date for the query in the following
#'   format: 'yyyy-mm-dd'.
#'
#' @param end a scalar character vector. Specifies the end date for the query in the following
#'   format: 'yyyy-mm-dd'.
#'
#' @param verbose a logical scalar vector. Should progression messages be printed? Defaults to TRUE.
#'
#'
#' @return An S4 object of class \linkS4class{EquityMarket}.
#'
#'
#' @seealso
#'
#'   \itemize{
#'
#'     \item{The \link[BBGsymbols]{fields} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'      (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite) for details on the
#'      Bloomnerg fields used here.}
#'
#'     \item{Helper datasets in the \href{https://github.com/bautheac/fewISOs/}{\pkg{fewISOs}} and
#'     \href{https://github.com/bautheac/GICS/}{\pkg{GICS}} packages
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite).}
#'
#'     \item{The \href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite,
#'     in particular the \href{https://github.com/bautheac/storethat/}{\pkg{storethat}}
#'     package.}
#'
#'   }
#'
#'
#' @examples \dontrun{
#'
#'     storethat_equity_market(tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'   }
#'
#'
#' @importFrom magrittr "%<>%"
#'
#'
#' @export
storethat_equity_market <- function(file = NULL, tickers, start, end, verbose = TRUE){


  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file),
              stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database
           file (ie. ~/storethat.sqlite)")


  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)


  query <- "SELECT id, ticker FROM tickers_equity;"; query <- RSQLite::dbGetQuery(con, query)
  if (! all(tickers %in% query$ticker))
    stop("The parameter 'tickers' must be supplied as a character vector; one or more of '",
         paste(query$ticker, collapse = "', '"), "'")

  if (! all(stringr::str_detect(c(start, end), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))
    stop("The parameters 'start' and 'end' must be supplied as scalar character vectors of dates
         (yyyy-mm-dd)")

  if (! rlang::is_scalar_logical(verbose))
    stop("The parameter 'verbose' must be supplied as a scalar logical vector")


  tickers <- query
  dates <- paste0("SELECT * FROM support_dates WHERE date >= '", start, "' AND date <= '",
                  end, "';")
  dates <- RSQLite::dbGetQuery(con, dates)


  data <- lapply(unique(dates$period), function(z){

    query <- paste0("SELECT * FROM data_equity_market_", z, " WHERE ticker_id IN (",
                    paste(tickers$id, collapse = ", "),
                    ") AND date_id >= ", min(dates$id), " AND date_id <= ", max(dates$id), ";")
    query <- RSQLite::dbGetQuery(con, query)

    if (verbose) done(paste0("Period ", data.table::first(dplyr::filter(dates, period == z)$date), "/",
                             data.table::last(dplyr::filter(dates, period == z)$date)))

    query

  }) %>% data.table::rbindlist()


  fields <- paste0("SELECT id, instrument, book, symbol FROM support_fields WHERE instrument =
                   'equity' AND book = 'market' AND id IN (",
                   paste(unique(data$field_id), collapse = ", "), ");")
  fields <- RSQLite::dbGetQuery(con, fields)


  data %<>% dplyr::left_join(dplyr::select(tickers, id, ticker), by = c("ticker_id" = "id")) %>%
    dplyr::left_join(dplyr::select(dates, id, date), by = c("date_id" = "id")) %>%
    dplyr::left_join(fields, by = c("field_id" = "id")) %>%
    dplyr::select(ticker, field = symbol, date, value) %>%
    dplyr::arrange(ticker, field, date)


  fields <- dplyr::left_join(dplyr::distinct(data, ticker, symbol = field),
                             dplyr::select(fields, instrument, book, symbol),
                             by = "symbol") %>%
    dplyr::select(ticker, instrument, book, symbol) %>%
    dplyr::arrange(ticker, instrument, book)

  methods::new("EquityMarket", tickers = data.table::as.data.table(dplyr::distinct(tickers, ticker)),
               fields = data.table::as.data.table(fields),
               data = data.table::as.data.table(data), call = match.call())
}


### book ####

#' Equity historical book data from a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database
#'
#'
#' @description Retrieve equity historical book data previously stored in a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
#'   Books include 'key stats', 'income statement', 'balance sheet',
#'   'cash flow statement' and 'ratios'.
#'
#'
#' @param file a scalar chatacter vector. Specifies the target \pkg{storethat}
#'   SQLite database file.
#'
#' @param book a scalar chatacter vector, 'key stats', 'income statement',
#'   'balance sheet', 'cash flow statement' or 'ratios'.
#'
#' @param tickers a chatacter vector. Specifies the Bloomberg equity tickers
#'   to query data for.
#'
#' @param start a scalar character vector. Specifies the starting date for the query
#'   in the following format: 'yyyy-mm-dd'.
#'
#' @param end a scalar character vector. Specifies the end date for the query in the
#'   following format: 'yyyy-mm-dd'.
#'
#' @param verbose a logical scalar vector. Should progression messages be printed?
#'   Defaults to TRUE.
#'
#'
#' @return An S4 object of class \linkS4class{EquityKS} (\code{book = 'key stats'}),
#'   \linkS4class{EquityIS} (\code{book = 'income statement'}), \\linkS4class{EquityBS}
#'   (\code{book = 'balance sheet'}), \linkS4class{EquityCF}
#'   (\code{book = 'cash flow statement'}), \linkS4class{EquityRatios}
#'   (\code{book = 'ratios'}).
#'
#'
#' @seealso
#'
#'   \itemize{
#'
#'     \item{The \link[BBGsymbols]{fields} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite) for details
#'     on the Bloomnerg fields used here.}
#'
#'     \item{Helper datasets in the
#'     \href{https://github.com/bautheac/fewISOs/}{\pkg{fewISOs}} and
#'     \href{https://github.com/bautheac/GICS/}{\pkg{GICS}} packages
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite).}
#'
#'     \item{The \href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite,
#'     in particular the \href{https://github.com/bautheac/storethat/}{\pkg{storethat}}
#'     package.}
#'
#'   }
#'
#'
#' @examples \dontrun{
#'
#'     storethat_equity_books(book = 'key stats',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2010-01-01", end = as.character(Sys.Date()))
#'
#'     storethat_equity_books(book = 'income statement',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2010-01-01", end = as.character(Sys.Date()))
#'
#'     storethat_equity_books(book = 'balance sheet',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2010-01-01", end = as.character(Sys.Date()))
#'
#'     storethat_equity_books(book = 'cash flow statement',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2010-01-01", end = as.character(Sys.Date()))
#'
#'     storethat_equity_books(book = 'ratios',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2010-01-01", end = as.character(Sys.Date()))
#'
#'   }
#'
#' @import BBGsymbols
#'
#' @export
storethat_equity_book <- function(file = NULL, book, tickers, start, end, verbose = TRUE){


  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file),
              stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database
           file (ie. ~/storethat.sqlite)")


  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)
  fields <- "SELECT * FROM support_fields WHERE instrument = 'equity' AND book NOT IN
  ('info', 'market');"
  fields <- RSQLite::dbGetQuery(con, fields)

  if (! all(rlang::is_scalar_character(book), book %in% unique(fields$book)))
    stop("The parameter book' must be supplied as a scalar character vector; one of '",
         paste(fields, collapse = "', '"), "'")

  query <- "SELECT * FROM tickers_equity;"; query <- RSQLite::dbGetQuery(con, query)
  if (! all(tickers %in% query$ticker))
    stop("The parameter 'tickers' must be supplied as a character vector; one or more of '",
         paste(query$ticker, collapse = "', '"), "'")

  if (! all(stringr::str_detect(c(start, end), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))
    stop("The parameters 'start' and 'end' must be supplied as scalar character vectors of
         dates (yyyy-mm-dd)")

  if (! rlang::is_scalar_logical(verbose))
    stop("The parameter 'verbose' must be supplied as a scalar logical vector")


  fields <- dplyr::filter(fields, book == !!book)

  tickers <- query

  dates <- paste0("SELECT * FROM support_dates WHERE date >= '", start, "' AND date <= '",
                  end, "';")
  dates <- RSQLite::dbGetQuery(con, dates)


  data <- lapply(unique(dates$period), function(z){

    query <- paste0("SELECT * FROM data_equity_book_", z, " WHERE ticker_id IN (",
                    paste(tickers$id, collapse = ", "),
                    ") AND field_id IN (", paste(fields$id, collapse = ", "),
                    ") AND date_id >= ", min(dates$id), " AND date_id <= ",
                    max(dates$id), ";")
    query <- RSQLite::dbGetQuery(con, query)

    if (verbose) done(paste0("Period ",
                             data.table::first(dplyr::filter(dates, period == z)$date), "/",
                             data.table::last(dplyr::filter(dates, period == z)$date)))

    query

  }) %>% data.table::rbindlist() %>%
    dplyr::left_join(dplyr::select(tickers, id, ticker), by = c("ticker_id" = "id")) %>%
    dplyr::left_join(dplyr::select(dates, id, date), by = c("date_id" = "id")) %>%
    dplyr::left_join(fields, by = c("field_id" = "id")) %>%
    dplyr::select(ticker, field = symbol, date, value) %>%
    dplyr::arrange(ticker, field, date)


  fields <- dplyr::left_join(dplyr::distinct(data, ticker, symbol = field),
                             dplyr::select(fields, -id), by = "symbol") %>%
    dplyr::select(ticker, instrument:subsection, symbol) %>%
    dplyr::arrange(ticker, instrument, book, type, subtype, section , subsection)


  methods::new(dplyr::case_when(book == "key stats" ~ "EquityKS", book == "balance sheet" ~ "EquityBS",
                                book == "cash flow statement" ~ "EquityCF",
                                book == "income statement" ~ "EquityIS",
                                book == "ratios" ~ "EquityRatios"),
               tickers = data.table::as.data.table(dplyr::select(tickers, -id)),
               fields = data.table::as.data.table(fields),
               data = data.table::as.data.table(data), call = match.call())

}



### info ####

#' Corporate qualitative information from a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database
#'
#'
#' @description Provided with a set of equity Bloomberg tickers retrieves qualitative information
#'   on the corresponding corporation previously stored in a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}}
#'   SQLite database.
#'
#'
#' @param file a scalar chatacter vector. Specifies the target \pkg{storethat} SQLite database file.
#'
#' @param tickers a chatacter vector. Specifies the equity Bloomberg tickers to query data for.
#'
#'
#' @return An S4 object of class \linkS4class{EquityInfo}.
#'
#'
#' @seealso
#'
#'   \itemize{
#'
#'     \item{The \link[BBGsymbols]{fields} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'      (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite) for details on the Bloomnerg
#'      fields used here.}
#'
#'     \item{Helper datasets in the \href{https://github.com/bautheac/fewISOs/}{\pkg{fewISOs}} and
#'     \href{https://github.com/bautheac/GICS/}{\pkg{GICS}} packages
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite).}
#'
#'     \item{The \href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite,
#'     in particular the \href{https://github.com/bautheac/storethat/}{\pkg{storethat}}
#'     package.}
#'
#'   }
#'
#'
#' @examples \dontrun{
#'     storethat_equity_info(tickers = c("BP/ LN Equity", "WEIR LN Equity"))
#'   }
#'
#'
#' @export
storethat_equity_info <- function(file = NULL, tickers){


  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file), stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database file (ie. ~/storethat.sqlite)")


  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)


  query <- "SELECT id, ticker FROM tickers_equity;"; query <- RSQLite::dbGetQuery(con, query)
  if (! all(tickers %in% query$ticker))
    stop("The parameter 'tickers' must be supplied as a character vector; one or more of '",
         paste(query$ticker, collapse = "', '"), "'")


  tickers <- query

  data <- paste0("SELECT ticker_id, field_id, value FROM data_equity_info WHERE ticker_id IN (",
                 paste(tickers$id, collapse = ", "), ");")
  data <- RSQLite::dbGetQuery(con, data)

  fields <- paste0("SELECT id, instrument, book, symbol FROM support_fields WHERE instrument = 'equity'
                   AND book = 'info' AND id IN (",
                   paste(unique(data$field_id), collapse = ", "), ");")
  fields <- RSQLite::dbGetQuery(con, fields)


  RSQLite::dbDisconnect(con)


  data <- dplyr::left_join(data, tickers, by = c("ticker_id" = "id")) %>%
    dplyr::left_join(fields, by = c("field_id" = "id")) %>%
    dplyr::select(ticker, symbol, value)

  fields <- dplyr::left_join(dplyr::distinct(data, ticker, symbol), fields, by = "symbol") %>%
    dplyr::select(ticker, instrument, book, symbol)


  methods::new("EquityInfo", info = tibble::as.tibble(data),
               fields = data.table::as.data.table(fields), call = match.call())
}



## fund ####

### market ####

#' Fund historical market data from a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database
#'
#'
#' @description Provided with a set of Bloomberg fund tickers and a time period,
#'   retrieves the corresponding fund historical market data previously stored in a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
#'
#'
#' @param file a scalar chatacter vector. Specifies the target \pkg{storethat}
#'   SQLite database file.
#'
#' @param tickers a chatacter vector. Specifies the Bloomberg fund tickers to query
#'   data for.
#'
#' @param start a scalar character vector. Specifies the starting date for the query
#'   in the following format: 'yyyy-mm-dd'.
#'
#' @param end a scalar character vector. Specifies the end date for the query in the
#'   following format: 'yyyy-mm-dd'.
#'
#' @param verbose a logical scalar vector. Should progression messages be printed?
#'   Defaults to TRUE.
#'
#'
#' @return An S4 object of class \linkS4class{FundMarket}.
#'
#'
#' @seealso
#'
#'   \itemize{
#'
#'     \item{The \link[BBGsymbols]{fields} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'      (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite) for details on the
#'      Bloomnerg fields used here.}
#'
#'     \item{Helper datasets in the
#'     \href{https://github.com/bautheac/fewISOs/}{\pkg{fewISOs}} and
#'     \href{https://github.com/bautheac/GICS/}{\pkg{GICS}} packages
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite).}
#'
#'     \item{The \href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite,
#'     in particular the \href{https://github.com/bautheac/storethat/}{\pkg{storethat}}
#'     package.}
#'
#'   }
#'
#'
#' @examples \dontrun{
#'
#'     storethat_fund_market(tickers = c("SPY US Equity", "IVV US Equity"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'   }
#'
#'
#' @importFrom magrittr "%<>%"
#'
#'
#' @export
storethat_fund_market <- function(file = NULL, tickers, start, end, verbose = TRUE){


  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file),
              stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite
           database file (ie. ~/storethat.sqlite)")


  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)


  query <- "SELECT id, ticker FROM tickers_fund;"; query <- RSQLite::dbGetQuery(con, query)
  if (! all(tickers %in% query$ticker))
    stop("The parameter 'tickers' must be supplied as a character vector; one or more of '",
         paste(query$ticker, collapse = "', '"), "'")

  if (! all(stringr::str_detect(c(start, end), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))
    stop("The parameters 'start' and 'end' must be supplied as scalar character
         vectors of dates (yyyy-mm-dd)")

  if (! rlang::is_scalar_logical(verbose))
    stop("The parameter 'verbose' must be supplied as a scalar logical vector")


  tickers <- query

  dates <- paste0("SELECT * FROM support_dates WHERE date >= '", start, "' AND date <= '",
                  end, "';")
  dates <- RSQLite::dbGetQuery(con, dates)


  data <- lapply(unique(dates$period), function(z){
    query <- paste0("SELECT * FROM data_fund_market_", z, " WHERE ticker_id IN (",
                    paste(tickers$id, collapse = ", "),
                    ") AND date_id >= ", min(dates$id), " AND date_id <= ",
                    max(dates$id), ";")
    query <- RSQLite::dbGetQuery(con, query)

    if (verbose) done(paste0("Period ",
                             data.table::first(dplyr::filter(dates, period == z)$date), "/",
                             data.table::last(dplyr::filter(dates, period == z)$date)))

    query

  }) %>% data.table::rbindlist()


  fields <- paste0("SELECT * FROM support_fields WHERE id IN (",
                   paste(unique(data$field_id), collapse = ", "), ");")
  fields <- RSQLite::dbGetQuery(con, fields)


  data %<>% dplyr::left_join(dplyr::select(tickers, id, ticker), by = c("ticker_id" = "id")) %>%
    dplyr::left_join(dplyr::select(dates, id, date), by = c("date_id" = "id")) %>%
    dplyr::left_join(fields, by = c("field_id" = "id")) %>%
    dplyr::select(ticker, field = symbol, date, value) %>%
    dplyr::arrange(ticker, field, date)


  fields <- dplyr::left_join(dplyr::distinct(data, ticker, symbol = field),
                             dplyr::select(fields, instrument, book, symbol),
                             by = "symbol") %>%
    dplyr::select(ticker, instrument, book, symbol) %>%
    dplyr::arrange(ticker, instrument, book)

  methods::new("FundMarket", tickers = data.table::as.data.table(dplyr::select(tickers, -id)),
               fields = data.table::as.data.table(fields), data = data.table::as.data.table(data),
               call = match.call())
}




### info ####

#' Fund qualitative information from a \href{https://github.com/bautheac/storethat/}{\pkg{storethat}}
#'   SQLite database
#'
#'
#' @description Provided with a set of equity Bloomberg tickers retrieves qualitative information
#'   on the corresponding fund previously stored in a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
#'
#'
#' @param file a scalar chatacter vector. Specifies the target \pkg{storethat} SQLite database file.
#'
#' @param tickers a chatacter vector. Specifies the futures active contract Bloomberg tickers to
#'   query data for.
#'
#'
#' @return An S4 object of class \linkS4class{FundInfo}.
#'
#'
#' @seealso
#'
#'   \itemize{
#'
#'     \item{The \link[BBGsymbols]{fields} dataset in the
#'     \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} package
#'      (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite) for details on the Bloomnerg
#'      fields used here.}
#'
#'     \item{Helper datasets in the \href{https://github.com/bautheac/fewISOs/}{\pkg{fewISOs}} and
#'     \href{https://github.com/bautheac/GICS/}{\pkg{GICS}} packages
#'     (\href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite).}
#'
#'     \item{The \href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite,
#'     in particular the \href{https://github.com/bautheac/storethat/}{\pkg{storethat}}
#'     package.}
#'
#'   }
#'
#'
#' @examples \dontrun{
#'
#'     storethat_fund_info(tickers = c("SPY US Equity", "IVV US Equity"))
#'
#'   }
#'
#'
#' @export
storethat_fund_info <- function(file = NULL, tickers){


  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file),
              stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite
           database file (ie. ~/storethat.sqlite)")


  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)


  query <- "SELECT id, ticker FROM tickers_fund;"
  query <- RSQLite::dbGetQuery(con, query)
  if (! all(tickers %in% query$ticker))
    stop("The parameter 'tickers' must be supplied as a character vector; one or more of '",
         paste(query$ticker, collapse = "', '"), "'")

  tickers <- query
  data <- paste0("SELECT ticker_id, field_id, value FROM data_fund_info WHERE ticker_id IN (",
                 paste(tickers$id, collapse = ", "), ");")
  data <- RSQLite::dbGetQuery(con, data)

  fields <- paste0("SELECT id, instrument, book, symbol FROM support_fields WHERE
                   instrument = 'fund' AND book = 'info' AND id IN (",
                   paste(unique(data$field_id), collapse = ", "), ");")
  fields <- RSQLite::dbGetQuery(con, fields)

  RSQLite::dbDisconnect(con)

  data <- dplyr::left_join(data, tickers, by = c("ticker_id" = "id")) %>%
    dplyr::left_join(fields, by = c("field_id" = "id")) %>%
    dplyr::select(ticker, symbol, value)

  fields <- dplyr::left_join(dplyr::distinct(data, ticker, symbol), fields, by = "symbol") %>%
    dplyr::select(ticker, instrument, book, symbol)

  methods::new("FundInfo", info = tibble::as.tibble(data),
               fields = data.table::as.data.table(fields), call = match.call())
}





#' Update your \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite
#'   database.
#'
#'
#' @description Updates an existing \href{https://github.com/bautheac/storethat/}{\pkg{storethat}}
#'   SQLite database with fresh data up to date.
#'
#'
#' @param file a scalar character vector. Specifies the path to the appropriate 'storethat.sqlite'
#'   file.
#'
#' @param instrument a scalar character vector. Specifies the financial instruments to get a
#'   snapshot for. Must be one of 'all', equity', 'fund' or 'futures'.
#'
#' @param book a scalar character vector. Instrument dependent; for a given instrument, specifies
#'   the book for the snapshot; 'all' snapshots all the books available for the given instrument.
#'
#' @param name a scalar character vector. Instrument dependent; for a given instrument, specifies
#'   the name for the snapshot; 'all' snapshots all the names available for the given instrument.
#'
#' @param verbose a logical scalar vector. Should progression messages be printed?
#'   Defaults to TRUE.
#'
#'
#' @details For a given financial instrument, updates the database by book and name.
#'
#'
#' @seealso The \href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite,
#'   in particular the \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} &
#'   \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} packages.
#'
#'
#' @examples \dontrun{storethat_update()}
#'
#'
#' @export
storethat_update <- function(file = NULL, instrument = "all", book = "all", name = "all",
                             verbose = TRUE){

  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file),
              stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite
           database file (ie. ~/storethat.sqlite)")


  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)


  instruments <- "SELECT DISTINCT instrument FROM support_fields;"
  instruments <- RSQLite::dbGetQuery(con = con, instruments)
  if (! instrument %in% c("all", instruments$instrument))
    stop("Parameter 'instrument' must be supplied as a scalar character vector; one of '",
         paste(instruments$instrument, collapse = "', '"), "'")


  books <- switch(instrument,
                  all = "SELECT DISTINCT book FROM support_fields;",
                  paste0("SELECT DISTINCT book FROM support_fields WHERE instrument IN ('",
                         paste(instrument, collapse = "', '"), "');")
  )
  books <- RSQLite::dbGetQuery(con = con, books)
  if (! book %in% c("all", books$book))
    stop("Parameter 'book' must be supplied as a scalar character vector; one of '",
         paste(c("all", books$book), collapse = "', '"), "'")


  names <- switch(instrument,
                  all = {
                    lapply(instruments$instrument, function(x)
                      RSQLite::dbGetQuery(con = con, paste0("SELECT * FROM tickers_", x, ";")) %>%
                        dplyr::mutate(instrument = x) %>%
                        dplyr::select(instrument, dplyr::everything())) %>%
                      data.table::rbindlist(fill = TRUE)
                  },
                  RSQLite::dbGetQuery(con = con, paste0("SELECT * FROM tickers_",
                                                        instrument, ";")) %>%
                    dplyr::mutate(instrument = !! instrument) %>%
                    dplyr::select(instrument, dplyr::everything())
  )
  if (! name %in% c("all", names$ticker))
    stop("Parameter 'name' must be supplied as a scalar character vector; one of '",
         paste(c("all", names$ticker), collapse = "', '"), "'")
  if (name != "all") names %<>% dplyr::filter(ticker == !! name)


  RSQLite::dbDisconnect(con)


  switch(instrument,
         all = {

           for (x in instruments$instrument)
             do.call(what = paste0("storethat_update_", x),
                     args = list(book, dplyr::filter(names, instrument == x) %>%
                                   dplyr::select(-instrument),
                                 file = file, verbose = verbose))

         },

         do.call(what = paste0("storethat_update_", instrument),
                 args = list(book,
                             dplyr::filter(names, instrument == !! instrument) %>%
                               dplyr::select(-instrument),
                             file = file, verbose = verbose))

  )

}











