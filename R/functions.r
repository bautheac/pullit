
# tickers ####

#' Constructs Bloomberg futures term structure tickers.
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


# futures ####

## market ####

### global ####

#' Retrieves futures market historical data.
#'
#'
#' @description Provided with a set of futures active contract Bloomberg tickers,
#'   term structure positions, roll parameters and a time period, queries Bloomberg
#'   for the corresponding futures historical market data or retrieves it from an
#'   existing \href{https://github.com/bautheac/storethat/}{\pkg{storethat}}
#'   SQLite database.
#'
#'
#' @param source a scalar character vector. Specifies the data source for the query:
#'   "Bloomberg" or "storethat". Defaults to "Bloomberg".
#'
#' @param type a scalar character vector, 'term structure' or 'aggregate'.
#'   'term structure' returns individual futures chain data for a selected portion
#'   of the term structure (specify desired positions in \code{TS_positions}) while
#'   'aggregate' returns aggregated data over the whole term structure for the
#'   corresponding names (\code{active_contract_tickers}).
#'
#' @param file a scalar chatacter vector. Optional parameter that specifies the
#'   target \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite
#'   database file to retrieve data from.
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
#'     BBG_TS <- pull_futures_market(source = "Bloomberg", type = 'term structure',
#'       active_contract_tickers = c("W A Comdty", "KWA Comdty"),
#'       start = "2000-01-01", end = as.character(Sys.Date()),
#'       TS_positions = 1L:5L, roll_type = "A", roll_days = 0L,
#'       roll_months = 0L, roll_adjustment = "N")
#'
#'     BBG_agg <- pull_futures_market(source = "Bloomberg", type = "aggregate",
#'       active_contract_tickers = c("W A Comdty", "KWA Comdty"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'     storethat_TS <- pull_futures_market(source = "storethat", type = 'term structure',
#'       active_contract_tickers = c("W A Comdty", "KWA Comdty"),
#'       start = "2000-01-01", end = as.character(Sys.Date()),
#'       TS_positions = 1L:5L, roll_type = "A", roll_days = 0L,
#'       roll_months = 0L, roll_adjustment = "N")
#'
#'     storethat_agg <- pull_futures_market(source = "storethat", file = "~/storethat.sqlite",
#'       type = "aggregate", active_contract_tickers = c("W A Comdty", "KWA Comdty"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'   }
#'
#'
#' @import BBGsymbols
#' @importFrom magrittr "%<>%"
#'
#'
#' @export
pull_futures_market <- function(source = "Bloomberg", type = "term structure", active_contract_tickers,
                                start, end, TS_positions = 1L:5L, roll_type = "A", roll_days = 0L,
                                roll_months = 0L, roll_adjustment = "N", file = NULL, verbose = T, ...){

  switch(source,
         Bloomberg = BBG_futures_market(type, active_contract_tickers, start, end, TS_positions, roll_type,
                                        roll_days, roll_months, roll_adjustment, verbose, ...),
         storethat = storethat_futures_market(file = file, type = type, active_contract_tickers = active_contract_tickers,
                                              start = start, end = end, TS_positions = TS_positions, roll_type = roll_type,
                                              roll_days = roll_days, roll_months = roll_months,
                                              roll_adjustment = roll_adjustment, verbose = verbose),
         stop("The parameters 'source' must be supplied as a scalar character vector:
              'Bloomberg' or 'storethat'.")
         )

}


#### Bloomberg ####

#' Retrieves futures market historical data from Bloomberg.
#'
#'
#' @description Provided with a set of futures active contract Bloomberg tickers,
#'   term structure positions, roll parameters and a time period, queries Bloomberg
#'   for the corresponding futures historical market data.
#'
#'
#' @param type a scalar character vector, 'term structure' or 'aggregate'.
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
BBG_futures_market <- function(type, active_contract_tickers, start, end, TS_positions, roll_type,
                               roll_days, roll_months, roll_adjustment, verbose, ...){

  utils::data(list = c("fields", "rolls"), package = "BBGsymbols", envir = environment())

  if (! is.character(active_contract_tickers))
    stop("The parameter 'active_contract_tickers' must be supplied as a character vector of futures
         active contract Bloomberg tickers")

  if (! all(rlang::is_scalar_character(type),
            type %in% (dplyr::distinct(dplyr::filter(fields, instrument == "futures", book == "market"), type) %>%
                       purrr::flatten_chr())))
    stop("The parameter 'type' must be supplied as a scalar character vector; one of '",
         paste((dplyr::distinct(dplyr::filter(fields, instrument == "futures", book == "market"), type) %>%
                  purrr::flatten_chr()), collapse = "', '"), "'")

  if (! all(NROW(c(start, end)) == 2L, stringr::str_detect(c(start, end), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))
    stop("The parameters 'start' and 'end' must be supplied as scalar character vectors of dates (yyyy-mm-dd)")

  if (! is.null(TS_positions))
    if (! all(is.integer(TS_positions))) stop("The parameters 'TS_positions' must be supplied as a vector of integers")

  if (! all(is.null(roll_days), is.null(roll_months)))
    if (! all(rlang::is_scalar_integer(roll_days), rlang::is_scalar_integer(roll_months)))
      stop("The parameters 'roll_days' and 'roll_months' must be supplied as scalar integer vectors")

  if (! all(is.null(roll_type), is.null(roll_adjustment)))
    if (! all(rlang::is_scalar_character(roll_type), rlang::is_scalar_character(roll_adjustment),
              roll_type %in% dplyr::filter(rolls, roll == "type")$symbol,
              roll_adjustment %in% dplyr::filter(rolls, roll == "adjustment")$symbol)
        )
      stop("The parameters 'roll_type' and 'roll_adjustment' must be supplied as scalar character vectors; one of '",
           paste(dplyr::filter(rolls, roll == "type")$symbol, collapse = "', '"), "' ('roll_type') or '",
           paste(dplyr::filter(rolls, roll == "adjustment")$symbol, collapse = "', '"), "' ('roll_adjustment')")

  if (! rlang::is_scalar_logical(verbose))
    stop("The parameter 'verbose' must be supplied as a scalar logical vector")

  switch(type,
         `term structure` = BBG_futures_TS(active_contract_tickers = active_contract_tickers, start = start,
                                           end = end, TS_positions = TS_positions, roll_type = roll_type,
                                           roll_days = roll_days, roll_months = roll_months,
                                           roll_adjustment = roll_adjustment, verbose = verbose, ...),
         `aggregate` = BBG_futures_aggregate(active_contract_tickers = active_contract_tickers, start = start,
                                             end = end, verbose = verbose, ...),
         stop("The parameters 'type' must be supplied as a scalar character vector:
              'term structure' or 'aggregate'.")
         )
}


#### storethat ####

#' Retrieves futures historical market data from a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
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
#' @param type a scalar character vector, 'term structure' or 'aggregate'. 'term structure'
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
storethat_futures_market <- function(file, type, active_contract_tickers, start, end, TS_positions,
                                     roll_type, roll_days, roll_months, roll_adjustment, verbose){


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

  if (! all(NROW(c(start, end)) == 2L, stringr::str_detect(c(start, end), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))
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
         `term structure` = storethat_futures_TS(active_contract_tickers = active_contract_tickers, start = start, end = end,
                                                 TS_positions = TS_positions, roll_type = roll_type, roll_days = roll_days,
                                                 roll_months = roll_months, roll_adjustment = roll_adjustment,
                                                 verbose = verbose, file = file),
         `aggregate` = storethat_futures_aggregate(active_contract_tickers = active_contract_tickers, start = start,
                                                   end = end, verbose = verbose, file = file),
         `spot` = storethat_futures_spot(active_contract_tickers = active_contract_tickers, start = start,
                                         end = end, verbose = verbose, file = file),
         stop("The parameters 'type' must be supplied as a scalar character vector:
              'term structure', 'aggregate' or 'spot'.")
         )
}





### term structure ####

#### Bloomberg ####

#' Retrieves futures term structure historical data from Bloomberg.
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
                           roll_months, roll_adjustment, verbose = T, ...){

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
      lapply(names(data), function(x) {
        if (nrow(data[[x]]) > 0L) { data[[x]]$ticker <- x; data[[x]] }
        else NULL
        }) %>% data.table::rbindlist(use.names = TRUE)
    } else {
      if(nrow(data) < 1L) NULL
      else { data$ticker <- tickers; data }
    }
    data$`active contract ticker` <- y
    if (verbose) done(y); data
  }) %>%
    data.table::rbindlist(use.names = TRUE)

  data %<>%
    tidyr::gather(field, value, -c(`active contract ticker`, ticker, date)) %>%
    dplyr::select(`active contract ticker`, ticker, field, date, value) %>%
    dplyr::arrange(`active contract ticker`, ticker, field, date) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01"), value = as.numeric(value))

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


#### storethat ####

#' Retrieves futures term structure historical data from a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}}
#'   SQLite database.
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


  term_structure_tickers <- sapply(
    active_contract_tickers, function(y) sapply(
      TS_positions, function(x) {
        futures_ticker( y, TS_position = x, roll_type, roll_days, roll_months, roll_adjustment)
      }
    )
  )


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
    dplyr::select(ticker, field = symbol, date, value) %>%
    dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>%
    dplyr::arrange(ticker, field, date)


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




### aggregate ####

#### Bloomberg ####

#' Retrieves futures aggregate historical data from Bloomberg.
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
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01"), value = as.numeric(value)) %>%
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


#### storethat ####

#' Retrieves futures aggregate historical data from from a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
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


  data %<>% dplyr::left_join(dplyr::select(active_contract_tickers, id, ticker), by = c("ticker_id" = "id")) %>%
    dplyr::left_join(dplyr::select(dates, id, date), by = c("date_id" = "id")) %>%
    dplyr::left_join(fields, by = c("field_id" = "id")) %>% dplyr::select(ticker, field = symbol, date, value) %>%
    dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% dplyr::arrange(ticker, field, date)


  fields <- dplyr::left_join(dplyr::distinct(data, ticker, symbol = field),
                             dplyr::select(fields, instrument, book, type, symbol),
                             by = "symbol") %>%
    dplyr::left_join(dplyr::select(active_contract_tickers, ticker), by = "ticker") %>%
    dplyr::select(ticker, instrument, book, type, symbol)

  active_contract_tickers <- dplyr::distinct(data, ticker)

  methods::new("FuturesAggregate", active_contract_tickers = data.table::as.data.table(active_contract_tickers),
               fields = data.table::as.data.table(fields), data = data.table::as.data.table(data),
               call = match.call())
}


### spot ####

#### random source ####
#' Creates a FuturesSpot object from futures spot data.
#'
#' @description Futures spot data refers to data on the corresponding cash market. Futures spot/cash data
#'   is not avaiable on Bloomberg at the time of writing. Should spot data be retrieved from other data
#'   source(s), this function would creates a FuturesSpot data object from it in order to facilitates
#'   storage in a \href{https://bautheac.github.io/storethat/}{\pkg{storethat}} database.
#'
#'
#' @param data a dataframe containing futures spot data. Columns should include:
#'
#'   \itemize{
#'     \item{ticker: futures active contract Bloomberg tickers.}
#'     \item{field: field Bloomberg symbols.
#'       See \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} for a list
#'       of accepted symbols.}
#'     \item{date: observation date (YYYY-MM-DD).}
#'     \item{value: observation value.}
#'   }
#'
#' @return An S4 object of class \linkS4class{FuturesSpot}.
#'
#' @import BBGsymbols
#' @importFrom magrittr "%<>%"
#'
#' @export
random_futures_spot <- function(data){

  utils::data(list = c("fields"), package = "BBGsymbols", envir = environment())
  fields <- dplyr::filter(fields, instrument == "futures", book == "market", type == "spot")

  stopifnot(is.data.frame(data), all.equal(names(data), c("ticker", "field", "date", "value")), unique(data$field) %in% fields$symbol)

  data %<>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::arrange(ticker, field, date) %>%
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01"), value = as.numeric(value))

  active_contract_tickers <- dplyr::distinct(data, ticker)

  fields <- dplyr::distinct(data, ticker, field) %>%
    dplyr::left_join(dplyr::select(fields, instrument, book, type, symbol),
                     by = c("field" = "symbol")) %>%
    dplyr::select(ticker, instrument, book, type, symbol = field) %>%
    dplyr::arrange(ticker, instrument, book, type)

  methods::new("FuturesSpot",
               active_contract_tickers = data.table::as.data.table(dplyr::arrange(active_contract_tickers)),
               fields = data.table::as.data.table(fields), data = data.table::as.data.table(data),
               call = match.call())
}



#### storethat ####

#' Retrieves futures spot historical data from from a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
#'
#'
#' @description Provided with a set of Bloomberg futures active contract tickers and
#'   a time period, retrieves the corresponding futures historical spot data previously
#'   stored in a \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite
#'   database. Futures spot data refers to the corresponding cash market.
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
#' @return An S4 object of class \linkS4class{FuturesSpot}.
#'
#'
#' @examples \dontrun{
#'
#'     storethat_futures_market(type = "spot",
#'       active_contract_tickers = c("W A Comdty", "KWA Comdty"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'   }
#'
#'
#' @importFrom magrittr "%<>%"
storethat_futures_spot <- function(file, active_contract_tickers, start, end, verbose){


  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)


  active_contract_tickers <- paste0("SELECT * FROM tickers_futures WHERE ticker IN ('",
                                    paste(active_contract_tickers, collapse = "', '"), "');")
  active_contract_tickers <- RSQLite::dbGetQuery(con, active_contract_tickers)


  dates <- paste0("SELECT * FROM support_dates WHERE date >= '", start, "' AND date <= '",
                  end, "';")
  dates <- RSQLite::dbGetQuery(con, dates)


  data <- lapply(unique(dates$period), function(z){

    query <- paste0("SELECT * FROM data_futures_spot_", z, " WHERE ticker_id IN (",
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


  data %<>% dplyr::left_join(dplyr::select(active_contract_tickers, id, ticker), by = c("ticker_id" = "id")) %>%
    dplyr::left_join(dplyr::select(dates, id, date), by = c("date_id" = "id")) %>%
    dplyr::left_join(fields, by = c("field_id" = "id")) %>% dplyr::select(ticker, field = symbol, date, value) %>%
    dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% dplyr::arrange(ticker, field, date)


  fields <- dplyr::left_join(dplyr::distinct(data, ticker, symbol = field),
                             dplyr::select(fields, instrument, book, type, symbol),
                             by = "symbol") %>%
    dplyr::left_join(dplyr::select(active_contract_tickers, ticker), by = "ticker") %>%
    dplyr::select(ticker, instrument, book, type, symbol)

  active_contract_tickers <- dplyr::distinct(data, ticker)

  methods::new("FuturesSpot", active_contract_tickers = data.table::as.data.table(active_contract_tickers),
               fields = data.table::as.data.table(fields), data = data.table::as.data.table(data),
               call = match.call())
}


## CFTC ####

### global ####

#' Retrieves futures CFTC reports historical data.
#'
#'
#' @description Provided with a set of futures active contract Bloomberg tickers and a
#'   time period, queries Bloomberg for the corresponding futures CFTC reports
#'   historical data or retrieves it from an
#'   existing \href{https://github.com/bautheac/storethat/}{\pkg{storethat}}
#'   SQLite database.
#'
#'
#' @param source a scalar character vector. Specifies the data source for the query:
#'   "Bloomberg" or "storethat". Defaults to "Bloomberg".
#'
#' @param file a scalar chatacter vector. Optional parameter that specifies the
#'   target \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite
#'   database file to retrieve data from.
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
#'     \item{The \link[BBGsymbols]{tickers_cftc} dataset in the
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
#'
#'     BBG_CFTC <- pull_futures_CFTC(source = "Bloomberg",
#'       active_contract_tickers = c("W A Comdty", "KWA Comdty"),
#'       start = "2010-01-01", end = as.character(Sys.Date()))
#'
#'     storethat_CFTC <- pull_futures_CFTC(source = "storethat",
#'       active_contract_tickers = c("W A Comdty", "KWA Comdty"),
#'       start = "2010-01-01", end = as.character(Sys.Date()))
#'
#'   }
#'
#'
#' @import BBGsymbols
#' @importFrom magrittr "%<>%"
#'
#'
#' @export
pull_futures_CFTC <- function(source = "Bloomberg", active_contract_tickers, start, end,
                              verbose = T, file = NULL, ...){

  switch(source,
         Bloomberg = BBG_futures_CFTC(active_contract_tickers, start, end, verbose, ...),
         storethat = storethat_futures_CFTC(active_contract_tickers, start, end, file, verbose),
         stop("The parameters 'source' must be supplied as a scalar character vector:
              'Bloomberg' or 'storethat'.")
         )

}



### Bloomberg ####

#' Retrieves futures CFTC reports historical data from Bloomberg.
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
#'     \item{The \link[BBGsymbols]{tickers_cftc} dataset in the
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
#'
#'     BBG_futures_CFTC(active_contract_tickers = c("W A Comdty", "KWA Comdty"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'   }
#'
#'
#' @import BBGsymbols
#' @importFrom magrittr "%<>%"
BBG_futures_CFTC <- function(active_contract_tickers, start, end, verbose = T, ...){

  utils::data(list = c("fields", "tickers_cftc"), package = "BBGsymbols", envir = environment())

  if (! is.character(active_contract_tickers))
    stop("The parameter 'active_contract_tickers' must be supplied as a character vector of
         futures active contract Bloomberg tickers")

  if (! all(NROW(c(start, end)) == 2L, stringr::str_detect(c(start, end), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))
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
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01"), value = as.numeric(value))

  if (nrow(data) == 0L) warning("No CFTC data found.")

  active_contract_tickers <- dplyr::distinct(data, `active contract ticker`)

  tickers <- dplyr::distinct(data, `active contract ticker`, ticker)

  methods::new("FuturesCFTC",
               active_contract_tickers = data.table::as.data.table(dplyr::arrange(active_contract_tickers)),
               cftc_tickers = data.table::as.data.table(tickers),
               data = data.table::as.data.table(dplyr::select(data, ticker, date, value)),
               call = match.call())
}

### storethat ####

#' Retrieves futures CFTC position historical data from from a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
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
#'     \item{The \link[BBGsymbols]{tickers_cftc} dataset in the
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
#'
#'   }
#'
#'
#' @importFrom magrittr "%<>%"
storethat_futures_CFTC <- function(active_contract_tickers, start, end, file = NULL, verbose = T){


  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file),
              stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite
           database file (ie. ~/storethat.sqlite)")


  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)


  tickers <- "SELECT id, ticker FROM tickers_futures;"
  tickers <- RSQLite::dbGetQuery(con, tickers)
  if (! all(active_contract_tickers %in% tickers$ticker))
    stop("The parameter 'active_contract_tickers' must be supplied as a character
         vector; one or more of '", paste(tickers$ticker, collapse = "', '"), "'")

  tickers <- paste0("SELECT id, ticker FROM tickers_futures WHERE ticker IN ('",
                    paste(active_contract_tickers, collapse = "', '"), "');")
  tickers <- RSQLite::dbGetQuery(con, tickers)


  if (! all(NROW(c(start, end)) == 2L, stringr::str_detect(c(start, end), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))
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
    dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>%
    dplyr::arrange(`active contract ticker`, ticker, field, date)


  RSQLite::dbDisconnect(con)


  methods::new("FuturesCFTC",
               active_contract_tickers = dplyr::distinct(cftc_tickers, ticker = `active contract ticker`) %>%
                 data.table::as.data.table(),
               cftc_tickers = dplyr::select(cftc_tickers, -c(id, active_contract_ticker_id)) %>%
                 data.table::as.data.table(),
               data = data.table::as.data.table(data), call = match.call())
}





## info ####

### global ####

#' Retrieves futures series qualitative information.
#'
#'
#' @description Provided with a set of futures active contract Bloomberg tickers
#'   retrieves qualitative information on the corresponding futures series or
#'   retrieves it from an existing
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database..
#'
#'
#' @param source a scalar character vector. Specifies the data source for the query:
#'   "Bloomberg" or "storethat". Defaults to "Bloomberg".
#'
#' @param file a scalar chatacter vector. Optional parameter that specifies the
#'   target \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite
#'   database file to retrieve data from.
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
#'     BBG_info <- pull_futures_info(source = "Bloomberg",
#'       active_contract_tickers = c("W A Comdty", "KWA Comdty"))
#'
#'     storethat_info <- pull_futures_info(source = "storethat",
#'       active_contract_tickers = c("W A Comdty", "KWA Comdty"))
#'
#'   }
#'
#'
#' @export
pull_futures_info <- function(source = "Bloomberg", active_contract_tickers, file = NULL, ...){

  switch(source,
         Bloomberg = BBG_futures_info(active_contract_tickers, ...),
         storethat = storethat_futures_info(active_contract_tickers, file),
         stop("The parameters 'source' must be supplied as a scalar character vector:
              'Bloomberg' or 'storethat'.")
  )

}


### Bloomberg ####

#' Retrieves futures series qualitative information from Bloomberg.
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


### storethat ####

#' Retrieves futures series qualitative information from a
#' \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
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
#'
#'     storethat_futures_info(active_contract_tickers = c("W A Comdty", "KWA Comdty"))
#'
#'   }
storethat_futures_info <- function(active_contract_tickers, file = NULL){


  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file), stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database file (ie. ~/storethat.sqlite)")


  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)


  tickers <- "SELECT id, ticker FROM tickers_futures;"; tickers <- RSQLite::dbGetQuery(con, tickers)
  if (! all(active_contract_tickers %in% tickers$ticker))
    stop("The parameter 'active_contract_tickers' must be supplied as a character vector; one or more of '",
         paste(tickers$ticker, collapse = "', '"), "'")

  tickers <- paste0("SELECT id, ticker FROM tickers_futures WHERE ticker IN ('",
                    paste(active_contract_tickers, collapse = "', '"), "');")
  tickers <- RSQLite::dbGetQuery(con, tickers)


  data <- paste0("SELECT ticker_id, field_id, value FROM data_futures_info WHERE ticker_id IN (",
                 paste(tickers$id, collapse = ", "), ");")
  data <- RSQLite::dbGetQuery(con, data)


  fields <- paste0("SELECT id, instrument, book, symbol FROM support_fields WHERE instrument = 'futures'
                   AND book = 'info' AND id IN (", paste(unique(data$field_id), collapse = ", "), ");")
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



# equity ####

## market ####

### global ####

#' Retrieves equity historical market data.
#'
#'
#' @description Provided with a set of equity Bloomberg tickers and a time period,
#'   queries Bloomberg for the corresponding equity historical market data or
#'   retrieves it from an existing
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database..
#'
#'
#' @param source a scalar character vector. Specifies the data source for the query:
#'   "Bloomberg" or "storethat". Defaults to "Bloomberg".
#'
#' @param file a scalar chatacter vector. Optional parameter that specifies the
#'   target \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite
#'   database file to retrieve data from.
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
#'     BBG_mkt <- pull_equity_market(source = "Bloomberg",
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'     storethat_mkt <- pull_equity_market(source = "storethat",
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'   }
#'
#'
#' @import BBGsymbols
#' @importFrom magrittr "%<>%"
#'
#' @export
pull_equity_market <- function(source = "Bloomberg", tickers, start, end,
                               verbose = T, file = NULL, ...){

  switch(source,
         Bloomberg = BBG_equity_market(tickers, start, end, verbose, ...),
         storethat = storethat_equity_market(file, tickers, start, end, verbose),
         stop("The parameters 'source' must be supplied as a scalar character vector:
              'Bloomberg' or 'storethat'.")
  )

}


### Bloomberg ####

#' Retrieves equity historical market data from Bloomberg.
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
BBG_equity_market <- function(tickers, start, end, verbose = T, ...){

  utils::data(list = c("fields"), package = "BBGsymbols", envir = environment())

  if (! is.character(tickers))
    stop("The parameter 'tickers' must be supplied as a character vector of
         equity Bloomberg tickers")

  if (! all(NROW(c(start, end)) == 2L, stringr::str_detect(c(start, end), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))
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
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01"), value = as.numeric(value)) %>%
    dplyr::arrange(ticker, field, date)

  if (nrow(data) == 0L) warning("No market data found.")

  tickers <- dplyr::distinct(data, ticker)

  fields <- dplyr::filter(fields, instrument == "equity", book == "market")

  fields <- dplyr::distinct(data, ticker, field) %>%
    dplyr::left_join(dplyr::select(fields, instrument, book, symbol), by = c("field" = "symbol")) %>%
    dplyr::select(ticker, instrument, book, symbol = field) %>% dplyr::arrange(ticker, instrument, book)

  methods::new("EquityMarket", tickers = data.table::as.data.table(dplyr::arrange(tickers)),
               fields = data.table::as.data.table(fields), data = data.table::as.data.table(data), call = match.call())
}

### storethat ####

#' Retrieves equity historical market data from a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}}
#'   SQLite database.
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
storethat_equity_market <- function(file, tickers, start, end, verbose){


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

  query <- paste0("SELECT id, ticker FROM tickers_equity WHERE ticker IN ('",
                  paste(tickers, collapse = "', '"), "');")
  tickers <- RSQLite::dbGetQuery(con, query)

  if (! all(NROW(c(start, end)) == 2L, stringr::str_detect(c(start, end), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))
    stop("The parameters 'start' and 'end' must be supplied as scalar character vectors of dates
         (yyyy-mm-dd)")

  if (! rlang::is_scalar_logical(verbose))
    stop("The parameter 'verbose' must be supplied as a scalar logical vector")


  dates <- paste0("SELECT * FROM support_dates WHERE date >= '", start, "' AND date <= '", end, "';")
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


  RSQLite::dbDisconnect(con)


  data %<>% dplyr::left_join(dplyr::select(tickers, id, ticker), by = c("ticker_id" = "id")) %>%
    dplyr::left_join(dplyr::select(dates, id, date), by = c("date_id" = "id")) %>%
    dplyr::left_join(fields, by = c("field_id" = "id")) %>%
    dplyr::select(ticker, field = symbol, date, value) %>%
    dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>%
    dplyr::arrange(ticker, field, date)


  fields <- dplyr::left_join(dplyr::distinct(data, ticker, symbol = field),
                             dplyr::select(fields, instrument, book, symbol),
                             by = "symbol") %>%
    dplyr::select(ticker, instrument, book, symbol) %>% dplyr::arrange(ticker, instrument, book)

  methods::new("EquityMarket", tickers = data.table::as.data.table(dplyr::distinct(tickers, ticker)),
               fields = data.table::as.data.table(fields),
               data = data.table::as.data.table(data), call = match.call())
}





## book ####

### global ####

#' Retrieves equity historical book data.
#'
#'
#' @description Retrieves equity historical book data from Bloomberg. Books include
#'   'key stats', 'income statement', 'balance sheet', 'cash flow statement' and
#'   'ratios' or retrieves it from an existing
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
#'
#'
#' @param source a scalar character vector. Specifies the data source for the query:
#'   "Bloomberg" or "storethat". Defaults to "Bloomberg".
#'
#' @param file a scalar chatacter vector. Optional parameter that specifies the
#'   target \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite
#'   database file to retrieve data from.
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
#'     BBG_KS <- pull_equity_books(source = "Bloomberg", book = 'key stats',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"), start = "2000-01-01",
#'       end = as.character(Sys.Date()))
#'
#'     BBG_PL <- pull_equity_books(source = "Bloomberg", book = 'income statement',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"), start = "2000-01-01",
#'       end = as.character(Sys.Date()))
#'
#'     BBG_BS <- pull_equity_books(source = "Bloomberg", book = 'balance sheet',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"), start = "2000-01-01",
#'       end = as.character(Sys.Date()))
#'
#'     BBG_CF <- pull_equity_books(source = "Bloomberg", book = 'cash flow statement',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"), start = "2000-01-01",
#'       end = as.character(Sys.Date()))
#'
#'     BBG_R <- pull_equity_books(source = "Bloomberg", book = 'ratios',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"), start = "2000-01-01",
#'       end = as.character(Sys.Date()))
#'
#'     storethat_KS <- pull_equity_books(source = "storethat", file = "~/storethat.sqlite",
#'       book = 'key stats', tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'     storethat_PL <- pull_equity_books(source = "storethat", book = 'income statement',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"), start = "2000-01-01",
#'       end = as.character(Sys.Date()))
#'
#'     storethat_BS <- pull_equity_books(source = "storethat", file = "~/storethat.sqlite",
#'       book = 'balance sheet', tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'     storethat_CF <- pull_equity_books(source = "storethat", book = 'cash flow statement',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"), start = "2000-01-01",
#'       end = as.character(Sys.Date()))
#'
#'     storethat_R <- pull_equity_books(source = "storethat",  file = "~/storethat.sqlite",
#'       book = 'ratios', tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'
#'   }
#'
#'
#' @import BBGsymbols
#'
#'
#' @export
pull_equity_book <- function(source = "Bloomberg", book, tickers, start, end,
                             verbose = T, file = NULL, ...){

  switch(source,
         Bloomberg = BBG_equity_book(book, tickers, start, end, verbose, ...),
         storethat = storethat_equity_book(file, book, tickers, start, end, verbose),
         stop("The parameters 'source' must be supplied as a scalar character vector:
              'Bloomberg' or 'storethat'.")
  )

}


### Bloomberg ####

#' Retrieves equity historical book data from Bloomberg.
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
#'     BBG_KS <- BBG_equity_books(book = 'key stats',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'     BBG_PL<- BBG_equity_books(book = 'income statement',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'     BBG_BS <- BBG_equity_books(book = 'balance sheet',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'     BBG_CF <- BBG_equity_books(book = 'cash flow statement',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'     BBG_R <- BBG_equity_books(book = 'ratios',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'   }
#'
#'
#' @import BBGsymbols
BBG_equity_book <- function(book, tickers, start, end, verbose = T, ...){


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

  if (! all(NROW(c(start, end)) == 2L, stringr::str_detect(c(start, end), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))
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
    dplyr::select(ticker, field, date, value) %>%
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01"), value = as.numeric(value))

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


### storethat ####

#' Retrieves equity historical book data from a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
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
#'     storethat_KS <- storethat_equity_books(book = 'key stats',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2010-01-01", end = as.character(Sys.Date()))
#'
#'     storethat_PL <- storethat_equity_books(book = 'income statement',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2010-01-01", end = as.character(Sys.Date()))
#'
#'     storethat_BS <- storethat_equity_books(book = 'balance sheet',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2010-01-01", end = as.character(Sys.Date()))
#'
#'     storethat_CF <- storethat_equity_books(book = 'cash flow statement',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2010-01-01", end = as.character(Sys.Date()))
#'
#'     storethat_R <- storethat_equity_books(book = 'ratios',
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"),
#'       start = "2010-01-01", end = as.character(Sys.Date()))
#'
#'   }
#'
#' @import BBGsymbols
storethat_equity_book <- function(file = NULL, book, tickers, start, end, verbose = T){


  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file),
              stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database
           file (ie. ~/storethat.sqlite)")


  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)
  fields <- "SELECT * FROM support_fields WHERE instrument = 'equity' AND book NOT IN ('info', 'market');"
  fields <- RSQLite::dbGetQuery(con, fields)

  if (! all(rlang::is_scalar_character(book), book %in% unique(fields$book)))
    stop("The parameter book' must be supplied as a scalar character vector; one of '",
         paste(fields, collapse = "', '"), "'")

  query <- "SELECT * FROM tickers_equity;"; query <- RSQLite::dbGetQuery(con, query)
  if (! all(tickers %in% query$ticker))
    stop("The parameter 'tickers' must be supplied as a character vector; one or more of '",
         paste(query$ticker, collapse = "', '"), "'")

  query <- paste0("SELECT * FROM tickers_equity WHERE ticker IN ('", paste(tickers, collapse = "', '"), "');")
  tickers <- RSQLite::dbGetQuery(con, query)

  if (! all(NROW(c(start, end)) == 2L, stringr::str_detect(c(start, end), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))
    stop("The parameters 'start' and 'end' must be supplied as scalar character vectors of dates (yyyy-mm-dd)")

  if (! rlang::is_scalar_logical(verbose))
    stop("The parameter 'verbose' must be supplied as a scalar logical vector")


  fields <- dplyr::filter(fields, book == !!book)

  dates <- paste0("SELECT * FROM support_dates WHERE date >= '", start,
                  "' AND date <= '", end, "';")
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
    dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>%
    dplyr::arrange(ticker, field, date)


  fields <- dplyr::left_join(dplyr::distinct(data, ticker, symbol = field),
                             dplyr::select(fields, -id), by = "symbol") %>%
    dplyr::select(ticker, instrument:subsection, symbol) %>%
    dplyr::arrange(ticker, instrument, book, type, subtype, section , subsection)


  RSQLite::dbDisconnect(con)


  methods::new(dplyr::case_when(book == "key stats" ~ "EquityKS", book == "balance sheet" ~ "EquityBS",
                                book == "cash flow statement" ~ "EquityCF",
                                book == "income statement" ~ "EquityIS",
                                book == "ratios" ~ "EquityRatios"),
               tickers = data.table::as.data.table(dplyr::select(tickers, -id)),
               fields = data.table::as.data.table(fields),
               data = data.table::as.data.table(data), call = match.call())

}





## info ####

### global ####

#' Retrieves equity qualitative information.
#'
#'
#' @description Provided with a set of equity Bloomberg tickers pulls qualitative
#'   information on the corresponding corporations from Bloomberg or retrieves it
#'   from an existing
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
#'
#'
#' @param source a scalar character vector. Specifies the data source for the query:
#'   "Bloomberg" or "storethat". Defaults to "Bloomberg".
#'
#' @param file a scalar chatacter vector. Optional parameter that specifies the
#'   target \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite
#'   database file to retrieve data from.
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
#'
#'     BBG_info <- pull_equity_info(source = "Bloomberg",
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"))
#'
#'     storethat_info <- pull_equity_info(source = "storethat",
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"))
#'
#'     storethat_info <- pull_equity_info(source = "storethat", file = "~/storethat.sqlite",
#'       tickers = c("BP/ LN Equity", "WEIR LN Equity"))
#'
#'   }
#'
#'
#' @import BBGsymbols
#' @importFrom magrittr "%<>%"
#'
#'
#' @export
pull_equity_info <- function(source = "Bloomberg", tickers, file = NULL, ...){

  switch(source,
         Bloomberg = BBG_equity_info(tickers, ...),
         storethat = storethat_equity_info(tickers, file),
         stop("The parameters 'source' must be supplied as a scalar character vector:
              'Bloomberg' or 'storethat'.")
         )

}


### Bloomberg ####

#' Retrieves corporate qualitative information from Bloomberg.
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


### storethat ####

#' Retrieves Corporate qualitative information from a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
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
#'
#'     storethat_equity_info(tickers = c("BP/ LN Equity", "WEIR LN Equity"))
#'
#'   }
storethat_equity_info <- function(tickers, file = NULL){


  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file), stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database file (ie. ~/storethat.sqlite)")


  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)


  query <- "SELECT id, ticker FROM tickers_equity;"; query <- RSQLite::dbGetQuery(con, query)
  if (! all(tickers %in% query$ticker))
    stop("The parameter 'tickers' must be supplied as a character vector; one or more of '",
         paste(query$ticker, collapse = "', '"), "'")

  query <- paste0("SELECT id, ticker FROM tickers_equity WHERE ticker IN ('", paste(tickers, collapse = "', '"), "');")
  tickers <- RSQLite::dbGetQuery(con, query)

  data <- paste0("SELECT ticker_id, field_id, value FROM data_equity_info WHERE ticker_id IN (",
                 paste(tickers$id, collapse = ", "), ");")
  data <- RSQLite::dbGetQuery(con, data)

  fields <- paste0("SELECT id, instrument, book, symbol FROM support_fields WHERE instrument = 'equity'
                   AND book = 'info' AND id IN (", paste(unique(data$field_id), collapse = ", "), ");")
  fields <- RSQLite::dbGetQuery(con, fields)


  RSQLite::dbDisconnect(con)


  data <- dplyr::left_join(data, tickers, by = c("ticker_id" = "id")) %>%
    dplyr::left_join(fields, by = c("field_id" = "id")) %>%
    dplyr::select(ticker, symbol, value)

  fields <- dplyr::left_join(dplyr::distinct(data, ticker, symbol), fields, by = "symbol") %>%
    dplyr::select(ticker, instrument, book, symbol)


  methods::new("EquityInfo", info = tibble::as.tibble(data), fields = data.table::as.data.table(fields),
               call = match.call())
}







# fund ####

## market ####

### global ####

#' Retrieves fund historical market data.
#'
#'
#' @description Provided with a set of Bloomberg fund tickers and a time period,
#'   queries Bloomberg for the corresponding fund historical market data or
#'   retrieves it from an existing
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
#'
#'
#' @param source a scalar character vector. Specifies the data source for the query:
#'   "Bloomberg" or "storethat". Defaults to "Bloomberg".
#'
#' @param file a scalar chatacter vector. Optional parameter that specifies the
#'   target \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite
#'   database file to retrieve data from.
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
#'
#'     BBG_mkt <- pull_fund_market(source = "Bloomberg", tickers = c("SPY US Equity", "IVV US Equity"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'     storethat_mkt <- pull_fund_market(source = "Bloomberg", file = "~/storethat.sqlite",
#'       tickers = c("SPY US Equity", "IVV US Equity"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'     storethat_mkt <- pull_fund_market(source = "Bloomberg", tickers = c("SPY US Equity", "IVV US Equity"),
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
pull_fund_market <- function(source = "Bloomberg", tickers, start, end,
                             verbose = T, file = NULL, ...){

  switch(source,
         Bloomberg = BBG_fund_market(tickers, start, end, verbose, ...),
         storethat = storethat_fund_market(file, tickers, start, end, verbose),
         stop("The parameters 'source' must be supplied as a scalar character vector:
              'Bloomberg' or 'storethat'.")
         )

}


### Bloomberg ####

#' Retrieves fund historical market data from Bloomberg.
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
#'
#'     BBG_fund_market(tickers = c("SPY US Equity", "IVV US Equity"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'   }
#'
#'
#' @import BBGsymbols
#' @importFrom magrittr "%<>%"
BBG_fund_market <- function(tickers, start, end, verbose = T, ...){

  utils::data(list = c("fields"), package = "BBGsymbols", envir = environment())

  if (! is.character(tickers))
    stop("The parameter 'tickers' must be supplied as a character vector of equity Bloomberg tickers")

  if (! all(NROW(c(start, end)) == 2L, stringr::str_detect(c(start, end), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))
    stop("The parameters 'start' and 'end' must be supplied as scalar character vectors of dates (yyyy-mm-dd)")

  if (! rlang::is_scalar_logical(verbose))
    stop("The parameter 'verbose' must be supplied as a scalar logical vector")

  data <- lapply(tickers, function(x) {
    data <- BBG_pull_historical_market(x, fields = dplyr::filter(fields, instrument == "fund", book == "market") %>%
                                         dplyr::select(symbol) %>% purrr::flatten_chr(),
                                       start, end, ...) %>% dplyr::mutate(ticker = x)
    if (verbose) done(x); data
  }) %>%
    data.table::rbindlist(use.names = TRUE)

  data %<>%
    tidyr::gather(field, value, -c(ticker, date)) %>% dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::select(ticker, field, date, value) %>%
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01"), value = as.numeric(value)) %>%
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


### storethat ####

#' Retrieves fund historical market data from a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
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
storethat_fund_market <- function(file = NULL, tickers, start, end, verbose = T){


  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file), stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database file (ie. ~/storethat.sqlite)")


  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)


  query <- "SELECT id, ticker FROM tickers_fund;"; query <- RSQLite::dbGetQuery(con, query)
  if (! all(tickers %in% query$ticker))
    stop("The parameter 'tickers' must be supplied as a character vector; one or more of '",
         paste(query$ticker, collapse = "', '"), "'")

  query <- paste0("SELECT id, ticker FROM tickers_fund WHERE ticker IN ('", paste(tickers, collapse = "', '"), "');")
  tickers <- RSQLite::dbGetQuery(con, query)

  if (! all(NROW(c(start, end)) == 2L, stringr::str_detect(c(start, end), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))
    stop("The parameters 'start' and 'end' must be supplied as scalar character vectors of dates (yyyy-mm-dd)")

  if (! rlang::is_scalar_logical(verbose))
    stop("The parameter 'verbose' must be supplied as a scalar logical vector")


  dates <- paste0("SELECT * FROM support_dates WHERE date >= '", start, "' AND date <= '", end, "';")
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


  fields <- paste0("SELECT * FROM support_fields WHERE id IN (", paste(unique(data$field_id), collapse = ", "), ");")
  fields <- RSQLite::dbGetQuery(con, fields)


  data %<>% dplyr::left_join(dplyr::select(tickers, id, ticker), by = c("ticker_id" = "id")) %>%
    dplyr::left_join(dplyr::select(dates, id, date), by = c("date_id" = "id")) %>%
    dplyr::left_join(fields, by = c("field_id" = "id")) %>% dplyr::select(ticker, field = symbol, date, value) %>%
    dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% dplyr::arrange(ticker, field, date)


  fields <- dplyr::left_join(dplyr::distinct(data, ticker, symbol = field),
                             dplyr::select(fields, instrument, book, symbol),
                             by = "symbol") %>%
    dplyr::select(ticker, instrument, book, symbol) %>%
    dplyr::arrange(ticker, instrument, book)


  RSQLite::dbDisconnect(con)


  methods::new("FundMarket", tickers = data.table::as.data.table(dplyr::select(tickers, -id)),
               fields = data.table::as.data.table(fields), data = data.table::as.data.table(data),
               call = match.call())
}




## info ####

### global ####

#' Retrieves fund qualitative information.
#'
#'
#' @description Provided with a set of fund Bloomberg tickers pulls qualitative
#'   information on the corresponding fund(s) or
#'   retrieves it from an existing
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
#'
#'
#' @param source a scalar character vector. Specifies the data source for the query:
#'   "Bloomberg" or "storethat". Defaults to "Bloomberg".
#'
#' @param file a scalar chatacter vector. Optional parameter that specifies the
#'   target \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite
#'   database file to retrieve data from.
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
#'
#'     BBG_info <- pull_fund_info(source = "Bloomberg", tickers = c("SPY US Equity", "IVV US Equity"))
#'
#'     storethat_info <- pull_fund_info(source = "storethat", file = "~/storethat.sqlite",
#'       tickers = c("SPY US Equity", "IVV US Equity"))
#'
#'     storethat_info <- pull_fund_info(source = "storethat", tickers = c("SPY US Equity", "IVV US Equity"))
#'
#'   }
#'
#'
#' @importFrom magrittr "%<>%"
#'
#' @export
pull_fund_info <- function(source = "Bloomberg", tickers, file = NULL, ...){

  switch(source,
         Bloomberg = BBG_fund_info(tickers, ...),
         storethat = storethat_fund_info(file, tickers),
         stop("The parameters 'source' must be supplied as a scalar character vector:
              'Bloomberg' or 'storethat'.")
  )

}



### Bloomberg ####

#' Retrieves fund qualitative information from Bloomberg.
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
                             by = "symbol") %>%
    dplyr::select(ticker, instrument, book, symbol)


  Rblpapi::blpDisconnect(con)


  methods::new("FundInfo", info = tibble::as.tibble(query),
               fields = data.table::as.data.table(fields), call = match.call())
}


### storethat ####

#' Retrieves fund qualitative information from a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}}
#'   SQLite database.
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
storethat_fund_info <- function(file = NULL, tickers){


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

  query <- paste0("SELECT id, ticker FROM tickers_fund WHERE ticker IN ('", paste(tickers, collapse = "', '"), "');")
  tickers <- RSQLite::dbGetQuery(con, query)


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








# index ####

## market ####

### global ####

#' Retrieves index historical market data.
#'
#'
#' @description Provided with a set of Bloomberg index tickers and a time period,
#'   queries Bloomberg for the corresponding index historical market data or
#'   retrieves it from an existing
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
#'
#'
#' @param source a scalar character vector. Specifies the data source for the query:
#'   "Bloomberg" or "storethat". Defaults to "Bloomberg".
#'
#' @param file a scalar chatacter vector. Optional parameter that specifies the
#'   target \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite
#'   database file to retrieve data from.
#'
#' @param tickers a chatacter vector. Specifies the Bloomberg index tickers to
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
#'
#'     BBG_mkt <- pull_index_market(source = "Bloomberg", tickers = c("NEIXCTA Index", "BARCCTA Index"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'     storethat_mkt <- pull_index_market(source = "storethat", file = "~/storethat.sqlite",
#'       tickers = c("NEIXCTA Index", "BARCCTA Index"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'     storethat_mkt <- pull_index_market(source = "storethat", tickers = c("NEIXCTA Index", "BARCCTA Index"),
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
pull_index_market <- function(source = "Bloomberg", tickers, start, end,
                              verbose = T, file = NULL, ...){

  switch(source,
         Bloomberg = BBG_index_market(tickers, start, end, verbose, ...),
         storethat = storethat_index_market(file, tickers, start, end, verbose),
         stop("The parameters 'source' must be supplied as a scalar character vector:
              'Bloomberg' or 'storethat'.")
         )

}


### Bloomberg ####

#' Retrieves index historical market data from Bloomberg.
#'
#'
#' @description Provided with a set of Bloomberg index tickers and a time period,
#'   queries Bloomberg for the corresponding index historical market data.
#'
#'
#' @param tickers a chatacter vector. Specifies the Bloomberg index tickers to
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
#' @return An S4 object of class \linkS4class{IndexMarket}.
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
#'
#'     BBG_index_market(tickers = c("NEIXCTA Index", "BARCCTA Index"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'   }
#'
#'
#' @import BBGsymbols
#' @importFrom magrittr "%<>%"
BBG_index_market <- function(tickers, start, end, verbose = T, ...){

  utils::data(list = c("fields"), package = "BBGsymbols", envir = environment())

  if (! is.character(tickers))
    stop("The parameter 'tickers' must be supplied as a character vector of equity Bloomberg tickers")

  if (! all(NROW(c(start, end)) == 2L, stringr::str_detect(c(start, end), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))
    stop("The parameters 'start' and 'end' must be supplied as scalar character
         vectors of dates (yyyy-mm-dd)")

  if (! rlang::is_scalar_logical(verbose))
    stop("The parameter 'verbose' must be supplied as a scalar logical vector")

  data <- lapply(tickers, function(x) {
    data <- BBG_pull_historical_market(x,
                                       fields = dplyr::filter(fields, instrument == "index", book == "market") %>%
                                         dplyr::select(symbol) %>% purrr::flatten_chr(),
                                       start, end, ...) %>% dplyr::mutate(ticker = x)
    if (verbose) done(x); data
  }) %>%
    data.table::rbindlist(use.names = TRUE)

  data %<>%
    tidyr::gather(field, value, -c(ticker, date)) %>% dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::select(ticker, field, date, value) %>%
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01"), value = as.numeric(value)) %>%
    dplyr::arrange(ticker, field, date)

  if (nrow(data) == 0L) warning("No market data found")

  tickers <- dplyr::distinct(data, ticker)

  fields <- dplyr::filter(fields, instrument == "index", book == "market")

  fields <- dplyr::distinct(data, ticker, field) %>% dplyr::left_join(dplyr::select(fields, instrument, book, symbol),
                                                                      by = c("field" = "symbol")) %>%
    dplyr::select(ticker, instrument, book, symbol = field) %>% dplyr::arrange(ticker, instrument, book)

  methods::new("IndexMarket", tickers = data.table::as.data.table(dplyr::arrange(tickers)),
               fields = data.table::as.data.table(fields), data = data.table::as.data.table(data),
               call = match.call())
}


### storethat ####

#' Retrieves index historical market data from a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
#'
#'
#' @description Provided with a set of Bloomberg index tickers and a time period,
#'   retrieves the corresponding index historical market data previously stored in a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
#'
#'
#' @param file a scalar chatacter vector. Specifies the target \pkg{storethat}
#'   SQLite database file.
#'
#' @param tickers a chatacter vector. Specifies the Bloomberg index tickers to query
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
#' @return An S4 object of class \linkS4class{IndexMarket}.
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
#'     storethat_index_market(tickers = c("NEIXCTA Index", "BARCCTA Index"),
#'       start = "2000-01-01", end = as.character(Sys.Date()))
#'
#'   }
#'
#'
#' @importFrom magrittr "%<>%"
storethat_index_market <- function(file = NULL, tickers, start, end, verbose = T){


  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file),
              stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite
           database file (ie. ~/storethat.sqlite)")


  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)


  query <- "SELECT id, ticker FROM tickers_index;"; query <- RSQLite::dbGetQuery(con, query)
  if (! all(tickers %in% query$ticker))
    stop("The parameter 'tickers' must be supplied as a character vector; one or more of '",
         paste(query$ticker, collapse = "', '"), "'")

  query <- paste0("SELECT id, ticker FROM tickers_index WHERE ticker IN ('",
                  paste(tickers, collapse = "', '"), "');")
  tickers <- RSQLite::dbGetQuery(con, query)

  if (! all(NROW(c(start, end)) == 2L, stringr::str_detect(c(start, end), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))
    stop("The parameters 'start' and 'end' must be supplied as scalar character
         vectors of dates (yyyy-mm-dd)")

  if (! rlang::is_scalar_logical(verbose))
    stop("The parameter 'verbose' must be supplied as a scalar logical vector")


  dates <- paste0("SELECT * FROM support_dates WHERE date >= '", start, "' AND date <= '", end, "';")
  dates <- RSQLite::dbGetQuery(con, dates)


  data <- lapply(unique(dates$period), function(z){
    query <- paste0("SELECT * FROM data_index_market_", z, " WHERE ticker_id IN (",
                    paste(tickers$id, collapse = ", "),
                    ") AND date_id >= ", min(dates$id), " AND date_id <= ",
                    max(dates$id), ";")
    query <- RSQLite::dbGetQuery(con, query)

    if (verbose) done(paste0("Period ",
                             data.table::first(dplyr::filter(dates, period == z)$date), "/",
                             data.table::last(dplyr::filter(dates, period == z)$date)))

    query

  }) %>% data.table::rbindlist()


  fields <- paste0("SELECT * FROM support_fields WHERE id IN (", paste(unique(data$field_id), collapse = ", "), ");")
  fields <- RSQLite::dbGetQuery(con, fields)


  RSQLite::dbDisconnect(con)


  data %<>% dplyr::left_join(dplyr::select(tickers, id, ticker), by = c("ticker_id" = "id")) %>%
    dplyr::left_join(dplyr::select(dates, id, date), by = c("date_id" = "id")) %>%
    dplyr::left_join(fields, by = c("field_id" = "id")) %>% dplyr::select(ticker, field = symbol, date, value) %>%
    dplyr::mutate(date = as.Date(date), value = as.numeric(value)) %>% dplyr::arrange(ticker, field, date)


  fields <- dplyr::left_join(dplyr::distinct(data, ticker, symbol = field),
                             dplyr::select(fields, instrument, book, symbol),
                             by = "symbol") %>%
    dplyr::select(ticker, instrument, book, symbol) %>% dplyr::arrange(ticker, instrument, book)

  methods::new("IndexMarket", tickers = data.table::as.data.table(dplyr::select(tickers, -id)),
               fields = data.table::as.data.table(fields), data = data.table::as.data.table(data),
               call = match.call())
}



### random source ####

#' Creates an IndexMarket object from index market data contained in a dataframe.
#'
#' @description Should index data be retrieved from other source(s) than Bloomberg,
#'   this function creates a IndexMarket data object from it in order to facilitates
#'   storage in a \href{https://bautheac.github.io/storethat/}{\pkg{storethat}} database.
#'
#'
#' @param data a dataframe containing index market data. Columns should include:
#'
#'   \itemize{
#'     \item{ticker: index Bloomberg tickers.}
#'     \item{field: field Bloomberg symbols.
#'       See \href{https://github.com/bautheac/BBGsymbols/}{\pkg{BBGsymbols}} for a list
#'       of accepted symbols.}
#'     \item{date: observation date (YYYY-MM-DD).}
#'     \item{value: observation value.}
#'   }
#'
#' @return An S4 object of class \linkS4class{IndexMarket}.
#'
#' @import BBGsymbols
#' @importFrom magrittr "%<>%"
#'
#' @export
random_index_market <- function(data){

  utils::data(list = c("fields"), package = "BBGsymbols", envir = environment())
  fields <- dplyr::filter(fields, instrument == "index", book == "market")

  stopifnot(is.data.frame(data), all.equal(names(data), c("ticker", "field", "date", "value")),
            unique(data$field) %in% fields$symbol)

  data %<>% dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::arrange(ticker, field, date) %>%
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01"), value = as.numeric(value))

  tickers <- dplyr::distinct(data, ticker)

  fields <- dplyr::distinct(data, ticker, field) %>%
    dplyr::left_join(dplyr::select(fields, instrument, book, symbol),
                     by = c("field" = "symbol")) %>%
    dplyr::select(ticker, instrument, book, symbol = field) %>%
    dplyr::arrange(ticker, instrument, book)

  methods::new("IndexMarket", tickers = data.table::as.data.table(tickers),
               fields = data.table::as.data.table(fields), data = data.table::as.data.table(data),
               call = match.call())
}



## info ####

### global ####

#' Retrieves index qualitative information.
#'
#'
#' @description Provided with a set of index Bloomberg tickers pulls qualitative
#'   information on the corresponding index(es) or
#'   retrieves it from an existing
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
#'
#'
#' @param source a scalar character vector. Specifies the data source for the query:
#'   "Bloomberg" or "storethat". Defaults to "Bloomberg".
#'
#' @param file a scalar chatacter vector. Optional parameter that specifies the
#'   target \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite
#'   database file to retrieve data from.
#'
#' @param tickers a chatacter vector. Specifies the futures active contract Bloomberg
#'   tickers to query data for.
#'
#' @param ... optional parameters to pass to the \link[Rblpapi]{bdp} function from the
#' \href{http://dirk.eddelbuettel.com/code/rblpapi.html}{\pkg{Rblpapi}} package used
#'   for the query (\code{options} parameter).
#'
#'
#' @return An S4 object of class \linkS4class{IndexInfo}.
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
#'
#'     BBG_info <- pull_index_info(source = "Bloomberg", tickers = c("NEIXCTA Index", "BARCCTA Index"))
#'
#'     storethat_info <- pull_index_info(source = "storethat", file = "~/storethat.sqlite",
#'       tickers = c("NEIXCTA Index", "BARCCTA Index"))
#'
#'     storethat_info <- pull_index_info(source = "storethat", tickers = c("NEIXCTA Index", "BARCCTA Index"))
#'
#'   }
#'
#'
#' @importFrom magrittr "%<>%"
#'
#' @export
pull_index_info <- function(source = "Bloomberg", tickers, file = NULL, ...){

  switch(source,
         Bloomberg = BBG_index_info(tickers, ...),
         storethat = storethat_index_info(file, tickers),
         stop("The parameters 'source' must be supplied as a scalar character vector:
              'Bloomberg' or 'storethat'.")
         )

}



### Bloomberg ####

#' Retrieves index qualitative information from Bloomberg.
#'
#'
#' @description Provided with a set of index Bloomberg tickers retrieves qualitative
#'   information on the corresponding index(es).
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
#' @return An S4 object of class \linkS4class{IndexInfo}.
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
#'
#'     BBG_index_info(tickers = c("NEIXCTA Index", "BARCCTA Index"))
#'
#'   }
#'
#'
#' @importFrom magrittr "%<>%"
BBG_index_info <- function(tickers, ...){


  utils::data(list = c("fields"), package = "BBGsymbols", envir = environment())


  fields <- dplyr::filter(fields, instrument == "index", book == "info")


  con <- tryCatch({
    Rblpapi::blpConnect()
  }, error = function(e)
    stop("Unable to connect Bloomberg. Please open a Bloomberg session on this terminal",
         call. = FALSE))


  query <- Rblpapi::bdp(securities = tickers, fields = dplyr::select(fields, symbol) %>% purrr::flatten_chr(),
                        con = con) %>% dplyr::mutate(ticker = row.names(.)) %>%
    dplyr::mutate_all(dplyr::funs(as.character)) %>% tidyr::gather(field, value, -ticker) %>%
    dplyr::mutate(field = forcats::as_factor(field)) %>% dplyr::arrange(ticker, field) %>%
    dplyr::mutate(field = as.character(field))


  fields <- dplyr::left_join(dplyr::distinct(query, ticker, symbol = field),
                             dplyr::select(fields, instrument, book, symbol),
                             by = "symbol") %>% dplyr::select(ticker, instrument, book, symbol)


  Rblpapi::blpDisconnect(con)


  methods::new("IndexInfo", info = tibble::as.tibble(query),
               fields = data.table::as.data.table(fields), call = match.call())
}


### storethat ####

#' Retrieves index qualitative information from a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}}
#'   SQLite database.
#'
#'
#' @description Provided with a set of equity Bloomberg tickers retrieves qualitative information
#'   on the corresponding index previously stored in a
#'   \href{https://github.com/bautheac/storethat/}{\pkg{storethat}} SQLite database.
#'
#'
#' @param file a scalar chatacter vector. Specifies the target \pkg{storethat} SQLite database file.
#'
#' @param tickers a chatacter vector. Specifies the futures active contract Bloomberg tickers to
#'   query data for.
#'
#'
#' @return An S4 object of class \linkS4class{IndexInfo}.
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
#'     storethat_index_info(tickers = c("NEIXCTA Index", "BARCCTA Index"))
#'
#'   }
storethat_index_info <- function(file = NULL, tickers){


  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file),
              stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite
           database file (ie. ~/storethat.sqlite)")


  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)


  query <- "SELECT id, ticker FROM tickers_index;"; query <- RSQLite::dbGetQuery(con, query)
  if (! all(tickers %in% query$ticker))
    stop("The parameter 'tickers' must be supplied as a character vector; one or more of '",
         paste(query$ticker, collapse = "', '"), "'")

  query <- paste0("SELECT id, ticker FROM tickers_index WHERE ticker IN ('", paste(tickers, collapse = "', '"), "');")
  tickers <- RSQLite::dbGetQuery(con, query)


  data <- paste0("SELECT ticker_id, field_id, value FROM data_index_info WHERE ticker_id IN (",
                 paste(tickers$id, collapse = ", "), ");")
  data <- RSQLite::dbGetQuery(con, data)

  fields <- paste0("SELECT id, instrument, book, symbol FROM support_fields WHERE
                   instrument = 'index' AND book = 'info' AND id IN (",
                   paste(unique(data$field_id), collapse = ", "), ");")
  fields <- RSQLite::dbGetQuery(con, fields)

  RSQLite::dbDisconnect(con)

  data <- dplyr::left_join(data, tickers, by = c("ticker_id" = "id")) %>%
    dplyr::left_join(fields, by = c("field_id" = "id")) %>% dplyr::select(ticker, symbol, value)

  fields <- dplyr::left_join(dplyr::distinct(data, ticker, symbol), fields, by = "symbol") %>%
    dplyr::select(ticker, instrument, book, symbol)

  methods::new("IndexInfo", info = tibble::as.tibble(data),
               fields = data.table::as.data.table(fields), call = match.call())
}








# Update ####

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
#'   snapshot for. Must be one of 'all', equity', 'index' or 'futures'.
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
storethat_update <- function(instrument = "all", book = "all", name = "all",
                             file = NULL, verbose = T){

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






