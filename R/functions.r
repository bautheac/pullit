#' Futures market historical data from Bloomberg
#'
#' @description Provided with a set of Bloomberg futures active contract tickers, term structure positions, roll parameters and a time period,
#'   queries Bloomberg for the corresponding futures historical market data.
#'
#' @param type A scalar character vector, 'term sructure' or 'aggregate'. 'term structure' returns individual futures chain data for a selected
#'   portion of the term structure (specify desired positions in \code{TS_positions}) while 'aggregate' returns aggregated data over the whole
#'   term structure for the corresponding names (specify desired names via corresponding tickers in \code{active_contract_tickers}).
#'   Defaults to 'term structure'.
#' @param active_contract_tickers A chatacter vector. Specifies the Bloomberg futures active contract tickers to query data for.
#'   Defaults to 'C A Comdty', the Bloomberg active contract ticker for the XCBT corn futures series.
#' @param start A scalar character vector. Specifies the starting date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-01-01'.
#' @param end A scalar character vector. Specifies the end date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-06-30'.
#' @param TS_positions An integer vector. Specifies the term structure positions to query data for.
#' Defaults to 1: front nearby contract for the corresponding futures series.
#' @param roll_type A scalar chatacter vector. Specifies roll type to use for term structure ticker construction.
#'   Must be one of 'A', 'B', 'D', 'F', 'N', 'O' or 'R'.
#'   Defaults to 'A' or 'With active future': rolls to the most actively traded contract in the futures series.
#' @param roll_days A scalar integer vector. Specifies the day the roll should be done.
#'   Refers to the day of the month (\code{roll_type} = 'F') or the number of days before a reference date
#'   (\code{roll_type} = 'D', \code{roll_type} = 'N', \code{roll_type} = 'O', \code{roll_type} = 'R').
#'   Works in tandem with \code{roll_months} below.
#'   Defaults to 0.
#' @param roll_months A scalar integer vector. Specifies the month the roll should be done.
#'   Refers to the number of months before a reference date (\code{roll_type} = 'D', \code{roll_type} = 'N', \code{roll_type} = 'O',
#'   \code{roll_type} = 'R'). Works in tandem with \code{roll_days} above.
#'   Defaults to 0.
#' @param roll_adjustment A scalar chatacter vector. Specifies roll adjustment method to use for term structure ticker construction.
#'   Must be one of 'D', 'N', 'R', or 'W'. Defaults to 'N' or 'None'.
#' @param verbose A logical scalar vector. Should progression messages be printed? Defaults to TRUE.
#' @param ... Optional parameters to pass to the \code{\link[Rblpapi]{bdh}} function from the \code{Rblpapi} package used
#'   for the query (\code{options} parameter).
#'
#' @return An S4 object of class \code{\linkS4class{FuturesTS}} (\code{type = 'term structure'}) or \code{\linkS4class{FuturesAggregate}}
#'   \code{type = 'aggregate'}). Slots include:
#'   \itemize{
#'     \item{\code{tickers}: a data.table showing the tickers for which data has been found.}
#'     \item{\code{fields}: a data.table showing the data fields for which data has been found.}
#'     \item{\code{data}: a data.table containing the data retrieved.}
#'     \item{\code{call}: a scalar character vector showing the original call to the constructor.}
#'   }
#'
#' @seealso The \code{\link[bbgsymbols]{fields}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg data fields used here.
#'
#' @examples
#' \dontrun{
#'   bbg_futures_market(type = 'term structure')
#'   bbg_futures_market(type = 'aggregate')
#' }
#'
#' @export
bbg_futures_market <- function(type = "term structure",
                               active_contract_tickers = "C A Comdty",
                               start = "2018-01-01",
                               end = "2018-06-30",
                               TS_positions = 1L,
                               roll_type = "A",
                               roll_days = 0L,
                               roll_months = 0L,
                               roll_adjustment = "N",
                               verbose = TRUE,
                               ...){

  if (! rlang::is_scalar_character(type) & type %in% c("term structure", "aggregate"))
    stop("The parameter 'type' must be supplied as a scalar character vector; one of 'term structure' or 'aggregate'")
  if (! is.character(active_contract_tickers)) stop("The parameter 'active_contract_tickers' must be supplied as a character vector of Bloomberg tickers")
  if (! is.integer(TS_positions)) stop("The parameter 'TS_positions' must be supplied as a vector of integers")
  if (! rlang::is_scalar_logical(verbose)) stop("The parameter 'verbose' must be supplied as a scalar logical vector")

  switch(type,
         `term structure` = bbg_futures_TS(active_contract_tickers = active_contract_tickers, start = start, end = end, TS_positions = TS_positions,
                                           roll_type = roll_type, roll_days = roll_days, roll_months = roll_months, roll_adjustment = roll_adjustment,
                                           verbose = verbose, ...),
         `aggregate` = bbg_futures_aggregate(active_contract_tickers = active_contract_tickers, start = start, end = end, verbose = verbose, ...)
  )
}


#' Futures term structure historical data from Bloomberg
#'
#' @description Provided with a set of Bloomberg futures active contract tickers, term structure positions, roll parameters and a time period,
#'   queries Bloomberg for the corresponding term structure historical data.
#'
#' @param active_contract_tickers A chatacter vector. Specifies the Bloomberg futures active contract tickers to query data for.
#'   Defaults to 'C A Comdty', the Bloomberg active contract ticker for the XCBT corn futures series.
#' @param start A scalar character vector. Specifies the starting date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-01-01'.
#' @param end A scalar character vector. Specifies the end date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-06-30'.
#' @param TS_positions An integer vector. Specifies the term structure positions to query data for.
#' Defaults to 1: front nearby contract for the corresponding futures series.
#' @param roll_type A scalar chatacter vector. Specifies roll type to use for term structure ticker construction.
#'   Must be one of 'A', 'B', 'D', 'F', 'N', 'O' or 'R'.
#'   Defaults to 'A' or 'With active future': rolls to the most actively traded contract in the futures series.
#' @param roll_days A scalar integer vector. Specifies the day the roll should be done.
#'   Refers to the day of the month (\code{roll_type} = 'F') or the number of days before a reference date
#'   (\code{roll_type} = 'D', \code{roll_type} = 'N', \code{roll_type} = 'O', \code{roll_type} = 'R').
#'   Works in tandem with \code{roll_months} below.
#'   Defaults to 0.
#' @param roll_months A scalar integer vector. Specifies the month the roll should be done.
#'   Refers to the number of months before a reference date (\code{roll_type} = 'D', \code{roll_type} = 'N', \code{roll_type} = 'O',
#'   \code{roll_type} = 'R'). Works in tandem with \code{roll_days} above.
#'   Defaults to 0.
#' @param roll_adjustment A scalar chatacter vector. Specifies roll adjustment method to use for term structure ticker construction.
#'   Must be one of 'D', 'N', 'R', or 'W'. Defaults to 'N' or 'None'.
#' @param verbose A logical scalar vector. Should progression messages be printed? Defaults to TRUE.
#' @param ... Optional parameters to pass to the \code{\link[Rblpapi]{bdh}} function from the \code{Rblpapi} package used
#'   for the query (\code{options} parameter).
#'
#' @return An S4 object of class \code{\linkS4class{FuturesTS}} with slots:
#'   \itemize{
#'     \item{\code{tickers}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'         \item{\code{ticker}: futures term structure Bloomberg tickers for which data has been found.}
#'         \item{\code{TS position}: corresponding futures term structure position.}
#'         \item{\code{roll type}: corresponding roll type (by name).}
#'         \item{\code{roll days}: corresponding day(s) offset.}
#'         \item{\code{roll months}: corresponding month(s) offset.}
#'         \item{\code{roll adjustment}: corresponding roll adjustment (by name).}
#'       }
#'     }
#'     \item{\code{fields}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'         \item{\code{TS position}: corresponding futures chain term structure positions for which data has been found.}
#'         \item{\code{field}: futures Bloomberg market data fields for which data has been found.}
#'       }
#'     }
#'     \item{\code{data}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'         \item{\code{ticker}: futures term structure Bloomberg tickers for which data has been found.}
#'         \item{\code{field}: futures Bloomberg market data fields for which data has been found.}
#'         \item{\code{date}: observation date.}
#'         \item{\code{value}: corresponding value.}
#'       }
#'     }
#'     \item{\code{call}: a scalar character vector showing the original call to the constructor.}
#'   }
#'
#' @seealso The \code{\link[bbgsymbols]{fields}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg fields used here.
#'
#' @examples
#' \dontrun{bbg_futures_TS()}
#'
#' @import bbgsymbols
#' @importFrom magrittr "%>%" "%<>%"
#'
bbg_futures_TS <- function(active_contract_tickers = "C A Comdty",
                           start = "2018-01-01",
                           end = "2018-06-30",
                           TS_positions = 1L,
                           roll_type = "A",
                           roll_days = 0L,
                           roll_months = 0L,
                           roll_adjustment = "N",
                           verbose = TRUE,
                           ...){

  data(list = c("tickers_futures", "fields", "rolls"), package = "bbgsymbols", envir = environment())

  if (! is.character(active_contract_tickers)) stop("The parameter 'active_contract_tickers' must be supplied as a character vector of Bloomberg tickers")
  if (! is.integer(TS_positions)) stop("The parameter 'TS_positions' must be supplied as a vector of integers")
  if (! rlang::is_scalar_logical(verbose)) stop("The parameter 'verbose' must be supplied as a scalar logical vector")

  data <- lapply(active_contract_tickers, function(y){
    tickers <- sapply(TS_positions, function(x) futures_ticker(y, TS_position = x, roll_type, roll_days, roll_months, roll_adjustment))
    data <- bbg_pull_historical_market(tickers, fields = dplyr::filter(fields, instrument == "futures", type == "market") %>% dplyr::select(symbol) %>% purrr::flatten_chr(),
                                start, end, ...)

    data <- if (! is.data.frame(data)) {
      lapply(names(data), function(x) {
      if (nrow(data[[x]]) > 0L) {
        data[[x]]$ticker <- x
        data[[x]]
      } else {
        NULL
      }
    }) %>%
      data.table::rbindlist(use.names = TRUE)
      }
    else {
      if(nrow(data) < 1L) NULL
      else {
        data$ticker <- tickers
        data
        }
    }
    data$`active contract ticker` <- y
    if (verbose) message(y); data
  }) %>%
    data.table::rbindlist(use.names = TRUE)

  data %<>%
    tidyr::gather(field, value, -c(`active contract ticker`, ticker, date)) %>%
    dplyr::select(`active contract ticker`, ticker, field, date, value) %>%
    dplyr::arrange(`active contract ticker`, ticker, field, date) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01"))

  if (nrow(data) == 0L) warning("No term structure data found.")

  tickers <- dplyr::distinct(data, `active contract ticker`, ticker) %>%
    dplyr::mutate(`TS position` = stringr::str_extract(ticker, pattern = "(?<=^.{0,10})\\d(?=\\s[A-Z]:)"),
                  `roll type` = stringr::str_extract(ticker, pattern = "(?<= )[A-Z](?=:)"),
                  `roll days` = stringr::str_extract(ticker, pattern = "(?<=:)\\d{2}(?=_)") %>% as.numeric(),
                  `roll months` = stringr::str_extract(ticker, pattern = "(?<=_)\\d(?=_)") %>% as.numeric(),
                  `roll adjustment` = stringr::str_extract(ticker, pattern = "(?<=_)[A-Z](?= )")) %>%
    dplyr::left_join(dplyr::filter(rolls, roll == "type") %>% dplyr::select(symbol, name), by = c("roll type" = "symbol")) %>%
    dplyr::select(`active contract ticker`, ticker, `TS position`, `roll type` = name, `roll days`, `roll months`, `roll adjustment`) %>%
    dplyr::left_join(dplyr::filter(rolls, roll == "adjustment") %>% dplyr::select(symbol, name), by = c("roll adjustment" = "symbol")) %>%
    dplyr::select(`active contract ticker`, `TS position`, `roll type`, `roll days`, `roll months`, `roll adjustment` = name) %>%
    dplyr::left_join(tickers_futures, by = c("active contract ticker" = "ticker"))

  methods::new("FuturesTS",
               tickers = tibble::as.tibble(tickers),
               fields = data.table::as.data.table(dplyr::distinct(data, `active contract ticker`, ticker, field)),
               data = data.table::as.data.table(data),
               call = match.call()
  )
}



#' Futures aggregate historical data from Bloomberg
#'
#' @description Provided with a set of Bloomberg futures active contract tickers and a time period,
#'   queries Bloomberg for the corresponding futures historical aggregate data. For each individual field,
#'   futures aggregate data represents the aggregation of the corresponding field values over all the
#'   corresponding term structure contracts.
#'
#' @param active_contract_tickers A chatacter vector. Specifies the Bloomberg futures active contract tickers to query data for.
#'   Defaults to 'C A Comdty', the Bloomberg active contract ticker for the XCBT corn futures series.
#' @param start A scalar character vector. Specifies the starting date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-01-01'.
#' @param end A scalar character vector. Specifies the end date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-06-30'.
#' @param verbose A logical scalar vector. Should progression messages be printed? Defaults to TRUE.
#' @param ... Optional parameters to pass to the \code{\link[Rblpapi]{bdh}} function from the \code{Rblpapi} package used
#'   for the query (\code{options} parameter).
#'
#' @return An S4 object of class \code{\linkS4class{FuturesAggregate}} with slots:
#'   \itemize{
#'     \item{\code{tickers}: a character vector. Active contract Bloomberg tickers for which data has been found.}
#'     \item{\code{fields}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'         \item{\code{field}: futures Bloomberg aggregate data fields for which data has been found.}
#'       }
#'     }
#'     \item{\code{data}: a tibble. Columns inlude:
#'       \itemize{
#'         \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'         \item{\code{field}: Bloomberg aggregate data fields for which data has been found.}
#'         \item{\code{date}: observation date.}
#'         \item{\code{value}: corresponding value.}
#'       }
#'     }
#'     \item{\code{call}: a scalar character vector showing the original call to the constructor.}
#'   }
#'
#' @seealso The \code{\link[bbgsymbols]{fields}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg fields used here.
#'
#' @examples
#' \dontrun{bbg_futures_aggregate()}
#'
#' @import bbgsymbols
#' @importFrom magrittr "%>%" "%<>%"
#'
#' @export
bbg_futures_aggregate <- function(active_contract_tickers = "C A Comdty",
                                  start = "2018-01-01",
                                  end = "2018-06-30",
                                  verbose = TRUE,
                                  ...){

  call <- deparse(match.call())
  data(list = c("tickers_futures", "fields"), package = "bbgsymbols", envir = environment())

  if (! is.character(active_contract_tickers)) stop("The parameter 'active_contract_tickers' must be supplied as a character vector of Bloomberg tickers")
  if (! rlang::is_scalar_logical(verbose)) stop("The parameter 'verbose' must be supplied as a scalar logical vector")

  data <- lapply(active_contract_tickers, function(x) {
    data <- bbg_pull_historical_market(x, fields = dplyr::filter(fields, instrument == "futures", type == "aggregate") %>%
                                         dplyr::select(symbol) %>% purrr::flatten_chr(), start, end, ...) %>%
                   dplyr::mutate(`active contract ticker` = x)
    if (verbose) message(x); data
    }) %>%
    data.table::rbindlist(use.names = TRUE)

  data %<>%
    tidyr::gather(field, value, -c(`active contract ticker`, date)) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::arrange(`active contract ticker`, field, date) %>%
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01")) %>%
    dplyr::select(`active contract ticker`, field, date, value)

  if (nrow(data) == 0L) warning("No aggregate data found.")

  methods::new("FuturesAggregate",
               tickers = dplyr::left_join(dplyr::distinct(data, `active contract ticker`), tickers_futures, by = c("active contract ticker", "ticker")),
               fields = data.table::as.data.table(dplyr::distinct(data, `active contract ticker`, field)),
               data = data.table::as.data.table(data),
               call = match.call()
  )
}







#' Futures CFTC reports historical data from Bloomberg
#'
#' @description Provided with a set of Bloomberg futures active contract tickers and a time period,
#'   queries Bloomberg for the corresponding futures CFTC reports data.
#'
#' @param active_contract_tickers A chatacter vector. Specifies the Bloomberg futures active contract tickers to query data for.
#'   Defaults to 'C A Comdty', the Bloomberg active contract ticker for the XCBT corn futures series.
#' @param start A scalar character vector. Specifies the starting date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-01-01'.
#' @param end A scalar character vector. Specifies the end date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-06-30'.
#' @param verbose A logical scalar vector. Should progression messages be printed? Defaults to TRUE.
#' @param ... Optional parameters to pass to the \code{\link[Rblpapi]{bdh}} function from the \code{Rblpapi} package used
#'   for the query (\code{options} parameter).
#'
#' @return An S4 object of class \code{\linkS4class{FuturesCFTC}} with slots:
#'   \itemize{
#'     \item{\code{tickers}: a data.table. Active contract and corresponding position Bloomberg tickers for which data has been found. Columns include:
#'       \itemize{
#'         \item{active contract ticker: futures series active contract Bloomberg ticker.}
#'         \item{position ticker: CFTC position Bloomberg ticker.}
#'       }
#'     }
#'     \item{\code{fields}: a data.table. Columns include:
#'       \itemize{
#'         \item{\code{format}: report formats ('legacy', 'disaggregated', 'supplemental' or 'traders in financial futures')
#'           for which data has been found.}
#'         \item{\code{underlying}: underlying instruments ('futures only' or 'futures & options') for which data has been found..}
#'         \item{\code{unit}: counting units (number of 'contracts', 'traders' or 'total') for which data has been found.}
#'         \item{\code{participant}: trader classifications for which data has been found. Report specific:
#'           \itemize{
#'             \item{legacy: 'commercial', 'non-commercial', 'non-reportable', 'total'.}
#'             \item{disaggregated: 'managed money', 'producer/merchant/processor/user', 'swap dealers', 'other reportables'.}
#'             \item{supplemental: 'commercial - non-CIT', 'non-commercial - non-CIT', 'index traders - non-CIT', 'index traders'.}
#'             \item{traders in financial futures: 'asset manager/institutional', 'dealer/intermediary', 'leveraged funds', 'other reportables'.}
#'           }
#'         }
#'         \item{\code{position}: trader positions for which data has been found. Participant specific.
#'           \itemize{
#'             \item{commercial: 'long', 'short', 'net'.}
#'             \item{non-commercial: 'long', 'short', 'net', 'spreading'.}
#'             \item{non-reportable: 'long', 'short', 'net'.}
#'             \item{total: 'long', 'short', 'net', 'open interest', 'total'.}
#'             \item{managed money: 'long', 'short', 'net', 'spreading'.}
#'             \item{producer/merchant/processor/user: 'long', 'short', 'net'.}
#'             \item{swap dealers: 'long', 'short', 'net', 'spreading'.}
#'             \item{other reportables: 'long', 'short', 'net', 'spreading'.}
#'             \item{commercial - non-CIT: 'long', 'short', 'net'.}
#'             \item{non-commercial - non-CIT: 'long', 'short', 'net', 'spreading'.}
#'             \item{index traders - non-CIT: 'long', 'short'.}
#'             \item{index traders: 'long', 'short', 'net'.}
#'             \item{asset manager/institutional: 'long', 'short', 'net', 'spreading'.}
#'             \item{dealer/intermediary: 'long', 'short', 'net', 'spreading'.}
#'             \item{leveraged funds: 'long', 'short', 'net', 'spreading'.}
#'             \item{other reportables: 'long', 'short', 'net', 'spreading'.}
#'          }
#'        }
#'        \item{\code{position ticker}: corresponding CFTC position Bloomberg ticker.}
#'      }
#'    }
#'     \item{\code{data}: a tibble. Columns inlude:
#'       \itemize{
#'         \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'         \item{\code{position ticker}: CFTC position Bloomberg tickers for which data has been found.}
#'         \item{\code{date}: observation date.}
#'         \item{\code{value}: observed value}
#'       }
#'     }
#'     \item{\code{call}: a scalar character vector showing the original call to the constructor.}
#'  }
#'
#' @seealso The \code{\link[bbgsymbols]{tickers_cftc}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg fields used here.
#'
#' @examples
#' \dontrun{bbg_futures_CFTC()}
#'
#' @import bbgsymbols
#' @importFrom magrittr "%>%" "%<>%"
#'
#' @export
bbg_futures_CFTC <- function(active_contract_tickers = "C A Comdty",
                             start = "2018-01-01",
                             end = "2018-06-30",
                             verbose = TRUE,
                             ...){

  data(list = c("tickers_cftc", "tickers_futures"), package = "bbgsymbols", envir = environment())

  if (! is.character(active_contract_tickers)) stop("The parameter 'active_contract_tickers' must be supplied as a character vector of Bloomberg tickers")
  if (! rlang::is_scalar_logical(verbose)) stop("The parameter 'verbose' must be supplied as a scalar logical vector")

  data <- lapply(active_contract_tickers, function(x) {
    tickers <- dplyr::filter(tickers_cftc, `active contract ticker` == x) %>% dplyr::select(ticker) %>% purrr::flatten_chr()
    if (NROW(tickers) == 0L) stop(paste0("No CFTC data for ", x, "."))
    data <- bbg_pull_historical_market(tickers, fields = "PX_LAST", start, end, ...)

    data <- lapply(names(data), function(y) {
      if(nrow(data[[y]]) == 0L) NULL
      else dplyr::mutate(data[[y]], ticker = y)
    }) %>%
      data.table::rbindlist(use.names = TRUE)

    data %<>%
      dplyr::mutate(`active contract ticker` = x) %>%
      dplyr::left_join(dplyr::select(tickers_cftc, format, underlying, `unit` = unit, participant, position, ticker), by = "ticker")
    if (verbose) message(x); data
  }) %>%
    data.table::rbindlist(use.names = TRUE) %>%
    dplyr::select(`active contract ticker`, ticker, format, underlying, `unit`, participant, position, date, value = PX_LAST) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01"))

  if (nrow(data) == 0L) warning("No CFTC data found.")

  tickers <- dplyr::distinct(data, `active contract ticker`) %>%
    dplyr::left_join(tickers_futures, by = c("active contract ticker" = "ticker"))

  methods::new("FuturesCFTC",
               tickers = tibble::as.tibble(tickers),
               fields = data.table::as.data.table(dplyr::distinct(data, `active contract ticker`, format, underlying, `unit`, participant, position)),
               data = data.table::as.data.table(dplyr::select(data, `active contract ticker`, `position ticker` = ticker, date, value)),
               call = match.call()
  )
}


#' Equity historical market data from Bloomberg
#'
#' @description Provided with a set of Bloomberg equity tickers and a time period,
#'   queries Bloomberg for the corresponding equity historical market data.
#'
#' @param tickers A chatacter vector. Specifies the Bloomberg equity tickers to query data for.
#'   Defaults to 'NEM US Equity', the Bloomberg equity ticker for the Newmont Minning Corporation.
#' @param start A scalar character vector. Specifies the starting date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-01-01'.
#' @param end A scalar character vector. Specifies the end date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-06-30'.
#' @param verbose A logical scalar vector. Should progression messages be printed? Defaults to TRUE.
#' @param ... Optional parameters to pass to the \code{\link[Rblpapi]{bdh}} function from the \code{Rblpapi} package used
#'   for the query (\code{options} parameter).
#'
#' @return An S4 object of class \code{\linkS4class{EquityMarket}} with slots:
#'   \itemize{
#'     \item{\code{tickers}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'         \item{\code{field}: equity Bloomberg company information data fields. Descriptive elements on the corporation identified.}
#'         \item{\code{name}: corresponding information data field names.}
#'         \item{\code{value}: corresponding information data field value.}#'
#'       }
#'     }
#'     \item{\code{fields}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'         \item{\code{field}: equity Bloomberg market data fields for which data has been found.}
#'       }
#'     }
#'     \item{\code{data}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'         \item{\code{field}: equity Bloomberg market data fields for which data has been found.}
#'         \item{\code{date}: observation date.}
#'         \item{\code{value}: corresponding value.}
#'       }
#'     }
#'     \item{\code{call}: a scalar character vector showing the original call to the constructor.}
#'   }
#'
#' @seealso The \code{\link[bbgsymbols]{fields}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg fields used here.
#'
#' @examples
#' \dontrun{bbg_equity_market()}
#'
#' @import bbgsymbols
#' @importFrom magrittr "%>%" "%<>%"
#'
#' @export

bbg_equity_market <- function(tickers = "NEM US Equity",
                              start = "2018-01-01",
                              end = "2018-06-30",
                              verbose = TRUE,
                              ...){

  data(list = c("fields"), package = "bbgsymbols", envir = environment())

  if (! is.character(tickers)) stop("The parameter 'tickers' must be supplied as a character vector of Bloomberg tickers")
  if (! rlang::is_scalar_logical(verbose)) stop("The parameter 'verbose' must be supplied as a scalar logical vector")

  data <- lapply(tickers, function(x) {
    data <- bbg_pull_historical_market(x, fields = dplyr::filter(fields, instrument == "equity", type == "market") %>% dplyr::select(symbol) %>% purrr::flatten_chr(),
                               start, end, ...) %>%
      dplyr::mutate(ticker = x)
    if (verbose) message(x); data
  }) %>%
    data.table::rbindlist(use.names = TRUE)

  data %<>%
    tidyr::gather(field, value, -c(ticker, date)) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::select(ticker, field, date, value) %>%
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01")) %>%
    dplyr::arrange(ticker, field, date)

  if (nrow(data) == 0L) warning("No market data found.")

  tickers <- Rblpapi::bdp(dplyr::distinct(data, ticker) %>% purrr::flatten_chr(),
                          dplyr::filter(fields, instrument == "equity", type == "info", ! stringr::str_detect(symbol, "^GICS_(?!.*SUB)")) %>%
                            dplyr::select(symbol) %>%
                            purrr::flatten_chr()) %>%
    dplyr::mutate(ticker = row.names(.)) %>%
    tidyr::gather(field, value, -ticker) %>%
    dplyr::left_join(dplyr::filter(fields, instrument == "equity", type == "info") %>% dplyr::select(symbol, name), by = c("field" = "symbol"))  %>%
    dplyr::mutate(name = forcats::as_factor(name)) %>%
    dplyr::select(ticker, field = name, value) %>%
    tidyr::spread(field, value)

  methods::new("EquityMarket",
      tickers = tibble::as.tibble(tickers),
      fields = data.table::as.data.table(dplyr::distinct(data, ticker, field)),
      data = data.table::as.data.table(data),
      call = match.call()
  )
}




#' Equity historical book data from Bloomberg
#'
#' @description Retrieve equity historical book data from Bloomberg. Books include 'key stats', 'income statement', 'balance sheet',
#'   'cash flow statement' and 'ratios' with the corresponding data fields retrieved from the 'fields' dataset of the \code{bbgsymbols}
#'   package.
#'
#' @param book A scalar chatacter vector, 'key stats', 'income statement', 'balance sheet', 'cash flow statement' or 'ratios'.
#'   Defaults to 'key stats' which returns historical data for a set of key financial indicators; a quick dynamic financial
#'   snapshot of the selected names.
#' @param tickers A chatacter vector. Specifies the Bloomberg equity tickers to query data for.
#'   Defaults to 'NEM US Equity', the Bloomberg equity ticker for the Newmont Minning Corporation.
#' @param start A scalar character vector. Specifies the starting date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-01-01'.
#' @param end A scalar character vector. Specifies the end date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-06-30'.
#' @param verbose A logical scalar vector. Should progression messages be printed? Defaults to TRUE.
#' @param ... Optional parameters to pass to the \code{\link[Rblpapi]{bdh}} function from the \code{Rblpapi} package used
#'   for the query (\code{options} parameter).
#'
#' @return An S4 object of class \code{\linkS4class{EquityKS}} (\code{book = 'key stats'}), \code{\linkS4class{EquityIS}}
#'   (\code{book = 'income statement'}), \code{\linkS4class{EquityBS}} (\code{book = 'balance sheet'}),
#'    \code{\linkS4class{EquityCF}} (\code{book = 'cash flow statement'}), \code{\linkS4class{EquityRatios}}
#'    (\code{book = 'ratios'}). Slots include:
#'   \itemize{
#'     \item{\code{tickers}: a tibble. Equity Bloomberg tickers for which data has been found and corresponding descriptive elements.}
#'     \item{\code{fields}: a data.table. Data fields for which data has been found.}
#'     \item{\code{data}: a data.table The data retrieved in the query.}
#'     \item{\code{call}: a scalar character vector. The original call to the constructor.}
#'   }
#'
#' @seealso The \code{\link[bbgsymbols]{fields}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg fields used here.
#'
#' @examples
#' \dontrun{
#'   bbg_equity_books(book = 'key stats')
#'   bbg_equity_books(book = 'income statement')
#'   bbg_equity_books(book = 'balance sheet')
#'   bbg_equity_books(book = 'cash flow statement')
#'   bbg_equity_books(book = 'ratios')
#' }
#'
#' @import bbgsymbols
#' @importFrom magrittr "%>%"
#'
#' @export
bbg_equity_books <- function(book = "key stats",
                             tickers = "NEM US Equity",
                             start = "2018-01-01",
                             end = "2018-06-30",
                             verbose = TRUE,
                             ...){

  data(list = c("fields"), package = "bbgsymbols", envir = environment())

  if (! rlang::is_scalar_character(book) & book %in% c("key stats", "income statement", "balance sheet", "cash flow statement", "ratios"))
    stop("The parameter 'book' must be supplied as a scalar character vector, one of 'key stats', 'income statement', 'balance sheet', 'cash flow statement' or 'ratios'")
  if (! is.character(tickers)) stop("The parameter 'tickers' must be supplied as a character vector of Bloomberg equity tickers")
  if (! rlang::is_scalar_logical(verbose)) stop("The parameter 'verbose' must be supplied as a scalar logical vector (TRUE or FALSE)")
  if (! "periodicitySelection" %in% names(list(...))) utils::modifyList(list(...), list(periodicitySelection = "MONTHLY"))

  symbols <- dplyr::filter(fields, instrument == "equity", type == book)

  data <- lapply(dplyr::distinct(symbols, symbol) %>% purrr::flatten_chr(), function(x) {
    data <- bbg_pull_historical_books(tickers = tickers, field = x, start, end, ...)
    if (verbose) message(x); data
  }) %>%
    data.table::rbindlist(use.names = TRUE) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::arrange(ticker, field, date) %>%
    dplyr::select(ticker, field, date, value)

  if (nrow(data) == 0L) warning(paste("No", book, "data found.", sep = " "))

  tickers <- Rblpapi::bdp(dplyr::distinct(data, ticker) %>% purrr::flatten_chr(),
                          dplyr::filter(fields, instrument == "equity", type == "info", ! stringr::str_detect(symbol, "^GICS_(?!.*SUB)")) %>%
                            dplyr::select(symbol) %>%
                            purrr::flatten_chr()) %>%
    dplyr::mutate(ticker = row.names(.)) %>%
    tidyr::gather(field, value, -ticker) %>%
    dplyr::left_join(dplyr::filter(fields, instrument == "equity", type == "info") %>% dplyr::select(symbol, name), by = c("field" = "symbol"))  %>%
    dplyr::mutate(name = forcats::as_factor(name)) %>%
    dplyr::select(ticker, field = name, value) %>%
    tidyr::spread(field, value)

  fields <- dplyr::left_join(dplyr::distinct(data, ticker, field), symbols, by = c("field" = "symbol"))

  fields <- switch(book,
                   `key stats` = data.table::as.data.table(dplyr::select(fields, ticker, field = name)),
                   `income statement` = data.table::as.data.table(dplyr::select(fields, ticker, field = name)),
                   `balance sheet` = data.table::as.data.table(dplyr::select(fields, ticker, section, subsection, field = name)),
                   `cash flow statement` = data.table::as.data.table(dplyr::select(fields, ticker, section, field = name)),
                   `ratios` = data.table::as.data.table(dplyr::select(data, ticker, section, subsection, field = name))

  )

  methods::new(dplyr::case_when(book == "key stats" ~ "EquityKS", book == "balance sheet" ~ "EquityBS",
                                book == "cash flow statement" ~ "EquityCF", book == "income statement" ~ "EquityIS",
                                book == "ratios" ~ "EquityRatios"),
               tickers = tibble::as.tibble(tickers),
               fields = fields,
               data = data.table::as.data.table(data),
               call = match.call()
  )
}











#' Futures historical market data from a \code{storethat} SQLite database
#'
#' @description Provided with a set of Bloomberg futures active contract tickers, term structure positions, roll parameters and a time period,
#'   retrieves the corresponding futures market historical data previously stored in a \code{storethat} SQLite database.
#'
#' @param file A scalar chatacter vector. Specifies the target \code{storethat} SQLite database file.
#' @param type A scalar character vector, 'term sructure' or 'aggregate'. 'term structure' returns individual futures chain data for a selected
#'   portion of the term structure (specify desired positions in \code{TS_positions}) while 'aggregate' returns aggregated data over the whole
#'   term structure for the corresponding names (specify desired names via corresponding tickers in \code{active_contract_tickers}).
#'   Defaults to 'term structure'.
#' @param active_contract_tickers A chatacter vector. Specifies the Bloomberg futures active contract tickers to query data for.
#'   Defaults to 'C A Comdty', the Bloomberg active contract ticker for the XCBT corn futures series.
#' @param start A scalar character vector. Specifies the starting date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-01-01'.
#' @param end A scalar character vector. Specifies the end date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-06-30'.
#' @param TS_positions An integer vector. Specifies the term structure positions to query data for.
#' Defaults to 1: front nearby contract for the corresponding futures series.
#' @param roll_type A scalar chatacter vector. Specifies roll type to use for term structure ticker construction.
#'   Must be one of 'A', 'B', 'D', 'F', 'N', 'O' or 'R'.
#'   Defaults to 'A' or 'With active future': rolls to the most actively traded contract in the futures series.
#' @param roll_days A scalar integer vector. Specifies the day the roll should be done.
#'   Refers to the day of the month (\code{roll_type} = 'F') or the number of days before a reference date
#'   (\code{roll_type} = 'D', \code{roll_type} = 'N', \code{roll_type} = 'O', \code{roll_type} = 'R').
#'   Works in tandem with `roll_months` below.
#'   Defaults to 0.
#' @param roll_months A scalar integer vector. Specifies the month the roll should be done.
#'   Refers to the number of months before a reference date (\code{roll_type} = 'D', \code{roll_type} = 'N', \code{roll_type} = 'O',
#'   \code{roll_type} = 'R'). Works in tandem with `roll_days` above.
#'   Defaults to 0.
#' @param roll_adjustment A scalar chatacter vector. Specifies roll adjustment method to use for term structure ticker construction.
#'   Must be one of 'D', 'N', 'R', or 'W'. Defaults to 'N' or 'None'.
#' @param verbose A logical scalar vector. Should progression messages be printed? Defaults to TRUE.
#' @param ... optional extra parameters for grouping:
#'   \itemize{
#'     \item{\code{asset_class}: a character vector. See column 'asset class' of the \code{tickers_futures}
#'       dataset in the  \code{bbgsymbols}.}
#'     \item{\code{sector}: a character vector. See column 'sector' of the \code{\link[bbgsymbols]{tickers_futures}} dataset in the  \code{bbgsymbols}.}
#'     \item{\code{subsector}: a character vector. See column 'subsector' of the \code{\link[bbgsymbols]{tickers_futures}} dataset in the  \code{bbgsymbols}.}
#'     \item{\code{exchange}: a character vector. See column 'symbol' of the \code{\link[fewISOs]{exchanges}} dataset in the \code{fewISOs}.}
#'     \item{\code{currency}: a character vector. See column 'symbol' of the \code{\link[fewISOs]{currencies}} dataset in the \code{fewISOs}.}
#'   }
#'
#' @return An S4 object of class \code{\linkS4class{FuturesTS}} (\code{type = 'term structure'}) or \code{\linkS4class{FuturesAggregate}}
#'   \code{type = 'aggregate'}). Slots include:
#'   \itemize{
#'     \item{\code{tickers}: a data.table showing the tickers for which data has been found.}
#'     \item{\code{fields}: a data.table showing the data fields for which data has been found.}
#'     \item{\code{data}: a data.table containing the data retrieved.}
#'     \item{\code{call}: a scalar character vector showing the original call to the constructor.}
#'   }
#'
#' @seealso The \code{\link[bbgsymbols]{fields}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg data fields used here.
#'
#' @examples
#' \dontrun{
#'   bbg_futures_market(type = 'term structure')
#'   bbg_futures_market(type = 'aggregate')
#' }
#'
#' @export
storethat_futures_market <- function(file = NULL,
                                     type = "term structure",
                                     active_contract_tickers = "C A Comdty",
                                     start = "2018-01-01",
                                     end = "2018-06-30",
                                     TS_positions = 1L,
                                     roll_type = "A",
                                     roll_days = 0L,
                                     roll_months = 0L,
                                     roll_adjustment = "N",
                                     verbose = TRUE,
                                     ...){

  if (! rlang::is_scalar_character(type) & type %in% c("term structure", "aggregate"))
    stop("The parameter 'type' must be supplied as a scalar character vector; one of 'term structure' or 'aggregate'")
  if (! is.character(active_contract_tickers)) stop("The parameter 'active_contract_tickers' must be supplied as a character vector of Bloomberg tickers")
  if (! is.integer(TS_positions)) stop("The parameter 'TS_positions' must be supplied as a vector of integers")
  if (! rlang::is_scalar_logical(verbose)) stop("The parameter 'verbose' must be supplied as a scalar logical vector")

  switch(type,
         `term structure` = storethat_futures_TS(active_contract_tickers = active_contract_tickers, start = start, end = end, TS_positions = TS_positions,
                                                 roll_type = roll_type, roll_days = roll_days, roll_months = roll_months, roll_adjustment = roll_adjustment,
                                                 verbose = verbose, file = file),
         `aggregate` = storethat_futures_aggregate(active_contract_tickers = active_contract_tickers, start = start, end = end, verbose = verbose, file = file)
  )
}




#' Futures term structure historical data from a \code{storethat} SQLite database
#'
#' @description Provided with a set of Bloomberg futures active contract tickers, term structure positions, roll parameters and a time period,
#'   retrieves the corresponding term structure historical data previously stored in a \code{storethat} SQLite database.
#'
#' @param file A scalar chatacter vector. Specifies the target \code{storethat} SQLite database file.
#' @param active_contract_tickers A chatacter vector. Specifies the Bloomberg futures active contract tickers to query data for.
#'   Defaults to 'C A Comdty', the Bloomberg active contract ticker for the XCBT corn futures series.
#' @param start A scalar character vector. Specifies the starting date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-01-01'.
#' @param end A scalar character vector. Specifies the end date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-06-30'.
#' @param TS_positions An integer vector. Specifies the term structure positions to query data for.
#' Defaults to 1: front nearby contract for the corresponding futures series.
#' @param roll_type A scalar chatacter vector. Specifies roll type to use for term structure ticker construction.
#'   Must be one of 'A', 'B', 'D', 'F', 'N', 'O' or 'R'.
#'   Defaults to 'A' or 'With active future': rolls to the most actively traded contract in the futures series.
#' @param roll_days A scalar integer vector. Specifies the day the roll should be done.
#'   Refers to the day of the month (\code{roll_type} = 'F') or the number of days before a reference date
#'   (\code{roll_type} = 'D', \code{roll_type} = 'N', \code{roll_type} = 'O', \code{roll_type} = 'R').
#'   Works in tandem with `roll_months` below.
#'   Defaults to 0.
#' @param roll_months A scalar integer vector. Specifies the month the roll should be done.
#'   Refers to the number of months before a reference date (\code{roll_type} = 'D', \code{roll_type} = 'N', \code{roll_type} = 'O',
#'   \code{roll_type} = 'R'). Works in tandem with `roll_days` above.
#'   Defaults to 0.
#' @param roll_adjustment A scalar chatacter vector. Specifies roll adjustment method to use for term structure ticker construction.
#'   Must be one of 'D', 'N', 'R', or 'W'. Defaults to 'N' or 'None'.
#' @param verbose A logical scalar vector. Should progression messages be printed? Defaults to TRUE.
#' @param ... optional extra parameters for grouping:
#'   \itemize{
#'     \item{\code{asset_class}: a character vector. See column 'asset class' of the \code{\link[bbgsymbols]{tickers_futures}} dataset in the  \code{bbgsymbols}.}
#'     \item{\code{sector}: a character vector. See column 'sector' of the \code{\link[bbgsymbols]{tickers_futures}} dataset in the  \code{bbgsymbols}.}
#'     \item{\code{subsector}: a character vector. See column 'subsector' of the \code{\link[bbgsymbols]{tickers_futures}} dataset in the  \code{bbgsymbols}.}
#'     \item{\code{exchange}: a character vector. See column 'symbol' of the \code{\link[fewISOs]{exchanges}} dataset in the \code{fewISOs}.}
#'     \item{\code{currency}: a character vector. See column 'symbol' of the \code{\link[fewISOs]{currencies}} dataset in the \code{fewISOs}.}
#'   }
#'
#'
#' @return An S4 object of class \code{\linkS4class{FuturesTS}} with slots:
#'   \itemize{
#'     \item{\code{tickers}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'         \item{\code{ticker}: futures term structure Bloomberg tickers for which data has been found.}
#'         \item{\code{TS position}: corresponding futures term structure position.}
#'         \item{\code{roll type}: corresponding roll type (by name).}
#'         \item{\code{roll days}: corresponding day(s) offset.}
#'         \item{\code{roll months}: corresponding month(s) offset.}
#'         \item{\code{roll adjustment}: corresponding roll adjustment (by name).}
#'       }
#'     }
#'     \item{\code{fields}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'         \item{\code{TS position}: corresponding futures chain term structure positions for which data has been found.}
#'         \item{\code{field}: futures Bloomberg market data fields for which data has been found.}
#'       }
#'     }
#'     \item{\code{data}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'         \item{\code{TS position}: futures chain term structure positions for which data has been found.}
#'         \item{\code{field}: futures Bloomberg market data fields for which data has been found.}
#'         \item{\code{date}: observation date.}
#'         \item{\code{value}: corresponding value.}
#'       }
#'     }
#'     \item{\code{call}: a scalar character vector showing the original call to the constructor.}
#'   }
#'
#' @seealso The \code{\link[bbgsymbols]{fields}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg fields used here.
#'
#' @examples
#' \dontrun{bbg_futures_TS()}
#'
#' @import bbgsymbols
#' @importFrom magrittr "%>%" "%<>%"
#'
storethat_futures_TS <- function(file = NULL,
                                 active_contract_tickers = "C A Comdty",
                                 start = "2018-01-01",
                                 end = "2018-06-30",
                                 TS_positions = 1L,
                                 roll_type = "A",
                                 roll_days = 0L,
                                 roll_months = 0L,
                                 roll_adjustment = "N",
                                 verbose = TRUE,
                                 ...){

  data(list = c("tickers_futures", "fields", "rolls"), package = "bbgsymbols", envir = environment())

  if (! is.character(active_contract_tickers)) stop("The parameter 'active_contract_tickers' must be
                                                    supplied as a character vector of Bloomberg tickers")
  if (! is.integer(TS_positions)) stop("The parameter 'TS_positions' must be supplied as a vector of integers")
  if (! rlang::is_scalar_logical(verbose)) stop("The parameter 'verbose' must be supplied as a scalar logical vector")
  if (is.null(file)) file <- "~/storethat.sqlite"
  else
    if (! all(rlang::is_scalar_character(file), stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database file (ie. ~/storethat.sqlite)")

  if ( any(c("asset_class", "sector", "subsector", "exchange", "currency") %in% names(list(...)))){
    tickers <- futures_groups(file, ...)
    if ( all(!is.null(tickers), NROW(tickers) == 0L)) stop("No ticker found for the specified grouping")
    if (! any(active_contract_tickers %in% tickers))
      warning("'", paste(active_contract_tickers[! active_contract_tickers %in% tickers], collapse = "', '"), "' not in specified grouping.")
    active_contract_tickers <- tickers
  }

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)
  data <- lapply(active_contract_tickers, function(y){
    tickers <- sapply(TS_positions, function(x) futures_ticker(y, TS_position = x, roll_type, roll_days, roll_months, roll_adjustment))

    query <- paste0("SELECT DISTINCT id, symbol AS ticker, active_contract_ticker_id
                    FROM tickers_support_futures_ts WHERE symbol ",
                    ifelse(NROW(tickers) == 1L,
                           paste0("= '", tickers, "';"),
                           paste0("IN ('", paste(tickers, collapse = "', '"), "');"))
    )

    tickers <- RSQLite::dbGetQuery(con, query)
    query <- paste0("SELECT DISTINCT id, symbol FROM tickers_futures WHERE id ",
                    ifelse(NROW(tickers$active_contract_ticker_id) == 1L,
                           paste0("= ", tickers$active_contract_ticker_id, ";"),
                           paste0("IN (", paste(tickers$active_contract_ticker_id, collapse = ", "), ");"))
    )
    tickers <- tickers %>%
      dplyr::left_join(RSQLite::dbGetQuery(con, query), by = c("active_contract_ticker_id" = "id")) %>%
      dplyr::select(id, ticker, `active contract ticker` = symbol)
    query <- paste0("SELECT id, date FROM support_dates WHERE date >= '", start, "' AND date <= '", end, "';")
    dates <- RSQLite::dbGetQuery(con, query)

    query <- paste0("SELECT * FROM data_futures_ts WHERE ticker_id ",
                    ifelse(NROW(tickers$id) == 1L,
                           paste0("= ", tickers$id),
                           paste0("IN (", paste0(tickers$id, collapse = ", "), ")")),
                    " AND date_id ",
                    ifelse(NROW(dates$id) == 1L,
                           paste0("= ", dates$id),
                           paste0("IN (", paste0(dates$id, collapse = ", "), ")")),
                    ";"
    )

    data <- RSQLite::dbGetQuery(con, query) %>%
      dplyr::left_join(tickers, by = c("ticker_id" = "id")) %>%
      dplyr::left_join(dates, by = c("date_id" = "id")) %>%
      dplyr::select(-c(ticker_id, date_id)) %>%
      tidyr::gather(field, value, -c(ticker, `active contract ticker`, date)) %>%
      dplyr::filter(stats::complete.cases(.))
    if (verbose) message(y); data
  }) %>%
    data.table::rbindlist(use.names = TRUE)

  RSQLite::dbDisconnect(con)

  if (nrow(data) == 0L) warning("No term structure data found.")

  data %<>%
    dplyr::arrange(`active contract ticker`, ticker, field, date) %>%
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01"))

  tickers <- dplyr::distinct(data, `active contract ticker`, ticker) %>%
    dplyr::mutate(`TS position` = stringr::str_extract(ticker, pattern = "(?<=^.{0,10})\\d(?=\\s[A-Z]:)"),
                  `roll type` = stringr::str_extract(ticker, pattern = "(?<= )[A-Z](?=:)"),
                  `roll days` = stringr::str_extract(ticker, pattern = "(?<=:)\\d{2}(?=_)") %>% as.numeric(),
                  `roll months` = stringr::str_extract(ticker, pattern = "(?<=_)\\d(?=_)") %>% as.numeric(),
                  `roll adjustment` = stringr::str_extract(ticker, pattern = "(?<=_)[A-Z](?= )")) %>%
    dplyr::left_join(dplyr::filter(rolls, roll == "type") %>% dplyr::select(symbol, name), by = c("roll type" = "symbol")) %>%
    dplyr::select(`active contract ticker`, ticker, `TS position`, `roll type` = name, `roll days`, `roll months`, `roll adjustment`) %>%
    dplyr::left_join(dplyr::filter(rolls, roll == "adjustment") %>% dplyr::select(symbol, name), by = c("roll adjustment" = "symbol")) %>%
    dplyr::select(`active contract ticker`, `TS position`, `roll type`, `roll days`, `roll months`, `roll adjustment` = name) %>%
    dplyr::left_join(tickers_futures, by = c("active contract ticker" = "ticker"))

  methods::new("FuturesTS",
               tickers = tibble::as.tibble(tickers),
               fields = data.table::as.data.table(dplyr::distinct(data, `active contract ticker`, ticker, field)),
               data = data.table::as.data.table(data),
               call = match.call()
  )
}



#' Futures aggregate historical data from Bloomberg
#'
#' @description Provided with a set of Bloomberg futures active contract tickers and a time period,
#'   retrieves the corresponding futures historical aggregate data previously stored in a \code{storethat}
#'   SQLite database. For each individual field, futures aggregate data represents the aggregation of the
#'   corresponding field values over all the corresponding term structure contracts.
#'
#' @param file A scalar chatacter vector. Specifies the target \code{storethat} SQLite database file.
#' @param active_contract_tickers A chatacter vector. Specifies the Bloomberg futures active contract tickers to query data for.
#'   Defaults to 'C A Comdty', the Bloomberg active contract ticker for the XCBT corn futures series.
#' @param start A scalar character vector. Specifies the starting date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-01-01'.
#' @param end A scalar character vector. Specifies the end date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-06-30'.
#' @param verbose A logical scalar vector. Should progression messages be printed? Defaults to TRUE.
#' @param ... optional extra parameters for grouping:
#'   \itemize{
#'     \item{\code{asset_class}: a character vector. See column 'asset class' of the \code{\link[bbgsymbols]{tickers_futures}} dataset in the  \code{bbgsymbols}.}
#'     \item{\code{sector}: a character vector. See column 'sector' of the \code{\link[bbgsymbols]{tickers_futures}} dataset in the  \code{bbgsymbols}.}
#'     \item{\code{subsector}: a character vector. See column 'subsector' of the \code{\link[bbgsymbols]{tickers_futures}} dataset in the  \code{bbgsymbols}.}
#'     \item{\code{exchange}: a character vector. See column 'symbol' of the \code{\link[fewISOs]{exchanges}} dataset in the \code{fewISOs}.}
#'     \item{\code{currency}: a character vector. See column 'symbol' of the \code{\link[fewISOs]{currencies}} dataset in the \code{fewISOs}.}
#'   }
#'
#' @return An S4 object of class \code{\linkS4class{FuturesAggregate}} with slots:
#'   \itemize{
#'     \item{\code{tickers}: a character vector. Active contract Bloomberg tickers for which data has been found.}
#'     \item{\code{fields}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'         \item{\code{field}: futures Bloomberg aggregate data fields for which data has been found.}
#'       }
#'     }
#'     \item{\code{data}: a tibble. Columns inlude:
#'       \itemize{
#'         \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'         \item{\code{field}: Bloomberg aggregate data fields for which data has been found.}
#'         \item{\code{date}: observation date.}
#'         \item{\code{value}: corresponding value.}
#'       }
#'     }
#'     \item{\code{call}: a scalar character vector showing the original call to the constructor.}
#'   }
#'
#' @seealso The \code{\link[bbgsymbols]{fields}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg fields used here.
#'
#' @examples
#' \dontrun{bbg_futures_aggregate()}
#'
#' @import bbgsymbols
#' @importFrom magrittr "%>%" "%<>%"
#'
#' @export
storethat_futures_aggregate <- function(file = NULL,
                                        active_contract_tickers = "C A Comdty",
                                        start = "2018-01-01",
                                        end = "2018-06-30",
                                        verbose = TRUE,
                                        ...){

  data(list = c("fields"), package = "bbgsymbols", envir = environment())

  if (! is.character(active_contract_tickers)) stop("The parameter 'active_contract_tickers' must be supplied as a character vector of Bloomberg tickers")
  if (! rlang::is_scalar_logical(verbose)) stop("The parameter 'verbose' must be supplied as a scalar logical vector")
  if (is.null(file)) file <- "~/storethat.sqlite"
  else
    if (! all(rlang::is_scalar_character(file) & stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database file (ie. ~/storethat.sqlite)")

  if ( any(c("asset_class", "sector", "subsector", "exchange", "currency") %in% names(list(...)))){
    tickers <- futures_groups(file, ...)
    if ( all(!is.null(tickers), NROW(tickers) == 0L)) stop("No ticker found for the specified grouping")
    if (! any(active_contract_tickers %in% tickers))
      warning("'", paste(active_contract_tickers[! active_contract_tickers %in% tickers], collapse = "', '"), "' not in specified grouping.")
    active_contract_tickers <- tickers
  }

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)
  data <- lapply(active_contract_tickers, function(x) {
    query <- paste0("SELECT DISTINCT id, symbol FROM tickers_futures WHERE symbol ",
                    paste0("= '", x, "';")
    )
    tickers <- RSQLite::dbGetQuery(con, query)
    query <- paste0("SELECT id, date FROM support_dates WHERE date >= '", start, "' AND date <= '", end, "';")
    dates <- RSQLite::dbGetQuery(con, query)

    query <- paste0("SELECT * FROM data_futures_aggregate WHERE ticker_id ",
                    ifelse(NROW(tickers$id) == 1L,
                           paste0("= ", tickers$id),
                           paste0("IN (", paste0(tickers$id, collapse = ", "), ")")),
                    " AND date_id ",
                    ifelse(NROW(dates$id) == 1L,
                           paste0("= ", dates$id),
                           paste0("IN (", paste0(dates$id, collapse = ", "), ")")),
                    ";"
    )

    data <- RSQLite::dbGetQuery(con, query) %>%
      dplyr::left_join(tickers, by = c("ticker_id" = "id")) %>%
      dplyr::left_join(dates, by = c("date_id" = "id")) %>%
      dplyr::select(-c(ticker_id, date_id)) %>%
      dplyr::rename(`active contract ticker` = symbol) %>%
      tidyr::gather(field, value, -c(`active contract ticker`, date)) %>%
      dplyr::filter(stats::complete.cases(.))
    if (verbose) message(x); data
  }) %>%
    data.table::rbindlist(use.names = TRUE)

  if (nrow(data) == 0L) warning("No aggregate data found.")

  RSQLite::dbDisconnect(con)

  data %<>%
    dplyr::arrange(`active contract ticker`, field, date) %>%
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01")) %>%
    dplyr::select(`active contract ticker`, field, date, value)

  methods::new("FuturesAggregate",
               tickers = dplyr::left_join(dplyr::distinct(data, `active contract ticker`), tickers_futures, by = c("active contract ticker", "ticker")),
               fields = data.table::as.data.table(dplyr::distinct(data, `active contract ticker`, field)),
               data = data.table::as.data.table(data),
               call = match.call()
  )
}




#' Futures CFTC position historical data from Bloomberg
#'
#' @description Provided with a set of Bloomberg futures active contract tickers and a time period,
#'   retrieves the corresponding futures CFTC position historical data previously stored in a \code{storethat}
#'   SQLite database.
#'
#' @param file A scalar chatacter vector. Specifies the target \code{storethat} SQLite database file.
#' @param active_contract_tickers A chatacter vector. Specifies the Bloomberg futures active contract tickers to query data for.
#'   Defaults to 'C A Comdty', the Bloomberg active contract ticker for the XCBT corn futures series.
#' @param start A scalar character vector. Specifies the starting date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-01-01'.
#' @param end A scalar character vector. Specifies the end date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-06-30'.
#' @param verbose A logical scalar vector. Should progression messages be printed? Defaults to TRUE.
#' @param ... optional extra parameters for grouping:
#'   \itemize{
#'     \item{\code{asset_class}: a character vector. See column 'asset class' of the \code{\link[bbgsymbols]{tickers_futures}} dataset in the  \code{bbgsymbols}.}
#'     \item{\code{sector}: a character vector. See column 'sector' of the \code{\link[bbgsymbols]{tickers_futures}} dataset in the  \code{bbgsymbols}.}
#'     \item{\code{subsector}: a character vector. See column 'subsector' of the \code{\link[bbgsymbols]{tickers_futures}} dataset in the  \code{bbgsymbols}.}
#'     \item{\code{exchange}: a character vector. See column 'symbol' of the \code{\link[fewISOs]{exchanges}} dataset in the \code{fewISOs}.}
#'     \item{\code{currency}: a character vector. See column 'symbol' of the \code{\link[fewISOs]{currencies}} dataset in the \code{fewISOs}.}
#'   }
#'
#' @return An S4 object of class \code{\linkS4class{FuturesCFTC}} with slots:
#'   \itemize{
#'     \item{\code{tickers}: a data.table. Active contract and corresponding position Bloomberg tickers for which data has been found. Columns include:
#'       \itemize{
#'         \item{active contract ticker: futures series active contract Bloomberg ticker.}
#'         \item{position ticker: CFTC position Bloomberg ticker.}
#'       }
#'     }
#'     \item{\code{fields}: a data.table. Columns include:
#'       \itemize{
#'         \item{\code{format}: report formats ('legacy', 'disaggregated', 'supplemental' or 'traders in financial futures')
#'           for which data has been found.}
#'         \item{\code{underlying}: underlying instruments ('futures only' or 'futures & options') for which data has been found..}
#'         \item{\code{unit}: counting units (number of 'contracts', 'traders' or 'total') for which data has been found.}
#'         \item{\code{participant}: trader classifications for which data has been found. Report specific:
#'           \itemize{
#'             \item{legacy: 'commercial', 'non-commercial', 'non-reportable', 'total'.}
#'             \item{disaggregated: 'managed money', 'producer/merchant/processor/user', 'swap dealers', 'other reportables'.}
#'             \item{supplemental: 'commercial - non-CIT', 'non-commercial - non-CIT', 'index traders - non-CIT', 'index traders'.}
#'             \item{traders in financial futures: 'asset manager/institutional', 'dealer/intermediary', 'leveraged funds', 'other reportables'.}
#'           }
#'         }
#'         \item{\code{position}: trader positions for which data has been found. Participant specific.
#'           \itemize{
#'             \item{commercial: 'long', 'short', 'net'.}
#'             \item{non-commercial: 'long', 'short', 'net', 'spreading'.}
#'             \item{non-reportable: 'long', 'short', 'net'.}
#'             \item{total: 'long', 'short', 'net', 'open interest', 'total'.}
#'             \item{managed money: 'long', 'short', 'net', 'spreading'.}
#'             \item{producer/merchant/processor/user: 'long', 'short', 'net'.}
#'             \item{swap dealers: 'long', 'short', 'net', 'spreading'.}
#'             \item{other reportables: 'long', 'short', 'net', 'spreading'.}
#'             \item{commercial - non-CIT: 'long', 'short', 'net'.}
#'             \item{non-commercial - non-CIT: 'long', 'short', 'net', 'spreading'.}
#'             \item{index traders - non-CIT: 'long', 'short'.}
#'             \item{index traders: 'long', 'short', 'net'.}
#'             \item{asset manager/institutional: 'long', 'short', 'net', 'spreading'.}
#'             \item{dealer/intermediary: 'long', 'short', 'net', 'spreading'.}
#'             \item{leveraged funds: 'long', 'short', 'net', 'spreading'.}
#'             \item{other reportables: 'long', 'short', 'net', 'spreading'.}
#'          }
#'        }
#'        \item{\code{position ticker}: corresponding CFTC position Bloomberg ticker.}
#'      }
#'    }
#'     \item{\code{data}: a tibble. Columns inlude:
#'       \itemize{
#'         \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'         \item{\code{position ticker}: CFTC position Bloomberg tickers for which data has been found.}
#'         \item{\code{date}: observation date.}
#'         \item{\code{value}: observed value}
#'       }
#'     }
#'     \item{\code{call}: a scalar character vector showing the original call to the constructor.}
#'  }
#'
#' @seealso The \code{\link[bbgsymbols]{tickers_cftc}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg fields used here.
#'
#' @examples
#' \dontrun{storethat_futures_cftc()}
#'
#' @import bbgsymbols
#' @importFrom magrittr "%>%" "%<>%"
#'
#' @export
storethat_futures_cftc <- function(file = NULL,
                                   active_contract_tickers = "C A Comdty",
                                   start = "2018-01-01",
                                   end = "2018-06-30",
                                   verbose = TRUE,
                                   ...){

  data(list = c("fields"), package = "bbgsymbols", envir = environment())

  if (! is.character(active_contract_tickers)) stop("The parameter 'active_contract_tickers' must be supplied as a character vector of Bloomberg tickers")
  if (! rlang::is_scalar_logical(verbose)) stop("The parameter 'verbose' must be supplied as a scalar logical vector")
  if (is.null(file)) file <- "~/storethat.sqlite"
  else
    if (! all(rlang::is_scalar_character(file) & stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database file (ie. ~/storethat.sqlite)")

  if ( any(c("asset_class", "sector", "subsector", "exchange", "currency") %in% names(list(...)))){
    tickers <- futures_groups(file, ...)
    if ( all(!is.null(tickers), NROW(tickers) == 0L)) stop("No ticker found for the specified grouping")
    if (! any(active_contract_tickers %in% tickers))
      warning("'", paste(active_contract_tickers[! active_contract_tickers %in% tickers], collapse = "', '"), "' not in specified grouping.")
    active_contract_tickers <- tickers
  }

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)
  data <- lapply(active_contract_tickers, function(x) {

    query <- paste0("SELECT active_contract_ticker_id, active_contract_ticker, position_ticker_id, position_ticker
                    FROM (SELECT id, symbol AS active_contract_ticker FROM tickers_futures WHERE symbol ",
                    paste0("= '", x, "') A "),
                    "LEFT JOIN (SELECT id AS position_ticker_id, symbol AS position_ticker, active_contract_ticker_id FROM tickers_support_futures_cftc) B
                    ON A.id = B.active_contract_ticker_id;")
    tickers <- RSQLite::dbGetQuery(con, query)
    query <- paste0("SELECT id, date FROM support_dates WHERE date >= '", start, "' AND date <= '", end, "';")
    dates <- RSQLite::dbGetQuery(con, query)

    query <- paste0("SELECT * FROM data_futures_cftc WHERE ticker_id IN (",
                    paste0(tickers$ position_ticker_id, collapse = ", "),
                    ") AND date_id ",
                    ifelse(NROW(dates$id) == 1L,
                           paste0("= ", dates$id),
                           paste0("IN (", paste0(dates$id, collapse = ", "), ")")),
                    ";"
    )

    data <- RSQLite::dbGetQuery(con, query) %>%
      dplyr::left_join(tickers, by = c("ticker_id" = "position_ticker_id")) %>%
      dplyr::left_join(dates, by = c("date_id" = "id")) %>%
      dplyr::select(-c(ticker_id, date_id)) %>%
      dplyr::select(`active contract ticker` = active_contract_ticker, `position ticker` = position_ticker, date, value) %>%
      dplyr::filter(stats::complete.cases(.))
    if (verbose) message(x); data
  }) %>%
    data.table::rbindlist(use.names = TRUE)

  if (nrow(data) == 0L) warning("No aggregate data found.")

  data %<>%
    dplyr::arrange(`active contract ticker`, `position ticker`, date) %>%
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01")) %>%
    dplyr::select(`active contract ticker`, `position ticker`, date, value)

  query <- paste0("SELECT symbol, name, asset_class, sector, subsector, TS_length,
                  MIC, currency_id, FIGI FROM (SELECT * FROM tickers_futures WHERE symbol IN ('",
                  paste(dplyr::distinct(data, `active contract ticker`)$`active contract ticker`, collapse = "', '"),
                  "')) A LEFT JOIN (SELECT id AS exchange_id, symbol AS MIC FROM support_exchanges) B ON A.MIC_id = B.exchange_id")
  query <- paste0("SELECT symbol AS 'active contract ticker', name, asset_class AS 'asset class', sector, subsector, TS_length AS 'term structure length',
                  MIC, currency, FIGI FROM (",
                  query, ") C LEFT JOIN (SELECT id AS currency_id, symbol AS currency FROM support_currencies) D ON C.currency_id = D.currency_id;")
  tickers <- tibble::as.tibble(RSQLite::dbGetQuery(con, query))
  query <- paste0("SELECT symbol AS 'active contract ticker', format, underlying, unit, participant, position FROM
                  (SELECT active_contract_ticker_id, format, underlying, unit, participant, position FROM
                  tickers_support_futures_cftc WHERE symbol IN ('",
                  paste(dplyr::distinct(data, `position ticker`)$`position ticker`, collapse = "', '"),
                  "')) A LEFT JOIN (SELECT id, symbol FROM tickers_futures) B ON A.active_contract_ticker_id = B.id;")
  fields <- tibble::as.tibble(RSQLite::dbGetQuery(con, query))
  RSQLite::dbDisconnect(con)

  methods::new("FuturesCFTC",
               tickers = tickers,
               fields = data.table::as.data.table(fields),
               data = data.table::as.data.table(data),
               call = match.call()
  )
}





#' Equity historical market data from a \code{storethat} SQLite database.
#'
#' @description Provided with a set of Bloomberg equity tickers and a time period,
#'   retrieves the corresponding equity historical market data previously stored in a \code{storethat}
#'   SQLite database.
#'
#' @param file A scalar chatacter vector. Specifies the target \code{storethat} SQLite database file.
#' @param tickers A chatacter vector. Specifies the Bloomberg equity tickers to query data for.
#'   Defaults to 'NEM US Equity', the Bloomberg equity ticker for the Newmont Minning Corporation.
#' @param start A scalar character vector. Specifies the starting date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-01-01'.
#' @param end A scalar character vector. Specifies the end date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-06-30'.
#' @param verbose A logical scalar vector. Should progression messages be printed? Defaults to TRUE.
#' @param ... optional extra parameters for grouping:
#'   \itemize{
#'     \item{\code{type}: a character vector. See 'SECURITY_TYP' data field on Bloomberg.}
#'     \item{\code{exchange}: a character vector. See column 'symbol' of the \code{\link[fewISOs]{exchanges}} dataset in the \code{fewISOs} package.}
#'     \item{\code{country}: a character vector. See column 'symbol' of the \code{\link[fewISOs]{countries}} dataset in the \code{fewISOs} package.}
#'     \item{\code{currency}: a character vector. See column 'symbol' of the \code{\link[fewISOs]{currencies}} dataset in the \code{fewISOs} package.}
#'     \item{\code{sector}: a character vector. See column 'sector name' of the \code{GICS} dataset in the \code{storethat} package.}
#'     \item{\code{industry_group}: a character vector. See column 'industry group name' of the \code{GICS} dataset in the \code{storethat} package.}
#'     \item{\code{industry}: a character vector. See column 'industry name' of the \code{GICS} dataset in the \code{storethat} package.}
#'     \item{\code{subindustry}: a character vector. See column 'subindustry name' of the \code{GICS} dataset in the \code{storethat} package.}
#'   }
#'
#' @return An S4 object of class \code{\linkS4class{EquityMarket}} with slots:
#'   \itemize{
#'     \item{\code{tickers}: a character vector. Equity Bloomberg tickers for which data has been found.}
#'     \item{\code{fields}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'         \item{\code{field}: equity Bloomberg market data fields for which data has been found.}
#'       }
#'     }
#'     \item{\code{data}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'         \item{\code{field}: equity Bloomberg market data fields for which data has been found.}
#'         \item{\code{date}: observation date.}
#'         \item{\code{value}: corresponding value.}
#'       }
#'     }
#'     \item{\code{call}: a scalar character vector showing the original call to the constructor.}
#'   }
#'
#' @seealso The \code{\link[bbgsymbols]{fields}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg fields used here.
#'
#' @examples
#' \dontrun{bbg_equity_market()}
#'
#' @import bbgsymbols
#' @importFrom magrittr "%>%" "%<>%"
#'
#' @export
storethat_equity_market <- function(file = NULL,
                                    tickers = "NEM US Equity",
                                    start = "2018-01-01",
                                    end = "2018-06-30",
                                    verbose = TRUE,
                                    ...){

  data(list = c("fields"), package = "bbgsymbols", envir = environment())

  if (! is.character(tickers)) stop("The parameter 'tickers' must be supplied as a character vector of Bloomberg tickers")
  if (! rlang::is_scalar_logical(verbose)) stop("The parameter 'verbose' must be supplied as a scalar logical vector")
  if (is.null(file)) file <- "~/storethat.sqlite"
  else
    if (! all(rlang::is_scalar_character(file) & stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database file (ie. ~/storethat.sqlite)")

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)

  if ( any(c("type", "exchange", "country", "currency", "sector", "industry_group", "industry", "subindustry") %in% names(list(...)))){
    data <- equity_groups(file, ...)
    if ( all(!is.null(data), NROW(data) == 0L)) stop("No ticker found for the specified grouping")
    if (! any(tickers %in% data))
      warning("'", paste(tickers[! tickers %in% data], collapse = "', '"), "' not in specified grouping.")
    tickers <- data
  }

  data <- lapply(tickers, function(x) {
    query <- paste0("SELECT DISTINCT id, symbol FROM tickers_equity WHERE symbol ",
                    paste0("= '", x, "';")
    )
    tickers <- RSQLite::dbGetQuery(con, query)
    query <- paste0("SELECT id, date FROM support_dates WHERE date >= '", start, "' AND date <= '", end, "';")
    dates <- RSQLite::dbGetQuery(con, query)

    query <- paste0("SELECT * FROM data_equity_market WHERE ticker_id ",
                    paste0("= ", tickers$id),
                    " AND date_id ",
                    ifelse(NROW(dates$id) == 1L,
                           paste0("= ", dates$id),
                           paste0("IN (", paste0(dates$id, collapse = ", "), ")")),
                    ";"
    )

    data <- RSQLite::dbGetQuery(con, query) %>%
      dplyr::left_join(tickers, by = c("ticker_id" = "id")) %>%
      dplyr::left_join(dates, by = c("date_id" = "id")) %>%
      dplyr::select(-c(ticker_id, date_id)) %>%
      dplyr::rename(ticker = symbol) %>%
      tidyr::gather(field, value, -c(ticker, date)) %>%
      dplyr::filter(stats::complete.cases(.))
    if (verbose) message(x); data
  }) %>%
    data.table::rbindlist(use.names = TRUE)

  if (nrow(data) == 0L) warning("No equity historical market data found.")

  data %<>%
    tidyr::gather(field, value, -c(ticker, date)) %>%
    dplyr::select(ticker, field, date, value) %>%
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01")) %>%
    dplyr::arrange(ticker, field, date)

  tickers <- dplyr::distinct(data, ticker)$ticker
  query <- paste0("(SELECT symbol AS ticker, name, type, MIC_id, country_id, currency_id, GICS_id, FIGI, description
                  FROM tickers_equity WHERE symbol ",
                  ifelse(NROW(tickers) == 1L,
                         paste0("= '", tickers, "'"),
                         paste0("IN ('", paste0(tickers, collapse = "', '"), "')")),
                  ")"
  )
  query <- paste0("(SELECT ticker, name, type, symbol AS exchange, country_id, currency_id, GICS_id, FIGI, description FROM ",
                  query,
                  " A LEFT JOIN (SELECT id, symbol FROM support_exchanges) B ON A.MIC_id = B.id)")
  query <- paste0("(SELECT ticker, name, type, exchange, symbol AS country, currency_id, GICS_id, FIGI, description FROM ",
                  query,
                  " C LEFT JOIN (SELECT id, symbol FROM support_countries) D ON C.country_id = D.id)")
  query <- paste0("(SELECT ticker, name, type, exchange, country, symbol AS currency, GICS_id, FIGI, description FROM ",
                  query,
                  " E LEFT JOIN (SELECT id, symbol FROM support_currencies) F ON E.currency_id = F.id)")
  query <- paste0("SELECT ticker, name, type AS 'security type', exchange, country AS 'country of incorporation',
                  currency, sector_name AS sector, industry_group_name AS 'industry group', industry_name AS 'industry',
                  subindustry_name AS subindustry, FIGI, description FROM ",
                  query,
                  " G LEFT JOIN (SELECT sector_name, industry_group_name, industry_name, subindustry_name, id FROM
                  support_GICS) H ON G.GICS_id = H.id;")

  tickers <- RSQLite::dbGetQuery(con, query)
  RSQLite::dbDisconnect(con)

  methods::new("EquityMarket",
               tickers = tibble::as.tibble(tickers),
               fields = data.table::as.data.table(dplyr::distinct(data, ticker, field)),
               data = data.table::as.data.table(data),
               call = match.call()
  )
}




#' Equity historical book data from Bloomberg
#'
#' @description Retrieve equity historical book data from Bloomberg. Books include 'key stats', 'income statement', 'balance sheet',
#'   'cash flow statement' and 'ratios' with the corresponding data fields retrieved from the 'fields' dataset of the \code{bbgsymbols}
#'   package.
#'
#' @param file A scalar chatacter vector. Specifies the target \code{storethat} SQLite database file.
#' @param book A scalar chatacter vector, 'key stats', 'income statement', 'balance sheet', 'cash flow statement' or 'ratios'.
#'   Defaults to 'key stats' which returns historical data for a set of key financial indicators; a quick dynamic financial
#'   snapshot of the selected names.
#' @param tickers A chatacter vector. Specifies the Bloomberg equity tickers to query data for.
#'   Defaults to 'NEM US Equity', the Bloomberg equity ticker for the Newmont Minning Corporation.
#' @param start A scalar character vector. Specifies the starting date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-01-01'.
#' @param end A scalar character vector. Specifies the end date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-06-30'.
#' @param verbose A logical scalar vector. Should progression messages be printed? Defaults to TRUE.
#' @param ... optional extra parameters for grouping:
#'   \itemize{
#'     \item{\code{type}: a character vector. See 'SECURITY_TYP' data field on Bloomberg.}
#'     \item{\code{exchange}: a character vector. See column 'symbol' of the \code{\link[fewISOs]{exchanges}} dataset in the \code{fewISOs} package.}
#'     \item{\code{country}: a character vector. See column 'symbol' of the \code{\link[fewISOs]{countries}} dataset in the \code{fewISOs} package.}
#'     \item{\code{currency}: a character vector. See column 'symbol' in \code{\link[fewISOs]{currencies}} dataset in the \code{fewISOs} package.}
#'     \item{\code{sector}: a character vector. See column 'sector name' of the \code{GICS} dataset in the \code{storethat} package.}
#'     \item{\code{industry_group}: a character vector. See column 'industry group name' of the \code{GICS} dataset in the \code{storethat} package.}
#'     \item{\code{industry}: a character vector. See column 'industry name' of the \code{GICS} dataset in the \code{storethat} package.}
#'     \item{\code{subindustry}: a character vector. See column 'subindustry name' of the \code{GICS} dataset in the \code{storethat} package.}
#'   }
#'
#' @return An S4 object of class \code{\linkS4class{EquityKS}} (\code{book = 'key stats'}), \code{\linkS4class{EquityIS}}
#'   (\code{book = 'income statement'}), \code{\linkS4class{EquityBS}} (\code{book = 'balance sheet'}),
#'    \code{\linkS4class{EquityCF}} (\code{book = 'cash flow statement'}), \code{\linkS4class{EquityRatios}}
#'    (\code{book = 'ratios'}). Slots include:
#'   \itemize{
#'     \item{\code{tickers}: a data.table. Equity Bloomberg tickers for which data has been found.}
#'     \item{\code{fields}: a data.table. Data fields for which data has been found.}
#'     \item{\code{data}: a data.table The data retrieved in the query.}
#'     \item{\code{call}: a scalar character vector. The original call to the constructor.}
#'   }
#'
#' @seealso The \code{\link[bbgsymbols]{fields}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg fields used here.
#'
#' @examples
#' \dontrun{
#'   bbg_equity_books(book = 'key stats')
#'   bbg_equity_books(book = 'income statement')
#'   bbg_equity_books(book = 'balance sheet')
#'   bbg_equity_books(book = 'cash flow statement')
#'   bbg_equity_books(book = 'ratios')
#' }
#'
#' @import bbgsymbols
#' @importFrom magrittr "%>%"
#'
#' @export
storethat_equity_books <- function(file = NULL,
                                   book = "key stats",
                                   tickers = "NEM US Equity",
                                   start = "2018-01-01",
                                   end = "2018-06-30",
                                   verbose = TRUE,
                                   ...){

  data(list = c("fields"), package = "bbgsymbols", envir = environment())

  if (! rlang::is_scalar_character(book) & book %in% c("key stats", "income statement", "balance sheet", "cash flow statement", "ratios"))
    stop("The parameter 'book' must be supplied as a scalar character vector, one of 'key stats', 'income statement', 'balance sheet',
         'cash flow statement' or 'ratios'")
  if (! is.character(tickers)) stop("The parameter 'tickers' must be supplied as a character vector of Bloomberg equity tickers")
  if (! rlang::is_scalar_logical(verbose)) stop("The parameter 'verbose' must be supplied as a scalar logical vector (TRUE or FALSE)")
  if (is.null(file)) file <- "~/storethat.sqlite"
  else
    if (! all(rlang::is_scalar_character(file), stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database file (ie. ~/storethat.sqlite)")

  if ( any(c("type", "exchange", "country", "currency", "sector", "industry_group", "industry", "subindustry") %in% names(list(...)))){
    data <- equity_groups(file, ...)
    if ( all(!is.null(data), NROW(data) == 0L)) stop("No ticker found for the specified grouping")
    if (! any(tickers %in% data))
      warning("'", paste(tickers[! tickers %in% data], collapse = "', '"), "' not in specified grouping.")
    tickers <- data
  }

  symbols <- dplyr::filter(fields, instrument == "equity", type == book)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)

  data <- lapply(tickers, function(x) {
    query <- paste0("SELECT DISTINCT id, symbol FROM tickers_equity WHERE symbol ",
                    paste0("= '", x, "';")
    )
    tickers <- RSQLite::dbGetQuery(con, query)
    if (nrow(tickers) == 0L) stop(paste0("No data found for '", x, "'"))
    query <- paste0("SELECT id, date FROM support_dates WHERE date >= '", start, "' AND date <= '", end, "';")
    dates <- RSQLite::dbGetQuery(con, query)

    query <- paste0("SELECT * FROM data_equity_",
                    dplyr::case_when(book == "key stats" ~ "KS", book == "income statement" ~ "IS", book == "balance sheet" ~ "BS",
                                     book == "cash flow statement" ~ "CF", book == "ratios" ~ "ratios"),
                    " WHERE ticker_id ",
                    paste0("= ", tickers$id),
                    " AND date_id ",
                    ifelse(NROW(dates$id) == 1L,
                           paste0("= ", dates$id),
                           paste0("IN (", paste0(dates$id, collapse = ", "), ")")),
                    ";"
    )

    data <- RSQLite::dbGetQuery(con, query) %>%
      dplyr::left_join(tickers, by = c("ticker_id" = "id")) %>%
      dplyr::left_join(dates, by = c("date_id" = "id")) %>%
      dplyr::select(-c(ticker_id, date_id)) %>%
      dplyr::rename(ticker = symbol) %>%
      tidyr::gather(field, value, -c(ticker, date)) %>%
      dplyr::filter(stats::complete.cases(.))
    if (verbose) message(x); data
  }) %>%
    data.table::rbindlist(use.names = TRUE) %>%
    dplyr::arrange(ticker, field, date) %>%
    dplyr::select(ticker, field, date, value)

  if (nrow(data) == 0L) warning(paste("No", book, "historical data found.", sep = " "))

  tickers <- dplyr::distinct(data, ticker)$ticker
  query <- paste0("(SELECT symbol AS ticker, name, type, MIC_id, country_id, currency_id, GICS_id, FIGI, description
                  FROM tickers_equity WHERE symbol ",
                  ifelse(NROW(tickers) == 1L,
                         paste0("= '", tickers, "'"),
                         paste0("IN ('", paste0(tickers, collapse = "', '"), "')")),
                  ")"
  )
  query <- paste0("(SELECT ticker, name, type, symbol AS exchange, country_id, currency_id, GICS_id, FIGI, description FROM ",
                  query,
                  " A LEFT JOIN (SELECT id, symbol FROM support_exchanges) B ON A.MIC_id = B.id)")
  query <- paste0("(SELECT ticker, name, type, exchange, symbol AS country, currency_id, GICS_id, FIGI, description FROM ",
                  query,
                  " C LEFT JOIN (SELECT id, symbol FROM support_countries) D ON C.country_id = D.id)")
  query <- paste0("(SELECT ticker, name, type, exchange, country, symbol AS currency, GICS_id, FIGI, description FROM ",
                  query,
                  " E LEFT JOIN (SELECT id, symbol FROM support_currencies) F ON E.currency_id = F.id)")
  query <- paste0("SELECT ticker, name, type AS 'security type', exchange, country AS 'country of incorporation',
                  currency, sector_name AS sector, industry_group_name AS 'industry group', industry_name AS 'industry',
                  subindustry_name AS subindustry, FIGI, description FROM ",
                  query,
                  " G LEFT JOIN (SELECT sector_name, industry_group_name, industry_name, subindustry_name, id FROM
                  support_GICS) H ON G.GICS_id = H.id;")

  tickers <- RSQLite::dbGetQuery(con, query)
  RSQLite::dbDisconnect(con)

  fields <- dplyr::left_join(dplyr::distinct(data, ticker, field), symbols, by = c("field" = "symbol"))

  fields <-   switch(book,
                     `key stats` = data.table::as.data.table(dplyr::select(fields, ticker, field = name)),
                     `income statement` = data.table::as.data.table(dplyr::select(fields, ticker, field = name)),
                     `balance sheet` = data.table::as.data.table(dplyr::select(fields, ticker, section, subsection, field = name)),
                     `cash flow statement` = data.table::as.data.table(dplyr::select(fields, ticker, section, field = name)),
                     `ratios` = data.table::as.data.table(dplyr::select(data, ticker, section, subsection, field = name))

  )

  methods::new(dplyr::case_when(book == "key stats" ~ "EquityKS", book == "balance sheet" ~ "EquityBS",
                                book == "cash flow statement" ~ "EquityCF", book == "income statement" ~ "EquityIS",
                                book == "ratios" ~ "EquityRatios"),
               tickers = tibble::as.tibble(tickers),
               fields = fields,
               data = data.table::as.data.table(data),
               call = match.call()
  )
}

