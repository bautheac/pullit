#' Futures market historical data from Bloomberg
#'
#' @description Provided with a set of Bloomberg futures active contract tickers, term structure positions, roll parameters and a time period,
#'   queries Bloomberg for the corresponding term structure historical data.
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
#'   Works in tandem with `roll_months` below.
#'   Defaults to 0.
#' @param roll_months A scalar integer vector. Specifies the month the roll should be done.
#'   Refers to the number of months before a reference date (\code{roll_type} = 'D', \code{roll_type} = 'N', \code{roll_type} = 'O',
#'   \code{roll_type} = 'R'). Works in tandem with `roll_days` above.
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
    stop("The parameter 'type' must be supplied as a scalar character vector; one of 'term structure' or 'aggregate'.")
  if (! is.character(active_contract_tickers)) stop("The parameter 'active_contract_tickers' must be supplied as a character vector of Bloomberg tickers.")
  if (! is.integer(TS_positions)) stop("The parameter 'TS_positions' must be supplied as a vector of integers.")
  if (! rlang::is_scalar_logical(verbose)) stop("The parameter 'verbose' must be supplied as a scalar logical vector.")

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
#'   Works in tandem with `roll_months` below.
#'   Defaults to 0.
#' @param roll_months A scalar integer vector. Specifies the month the roll should be done.
#'   Refers to the number of months before a reference date (\code{roll_type} = 'D', \code{roll_type} = 'N', \code{roll_type} = 'O',
#'   \code{roll_type} = 'R'). Works in tandem with `roll_days` above.
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

  call <- deparse(match.call())
  data(list = c("fields", "rolls"), package = "bbgsymbols", envir = environment())

  if (! is.character(active_contract_tickers)) stop("The parameter 'active_contract_tickers' must be supplied as a character vector of Bloomberg tickers.")
  if (! is.integer(TS_positions)) stop("The parameter 'TS_positions' must be supplied as a vector of integers.")
  if (! rlang::is_scalar_logical(verbose)) stop("The parameter 'verbose' must be supplied as a scalar logical vector.")

  data <- lapply(active_contract_tickers, function(y){
    if (verbose) message(y)
    tickers <- sapply(TS_positions, function(x) futures_ticker(y, TS_position = x, roll_type, roll_days, roll_months, roll_adjustment))
    data <- bbg_pull_historical_market(tickers, fields = dplyr::filter(fields, instrument == "futures", type == "market") %>% dplyr::select(symbol) %>% purrr::flatten_chr(),
                                start, end, ...)

    data <- if (length(tickers) > 1L) lapply(seq_along(TS_positions), function(x) {
      if (nrow(data[[x]]) > 0L) { data[[x]]$ticker <- tickers[x]; data[[x]]$`TS position` <- TS_positions[x] }
      data[[x]]
    }) %>%
      data.table::rbindlist(use.names = TRUE)
    else {
      if(nrow(data) < 1L) data[1L, ] <- rep(NA, ncol(data))
      else { data$ticker <- tickers; data$`TS position` <- TS_positions }
      data
    }
    data$`active contract ticker` <- y
    data
  }) %>%
    data.table::rbindlist(use.names = TRUE)

  data %<>%
    tidyr::gather(field, value, -c(`active contract ticker`, ticker, `TS position`, date)) %>%
    dplyr::select(`active contract ticker`, ticker, `TS position`, field, date, value) %>%
    dplyr::arrange(`active contract ticker`, ticker, field, date) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01"))

  if (nrow(data) == 0L) warning("No term structure data found.")

  tickers <- dplyr::distinct(data, `active contract ticker`, ticker, `TS position`) %>%
    dplyr::mutate(`roll type` = stringr::str_extract(ticker, pattern = "(?<= )[A-Z](?=:)"),
           `roll days` = stringr::str_extract(ticker, pattern = "(?<=:)\\d{2}(?=_)") %>% as.numeric(),
           `roll months` = stringr::str_extract(ticker, pattern = "(?<=_)\\d(?=_)") %>% as.numeric(),
           `roll adjustment` = stringr::str_extract(ticker, pattern = "(?<=_)[A-Z](?= )")) %>%
    dplyr::left_join(dplyr::filter(rolls, roll == "type") %>% dplyr::select(symbol, name), by = c("roll type" = "symbol")) %>%
    dplyr::select(`active contract ticker`, ticker, `TS position`, `roll type` = name, `roll days`, `roll months`, `roll adjustment`) %>%
    dplyr::left_join(dplyr::filter(rolls, roll == "adjustment") %>% dplyr::select(symbol, name), by = c("roll adjustment" = "symbol")) %>%
    dplyr::select(`active contract ticker`, `TS position`, ticker, `roll type`, `roll days`, `roll months`, `roll adjustment` = name)

  methods::new("FuturesTS",
      tickers = tickers %>%
        data.table::as.data.table(),
      fields = dplyr::distinct(data, `active contract ticker`, `TS position`, field) %>%
                  data.table::as.data.table(),
      data = data %>%
        dplyr::select(-ticker) %>%
        data.table::as.data.table(),
      call = call
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
  data(list = c("fields"), package = "bbgsymbols", envir = environment())

  if (! is.character(active_contract_tickers)) stop("The parameter 'active_contract_tickers' must be supplied as a character vector of Bloomberg tickers.")
  if (! rlang::is_scalar_logical(verbose)) stop("The parameter 'verbose' must be supplied as a scalar logical vector.")

  data <- lapply(active_contract_tickers, function(x) {
    if (verbose) message(x)
    bbg_pull_historical_market(x, fields = dplyr::filter(fields, instrument == "futures", type == "aggregate") %>% dplyr::select(symbol) %>% purrr::flatten_chr(), start, end, ...) %>%
                   dplyr::mutate(`active contract ticker` = x)}
    ) %>%
    data.table::rbindlist(use.names = TRUE)

  data %<>%
    tidyr::gather(field, value, -c(`active contract ticker`, date)) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::arrange(`active contract ticker`, field, date) %>%
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01")) %>%
    dplyr::select(`active contract ticker`, field, date, value)

  if (nrow(data) == 0L) warning("No aggregate data found.")

  methods::new("FuturesAggregate",
      tickers = dplyr::distinct(data, `active contract ticker`) %>%
        purrr::flatten_chr(),
      fields = dplyr::distinct(data, `active contract ticker`, field) %>%
        data.table::as.data.table(),
      data = data %>%
        data.table::as.data.table(),
      call = call
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
#'     \item{\code{tickers}: a character vector. Active contract Bloomberg tickers for which data has been found.}
#'     \item{\code{fields}: a tibble. Columns include:
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
#'      }
#'    }
#'     \item{\code{data}: a tibble. Columns inlude:
#'       \itemize{
#'         \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'         \item{\code{format}: report formats for which data has been found.}
#'         \item{\code{underlying}: underlying instruments for which data has been found..}
#'         \item{\code{unit}: counting units for which data has been found.}
#'         \item{\code{participant}: trader classifications for which data has been found.}
#'         \item{\code{position}: trader positions for which data has been found.}
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

  call <- deparse(match.call())
  data(list = c("tickers_cftc"), package = "bbgsymbols", envir = environment())

  if (! is.character(active_contract_tickers)) stop("The parameter 'active_contract_tickers' must be supplied as a character vector of Bloomberg tickers.")
  if (! rlang::is_scalar_logical(verbose)) stop("The parameter 'verbose' must be supplied as a scalar logical vector.")

  data <- lapply(active_contract_tickers, function(x) {
    if (verbose) message(x)
    tickers <- dplyr::filter(tickers_cftc, `active contract ticker` == x) %>% dplyr::select(ticker) %>% purrr::flatten_chr()
    if (NROW(tickers) == 0L) stop(paste0("No CFTC data for ", x, "."))
    data <- bbg_pull_historical_market(tickers, fields = "PX_LAST", start, end, ...)

    data <- lapply(seq_along(tickers), function(y) {
      if(nrow(data[[y]]) == 0L) NULL
      else dplyr::mutate(data[[y]], ticker = tickers[y])
    }) %>%
      data.table::rbindlist(use.names = TRUE)

    data %>%
      dplyr::mutate(`active contract ticker` = x) %>%
      dplyr::left_join(dplyr::select(tickers_cftc, format, underlying, `unit` = unit, participant, position, ticker), by = "ticker")

  }) %>%
    data.table::rbindlist(use.names = TRUE) %>%
    dplyr::select(`active contract ticker`, format, underlying, `unit`, participant, position, date, value = PX_LAST) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01"))

  if (nrow(data) == 0L) warning("No CFTC data found.")

  methods::new("FuturesCFTC",
      tickers = dplyr::distinct(data, `active contract ticker`) %>%
        purrr::flatten_chr(),
      fields = dplyr::distinct(data, `active contract ticker`, format, underlying, `unit`, participant, position) %>%
        data.table::as.data.table(),
      data = data %>%
        data.table::as.data.table(),
      call = call
  )
}


#' Market historical data from Bloomberg
#'
#' @description Provided with a set of Bloomberg equity tickers and a time period,
#'   queries Bloomberg for the corresponding market historical data.
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

bbg_equity_market <- function(tickers = "NEM US Equity",
                              start = "2018-01-01",
                              end = "2018-06-30",
                              verbose = TRUE,
                              ...){

  call <- deparse(match.call())
  data(list = c("fields"), package = "bbgsymbols", envir = environment())

  if (! is.character(tickers)) stop("The parameter 'tickers' must be supplied as a character vector of Bloomberg tickers.")
  if (! rlang::is_scalar_logical(verbose)) stop("The parameter 'verbose' must be supplied as a scalar logical vector.")

  data <- lapply(tickers, function(x) {
    if (verbose) message(x)
    bbg_pull_historical_market(x, fields = dplyr::filter(fields, instrument == "equity", type == "market") %>% dplyr::select(symbol) %>% purrr::flatten_chr(),
                               start, end, ...) %>%
      dplyr::mutate(ticker = x)
  }) %>%
    data.table::rbindlist(use.names = TRUE)

  data %<>%
    tidyr::gather(field, value, -c(ticker, date)) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::select(ticker, field, date, value) %>%
    dplyr::mutate(date = as.Date(date, origin = "1970-01-01")) %>%
    dplyr::arrange(ticker, field, date)

  if (nrow(data) == 0L) warning("No market data found.")

  methods::new("EquityMarket",
      tickers = dplyr::distinct(data, ticker) %>%
        data.table::as.data.table(),
      fields = dplyr::distinct(data, ticker, field) %>%
        data.table::as.data.table(),
      data = data %>%
        data.table::as.data.table(),
      call = call
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
bbg_equity_books <- function(book = "key stats",
                             tickers = "NEM US Equity",
                             start = "2018-01-01",
                             end = "2018-06-30",
                             verbose = TRUE,
                             ...){

  call <- deparse(match.call())
  data(list = c("fields"), package = "bbgsymbols", envir = environment())

  if (! rlang::is_scalar_character(book) & book %in% c("key stats", "income statement", "balance sheet", "cash flow", "ratios"))
    stop("The parameter 'book' must be supplied as a scalar character vector, one of 'key stats', 'income statement', 'balance sheet', 'cash flow' or 'ratios'.")
  if (! is.character(tickers)) stop("The parameter 'tickers' must be supplied as a character vector of Bloomberg equity tickers.")
  if (! rlang::is_scalar_logical(verbose)) stop("The parameter 'verbose' must be supplied as a scalar logical vector (TRUE or FALSE).")
  if (! "periodicitySelection" %in% names(list(...))) utils::modifyList(list(...), list(periodicitySelection = "MONTHLY"))

  symbols <- dplyr::filter(fields, instrument == "equity", type == book)

  data <- lapply(dplyr::distinct(symbols, symbol) %>% purrr::flatten_chr(), function(x) {
    if (verbose) message(x)
    bbg_pull_historical_books(tickers = tickers, field = x, start, end, ...)
  }) %>%
    data.table::rbindlist(use.names = TRUE)

  data <- switch(book,
                 `key stats` = data %>%
                   dplyr::filter(stats::complete.cases(.)) %>%
                   dplyr::left_join(symbols %>%
                                      dplyr::select(name, symbol) %>%
                                      dplyr::mutate(name = forcats::as_factor(name)),
                                    by = c("field" = "symbol")) %>%
                   dplyr::arrange(ticker, name, date) %>%
                   dplyr::select(ticker, name, date, value),
                 `income statement` = data %>%
                   dplyr::filter(stats::complete.cases(.)) %>%
                   dplyr::left_join(symbols %>%
                                      dplyr::select(name, symbol) %>%
                                      dplyr::mutate(name = forcats::as_factor(name)),
                                    by = c("field" = "symbol")) %>%
                   dplyr::arrange(ticker, name, date) %>%
                   dplyr::select(ticker, name, date, value),
                 `balance sheet` = data %>%
                   dplyr::filter(stats::complete.cases(.)) %>%
                   dplyr::left_join(symbols %>%
                                      dplyr::select(section, subsection, name, symbol) %>%
                                      dplyr::mutate_at(dplyr::vars(section, name), dplyr::funs(forcats::as_factor(.))),
                                    by = c("field" = "symbol")) %>%
                   dplyr::arrange(ticker, section, subsection, name, date) %>%
                   dplyr::select(ticker, section, subsection, name, date, value),
                 `cash flow statement` = data %>%
                   dplyr::filter(stats::complete.cases(.)) %>%
                   dplyr::left_join(symbols %>%
                                      dplyr::select(section, name, symbol) %>%
                                      dplyr::mutate_at(dplyr::vars(section, name), dplyr::funs(forcats::as_factor(.))),
                                    by = c("field" = "symbol")) %>%
                   dplyr::arrange(ticker, section, name, date) %>%
                   dplyr::select(ticker, section, name, date, value),
                 `ratios` = data %>%
                   dplyr::filter(stats::complete.cases(.)) %>%
                   dplyr::left_join(symbols %>%
                                      dplyr::select(section, subsection, name, symbol) %>%
                                      dplyr::mutate_at(dplyr::vars(section, subsection, name), dplyr::funs(forcats::as_factor(.))),
                                    by = c("field" = "symbol")) %>%
                   dplyr::arrange(ticker, section, subsection, name, date) %>%
                   dplyr::select(ticker, section, subsection, name, date, value)
  )

  if (nrow(data) == 0L) warning(paste("No", book, "data found.", sep = " "))

  fields <-   switch(book,
                     `key stats` = dplyr::distinct(data, ticker, name) %>%
                       data.table::as.data.table(),
                     `income statement` = dplyr::distinct(data, ticker, name) %>%
                       data.table::as.data.table(),
                     `balance sheet` = dplyr::distinct(data, ticker, section, subsection, name) %>%
                       data.table::as.data.table(),
                     `cash flow statement` = dplyr::distinct(data, ticker, section, name) %>%
                       data.table::as.data.table(),
                     `ratios` = dplyr::distinct(data, ticker, section, subsection, name) %>%
                       data.table::as.data.table()
  )

  methods::new(dplyr::case_when(book == "key stats" ~ "EquityKS", book == "balance sheet" ~ "EquityBS",
                                book == "cash flow statement" ~ "EquityCF", book == "income statement" ~ "EquityIS",
                                book == "ratios" ~ "EquityRatios"),
               tickers = dplyr::distinct(data, ticker) %>% data.table::as.data.table(),
               fields = fields,
               data = data.table::as.data.table(data),
               call = call
  )
}

