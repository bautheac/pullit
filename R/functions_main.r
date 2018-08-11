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
#' @export

bbg_futures_TS <- function(active_contract_tickers = "C A Comdty",
                           start = "2018-01-01",
                           end = "2018-06-30",
                           TS_positions = 1L,
                           roll_type = "A",
                           roll_days = 0L,
                           roll_months = 0L,
                           roll_adjustment = "N",
                           ...){

  if (! is.character(active_contract_tickers)) stop("The parameter 'active_contract_tickers' must be supplied as a character vector of Bloomberg tickers.")
  if (! is.integer(TS_positions)) stop("The parameter 'TS_positions' must be supplied as a vector of integers.")

  data <- lapply(active_contract_tickers, function(y){

    tickers <- sapply(TS_positions, function(x) futures_ticker(y, TS_position = x, roll_type, roll_days, roll_months, roll_adjustment))
    data <- bbg_pull_historical(tickers, fields = dplyr::filter(fields, instrument == "futures", type == "market") %>% dplyr::select(symbol) %>% purrr::flatten_chr(),
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
    dplyr::filter(stats::complete.cases(.))

  if (nrow(data) == 0L) warning("No term structure data found.")

  tickers <- dplyr::distinct(data, `active contract ticker`, ticker, `TS position`) %>%
    dplyr::mutate(`roll type` = stringr::str_extract(ticker, pattern = "(?<= )[A-Z](?=:)"),
           `roll days` = stringr::str_extract(ticker, pattern = "(?<=:)\\d{2}(?=_)") %>% as.numeric(),
           `roll months` = stringr::str_extract(ticker, pattern = "(?<=:)\\d(?=_)") %>% as.numeric(),
           `roll adjustment` = stringr::str_extract(ticker, pattern = "(?<=_)[A-Z](?= )")) %>%
    dplyr::left_join(dplyr::filter(rolls, roll == "type") %>% dplyr::select(symbol, name), by = c("roll type" = "symbol")) %>%
    dplyr::select(`active contract ticker`, ticker, `TS position`, `roll type` = name, `roll days`, `roll months`, `roll adjustment`) %>%
    dplyr::left_join(dplyr::filter(rolls, roll == "adjustment") %>% dplyr::select(symbol, name), by = c("roll type" = "symbol")) %>%
    dplyr::select(`active contract ticker`, `TS position`, ticker, `roll type`, `roll days`, `roll months`, `roll adjustment` = name)

  methods::new("FuturesTS",
      tickers = tickers,
      fields = dplyr::distinct(data, `active contract ticker`, `TS position`, field),
      data = data %>% dplyr::select(-ticker),
      call = deparse(match.call())
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
                                  ...){

  if (! is.character(active_contract_tickers)) stop("The parameter 'active_contract_tickers' must be supplied as a character vector of Bloomberg tickers.")

  data <- lapply(active_contract_tickers, function(x) bbg_pull_historical(x, fields = dplyr::filter(fields, instrument == "futures", type == "aggregate") %>% dplyr::select(symbol) %>% purrr::flatten_chr(), start, end, ...) %>%
                   dplyr::mutate(`active contract ticker` = x)) %>%
    data.table::rbindlist(use.names = TRUE)

  data %<>%
    tidyr::gather(field, value, -c(`active contract ticker`, date)) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::select(`active contract ticker`, field, date, value)

  if (nrow(data) == 0L) warning("No aggregate data found.")

  methods::new("FuturesAggregate",
      tickers = dplyr::distinct(data, `active contract ticker`) %>% purrr::flatten_chr(),
      fields = dplyr::distinct(data, `active contract ticker`, field),
      data = data,
      call = deparse(match.call())
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
                             ...){

  if (! is.character(active_contract_tickers)) stop("The parameter 'active_contract_tickers' must be supplied as a character vector of Bloomberg tickers.")

  data <- lapply(active_contract_tickers, function(x) {

    tickers <- dplyr::filter(tickers_cftc, `active contract ticker` == y) %>% dplyr::select(ticker) %>% purrr::flatten_chr()

    data <- bbg_pull_historical(tickers, fields = "PX_LAST", start, end, ...) %>%
      dplyr::mutate(`active contract ticker` = x)

    data <- if (length(tickers) > 1L)
      lapply(seq_along(tickers), function(y) {
        if(nrow(data[[y]]) > 0L) data[[y]]$ticker <- tickers[y]
        data[[y]]
      }) %>%
      data.table::rbindlist(use.names = TRUE)
    else {
      if(nrow(data) < 1L) data[1L, ] <- rep(NA, ncol(data))
      data$ticker <- tickers
      data
    } %>%
      dplyr::select(ticker, dplyr::everything())

    data %>%
      dplyr::left_join(dplyr::select(tickers_cftc, format, underlying, `unit` = unit, participant, position), by = "ticker")

  }) %>%
    data.table::rbindlist(use.names = TRUE) %>%
    dplyr::select(`active contract ticker`, format, underlying, `unit`, participant, position, date, value = PX_LAST) %>%
    dplyr::filter(stats::complete.cases(.))

  if (nrow(data) == 0L) warning("No CFTC data found.")

  methods::new("FuturesCFTC",
      tickers = dplyr::distinct(data, `active contract ticker`) %>% purrr::flatten_chr(),
      fields = dplyr::distinct(data, `active contract ticker`, format, underlying, `unit`, participant, position),
      data = data,
      call = deparse(match.call())
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
                              ...){

  if (! is.character(tickers)) stop("The parameter 'tickers' must be supplied as a character vector of Bloomberg tickers.")

  data <- lapply(tickers, function(x) bbg_pull_historical(x,
                                                          fields = dplyr::filter(fields, instrument == "equity", type == "market") %>%
                                                            dplyr::select(symbol) %>% purrr::flatten_chr(),
                                                          start,
                                                          end,
                                                          ...) %>%
                   dplyr::mutate(ticker = x)) %>%
    data.table::rbindlist(use.names = TRUE)

  data %<>%
    tidyr::gather(field, value, -c(ticker, date)) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::select(ticker, field, date, value)

  if (nrow(data) == 0L) warning("No market data found.")

  methods::new("EquityMarket",
      tickers = dplyr::distinct(data, ticker),
      fields = dplyr::distinct(data, ticker, field),
      data = data,
      call = deparse(match.call())
  )
}


#' Balance sheet historical data from Bloomberg
#'
#' @description Provided with a set of Bloomberg equity tickers and a time period,
#'   queries Bloomberg for the corresponding balance sheet historical data.
#'
#' @param tickers A chatacter vector. Specifies the Bloomberg equity tickers to query data for.
#'   Defaults to 'NEM US Equity', the Bloomberg equity ticker for the Newmont Minning Corporation.
#' @param start A scalar character vector. Specifies the starting date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-01-01'.
#' @param end A scalar character vector. Specifies the end date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-06-30'.
#' @param ... Optional parameters to pass to the \code{\link[Rblpapi]{bdh}} function from the \code{Rblpapi} package used
#'   for the query (\code{options} parameter).
#'
#' @return An S4 object of class \code{\linkS4class{EquityBS}} with slots:
#'   \itemize{
#'     \item{\code{tickers}: a character vector. Equity Bloomberg tickers for which data has been found.}
#'     \item{\code{fields}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'         \item{\code{section}: balance sheet sections ('asset', 'liabilities') for which data has been found.}
#'         \item{\code{subsection}: balance sheet subsections ('asset': 'current', 'long term'; 'liabilities':
#'           'debt', 'equity') for which data has been found.}
#'         \item{\code{name}: balance sheet Bloomberg data field names for which data has been found.}
#'       }
#'     }
#'     \item{\code{data}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'         \item{\code{section}: balance sheet sections ('asset', 'liabilities') for which data has been found.}
#'         \item{\code{subsection}: balance sheet subsections ('asset': 'current', 'long term'; 'liabilities':
#'           'debt', 'equity') for which data has been found.}
#'         \item{\code{name}: balance sheet Bloomberg data field names for which data has been found.}
#'         \item{\code{date}: observation date.}
#'         \item{\code{value}: corresponding observation.}
#'       }
#'     }
#'     \item{\code{call}: a scalar character vector showing the original call to the constructor.}
#'   }
#'
#' @seealso The \code{\link[bbgsymbols]{fields}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg fields used here.
#'
#' @examples
#' \dontrun{bbg_equity_BS()}
#'
#' @import bbgsymbols
#' @importFrom data.table ":="
#' @importFrom magrittr "%>%" "%<>%"
#'
#' @export

bbg_equity_BS <- function(tickers = "NEM US Equity",
                          start = "2018-01-01",
                          end = "2018-06-30",
                          ...){

  if (! is.character(tickers)) stop("The parameter 'tickers' must be supplied as a character vector of Bloomberg tickers.")

  symbols <- dplyr::filter(fields, instrument == "equity", type == "balance sheet", ! stringr::str_detect(symbol, "\\s+[-+]\\s+")) %>%
    dplyr::distinct(symbol) %>% purrr::flatten_chr()
  data <- lapply(tickers, function(x) bbg_pull_historical(x, fields = symbols, start, end, ...) %>%
                   dplyr::mutate(ticker = x)) %>%
    data.table::rbindlist(use.names = TRUE)

  symbols <- dplyr::filter(fields, instrument == "equity", type == "balance sheet", stringr::str_detect(symbol, "\\s+[-+]\\s+")) %>%
    dplyr::distinct(symbol) %>% purrr::flatten_chr()

  for(i in symbols) data %<>% dplyr::mutate(!! i := eval(parse(text = i)))

  data %<>%
    tidyr::gather(field, value, -c(ticker, date)) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::left_join(dplyr::filter(fields, instrument == "equity", type == "balance sheet") %>%
                dplyr::group_by(section, subsection, symbol) %>%
                dplyr::filter(dplyr::row_number() == 1L) %>%
                dplyr::ungroup() %>%
                dplyr::select(section, subsection, rank, name, symbol),
              by = c("field = symbol")) %>%
    dplyr::select(ticker, section, subsection, rank, name, field, date, value)

  data <- fields %>%
    dplyr::filter(instrument == "equity", type == "balance sheet") %>%
    dplyr::group_by(section, subsection, symbol) %>%
    dplyr::filter(dplyr::n() > 1L, dplyr::row_number() == 2L) %>%
    dplyr::ungroup() %>%
    dplyr::select(section, subsection, rank, name, symbol) %>%
    dplyr::left_join(data, by = c("symbol = field")) %>%
    rbind(data) %>%
    dplyr::arrange(ticker, section, subsection, rank)

  if (nrow(data) == 0L) warning("No balance sheet data found.")

  methods::new("EquityBS",
      tickers = dplyr::distinct(data, ticker) %>% purrr::flatten_chr(),
      fields = dplyr::distinct(data, ticker, section, subsection, name),
      data = dplyr::select(data, -c(rank, symbol)),
      call = deparse(match.call())
  )

}




#' Cash flow statement historical data from Bloomberg
#'
#' @description Provided with a set of Bloomberg equity tickers and a time period,
#'   queries Bloomberg for the corresponding cash flow statement historical data.
#'
#' @param tickers A chatacter vector. Specifies the Bloomberg equity tickers to query data for.
#'   Defaults to 'NEM US Equity', the Bloomberg equity ticker for the Newmont Minning Corporation.
#' @param start A scalar character vector. Specifies the starting date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-01-01'.
#' @param end A scalar character vector. Specifies the end date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-06-30'.
#' @param ... Optional parameters to pass to the \code{\link[Rblpapi]{bdh}} function from the \code{Rblpapi} package used
#'   for the query (\code{options} parameter).
#'
#' @return An S4 object of class \code{\linkS4class{EquityCF}} with slots:
#'   \itemize{
#'     \item{\code{tickers}: a character vector. Equity Bloomberg tickers for which data has been found.}
#'     \item{\code{fields}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'         \item{\code{section}: cash flow statement sections ('operating', 'financing', 'investing',
#'           'total') for which data has been found.}
#'         \item{\code{name}: cash flow statement Bloomberg field names for which data has been found.}
#'       }
#'     }
#'     \item{\code{data}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'         \item{\code{section}: cash flow statement sections ('operating', 'financing', 'investing',
#'           'total') for which data has been found.}
#'         \item{\code{name}: cash flow statement Bloomberg field names for which data has been found.}
#'         \item{\code{date}: observation date.}
#'         \item{\code{value}: corresponding observation.}
#'       }
#'     }
#'     \item{\code{call}: a scalar character vector showing the original call to the constructor.}
#'   }
#'
#' @seealso The \code{\link[bbgsymbols]{fields}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg fields used here.
#'
#' @examples
#' \dontrun{bbg_equity_CF()}
#'
#' @import bbgsymbols
#' @importFrom data.table ":="
#' @importFrom magrittr "%>%" "%<>%"
#'
#' @export

bbg_equity_CF <- function(tickers = "NEM US Equity",
                          start = "2018-01-01",
                          end = "2018-06-30",
                          ...){

  if (! is.character(tickers)) stop("The parameter 'tickers' must be supplied as a character vector of Bloomberg tickers.")

  symbols <- dplyr::filter(fields, instrument == "equity", type == "cash flow", ! stringr::str_detect(symbol, "\\s+[-+]\\s+")) %>%
    dplyr::distinct(symbol) %>% purrr::flatten_chr()
  data <- lapply(tickers, function(x) bbg_pull_historical(x, fields = symbols, start, end, ...) %>%
                   dplyr::mutate(ticker = x)) %>%
    data.table::rbindlist(use.names = TRUE)

  symbols <- dplyr::filter(fields, instrument == "equity", type == "cash flow", stringr::str_detect(symbol, "\\s+[-+]\\s+")) %>%
    dplyr::distinct(symbol) %>% purrr::flatten_chr()

  for(i in symbols) data %<>% dplyr::mutate(!! i := eval(parse(text = i)))

  data %<>%
    tidyr::gather(field, value, -c(ticker, date)) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::left_join(dplyr::filter(fields, instrument == "equity", type == "cash flow") %>%
                       dplyr::group_by(section, symbol) %>%
                       dplyr::filter(dplyr::row_number() == 1L) %>%
                       dplyr::ungroup() %>%
                  dplyr::select(section, rank, name, symbol),
              by = c("field = symbol")) %>%
    dplyr::select(ticker, section, rank, name, field, date, value)

  data <- fields %>%
    dplyr::filter(instrument == "equity", type == "cash flow") %>%
    dplyr::group_by(section, symbol) %>%
    dplyr::filter(dplyr::n() > 1L, dplyr::row_number() == 2L) %>%
    dplyr::ungroup() %>%
    dplyr::select(section, rank, name, symbol) %>%
    dplyr::left_join(data, by = c("symbol = field")) %>%
    rbind(data) %>%
    dplyr::arrange(ticker, date, section, rank)

  if (nrow(data) == 0L) warning("No cash flow data found.")

  methods::new("EquityCF",
      tickers = dplyr::distinct(data, ticker) %>% purrr::flatten_chr(),
      fields = dplyr::distinct(data, ticker, section, name),
      data = dplyr::select(data, -c(rank, symbol)),
      call = deparse(match.call())
  )

}



#' Income statement historical data from Bloomberg
#'
#' @description Provided with a set of Bloomberg equity tickers and a time period,
#'   queries Bloomberg for the corresponding income statement historical data.
#'
#' @param tickers A chatacter vector. Specifies the Bloomberg equity tickers to query data for.
#'   Defaults to 'NEM US Equity', the Bloomberg equity ticker for the Newmont Minning Corporation.
#' @param start A scalar character vector. Specifies the starting date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-01-01'.
#' @param end A scalar character vector. Specifies the end date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-06-30'.
#' @param ... Optional parameters to pass to the \code{\link[Rblpapi]{bdh}} function from the \code{Rblpapi} package used
#'   for the query (\code{options} parameter).
#'
#' @return An S4 object of class \code{\linkS4class{EquityIS}} with slots:
#'   \itemize{
#'     \item{\code{tickers}: a character vector. Equity Bloomberg tickers for which data has been found.}
#'     \item{\code{fields}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'         \item{\code{name}: income statement Bloomberg field names for which data has been found.}
#'       }
#'     }
#'     \item{\code{data}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'         \item{\code{name}: income statement Bloomberg field names for which data has been found.}
#'         \item{\code{date}: observation date.}
#'         \item{\code{value}: corresponding observation.}
#'       }
#'     }
#'     \item{\code{call}: a scalar character vector showing the original call to the constructor.}
#'   }
#'
#' @seealso The \code{\link[bbgsymbols]{fields}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg fields used here.
#'
#' @examples
#' \dontrun{bbg_equity_IS()}
#'
#' @import bbgsymbols
#' @importFrom data.table ":="
#' @importFrom magrittr "%>%" "%<>%"
#'
#' @export

bbg_equity_IS <- function(tickers = "NEM US Equity",
                          start = "2018-01-01",
                          end = "2018-06-30",
                          ...){

  if (! is.character(tickers)) stop("The parameter 'tickers' must be supplied as a character vector of Bloomberg tickers.")

  symbols <- dplyr::filter(fields, instrument == "equity", type == "income statement", ! stringr::str_detect(symbol, "\\s+[-+]\\s+")) %>%
    dplyr::distinct(symbol) %>% purrr::flatten_chr()
  data <- lapply(tickers, function(x) bbg_pull_historical(x, fields = symbols, start, end, ...) %>%
                   dplyr::mutate(ticker = x)) %>%
    data.table::rbindlist(use.names = TRUE)

  symbols <- dplyr::filter(fields, instrument == "equity", type == "income statement", stringr::str_detect(symbol, "\\s+[-+]\\s+")) %>%
    dplyr::distinct(symbol) %>% purrr::flatten_chr()

  for(i in symbols) data %<>% dplyr::mutate(!! i := eval(parse(text = i)))

  data %<>%
    tidyr::gather(field, value, -c(ticker, date)) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::left_join(dplyr::filter(fields, instrument == "equity", type == "income statement") %>%
                dplyr::select(rank, name, symbol),
              by = c("field = symbol")) %>%
    dplyr::select(ticker, rank, name, field, date, value)

  if (nrow(data) == 0L) warning("No income statement data found.")

  methods::new("EquityIS",
               tickers = dplyr::distinct(data, ticker) %>% purrr::flatten_chr(),
               start = min(dplyr::distinct(data, date)),
               end = max(dplyr::distinct(data, date)),
               fields = dplyr::distinct(data, ticker, name),
               data = dplyr::select(data, -symbol),
               call = deparse(match.call())
               )

}






#' Financial ratios historical data from Bloomberg
#'
#' @description Provided with a set of Bloomberg equity tickers and a time period,
#'   queries Bloomberg for the corresponding financial ratios historical data.
#'
#' @param tickers A chatacter vector. Specifies the Bloomberg equity tickers to query data for.
#'   Defaults to 'NEM US Equity', the Bloomberg equity ticker for the Newmont Minning Corporation.
#' @param start A scalar character vector. Specifies the starting date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-01-01'.
#' @param end A scalar character vector. Specifies the end date for the query in the following format: 'yyyy-mm-dd'.
#'   Defaults to '2018-06-30'.
#' @param ... Optional parameters to pass to the \code{\link[Rblpapi]{bdh}} function from the \code{Rblpapi} package used
#'   for the query (\code{options} parameter).
#'
#' @return An S4 object of class \code{\linkS4class{EquityRatios}} with slots:
#'   \itemize{
#'     \item{\code{tickers}: a character vector. Equity Bloomberg tickers for which data has been found.}
#'     \item{\code{fields}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'         \item{\code{type}: financial ratio types ('profitability', 'margin', 'asset turnover',
#'           'short term liquidity', 'long term solvency') for which data has been found.}
#'         \item{\code{name}: financial ratio Bloomberg data field names for which data has been found.}
#'       }
#'     }
#'     \item{\code{data}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'         \item{\code{type}: financial ratio types ('profitability', 'margin', 'asset turnover',
#'           'short term liquidity', 'long term solvency') for which data has been found.}
#'         \item{\code{name}: financial ratio Bloomberg data field names for which data has been found.}
#'         \item{\code{date}: observation date.}
#'         \item{\code{value}: corresponding observation.}
#'       }
#'     }
#'     \item{\code{call}: a scalar character vector showing the original call to the constructor.}
#'   }
#'
#' @seealso The \code{\link[bbgsymbols]{fields}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg fields used here.
#'
#' @examples
#' \dontrun{bbg_equity_ratios()}
#'
#' @import bbgsymbols
#' @importFrom magrittr "%>%" "%<>%"
#'
#' @export

bbg_equity_ratios <- function(tickers = "NEM US Equity",
                              start = "2018-01-01",
                              end = "2018-06-30",
                              ...){

  if (! is.character(tickers)) stop("The parameter 'tickers' must be supplied as a character vector of Bloomberg tickers.")

  symbols <- dplyr::filter(fields, instrument == "equity", type == "ratios") %>%
    dplyr::distinct(symbol) %>%
    purrr::flatten_chr()
  data <- lapply(tickers, function(x) bbg_pull_historical(x, fields = symbols, start, end, ...) %>%
                   dplyr::mutate(ticker = x)) %>%
    data.table::rbindlist(use.names = TRUE)

  data %<>%
    tidyr::gather(field, value, -c(ticker, date)) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::left_join(dplyr::filter(fields, instrument == "equity", type == "ratios") %>%
                       dplyr::select(section, rank, name, symbol),
              by = c("field = symbol")) %>%
    dplyr::select(ticker, type = section, rank, name, field, date, value)

  if (nrow(data) == 0L) warning("No ratios data found.")

  methods::new("EquityRatios",
               tickers = dplyr::distinct(data, ticker) %>% purrr::flatten_chr(),
               fields = dplyr::distinct(data, ticker, type, name),
               data = dplyr::select(data, -c(rank, symbol)),
               call = deparse(match.call())
               )

}





