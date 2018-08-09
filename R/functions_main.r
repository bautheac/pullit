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
#'     \item{\code{dataset}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'         \item{\code{TS position}: futures chain term structure positions for which data has been found.}
#'         \item{\code{field}: futures Bloomberg market data fields for which data has been found.}
#'         \item{\code{date}: observation date.}
#'         \item{\code{value}: corresponding value.}
#'       }
#'     }
#'   }
#'
#' @seealso The \code{\link[bbgsymbols]{fields}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg fields used here.
#'
#' @examples
#' \dontrun{bbg_futures_TS()}
#'
#' @import bbgsymbols
#' @importFrom data.table rbindlist
#' @importFrom dplyr distinct filter left_join mutate select
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom purrr flatten_chr
#' @importFrom stats complete.cases
#' @importFrom stringr str_extract
#' @importFrom tidyr gather
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
    data <- bbg_pull_historical(tickers, fields = filter(fields, instrument == "futures", type == "market") %>% select(symbol) %>% flatten_chr(),
                                start, end, ...)

    data <- if (length(tickers) > 1L) lapply(seq_along(TS_positions), function(x) {
      if (nrow(data[[x]]) > 0L) { data[[x]]$ticker <- tickers[x]; data[[x]]$`TS position` <- TS_positions[x] }
      data[[x]]
    }) %>%
      rbindlist(use.names = TRUE)
    else {
      if(nrow(data) < 1L) data[1L, ] <- rep(NA, ncol(data))
      else { data$ticker <- tickers; data$`TS position` <- TS_positions }
      data
    }
    data$`active contract ticker` <- y
    data
  }) %>%
    rbindlist(use.names = TRUE)

  data %<>%
    gather(field, value, -c(`active contract ticker`, ticker, `TS position`, date)) %>%
    select(`active contract ticker`, ticker, `TS position`, field, date, value) %>%
    filter(complete.cases(.))

  if (nrow(data) == 0L) warning("No term structure data found.")

  tickers <- distinct(data, `active contract ticker`, ticker, `TS position`) %>%
    mutate(`roll type` = str_extract(ticker, pattern = "(?<= )[ABDFNOR](?=:)"),
           `roll days` = str_extract(ticker, pattern = "(?<=:)\\d{2}(?=_)") %>% as.numeric(),
           `roll months` = str_extract(ticker, pattern = "(?<=:)\\d(?=_)") %>% as.numeric(),
           `roll adjustment` = str_extract(ticker, pattern = "(?<=_)[DNRW](?= )")) %>%
    left_join(filter(rolls, roll == "type") %>% select(symbol, name), by = c("roll type" = "symbol")) %>%
    select(`active contract ticker`, ticker, `TS position`, `roll type` = name, `roll days`, `roll months`, `roll adjustment`) %>%
    left_join(filter(rolls, roll == "adjustment") %>% select(symbol, name), by = c("roll type" = "symbol")) %>%
    select(`active contract ticker`, `TS position`, ticker, `roll type`, `roll days`, `roll months`, `roll adjustment` = name)

  .FuturesTS(tickers = tickers,
             fields = distinct(data, `active contract ticker`, `TS position`, field),
             dataset = data %>% select(-ticker)
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
#'     \item{\code{dataset}: a tibble. Columns inlude:
#'       \itemize{
#'         \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'         \item{\code{field}: Bloomberg aggregate data fields for which data has been found.}
#'         \item{\code{date}: observation date.}
#'         \item{\code{value}: corresponding value.}
#'       }
#'     }
#'   }
#'
#' @seealso The \code{\link[bbgsymbols]{fields}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg fields used here.
#'
#' @examples
#' \dontrun{bbg_futures_aggregate()}
#'
#' @import bbgsymbols
#' @importFrom data.table rbindlist
#' @importFrom dplyr distinct filter mutate select
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom purrr flatten_chr
#' @importFrom stats complete.cases
#' @importFrom tidyr gather
#'
#' @export

bbg_futures_aggregate <- function(active_contract_tickers = "C A Comdty",
                                  start = "2018-01-01",
                                  end = "2018-06-30",
                                  ...){

  if (! is.character(active_contract_tickers)) stop("The parameter 'active_contract_tickers' must be supplied as a character vector of Bloomberg tickers.")

  data <- lapply(active_contract_tickers, function(x) bbg_pull_historical(x, fields = filter(fields, instrument == "futures", type == "aggregate") %>% select(symbol) %>% flatten_chr(), start, end, ...) %>%
                   mutate(`active contract ticker` = x)) %>%
    rbindlist(use.names = TRUE)

  data %<>%
    gather(field, value, -c(`active contract ticker`, date)) %>%
    filter(complete.cases(.)) %>%
    select(`active contract ticker`, field, date, value)

  if (nrow(data) == 0L) warning("No aggregate data found.")

  .FuturesAggregate(tickers = distinct(data, `active contract ticker`) %>% flatten_chr(),
                    fields = distinct(data, `active contract ticker`, field),
                    dataset = data
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
#'     \item{\code{dataset}: a tibble. Columns inlude:
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
#'  }
#'
#' @seealso The \code{\link[bbgsymbols]{tickers_cftc}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg fields used here.
#'
#' @examples
#' \dontrun{bbg_futures_CFTC()}
#'
#' @import bbgsymbols
#' @importFrom data.table rbindlist
#' @importFrom dplyr everything distinct filter mutate select
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom purrr flatten_chr
#' @importFrom stats complete.cases
#' @importFrom tidyr gather
#'
#' @export

bbg_futures_CFTC <- function(active_contract_tickers = "C A Comdty",
                             start = "2018-01-01",
                             end = "2018-06-30",
                             ...){

  if (! is.character(active_contract_tickers)) stop("The parameter 'active_contract_tickers' must be supplied as a character vector of Bloomberg tickers.")

  data <- lapply(active_contract_tickers, function(x) {

    tickers <- filter(tickers_cftc, `active contract ticker` == y) %>% select(ticker) %>% flatten_chr()

    data <- bbg_pull_historical(tickers, fields = "PX_LAST", start, end, ...) %>%
      mutate(`active contract ticker` = x)

    data <- if (length(tickers) > 1L)
      lapply(seq_along(tickers), function(y) {
        if(nrow(data[[y]]) > 0L) data[[y]]$ticker <- tickers[y]
        data[[y]]
      }) %>%
      rbindlist(use.names = TRUE)
    else {
      if(nrow(data) < 1L) data[1L, ] <- rep(NA, ncol(data))
      data$ticker <- tickers
      data
    } %>%
      select(ticker, everything())

    data %>%
      left_join(select(tickers_cftc, format, underlying, `unit` = unit, participant, position), by = "ticker")

  }) %>%
    rbindlist(use.names = TRUE) %>%
    select(`active contract ticker`, format, underlying, `unit`, participant, position, date, value = PX_LAST) %>%
    filter(complete.cases(.))

  if (nrow(data) == 0L) warning("No CFTC data found.")

  .FuturesCFTC(tickers = distinct(data, `active contract ticker`) %>% flatten_chr(),
               fields = distinct(data, `active contract ticker`, format, underlying, `unit`, participant, position),
               dataset = data
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
#'     \item{\code{dataset}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'         \item{\code{field}: equity Bloomberg market data fields for which data has been found.}
#'         \item{\code{date}: observation date.}
#'         \item{\code{value}: corresponding value.}
#'       }
#'     }
#'   }
#'
#' @seealso The \code{\link[bbgsymbols]{fields}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg fields used here.
#'
#' @examples
#' \dontrun{bbg_equity_market()}
#'
#' @import bbgsymbols
#' @importFrom data.table rbindlist
#' @importFrom dplyr distinct filter mutate select
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom purrr flatten_chr
#' @importFrom stats complete.cases
#' @importFrom tidyr gather
#'
#' @export

bbg_equity_market <- function(tickers = "NEM US Equity",
                              start = "2018-01-01",
                              end = "2018-06-30",
                              ...){

  if (! is.character(tickers)) stop("The parameter 'tickers' must be supplied as a character vector of Bloomberg tickers.")

  data <- lapply(tickers, function(x) bbg_pull_historical(x,
                                                          fields = filter(fields, instrument == "equity", type == "market") %>%
                                                            select(symbol) %>% flatten_chr(),
                                                          start,
                                                          end,
                                                          ...) %>%
                   mutate(ticker = x)) %>%
    rbindlist(use.names = TRUE)

  data %<>%
    gather(field, value, -c(ticker, date)) %>%
    filter(complete.cases(.)) %>%
    select(ticker, field, date, value)

  if (nrow(data) == 0L) warning("No market data found.")

  .EquityMarket(tickers = distinct(data, ticker),
                fields = distinct(data, ticker, field),
                dataset = data
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
#'     \item{\code{dataset}: a tibble. Columns include:
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
#'   }
#'
#' @seealso The \code{\link[bbgsymbols]{fields}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg fields used here.
#'
#' @examples
#' \dontrun{bbg_equity_BS()}
#'
#' @import bbgsymbols
#' @importFrom data.table ":=" rbindlist
#' @importFrom dplyr arrange distinct filter group_by left_join mutate n row_number select ungroup
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom purrr flatten_chr
#' @importFrom stats complete.cases
#' @importFrom stringr str_detect
#' @importFrom tidyr gather
#'
#' @export

bbg_equity_BS <- function(tickers = "NEM US Equity",
                          start = "2018-01-01",
                          end = "2018-06-30",
                          ...){

  if (! is.character(tickers)) stop("The parameter 'tickers' must be supplied as a character vector of Bloomberg tickers.")

  symbols <- filter(fields, instrument == "equity", type == "balance sheet", ! str_detect(symbol, "\\s+[-+]\\s+")) %>%
    distinct(symbol) %>% flatten_chr()
  data <- lapply(tickers, function(x) bbg_pull_historical(x, fields = symbols, start, end, ...) %>%
                   mutate(ticker = x)) %>%
    rbindlist(use.names = TRUE)

  symbols <- filter(fields, instrument == "equity", type == "balance sheet", str_detect(symbol, "\\s+[-+]\\s+")) %>%
    distinct(symbol) %>% flatten_chr()

  for(i in symbols) data %<>% mutate(!! i := eval(parse(text = i)))

  data %<>%
    gather(field, value, -c(ticker, date)) %>%
    filter(complete.cases(.)) %>%
    left_join(filter(fields, instrument == "equity", type == "balance sheet") %>%
                group_by(section, subsection, symbol) %>%
                filter(row_number() == 1L) %>%
                ungroup() %>%
                select(section, subsection, rank, name, symbol),
              by = c("field = symbol")) %>%
    select(ticker, section, subsection, rank, name, field, date, value)

  data <- fields %>%
    filter(instrument == "equity", type == "balance sheet") %>%
    group_by(section, subsection, symbol) %>%
    filter(n() > 1L, row_number() == 2L) %>%
    ungroup() %>%
    select(section, subsection, rank, name, symbol) %>%
    left_join(data, by = c("symbol = field")) %>%
    rbind(data) %>%
    arrange(ticker, section, subsection, rank)

  if (nrow(data) == 0L) warning("No balance sheet data found.")

  .EquityBS(tickers = distinct(data, ticker) %>% flatten_chr(),
            fields = distinct(data, ticker, section, subsection, name),
            dataset = select(data, -c(rank, symbol))
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
#'     \item{\code{dataset}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'         \item{\code{section}: cash flow statement sections ('operating', 'financing', 'investing',
#'           'total') for which data has been found.}
#'         \item{\code{name}: cash flow statement Bloomberg field names for which data has been found.}
#'         \item{\code{date}: observation date.}
#'         \item{\code{value}: corresponding observation.}
#'       }
#'     }
#'   }
#'
#' @seealso The \code{\link[bbgsymbols]{fields}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg fields used here.
#'
#' @examples
#' \dontrun{bbg_equity_CF()}
#'
#' @import bbgsymbols
#' @importFrom data.table ":=" rbindlist
#' @importFrom dplyr arrange distinct filter group_by left_join mutate n row_number select ungroup
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom purrr flatten_chr
#' @importFrom stats complete.cases
#' @importFrom stringr str_detect
#' @importFrom tidyr gather
#'
#' @export

bbg_equity_CF <- function(tickers = "NEM US Equity",
                          start = "2018-01-01",
                          end = "2018-06-30",
                          ...){

  if (! is.character(tickers)) stop("The parameter 'tickers' must be supplied as a character vector of Bloomberg tickers.")

  symbols <- filter(fields, instrument == "equity", type == "cash flow", ! str_detect(symbol, "\\s+[-+]\\s+")) %>%
    distinct(symbol) %>% flatten_chr()
  data <- lapply(tickers, function(x) bbg_pull_historical(x, fields = symbols, start, end, ...) %>%
                   mutate(ticker = x)) %>%
    rbindlist(use.names = TRUE)

  symbols <- filter(fields, instrument == "equity", type == "cash flow", str_detect(symbol, "\\s+[-+]\\s+")) %>%
    distinct(symbol) %>% flatten_chr()

  for(i in symbols) data %<>% mutate(!! i := eval(parse(text = i)))

  data %<>%
    gather(field, value, -c(ticker, date)) %>%
    filter(complete.cases(.)) %>%
    left_join(filter(fields, instrument == "equity", type == "cash flow") %>%
                group_by(section, symbol) %>%
                filter(row_number() == 1L) %>%
                ungroup() %>%
                select(section, rank, name, symbol),
              by = c("field = symbol")) %>%
    select(ticker, section, rank, name, field, date, value)

  data <- fields %>%
    filter(instrument == "equity", type == "cash flow") %>%
    group_by(section, symbol) %>%
    filter(n() > 1L, row_number() == 2L) %>%
    ungroup() %>%
    select(section, rank, name, symbol) %>%
    left_join(data, by = c("symbol = field")) %>%
    rbind(data) %>%
    arrange(ticker, date, section, rank)

  if (nrow(data) == 0L) warning("No cash flow data found.")

  .EquityCF(tickers = distinct(data, ticker) %>% flatten_chr(),
            fields = distinct(data, ticker, section, name),
            dataset = select(data, -c(rank, symbol))
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
#'     \item{\code{dataset}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'         \item{\code{name}: income statement Bloomberg field names for which data has been found.}
#'         \item{\code{date}: observation date.}
#'         \item{\code{value}: corresponding observation.}
#'       }
#'     }
#'   }
#'
#' @seealso The \code{\link[bbgsymbols]{fields}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg fields used here.
#'
#' @examples
#' \dontrun{bbg_equity_IS()}
#'
#' @import bbgsymbols
#' @importFrom data.table ":=" rbindlist
#' @importFrom dplyr arrange distinct filter group_by left_join mutate n row_number select ungroup
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom purrr flatten_chr
#' @importFrom stats complete.cases
#' @importFrom stringr str_detect
#' @importFrom tidyr gather
#'
#' @export

bbg_equity_IS <- function(tickers = "NEM US Equity",
                          start = "2018-01-01",
                          end = "2018-06-30",
                          ...){

  if (! is.character(tickers)) stop("The parameter 'tickers' must be supplied as a character vector of Bloomberg tickers.")

  symbols <- filter(fields, instrument == "equity", type == "income statement", ! str_detect(symbol, "\\s+[-+]\\s+")) %>%
    distinct(symbol) %>% flatten_chr()
  data <- lapply(tickers, function(x) bbg_pull_historical(x, fields = symbols, start, end, ...) %>%
                   mutate(ticker = x)) %>%
    rbindlist(use.names = TRUE)

  symbols <- filter(fields, instrument == "equity", type == "income statement", str_detect(symbol, "\\s+[-+]\\s+")) %>%
    distinct(symbol) %>% flatten_chr()

  for(i in symbols) data %<>% mutate(!! i := eval(parse(text = i)))

  data %<>%
    gather(field, value, -c(ticker, date)) %>%
    filter(complete.cases(.)) %>%
    left_join(filter(fields, instrument == "equity", type == "income statement") %>%
                select(rank, name, symbol),
              by = c("field = symbol")) %>%
    select(ticker, rank, name, field, date, value)

  if (nrow(data) == 0L) warning("No income statement data found.")

  .EquityIS(tickers = distinct(data, ticker) %>% flatten_chr(),
            start = min(distinct(data, date)),
            end = max(distinct(data, date)),
            fields = distinct(data, ticker, name),
            dataset = select(data, -symbol)
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
#'     \item{\code{dataset}: a tibble. Columns include:
#'       \itemize{
#'         \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'         \item{\code{type}: financial ratio types ('profitability', 'margin', 'asset turnover',
#'           'short term liquidity', 'long term solvency') for which data has been found.}
#'         \item{\code{name}: financial ratio Bloomberg data field names for which data has been found.}
#'         \item{\code{date}: observation date.}
#'         \item{\code{value}: corresponding observation.}
#'       }
#'     }
#'   }
#'
#' @seealso The \code{\link[bbgsymbols]{fields}} dataset in the \code{bbgsymbols} package for details on the Bloomnerg fields used here.
#'
#' @examples
#' \dontrun{bbg_equity_ratios()}
#'
#' @import bbgsymbols
#' @importFrom data.table rbindlist
#' @importFrom dplyr arrange distinct filter left_join mutate select
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom purrr flatten_chr
#' @importFrom stats complete.cases
#' @importFrom stringr str_detect
#' @importFrom tidyr gather
#'
#' @export

bbg_equity_ratios <- function(tickers = "NEM US Equity",
                              start = "2018-01-01",
                              end = "2018-06-30",
                              ...){

  if (! is.character(tickers)) stop("The parameter 'tickers' must be supplied as a character vector of Bloomberg tickers.")

  symbols <- filter(fields, instrument == "equity", type == "ratios") %>%
    distinct(symbol) %>%
    flatten_chr()
  data <- lapply(tickers, function(x) bbg_pull_historical(x, fields = symbols, start, end, ...) %>%
                   mutate(ticker = x)) %>%
    rbindlist(use.names = TRUE)

  data %<>%
    gather(field, value, -c(ticker, date)) %>%
    filter(complete.cases(.)) %>%
    left_join(filter(fields, instrument == "equity", type == "ratios") %>%
                select(section, rank, name, symbol),
              by = c("field = symbol")) %>%
    select(ticker, type = section, rank, name, field, date, value)

  if (nrow(data) == 0L) warning("No ratios data found.")

  .EquityRatios(tickers = distinct(data, ticker) %>% flatten_chr(),
                fields = distinct(data, ticker, type, name),
                dataset = select(data, -c(rank, symbol))
  )
}





