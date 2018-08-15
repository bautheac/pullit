#' Bloomberg futures term structure ticker
#'
#' @description Provided with a futures active contract ticker, a term structure position and a set of roll parameters, constructs the
#'   corresponding futures term structure Bloomberg ticker according Bloomberg futures term structure ticker construction method.
#'
#' @param active_contract_ticker A scalar chatacter vector. Specifies the Bloomberg futures active contract ticker to use for term structure ticker construction.
#'   Defaults to 'C A Comdty', the Bloomberg active contract ticker for the XCBT corn futures series.
#' @param TS_position A scalar integer vector. Specifies the term structure position desired. Defaults to 1: front nearby contract for the corresponding futures series.
#' @param roll_type A scalar chatacter vector. Specifies roll type to use for term structure ticker construction. Must be one of 'A', 'B', 'D', 'F', 'N', 'O' or 'R'.
#'   Defaults to 'A' or 'With active future': rolls to the most actively traded contract in the futures series.
#' @param roll_days A scalar integer vector. Specifies the day the roll should be done. Refers to the day of the month (\code{roll_type} = 'F') or
#'   the number of days before a reference date (\code{roll_type} = 'D', \code{roll_type} = 'N', \code{roll_type} = 'O', \code{roll_type} = 'R'). Works in tandem with `roll_months` below.
#'   Defaults to 0.
#' @param roll_months A scalar integer vector. Specifies the month the roll should be done. Refers to the number of months before a
#'   reference date (\code{roll_type} = 'D', \code{roll_type} = 'N', \code{roll_type} = 'O', \code{roll_type} = 'R'). Works in tandem with `roll_days` above.
#'   Defaults to 0.
#' @param roll_adjustment A scalar chatacter vector. Specifies roll adjustment method to use for term structure ticker construction.
#'   Must be one of 'D', 'N', 'R', or 'W'. Defaults to 'N' or 'None'.
#'
#' @return A scalar character vector containing the corresponding term structure ticker.
#'
#' @seealso \code{\link{bbg_futures_TS}}
#'
#' @examples futures_ticker(active_contract_ticker = "C A Comdty", TS_position = 5L,
#'   roll_type = "A", roll_days = 0L, roll_months = 0L, roll_adjustment = "N")
#'
#' @details See 'DOCS #2072138 <GO>' on a Bloomberg terminal to learn more about the Bloomberg rolling conventions.
#'
#' @export

futures_ticker <- function(active_contract_ticker = "C A Comdty",
                           TS_position = 1L,
                           roll_type = "A",
                           roll_days = 0L,
                           roll_months = 0L,
                           roll_adjustment = "N"){

  if (! rlang::is_scalar_character(active_contract_ticker)) stop("The parameter 'active_contract_ticker' must be supplied as a scalar character vector.")
  if (! rlang::is_scalar_integer(TS_position)) stop("The parameter 'TS_position' must be supplied as a scalar integer vector.")
  if (! all(rlang::is_scalar_character(roll_type), roll_type %in% c("A", "B", "D", "F", "N", "O", "R")))
    stop("The parameter 'roll_type' must be one of 'A', 'B', 'D', 'F', 'N', 'O' or 'R'.")
  if (! all(rlang::is_scalar_integer(roll_days), roll_days <= 31L)) stop("The parameter 'roll_days' must be supplied as a scalar integer vector between 0 and 31.")
  if (! all(rlang::is_scalar_integer(roll_months), roll_months <= 12L)) stop("The parameter 'roll_months' must be supplied as a scalar integer vector between 0 and 12.")
  if (! all(rlang::is_scalar_character(roll_adjustment), roll_adjustment %in% c("D", "N", "R", "W")))
    stop("The parameter 'roll_adjustment' must be one of 'D', 'N', 'R' or 'W'.")

  split <- stringr::str_split(string = active_contract_ticker, pattern = "A ", simplify = FALSE) %>% purrr::flatten_chr()
  paste0(split[NROW(split) - 1L], TS_position, " ", roll_type, ":", dplyr::if_else(roll_days < 10L, paste0(0L, roll_days), paste0(roll_days)), "_", roll_months, "_", roll_adjustment, " ", split[NROW(split)])

}



#' Pull historical market data from Bloomberg
#'
#' @param tickers A character vector. Specifies the Bloomberg tickers for the query.
#' @param fields A character vector. Specifies the Bloomberg fields for the query.
#' @param start A scalar character vector. Specifies the starting date for the query
#'   in the following format: 'yyyy-mm-dd'.
#' @param end A scalar character vector. Specifies the end date for the query in the
#'   following format: 'yyyy-mm-dd'.
#' @param ... Optional parameters to pass to \code{\link[Rblpapi]{bdh}} function from the \code{Rblpapi} package used
#'   for the query (\code{options} parameter).
bbg_pull_historical_market <- function(tickers, fields, start, end, ...){

  if (! is.character(tickers)) stop("The parameter 'tickers' must be supplied as a character vector of Bloomberg tickers.")
  if (! is.character(fields)) stop("The parameter 'fields' must be supplied as a character vector of Bloomberg fields.")
  if (! all(rlang::is_scalar_character(start), rlang::is_scalar_character(end), grepl(pattern = "^\\d{4}-\\d{2}-\\d{2}$", x = c(start, end))))
    stop("The parameters 'start' and 'end' must be supplied as scalar character vectors of dates in the following format: 'yyyy-mm-dd'")

  con <- tryCatch({
    Rblpapi::blpConnect()
  }, error = function(e) stop("Unable to connect Bloomberg. Please open a Bloomberg session on this terminal.", call. = FALSE))

  bbg_pull <- Rblpapi::bdh(securities = tickers,
                           fields = fields,
                           start.date = as.Date(start),
                           end.date = as.Date(end),
                           options = ...,
                           con = con)
  Rblpapi::blpDisconnect(con); bbg_pull

}



#' Pull historical book data from Bloomberg
#'
#' @param tickers A character vector. Specifies the Bloomberg tickers for the query
#' @param field A character vector. Specifies the Bloomberg field for the query.
#' @param start A scalar character vector. Specifies the starting date for the query
#'   in the following format: 'yyyy-mm-dd'.
#' @param end A scalar character vector. Specifies the end date for the query in the
#'   following format: 'yyyy-mm-dd'.
#' @param ... Optional parameters to pass to \code{\link[Rblpapi]{bdh}} function from the \code{Rblpapi} package used
#'   for the query (\code{options} parameter).
bbg_pull_historical_books <- function(tickers, field, start, end, ...){

  if (! is.character(tickers)) stop("The parameter 'tickers' must be supplied as a character vector of Bloomberg tickers.")
  if (! is.character(field)) stop("The parameter 'fields' must be supplied as a character vector of Bloomberg fields.")
  if (! all(rlang::is_scalar_character(start), rlang::is_scalar_character(end), grepl(pattern = "^\\d{4}-\\d{2}-\\d{2}$", x = c(start, end))))
    stop("The parameters 'start' and 'end' must be supplied as scalar character vectors of dates in the following format: 'yyyy-mm-dd'")

  con <- tryCatch({
    Rblpapi::blpConnect()
  }, error = function(e) stop("Unable to connect Bloomberg. Please open a Bloomberg session on this terminal.", call. = FALSE))

  bbg_pull <- Rblpapi::bdh(securities = tickers,
                           field = field,
                           start.date = as.Date(start),
                           end.date = as.Date(end),
                           options = unlist(list(...)),
                           con = con)
  bbg_pull <- if (! is.data.frame(bbg_pull))
    lapply(names(bbg_pull), function(x) dplyr::mutate(bbg_pull[[x]], ticker = x)) %>%
    data.table::rbindlist(use.names = TRUE)
  else dplyr::mutate(bbg_pull, ticker = tickers)

  Rblpapi::blpDisconnect(con); bbg_pull %>% tidyr::gather(field, value, -c(date, ticker))

}


