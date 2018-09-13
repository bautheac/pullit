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

  if (! rlang::is_scalar_character(active_contract_ticker)) stop("The parameter 'active_contract_ticker' must be supplied as a scalar character vector")
  if (! rlang::is_scalar_integer(TS_position)) stop("The parameter 'TS_position' must be supplied as a scalar integer vector")
  if (! all(rlang::is_scalar_character(roll_type), roll_type %in% c("A", "B", "D", "F", "N", "O", "R")))
    stop("The parameter 'roll_type' must be one of 'A', 'B', 'D', 'F', 'N', 'O' or 'R'")
  if (! all(rlang::is_scalar_integer(roll_days), roll_days <= 31L)) stop("The parameter 'roll_days' must be supplied as a scalar integer vector between 0 and 31")
  if (! all(rlang::is_scalar_integer(roll_months), roll_months <= 12L)) stop("The parameter 'roll_months' must be supplied as a scalar integer vector between 0 and 12")
  if (! all(rlang::is_scalar_character(roll_adjustment), roll_adjustment %in% c("D", "N", "R", "W")))
    stop("The parameter 'roll_adjustment' must be one of 'D', 'N', 'R' or 'W'")

  split <- stringr::str_split(string = active_contract_ticker, pattern = "A ", simplify = FALSE) %>% purrr::flatten_chr()
  paste0(split[NROW(split) - 1L], TS_position, " ", roll_type, ":",
         dplyr::if_else(roll_days < 10L, paste0(0L, roll_days), paste0(roll_days)), "_",
         roll_months, "_", roll_adjustment, " ", split[NROW(split)])

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

  if (! is.character(tickers)) stop("The parameter 'tickers' must be supplied as a character vector of Bloomberg tickers")
  if (! is.character(fields)) stop("The parameter 'fields' must be supplied as a character vector of Bloomberg fields")
  if (! all(rlang::is_scalar_character(start), rlang::is_scalar_character(end), grepl(pattern = "^\\d{4}-\\d{2}-\\d{2}$", x = c(start, end))))
    stop("The parameters 'start' and 'end' must be supplied as scalar character vectors of dates in the following format: 'yyyy-mm-dd'")

  con <- tryCatch({
    Rblpapi::blpConnect()
  }, error = function(e) stop("Unable to connect Bloomberg. Please open a Bloomberg session on this terminal", call. = FALSE))

  bbg_pull <- Rblpapi::bdh(securities = tickers,
                           fields = fields,
                           start.date = as.Date(start),
                           end.date = as.Date(end),
                           int.as.double = TRUE,
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

  if (! is.character(tickers)) stop("The parameter 'tickers' must be supplied as a character vector of Bloomberg tickers")
  if (! is.character(field)) stop("The parameter 'fields' must be supplied as a character vector of Bloomberg fields")
  if (! all(rlang::is_scalar_character(start), rlang::is_scalar_character(end), grepl(pattern = "^\\d{4}-\\d{2}-\\d{2}$", x = c(start, end))))
    stop("The parameters 'start' and 'end' must be supplied as scalar character vectors of dates in the following format: 'yyyy-mm-dd'")

  con <- tryCatch({
    Rblpapi::blpConnect()
  }, error = function(e) stop("Unable to connect Bloomberg. Please open a Bloomberg session on this terminal", call. = FALSE))

  bbg_pull <- Rblpapi::bdh(securities = tickers,
                           field = field,
                           start.date = as.Date(start),
                           end.date = as.Date(end),
                           int.as.double = TRUE,
                           options = unlist(list(...)),
                           con = con)
  bbg_pull <- if (! is.data.frame(bbg_pull))
    lapply(names(bbg_pull), function(x) dplyr::mutate(bbg_pull[[x]], ticker = x)) %>%
    data.table::rbindlist(use.names = TRUE)
  else dplyr::mutate(bbg_pull, ticker = tickers)

  Rblpapi::blpDisconnect(con); bbg_pull %>% tidyr::gather(field, value, -c(date, ticker))

}








futures_groups <- function(file, ...){
  tickers <- NULL
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)
  if(!is.null(list(...)[["asset_class"]])){
    if (! all(is.character(list(...)[["asset_class"]]),
              list(...)[["asset_class"]] %in% RSQLite::dbGetQuery(con, "SELECT DISTINCT asset_class FROM tickers_futures;")$asset_class)){
      stop("The parameter 'asset_class' must be supplied as a character vector specifying one or more asset class(es) that exist(s) in table
           tickers_futures")
    } else {
      query <- paste0("SELECT DISTINCT symbol FROM tickers_futures WHERE asset_class IN ('",
                      paste(list(...)[["asset_class"]], collapse = "', '"), "');")
      tickers <- RSQLite::dbGetQuery(con, query)$symbol
    }
  }
  if(!is.null(list(...)[["sector"]])){
    if (! all(is.character(list(...)[["sector"]]),
              list(...)[["sector"]] %in% RSQLite::dbGetQuery(con, "SELECT DISTINCT sector FROM tickers_futures;")$sector)){
      stop("The parameter 'sector' must be supplied as a character vector specifying one or more sector(s) that exist(s) in table
           tickers_futures")
    } else {
      query <- paste0("SELECT DISTINCT symbol FROM tickers_futures WHERE sector IN ('",
                      paste(list(...)[["sector"]], collapse = "', '"), "');")
      if (is.null(tickers)) tickers <- RSQLite::dbGetQuery(con, query)$symbol
      else tickers <- intersect(tickers, RSQLite::dbGetQuery(con, query)$symbol)
    }
  }
  if(!is.null(list(...)[["subsector"]])){
    if (! all(is.character(list(...)[["subsector"]]),
              list(...)[["subsector"]] %in% RSQLite::dbGetQuery(con, "SELECT DISTINCT subsector FROM tickers_futures;")$subsector)){
      stop("The parameter 'subsector' must be supplied as a character vector specifying one or more subsector(s) that exist(s) in table
           tickers_futures")
    } else {
      query <- paste0("SELECT DISTINCT symbol FROM tickers_futures WHERE subsector IN ('",
                      paste(list(...)[["subsector"]], collapse = "', '"), "');")
      if (is.null(tickers)) tickers <- RSQLite::dbGetQuery(con, query)$symbol
      else tickers <- intersect(tickers, RSQLite::dbGetQuery(con, query)$symbol)
    }
  }
  if(!is.null(list(...)[["exchange"]])){
    if (! all(is.character(list(...)[["exchange"]]),
              list(...)[["exchange"]] %in% RSQLite::dbGetQuery(con, "SELECT DISTINCT symbol FROM support_exchanges;")$symbol)){
      stop("The parameter 'exchange' must be supplied as a character vector specifying one or more exchange(s) that exist(s) in table
           support_exhanges")
    } else {
      query <- paste0("(SELECT id FROM support_exchanges WHERE symbol IN ('",
                      paste(list(...)[["exchange"]], collapse = "', '"), "'))")
      query <- paste0("SELECT symbol FROM tickers_futures WHERE MIC_id IN (",
                      query, ");")
      if (is.null(tickers)) tickers <- RSQLite::dbGetQuery(con, query)$symbol
      else tickers <- intersect(tickers, RSQLite::dbGetQuery(con, query)$symbol)
    }
  }
  if(!is.null(list(...)[["currency"]])){
    if (! all(is.character(list(...)[["currency"]]),
              list(...)[["currency"]] %in% RSQLite::dbGetQuery(con, "SELECT DISTINCT symbol FROM support_currencies;")$symbol)){
      stop("The parameter 'currency' must be supplied as a character vector specifying one or more currency(ies) that exist(s) in table
           support_currencies")
    } else {
      query <- paste0("(SELECT id FROM support_exhanges WHERE symbol IN ('",
                      paste(list(...)[["currency"]], collapse = "', '"), "'))")
      query <- paste0("SELECT symbol FROM tickers_futures WHERE currency_id IN (",
                      query, ");")
      if (is.null(tickers)) tickers <- RSQLite::dbGetQuery(con, query)$symbol
      else tickers <- intersect(tickers, RSQLite::dbGetQuery(con, query)$symbol)
    }
  }
  RSQLite::dbDisconnect(con)
  tickers
}






equity_groups <- function(file, ...){

  tickers <- NULL
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)

  if(!is.null(list(...)[["type"]])){
    if (! all(is.character(list(...)[["type"]]),
              list(...)[["type"]] %in% RSQLite::dbGetQuery(con, "SELECT DISTINCT type FROM tickers_equity;")$type)){
      stop("The parameter 'type' must be supplied as a character vector specifying one or more security type(s) that exist(s) in table
           tickers_equity")
    } else {
      query <- paste0("SELECT DISTINCT symbol FROM tickers_equity WHERE type IN ('",
                      paste(list(...)[["type"]], collapse = "', '"), "');")
      tickers <- RSQLite::dbGetQuery(con, query)$symbol
    }
  }
  if(!is.null(list(...)[["exchange"]])){
    if (! all(is.character(list(...)[["exchange"]]),
              list(...)[["exchange"]] %in% RSQLite::dbGetQuery(con, "SELECT DISTINCT symbol FROM support_exchanges;")$symbol)){
      stop("The parameter 'exchange' must be supplied as a character vector specifying one or more exchange(s) that exist(s) in table
           support_exhanges")
    } else {
      query <- paste0("(SELECT id FROM support_exchanges WHERE symbol IN ('",
                      paste(list(...)[["exchange"]], collapse = "', '"), "'))")
      query <- paste0("SELECT symbol FROM tickers_equity WHERE MIC_id IN (",
                      query, ");")
      if (is.null(tickers)) tickers <- RSQLite::dbGetQuery(con, query)$symbol
      else tickers <- intersect(tickers, RSQLite::dbGetQuery(con, query)$symbol)
    }
  }
  if(!is.null(list(...)[["country"]])){
    if (! all(is.character(list(...)[["country"]]),
              list(...)[["country"]] %in% RSQLite::dbGetQuery(con, "SELECT DISTINCT symbol FROM support_countries;")$symbol)){
      stop("The parameter 'country' must be supplied as a character vector specifying one or more country(ies) that exist(s) in table
           support_countries")
    } else {
      query <- paste0("(SELECT id FROM support_countries WHERE symbol IN ('",
                      paste(list(...)[["country"]], collapse = "', '"), "'))")
      query <- paste0("SELECT symbol FROM tickers_equity WHERE country_id IN (",
                      query, ");")
      if (is.null(tickers)) tickers <- RSQLite::dbGetQuery(con, query)$symbol
      else tickers <- intersect(tickers, RSQLite::dbGetQuery(con, query)$symbol)
    }
  }
  if(!is.null(list(...)[["currency"]])){
    if (! all(is.character(list(...)[["currency"]]),
              list(...)[["currency"]] %in% RSQLite::dbGetQuery(con, "SELECT DISTINCT symbol FROM support_currencies;")$symbol)){
      stop("The parameter 'currency' must be supplied as a character vector specifying one or more currency(ies) that exist(s) in table
           support_currencies")
    } else {
      query <- paste0("(SELECT id FROM support_currencies WHERE symbol IN ('",
                      paste(list(...)[["currency"]], collapse = "', '"), "'))")
      query <- paste0("SELECT symbol FROM tickers_equity WHERE currency_id IN (",
                      query, ");")
      if (is.null(tickers)) tickers <- RSQLite::dbGetQuery(con, query)$symbol
      else tickers <- intersect(tickers, RSQLite::dbGetQuery(con, query)$symbol)
    }
  }
  if(!is.null(list(...)[["sector"]])){
    if (! all(is.character(list(...)[["sector"]]),
              list(...)[["sector"]] %in% RSQLite::dbGetQuery(con, "SELECT DISTINCT sector_name FROM support_GICS;")$symbol)){
      stop("The parameter 'sector' must be supplied as a character vector specifying one or more sector(s) that exist(s) in table
           support_GICS")
    } else {
      query <- paste0("(SELECT subindustry_id FROM support_GICS WHERE sector_name IN ('",
                      paste(list(...)[["sector"]], collapse = "', '"), "'))")
      query <- paste0("SELECT symbol FROM tickers_equity WHERE subindustry_id IN (",
                      query, ");")
      if (is.null(tickers)) tickers <- RSQLite::dbGetQuery(con, query)$symbol
      else tickers <- intersect(tickers, RSQLite::dbGetQuery(con, query)$symbol)
    }
  }
  if(!is.null(list(...)[["industry_group"]])){
    if (! all(is.character(list(...)[["industry_group"]]),
              list(...)[["industry_group"]] %in% RSQLite::dbGetQuery(con, "SELECT DISTINCT industry_group_name FROM support_GICS;")$symbol)){
      stop("The parameter 'industry_group' must be supplied as a character vector specifying one or more industry group(s) that exist(s) in table
           support_GICS")
    } else {
      query <- paste0("(SELECT subindustry_id FROM support_GICS WHERE industry_group_name IN ('",
                      paste(list(...)[["sector"]], collapse = "', '"), "'))")
      query <- paste0("SELECT symbol FROM tickers_equity WHERE subindustry_id IN (",
                      query, ");")
      if (is.null(tickers)) tickers <- RSQLite::dbGetQuery(con, query)$symbol
      else tickers <- intersect(tickers, RSQLite::dbGetQuery(con, query)$symbol)
    }
  }
  if(!is.null(list(...)[["industry"]])){
    if (! all(is.character(list(...)[["industry"]]),
              list(...)[["industry"]] %in% RSQLite::dbGetQuery(con, "SELECT DISTINCT industry_name FROM support_GICS;")$symbol)){
      stop("The parameter 'industry' must be supplied as a character vector specifying one or more industry(ies) that exist(s) in table
           support_GICS")
    } else {
      query <- paste0("(SELECT subindustry_id FROM support_GICS WHERE industry_name IN ('",
                      paste(list(...)[["industry"]], collapse = "', '"), "'))")
      query <- paste0("SELECT symbol FROM tickers_equity WHERE subindustry_id IN (",
                      query, ");")
      if (is.null(tickers)) tickers <- RSQLite::dbGetQuery(con, query)$symbol
      else tickers <- intersect(tickers, RSQLite::dbGetQuery(con, query)$symbol)
    }
  }
  if(!is.null(list(...)[["subindustry"]])){
    if (! all(is.character(list(...)[["subindustry"]]),
              list(...)[["subindustry"]] %in% RSQLite::dbGetQuery(con, "SELECT DISTINCT subindustry_name FROM support_GICS;")$symbol)){
      stop("The parameter 'subindustry' must be supplied as a character vector specifying one or more subindustry(ies) that exist(s)
            in table support_GICS")
    } else {
      query <- paste0("(SELECT subindustry_id FROM support_GICS WHERE subindustry_name IN ('",
                      paste(list(...)[["subindustry"]], collapse = "', '"), "'))")
      query <- paste0("SELECT symbol FROM tickers_equity WHERE subindustry_id IN (",
                      query, ");")
      if (is.null(tickers)) tickers <- RSQLite::dbGetQuery(con, query)$symbol
      else tickers <- intersect(tickers, RSQLite::dbGetQuery(con, query)$symbol)
    }
  }
  RSQLite::dbDisconnect(con)
  tickers
}

