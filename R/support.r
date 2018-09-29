# Bloomberg ####

## historical ####

### market ####

#' Pull historical market data from Bloomberg
#'
#' @param tickers A character vector. Specifies the Bloomberg tickers for the query.
#' @param fields A character vector. Specifies the Bloomberg fields for the query.
#' @param start A scalar character vector. Specifies the starting date for the query
#'   in the following format: 'yyyy-mm-dd'.
#' @param end A scalar character vector. Specifies the end date for the query in the
#'   following format: 'yyyy-mm-dd'.
#' @param ... Optional parameters to pass to the \link[Rblpapi]{bdp} function from the
#' \href{http://dirk.eddelbuettel.com/code/rblpapi.html}{\pkg{Rblpapi}} package used
#'   for the query (\code{options} parameter).
BBG_pull_historical_market <- function(tickers, fields, start, end, ...){

  con <- tryCatch({
    Rblpapi::blpConnect()
  }, error = function(e) stop("Unable to connect Bloomberg. Please open a Bloomberg session on this terminal", call. = FALSE))

  query <- Rblpapi::bdh(securities = tickers, fields = fields, start.date = as.Date(start), end.date = as.Date(end), int.as.double = TRUE,
                        options = ..., con = con)
  Rblpapi::blpDisconnect(con); query

}


### book ####

#' Pull historical book data from Bloomberg
#'
#' @param tickers A character vector. Specifies the Bloomberg tickers for the query
#' @param field A character vector. Specifies the Bloomberg field for the query.
#' @param start A scalar character vector. Specifies the starting date for the query
#'   in the following format: 'yyyy-mm-dd'.
#' @param end A scalar character vector. Specifies the end date for the query in the
#'   following format: 'yyyy-mm-dd'.
#' @param ... Optional parameters to pass to the \link[Rblpapi]{bdp} function from the
#' \href{http://dirk.eddelbuettel.com/code/rblpapi.html}{\pkg{Rblpapi}} package used
#'   for the query (\code{options} parameter).
BBG_pull_historical_books <- function(tickers, field, start, end, ...){

  con <- tryCatch({
    Rblpapi::blpConnect()
  }, error = function(e)
    stop("Unable to connect Bloomberg. Please open a Bloomberg session on this terminal",
         call. = FALSE))

  query <- Rblpapi::bdh(securities = tickers, field = field, start.date = as.Date(start),
                        end.date = as.Date(end), int.as.double = TRUE,
                        options = unlist(list(...)), con = con)
  query <- if (! is.data.frame(query))
    lapply(names(query), function(x) dplyr::mutate(query[[x]], ticker = x)) %>%
    data.table::rbindlist(use.names = TRUE)
  else dplyr::mutate(query, ticker = tickers)

  Rblpapi::blpDisconnect(con); tidyr::gather(query, field, value, -c(date, ticker))

}






# graphics ####
bulletize <- function(line, bullet = "*") paste0(bullet, " ", line)

done <- function(..., .envir = parent.frame()) {
  out <- glue::glue(..., .envir = .envir)
  cat(bulletize(out, bullet = done_bullet()), "\n", sep = "")
}

done_bullet <- function() crayon::green(clisymbols::symbol$tick)







# storethat ####

## update ####

### futures ####
storethat_update_futures <- function(book, names, ...){

  switch(book,

         market = storethat_update_futures_market(names, ...),

         CFTC = storethat_update_futures_CFTC(names, ...),

         info = storethat_update_info(instrument = "futures", names, ...),

         all = {

           storethat_update_futures_market(names, ...)
           storethat_update_futures_CFTC(names, ...)
           storethat_update_info(instrument = "futures", names, ...)

         }
  )
}


#### market ####
storethat_update_futures_market <- function(names, ...){


  data <- lapply(names$ticker, function(x)
    storethat::db_snapshot(file = list(...)[["file"]], instrument = "futures", book = "market", name = x)
  ) %>% data.table::rbindlist()


  storethat_update_futures_ts(tickers = dplyr::filter(data, ! is.na(ticker))$ticker,
                              start = min(as.Date(dplyr::filter(data, ! is.na(ticker))$end)),
                              ...)


  storethat_update_futures_aggregate(tickers = dplyr::filter(data, is.na(ticker))$`active contract ticker`,
                                     start = min(as.Date(dplyr::filter(data, is.na(ticker))$end)),
                                     ...)
}


##### term structure ####
storethat_update_futures_ts <- function(tickers, start, ...){


  op <- options(); options(dplyr.show_progress = TRUE)


  tickers <- paste0("SELECT active_contract_ticker_id, ticker AS term_structure_ticker,
                    position, roll_type_symbol, roll_days, roll_months, roll_adjustment_symbol
                    FROM tickers_support_futures_ts WHERE ticker IN ('",
                    paste(tickers, collapse = "', '"), "')")
  tickers <- paste0("SELECT ticker AS active_contract_ticker, term_structure_ticker, position,
                    roll_type_symbol, roll_days, roll_months, roll_adjustment_symbol FROM (",
                    tickers, ") A LEFT JOIN tickers_futures B ON A.active_contract_ticker_id =
                    B.id")
  con <- RSQLite::dbConnect(RSQLite::SQLite(), list(...)[["file"]])
  tickers <- RSQLite::dbGetQuery(con = con, tickers)
  RSQLite::dbDisconnect(con)


  tickers <- dplyr::group_by(tickers, roll_type_symbol, roll_days, roll_months, roll_adjustment_symbol) %>%
    dplyr::do({


      if (list(...)[["verbose"]]) message("Retrieving futures term structure data from Bloomberg.\n")

      term_structure <- BBG_futures_market(type = "term structure",
                                           active_contract_tickers = unique(.$active_contract_ticker),
                                           start = start, end = Sys.Date(), TS_positions = unique(.$position),
                                           roll_type = unique(.$roll_type_symbol),
                                           roll_days = unique(.$roll_days), roll_months = unique(.$roll_months),
                                           roll_adjustment = unique(.$roll_adjustment_symbol),
                                           verbose = list(...)[["verbose"]])


      if (list(...)[["verbose"]]) message("\nUpdating database with retrieved data.\n")

      storethat::db_store(file = list(...)[["file"]], term_structure, verbose = list(...)[["verbose"]])
      tibble::tibble(tickers = unique(.$term_structure_ticker))
    })


  options(op)
}


##### aggregate ####
storethat_update_futures_aggregate <- function(tickers, start, ...){


  if (list(...)[["verbose"]]) message("\n\nRetrieving futures aggregate data from Bloomberg.\n")

  aggregate <- BBG_futures_market(type = "aggregate", active_contract_tickers = unique(tickers),
                                  start = start, end = Sys.Date(), verbose = list(...)[["verbose"]])


  if (list(...)[["verbose"]]) message("\nUpdating database with retrieved data.\n")

  storethat::db_store(file = list(...)[["file"]], aggregate, verbose = list(...)[["verbose"]])

}


#### CFTC ####
storethat_update_futures_CFTC <- function(names, ...){

  data <- lapply(names$ticker, function(x)
    storethat::db_snapshot(file = list(...)[["file"]], instrument = "futures", book = "CFTC", name = x)
  ) %>% data.table::rbindlist()


  if (list(...)[["verbose"]]) message("Retrieving futures position data from Bloomberg.\n")

  CFTC <- BBG_futures_CFTC(active_contract_tickers = dplyr::filter(data, is.na(ticker))$ticker,
                           start, as.character(Sys.Date()), verbose = list(...)[["verbose"]])

  if (list(...)[["verbose"]]) message("\nUpdating database with retrieved data.\n")

  storethat::db_store(CFTC, ...)

}


### equity ####
storethat_update_equity <- function(book, names, ...){

  switch(book,

         market = storethat_update_equity_market(names, ...),

         info = storethat_update_info(instrument = "equity", names, ...),

         all = {

           storethat_update_equity_market(names, ...)

           for (x in c("balance sheet", "cash flow statement", "income statement", "key stats", "ratios"))
             storethat_update_equity_book(book = x, names, ...)

           storethat_update_info(instrument = "equity", names, ...)

         },
         storethat_update_equity_book(book, names, ...)
  )
}

#### market ####
storethat_update_equity_market <- function(names, ...){


  data <- lapply(names$ticker, function(x)
    storethat::db_snapshot(file = list(...)[["file"]], instrument = "equity", book = "market", name = x)
  ) %>% data.table::rbindlist()


  storethat_update_market(instrument = "equity", tickers = unique(data$ticker),
                          fields = unique(data$field), start = min(data$end), ...)
}



#### book ####
storethat_update_equity_book <- function(book, names, ...){


  data <- lapply(names$ticker, function(x)
    storethat::db_snapshot(file = list(...)[["file"]], instrument = "equity", book = book, name = x)
  ) %>% data.table::rbindlist()


  storethat_update_book(instrument = "equity", book, tickers = unique(data$ticker),
                          fields = unique(data$field), start = min(data$end), ...)

}




### fund ####
storethat_update_fund <- function(book, names, ...){

  switch(book,

         market = storethat_update_fund_market(names, ...),

         info = storethat_update_info(instrument = "fund", names, ...),

         all = {

           storethat_update_fund_market(names, ...)

           storethat_update_info(instrument = "fund", names, ...)

         }
  )
}

#### market ####
storethat_update_fund_market <- function(names, ...){


  data <- lapply(names$ticker, function(x)
    storethat::db_snapshot(file = list(...)[["file"]], instrument = "fund", book = "market", name = x)
  ) %>% data.table::rbindlist()


  storethat_update_market(instrument = "fund", tickers = unique(data$ticker), start = min(data$end), ...)
}






### global ####

#### market ####
storethat_update_market <- function(instrument, tickers, start, ...){


  if (list(...)[["verbose"]])
    message("Retrieving ", instrument, " market data from Bloomberg.\n")

  market <- do.call(what = paste0("BBG_", instrument, "_market"),
                    args = list(tickers = tickers, start = start, end = as.character(Sys.Date()),
                                verbose = list(...)[["verbose"]]))


  if (list(...)[["verbose"]]) message("\nUpdating database with retrieved data.\n")

  storethat::db_store(file = list(...)[["file"]], object = market, verbose = list(...)[["verbose"]])

}

#### book ####
storethat_update_book <- function(instrument, book, tickers, start, ...){


  if (list(...)[["verbose"]])
    message("Retrieving ", instrument, " book data from Bloomberg.\n")

  book <- do.call(what = paste0("BBG_", instrument, "_book"),
                  args = list(book = book, tickers = tickers, start = start, end = as.character(Sys.Date()),
                                verbose = list(...)[["verbose"]]))


  if (list(...)[["verbose"]]) message("\nUpdating database with retrieved data.\n")

  storethat::db_store(file = list(...)[["file"]], object = book, verbose = list(...)[["verbose"]])

}

#### info ####
storethat_update_info <- function(instrument, names, ...){


  data <- lapply(names$ticker, function(x)
    storethat::db_snapshot(file = list(...)[["file"]], instrument = instrument, book = "info", name = x)
  ) %>% data.table::rbindlist()


  if (list(...)[["verbose"]])
    message("Retrieving ", instrument, " qualitative data from Bloomberg.\n")

  info <- do.call(what = paste0("BBG_", instrument, "_info"), args = list(tickers = unique(data$ticker)))


  if (list(...)[["verbose"]]) message("\nUpdating database with retrieved data.\n")

  storethat::db_store(file = list(...)[["file"]], object = info)

}





