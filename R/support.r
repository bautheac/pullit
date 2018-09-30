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
    db_snapshot(file = list(...)[["file"]], instrument = "futures", book = "market", name = x)
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

      db_store(file = list(...)[["file"]], term_structure, verbose = list(...)[["verbose"]])
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

  db_store(file = list(...)[["file"]], aggregate, verbose = list(...)[["verbose"]])

}


#### CFTC ####
storethat_update_futures_CFTC <- function(names, ...){

  data <- lapply(names$ticker, function(x)
    db_snapshot(file = list(...)[["file"]], instrument = "futures", book = "CFTC", name = x)
  ) %>% data.table::rbindlist()


  if (list(...)[["verbose"]]) message("Retrieving futures position data from Bloomberg.\n")

  CFTC <- BBG_futures_CFTC(active_contract_tickers = dplyr::filter(data, is.na(ticker))$ticker,
                           start, as.character(Sys.Date()), verbose = list(...)[["verbose"]])

  if (list(...)[["verbose"]]) message("\nUpdating database with retrieved data.\n")

  db_store(CFTC, ...)

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
    db_snapshot(file = list(...)[["file"]], instrument = "equity", book = "market", name = x)
  ) %>% data.table::rbindlist()


  storethat_update_market(instrument = "equity", tickers = unique(data$ticker),
                          fields = unique(data$field), start = min(data$end), ...)
}



#### book ####
storethat_update_equity_book <- function(book, names, ...){


  data <- lapply(names$ticker, function(x)
    db_snapshot(file = list(...)[["file"]], instrument = "equity", book = book, name = x)
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
    db_snapshot(file = list(...)[["file"]], instrument = "fund", book = "market", name = x)
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

  db_store(file = list(...)[["file"]], object = market, verbose = list(...)[["verbose"]])

}

#### book ####
storethat_update_book <- function(instrument, book, tickers, start, ...){


  if (list(...)[["verbose"]])
    message("Retrieving ", instrument, " book data from Bloomberg.\n")

  book <- do.call(what = paste0("BBG_", instrument, "_book"),
                  args = list(book = book, tickers = tickers, start = start, end = as.character(Sys.Date()),
                                verbose = list(...)[["verbose"]]))


  if (list(...)[["verbose"]]) message("\nUpdating database with retrieved data.\n")

  db_store(file = list(...)[["file"]], object = book, verbose = list(...)[["verbose"]])

}

#### info ####
storethat_update_info <- function(instrument, names, ...){


  data <- lapply(names$ticker, function(x)
    db_snapshot(file = list(...)[["file"]], instrument = instrument, book = "info", name = x)
  ) %>% data.table::rbindlist()


  if (list(...)[["verbose"]])
    message("Retrieving ", instrument, " qualitative data from Bloomberg.\n")

  info <- do.call(what = paste0("BBG_", instrument, "_info"), args = list(tickers = unique(data$ticker)))


  if (list(...)[["verbose"]]) message("\nUpdating database with retrieved data.\n")

  db_store(file = list(...)[["file"]], object = info)

}








# snapshot ####

## main ####

db_snapshot <- function(file = NULL, instrument, book = "all", name = "all"){

  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file),
              stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite
           database file (ie. ~/storethat.sqlite)")


  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)


  instruments <- "SELECT DISTINCT instrument FROM support_fields;"
  instruments <- RSQLite::dbGetQuery(con = con, instruments)
  if (! instrument %in% instruments$instrument)
    stop("Parameter 'instrument' must be supplied as a scalar character vector; one of '",
         paste(instruments$instrument, collapse = "', '"), "'")


  books <- switch(instrument, all = "SELECT DISTINCT book FROM support_fields;",
                  paste0("SELECT DISTINCT book FROM support_fields WHERE instrument = '",
                         instrument, "';")
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
                  RSQLite::dbGetQuery(con = con, paste0("SELECT * FROM tickers_", instrument, ";"))%>%
                    dplyr::mutate(instrument = !! instrument) %>%
                    dplyr::select(instrument, dplyr::everything())
  )
  if (! name %in% c("all", names$ticker))
    stop("Parameter 'name' must be supplied as a scalar character vector; one of '",
         paste(c("all", names$ticker), collapse = "', '"), "'")
  if (name != "all") names %<>% dplyr::filter(ticker == !! name)


  dates <- "SELECT * FROM support_dates;"; dates <- RSQLite::dbGetQuery(con = con, dates)


  data <- switch(instrument,

                 fund = db_snapshot_fund(book, dplyr::filter(names, instrument == !! instrument) %>%
                                           dplyr::select(-instrument), dates, con) %>%
                   dplyr::left_join(dplyr::select(names, ticker_id = id, ticker),
                                    by = "ticker_id") %>%
                   dplyr::select(ticker, field, start, end),

                 futures = db_snapshot_futures(book, dplyr::filter(names, instrument == !! instrument) %>%
                                                 dplyr::select(-instrument), dates, con) %>%
                   dplyr::left_join(dplyr::select(names, active_contract_ticker_id = id,
                                                  `active contract ticker` = ticker),
                                    by = "active_contract_ticker_id") %>%
                   dplyr::select(`active contract ticker`, ticker, field, start, end),

                 equity = db_snapshot_equity(book, dplyr::filter(names, instrument == !! instrument) %>%
                                               dplyr::select(-instrument), dates, con) %>%
                   dplyr::left_join(dplyr::select(names, ticker_id = id, ticker),
                                    by = "ticker_id") %>%
                   dplyr::select(ticker, field, start, end)

  )


  RSQLite::dbDisconnect(con); data
}




## equity ####

db_snapshot_equity <- function(book, names, dates, con){

  switch(book,

         market = db_snapshot_market(instrument = "equity", names, dates, con),

         info = db_snapshot_info(instrument = "equity", names, dates, con),

         all = {

           books <- "SELECT DISTINCT book FROM support_fields WHERE instrument = 'equity'
           AND book NOT IN ('market', 'info');"
           books <- RSQLite::dbGetQuery(con = con, books)$book

           lapply(books, function(x) db_snapshot_book(instrument = "equity", book = x,
                                                      names, dates, con)) %>%
             data.table::rbindlist()

         },

         db_snapshot_equity_book(book, names, dates, con)
  )

}


### book ####
db_snapshot_equity_book <- function(book, names, dates, con){

  fields <- paste0("SELECT * FROM support_fields WHERE instrument = 'equity'
                   AND book = '", book, "';")
  fields <- RSQLite::dbGetQuery(con = con, fields)

  # data <- db_snapshot_historical("data_equity_book", names, fields, dates, con)
  data <- db_snapshot_book(instrument = "equity", book, names, dates, con)

  purrr::map2(list(c("start"), c("end")),
              dplyr::lst(min = min, max = max),
              ~ dplyr::group_by(data, ticker_id, field) %>%
                dplyr::summarise_at(.x, .y)) %>%
    purrr::reduce(dplyr::inner_join, by = c("ticker_id", "field")) %>%
    dplyr::ungroup() %>% dplyr::select(ticker_id, field, start, end)

}






## futures ####

db_snapshot_futures <- function(book, names, dates, con){

  switch(book,

         market = {

           tickers <- paste0("SELECT id AS ticker_id, ticker FROM
                             tickers_support_futures_ts WHERE active_contract_ticker_id
                             IN (", paste(names$id, collapse = ", "), ");")
           tickers <- RSQLite::dbGetQuery(con = con, tickers)

           db_snapshot_market(instrument = "futures", names, dates, con) %>%
             dplyr::left_join(tickers, by = "ticker_id") %>%
             dplyr::select(active_contract_ticker_id, ticker, field, start, end)

         },

         CFTC = {

           tickers <- paste0("SELECT id AS ticker_id, ticker FROM
                             tickers_support_futures_cftc WHERE active_contract_ticker_id
                             IN (", paste(names$id, collapse = ", "), ");")
           tickers <- RSQLite::dbGetQuery(con = con, tickers)

           db_snapshot_futures_cftc(names, dates, con) %>%
             dplyr::left_join(tickers, by = "ticker_id") %>%
             dplyr::select(active_contract_ticker_id, ticker, field, start, end)

         },

         info = db_snapshot_info(instrument = "futures", names, dates, con) %>%
           dplyr::select(active_contract_ticker_id = ticker_id, field, start, end) %>%
           dplyr::mutate(ticker = NA),

         all = {

           tickers <- paste0("SELECT id AS ticker_id, ticker FROM
                             tickers_support_futures_ts WHERE active_contract_ticker_id
                             IN (", paste(names$id, collapse = ", "), ");")
           tickers <- RSQLite::dbGetQuery(con = con, tickers)

           market <- db_snapshot_market(instrument = "futures", names, dates, con) %>%
             dplyr::left_join(tickers, by = "ticker_id") %>%
             dplyr::select(active_contract_ticker_id, ticker, field, start, end)


           tickers <- paste0("SELECT id AS ticker_id, ticker FROM
                             tickers_support_futures_cftc WHERE active_contract_ticker_id
                             IN (", paste(names$id, collapse = ", "), ");")
           tickers <- RSQLite::dbGetQuery(con = con, tickers)

           CFTC <- db_snapshot_futures_cftc(names, dates, con) %>%
             dplyr::left_join(tickers, by = "ticker_id") %>%
             dplyr::select(active_contract_ticker_id, ticker, field, start, end)


           info <- db_snapshot_info(instrument = "futures", names, dates, con) %>%
             dplyr::select(active_contract_ticker_id = ticker_id, field, start, end) %>%
             dplyr::mutate(ticker = NA)


           plyr::rbind.fill(market, CFTC, info)

         }
           )
}


### market ####

#### term structure ####
db_snapshot_futures_ts <- function(names, dates, con){

  tickers <- paste0("SELECT * FROM tickers_support_futures_ts WHERE active_contract_ticker_id
                    IN (",
                    paste(unique(names$id), collapse = ", "), ");")
  tickers <- RSQLite::dbGetQuery(con = con, tickers)

  fields <- "SELECT * FROM support_fields WHERE instrument = 'futures' AND book = 'market' AND
  type = 'term structure';"
  fields <- RSQLite::dbGetQuery(con = con, fields)

  data <- db_snapshot_historical("data_futures_ts", tickers, fields, dates, con)

  purrr::map2(list(c("start"), c("end")),
              dplyr::lst(min = min, max = max),
              ~ dplyr::group_by(data, ticker_id, field_id) %>%
                dplyr::summarise_at(.x, .y)) %>%
    purrr::reduce(dplyr::inner_join, by = c("ticker_id", "field_id")) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(dplyr::select(tickers, id, active_contract_ticker_id),
                     by = c("ticker_id" = "id")) %>%
    dplyr::left_join(dplyr::select(fields, field_id = id, symbol), by = "field_id") %>%
    dplyr::select(active_contract_ticker_id, ticker_id, field = symbol, start, end) %>%
    dplyr::left_join(dplyr::select(dates, date_id = id, date), by = c("start" = "date_id")) %>%
    dplyr::select(active_contract_ticker_id, ticker_id, field, start = date, end) %>%
    dplyr::left_join(dplyr::select(dates, date_id = id, date), by = c("end" = "date_id")) %>%
    dplyr::select(active_contract_ticker_id, ticker_id, field, start, end = date)

}

#### aggregate ####
db_snapshot_futures_aggregate <- function(names, dates, con){

  fields <- "SELECT * FROM support_fields WHERE instrument = 'futures' AND book = 'market' AND
  type = 'aggregate';"
  fields <- RSQLite::dbGetQuery(con = con, fields)

  data <- db_snapshot_historical("data_futures_aggregate", names, fields, dates, con)

  purrr::map2(list(c("start"), c("end")),
              dplyr::lst(min = min, max = max),
              ~ dplyr::group_by(data, ticker_id, field_id) %>%
                dplyr::summarise_at(.x, .y)) %>%
    purrr::reduce(dplyr::inner_join, by = c("ticker_id", "field_id")) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(dplyr::select(fields, field_id = id, symbol), by = "field_id") %>%
    dplyr::select(active_contract_ticker_id = ticker_id, field = symbol, start, end) %>%
    dplyr::left_join(dplyr::select(dates, date_id = id, date), by = c("start" = "date_id")) %>%
    dplyr::select(active_contract_ticker_id, field, start = date, end) %>%
    dplyr::left_join(dplyr::select(dates, date_id = id, date), by = c("end" = "date_id")) %>%
    dplyr::select(active_contract_ticker_id, field, start, end = date)

}


### CFTC ####
db_snapshot_futures_cftc <- function(names, dates, con){


  tickers <- paste0("SELECT * FROM tickers_support_futures_cftc WHERE active_contract_ticker_id
                    IN (", paste(unique(names$id), collapse = ", "), ");")
  tickers <- RSQLite::dbGetQuery(con = con, tickers)


  fields <- "SELECT * FROM support_fields WHERE instrument = 'futures' AND book = 'CFTC';"
  fields <- RSQLite::dbGetQuery(con = con, fields)


  data <-   lapply(unique(dates$period), function(i){
    query <- paste0("SELECT ticker_id, MIN(date_id) AS start, MAX(date_id) AS end FROM
                    data_futures_cftc_", i, " WHERE ticker_id IN (", paste(tickers$id, collapse = ", ")
                    , ") GROUP BY ticker_id;")
    RSQLite::dbGetQuery(con = con, query)
  }) %>% data.table::rbindlist() %>%
    dplyr::mutate(field_id = fields$id)


  purrr::map2(list(c("start"), c("end")),
              dplyr::lst(min = min, max = max),
              ~ dplyr::group_by(data, ticker_id, field_id) %>%
                dplyr::summarise_at(.x, .y)) %>%
    purrr::reduce(dplyr::inner_join, by = c("ticker_id", "field_id")) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(dplyr::select(tickers, id, active_contract_ticker_id),
                     by = c("ticker_id" = "id")) %>%
    dplyr::left_join(dplyr::select(fields, field_id = id, symbol), by = "field_id") %>%
    dplyr::select(active_contract_ticker_id, ticker_id, field = symbol, start, end) %>%
    dplyr::left_join(dplyr::select(dates, date_id = id, date), by = c("start" = "date_id")) %>%
    dplyr::select(active_contract_ticker_id, ticker_id, field, start = date, end) %>%
    dplyr::left_join(dplyr::select(dates, date_id = id, date), by = c("end" = "date_id")) %>%
    dplyr::select(active_contract_ticker_id, ticker_id, field, start, end = date)

}








## fund ####

db_snapshot_fund <- function(book, names, dates, con){

  switch(book,

         market = db_snapshot_market(instrument = "fund", names, dates, con),

         info = db_snapshot_info(instrument = "fund", names, dates, con),

         all = rbind(db_snapshot_market(instrument = "fund", names, dates, con),
                     db_snapshot_info(instrument = "fund", names, dates, con))

  )

}



## global ####

### historical ####
db_snapshot_historical <- function(table, names, fields, dates, con){

  lapply(unique(dates$period), function(i){
    query <- paste0("SELECT ticker_id, field_id, MIN(date_id) AS start, MAX(date_id) AS end FROM
                    ", table, "_", i, " WHERE ticker_id IN (", paste(names$id, collapse = ", ")
                    , ") AND field_id IN (", paste(fields$id, collapse = ", ")
                    , ") GROUP BY ticker_id, field_id;")
    RSQLite::dbGetQuery(con = con, query)
  }) %>% data.table::rbindlist()

}


#### market ####
db_snapshot_market <- function(instrument, names, dates, con){


  switch(instrument,

         futures = {

           term_structure <- db_snapshot_futures_ts(names, dates, con)
           aggregate <- db_snapshot_futures_aggregate(names, dates, con)
           plyr::rbind.fill(term_structure, aggregate)

         },

         {

           fields <- paste0("SELECT * FROM support_fields WHERE instrument = '",
                            instrument, "' AND book = 'market';")
           fields <- RSQLite::dbGetQuery(con = con, fields)

           data <- db_snapshot_historical(paste0("data_", instrument, "_market"),
                                          names, fields, dates, con)

           purrr::map2(list(c("start"), c("end")),
                       dplyr::lst(min = min, max = max),
                       ~ dplyr::group_by(data, ticker_id, field_id) %>%
                         dplyr::summarise_at(.x, .y)) %>%
             purrr::reduce(dplyr::inner_join, by = c("ticker_id", "field_id")) %>%
             dplyr::ungroup() %>% dplyr::select(ticker_id, field_id, start, end) %>%
             dplyr::left_join(dplyr::select(fields, field_id = id, symbol), by = "field_id") %>%
             dplyr::select(ticker_id, field = symbol, start, end) %>%
             dplyr::left_join(dplyr::select(dates, date_id = id, date), by = c("start" = "date_id")) %>%
             dplyr::select(ticker_id, field, start = date, end) %>%
             dplyr::left_join(dplyr::select(dates, date_id = id, date), by = c("end" = "date_id")) %>%
             dplyr::select(ticker_id, field, start, end = date)

         }

  )

}



#### book ####
db_snapshot_book <- function(instrument, book, names, dates, con){

  fields <- paste0("SELECT * FROM support_fields WHERE instrument = '",
                   instrument, "' AND book = '", book, "';")
  fields <- RSQLite::dbGetQuery(con = con, fields)

  data <- db_snapshot_historical(paste0("data_", instrument, "_book"),
                                 names, fields, dates, con)

  purrr::map2(list(c("start"), c("end")),
              dplyr::lst(min = min, max = max),
              ~ dplyr::group_by(data, ticker_id, field_id) %>%
                dplyr::summarise_at(.x, .y)) %>%
    purrr::reduce(dplyr::inner_join, by = c("ticker_id", "field_id")) %>%
    dplyr::ungroup() %>% dplyr::select(ticker_id, field_id, start, end) %>%
    dplyr::left_join(dplyr::select(fields, field_id = id, symbol), by = "field_id") %>%
    dplyr::select(ticker_id, field = symbol, start, end) %>%
    dplyr::left_join(dplyr::select(dates, date_id = id, date), by = c("start" = "date_id")) %>%
    dplyr::select(ticker_id, field, start = date, end) %>%
    dplyr::left_join(dplyr::select(dates, date_id = id, date), by = c("end" = "date_id")) %>%
    dplyr::select(ticker_id, field, start, end = date)

}



### info ####

db_snapshot_info <- function(instrument, names, dates, con){

  fields <- paste0("SELECT * FROM support_fields WHERE instrument = '",
                   instrument, "' AND book = 'info';")
  fields <- RSQLite::dbGetQuery(con = con, fields)


  query <- paste0("SELECT ticker_id, field_id, MIN(date_id) AS start, MAX(date_id) AS end FROM
                  data_", instrument, "_info WHERE ticker_id IN (", paste(names$id, collapse = ", ")
                  , ") GROUP BY ticker_id, field_id;")
  RSQLite::dbGetQuery(con = con, query) %>%
    dplyr::left_join(dplyr::select(fields, field_id = id, symbol), by = "field_id") %>%
    dplyr::select(ticker_id, field = symbol, start, end) %>%
    dplyr::left_join(dplyr::select(dates, date_id = id, date), by = c("start" = "date_id")) %>%
    dplyr::select(ticker_id, field, start = date, end) %>%
    dplyr::left_join(dplyr::select(dates, date_id = id, date), by = c("end" = "date_id")) %>%
    dplyr::select(ticker_id, field, start, end = date)

}



# store ####

setGeneric("db_store", function(object, file = NULL, verbose = TRUE) standardGeneric("db_store"))

## futures ####

### term structure ####
setMethod("db_store", signature = c(object = "FuturesTS"), function(object, file, verbose){

  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file), stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database file (ie. ~/storethat.sqlite)")

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)

  if (! all_fields_exist(fields = object@fields, con = con)) update_fields(fields = object@fields, con = con)
  fields <- "SELECT id, instrument, book, type, symbol FROM support_fields WHERE instrument = 'futures' AND book = 'market' AND type = 'term structure';"
  fields <- RSQLite::dbGetQuery(con = con, fields)

  if (! all_tickers_exist(tickers = unique(object@active_contract_tickers$ticker),
                          table_tickers = "tickers_futures", con))
    update_tickers(tickers = unique(object@active_contract_tickers$ticker), table_tickers = "tickers_futures", con)
  active_contract_tickers <- RSQLite::dbReadTable(con, "tickers_futures")

  if (! all_tickers_exist(tickers = unique(object@term_structure_tickers$ticker),
                          table_tickers = "tickers_support_futures_ts", con))
    update_term_structure_tickers(tickers = unique(object@term_structure_tickers), con)
  term_structure_tickers <- RSQLite::dbReadTable(con, "tickers_support_futures_ts")

  dates <- paste0("SELECT * FROM support_dates WHERE date >= '", min(object@data$date), "' AND date <= '",  max(object@data$date), "';")
  dates <- dplyr::semi_join(RSQLite::dbGetQuery(con, dates) %>% dplyr::mutate(date = as.Date(date)),
                            dplyr::distinct(object@data, date), by = "date") %>%
    dplyr::mutate(date = as.Date(date))

  for (i in unique(dates$period)){
    update_data(data = dplyr::semi_join(object@data, dplyr::filter(dates, period == i), by = "date"),
                table_data = paste0("data_futures_ts_", i), tickers = term_structure_tickers, fields = fields,
                dates = dplyr::filter(dates, period == i), con = con)
    if (verbose) done(paste0("Period ", data.table::first(dplyr::filter(dates, period == i)$date), "/",
                             data.table::last(dplyr::filter(dates, period == i)$date), " done."))
  }

  RSQLite::dbDisconnect(con)
})


### aggregate ####
setMethod("db_store", signature = c(object = "FuturesAggregate"), function(object, file, verbose){

  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file), stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database file (ie. ~/storethat.sqlite)")

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)

  if (! all_fields_exist(fields = object@fields, con = con)) update_fields(fields = object@fields, con = con)
  fields <- "SELECT * FROM support_fields WHERE instrument = 'futures' AND book = 'market' AND type = 'aggregate';"
  fields <- RSQLite::dbGetQuery(con = con, fields)

  if (!all_tickers_exist(tickers = unique(object@active_contract_tickers$ticker),
                         table_tickers = "tickers_futures", con))
    update_tickers(tickers = unique(object@active_contract_tickers$ticker), table_tickers = "tickers_futures", con)
  active_contract_tickers <- RSQLite::dbReadTable(con, "tickers_futures")

  dates <- paste0("SELECT * FROM support_dates WHERE date >= '", min(object@data$date), "' AND date <= '",  max(object@data$date), "';")
  dates <- dplyr::semi_join(RSQLite::dbGetQuery(con, dates) %>% dplyr::mutate(date = as.Date(date)),
                            dplyr::distinct(object@data, date), by = "date") %>%
    dplyr::mutate(date = as.Date(date))

  for (i in unique(dates$period)){
    update_data(data = dplyr::semi_join(object@data, dplyr::filter(dates, period == i), by = "date"),
                table_data = paste0("data_futures_aggregate_", i), tickers = active_contract_tickers, fields = fields,
                dates = dplyr::filter(dates, period == i), con = con)
    if (verbose) done(paste0("Period ", data.table::first(dplyr::filter(dates, period == i)$date), "/",
                             data.table::last(dplyr::filter(dates, period == i)$date), " done."))

  }

  RSQLite::dbDisconnect(con)
})



### CFTC ####
setMethod("db_store", signature = c(object = "FuturesCFTC"), function(object, file, verbose){

  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file), stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database file (ie. ~/storethat.sqlite)")

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)

  fields <- "SELECT * FROM support_fields WHERE instrument = 'futures' AND book = 'CFTC';"
  fields <- RSQLite::dbGetQuery(con = con, fields)

  if (!all_tickers_exist(tickers = unique(object@active_contract_tickers$ticker),
                         table_tickers = "tickers_futures", con))
    update_tickers(tickers = unique(object@active_contract_tickers$ticker), table_tickers = "tickers_futures", con)
  active_contract_tickers <- RSQLite::dbReadTable(con, "tickers_futures")

  if (!all_tickers_exist(tickers = unique(object@cftc_tickers$ticker),
                         table_tickers = "tickers_support_futures_cftc", con))
    update_cftc_tickers(tickers = object@cftc_tickers, con)
  cftc_tickers <- RSQLite::dbReadTable(con, "tickers_support_futures_cftc")

  dates <- paste0("SELECT * FROM support_dates WHERE date >= '", min(object@data$date), "' AND date <= '",  max(object@data$date), "';")
  dates <- dplyr::semi_join(RSQLite::dbGetQuery(con, dates) %>% dplyr::mutate(date = as.Date(date)),
                            dplyr::distinct(object@data, date), by = "date") %>%
    dplyr::mutate(date = as.Date(date))

  for (i in unique(dates$period)){
    update_data_cftc(data = dplyr::semi_join(object@data, dplyr::filter(dates, period == i), by = "date"),
                     table_data = paste0("data_futures_cftc_", i), tickers = cftc_tickers,
                     dates = dplyr::filter(dates, period == i), con = con)
    if (verbose) done(paste0("Period ", data.table::first(dplyr::filter(dates, period == i)$date), "/",
                             data.table::last(dplyr::filter(dates, period == i)$date), " done."))
  }

  RSQLite::dbDisconnect(con)
})



### info ####
setMethod("db_store", signature = c(object = "FuturesInfo"), function(object, file){

  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file), stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database file (ie. ~/storethat.sqlite)")

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)

  if (! all_fields_exist(fields = object@fields, con = con)) update_fields(fields = object@fields, con = con)
  fields <- "SELECT * FROM support_fields WHERE instrument = 'futures' AND book = 'info';"
  fields <- RSQLite::dbGetQuery(con = con, fields)

  if (!all_tickers_exist(tickers = unique(object@info$ticker),
                         table_tickers = "tickers_futures", con))
    update_tickers(tickers = unique(object@info$ticker), table_tickers = "tickers_futures", con)
  tickers <- RSQLite::dbReadTable(con, "tickers_futures")

  query <- paste0("DELETE FROM data_futures_info WHERE ticker_id IN (",
                  paste(dplyr::filter(tickers, ticker %in% unique(object@info$ticker))$id, collapse = ", "),
                  ");")
  RSQLite::dbExecute(con = con, query)

  date_id <- paste0("SELECT id from support_dates WHERE date = '", as.character(Sys.Date()), "';")
  date_id <- RSQLite::dbGetQuery(con, date_id) %>% purrr::flatten_chr()

  query <- dplyr::left_join(object@info, tickers, by = "ticker") %>% dplyr::select(ticker_id = id, field, value) %>%
    dplyr::mutate(field = as.character(field)) %>%
    dplyr::left_join(fields, by = c("field" = "symbol")) %>% dplyr::select(ticker_id, field_id = id, value) %>%
    dplyr::mutate(date_id = !! date_id) %>% dplyr::select(ticker_id, field_id, date_id, value)

  RSQLite::dbWriteTable(con = con, "data_futures_info", query, row.names = FALSE, overwrite = FALSE, append = TRUE)
  RSQLite::dbDisconnect(con)
})




## equity ####

### market ####
setMethod("db_store", signature = c(object = "EquityMarket"), function(object, file, verbose){

  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file), stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database file (ie. ~/storethat.sqlite)")

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)

  if (! all_fields_exist(fields = object@fields, con = con)) update_fields(fields = object@fields, con = con)
  fields <- "SELECT * FROM support_fields WHERE instrument = 'equity' AND book = 'market';"
  fields <- RSQLite::dbGetQuery(con = con, fields)

  if (!all_tickers_exist(tickers = unique(object@tickers$ticker),
                         table_tickers = "tickers_equity", con = con))
    update_tickers(tickers = unique(object@tickers$ticker), table_tickers = "tickers_equity", con = con)
  tickers <- RSQLite::dbReadTable(con, "tickers_equity")


  dates <- paste0("SELECT * FROM support_dates WHERE date >= '", min(object@data$date), "' AND date <= '",  max(object@data$date), "';")
  dates <- dplyr::semi_join(RSQLite::dbGetQuery(con, dates) %>% dplyr::mutate(date = as.Date(date)),
                            dplyr::distinct(object@data, date), by = "date") %>%
    dplyr::mutate(date = as.Date(date))

  for (i in unique(dates$period)){
    update_data(data = dplyr::semi_join(object@data, dplyr::filter(dates, period == i), by = "date"),
                table_data = paste0("data_equity_market_", i), tickers = tickers, fields = fields,
                dates = dplyr::filter(dates, period == i), con = con)
    if (verbose) done(paste0("Period ", data.table::first(dplyr::filter(dates, period == i)$date), "/",
                             data.table::last(dplyr::filter(dates, period == i)$date), " done."))
  }

  RSQLite::dbDisconnect(con)
})


### book ####
setMethod("db_store", signature = c(object = "EquityBook"), function(object, file, verbose){

  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file), stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database file (ie. ~/storethat.sqlite)")

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)

  book <- dplyr::case_when(class(object) == "EquityKS" ~ "key stats", class(object) == "EquityBS" ~ "balance sheet",
                           class(object) == "EquityCF" ~ "cash flow statement", class(object) == "EquityIS" ~ "income statement",
                           class(object) == "EquityRatios" ~ "ratios")

  if (! all_fields_exist(fields = object@fields, con = con)) update_fields(fields = object@fields, con = con)
  fields <- paste0("SELECT * FROM support_fields WHERE instrument = 'equity' AND book = '", book, "';")
  fields <- RSQLite::dbGetQuery(con = con, fields)

  if (!all_tickers_exist(tickers = unique(object@tickers$ticker), table_tickers = "tickers_equity", con = con))
    update_tickers(tickers = unique(object@tickers$ticker), table_tickers = "tickers_equity", con = con)
  tickers <- RSQLite::dbReadTable(con, "tickers_equity")

  dates <- paste0("SELECT * FROM support_dates WHERE date >= '", min(object@data$date), "' AND date <= '",  max(object@data$date), "';")
  dates <- dplyr::semi_join(RSQLite::dbGetQuery(con, dates) %>% dplyr::mutate(date = as.Date(date)),
                            dplyr::distinct(object@data, date), by = "date") %>%
    dplyr::mutate(date = as.Date(date))

  for (i in unique(dates$period)){
    update_data(data = dplyr::semi_join(object@data, dplyr::filter(dates, period == i), by = "date"),
                table_data = paste0("data_equity_book_", i), tickers = tickers, fields = fields,
                dates = dplyr::filter(dates, period == i), con = con)
    if (verbose) done(paste0("Period ", data.table::first(dplyr::filter(dates, period == i)$date), "/",
                             data.table::last(dplyr::filter(dates, period == i)$date), " done."))
  }

  RSQLite::dbDisconnect(con)
})


### info ####
setMethod("db_store", signature = c(object = "EquityInfo"), function(object, file){

  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file), stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database file (ie. ~/storethat.sqlite)")

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)

  if (! all_fields_exist(fields = object@fields, con = con)) update_fields(fields = object@fields, con = con)
  fields <- "SELECT * FROM support_fields WHERE instrument = 'equity' AND book = 'info';"
  fields <- RSQLite::dbGetQuery(con = con, fields)

  if (!all_tickers_exist(tickers = unique(object@info$ticker), table_tickers = "tickers_equity", con))
    update_tickers(tickers = unique(object@info$ticker), table_tickers = "tickers_equity", con)
  tickers <- RSQLite::dbReadTable(con, "tickers_equity")

  date_id <- paste0("SELECT id from support_dates WHERE date = '", as.character(Sys.Date()), "';")
  date_id <- RSQLite::dbGetQuery(con, date_id) %>% purrr::flatten_chr()

  query <- paste0("DELETE FROM data_equity_info WHERE ticker_id IN (",
                  paste(dplyr::filter(tickers, ticker %in% unique(object@info$ticker))$id, collapse = ", "),
                  ");")
  RSQLite::dbExecute(con = con, query)

  query <- dplyr::left_join(object@info, tickers, by = "ticker") %>% dplyr::select(ticker_id = id, field, value) %>%
    dplyr::mutate(field = as.character(field)) %>%
    dplyr::left_join(fields, by = c("field" = "symbol")) %>% dplyr::select(ticker_id, field_id = id, value) %>%
    dplyr::mutate(date_id = !! date_id) %>% dplyr::select(ticker_id, field_id, date_id, value)

  RSQLite::dbWriteTable(con = con, "data_equity_info", query, row.names = FALSE, overwrite = FALSE, append = TRUE)
  RSQLite::dbDisconnect(con)
})



## fund ####

### market ####
setMethod("db_store", signature = c(object = "FundMarket"), function(object, file, verbose){

  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file), stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database file (ie. ~/storethat.sqlite)")

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)

  if (! all_fields_exist(fields = object@fields, con = con)) update_fields(fields = object@fields, con = con)
  fields <- "SELECT * FROM support_fields WHERE instrument = 'fund' AND book = 'market';"
  fields <- RSQLite::dbGetQuery(con = con, fields)


  if (!all_tickers_exist(tickers = unique(object@tickers$ticker), table_tickers = "tickers_fund", con = con))
    update_tickers(tickers = unique(object@tickers$ticker), table_tickers = "tickers_fund", con = con)
  tickers <- RSQLite::dbReadTable(con, "tickers_fund")


  dates <- paste0("SELECT * FROM support_dates WHERE date >= '", min(object@data$date), "' AND date <= '",  max(object@data$date), "';")
  dates <- dplyr::semi_join(RSQLite::dbGetQuery(con, dates) %>% dplyr::mutate(date = as.Date(date)),
                            dplyr::distinct(object@data, date), by = "date") %>%
    dplyr::mutate(date = as.Date(date))

  for (i in unique(dates$period)){
    update_data(data = dplyr::semi_join(object@data, dplyr::filter(dates, period == i), by = "date"),
                table_data = paste0("data_fund_market_", i), tickers = tickers, fields = fields,
                dates = dplyr::filter(dates, period == i), con = con)
    if (verbose) done(paste0("Period ", data.table::first(dplyr::filter(dates, period == i)$date), "/",
                             data.table::last(dplyr::filter(dates, period == i)$date), " done."))
  }

  RSQLite::dbDisconnect(con)
})

### info ####
setMethod("db_store", signature = c(object = "FundInfo"), function(object, file){

  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file), stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database file (ie. ~/storethat.sqlite)")

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)

  if (! all_fields_exist(fields = object@fields, con = con)) update_fields(fields = object@fields, con = con)
  fields <- "SELECT * FROM support_fields WHERE instrument = 'fund' AND book = 'info';"
  fields <- RSQLite::dbGetQuery(con = con, fields)


  if (!all_tickers_exist(tickers = unique(object@info$ticker), table_tickers = "tickers_fund", con))
    update_tickers(tickers = unique(object@info$ticker), table_tickers = "tickers_fund", con)
  tickers <- RSQLite::dbReadTable(con, "tickers_fund")


  date_id <- paste0("SELECT id from support_dates WHERE date = '", as.character(Sys.Date()), "';")
  date_id <- RSQLite::dbGetQuery(con, date_id) %>% purrr::flatten_chr()

  query <- paste0("DELETE FROM data_fund_info WHERE ticker_id IN (",
                  paste(dplyr::filter(tickers, ticker %in% unique(object@info$ticker))$id, collapse = ", "),
                  ");")
  RSQLite::dbExecute(con = con, query)

  query <- dplyr::left_join(object@info, tickers, by = "ticker") %>% dplyr::select(ticker_id = id, field, value) %>%
    dplyr::mutate(field = as.character(field)) %>%
    dplyr::left_join(fields, by = c("field" = "symbol")) %>% dplyr::select(ticker_id, field_id = id, value) %>%
    dplyr::mutate(date_id = !! date_id) %>% dplyr::select(ticker_id, field_id, date_id, value)

  RSQLite::dbWriteTable(con = con, "data_fund_info", query, row.names = FALSE, overwrite = FALSE, append = TRUE)
  RSQLite::dbDisconnect(con)
})














