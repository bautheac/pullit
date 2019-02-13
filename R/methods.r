# show methods ####

## DataInfo ####
#' Show method for pullit S4 objects.
#'
#'
#' @param object an S4 object from \href{https://bautheac.github.io/pullit/}{\pkg{pullit}}
#'   suite.
#'
#'
#' @rdname show-methods
#'
#' @aliases show,DataInfo
#'
#'
#' @importFrom methods show
#'
#'
#' @export
setMethod("show", "DataInfo", function(object) {
  cat("S4 object of class",
      methods::is(object)[[1L]],
      "\n\nSlots inlude\n",
      "  info: access with get_info()\n",
      "  fields: access with get_fields()\n",
      "  call: access with get_call()")
})


## FuturesHistorical ####
#' @rdname show-methods
#'
#' @aliases show,FuturesHistorical
#'
#'
#' @importFrom methods show
#'
#'
#' @export
setMethod("show", "FuturesHistorical", function(object) {
  cat("S4 object of class",
      methods::is(object)[[1L]],
      "\n\nSlots inlude\n",
      "  active_contract_tickers: access with get_active_contract_tickers()\n",
      "  fields: access with get_fields()\n",
      "  data: access with get_data()\n",
      "  call: access with get_call()\n",
      "\nSee also: get_periods()")
})

## EquityHistorical ####
#' @rdname show-methods
#'
#' @aliases show,EquityHistorical
#'
#'
#' @importFrom methods show
#'
#'
#' @export
setMethod("show", signature = "EquityHistorical", function(object) {
  cat("S4 object of class",
      methods::is(object)[[1L]],
      "\n\nSlots inlude\n",
      "  tickers: access with get_tickers()\n",
      "  fields: access with get_fields()\n",
      "  data: access with get_data()\n",
      "  call: access with get_call()\n",
      "\nSee also: get_periods()")
})

## FundHistorical ####
#' @rdname show-methods
#'
#' @aliases show,FundHistorical
#'
#'
#' @importFrom methods show
#'
#'
#' @export
setMethod("show", signature = "FundHistorical", function(object) {
  cat("S4 object of class",
      methods::is(object)[[1L]],
      "\n\nSlots inlude\n",
      "  tickers: access with get_tickers()\n",
      "  fields: access with get_fields()\n",
      "  data: access with get_data()\n",
      "  call: access with get_call()\n",
      "\nSee also: get_periods()")
})

## FuturesTS ####
#' @rdname show-methods
#'
#' @aliases show,FuturesTS
#'
#'
#' @importFrom methods show
#'
#'
#' @export
setMethod("show", "FuturesTS", function(object) {
  cat("S4 object of class",
      methods::is(object)[[1L]],
      "\n\nSlots inlude\n",
      "  active_contract_tickers: access with get_active_contract_tickers()\n",
      "  term_structure_tickers: access with get_term_structure_tickers()\n",
      "  fields: access with get_fields()\n",
      "  data: access with get_data()\n",
      "  call: access with get_call()\n",
      "\nSee also: get_periods()")
})

## FuturesCFTC ####
#' @rdname show-methods
#'
#' @aliases show,FuturesCFTC
#'
#'
#' @importFrom methods show
#'
#'
#' @export
setMethod("show", "FuturesCFTC", function(object) {
  cat("S4 object of class",
      methods::is(object)[[1L]],
      "\n\nSlots inlude\n",
      "  active_contract_tickers: access with get_active_contract_tickers()\n",
      "  cftc_tickers: access with get_CFTC_tickers()\n",
      "  data: access with get_data()\n",
      "  call: access with get_call()\n",
      "\nSee also: get_periods()")
})

## IndexHistorical ####
#' @rdname show-methods
#'
#' @aliases show,IndexHistorical
#'
#'
#' @importFrom methods show
#'
#'
#' @export
setMethod("show", signature = "IndexHistorical", function(object) {
  cat("S4 object of class",
      methods::is(object)[[1L]],
      "\n\nSlots inlude\n",
      "  tickers: access with get_tickers()\n",
      "  fields: access with get_fields()\n",
      "  data: access with get_data()\n",
      "  call: access with get_call()\n",
      "\nSee also: get_periods()")
})




# accessors ####

## info ####
#' @rdname get_info-methods
#'
#' @aliases get_info,DataInfo
#'
#'
#' @importFrom tibble tibble
#'
#'
#' @export
setMethod("get_info", signature = c("DataInfo"), function(object) object@info)



## tickers ####

### DataHistorical ####
#' @rdname get_tickers-methods
#'
#' @aliases get_tickers,DataHistorical
#'
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_tickers", signature = c("DataHistorical"), function(object) object@tickers)

### FuturesHistorical ####
#' @rdname get_active_contract_tickers-methods
#'
#' @aliases get_active_contract_tickers,FuturesHistorical
#'
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_active_contract_tickers", signature = c("FuturesHistorical"), function(object) object@active_contract_tickers)

### FuturesTS ####
#' @rdname get_term_structure_tickers-methods
#'
#' @aliases get_term_structure_tickers,FuturesTS
#'
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_term_structure_tickers", signature = c("FuturesTS"), function(object) object@term_structure_tickers)

### FuturesCFTC ####
#' @rdname get_CFTC_tickers-methods
#'
#' @aliases get_CFTC_tickers,FuturesCFTC
#'
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_CFTC_tickers", signature = c("FuturesCFTC"), function(object) object@cftc_tickers)

### EquityHistorical ####
#' @rdname get_tickers-methods
#'
#' @aliases get_tickers,EquityHistorical
#'
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_tickers", signature = "EquityHistorical", function(object) object@tickers)

### FundHistorical ####
#' @rdname get_tickers-methods
#'
#' @aliases get_tickers,FundHistorical
#'
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_tickers", signature = "FundHistorical", function(object) object@tickers)


## fields ####

### DataInfo ####
#' @rdname get_fields-methods
#'
#' @aliases get_fields,DataInfo
#'
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_fields", "DataInfo", function(object) object@fields)

### DataHistorical ####
#' @rdname get_fields-methods
#'
#' @aliases get_fields,DataHistorical
#'
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_fields", "DataHistorical", function(object) object@fields)


## data ####

### DataHistorical ####
#' @rdname get_data-methods
#'
#' @aliases get_data,DataHistorical
#'
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_data", "DataHistorical", function(object) object@data)


## call ####

### DataInfo ####
#' @rdname get_call-methods
#'
#' @aliases get_call,DataInfo
#'
#'
#' @export
setMethod("get_call", "DataInfo", function(object) object@call)

### DataHistorical ####
#' @rdname get_call-methods
#'
#' @aliases get_call,DataHistorical
#'
#'
#' @export
setMethod("get_call", "DataHistorical", function(object) object@call)



# periods ####

## DataHistorical ####
#' @rdname get_periods-methods
#'
#' @aliases get_periods,DataHistorical
#'
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_periods", "DataHistorical", function(object) {

  dplyr::group_by(object@data, ticker, field) %>%
    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ticker, field) %>%
    data.table::as.data.table()

})

## FuturesMarket ####
#' @rdname get_periods-methods
#'
#' @aliases get_periods,DataHistorical,FuturesMarket
#'
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_periods", "FuturesMarket", function(object) {

  data <- switch (class(object),
                  "FuturesTS" = dplyr::left_join(object@data,
                                                 dplyr::select(object@term_structure_tickers, `active contract ticker`, ticker),
                                                 by = "ticker") %>%
                    dplyr::group_by(`active contract ticker`, ticker, field) %>%
                    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>% dplyr::ungroup() %>%
                    dplyr::arrange(`active contract ticker`, ticker, field),
                  "FuturesAggregate" = dplyr::group_by(object@data, ticker, field) %>%
                    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>% dplyr::ungroup() %>%
                    dplyr::arrange(ticker, field)
  )

  data.table::as.data.table(data)

})


## FuturesCFTC ####
#' @rdname get_periods-methods
#'
#' @aliases get_periods,DataHistorical,FuturesCFTC
#'
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_periods", "FuturesCFTC", function(object) {

  dplyr::left_join(object@data, object@cftc_tickers, by = "ticker") %>%
    dplyr::group_by(`active contract ticker`, ticker) %>%
    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(`active contract ticker`, ticker) %>%
    data.table::as.data.table()

})



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
    update_CFTC_tickers(tickers = object@cftc_tickers, con)
  cftc_tickers <- RSQLite::dbReadTable(con, "tickers_support_futures_cftc")

  dates <- paste0("SELECT * FROM support_dates WHERE date >= '", min(object@data$date), "' AND date <= '",  max(object@data$date), "';")
  dates <- dplyr::semi_join(RSQLite::dbGetQuery(con, dates) %>% dplyr::mutate(date = as.Date(date)),
                            dplyr::distinct(object@data, date), by = "date") %>%
    dplyr::mutate(date = as.Date(date))

  for (i in unique(dates$period)){
    update_data_CFTC(data = dplyr::semi_join(object@data, dplyr::filter(dates, period == i), by = "date"),
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




## index ####

### market ####
setMethod("db_store", signature = c(object = "IndexMarket"), function(object, file, verbose){

  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file), stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database file (ie. ~/storethat.sqlite)")

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)

  if (! all_fields_exist(fields = object@fields, con = con)) update_fields(fields = object@fields, con = con)
  fields <- "SELECT * FROM support_fields WHERE instrument = 'index' AND book = 'market';"
  fields <- RSQLite::dbGetQuery(con = con, fields)


  if (!all_tickers_exist(tickers = unique(object@tickers$ticker), table_tickers = "tickers_index", con = con))
    update_tickers(tickers = unique(object@tickers$ticker), table_tickers = "tickers_index", con = con)
  tickers <- RSQLite::dbReadTable(con, "tickers_index")


  dates <- paste0("SELECT * FROM support_dates WHERE date >= '", min(object@data$date), "' AND date <= '",  max(object@data$date), "';")
  dates <- dplyr::semi_join(RSQLite::dbGetQuery(con, dates) %>% dplyr::mutate(date = as.Date(date)),
                            dplyr::distinct(object@data, date), by = "date") %>%
    dplyr::mutate(date = as.Date(date))

  for (i in unique(dates$period)){
    update_data(data = dplyr::semi_join(object@data, dplyr::filter(dates, period == i), by = "date"),
                table_data = paste0("data_index_market_", i), tickers = tickers, fields = fields,
                dates = dplyr::filter(dates, period == i), con = con)
    if (verbose) done(paste0("Period ", data.table::first(dplyr::filter(dates, period == i)$date), "/",
                             data.table::last(dplyr::filter(dates, period == i)$date), " done."))
  }

  RSQLite::dbDisconnect(con)
})

### info ####
setMethod("db_store", signature = c(object = "IndexInfo"), function(object, file){

  if (is.null(file)) file <- file.choose()
  else
    if (! all(rlang::is_scalar_character(file), stringr::str_detect(file, pattern = ".+storethat\\.sqlite$")))
      stop("Parameter 'file' must be supplied as a valid 'storethat' SQLite database file (ie. ~/storethat.sqlite)")

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)

  if (! all_fields_exist(fields = object@fields, con = con)) update_fields(fields = object@fields, con = con)
  fields <- "SELECT * FROM support_fields WHERE instrument = 'index' AND book = 'info';"
  fields <- RSQLite::dbGetQuery(con = con, fields)


  if (!all_tickers_exist(tickers = unique(object@info$ticker), table_tickers = "tickers_index", con))
    update_tickers(tickers = unique(object@info$ticker), table_tickers = "tickers_index", con)
  tickers <- RSQLite::dbReadTable(con, "tickers_index")


  date_id <- paste0("SELECT id from support_dates WHERE date = '", as.character(Sys.Date()), "';")
  date_id <- RSQLite::dbGetQuery(con, date_id) %>% purrr::flatten_chr()

  query <- paste0("DELETE FROM data_index_info WHERE ticker_id IN (",
                  paste(dplyr::filter(tickers, ticker %in% unique(object@info$ticker))$id, collapse = ", "),
                  ");")
  RSQLite::dbExecute(con = con, query)

  query <- dplyr::left_join(object@info, tickers, by = "ticker") %>% dplyr::select(ticker_id = id, field, value) %>%
    dplyr::mutate(field = as.character(field)) %>%
    dplyr::left_join(fields, by = c("field" = "symbol")) %>% dplyr::select(ticker_id, field_id = id, value) %>%
    dplyr::mutate(date_id = !! date_id) %>% dplyr::select(ticker_id, field_id, date_id, value)

  RSQLite::dbWriteTable(con = con, "data_index_info", query, row.names = FALSE, overwrite = FALSE, append = TRUE)
  RSQLite::dbDisconnect(con)
})
