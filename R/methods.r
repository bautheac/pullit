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
      "  cftc_tickers: access with get_cftc_tickers()\n",
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
#' @rdname get_cftc_tickers-methods
#'
#' @aliases get_cftc_tickers,FuturesCFTC
#'
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_cftc_tickers", signature = c("FuturesCFTC"), function(object) object@cftc_tickers)

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
