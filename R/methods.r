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
