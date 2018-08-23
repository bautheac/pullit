# direct accessors ####

#' Show method for S4 object of class \code{BBGHistorical}.
#'
#' @param object an S4 object of class \linkS4class{BBGHistorical}.
#'
#' @importFrom methods show
#'
#' @export
setMethod("show", "BBGHistorical", function(object) {
  cat("S4 object of class",
      methods::is(object)[[1]],
      "\n\nSlots inlude\n",
      "  tickers: access with get_tickers()\n",
      "  fields: access with get_fields()\n",
      "  data: access with get_data()\n",
      "  call: access with get_call()\n",
      "\nSee also: get_periods()")
})


#' @rdname get_tickers-methods
#' @aliases get_tickers,BBGHistorical,ANY-method
setMethod("get_tickers", "BBGHistorical", function(object) object@tickers)

#' 'fields' slot accessor method for S4 objects of class \linkS4class{BBGHistorical}.
#'
#' @param object an S4 object of class \linkS4class{BBGHistorical}.
#'
#' @export
setMethod("get_fields", "BBGHistorical", function(object) object@fields)

#' 'data' slot accessor method for S4 objects of class \linkS4class{BBGHistorical}.
#'
#' @param object an S4 object of class \linkS4class{BBGHistorical}.
#'
#' @export
setMethod("get_data", "BBGHistorical", function(object) object@data)

#' 'call' slot accessor method for S4 objects of class \linkS4class{BBGHistorical}.
#'
#' @param object an S4 object of class \linkS4class{BBGHistorical}.
#'
#' @return A scalar character vector showing the original call to the constructor function,
#'
#' @export
setMethod("get_call", "BBGHistorical", function(object) object@call)


# periods ####

## FuturesTS ####

#' 'periods' method for S4 objects of class \linkS4class{FuturesTS}.
#'
#' @return For S4 objects of class \linkS4class{FuturesTS} columns include:
#'    \itemize{
#'      \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'      \item{\code{TS position}: futures term structure positions for which data has been found.}
#'      \item{\code{field}: futures market data fields for which data has been found.}
#'      \item{\code{start}: oldest date with observed data.}
#'      \item{\code{end}: most recent date with observed data.}
#'    }
#'
#' @importFrom magrittr "%>%"
#'
#' @rdname get_periods-methods
#' @aliases get_periods,BBGHistorical,FuturesTS
setMethod("get_periods", "FuturesTS", function(object) {
  object@data %>%
    dplyr::group_by(`active contract ticker`, `TS position`, field) %>%
    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(`active contract ticker`, `TS position`, field) %>%
    data.table::as.data.table()
})

## FuturesAggregate ####

#' 'periods' method for S4 objects of class \linkS4class{FuturesAggregate}.
#'
#' @return For S4 objects of class \linkS4class{FuturesAggregate} columns include:
#'    \itemize{
#'      \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'      \item{\code{field}: futures Bloomberg aggregate data fields for which data has been found.}
#'      \item{\code{start}: oldest date with observed data.}
#'      \item{\code{end}: most recent date observed with data.}
#'    }
#'
#' @importFrom magrittr "%>%"
#'
#' @rdname get_periods-methods
#' @aliases get_periods,BBGHistorical,FuturesAggregate
setMethod("get_periods", "FuturesAggregate", function(object) {
  object@data %>%
    dplyr::group_by(`active contract ticker`, field) %>%
    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(`active contract ticker`, field) %>%
    data.table::as.data.table()
})

## FuturesCFTC ####

#' 'periods' method for S4 objects of class \linkS4class{FuturesCFTC}.
#'
#' @return For S4 objects of class \linkS4class{FuturesCFTC} columns include:
#'   \itemize{
#'     \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'     \item{\code{format}: report formats for which data has been found.}
#'     \item{\code{underlying}: underlying instruments for which data has been found..}
#'     \item{\code{unit}: counting units for which data has been found.}
#'     \item{\code{participant}: trader classifications for which data has been found.}
#'     \item{\code{position}: trader positions for which data has been found.}
#'     \item{\code{start}: oldest date with observed data.}
#'     \item{\code{end}: most recent date observed with data.}
#'   }
#'
#' @importFrom magrittr "%>%"
#'
#' @rdname get_periods-methods
#' @aliases get_periods,BBGHistorical,FuturesCFTC
setMethod("get_periods", "FuturesCFTC", function(object) {
  object@data %>%
    dplyr::group_by(`active contract ticker`, format, underlying, unit, participant, position) %>%
    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(`active contract ticker`, format, underlying, unit, participant, position) %>%
    data.table::as.data.table()
})

# EquityMarket ####

#' 'periods' method for S4 objects of class \linkS4class{EquityMarket}.
#'
#' @return For S4 objects of class \linkS4class{EquityMarket} columns include:
#'   \itemize{
#'     \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'     \item{\code{field}: equity Bloomberg market data fields for which data has been found.}
#'     \item{\code{start}: oldest date with observed data.}
#'     \item{\code{end}: most recent date with observed data.}
#'   }
#'
#' @importFrom magrittr "%>%"
#'
#' @rdname get_periods-methods
#' @aliases get_periods,BBGHistorical,EquityMarket
setMethod("get_periods", "EquityMarket", function(object) {
  object@data %>%
    dplyr::group_by(ticker, field) %>%
    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ticker, field) %>%
    data.table::as.data.table()
})

## EquityKS ####

#' 'periods' method for S4 objects of class \linkS4class{EquityKS}.
#'
#' @return For S4 objects of class \linkS4class{EquityKS} columns include:
#'   \itemize{
#'     \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'     \item{\code{section}: balance sheet sections for which data has been found.}
#'     \item{\code{subsection}: balance sheet subsections for which data has been found.}
#'     \item{\code{name}: balance sheet Bloomberg data field names for which data has been found.}
#'     \item{\code{start}: oldest date with observed data.}
#'     \item{\code{end}: most recent date with observed data.}
#'   }
#'
#' @importFrom magrittr "%>%"
#'
#' @rdname get_periods-methods
#' @aliases get_periods,BBGHistorical,EquityKS
setMethod("get_periods", "EquityKS", function(object) {
  object@data %>%
    dplyr::group_by(ticker, name) %>%
    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ticker, name) %>%
    data.table::as.data.table()
})

## EquityBS ####

#' 'periods' method for S4 objects of class \linkS4class{EquityBS}.
#'
#' @return For S4 objects of class \linkS4class{EquityBS} columns include:
#'   \itemize{
#'     \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'     \item{\code{section}: balance sheet sections for which data has been found.}
#'     \item{\code{subsection}: balance sheet subsections for which data has been found.}
#'     \item{\code{name}: balance sheet Bloomberg data field names for which data has been found.}
#'     \item{\code{start}: oldest date with observed data.}
#'     \item{\code{end}: most recent date with observed data.}
#'   }
#'
#' @importFrom magrittr "%>%"
#'
#' @rdname get_periods-methods
#' @aliases get_periods,BBGHistorical,EquityBS
setMethod("get_periods", "EquityBS", function(object) {
  object@data %>%
    dplyr::group_by(ticker, section, subsection, name) %>%
    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ticker, section, subsection, name) %>%
    data.table::as.data.table()
})

## EquityIS ####

#' 'periods' method for S4 objects of class \linkS4class{EquityIS}.
#'
#' @return For S4 objects of class \linkS4class{EquityIS} columns include:
#'   \itemize{
#'     \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'     \item{\code{name}: income statement Bloomberg field names for which data has been found.}
#'     \item{\code{start}: oldest date with observed data.}
#'     \item{\code{end}: most recent date with observed data.}
#'   }
#'
#' @importFrom magrittr "%>%"
#'
#' @rdname get_periods-methods
#' @aliases get_periods,BBGHistorical,EquityIS
setMethod("get_periods", "EquityIS", function(object) {
  object@data %>%
    dplyr::group_by(ticker, name) %>%
    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ticker, name) %>%
    data.table::as.data.table()
})

## EquityCF ####

#' 'periods' method for S4 objects of class \linkS4class{EquityCF}.
#'
#' @return For S4 objects of class \linkS4class{EquityCF} columns include:
#'   \itemize{
#'     \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'     \item{\code{section}: cash flow statement sections for which data has been found.}
#'     \item{\code{name}: cash flow statement Bloomberg field names for which data has been found.}
#'     \item{\code{start}: oldest date with observed data.}
#'     \item{\code{end}: most recent date with observed data.}
#'   }
#'
#' @importFrom magrittr "%>%"
#'
#' @rdname get_periods-methods
#' @aliases get_periods,BBGHistorical,EquityCF
setMethod("get_periods", "EquityCF", function(object) {
  object@data %>%
    dplyr::group_by(ticker, section, name) %>%
    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ticker, section, name) %>%
    data.table::as.data.table()
})

## EquityRatios ####

#' 'periods' method for S4 objects of class \linkS4class{EquityRatios}.
#'
#' @return For S4 objects of class \linkS4class{EquityRatios} columns include:
#'   \itemize{
#'     \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'     \item{\code{type}: financial ratio types for which data has been found.}
#'     \item{\code{name}: financial ratio Bloomberg data field names for which data has been found.}
#'     \item{\code{start}: oldest date with observed data.}
#'     \item{\code{end}: most recent date with observed data.}
#'   }
#'
#' @importFrom magrittr "%>%"
#'
#' @rdname get_periods-methods
#' @aliases get_periods,BBGHistorical,EquityRatios
setMethod("get_periods", "EquityRatios", function(object) {
  object@data %>%
    dplyr::group_by(ticker, section, subsection, name) %>%
    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ticker, section, subsection, name) %>%
    data.table::as.data.table()
})




