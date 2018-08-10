# FuturesTS ####

#' Show method for S4 object of class \code{FuturesTS}.
#'
#' @param object an S4 object of class \linkS4class{FuturesTS}.
#'
#' @importFrom methods is show
#'
#' @export
setMethod("show", signature(object = "FuturesTS"), function(object) {
  cat(is(object)[[1]],
      "Slots inlude:\n",
      "  tickers: access with tickers()\n",
      "  fields: access with fields()\n",
      "  dataset: access with dataset()\n",
      "See also: periods()")
})

#' 'tickers' slot accessor method for S4 objects of class \linkS4class{FuturesTS}.
#'
#' @param object an S4 object of class \linkS4class{FuturesTS}.
#'
#' @return A tibble. Columns include:
#'    \itemize{
#'      \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'      \item{\code{ticker}: futures term structure Bloomberg tickers for which data has been found.}
#'      \item{\code{TS position}: corresponding futures term structure position.}
#'      \item{\code{roll type}: corresponding roll type (by name).}
#'      \item{\code{roll days}: corresponding day(s) offset.}
#'      \item{\code{roll months}: corresponding month(s) offset.}
#'      \item{\code{roll adjustment}: corresponding roll adjustment (by name).}
#'    }
#'
#' @export
setMethod("tickers", "FuturesTS", function(object) object@tickers)

#' 'periods' method for S4 objects of class \linkS4class{FuturesTS}.
#'
#' @param object an S4 object of class \linkS4class{FuturesTS}.
#'
#' @return A tibble. Columns include:
#'    \itemize{
#'      \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'      \item{\code{TS position}: futures term structure positions for which data has been found.}
#'      \item{\code{field}: futures market data fields for which data has been found.}
#'      \item{\code{start}: oldest date with observed data.}
#'      \item{\code{end}: most recent date with observed data.}
#'    }
#'
#' @importFrom dplyr arrange funs group_by summarise_at ungroup vars
#' @importFrom magrittr "%>%" "%<>%"
#'
#' @export
setMethod("periods", "FuturesTS", function(object) {
  object@dataset %>%
    group_by(`active contract ticker`, `TS position`, field) %>%
    summarise_at(vars(date), funs(start = min, end = max)) %>%
    ungroup() %>%
    arrange(`active contract ticker`, `TS position`, field)
})

#' 'fields' slot accessor method for S4 objects of class \linkS4class{FuturesTS}.
#'
#' @param object an S4 object of class \linkS4class{FuturesTS}.
#'
#' @return A tibble. Columns include:
#'    \itemize{
#'      \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'      \item{\code{TS position}: corresponding futures chain term structure positions for which data has been found.}
#'      \item{\code{field}: futures Bloomberg market data fields for which data has been found.}
#'    }
#'
#' @export
setMethod("fields", "FuturesTS", function(object) object@fields )

#' 'dataset' slot accessor method for S4 objects of class \linkS4class{FuturesTS}.
#'
#' @param object an S4 object of class \linkS4class{FuturesTS}.
#'
#' @return A tibble. Columns include:
#'    \itemize{
#'      \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'      \item{\code{TS position}: futures chain term structure positions for which data has been found.}
#'      \item{\code{field}: futures Bloomberg market data fields for which data has been found.}
#'      \item{\code{date}: observation date.}
#'      \item{\code{value}: corresponding value.}
#'    }
#'
#' @export
setMethod("dataset", "FuturesTS", function(object) object@dataset )


# FuturesAggregate ####

#' Show method for S4 object of class \code{FuturesAggregate}.
#'
#' @param object an S4 object of class \linkS4class{FuturesAggregate}.
#'
#' @importFrom methods is show
#'
#' @export
setMethod("show", signature(object = "FuturesAggregate"), function(object) {
  cat(is(object)[[1]],
      "Slots inlude:\n",
      "  tickers: access with tickers()\n",
      "  fields: access with fields()\n",
      "  dataset: access with dataset()\n",
      "See also: periods()")
})

#' 'tickers' slot accessor method for S4 objects of class \linkS4class{FuturesAggregate}.
#'
#' @param object an S4 object of class \linkS4class{FuturesAggregate}.
#'
#' @return A character vector: active contract Bloomberg tickers for which data has been found.
#'
#' @export
setMethod("tickers", "FuturesAggregate", function(object) object@tickers)

#' 'periods' method for S4 objects of class \linkS4class{FuturesAggregate}.
#'
#' @param object an S4 object of class \linkS4class{FuturesAggregate}.
#'
#' @return A tibble. Columns include:
#'    \itemize{
#'      \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'      \item{\code{field}: futures Bloomberg aggregate data fields for which data has been found.}
#'      \item{\code{start}: oldest date with observed data.}
#'      \item{\code{end}: most recent date observed with data.}
#'    }
#'
#' @importFrom magrittr "%>%"
#'
#' @export
setMethod("periods", "FuturesAggregate", function(object) {
  object@dataset %>%
    dplyr::group_by(`active contract ticker`, field) %>%
    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(`active contract ticker`, field)
})

#' 'fields' slot accessor method for S4 objects of class \linkS4class{FuturesAggregate}.
#'
#' @param object an S4 object of class \linkS4class{FuturesAggregate}.
#'
#' @return A tibble. Columns include:
#'    \itemize{
#'      \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'      \item{\code{field}: futures Bloomberg aggregate data fields for which data has been found.}
#'    }
#'
#' @export
setMethod("fields", "FuturesAggregate", function(object) object@fields )

#' 'dataset' slot accessor method for S4 objects of class \linkS4class{FuturesAggregate}.
#'
#' @param object an S4 object of class \linkS4class{FuturesAggregate}.
#'
#' @return A tibble. Columns include:
#'    \itemize{
#'      \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'      \item{\code{field}: Bloomberg aggregate data fields for which data has been found.}
#'      \item{\code{date}: observation date.}
#'      \item{\code{value}: corresponding value.}
#'    }
#'
#' @export
setMethod("dataset", "FuturesAggregate", function(object) object@dataset )


# FuturesCFTC ####

#' Show method for S4 object of class \code{FuturesCFTC}.
#'
#' @param object an S4 object of class \linkS4class{FuturesCFTC}.
#'
#' @importFrom methods is show
#'
#' @export
setMethod("show", signature(object = "FuturesCFTC"), function(object) {
  cat(is(object)[[1]],
      "Slots inlude:\n",
      "  tickers: access with tickers()\n",
      "  fields: access with fields()\n",
      "  dataset: access with dataset()\n",
      "See also: periods()")
})

#' 'tickers' slot accessor method for S4 objects of class \linkS4class{FuturesCFTC}.
#'
#' @param object an S4 object of class \linkS4class{FuturesCFTC}.
#'
#' @return A character vector: active contract Bloomberg tickers for which data has been found.
#'
#' @export
setMethod("tickers", "FuturesCFTC", function(object) object@tickers)

#' 'periods' method for S4 objects of class \linkS4class{FuturesCFTC}.
#'
#' @param object an S4 object of class \linkS4class{FuturesCFTC}.
#'
#' @return A tibble. Columns include:
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
#' @export
setMethod("periods", "FuturesCFTC", function(object) {
  object@dataset %>%
    dplyr::group_by(`active contract ticker`, format, underlying, unit, participant, position) %>%
    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(`active contract ticker`, format, underlying, unit, participant, position)
})

#' 'fields' slot accessor method for S4 objects of class \linkS4class{FuturesCFTC}.
#'
#' @param object an S4 object of class \linkS4class{FuturesCFTC}.
#'
#' @return A tibble. Columns include:
#'   \itemize{
#'     \item{\code{format}: report formats ('legacy', 'disaggregated', 'supplemental' or 'traders in financial futures')
#'       for which data has been found.}
#'     \item{\code{underlying}: underlying instruments ('futures only' or 'futures & options') for which data has been found..}
#'     \item{\code{unit}: counting units (number of 'contracts', 'traders' or 'total') for which data has been found.}
#'     \item{\code{participant}: trader classifications for which data has been found. Report specific:
#'       \itemize{
#'         \item{legacy: 'commercial', 'non-commercial', 'non-reportable', 'total'.}
#'         \item{disaggregated: 'managed money', 'producer/merchant/processor/user', 'swap dealers', 'other reportables'.}
#'         \item{supplemental: 'commercial - non-CIT', 'non-commercial - non-CIT', 'index traders - non-CIT', 'index traders'.}
#'         \item{traders in financial futures: 'asset manager/institutional', 'dealer/intermediary', 'leveraged funds', 'other reportables'.}
#'       }
#'     }
#'     \item{\code{position}: trader positions for which data has been found. Participant specific.
#'       \itemize{
#'         \item{commercial: 'long', 'short', 'net'.}
#'         \item{non-commercial: 'long', 'short', 'net', 'spreading'.}
#'         \item{non-reportable: 'long', 'short', 'net'.}
#'         \item{total: 'long', 'short', 'net', 'open interest', 'total'.}
#'         \item{managed money: 'long', 'short', 'net', 'spreading'.}
#'         \item{producer/merchant/processor/user: 'long', 'short', 'net'.}
#'         \item{swap dealers: 'long', 'short', 'net', 'spreading'.}
#'         \item{other reportables: 'long', 'short', 'net', 'spreading'.}
#'         \item{commercial - non-CIT: 'long', 'short', 'net'.}
#'         \item{non-commercial - non-CIT: 'long', 'short', 'net', 'spreading'.}
#'         \item{index traders - non-CIT: 'long', 'short'.}
#'         \item{index traders: 'long', 'short', 'net'.}
#'         \item{asset manager/institutional: 'long', 'short', 'net', 'spreading'.}
#'         \item{dealer/intermediary: 'long', 'short', 'net', 'spreading'.}
#'         \item{leveraged funds: 'long', 'short', 'net', 'spreading'.}
#'         \item{other reportables: 'long', 'short', 'net', 'spreading'.}
#'       }
#'     }
#'   }
#'
#' @export
setMethod("fields", "FuturesCFTC", function(object) object@fields )

#' 'dataset' slot accessor method for S4 objects of class \linkS4class{FuturesCFTC}.
#'
#' @param object an S4 object of class \linkS4class{FuturesCFTC}.
#'
#' @return A tibble. Columns include:
#'   \itemize{
#'     \item{\code{active contract ticker}: futures active contract Bloomberg tickers for which data has been found.}
#'     \item{\code{format}: report formats for which data has been found.}
#'     \item{\code{underlying}: underlying instruments for which data has been found..}
#'     \item{\code{unit}: counting units for which data has been found.}
#'     \item{\code{participant}: trader classifications for which data has been found.}
#'     \item{\code{position}: trader positions for which data has been found.}
#'     \item{\code{date}: observation date.}
#'     \item{\code{value}: observed value}
#'   }
#'
#' @export
setMethod("dataset", "FuturesCFTC", function(object) object@dataset )


# EquityMarket ####

#' Show method for S4 object of class \code{EquityMarket}.
#'
#' @param object an S4 object of class \linkS4class{EquityMarket}.
#'
#' @importFrom methods is show
#'
#' @export
setMethod("show", signature(object = "EquityMarket"), function(object) {
  cat(is(object)[[1]],
      "Slots inlude:\n",
      "  tickers: access with tickers()\n",
      "  fields: access with fields()\n",
      "  dataset: access with dataset()\n",
      "See also: periods()")
})

#' 'tickers' slot accessor method for S4 objects of class \linkS4class{EquityMarket}.
#'
#' @param object an S4 object of class \linkS4class{EquityMarket}.
#'
#' @return A character vector: equity Bloomberg tickers for which data has been found.
#'
#' @export
setMethod("tickers", "EquityMarket", function(object) object@tickers)

#' 'periods' method for S4 objects of class \linkS4class{EquityMarket}.
#'
#' @param object an S4 object of class \linkS4class{EquityMarket}.
#'
#' @return A tibble. Columns include:
#'   \itemize{
#'     \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'     \item{\code{field}: equity Bloomberg market data fields for which data has been found.}
#'     \item{\code{start}: oldest date with observed data.}
#'     \item{\code{end}: most recent date with observed data.}
#'   }
#'
#' @importFrom magrittr "%>%"
#'
#' @export
setMethod("periods", "EquityMarket", function(object) {
  object@dataset %>%
    dplyr::group_by(ticker, field) %>%
    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ticker, field)
})

#' 'fields' slot accessor method for S4 objects of class \linkS4class{EquityMarket}.
#'
#' @param object an S4 object of class \linkS4class{EquityMarket}.
#'
#' @return A tibble. Columns include:
#'   \itemize{
#'     \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'     \item{\code{field}: equity Bloomberg market data fields for which data has been found.}
#'   }
#'
#' @export
setMethod("fields", "EquityMarket", function(object) object@fields )

#' 'dataset' slot accessor method for S4 objects of class \linkS4class{EquityMarket}.
#'
#' @param object an S4 object of class \linkS4class{EquityMarket}.
#'
#' @return A tibble. Columns include:
#'   \itemize{
#'     \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'     \item{\code{field}: equity Bloomberg market data fields for which data has been found.}
#'     \item{\code{date}: observation date.}
#'     \item{\code{value}: corresponding value.}
#'   }
#'
#' @export
setMethod("dataset", "EquityMarket", function(object) object@dataset )





# EquityBS ####

#' Show method for S4 object of class \code{EquityBS}.
#'
#' @param object an S4 object of class \linkS4class{EquityBS}.
#'
#' @importFrom methods is show
#'
#' @export
setMethod("show", signature(object = "EquityBS"), function(object) {
  cat(is(object)[[1]],
      "Slots inlude:\n",
      "  tickers: access with tickers()\n",
      "  fields: access with fields()\n",
      "  dataset: access with dataset()\n",
      "See also: periods()")
})

#' 'tickers' slot accessor method for S4 objects of class \linkS4class{EquityBS}.
#'
#' @param object an S4 object of class \linkS4class{EquityBS}.
#'
#' @return A character vector: equity Bloomberg tickers for which data has been found.
#'
#' @export
setMethod("tickers", "EquityBS", function(object) object@tickers)

#' 'periods' method for S4 objects of class \linkS4class{EquityBS}.
#'
#' @param object an S4 object of class \linkS4class{EquityBS}.
#'
#' @return A tibble. Columns include:
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
#' @export
setMethod("periods", "EquityBS", function(object) {
  object@dataset %>%
    dplyr::group_by(ticker, section, subsection, name) %>%
    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ticker, section, subsection, name)
})

#' 'fields' slot accessor method for S4 objects of class \linkS4class{EquityBS}.
#'
#' @param object an S4 object of class \linkS4class{EquityBS}.
#'
#' @return A tibble. Columns include:
#'   \itemize{
#'     \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'     \item{\code{section}: balance sheet sections ('asset', 'liabilities') for which data has been found.}
#'     \item{\code{subsection}: balance sheet subsections ('asset': 'current', 'long term'; 'liabilities':
#'       'debt', 'equity') for which data has been found.}
#'     \item{\code{name}: balance sheet Bloomberg data field names for which data has been found.}
#'   }
#'
#' @export
setMethod("fields", "EquityBS", function(object) object@fields )

#' 'dataset' slot accessor method for S4 objects of class \linkS4class{EquityBS}.
#'
#' @param object an S4 object of class \linkS4class{EquityBS}.
#'
#' @return A tibble. Columns include:
#'   \itemize{
#'     \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'     \item{\code{section}: balance sheet sections ('asset', 'liabilities') for which data has been found.}
#'     \item{\code{subsection}: balance sheet subsections ('asset': 'current', 'long term'; 'liabilities':
#'       'debt', 'equity') for which data has been found.}
#'     \item{\code{name}: balance sheet Bloomberg data field names for which data has been found.}
#'     \item{\code{date}: observation date.}
#'     \item{\code{value}: corresponding observation.}
#'   }
#'
#' @export
setMethod("dataset", "EquityBS", function(object) object@dataset )






# EquityCF ####

#' Show method for S4 object of class \code{EquityCF}.
#'
#' @param object an S4 object of class \linkS4class{EquityCF}.
#'
#' @importFrom methods is show
#'
#' @export
setMethod("show", signature(object = "EquityCF"), function(object) {
  cat(is(object)[[1]],
      "Slots inlude:\n",
      "  tickers: access with tickers()\n",
      "  fields: access with fields()\n",
      "  dataset: access with dataset()\n",
      "See also: periods()")
})

#' 'tickers' slot accessor method for S4 objects of class \linkS4class{EquityCF}.
#'
#' @param object an S4 object of class \linkS4class{EquityCF}.
#'
#' @return A character vector: equity Bloomberg tickers for which data has been found.
#'
#' @export
setMethod("tickers", "EquityCF", function(object) object@tickers)

#' 'periods' method for S4 objects of class \linkS4class{EquityCF}.
#'
#' @param object an S4 object of class \linkS4class{EquityCF}.
#'
#' @return A tibble. Columns include:
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
#' @export
setMethod("periods", "EquityCF", function(object) {
  object@dataset %>%
    dplyr::group_by(ticker, section, name) %>%
    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ticker, section, name)
})

#' 'fields' slot accessor method for S4 objects of class \linkS4class{EquityCF}.
#'
#' @param object an S4 object of class \linkS4class{EquityCF}.
#'
#' @return A tibble. Columns include:
#'   \itemize{
#'     \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'     \item{\code{section}: cash flow statement sections ('operating', 'financing', 'investing',
#'       'total') for which data has been found.}
#'     \item{\code{name}: cash flow statement Bloomberg field names for which data has been found.}
#'   }
#'
#' @export
setMethod("fields", "EquityCF", function(object) object@fields )

#' 'dataset' slot accessor method for S4 objects of class \linkS4class{EquityCF}.
#'
#' @param object an S4 object of class \linkS4class{EquityCF}.
#'
#' @return A tibble. Columns include:
#'   \itemize{
#'     \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'     \item{\code{section}: cash flow statement sections ('operating', 'financing', 'investing',
#'       'total') for which data has been found.}
#'     \item{\code{name}: cash flow statement Bloomberg field names for which data has been found.}
#'     \item{\code{date}: observation date.}
#'     \item{\code{value}: corresponding observation.}
#'   }
#'
#' @export
setMethod("dataset", "EquityCF", function(object) object@dataset )







# EquityIS ####

#' Show method for S4 object of class \code{EquityIS}.
#'
#' @param object an S4 object of class \linkS4class{EquityIS}.
#'
#' @importFrom methods is show
#'
#' @export
setMethod("show", signature(object = "EquityIS"), function(object) {
  cat(is(object)[[1]],
      "Slots inlude:\n",
      "  tickers: access with tickers()\n",
      "  fields: access with fields()\n",
      "  dataset: access with dataset()\n",
      "See also: periods()")
})

#' 'tickers' slot accessor method for S4 objects of class \linkS4class{EquityIS}.
#'
#' @param object an S4 object of class \linkS4class{EquityIS}.
#'
#' @return A character vector: equity Bloomberg tickers for which data has been found.
#'
#' @export
setMethod("tickers", "EquityIS", function(object) object@tickers)

#' 'periods' method for S4 objects of class \linkS4class{EquityIS}.
#'
#' @param object an S4 object of class \linkS4class{EquityIS}.
#'
#' @return A tibble. Columns include:
#'   \itemize{
#'     \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'     \item{\code{name}: income statement Bloomberg field names for which data has been found.}
#'     \item{\code{start}: oldest date with observed data.}
#'     \item{\code{end}: most recent date with observed data.}
#'   }
#'
#' @importFrom magrittr "%>%"
#'
#' @export
setMethod("periods", "EquityIS", function(object) {
  object@dataset %>%
    dplyr::group_by(ticker, name) %>%
    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ticker, name)
})

#' 'fields' slot accessor method for S4 objects of class \linkS4class{EquityIS}.
#'
#' @param object an S4 object of class \linkS4class{EquityIS}.
#'
#' @return A tibble. Columns include:
#'   \itemize{
#'     \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'     \item{\code{name}: income statement Bloomberg field names for which data has been found.}
#'   }
#'
#' @export
setMethod("fields", "EquityIS", function(object) object@fields )

#' 'dataset' slot accessor method for S4 objects of class \linkS4class{EquityIS}.
#'
#' @param object an S4 object of class \linkS4class{EquityIS}.
#'
#' @return A tibble. Columns include:
#'   \itemize{
#'     \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'     \item{\code{name}: income statement Bloomberg field names for which data has been found.}
#'     \item{\code{date}: observation date.}
#'     \item{\code{value}: corresponding observation.}
#'   }
#'
#' @export
setMethod("dataset", "EquityIS", function(object) object@dataset )





# EquityRatios ####

#' Show method for S4 object of class \code{EquityRatios}.
#'
#' @param object an S4 object of class \linkS4class{EquityRatios}.
#'
#' @importFrom methods is show
#'
#' @export
setMethod("show", signature(object = "EquityRatios"), function(object) {
  cat(is(object)[[1]],
      "Slots inlude:\n",
      "  tickers: access with tickers()\n",
      "  fields: access with fields()\n",
      "  dataset: access with dataset()\n",
      "See also: periods()")
})

#' 'tickers' slot accessor method for S4 objects of class \linkS4class{EquityRatios}.
#'
#' @param object an S4 object of class \linkS4class{EquityRatios}.
#'
#' @return A character vector: equity Bloomberg tickers for which data has been found.
#'
#' @export
setMethod("tickers", "EquityRatios", function(object) object@tickers)

#' 'periods' method for S4 objects of class \linkS4class{EquityRatios}.
#'
#' @param object an S4 object of class \linkS4class{EquityRatios}.
#'
#' @return A tibble. Columns include:
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
#' @export
setMethod("periods", "EquityRatios", function(object) {
  object@dataset %>%
    dplyr::group_by(ticker, type, name) %>%
    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ticker, type, name)
})

#' 'fields' slot accessor method for S4 objects of class \linkS4class{EquityRatios}.
#'
#' @param object an S4 object of class \linkS4class{EquityRatios}.
#'
#' @return A tibble. Columns include:
#'   \itemize{
#'     \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'     \item{\code{type}: financial ratio types ('profitability', 'margin', 'asset turnover',
#'       'short term liquidity', 'long term solvency') for which data has been found.}
#'     \item{\code{name}: financial ratio Bloomberg data field names for which data has been found.}
#'   }
#'
#' @export
setMethod("fields", "EquityRatios", function(object) object@fields )

#' 'dataset' slot accessor method for S4 objects of class \linkS4class{EquityRatios}.
#'
#' @param object an S4 object of class \linkS4class{EquityRatios}.
#'
#' @return A tibble. Columns include:
#'   \itemize{
#'     \item{\code{ticker}: equity Bloomberg tickers for which data has been found.}
#'     \item{\code{type}: financial ratio types ('profitability', 'margin', 'asset turnover',
#'       'short term liquidity', 'long term solvency') for which data has been found.}
#'     \item{\code{name}: financial ratio Bloomberg data field names for which data has been found.}
#'     \item{\code{date}: observation date.}
#'     \item{\code{value}: corresponding observation.}
#'   }
#'
#' @export
setMethod("dataset", "EquityRatios", function(object) object@dataset )

