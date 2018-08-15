if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "active contract ticker", "field", "fields", "instrument", "name", "participant", "position", "PX_LAST",
                                                        "roll", "roll adjustment", "roll days", "roll months", "roll type", "rolls", "section", "subsection", "symbol",
                                                        "ticker", "tickers_cftc", "TS position", "type", "underlying", "unit", "value", "y"))

setOldClass(c("tbl_df", "tbl", "data.frame"))

#' S4 class for Bloomberg historical data objects
#'
#' @importClassesFrom data.table data.table
#'
#' @export
setClass("BBGHistorical",
         representation(tickers = "data.table", fields = "data.table", data = "data.table", call = "character"))

#' S4 class for futures term structure objects
#' @export
setClass("FuturesTS", contains = "BBGHistorical")

#' S4 class for futures aggregate objects
#' @export
setClass("FuturesAggregate", contains = "BBGHistorical")

#' S4 class for futures CFTC objects
#' @export
setClass("FuturesCFTC", contains = "BBGHistorical")

#' S4 class for equity market objects
#' @export
setClass("EquityMarket", contains = "BBGHistorical")

#' S4 class for equity key stats objects
#' @export
setClass("EquityKS", contains = "BBGHistorical")

#' S4 class for equity balance sheet objects
#' @export
setClass("EquityBS", contains = "BBGHistorical")

#' S4 class for equity cash flow statement objects
#' @export
setClass("EquityCF", contains = "BBGHistorical")

#' S4 class for equity income statement objects
#' @export
setClass("EquityIS", contains = "BBGHistorical")

#' S4 class for equity ratios objects
#' @export
setClass("EquityRatios", contains = "BBGHistorical")















