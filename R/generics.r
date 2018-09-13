#' Generic method for accessing the 'tickers' slot of various S4 objects from the \code{pullit} package.
#'
#' @param object an S4 object of class \linkS4class{BBGHistorical} or childs.
#'
#' @return A tibble.
#'
#' @export
#'
#' @docType methods
#'
#' @rdname get_tickers-methods
setGeneric("get_tickers", function(object) standardGeneric("get_tickers"))

#' Generic method for accessing the 'periods' for which various S4 objects from the \code{pullit} package contain data.
#'
#' @param object an S4 object of class \linkS4class{BBGHistorical} or childs.
#'
#' @return A tibble.
#'
#' @export
#'
#' @docType methods
#'
#' @rdname get_periods-methods
setGeneric("get_periods", function(object) standardGeneric("get_periods"))

#' Generic method for accessing the 'fields' slot of various S4 objects from the \code{pullit} package.
#'
#' @param object an S4 object with a 'fields' slot.
#'
#' @export
setGeneric("get_fields", function(object) standardGeneric("get_fields"))

#' Generic method for accessing the 'data' slot of various S4 objects from the \code{pullit} package.
#'
#' @param object an S4 object with a 'data' slot.
#'
#' @export
setGeneric("get_data", function(object) standardGeneric("get_data"))

#' Generic method for accessing the 'data' slot of various S4 objects from the \code{pullit} package.
#'
#' @param object an S4 object with a 'call' slot.
#'
#' @export
setGeneric("get_call", function(object) standardGeneric("get_call"))

#' Generic method for plotting historical futures term structure data contained in an S4 object
#'   of class \linkS4class{FuturesTS} from the \code{pullit} package.
#'
#' @param object an S4 object of class \linkS4class{FuturesTS}.
#' @param ticker a scalar character vector. Active contract Bloomberg ticker to plot the
#'   term structure for.
#'
#' @export
setGeneric("plot_TS", function(object, ticker) standardGeneric("plot_TS"))
