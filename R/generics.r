#' Generic method for accessing the 'tickers' slot of various S4 objects from the \code{pullit} package.
#'
#' @param object an S4 object with a 'tickers' slot.
#'
#' @export
setGeneric("get_tickers", function(object) standardGeneric("get_tickers"))

#' Generic method for accessing the 'periods' for which various S4 objects from the \code{pullit} package contain data.
#'
#' @param object an S4 object with a 'periods' slot.
#'
#' @export
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
