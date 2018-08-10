#' Generic method for accessing the 'tickers' slot of various S4 objects from the \code{pullit} package.
#'
#' @param object an S4 object with a 'tickers' slot.
#'
#' @export
setGeneric("tickers", function(object) standardGeneric("tickers"))

#' Generic method for accessing the 'periods' for which various S4 objects from the \code{pullit} package contain data.
#'
#' @param object an S4 object with a 'periods' slot.
#'
#' @export
setGeneric("periods", function(object) standardGeneric("periods"))

#' Generic method for accessing the 'fields' slot of various S4 objects from the \code{pullit} package.
#'
#' @param object an S4 object with a 'fields' slot.
#'
#' @export
setGeneric("fields", function(object) standardGeneric("fields"))

#' Generic method for accessing the 'dataset' slot of various S4 objects from the \code{pullit} package.
#'
#' @param object an S4 object with a 'dataset' slot.
#'
#' @export
setGeneric("dataset", function(object) standardGeneric("dataset"))
