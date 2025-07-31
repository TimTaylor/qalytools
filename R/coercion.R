#' Coerce an EQ5D object
#'
# -------------------------------------------------------------------------
#' Methods to convert an EQ5D object to a data frame, tibble or data.table.
#'
# -------------------------------------------------------------------------
#' @param x An EQ5D object.
#'
#' @param row.names Not used.
#'
#' @param optional Not used.
#'
#' @param keep.rownames Not used.
#'
#' @param ... Not used.
#'
#' @param .rows Not used.
#'
#' @param .name_repair Not used.
#'
#' @param rownames Not used.
#'
# -------------------------------------------------------------------------
#' @note
#'
#' Apart from `x` no other parameters are used and they are only present
#' in the method signatures for compatibility with the underlying generic.
#'
# -------------------------------------------------------------------------
#' @return
#'
#' An appropriate representation of the data frame underlying the EQ5D object.
#' Only column names are preserved with all other attributes, including row
#' names, dropped.
#'
# -------------------------------------------------------------------------
#' @name coercion
NULL

#' @rdname coercion
#' @export
as.data.frame.EQ5D <- function(x, row.names, optional, ...) {
    if (!missing(row.names)) {
        .warning("`row.names` argument will be ignored.")
    }
    if (!missing(optional)) {
        .warning("`optional` argument will be ignored.")
    }
    list2DF(c(x))
}


#' @rdname coercion
#' @importFrom data.table as.data.table
#' @export
as.data.table.EQ5D <- function(x, keep.rownames, ...) {
    if (!missing(keep.rownames)) {
        .warning("`keep.rownames` argument will be ignored.")
    }
    out <- data.table::setDT(c(x))
    out
}

#' @rdname coercion
#' @importFrom tibble as_tibble
#' @export
as_tibble.EQ5D <- function(x, ..., .rows, .name_repair, rownames) {
    if (!missing(.rows))
        .warning("`.rows` argument will be ignored.")

    if (!missing(.name_repair))
        .warning("`.name_repair` argument will be ignored.")

    if (!missing(rownames))
        .warning("`.name_repair` argument will be ignored.")

    out <- tibble::new_tibble(c(x))
    out
}
