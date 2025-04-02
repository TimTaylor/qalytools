# -------------------------------------------------------------------------
#' Calculate limitation
#'
# -------------------------------------------------------------------------
#' Generic function that calculates the limitation of survey responses across
#' response dimensions. Here we define a limitation to be a dimension value not
#' equal to 1. Methods are provided for [EQ5D][as_eq5d] objects.
#'
# -------------------------------------------------------------------------
#'
#' @param x An \R object.
#'
#' @param ... Further arguments passed to or from other methods.
#'
# -------------------------------------------------------------------------
#' @return
#'
#' A [tibble][tibble::tbl_df-class] of the fraction of individuals without
#' limitation.
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' data(eq5d3l_example)
#' dat <- as_eq5d3l(
#'     eq5d3l_example,
#'     respondentID = "respondentID",
#'     surveyID = "surveyID",
#'     mobility = "MO",
#'     self_care = "SC",
#'     usual = "UA",
#'     pain = "PD",
#'     anxiety = "AD",
#'     vas = "vas"
#' )
#' calculate_limitation(dat)
#'
# -------------------------------------------------------------------------
#' @export
calculate_limitation <- function(x, ...) {
    UseMethod("calculate_limitation")
}

# -------------------------------------------------------------------------
#' @rdname calculate_limitation
#' @export
calculate_limitation.default <- function(x, ...) {
    .class_not_implemented(x)
}

# -------------------------------------------------------------------------
#' @rdname calculate_limitation
#' @export
calculate_limitation.EQ5D <- function(x, ...) {

    # For CRAN checks
    .N <- value <- NULL

    # get the survey variable and dimension names
    survey_var <- attr(x, "surveyID")
    dimensions <- .get_dimension_names(x)

    # convert to data.table after stripping attributes
    x <- data.table::setDT(c(x))

    # convert to long format and calculate percentage without limitation
    x <- data.table::melt(x, id.vars = survey_var, measure.vars = dimensions, variable.name = "dimension")
    x <- x[, list(without_limitation = sum(value == 1) / .N), by = c(survey_var, "dimension")]

    # return as tibble
    data.table::setDF(x)
    tibble::as_tibble(x)
}
