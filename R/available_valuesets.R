#' Available value sets
#'
#' @description
#'
#' Generic function that lists value sets available.
#'
#' @details
#'
#' `available_valuesets()` returns the available valuesets from the
#' [eq5d](https://cran.r-project.org/package=eq5d) package. It is an s3
#' generic that wraps the `eq5d::valuesets()` function providing additional
#' methods for [`eq5d`][new_eq5d] and `character` objects.
#'
#' For `character` objects, the input is expected to be the survey type with
#' various forms permitted:
#'
#'   - "eq5d5l", "EQ5D5L", "eq-5d-5l", "EQ-5D-5L", "5L", "5l"
#'   - "eq5d3l", "EQ5D3L", "eq-5d-3l", "EQ-5D-3L", "3L", "3l"
#'   - "eq5dy" , "EQ5DY" , "eq-5d-y" , "EQ-5D-Y" , "Y" , "y"
#'
#' If called with no arguments value sets for all available  version, type and
#' country combination are returned.
#'
#' @param x An \R object.
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A data.frame containing the available value sets for the given object
#' or survey type.
#'
#' @examples
#'
#' data("eq5d3l_example")
#' dat <- as_eq5d3l(
#'     eq5d3l_example,
#'     respondentID = "respondentID",
#'     surveyID = "surveyID",
#'     time_index = "time",
#'     mobility = "MO",
#'     self_care = "SC",
#'     usual = "UA",
#'     pain = "PD",
#'     anxiety = "AD",
#'     vas = "vas"
#' )
#' available_valuesets(dat)
#' available_valuesets("eq5d5l")
#'
#' @export
available_valuesets <- function(x, ...) {
    UseMethod("available_valuesets")
}

# -------------------------------------------------------------------------
#' @rdname available_valuesets
#' @export
available_valuesets.default <- function(x, ...) {
    if (missing(x) && !...length()) {
        return(.valuesets())
    }

    cls <- paste(class(x), collapse = ", ")
    cli_abort("Not implemented for class {.cls {cls}}")
}

# -------------------------------------------------------------------------
#' @rdname available_valuesets
#' @export
available_valuesets.EQ5D5L <- function(x, ...) {
    .valuesets(version = "5L")
}

# -------------------------------------------------------------------------
#' @rdname available_valuesets
#' @export
available_valuesets.EQ5D3L <- function(x, ...) {
    .valuesets(version = "3L")
}

# -------------------------------------------------------------------------
#' @rdname available_valuesets
#' @export
available_valuesets.EQ5DY <- function(x, ...) {
    .valuesets(version = "Y")
}

# -------------------------------------------------------------------------
#' @rdname available_valuesets
#' @export
available_valuesets.character <- function(x, ...) {
    if (!length(x) == 1L) {
        vec_assert(x, size = 1L)
    }

    x <- tolower(x)

    possible <- c("eq5d5l", "eq-5d-5l", "eq5d3l", "eq-5d-3l", "eq5dy", "eq-5d-y")
    if (!x %in% possible) {
        cli_abort(
            c(
                '{.arg x} should be one of',
                '"eq5d5l", "EQ5D5L", "eq-5d-5l", "EQ-5D-5L"',
                '"eq5d3l", "EQ5D3L", "eq-5d-3l", "EQ-5D-3L"',
                '"eq5dy" , "EQ5DY" , "eq-5d-y" , "EQ-5D-Y"'
            )
        )
    }

    x <- switch(
        x,
        "eq5d5l" = ,
        "eq-5d-5l" = "5L",
        "eq5d3l" = ,
        "eq-5d-3l" = "3L",
        "eq5dy" = ,
        "eq-5d-y" = "Y"
    )

    .valuesets(version = x)
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

.valuesets <- function(version = NULL) {
    out <- valuesets(version = version)
    class(out) <- c("tbl", "data.frame")
    out
}
