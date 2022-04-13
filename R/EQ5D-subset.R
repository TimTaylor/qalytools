# The following functions are needed to make data frame subclasses work nicely
# with dplyr.  It is based on the
# (guide)[(https://github.com/DavisVaughan/2020-06-01_dplyr-vctrs-compat)]
# by Davis Vaughan. The idea is to think to an object in terms of its invariants
# (structural information that must be true for an object to be of the specified
# subclass). Where an operation breaks these invariants, a data frame is
# returned instead of the input class.

# -------------------------------------------------------------------------
#' @export
`[.EQ5D` <- function(x, i, j, ...) {
    out <- NextMethod()
    .eq5d_reconstruct(out, x)
}

# -------------------------------------------------------------------------
#' @export
`[<-.EQ5D` <- function(x, i, j, ..., value) {
    out <- NextMethod()
    col_names <- .get_dimension_names(out)
    n <- if (inherits(x, "EQ5D5L")) 5L else 3L

    # avoid slow reconstruction checks
    dat <- out
    class(dat) <- "data.frame"
    dat <- dat[, col_names]

    # check the data is numeric
    if (!all(vapply(dat, is.numeric, logical(1)))) {
        stop("Dimension values should be whole numbers", call. = FALSE)
    }

    # check that the data is bounded correctly or na
    if (!all((dat >= 1 & dat <= n) | is.na(dat))) {
        stop(sprintf("Dimension values should either be NA or bounded by 1 and %d", n), call. = FALSE)
    }

    # check that the data is whole numbers or na
    if (!(all(.is_whole(dat) | is.na(dat)))) {
        stop("Dimension values should be whole numbers or NA", call. = FALSE)
    }

    .eq5d_reconstruct(out, x)
}

# -------------------------------------------------------------------------
#' @export
`names<-.EQ5D` <- function(x, value) {
    meta <- c(
        "respondentID", "surveyID", "time_index", "mobility",
        "self_care", "usual", "pain", "anxiety", "vas"
    )

    current_names <- names(x)
    for (v in meta) {
        var <- attr(x, v)
        var_index <- match(var, current_names)
        attr(x, v) <- value[var_index]
    }

    out <- NextMethod()
    .eq5d_reconstruct(out, x)
}

# -------------------------------------------------------------------------
#' @export
rbind.EQ5D <- function(..., deparse.level = 1) {
    x <- list(...)
    out <- lapply(x, as.data.frame)
    out <- do.call(rbind, out)
    .eq5d_reconstruct(out, x[[1]])
}


# -------------------------------------------------------------------------
# To quote "This function is a data frame specific helper.  Currently we are
# recommended to copy in to our own package but it may eventually find it's way
# in to one of the tidy packages."
.df_reconstruct <- function(x, to) {
    attrs <- attributes(to)
    attrs$names <- names(x) # Keep column and row names of `x`
    attrs$row.names <- .row_names_info(x, type = 0L)
    attributes(x) <- attrs # Otherwise copy over attributes of `to`
    x
}

# -------------------------------------------------------------------------
#' @param x data.frame to have it's invariants checked
#' @param to `EQ5D` object we want
#'
#' @return TRUE or FALSE
#'
#' @noRd
.eq5d_can_reconstruct <- function(x, to) {
    x_names <- names(x)

    # ensure dimension columns are present
    cols <- .get_dimension_names(to)

    if (!all(cols %in% x_names)) {
        return(FALSE)
    }

    # check other column attributes
    vars <- c(
        surveyID = attr(to, "surveyID"),
        respondentID = attr(to, "respondentID"),
        time_index = attr(to, "time_index"),
        vas = attr(to, "vas")
    )

    if (!(all(vars %in% x_names))) {
        return(FALSE)
    }

    # ensure no repeated measurements within x
    dat <- setDT(.subset(x, vars[c("surveyID", "respondentID")]))
    if (anyDuplicated(dat))
        return(FALSE)

    # else we can reconstruct
    TRUE
}

# -------------------------------------------------------------------------
#' Function to reconstruct object of EQ5D class
#'
#' Once we have encoded the invariant logic into .eq5d_can_reconstruct, we need
#' a second function that applies that check and either performs the actual
#' reconstruction, or falls back to a bare data frame (a data frame with only
#' essential attributes).
#'
#' @param x x data.frame to have it's invariants checked
#' @param to object we want
#'
#' @noRd
.eq5d_reconstruct <- function(x, to) {
    if (.eq5d_can_reconstruct(x, to)) {
        .df_reconstruct(x, to)
    } else {
        # strip most attributes from data.frame
        a <- list(names = names(x), row.names = attr(x, "row.names"), class = "data.frame")
        attributes(x) <- a
        x
    }
}

# -------------------------------------------------------------------------
# Registered in `.onLoad()` in zzz.R to avoid hard dependency on dplyr
dplyr_reconstruct_eq5d <- function(data, template) {
    .eq5d_reconstruct(data, template)
}
