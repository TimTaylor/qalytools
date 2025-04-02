# function to use in default s3 methods with no implementation
.class_not_implemented <- function(x, call = sys.call(-1L)) {
    cls <- class(x)
    .stop(sprintf("Not implemented for <%s> objects.", cls[1L]))
}


# return the version of an eq5d object
.get_version <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    if (inherits(x, "EQ5D5L")) {
        "5L"
    } else if (inherits(x, "EQ5D3L")) {
        "3L"
    } else if (inherits(x, "EQ5DY3L")) {
        "Y3L"
    } else {
        stop()
    }
}

# return a named vector of the dimension names
.get_dimension_names <- function(x) {
    unlist(attributes(x)[c("mobility", "self_care", "usual", "pain", "anxiety")])
}

# check if entries of a vector are whole numbers
.is_whole <- function(x, tol = .Machine$double.eps^0.5) {
    abs(x - round(x)) < tol
}

# assert class (returns input invisibly or errors)
.assert_class <- function(x, class, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    if (missing(x)) {
        .stop(
            sprintf("`%s` is missing, with no default.", arg),
            .call = call
        )

    }
    if (!inherits(x, class)) {
        .stop(
            sprintf("`%s` must have class <%s>.", arg, class),
            .call = call
        )
    }
    invisible(x)
}


# generate lookup
.make_lookup <- function(x) {
    dimensions <- c(
        mobility = attr(x, "mobility"),
        self_care = attr(x, "self_care"),
        usual = attr(x, "usual"),
        pain = attr(x, "pain"),
        anxiety = attr(x, "anxiety")
    )
    stats::setNames(c("MO", "SC", "UA", "PD", "AD"), dimensions)
}

# convert rownames to a column
.rownames_2_column <- function(x, var = "rowname") {
    stopifnot(is.data.frame(x))
    rnms <- rownames(x)
    nms <- names(x)
    if (var %in% nms)
        stop(sprintf("`%s` is already a column in the input data frame.", var))
    rownames(x) <- NULL
    stats::setNames(cbind(rnms, x), c(var, nms))
}

# -------------------------------------------------------------------------
# To quote Davis Vaughan ...
# "This function is a data frame specific helper. Currently we are recommended
#  to copy in to our own package but it may eventually find it's way in to one
#  of the tidy packages."
.df_reconstruct <- function(x, to) {
    attrs <- attributes(to)
    attrs$names <- names(x) # Keep column and row names of `x`
    attrs$row.names <- .row_names_info(x, type = 0L)
    attributes(x) <- attrs # Otherwise copy over attributes of `to`
    x
}
