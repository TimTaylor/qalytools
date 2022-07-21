# check if entries of a vector are whole numbers
.is_whole <- function(x, tol = .Machine$double.eps^0.5) {
    abs(x - round(x)) < tol
}

# assert character (returns invisibly or errors)
.assert_character <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    if (missing(x)) {
        msg <- sprintf("argument `%s` is missing, with no default.", arg)
        stop(simpleError(msg, call[1L]))
    }
    if (!is.character(x)) {
        msg <- sprintf("`%s` must be a character vector.", arg)
        stop(simpleError(msg, call[1L]))
    }
    invisible(x)
}

# assert class (returns input invisibly or errors)
.assert_class <- function(x, class, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    if (missing(x)) {
        msg <- sprintf("argument `%s` is missing, with no default.", arg)
        stop(simpleError(msg, call[1L]))
    }
    if (!inherits(x, class)) {
        msg <- sprintf("`%s` must have class <%s>.", arg, class)
        stop(simpleError(msg, call[1L]))
    }
    invisible(x)
}

# function to use in default s3 methods with no implementation
.class_not_implemented <- function(x, call = sys.call(-1L)) {
    cls <- class(x)
    cls <- paste0(cls, collapse = "/")
    msg <- sprintf("Not implemented for <%s> objects.", cls)
    stop(simpleError(msg, call[1L]))
}


# return the version of an eq5d object
.get_version <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    if (inherits(x, "EQ5D5L")) {
        "5L"
    } else if (inherits(x, "EQ5D3L")) {
        "3L"
    } else if (inherits(x, "EQ5DY")) {
        "Y"
    } else {
        msg <- sprintf("`%s` must be of class <EQ5D5L>, <EQ5D3L> or <EQ5DY>.", arg)
        stop(simpleError(msg, call[1L]))
    }
}


# return a named vector of the dimension names
.get_dimension_names <- function(x) {
    c(
        mobility = attr(x, "mobility"),
        self_care = attr(x, "self_care"),
        usual = attr(x, "usual"),
        pain = attr(x, "pain"),
        anxiety = attr(x, "anxiety")
    )
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
    setNames(c("MO", "SC", "UA", "PD", "AD"), dimensions)
}

# convert rownames to a column
.rownames_2_column <- function(x, var = "rowname") {
    stopifnot(is.data.frame(x))
    rnms <- rownames(x)
    nms <- names(x)
    if (var %in% nms) {
        stop(sprintf('"%s" is already a column in the input data frame.', var))
    }
    rownames(x) <- NULL
    setNames(cbind(rnms, x), c(var, nms))
}
