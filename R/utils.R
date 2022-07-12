# check if entries of a vector are whole numbers
.is_whole <- function(x, tol = .Machine$double.eps^0.5) {
    abs(x - round(x)) < tol
}

# is character of length 1
.is_scalar_character <- function(x) {
    is.character(x) && length(x) == 1
}

# assert character is length 1 (returns input invisibly or errors)
.assert_scalar_character <- function(x, label = deparse(substitute(x)), call = sys.call(-1)) {
    if (!(is.character(x) && length(x) == 1)) {
        msg <- sprintf("`%s` must be a character vector of length 1.", label)
        stop(simpleError(msg, call))
    }
    invisible(x)
}

# assert character (returns input invisibly or errors)
.assert_character <- function(x, label = deparse(substitute(x)), call = sys.call(-1)) {
    if (!is.character(x)) {
        msg <- sprintf("`%s` must be a character vector.", label)
        stop(simpleError(msg, call))
    }
    invisible(x)
}

# assert bool (returns input invisibly or errors)
.assert_bool <- function(x, label = deparse(substitute(x)), call = sys.call(-1)) {
    if (!(is.logical(x) && length(x) == 1L && !is.na(x))) {
        msg <- sprintf("`%s` must be TRUE or FALSE.", label)
        stop(simpleError(msg, call))
    }
    invisible(x)
}

# assert data frame (returns input invisibly or errors)
.assert_data_frame <- function(x, label = deparse(substitute(x)), call = sys.call(-1)) {
    if (!is.data.frame(x)) {
        msg <- sprintf("`%s` must be a data frame.", label)
        stop(simpleError(msg, call))
    }
    invisible(x)
}

# assert class (returns input invisibly or errors)
.assert_class <- function(x, class, label = deparse(substitute(x)), call = sys.call(-1)) {
    if (!inherits(x, class)) {
        msg <- sprintf("`%s` must have class %s.", label, class)
        stop(simpleError(msg, call))
    }
    invisible(x)
}

# function to use in default s3 methods with no implementation
.class_not_implemented <- function(x) {
    cls <- paste(class(x), collapse = ", ")
    msg <- sprintf("Not implemented for [%s] objects.", cls)
    call <- sys.call(-1)
    stop(simpleError(msg, call = call[1]))
}


# return the version of an eq5d object
.get_version <- function(x) {
    nm <- deparse(substitute(x))
    if (inherits(x, "EQ5D5L")) {
        "5L"
    } else if (inherits(x, "EQ5D3L")) {
        "3L"
    } else if (inherits(x, "EQ5DY")) {
        "Y"
    } else {
        stop(sprintf("`%s` must be of class 'EQ5D5L', 'EQ5D3L' or 'EQ5DY'", nm), call. = FALSE)
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
        stop(sprintf("`%s` is already a column in input data frame", var))
    }
    rownames(x) <- NULL
    setNames(cbind(rnms, x), c(var, nms))
}
