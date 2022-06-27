# check if entries of a vector are whole numbers
.is_whole <- function(x, tol = .Machine$double.eps^0.5) {
    abs(x - round(x)) < tol
}

# is character of length 1
.is_scalar_character <- function(x) {
    is.character(x) && length(x) == 1
}

# assert character is length 1 (returns input invisibly or errors)
# note that we are only using vec_assert for the nice error message not the
# condition check
.assert_scalar_chr <- function(x, arg = caller_arg(x), call = caller_env()) {
    if (!(is.character(x) && length(x) == 1)) {
        vec_assert(x, ptype = "", size = 1L, arg = arg, call = call)
    }
    invisible(x)
}

# assert character (returns input invisibly or errors)
# note that we are only using vec_assert for the nice error message not the
# condition check
.assert_chr <- function(x, arg = caller_arg(x), call = caller_env()) {
    if (!is.character(x)) {
        vec_assert(x, ptype = "", arg = arg, call = call)
    }
    invisible(x)
}

# assert bool (returns input invisibly or errors)
.assert_bool <- function(x, arg = caller_arg(x), call = caller_env()) {
    if (!(is.logical(x) && length(x) == 1L && !is.na(x))) {
        cli_abort("{.var {arg}} must be TRUE or FALSE", call = call)
    }
    invisible(x)
}

# assert data frame (returns input invisibly or errors)
.assert_data_frame <- function(x, arg = caller_arg(x), call = caller_env()) {
    if (!is.data.frame(x)) {
        cli_abort("{.var {arg}} must be a data.frame", call = call)
    }
    invisible(x)
}

# assert class (returns input invisibly or errors)
.assert_class <- function(x, class, arg = caller_arg(x), call = caller_env()) {
    if (!inherits(x, class)) {
        cli_abort("{.var {arg}} must be have class {.cls {class}}", call = call)
    }
    invisible(x)
}



# return the version of an eq5d object
.get_version <- function(x, arg = caller_arg(x)) {
    if (inherits(x, "EQ5D5L")) {
        "5L"
    } else if (inherits(x, "EQ5D3L")) {
        "3L"
    } else if (inherits(x, "EQ5DY")) {
        "Y"
    } else {
        cli_abort("{.arg {arg}} must be of class 'EQ5D5L', 'EQ5D3L' or 'EQ5DY'")
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
.rownames_2_column <- function(x, var = "rowname", call = caller_env()) {
    x <- .assert_data_frame(x, call = call)
    rnms <- rownames(x)
    nms <- names(x)
    if (var %in% nms) {
        cli_abort("{.arg {var}} is already a column in input data frame")
    }
    rownames(x) <- NULL
    setNames(cbind(rnms, x), c(var, nms))
}
