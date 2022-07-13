# check if entries of a vector are whole numbers
.is_whole <- function(x, tol = .Machine$double.eps^0.5) {
    abs(x - round(x)) < tol
}

# is character of length 1
.is_scalar_character <- function(x) {
    is.character(x) && length(x) == 1
}

# assert character and length 1 (returns invisibly or errors)
# uses cli and vec_assert for nice error messages
# additional branches alleviate the overhead of vec_assert when an error won't be triggered
.assert_scalar_character <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {

    if (missing(x)) {
        cli_abort("argument {.arg {arg}} is missing, with no default", call = call)
    }

    if (!(is.character(x) && length(x) == 1L)) {
        vec_assert(x, ptype = "character", size = 1L, arg = arg, call = call)
    }

    invisible(x)
}

# assert character (returns invisibly or errors)
# uses cli and vec_assert for nice error messages
# additional branches alleviate the overhead of vec_assert when an error won't be triggered
.assert_character <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
    if (missing(x)) {
        cli_abort("argument {.arg {arg}} is missing, with no default", call = call)
    }

    if (!is.character(x)) {
        vec_assert(x, ptype = "character", arg = arg, call = call)
    }

    invisible(x)
}

# assert bool (returns input invisibly or errors)
.assert_bool <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
    if (missing(x)) {
        cli_abort("argument {.arg {arg}} is missing, with no default.", call = call)
    }

    if (!(is.logical(x) && length(x) == 1L && !is.na(x))) {
        cli_abort("{.arg {arg}} must be TRUE or FALSE.", call = call)

    }

    invisible(x)
}

# assert data frame (returns input invisibly or errors)
.assert_data_frame <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
    if (missing(x)) {
        cli_abort("argument {.arg {arg}} is missing, with no default.", call = call)
    }

    if (!is.data.frame(x)) {
        cli_abort("{.arg {arg}} must be a data frame.", call = call)
    }

    invisible(x)
}

# assert class (returns input invisibly or errors)
.assert_class <- function(x, class, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
    if (!inherits(x, class)) {
        cli_abort("{.arg {arg}} must have class {.cls {class}}.", call = call)
    }

    invisible(x)
}

# function to use in default s3 methods with no implementation
.class_not_implemented <- function(x, call = rlang::caller_env()) {
    cls <- class(x)
    cli_abort("Not implemented for {.cls {cls}} objects.")
}


# return the version of an eq5d object
.get_version <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
    if (inherits(x, "EQ5D5L")) {
        "5L"
    } else if (inherits(x, "EQ5D3L")) {
        "3L"
    } else if (inherits(x, "EQ5DY")) {
        "Y"
    } else {
        cli_abort(
            "{.arg {arg}} must be of class {.cls EQ5D5L}, {.cls EQ5D3L} or {.cls EQ5DY}.",
            call = call
        )
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
        cli_abort("{.val {var}} is already a column in the input data frame.")
    }
    rownames(x) <- NULL
    setNames(cbind(rnms, x), c(var, nms))
}
