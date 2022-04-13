# is character of length 1
.is_scalar_character <- function(x) {
    is.character(x) && length(x) == 1
}

# assert character is length 1 (returns input invisibly or errors)
.assert_scalar_chr <- function(x, label = deparse(substitute(x))) {
    if (!(is.character(x) && length(x) == 1)) {
        stop(sprintf("`%s` must be a character vector of length 1", label), call. = FALSE)
    }
    invisible(x)
}

# is bool (TRUE/FALSE)
.is_bool <- function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x)
}

# check if entries of a vector are whole numbers
.is_whole <- function(x, tol = .Machine$double.eps^0.5) {
    abs(x - round(x)) < tol
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
