#' Calculate utility
#'
#' @description
#'
#' Generic function that calculates EQ5D index scores for given value sets.
#'
#' @details
#'
#' * `calculate_utility()` returns the utility index scores for a given object
#'   and value set combination. Methods that wrap functionality of the
#'   [eq5d](https://cran.r-project.org/package=eq5d) package are provided for
#'   EQ5D objects.
#'
#' * `add_utility()` is a wrapper around `calculate_utility()` which keeps all
#'   columns of the input data `x`.
#'
#' @param x An \R object.
#'
#' @param type `[character]` Method type(s) used for calculating the value sets.
#'
#' For EQ5D3L inputs this can be:
#'
#' - "TTO", the time trade-off valuation technique;
#' - "VAS", the visual analogue scale valuation technique;
#' - "RCW", a reverse crosswalk conversion to EQ5D5L values; or
#' - "DSU", the NICE Decision Support Unit's model that allows mappings on to
#'          EQ5D5L values accounting for both age and sex.
#'
#' For EQ5D5L inputs this can be:
#'
#' - "VT",  value sets generated via a EuroQol standardised valuation study
#'          protocol;
#' - "CW",  a crosswalk conversion EQ5D3L values; or
#' - "DSU", the NICE Decision Support Unit's model that allows mappings on to
#'          EQ5D5L values accounting for both age and sex.
#'
#' @param country `[character]` Value set countries to use.
#'
#' @param drop `[logical]` If TRUE (default), only columns corresponding to the
#' surveyID, respondentID and time_index are kept from the input `x`.
#'
#' @param age `[character]` Column in `x` representing the age, in years, of the
#' respondent. Only used if `type = "DSU"`.
#'
#' @param sex `[character]` Column in `x` representing the sex, in years, of the
#' respondent. Only used if `type = "DSU"`. Column entries must be one of
#' "Male", "M", "Female" or "F" (case insensitive).
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @note
#'
#' The methods for [`eq5d`][new_eq5d] objects expect `type` and `country`
#' to be of the same length but will recycle those of length one to a common
#' size.
#'
#' @return A data frame of utility values linked to responses.
#'
#' @seealso
#'
#' `available_valuesets()` for the method / country combinations available for
#' each survey type.
#'
#' @examples
#'
#' data("eq5d3l_example")
#' dat <- as_eq5d3l(
#'     eq5d3l_example,
#'     respondentID = "respondentID",
#'     surveyID = "surveyID",
#'     mobility = "MO",
#'     self_care = "SC",
#'     usual = "UA",
#'     pain = "PD",
#'     anxiety = "AD",
#'     vas = "vas",
#'     drop = FALSE
#' )
#' calculate_utility(dat, type = "TTO", country = c("UK", "Germany"))
#'
#' @export
calculate_utility <- function(x, type, country, ...) {
    UseMethod("calculate_utility")
}

# -------------------------------------------------------------------------
#' @rdname calculate_utility
#' @export
calculate_utility.default <- function(x, type, country, ...) {
    .class_not_implemented(x)
}


# -------------------------------------------------------------------------
#' @rdname calculate_utility
#' @export
calculate_utility.EQ5D5L <- function(
    x,
    type = "VT",
    country = "England",
    drop = TRUE,
    age = NULL,
    sex = NULL,
    ...
) {
    .calculate_utility(
        x = x,
        type = type, country = country,
        version = "5L",
        drop = drop,
        age = age,
        sex = sex
    )
}

# -------------------------------------------------------------------------
#' @rdname calculate_utility
#' @export
calculate_utility.EQ5D3L <- function(
    x,
    type = "VT",
    country = "England",
    drop = TRUE,
    age = NULL,
    sex = NULL,
    ...
) {
    .calculate_utility(
        x = x,
        type = type,
        country = country,
        version = "3L",
        drop = drop,
        age = age,
        sex = sex
    )
}

# -------------------------------------------------------------------------
#' @rdname calculate_utility
#' @export
calculate_utility.EQ5DY <- function(
    x,
    type = "VT",
    country = "England",
    drop = TRUE,
    age = NULL,
    sex = NULL,
    ...
) {
    .calculate_utility(
        x = x,
        type = type,
        country = country,
        version = "Y",
        drop = drop,
        age = age,
        sex = sex
    )
}

# -------------------------------------------------------------------------
#' @rdname calculate_utility
#' @export
add_utility <- function(x, type, country, ...) {
    UseMethod("add_utility")
}

# -------------------------------------------------------------------------
#' @rdname calculate_utility
#' @export
add_utility.default <- function(x, type, country, ...) {
    .class_not_implemented(x)
}

# -------------------------------------------------------------------------
#' @rdname calculate_utility
#' @export
add_utility.EQ5D5L <- function(
    x,
    type = "VT",
    country = "England",
    age = NULL,
    sex = NULL,
    ...
) {
    calculate_utility.EQ5D5L(
        x = x,
        type = type,
        country = country,
        drop = FALSE,
        age = age,
        sex = sex
    )
}

# -------------------------------------------------------------------------
#' @rdname calculate_utility
#' @export
add_utility.EQ5D3L <- function(
    x,
    type = "VT",
    country = "England",
    age = NULL,
    sex = NULL,
    ...
) {
    calculate_utility.EQ5D3L(
        x = x,
        type = type,
        country = country,
        drop = FALSE,
        age = age,
        sex = sex
    )
}

# -------------------------------------------------------------------------
#' @rdname calculate_utility
#' @export
add_utility.EQ5DY <- function(
    x,
    type = "VT",
    country = "England",
    age = NULL,
    sex = NULL,
    ...
) {
    calculate_utility.EQ5DY(
        x = x,
        type = type,
        country = country,
        drop = FALSE,
        age = age,
        sex = sex
    )
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
.calculate_utility <- function(x, type, country, version, drop, age, sex) {
    age_dat <- x[age]
    sex_dat <- x[sex]

    # check input types
    type <- .assert_character(type)
    stopifnot(is.character(type), is.character(country))

    # Recycle type and country inputs and check against available value sets
    if (length(type) == 1L && length(country) > 1L) {
        type <- rep_len(type, length(country))
    } else if (length(country) == 1L && length(type) > 1L) {
        country <- rep_len(type, length(type))
    } else if (length(type) != length(country)) {
        stop("lengths of `type` and `country` are not compatible.")
    } else if (!length(type) || !length(country)) {
        stop("`type` and `country` must have length greater than 0.")
    }

    # create data frame of combinations
    combos <- list2DF(list(type = type, country = country))

    # check combinations are valid
    valuesets <- available_valuesets(x)[c("Type", "Country")]
    combo_strings <- do.call(paste, c(combos, sep = "_"))
    valueset_strings <- do.call(paste, c(valuesets, sep = "_"))
    tmp <- combos[!combo_strings %in% valueset_strings, ]
    if (nrow(tmp)) {
        stop(
            "Invalid value set and country combination:\n",
            sprintf(" - Type = %s, Country = %s\n", tmp$type[1], tmp$country[1])
        )
    }

    # pull out and replicate respondents/surveyId for each combination
    resp <- attr(x, "respondentID")
    surv <- attr(x, "surveyID")

    r <- .subset2(x, resp)
    s <- .subset2(x, surv)

    r <- r[rep.int(seq_along(r), nrow(combos))]
    s <- s[rep.int(seq_along(s), nrow(combos))]

    # match names required for the eq5d function
    class(x) <- "data.frame"
    original <- x
    lookup <- .make_lookup(x)
    x <- x[, c(names(lookup), sex, age)]
    names(x) <- c(lookup[names(x)[1:5]], sex, age)

    # calculate values for each combination
    tmp <- .mapply(
        .eq5d,
        dots = list(country = combos$country, type = combos$type),
        MoreArgs = list(scores = x, version = version, age = age, sex = sex)
    )

    # combine with respondent and survey IDs
    tmp <- cbind(r, s, rbindlist(tmp))
    setnames(tmp, old = 1:3, new = c(resp, surv))
    setDF(tmp)

    if (isFALSE(drop)) {
        tmp <- merge(tmp, original, by = c(resp, surv), all.x = TRUE)
    }

    # construct utility and return
    new_utility(
        x = tmp,
        respondentID = resp,
        surveyID = surv,
        country = ".utility_country",
        type = ".utility_type",
        value = ".value"
    )
}


.eq5d <- function(country, type, scores, version, age, sex) {
    if (type == "DSU") {

        # check character vectors of length 1
        stopifnot(
            "For 'DSU' you must specify the `age` variable." = !is.null(age),
            "For 'DSU' you must specify the `sex` variable." = !is.null(sex)
        )

        age <- .assert_scalar_character(age)
        sex <- .assert_scalar_character(sex)

        # ensure present in data frame
        nms <- names(scores)
        vars <- c(age = age, sex = sex)
        for (i in seq_along(vars)) {
            v <- vars[i]
            if (!v %in% nms) {
                stop(
                    sprintf("`%s` variable (%s) not present in `x`", names(v), sQuote(v))
                )
            }
        }

        # check valid values
        ages <- .subset2(scores, age)
        if (!is.numeric(ages)) {
            stop("`age` variable in `x` should be a numeric vector")
        }
        if (length(which(ages < 18 | ages > 100))) {
            warning("`DSU` can only applied for ages in the range 18-100. Returning NA where this does not hold.")
        }

        # check valid values
        sexes <- .subset2(scores, sex)
        if (!is.character(sexes)) {
            stop("`sex` variable in `x` should be a character vector")
        }
        sexes <- tolower(sexes)
        if (any(!sexes %in% c("male", "m", "female", "f", NA_character_))) {
            stop('`sex` variable entries should be one of "Male", "M", "Female" or "F" (case independent)')
        }
    }

    out <- eq5d(
        scores = scores,
        version = version,
        type = type,
        country = country,
        ignore.invalid = TRUE,
        age = age,
        sex = sex
    )
    country <- rep_len(country, length(out))
    type <- rep_len(type, length(out))

    list2DF(list(.utility_country = country, .utility_type = type, .value = out))
}
