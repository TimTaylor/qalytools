#' Calculate the Paretian Classification of Health Change
#'
#' @description
#'
#' `calculate_pchc()` calculates the Paretian Classification of Health Change
#' (PCHC) in an individuals health state between two surveys. It wraps the
#' `eq5d::pchc()` function providing methods for [`EQ5D`][new_eq5d] objects.
#'
#' @param pre An `EQ5D` object for a single survey.
#'
#' @param post An `EQ5D` object for a single survey.
#'
#' @inheritParams eq5d::pchc
#'
#' @return
#'
#'   - For `by.dimension = FALSE`:
#'     - A data frame with columns 'Change', 'Number' and 'Percent'.
#'   - For `by.dimension = TRUE`:
#'     - A data frame with columns 'Dimension', 'Change', 'Number' and 'Percent'.
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
#' grp1 <- subset(dat, Group == "Group1")
#' grp2 <- subset(dat, Group == "Group2")
#' calculate_pchc(grp1, grp2)
#' calculate_pchc(grp1, grp2, by.dimension = TRUE)
#'
#' @export
calculate_pchc <- function(pre, post, no.problems = TRUE, by.dimension = FALSE) {

    # check inputs
    pre <- .assert_class(pre, "EQ5D")
    post <- .assert_class(post, "EQ5D")
    no.problems <- .assert_bool(no.problems)
    by.dimension <- .assert_bool(by.dimension)

    # get eq5d version - also acts as an input check class of pre and post
    version_pre <- .get_version(pre)
    version_post <- .get_version(post)
    if (version_pre != version_post) {
        stop("`pre` and `post` must be of the same survey type")
    }

    # check each input is for only one pre-survey
    s_pre <- attr(pre, "surveyID")
    tmp <- unique(.subset2(pre, s_pre))
    if (length(tmp) > 1) {
        stop("`pre` must only contain one survey")
    }

    # check each input is for only one pre-survey
    s_post <- attr(post, "surveyID")
    tmp <- unique(.subset2(post, s_post))
    if (length(tmp) > 1) {
        stop("`post` must only contain one survey")
    }

    # pull out the relevant column names from pre and rename for use with pchc
    lookup <- .make_lookup(pre)
    class(pre) <- "data.frame" # avoids slow .eq5d_can_reconstruct checks
    pre <- pre[, names(lookup)]
    names(pre) <- lookup[names(pre)]

    # pull out the relevant column names from post and rename for use with pchc
    lookup <- .make_lookup(post)
    class(post) <- "data.frame" # avoids slow .eq5d_can_reconstruct checks
    post <- post[, names(lookup)]
    names(post) <- lookup[names(post)]

    res <- pchc(
        pre = pre,
        post = post,
        version = version_pre,
        no.problems = no.problems,
        by.dimension = by.dimension,
        totals = FALSE
    )

    if (by.dimension) {
        reverse_lookup <- setNames(names(lookup), c("MO", "SC", "UA", "PD", "AD"))
        names(res) <- reverse_lookup[names(res)]
        tmp <- lapply(
            seq_along(res),
            function(i) {
                cbind(.Dimension = names(res)[i], .rownames_2_column(res[[i]], "Change"))
            }
        )
        res <- do.call(rbind, tmp)
    } else {
        res <- .rownames_2_column(res, "Change")
    }
    class(res) <- c("tbl", class(res))
    res
}
