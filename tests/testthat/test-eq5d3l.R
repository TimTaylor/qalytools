test_that("as_eq5d3l works", {

    # setup
    dat <- data.table::setnames(
        data.table::copy(eq5d3l_example),
        old = c("MO", "SC", "UA", "PD", "AD", "time"),
        new = c("mobility", "self_care", "usual", "pain", "anxiety", "time_index")
    )

    out <- as_eq5d3l(
        dat,
        respondentID = "respondentID",
        surveyID = "surveyID",
        mobility = "mobility",
        self_care = "self_care",
        usual = "usual",
        pain = "pain",
        anxiety = "anxiety",
        vas = "vas"
    )

    # class is correct
    expect_s3_class(out, c("EQ5D3L", "EQ5D", "tbl_df", "tbl", "data.frame"), exact = TRUE)

    # attributes are as expected
    expect_identical(attr(out, "surveyID"), "surveyID")
    expect_identical(attr(out, "respondentID"), "respondentID")
    expect_identical(attr(out, "mobility"), "mobility")
    expect_identical(attr(out, "self_care"), "self_care")
    expect_identical(attr(out, "usual"), "usual")
    expect_identical(attr(out, "pain"), "pain")
    expect_identical(attr(out, "anxiety"), "anxiety")
    expect_identical(attr(out, "vas"), "vas")
    expect_identical(ncol(out), ncol(dat))

    # Same number of rows as input
    expect_identical(nrow(dat), nrow(out))

    # expect column names
    expect_named(
        out,
        c("surveyID", "respondentID", "mobility", "self_care", "usual",
          "pain", "anxiety", "vas", "time_index", "Group"),
        ignore.order = TRUE
    )

})

test_that("EQ5D3L maintains class and names appropriately", {

    # setup
    dat <- data.table::setnames(
        data.table::copy(eq5d3l_example),
        old = c("MO", "SC", "UA", "PD", "AD", "time"),
        new = c("mobility", "self_care", "usual", "pain", "anxiety", "time_index")
    )

    out <- as_eq5d3l(
        dat,
        respondentID = "respondentID",
        surveyID = "surveyID",
        mobility = "mobility",
        self_care = "self_care",
        usual = "usual",
        pain = "pain",
        anxiety = "anxiety",
        vas = "vas"
    )

    # row selection maintains class
    expect_s3_class(out[1:10, ], "EQ5D3L")

    # class dropped/kept as expected
    tmp <- rbind(out[1:2, ], out[2:3, ])
    expect_false(inherits(tmp, "EQ5D"))
    tmp <- rbind(out[1:2, ], out[3:4, ])
    expect_s3_class(tmp, "EQ5D3L")
    expect_identical(tmp, out[1:4, ])

    # column selection may drop class if necessary columns are missing
    expect_s3_class(out[, -6], "EQ5D3L")
    expect_false(inherits(out[, 1:4], "EQ5D3L"))
    expect_false(inherits(out[, -9], "EQ5D3L"))
    expect_false(inherits(out[3, 3, drop = TRUE], "EQ5D3L"))

    # renaming maintains class and stores attributes
    tmp <- out
    tmp$Group <- tmp$time_index <- NULL
    names(tmp) <- sprintf("nm%d", seq_along(tmp))
    expect_s3_class(tmp, "EQ5D3L")
    expect_named(
        tmp,
        c(attr(tmp, "surveyID"),
          attr(tmp, "respondentID"),
          attr(tmp, "mobility"),
          attr(tmp, "self_care"),
          attr(tmp, "usual"),
          attr(tmp, "pain"),
          attr(tmp, "anxiety"),
          attr(tmp, "vas")),
        ignore.order = TRUE
    )

})

test_that("Adding incorrect values to an EQ5D3L object will error", {

    # setup
    dat <- data.table::setnames(
        data.table::copy(eq5d3l_example),
        old = c("MO", "SC", "UA", "PD", "AD", "time"),
        new = c("mobility", "self_care", "usual", "pain", "anxiety", "time_index")
    )

    out <- as_eq5d3l(
        dat,
        respondentID = "respondentID",
        surveyID = "surveyID",
        mobility = "mobility",
        self_care = "self_care",
        usual = "usual",
        pain = "pain",
        anxiety = "anxiety",
        vas = "vas"
    )

    out[3, 3] <- 3.0
    expect_error(out[3, 4] <- 4.5)
    expect_error(out[3, 4] <- "bob")
    expect_error(out[3, 4] <- 6L)
})
