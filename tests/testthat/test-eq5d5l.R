test_that("as_eq5d5l works", {

    # setup
    dat <- transform(EQ5D5L_surveys, surveyID = as.factor(surveyID))
    out <- as_eq5d5l(
        dat,
        surveyID = "surveyID",
        respondentID = "respondentID",
        mobility = "mobility",
        self_care = "self_care",
        usual = "usual",
        pain = "pain",
        anxiety = "anxiety",
        vas = "vas"
    )

    # class is correct
    expect_s3_class(out, c("EQ5D5L", "EQ5D", "tbl_df", "tbl", "data.frame"), exact = TRUE)

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
          "pain", "anxiety", "vas", "time_index", "sex", "age", "dummy"),
        ignore.order = TRUE
    )
})

test_that("EQ5D5L maintain and drop class appropriately", {

    # setup
    dat <- transform(EQ5D5L_surveys, surveyID = as.factor(surveyID))
    out <- as_eq5d5l(
        dat,
        surveyID = "surveyID",
        respondentID = "respondentID",
        mobility = "mobility",
        self_care = "self_care",
        usual = "usual",
        pain = "pain",
        anxiety = "anxiety",
        vas = "vas"
    )

    # row selection maintains class
    expect_s3_class(out[1:10, ], "EQ5D5L")

    # class dropped/kept as expected
    tmp <- rbind(out[1:2, ], out[2:3, ])
    expect_false(inherits(tmp, "EQ5D"))
    tmp <- rbind(out[1:2, ], out[3:4, ])
    expect_s3_class(tmp, "EQ5D5L")
    expect_identical(tmp, out[1:4, ])

    # column selection may drop class if necessary columns are missing
    expect_s3_class(out[, -(3:4)], "EQ5D5L")
    expect_false(inherits(out[, 1:4], "EQ5D5L"))
    expect_false(inherits(out[, -9], "EQ5D5L"))
    expect_false(inherits(out[3, 3, drop = TRUE], "EQ5D5L"))

    # renaming maintains class and stores attributes
    tmp <- out
    tmp$sex <- NULL
    tmp$age <- NULL
    tmp$dummy <- NULL
    tmp$time_index <- NULL
    names(tmp) <- sprintf("nm%d", seq_along(tmp))
    expect_s3_class(tmp, "EQ5D5L")
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

test_that("Adding incorrect values to an EQ5D5L object will error", {

    # setup
    dat <- transform(EQ5D5L_surveys, surveyID = as.factor(surveyID))
    out <- as_eq5d5l(
        dat,
        surveyID = "surveyID",
        respondentID = "respondentID",
        mobility = "mobility",
        self_care = "self_care",
        usual = "usual",
        pain = "pain",
        anxiety = "anxiety",
        vas = "vas"
    )

    tmp <- out
    tmp[3, 5] <- 4.0
    expect_s3_class(out, "EQ5D5L")
    expect_error(tmp[3, 5] <- 4.5)
    expect_error(tmp[3, 5] <- "bob")
    expect_error(tmp[3, 5] <- 6L)
})



test_that("validation works", {
    dat <- EQ5D5L_surveys
    class(dat) <- "data.frame"

    expect_snapshot_error(
        out <- as_eq5d5l(
            dat,
            respondentID = "TEST",
            surveyID = "surveyID",
            mobility = "mobility",
            self_care = "self_care",
            usual = "usual",
            pain = "pain",
            anxiety = "anxiety",
            vas = "vas"
        )
    )

    tmp <- as_eq5d5l(
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

    expect_snapshot_error(
        out <- as_eq5d5l(
            dat,
            respondentID = "respondentID",
            surveyID = "surveyID",
            mobility = "mobility",
            self_care = "self_care",
            usual = "usual",
            pain = "TEST",
            anxiety = "anxiety",
            vas = "vas"
        )
    )

    tmp <- rbind(dat, dat)
    expect_snapshot_error(
        as_eq5d5l(
            tmp,
            respondentID = "respondentID",
            surveyID = "surveyID",
            mobility = "mobility",
            self_care = "self_care",
            usual = "usual",
            pain = "pain",
            anxiety = "anxiety",
            vas = "vas"
        )
    )

    tmp <- dat
    tmp$pain <- tmp$pain + 0.5
    expect_snapshot_error(
        as_eq5d5l(
            tmp,
            respondentID = "respondentID",
            surveyID = "surveyID",
            mobility = "mobility",
            self_care = "self_care",
            usual = "usual",
            pain = "pain",
            anxiety = "anxiety",
            vas = "vas"
        )
    )

})
