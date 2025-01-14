test_that("validate_eq5d works", {
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
    tmp$pain <- tmp$pain + .5
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

    expect_snapshot_error(validate_eq5d(mtcars))

    tmp <- dat
    tmp$pain <- tmp$pain + 5
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
