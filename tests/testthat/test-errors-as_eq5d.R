test_that("as_eq5d errors correctly", {
    dat <- EQ5D5L_surveys
    names(dat)[which(names(dat) == "time_index")] <- ".time_index"
    expect_snapshot_error(
        as_eq5d5l(
            dat,
            respondentID = "respondentID",
            surveyID = "surveyID",
            mobility = "mobility",
            self_care = "self_care",
            usual = "usual",
            pain = "pain",
            anxiety = "anxiety",
            vas = "vas",
            drop = FALSE
        )
    )

    dat$surveyID <- as.character(dat$surveyID)
    expect_snapshot_warning(
        as_eq5d5l(
            dat,
            respondentID = "respondentID",
            surveyID = "surveyID",
            mobility = "mobility",
            self_care = "self_care",
            usual = "usual",
            pain = "pain",
            anxiety = "anxiety",
            time_index = ".time_index",
            vas = "vas",
            drop = FALSE
        )
    )


})
