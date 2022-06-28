test_that("validate_eq5d produces the expected errors", {
    dat <- EQ5D5L_surveys
    dat$surveyID <- factor(dat$surveyID)
    dat2 <- rbind(dat, dat)
    dat <- new_eq5d5l(
        dat,
        respondentID = "respondentID",
        surveyID = "surveyID",
        mobility = "mobility",
        self_care = "self_care",
        usual = "usual",
        pain = "pain",
        anxiety = "anxiety",
        vas = "vas",
        time_index = "time_index"
    )

    broken <- new_eq5d5l(
        dat2,
        respondentID = "respondentID",
        surveyID = "surveyID",
        mobility = "mobility",
        self_care = "self_care",
        usual = "usual",
        pain = "pain",
        anxiety = "anxiety",
        vas = "vas",
        time_index = "time_index"
    )

    expect_snapshot_error(validate_eq5d5l(broken))

    broken <- dat
    attr(broken, "surveyID") <- "error"
    expect_snapshot_error(validate_eq5d5l(broken))

    broken <- dat
    broken$surveyID <- as.character(broken$surveyID)
    expect_snapshot_error(validate_eq5d5l(broken))

    broken <- dat
    broken$pain <- NULL
    expect_snapshot_error(validate_eq5d5l(broken))




})
