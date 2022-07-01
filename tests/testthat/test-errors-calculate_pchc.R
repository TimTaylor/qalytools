test_that("calculate_pchc errors correctly", {

    dat3 <- eq5d3l_example

    dat3 <- as_eq5d3l(
        dat3,
        respondentID = "respondentID",
        surveyID = "surveyID",
        time_index = "time",
        mobility = "MO",
        self_care = "SC",
        usual = "UA",
        pain = "PD",
        anxiety = "AD",
        vas = "vas",
        drop = FALSE
    )

    dat5 <- EQ5D5L_surveys
    dat5$surveyID <- as.factor(dat5$surveyID)
    dat5 <- as_eq5d5l(
        dat5,
        respondentID = "respondentID",
        surveyID = "surveyID",
        time_index = "time_index",
        mobility = "mobility",
        self_care = "self_care",
        usual = "usual",
        pain = "pain",
        anxiety = "anxiety",
        vas = "vas",
        drop = FALSE
    )

    expect_snapshot_error(calculate_pchc(mtcars, mtcars))
    expect_snapshot_error(calculate_pchc(dat3, mtcars))
    expect_snapshot_error(calculate_pchc(dat3, dat3, no.problems = 1))
    expect_snapshot_error(calculate_pchc(dat3, dat3, by.dimension = 1))
    expect_snapshot_error(calculate_pchc(dat3, dat5))
    expect_snapshot_error(calculate_pchc(dat5, dat5))
    expect_snapshot_error(calculate_pchc(subset(dat5, surveyID == 1), dat5))

})
