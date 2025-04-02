test_that("available valuesets matches eq5d output", {

    dat <- as_eq5d3l(
        eq5d3l_example,
        respondentID = "respondentID",
        surveyID = "surveyID",
        mobility = "MO",
        self_care = "SC",
        usual = "UA",
        pain = "PD",
        anxiety = "AD",
        vas = "vas"
    )
    expect_identical(
        available_valuesets(dat),
        tibble::as_tibble(eq5d::valuesets(version = "3L"))
    )
    expect_identical(available_valuesets(dat), available_valuesets("eq5d3l"))

    dat <- as_eq5dy3l(
        eq5d3l_example,
        respondentID = "respondentID",
        surveyID = "surveyID",
        mobility = "MO",
        self_care = "SC",
        usual = "UA",
        pain = "PD",
        anxiety = "AD",
        vas = "vas"
    )
    expect_identical(
        available_valuesets(dat),
        tibble::as_tibble(eq5d::valuesets(version = "Y3L"))
    )
    expect_identical(available_valuesets(dat), available_valuesets("eq5dy3l"))

    dat <- as_eq5d5l(
        EQ5D5L_surveys,
        respondentID = "respondentID",
        surveyID = "surveyID",
        time_index = "time_index",
        mobility = "mobility",
        self_care = "self_care",
        usual = "usual",
        pain = "pain",
        anxiety = "anxiety",
        vas = "vas"
    )

    expect_identical(
        available_valuesets(dat),
        tibble::as_tibble(eq5d::valuesets(version = "5L"))
    )
    expect_identical(available_valuesets(dat), available_valuesets("eq5d5l"))

    expect_identical(
        available_valuesets(),
        tibble::as_tibble(eq5d::valuesets())
    )

})

test_that("errors are correct", {
    expect_snapshot(available_valuesets(mean), error = TRUE)
    expect_snapshot(available_valuesets("bob"), error = TRUE)
})
