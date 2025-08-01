test_that("calculate_utility works as expected for EQ5D3L", {

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

    # matches eq5d direct calculation
    tmp <- calculate_utility(out, type = "TTO", country = c("Germany", "France"))
    dat2 <- subset(dat, select = c(mobility, self_care, usual, pain, anxiety))
    names(dat2) <- c("MO", "SC", "UA", "PD", "AD")
    gutil <- eq5d::eq5d(dat2, version = "3L", type = "TTO", country = "Germany")
    futil <- eq5d::eq5d(dat2, version = "3L", type = "TTO", country = "France")
    tmp2 <- data.frame(
        respondentID = dat[["respondentID"]],
        .utility_country = c(
            rep_len("Germany", length(dat2[[1]])),
            rep_len("France", length(dat2[[1]]))
        ),
        .value = c(gutil, futil)
    )
    expect_identical(
        subset(tmp, select = c(respondentID, .utility_country, .value)),
        tmp2
    )

    # correct class
    expect_s3_class(tmp, "utility")

    # correct columns
    expect_named(
        tmp,
        c("respondentID", "surveyID", ".utility_country", ".utility_type", ".value")
    )
})

test_that("calculate_utility works as expected for EQ5D5L (non-DSU type)", {

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

    # matches eq5d direct calculation
    dat <- subset(out, surveyID == "survey01")
    tmp <- calculate_utility(dat, type = "VT", country = c("Germany", "France"))

    # correct class
    expect_s3_class(tmp, "utility")

    dat2 <- subset(dat, select = c(mobility, self_care, usual, pain, anxiety))
    names(dat2) <- c("MO", "SC", "UA", "PD", "AD")
    gutil <- eq5d::eq5d(dat2, version = "5L", type = "VT", country = "Germany")
    futil <- eq5d::eq5d(dat2, version = "5L", type = "VT", country = "France")
    tmp2 <- data.frame(
        respondentID = dat[[2]],
        .utility_country = c(
            rep_len("Germany", length(dat2[[1]])),
            rep_len("France", length(dat2[[1]]))
        ),
        .value = c(gutil, futil)
    )
    tmp2 <- tmp2[do.call(order, .subset(tmp2)), ]
    tmp <- tmp[do.call(order, .subset(tmp)), ]
    rownames(tmp2) <- NULL
    expect_identical(
        subset(tmp, select = c(respondentID, .utility_country, .value)),
        tmp2
    )

    # correct columns
    expect_named(
        tmp,
        c("respondentID", "surveyID", ".utility_country", ".utility_type", ".value")
    )
})

test_that("calculate_utility works as expected for EQ5D5L (DSU type)", {

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

    # matches eq5d direct calculation
    dat <- subset(out, surveyID == "survey01")
    tmp <- calculate_utility(dat, type = "DSU", country = "UK", age = "age", sex = "sex")

    # correct class
    expect_s3_class(tmp, "utility")

    dat2 <- subset(dat, select = c(mobility, self_care, usual, pain, anxiety, sex, age))
    names(dat2) <- c("MO", "SC", "UA", "PD", "AD", "sex", "age")
    uutil <- eq5d::eq5d(dat2, version = "5L", type = "DSU", country = "UK", age = "age", sex = "sex") # nolint: line_length_linter.

    tmp2 <- data.frame(
        respondentID = dat[[2]],
        .utility_country = c(rep_len("UK", length(dat2[[1]]))),
        .value = uutil
    )
    tmp2 <- tmp2[do.call(order, .subset(tmp2)), ]
    tmp <- tmp[do.call(order, .subset(tmp)), ]
    row.names(tmp2) <- NULL
    expect_identical(
        .subset(tmp, c("respondentID", ".utility_country", ".value")),
        .subset(tmp2, c("respondentID", ".utility_country", ".value"))
    )

    # correct columns
    expect_named(
        tmp,
        c("respondentID", "surveyID", ".utility_country", ".utility_type", ".value")
    )

})

test_that("calculate_utility errors correctly", {
    EQ5D5L_surveys$surveyID <- as.factor(EQ5D5L_surveys$surveyID)
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
        vas = "vas",
        drop = FALSE
    )

    expect_snapshot_error(
        calculate_utility(mtcars, type = "VT", country = "Denmark")
    )

    expect_snapshot_error(
        calculate_utility(dat, type = character(), country = character())
    )

    expect_snapshot_error(
        calculate_utility(dat, type = c("VT", "TTO"), country = c("England", "France", "denmark"))
    )

    expect_snapshot_error(
        calculate_utility(dat, type = "DSU", country = "UK", age = "age")
    )

    expect_snapshot_error(
        calculate_utility(dat, type = "DSU", country = "UK", sex = "sex")
    )

    expect_snapshot_error(
        calculate_utility(dat, type = "DSU", country = "UK", age = "age", sex = "pain")
    )

    expect_snapshot_error(
        calculate_utility(dat, type = "DSU", country = "UK", age = "sex", sex = "sex")
    )

    expect_snapshot_error(
        calculate_utility(dat, type = c("VT", "CW"), country = "England")
    )

    tmp <- subset(dat, surveyID == "survey01")
    tmp$age[1L] <- 1
    expect_snapshot_warning(
        calculate_utility(tmp, type = "DSU", country = "UK", age = "age", sex = "sex")
    )

    tmp <- subset(dat, surveyID == "survey01")
    tmp$sex[1L] <- "bob"
    expect_snapshot_error(
        calculate_utility(tmp, type = "DSU", country = "UK", age = "age", sex = "sex")
    )


})

