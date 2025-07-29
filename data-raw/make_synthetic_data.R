set.seed(32)

# -------------------------------------------------------------------------
# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

library(data.table)
library(eq5d)

# -------------------------------------------------------------------------
# constants ---------------------------------------------------------------
# -------------------------------------------------------------------------

# initially we generate for more individuals than we actually want as some
# observations will be thrown away at a later stage
ninit <- 10000
n <- 1000

# how many surveys
nsurveys <- 10

# ages we want to consider (inclusive)
lower_age <- 20
upper_age <- 80

# misc
male_survey_mu   <- c(1.0, 0.2, 0.85, 0.9, 0.9, rep(0.99, 5))
female_survey_mu <- c(1.0, 0.3, 0.75, 0.8, 0.9, rep(0.99, 5))
survey_phi       <- c(100, 10, 70, rep(100, 7))

# safety check
pars <- list(male_survey_mu, female_survey_mu, survey_phi)
stopifnot(length(check <- unique(lengths(pars))) == 1L)
stopifnot(check == nsurveys)

# -------------------------------------------------------------------------
# helper functions --------------------------------------------------------
# -------------------------------------------------------------------------

# Alternative parameterisation of beta distribution
rbeta_mu <- function(n, mu, phi) {
    rbeta(n, mu * phi, (1 - mu) * phi)
}

# for male means we add an age effect to base age
male_mu <- function(x, age, lower_age, upper_age) {
    x - 0.25 * (age - lower_age) / (upper_age - lower_age)
}


# -------------------------------------------------------------------------
# generate data -----------------------------------------------------------
# -------------------------------------------------------------------------

# Participants
dat <- data.table(
  id = seq_len(ninit),
  age = sample(lower_age:upper_age, size = ninit, replace = TRUE),
  sex = sample(c("Male", "Female"), size = ninit, replace = TRUE)
)

# Maximum values, used as the base for each participant
dat[, mu := ifelse(sex == "Male", male_mu(0.85, age, lower_age, upper_age), 0.85)]
dat[, max_value := rbeta_mu(ninit, mu, 25)]

survey_data <- lapply(
    seq_len(nsurveys),
    function(i) {
        tmp <- fifelse(
            dat$sex == "Male",
            rbeta_mu(ninit, male_survey_mu[i], survey_phi[i]),
            rbeta_mu(ninit, female_survey_mu[i], survey_phi[i])
        )
        copy(dat)[, `:=`(value = max_value * tmp, survey = i)]
    }
)
survey_data <- rbindlist(survey_data)
dat <- survey_data[,.(survey, id, age, sex, value)]
dat[, vas := value * rnorm(ninit, 1, 0.05)]
dat[, vas := pmax(pmin(vas, 1), 0)]


# Generate all possible utility values using the eq5d package
x=1:5
possible <- CJ(MO=x,SC=x,UA=x,PD=x,AD=x)
possible[, value := eq5d::eq5d(possible, "5L", "VT", "England")]
setorder(possible, value)

# match our synthetic data to get the nearest dimensions based on the utility value
out <- possible[dat, on = "value", roll = -Inf]
tmp <- out[out[, .I[which.max(value)],by=id]$V1]
tmp[, survey:=1]
out <- out[survey!=1]
out <- rbind(tmp,out)
out <- out[,.(surveyID=survey,respondentID=id,sex,age,mobility=MO,self_care=SC,usual=UA,pain=PD,anxiety=AD,time_index=30*survey, vas=vas)]

# pick n unique IDs
chosen <- out[order(respondentID), .(respondentID = unique(respondentID))][seq_len(n)]
EQ5D5L_surveys <- out[chosen, on="respondentID"]

# Ensure vas is integer between 0 and 100
EQ5D5L_surveys[, vas := round(vas * 100)]

# make surveyID nicer for examples
EQ5D5L_surveys[, surveyID := sprintf("survey%02d", surveyID)]

# Add a dummy variable
EQ5D5L_surveys[, dummy:=sample(c(TRUE, FALSE), size = .N, replace=TRUE)]

# save output
setDF(EQ5D5L_surveys)

EQ5D5L_surveys <- tibble::as_tibble(EQ5D5L_surveys)

write.csv(
    EQ5D5L_surveys,
    file = file.path("data-raw", "EQ5D5L_surveys.csv"),
    row.names = FALSE
)
usethis::use_data(EQ5D5L_surveys, overwrite = TRUE, version = 3)

# checking ----------------------------------------------------------------
library(qalytools)
library(ggplot2)

# calculate utility
util <- add_utility(
    as_eq5d5l(
        EQ5D5L_surveys,
        surveyID = "surveyID",
        respondentID = "respondentID",
        mobility = "mobility",
        self_care = "self_care",
        usual = "usual",
        pain = "pain",
        anxiety = "anxiety",
        time_index = "time_index",
        vas = "vas"),
    type = "VT",
    country = "England"
)


# plot
ggplot(util, aes(x=surveyID, y = .value, group = respondentID)) +
    geom_line(aes(colour=sex), alpha = 0.1) +
    geom_smooth(aes(group=sex,colour=sex))
