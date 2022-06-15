set.seed(32)
library(data.table)
library(eq5d)

# initially we generate for more individuals than we actually want as some
# observations will be thrown away at a later stage
ninit <- 10000
n <- 1000

# how many surveys
nsurveys <- 10

# ages we want to consider (inclusive)
lower_age <- 20
upper_age <- 80

# Assume beta distribution with varying alpha and fixed beta across surveys
male_survey_alpha <- c(8, 2, 3, 5, 7, 7.5, 7.8, 8, 8, 8)
female_survey_alpha <- c(8.5, 2, 3, 4, 5, 6, 7, 8, 8.5, 8.5)
beta <- 1

# for male alpha we add an age effect
male_alpha <- function(x, age, lower_age, upper_age) {
    x - 1.5 * (age - lower_age) / (upper_age - lower_age)
}

# function for generating an individual survey data
survey_data <- function(i) {
    DT <- data.table(
        survey = i,
        id =  seq_len(ninit),
        age = sample(lower_age:upper_age, size = ninit, replace = TRUE),
        sex = sample(c("Male", "Female"), size = ninit, replace = TRUE),
        beta = beta,
        alpha = female_survey_alpha[i],
        alpha2 = female_survey_alpha[i] # used for VAS
    )
    DT[sex == "Male", `:=`(alpha = male_alpha(male_survey_alpha[i], age, lower_age, upper_age),
                           alpha2 = male_survey_alpha[i])]
    DT[, `:=`(value = mapply(rbeta, shape1=alpha, shape2=beta, MoreArgs = list(n=1)),
              vas = mapply(rbeta, shape1=alpha2, shape2=beta, MoreArgs = list(n=1)))]
    DT
}

# generate all of the data and combine
dat <- lapply(seq_len(nsurveys), survey_data)
dat <- rbindlist(dat)

# Need to throw away values which do not fit the pattern we would like to create
keepid <- dat[, value[1]>=value[2] && value[3]>=value[2] && value[1] >= value[3], keyby=id][V1==TRUE, .(id)]
dat <- dat[keepid, on = "id"]

# Generate all possible utility values using the eq5d package
x=1:5
possible <- CJ(MO=x,SC=x,UA=x,PD=x,AD=x)
possible[,value:=eq5d(possible, "5L", "VT", "England")]

# match our synthetic data to get the nearest dimensions based on the utility value
out <- possible[dat, on = "value", roll = "nearest"]
out <- out[,.(surveyID=survey,respondentID=id,sex,age,mobility=MO,self_care=SC,usual=UA,pain=PD,anxiety=AD,time_index=30*survey, vas=vas)]

# pick n unique IDs
chosen <- out[order(respondentID), .(respondentID = unique(respondentID))][seq_len(n)]
EQ5D5L_surveys <- out[chosen, on="respondentID"]

# save output
setDF(EQ5D5L_surveys)
class(EQ5D5L_surveys) <- c("tbl", "data.frame")
write.csv(
    EQ5D5L_surveys,
    file = file.path("data-raw", "EQ5D5L_surveys.csv"),
    row.names = FALSE
)
usethis::use_data(EQ5D5L_surveys, overwrite = TRUE, version = 3)



# checking ----------------------------------------------------------------
library(surveyTools)
library(ggplot2)

# calculate utility
util <- add_utility(as_eq5d5l(EQ5D5L_surveys), type = "VT", country = "England")


# plot
ggplot(util, aes(x=surveyID, y = .value, group = respondentID)) +
    geom_line(aes(colour=sex), alpha = 0.1) +
    geom_smooth(aes(group=sex,colour=sex))






