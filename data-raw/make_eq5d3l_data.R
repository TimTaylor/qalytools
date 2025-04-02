# library(readxl)
# library(eq5d)
# library(usethis)

f <- tempfile()
download.file(
    "https://github.com/fragla/eq5d/raw/master/inst/extdata/eq5d3l_example.xlsx",
    destfile = f
)

dat <- readxl::read_xlsx(f)
dat$surveyID <- factor("survey01")
dat$respondentID <- 1:nrow(dat)
score <- eq5d::eq5d(dat,version = "3L", type = "TTO", country="UK")
dat$vas <- abs(score + rnorm(length(score), sd = 0.1))
dat$vas[dat$vas > 1] <- 1
dat$vas <- round(dat$vas * 100)

dat$time <- 0
eq5d3l_example <- tibble::as_tibble(dat)

usethis::use_data(eq5d3l_example, overwrite = TRUE, version = 3)
