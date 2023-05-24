require(testthat)
require(EdSurvey)
skip_if_not_installed("dplyr")

context("dplyr methods") 

if(!exists("edsurveyHome")) {
  if (Sys.info()[['sysname']] == "Windows") {
    edsurveyHome <- "C:/EdSurveyData/"
  } else {
    edsurveyHome <- "~/EdSurveyData/"
  }
}

if(!exists("forceCacheUpdate")){
  forceCacheUpdate <- FALSE
}

if (!dir.exists(edsurveyHome)) {
  dir.create(edsurveyHome)
}

context("dplyr methods") 

downloadTIMSS(root=edsurveyHome, year=2019, verbose = FALSE)

fin8.19 <- readTIMSS(path=file.path(edsurveyHome, "TIMSS", "2019"),
                         countries = "fin", gradeLvl = 8, verbose=FALSE)

suppressMessages(attach(fin8.19))


test_that("distinct",{
  fin8.19 %>%
    distinct(idstud) %>%
    nrow() %>%
    expect_equal(4874)
  
})

test_that("select",{
  fin8.19 %>%
    select(ends_with("_f")) %>%
    ncol() %>%
    expect_equal(397)
  
})










