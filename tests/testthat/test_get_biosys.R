context("biosys_get")

testthat::test_that("biosys_get won't work unauthenticated", {

    testthat::expect_error(biosys_get("projects", un="invalid", pw="invalid"))
})

biosys_api_online <- function(){
    ua <- httr::user_agent("http://github.com/parksandwildlife/biosystr")
    url <- "https://biosys.dbca.wa.gov.au/api/"
    res <- httr::GET(url, ua)
    res$status_code == 401
}

testthat::test_that("biosys_get returns something", {
    res <- biosys_get("", api="http://echo.jsontest.com/", query = list())
    testthat::expect_equal(res$response$status_code, 200)
    testthat::expect_s3_class(res, "biosys_api_response")
})

testthat::test_that("get_biosys fails if HTTP error is returned", {
    testthat::expect_error(
        biosys_get("", api="http://httpstat.us/401", query = list()))
    testthat::expect_error(
        biosys_get("", api="http://httpstat.us/500", query = list()))
    testthat::expect_error(
        biosys_get("", api="http://httpstat.us/404", query = list()))
})

testthat::test_that("get_biosys works with correct credentials", {

    # Credentials available?
    un <- Sys.getenv("BIOSYS_UN")
    pw <- Sys.getenv("BIOSYS_PW")

    if (un == "" || pw == "") skip(
        "Environment variables BIOSYS_UN or BIOSYS_PW not set, skipping...")

    # API accessible?
    if (!biosys_api_online()) skip(
        "BioSys API is not accessible from here, skipping...")

    res <- biosys_get("projects", un=un, pw=pw)

    testthat::expect_equal(res$response$status_code, 200)
})