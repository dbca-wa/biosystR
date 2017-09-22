#' Map given function, handle null as NA_real_ and flatten_dbl()
#'
#' @details Use this function to extract elements with NAs from a list of lists
#' into a tibble.
#'
#' @param .x An interable data object
#' @param .f A function to map over the data
#' @param ... Extra arguments to `map()`
#' @author Jennifer Bryan https://github.com/jennybc/
#' @importFrom magrittr %>%
#' @export
#' @examples
#' #data(animal_encounters)
#' #nn <- map_chr_hack(animal_encounters$content, c("properties", "name"))
#' #testthat::expect_true(is.na(nn[[1]]))
#' #testthat::expect_equal(nn[[2]], animal_encounters$content[[2]]$properties$name)
map_dbl_hack <- function(.x, .f, ...) {
    . <- ""
    purrr::map(.x, .f, ...) %>%
        purrr::map_if(is.null, ~ NA_real_) %>%
        purrr::flatten_dbl(.)
}