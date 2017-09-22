#' Return JSON from a BioSys API endpoint
#'
#' @description Call the WAStD API serializer's list view with given GET parameters,
#'   parse the response as text into a GeoJSON FeatureCollection.
#'   Parse the FeatureCollection using jsonlite::fromJSON and return its features
#'   as nested data.frame (simplify=TRUE) or as list of lists (simplify=FALSE).
#'   TODO: use pagination, see
#'   \url{https://cran.r-project.org/web/packages/jsonlite/vignettes/json-paging.html}
#' @param serializer (character) WAStD API serializer name (required).
#'   Possible values as per
#'   \code{https://biosys.dbca.wa.gov.au/api/} are:
#'   \itemize{
#'   \item projects
#'   \item datasets
#'   \item records
#'   \item sites
#'   }
#' @param query (list) API query parameters for format, limit, filtering
#'   (default: list(taxon='Cheloniidae', limit=10000, format='json'))
#' @param api (character) The BioSys API URL,
#'   default https://biosys.dbca.wa.gov.au/api/
#' @param un (character) The BioSys username, default \code{Sys.getenv("BIOSYS_UN")}
#' @param pw (character) The BioSys password, default \code{Sys.getenv("BIOSYS_PW")}
#' @importFrom httr add_headers http_error http_type status_code user_agent
#' @return An S3 object of class 'wastd_api_response' containing:
#'
#'   content: The retrieved JSON as list of lists
#'
#'   serializer: The called serializer, e.g. 'projects'
#'
#'   response: The API HTTP response with all metadata
#' @export
biosys_get <- function(serializer,
                       api="https://biosys.dbca.wa.gov.au/api/",
                       un=Sys.getenv("BIOSYS_UN"),
                       pw=Sys.getenv("BIOSYS_PW"),
                       query = list()){

    ua <- httr::user_agent("http://github.com/parksandwildlife/biosystr")
    url <- file.path(api, serializer)
    auth <- httr::authenticate(un, pw)

    res <- httr::GET(url, ua, auth, query = query)

    if (res$status_code == 401) {
        stop(paste(
            "Authorization failed.\n",
            "Either supply BioSys username and password as arguments un and pw,\n",
            "or set your BioSys username and password as system variables with",
            "Sys.setenv(BIOSYS_UN=\"usr\") and Sys.setenv(BIOSYS_PW=\"passwd\")."),
            call. = FALSE)
    }

    if (res$status_code > 401) {
        stop("Error - this request had some problem.",
             call. = FALSE)
    }


    if (httr::http_type(res) != "application/json") {
        stop(paste("API did not return JSON.\nIs", url, "a valid endpoint?"),
             call. = FALSE)
    }

    out <- httr::content(res, encoding = "UTF-8")

    if (identical(out, "")) {
        stop("The response did not return any content.", call. = FALSE)
    }

    structure(
        list(
            content = out,
            serializer = serializer,
            response = res
        ),
        class = "biosys_api_response"
    )
}

#' @title S3 print method for 'biosys_api_response'.
#' @description Prints a short representation of data returned by \
#'   code{\link{get_biosys}}.
#' @param x An object of class `biosys_api_response` as returned by
#'   \code{\link{get_biosys}}.
#' @param ... Extra parameters for `print`
#' @importFrom utils str
#' @export
print.biosys_api_response <- function(x, ...) {
    cat("<BioSys API endpoint", x$serializer, ">\n",
        "Retrieved on ", x$response$headers$date, ">\n", sep = "")
    utils::str(x$content)
    invisible(x)
}