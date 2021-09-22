#' Create API R packages
#' @param url a web url
#' @param github_api a S3 class
#' @importFrom jsonlite fromJSON
#' @export

github_api <- function(url) {
  #url <- modify_url("https://api.github.com", path = path)

  resp <- GET(url)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)

  if (http_error(resp)) {
    stop(
      sprintf(
        "GitHub API request failed [%s]\n%s\n<%s>",
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "github_api"
  )
}
