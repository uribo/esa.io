#' esa.io v1 wrapper function
#' @param verb character
#' @param path character
#' @param body list
#' @import httr
esaio_api <- function(verb = c("GET", "POST", "DELETE", "PATCH", "PUT"), path = NULL, body = NULL) {
  res <- httr::VERB(
    verb,
    url = "https://api.esa.io",
    path = path,
    config = httr::add_headers(
      `Authorization` = paste("Bearer", Sys.getenv("ESA_IO_TOKEN")),
      `Content-Type`  = "application/json"
    ),
    body = body,
    encode = "json"
  )

  result <-
    httr::content(res, encoding = "UTF-8", type = "application/json")

  if (is.null(body)) {
    return(result)
  } else {
    if (res %>% httr::status_code() %in% c(200, 201, 204)) {
      message(paste0("Post URL: ", result$url, "\nSuccess :)"))
    } else {
      res %>% httr::stop_for_status()
    }
  }
}
