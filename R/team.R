#' Get team information
#' @examples
#' description_team()
#' @name description_team
#' @export
description_team <- function() {
  path    <- "/v1/teams"
  esaio_api("GET", path)
}
