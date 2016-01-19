#' Add comment
#' @param team_name character
#' @param post_num integer
#' @param body_md character
#' @examples
#' \dontrun{
#' add_comment("us-lab", post_num = 15, body_md = "Test comment")
#' }
#' @name add_comment
#' @export
add_comment <- function(team_name, post_num, body_md = NULL) {
  path <-
    paste("v1/teams", team_name, "posts", post_num, "comments", sep = "/")
  esaio_api("POST", path, body = list(comment = list(body_md = body_md)))
}

#' Modify comment
#' @param team_name character
#' @param comment_id integer
#' @param body_md character
#' @examples
#' \dontrun{
#' modify_comment("us-lab",
#'  comment_id = 79598,
#'  body_md = "Change comment")
#' }
#' @name modify_comment
#' @export
modify_comment <-
  function(team_name,
           comment_id = NULL,
           body_md = NULL) {
    path <-
      paste("v1/teams", team_name, "comments", comment_id, sep = "/")
    esaio_api("PATCH", path, body = list(comment = list(body_md = body_md)))
  }

#' Delete comment
#' @param team_name character
#' @param comment_id integer
#' @examples
#' \dontrun{
#' delete_comment("us-lab", comment_id = 79601)
#' }
#' @name delete_comment
#' @export
delete_comment <- function(team_name, comment_id = NULL) {
  path <-
    paste("v1/teams", team_name, "comments", comment_id, sep = "/")
  esaio_api("DELETE", path)
}
