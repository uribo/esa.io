#' Get post description
#' @param team_name character.
#' @param post_num integer.
#' @examples
#' description_post(team_name = "us-lab", post_num = 17)
#' @name description_post
#' @export
description_post <- function(team_name, post_num) {
  path <- paste("v1/teams", team_name, "posts", post_num, sep = "/")
  esaio_api("GET", path)
}

#' Create new post
#' @param team_name character
#' @param post_name character. required
#' @param body_md character.
#' @param ... Additional parameters
#' @examples
#' \dontrun{
#' create_post("us-lab",
#' post_name = paste("sandbox", Sys.Date(), sep = "/"),
#' body_md = "Test",
#' tags = array("sandbox"),
#' wip = FALSE)
#' }
#' @name create_post
#' @export
create_post <-
  function(team_name,
           post_name = NULL,
           body_md = NULL,
           ...) {
    path <- paste("v1/teams", team_name, "posts", sep = "/")
    esaio_api("POST", path,
              body = list(
                post = list(
                  name = post_name,
                  body_md = body_md,
                  tags = NULL,
                  wip  = TRUE,
                  category = NULL
                )
              ))
  }

#' Create new post from RMarkdown
#' @param team_name character
#' @param file Input file (Rmd)
#' @param post_name character. required
#' @param ... Additional parameters
#' @importFrom rmarkdown render
#' @examples
#' \dontrun{
#' create_rmd_post("us-lab", file = "test.Rmd",
#'  post_name = "Post Rmarkdown file", tags = "sandbox")
#' }
#' @name create_rmd_post
#' @export
create_rmd_post <-
  function(team_name,
           file = NULL,
           post_name = NULL,
           ...) {
    path <- paste("v1/teams", team_name, "posts", sep = "/")

    rmarkdown::render(
      input = file,
      output_format = "md_document",
      output_file = paste(tempdir(), "post_file.md", sep = "/"),
      quiet = TRUE
    )

    esaio_api("POST", path,
              body = list(
                post = list(
                  name = post_name,
                  body_md = paste(readLines(
                    paste(tempdir(), "post_file.md", sep = "/"), encoding = "UTF-8"
                  ), collapse = "\n"),
                  tags = NULL,
                  wip  = TRUE,
                  category = NULL
                )
              ))

    file.remove(paste(tempdir(), "post_file.md", sep = "/"))
  }


#' Modify post
#' @param team_name character
#' @param post_num integer
#' @param post_name character
#' @param body_md character
#' @param append logical
#' @param ... Additional parameters
#' @examples
#' \dontrun{
#' modify_post("us-lab", post_num = 17,
#'  post_name = "New Title", body_md = "Append mode", append = TRUE)
#' modify_post("us-lab", post_num = 17,
#'  body_md = "Overwrite mode", append = FALSE)
#' }
#' @name modify_post
#' @export
modify_post <-
  function(team_name,
           post_num,
           post_name = NULL,
           body_md = NULL,
           append = TRUE,
           ...) {
    path <- paste("v1/teams", team_name, "posts", post_num, sep = "/")

    if (append == TRUE) {
      res_back <- description_post(team_name, post_num)
      body <- list(
        name = post_name,
        body_md = paste(body_md, res_back$body_md, sep = "\n"),
        tags = tags,
        original_revision = list(
          body_md = res_back$body_md,
          user    = res_back$updated_by$screen_name
        )
      )

    } else {
      body <- list(name = post_name,
                   body_md = body_md,
                   tags = tags)
    }
    esaio_api("PATCH", path,
              body = body)
  }

#' Delete post
#' @param team_name character
#' @param post_num integer
#' @examples
#' \dontrun{
#' delete_post("us-lab", 17)
#' }
#' @name delete_post
#' @export
delete_post <- function(team_name, post_num) {
  path <- paste("v1/teams", team_name, "posts", post_num, sep = "/")
  esaio_api("DELETE", path)
}
