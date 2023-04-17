### tools to help users preview their quizes ###



# html preview ------------------------------------------------------------

#' Preview a question in the viewer
#'
#' @param question question to preview
#'
#' @return Called for side effect
#' @export
#' @author Joseph Marlo
#'
#' @examples
#' # TBD
preview_question <- function(question){
  htmltools::html_print(question)
}


# test shiny app ----------------------------------------------------------

# this should use the module defined in R/shiny-module.R