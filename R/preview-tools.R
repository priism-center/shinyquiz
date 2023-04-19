### tools to help users preview their quizes ###



# html preview ------------------------------------------------------------

#' Tools for previewing quizes
#'
#' @param question question to preview
#'
#' @return Called for side effect
#' @export
#' @author Joseph Marlo
#'
#' @examples
#' # TBD
preview_quiz <- function(quiz){
  verify_quiz_structure(quiz)
  panels <- shiny::fluidPage(
    do.call(
      shiny::tabsetPanel, 
      c(id = 't', 
        purrr::map2(quiz@questions, seq_along(quiz@questions), function(q, i) {
          shiny::tabPanel(
            title = glue::glue('Question {i}'), 
            q@prompt
          )
        })
      )
    )
  )
  htmltools::html_print(panels)
}
#' @describeIn preview_quiz Preview a single question
preview_question <- function(question){
  verify_question_structure(question)
  htmltools::html_print(question@prompt)
}
