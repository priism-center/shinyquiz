### tools to help users preview their quizzes ###



# html preview ------------------------------------------------------------

#' Tools for previewing quizzes
#'
#' Launch a viewer to preview the html structure of the questions in a quiz.
#'
#' @param quiz an object of class 'quiz' to preview
#'
#' @return Called for side effect
#' @export
#' @author Joseph Marlo
#'
#' @examples
#' # TBD
#' @describeIn preview_quiz Preview a quiz
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
  cli::cli_alert_warning('Note `render` functions such as `shiny::renderPlot` may not show correctly in preview mode')
}

#' @param question an object of class 'quizQuestion' to preview
#' @export
#' @describeIn preview_quiz Preview a single question
preview_question <- function(question){
  verify_question_structure(question)
  htmltools::html_print(question@prompt)
}
