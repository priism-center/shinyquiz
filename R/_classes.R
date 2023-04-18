### define S4 classes ###
# TODO: is this the appropriate place?
# roxygen has trouble with this file so something preceding it is likely wrong
# temporarily moved functions to bottom of utils.R

# TODO: how to export classes?
# setClass('quizQuestion', slots = list(
#   question = 'shiny.tag',
#   answerUser = 'list',
#   answerUserDisplay = 'function', # how to print the user answer in the report
#   answerCorrectDisplay = 'character', # how to print the correct answer in the report
#   grader = 'function' # function that compares user answer to the correct answer
# ))

#' #' @title Verify a quiz question is the correct format
#' #'
#' #' @param question TBD
#' #'
#' #' @return invisible TRUE if all tests passed
#' #' @export
#' #'
#' #' @examples
#' #' # TBD
#' verify_question_structure <- function(question){
#'   
#'   if (!isTRUE(isS4(question))) cli::cli_abort('Must be an S4 object')
#'   if (!isTRUE(inherits(question, 'quizQuestion'))) cli::cli_abort('Must be an S4 object with class quizQuestion')
#'   
#'   if (!isTRUE(inherits(question@prompt, 'shiny.tag'))) cli::cli_abort('`question` must be of class shiny.tag. Preferably generated from htmltools::div().')
#'   
#'   if (!isTRUE(inherits(question@answerUserDisplay, 'function'))) cli::cli_abort('`answerUserDisplay` must be a function that accepts one argument and returns a character.')
#'   if (!isTRUE(inherits(question@answerCorrectDisplay, 'character'))) cli::cli_abort('`answerCorrectDisplay` must be a character.')
#'   if (!isTRUE(inherits(question@grader, 'function'))) cli::cli_abort('`grader` must be a function that accepts one argument and returns a boolean')
#'   
#'   # TODO: verify args
#'   
#'   
#'   return(invisible(TRUE))
#' }



# TODO: need class for quiz
# TODO: need verify_quiz_structure
