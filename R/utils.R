### general utility functions ###

#' Check to see if value is not in a vector
#'
#' This is just a negative of `%in%`
#'
#' @param ... 
#'
#' @return boolean
#' @author Joseph Marlo
#' @return NULL
#' @export
#' 
#' @examples
#' 1 %notin% 2:4
`%notin%` <- Negate(`%in%`)

#' Check if a value is truthy
#'
#' A value is truthy unless it is FALSE, NA, NULL, an empty data.frame, or empty list.
#'
#' Modified from shiny::isTruthy
#'
#' @param x An expression whose truthiness value we want to determine
#'
#' @return boolean
#'
#' @author Joseph Marlo
#' @export
#' @seealso [shiny::isTruthy()]
#'
#' @examples
#' is_truthy(TRUE)
#' is_truthy(FALSE)
#' is_truthy(1)
#' is_truthy(0)
#' is_truthy(NULL)
#' is_truthy(NA)
#' is_truthy(data.frame())
#' is_truthy(data.frame(x = 1))
is_truthy <- function(x){
  
  ##### additions
  if (inherits(x, "data.frame") && nrow(x) == 0)
    return(FALSE)
  if (inherits(x, "list") && length(x) == 0)
    return(FALSE)
  
  ##### shiny::isTruthy
  if (inherits(x, "try-error"))
    return(FALSE)
  if (!is.atomic(x))
    return(TRUE)
  if (is.null(x))
    return(FALSE)
  if (length(x) == 0)
    return(FALSE)
  if (all(is.na(x)))
    return(FALSE)
  if (is.character(x) && !any(nzchar(stats::na.omit(x))))
    return(FALSE)
  if (inherits(x, "shinyActionButtonValue") && x == 0)
    return(FALSE)
  if (is.logical(x) && !any(stats::na.omit(x)))
    return(FALSE)
  return(TRUE)
}


# to satisfy CMD CHECK when using pipe variables
if(getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      'Your Answer'
    )
  )
}

#' @title Verify a quiz question is the correct format
#'
#' @param question TBD
#'
#' @return invisible TRUE if all tests passed
#' @export
#'
#' @examples
#' # TBD
verify_question_structure <- function(question){
  
  if (!isTRUE(isS4(question))) cli::cli_abort('Must be an S4 object')
  if (!isTRUE(inherits(question, 'quizQuestion'))) cli::cli_abort('Must be an S4 object with class quizQuestion')
  
  if (!isTRUE(inherits(question@question, 'shiny.tag'))) cli::cli_abort('`question` must be of class shiny.tag. Preferably generated from htmltools::div().')
  
  if (!isTRUE(inherits(question@answerUserDisplay, 'function'))) cli::cli_abort('`answerUserDisplay` must be a function that accepts one argument and returns a character.')
  if (!isTRUE(inherits(question@answerCorrectDisplay, 'character'))) cli::cli_abort('`answerCorrectDisplay` must be a character.')
  if (!isTRUE(inherits(question@grader, 'function'))) cli::cli_abort('`grader` must be a function that accepts one argument and returns a boolean')
  
  # TODO: verify args
  
  
  return(invisible(TRUE))
}

#' S4 class for a quiz question
#'
#' @slot question shiny.tag. 
#' @slot answerUser list. 
#' @slot answerUserDisplay function. 
#' @slot answerCorrectDisplay character. 
#' @slot grader function. 
#'
#' @return
#' @export
#' @author Joseph Marlo
#'
#' @examples
#' TBD
setClass('quizQuestion', slots = list(
  question = 'shiny.tag',
  answerUser = 'list',
  answerUserDisplay = 'function', # how to print the user answer in the report
  answerCorrectDisplay = 'character', # how to print the correct answer in the report
  grader = 'function' # function that compares user answer to the correct answer
))