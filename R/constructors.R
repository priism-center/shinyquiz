### classes and constructors ###

#' Constructors for the quiz
#'
#' This should probably used only on the backend
#'
#' @param prompt 
#' @param answerUserDisplay 
#' @param answerCorrectDisplay 
#' @param grader 
#'
#' @return TBD
#' @export
#' @author Joseph Marlo
#'
#' @examples
#' #TBD
construct_quiz <- function(questions, messages){
  if (!is.list(questions)) cli::cli_abort('`questions` should be of class list')
  quiz <- new('quiz')
  quiz@questions <- questions
  quiz@messages <- messages
  
  verify_quiz_structure(quiz)
  
  return(quiz)
}

#' @describeIn construct_quiz Construct the question object
construct_question <- function(prompt, answerUserDisplay, answerCorrectDisplay, grader){
  question <- new('quizQuestion')
  question@prompt <- prompt
  question@answerUser = list(NA)
  question@answerUserDisplay <- answerUserDisplay
  question@answerCorrectDisplay <- answerCorrectDisplay
  question@grader <- grader
  
  verify_question_structure(question)
  
  return(question)
}

#' @describeIn construct_quiz Construct the messages object
construct_messages <- function(message_correct, message_wrong, message_skipped){
  messages <- new('quizMessages')
  messages@message_correct <- message_correct
  messages@message_wrong <- message_wrong
  messages@message_skipped <- message_skipped
  
  return(messages)
}

#' @title Verify a quiz question is the correct format
#'
#' @param question TBD
#'
#' @return invisible TRUE if all tests passed
#' @export
#' @author Joseph Marlo
#'
#' @examples
#' # TBD
verify_question_structure <- function(question){
  
  if (!isTRUE(isS4(question))) cli::cli_abort('Must be an S4 object')
  if (!isTRUE(inherits(question, 'quizQuestion'))) cli::cli_abort('Must be an S4 object with class quizQuestion')
  
  if (!isTRUE(inherits(question@prompt, 'shiny.tag'))) cli::cli_abort('`question` must be of class shiny.tag. Preferably generated from htmltools::div().')
  
  if (!isTRUE(inherits(question@answerUserDisplay, 'function'))) cli::cli_abort('`answerUserDisplay` must be a function that accepts one argument and returns a character.')
  if (!isTRUE(inherits(question@answerCorrectDisplay, 'character'))) cli::cli_abort('`answerCorrectDisplay` must be a character.')
  if (!isTRUE(inherits(question@grader, 'function'))) cli::cli_abort('`grader` must be a function that accepts one argument and returns a boolean')
  
  # if (!isTRUE(question@prompt))) cli::cli_abort('`grader` must be a function that accepts one argument and returns a boolean')
  
  # check to see if there is an input with id "answers"
  # TODO: this is a bit fragile
  id_detected <- question@prompt |> as.character() |> stringr::str_detect("\\banswers\\b")
  if (!isTRUE(id_detected)) cli::cli_abort("'`question` must contain an input with id = 'answers'. This is used to extract the user's answer.")
  
  # verify number of args in functions
  verify_n_args(question@answerUserDisplay, 1)
  verify_n_args(question@grader, 1)
  
  
  return(invisible(TRUE))
}

#' @describeIn verify_question_structure Verify a function has n arguments
verify_n_args <- function(fn, n) {
  is_true <- isTRUE(length(formals(fn)) == n)
  if (!is_true) cli::cli_abort('{deparse(substitute(fn))} must have {n} arguments')
  return(invisible(TRUE))
}

#' Verify that a quiz is a quiz
#'
#' @param quiz a quiz; see ...? Effectively a list of questions
#'
#' @return invisible TRUE if no errors
#' @export
#' @author Joseph Marlo
#'
#' @examples
#' #TBD
verify_quiz_structure <- function(quiz){
  if (!inherits(quiz, 'quiz')) cli::cli_abort('quiz must be of class quiz')
  if (!isTRUE(length(quiz@questions) > 0)) cli::cli_abort('No questions found')
  # if (!isTRUE(length(quiz@messages) > 0)) cli::cli_abort('No questions found')
  
  return(invisible(TRUE))
}

#' S4 class for a quiz question
#'
#' @slot prompt shiny.tag. 
#' @slot answerUser list. 
#' @slot answerUserDisplay function. 
#' @slot answerCorrectDisplay character. 
#' @slot grader function. 
#'
#' @return none, sets a class
#' @export
#' @author Joseph Marlo
#'
#' @examples
#' # TBD
setClass('quizQuestion', slots = list(
  prompt = 'shiny.tag',
  answerUser = 'list',
  answerUserDisplay = 'function', # how to print the user answer in the report
  answerCorrectDisplay = 'character', # how to print the correct answer in the report
  grader = 'function' # function that compares user answer to the correct answer
))

#' S4 class for a quiz messages to display at the end
#'
#' @slot message_correct character. 
#' @slot message_wrong character. 
#' @slot message_skipped character. 
#'
#' @return none, sets a class
#' @export
#' @author Joseph Marlo
#'
#' @examples
#' # TBD
setClass('quizMessages', slots = list(
  message_correct = 'character',
  message_wrong = 'character',
  message_skipped = 'character'
))

#' S4 class for a quiz
#'
#' @slot questions list. A list of quizQuestions
#' @slot messages quizMessages. 
#'
#' @return none, sets a class
#' @export
#' @author Joseph Marlo
#'
#' @examples
#' # TBD
setClass('quiz', slots = list(
  questions = 'list',
  messages = 'quizMessages'
))
