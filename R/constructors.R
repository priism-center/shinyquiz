### classes and constructors ###

#' Constructors for the quiz
#'
#' Construct a quiz, question, or messages object. 
#' 
#' See dev/example-app.R for current example.
#' 
#' TODO: This should probably used only on the backend and be internal only?
#'
#' @param questions a list with objects of class 'quizQuestions'
#' @param messages an object of class 'quizMessages'
#' 
#' @seealso [set_quiz_options()]
#'
#' @return an object of class `quiz`
#' @export
#' @author Joseph Marlo
#'
#' @examples
#' #TBD
#' @describeIn construct_quiz Construct a quiz object
construct_quiz <- function(questions, options = set_quiz_options()){
  if (!is.list(questions)) cli::cli_abort("`questions` should be of class 'list'")
  is_all_class_question <- isTRUE(all(purrr::map_lgl(questions, ~inherits(.x, 'quizQuestion'))))
  if (!is_all_class_question) cli::cli_abort("All items in `questions` should be of class 'quizQuestion'")
  # TODO: verify quiz options?
  
  # make quiz
  quiz <- methods::new('quiz')
  quiz@questions <- questions
  quiz@options <- options
  
  verify_quiz_structure(quiz)
  
  return(quiz)
}

#' Set the options for the quiz
#' 
#' These are options to be passed to a `quiz`.
#'
#' @param messages an object of class `quizMessages` containing the messages to show at the end. If not provided, defaults are used.
#' @param sandbox boolean. TBD
#' @param embed boolean. TBD TODO: remove?
#' @param ... other named options to pass to `quiz`
#' 
#' @seealso [construct_quiz()]
#'
#' @return a list
#' @export
set_quiz_options <- function(messages, sandbox = FALSE, embed = FALSE, ...){
  
  # set the default messages
  if (!hasArg(messages)) {
    messages <- construct_messages(
      message_correct = "Well done! You got all of them correct.",
      message_wrong = "Hmmm, bummer! You got at least one wrong.",
      message_skipped = "Quiz skipped. You can restart it using the button below."
    )
  }
  if (!inherits(messages, 'quizMessages')) cli::cli_abort("`messages` should be of class 'quizMessages'")
  
  quiz_options <- list(
    messages = messages,
    sandbox = isTRUE(sandbox),
    embed = isTRUE(embed),
    ...
  )
  
  return(quiz_options)
}

#' @param prompt an [htmltools::div] that represents a quiz question
#' @param answerUserDisplay a function that takes the user answer and prints it neatly. This is wrapped with [purrr::possibly()] to catch any errors.
#' @param answerCorrectDisplay a character that prints the correct answer neatly
#' @param grader a function that takes the user answer and determines if it is correct. Must return TRUE or FALSE. This is wrapped with [purrr::possibly()] to catch any errors.
#' @return an object of class `quizQuestion`
#' @export
#' @describeIn construct_quiz Construct a question object
construct_question <- function(prompt, answerUserDisplay, answerCorrectDisplay, grader){
  # TODO: add cli messages for arg types
  
  question <- methods::new('quizQuestion')
  question@prompt <- prompt
  question@answerUser = list(NA)
  question@answerUserDisplay <- purrr::possibly(answerUserDisplay, otherwise = '[Unable to print user response]')
  question@answerCorrectDisplay <- answerCorrectDisplay
  question@grader <- purrr::possibly(grader, otherwise = FALSE)
  
  verify_question_structure(question)
  
  return(question)
}

#' @param message_correct a string to be shown at the end of the quiz when the user gets all questions correct
#' @param message_wrong a string to be shown at the end of the quiz when the user gets at least one question wrong
#' @param message_skipped a string to be shown at the end of the quiz when the user skips the quiz or ends it early
#' @return an object of class `quizMessages`
#' @export
#' @describeIn construct_quiz Construct a messages object
construct_messages <- function(message_correct, message_wrong, message_skipped){
  messages <- methods::new('quizMessages')
  messages@message_correct <- message_correct
  messages@message_wrong <- message_wrong
  messages@message_skipped <- message_skipped
  
  return(messages)
}

#' Verify a quiz elements are the correct format
#'
#' @param question TBD
#' @keywords internal
#' @return invisible TRUE if all tests passed
#' @author Joseph Marlo
#'
#' @examples
#' # TBD
verify_question_structure <- function(question){
  
  if (!isTRUE(inherits(question, 'quizQuestion'))) cli::cli_abort('`question` must be an S4 object with class quizQuestion')
  
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

#' @keywords internal
#' @describeIn verify_question_structure Verify a function has n arguments
verify_n_args <- function(fn, n) {
  is_true <- isTRUE(length(formals(fn)) == n)
  if (!is_true) cli::cli_abort('{deparse(substitute(fn))} must have {n} arguments')
  return(invisible(TRUE))
}

#' @keywords internal
#' @describeIn verify_question_structure Verify the messages are the correct structure
verify_messages_structure <- function(messages){
  if (!isTRUE(inherits(messages, 'quizMessages'))) cli::cli_abort("`messages` be of class 'quizMessages'")
  
  return(invisible(TRUE))
}

#' @keywords internal
#' @describeIn verify_question_structure Verify a quiz is the correct structure
verify_quiz_structure <- function(quiz){
  if (!inherits(quiz, 'quiz')) cli::cli_abort('quiz must be of class quiz')
  if (!isTRUE(length(quiz@questions) > 0)) cli::cli_abort('No questions found')
  # if (!isTRUE(length(quiz@messages) > 0)) cli::cli_abort('No questions found')
  
  verify_messages_structure(quiz@options$messages)
  
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
#' @seealso [construct_question()]
setClass('quizQuestion', slots = list(
  prompt = 'shiny.tag', #TODO: figure out how to remove warning caused by this
  answerUser = 'list', # initially empty slot that will hold user answerss
  answerUserDisplay = 'function', # how to print the user answer in the report
  answerCorrectDisplay = 'character', # how to print the correct answer in the report
  grader = 'function' # function that compares user answer to the correct answer
  )
)

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
#' @seealso [construct_messages()]
setClass('quizMessages', slots = list(
  message_correct = 'character',
  message_wrong = 'character',
  message_skipped = 'character'
  )
)

#' S4 class for a quiz
#'
#' @slot questions list. A list of `quizQuestion`s
#' @slot options TBD.
#'
#' @return none, sets a class
#' @export
#' @author Joseph Marlo
#' 
#' @seealso [construct_quiz()]
setClass('quiz', slots = list(
  questions = 'list',
  # messages = 'quizMessages',
  options = 'list'
  )
)
