### classes and constructors ###

#' Constructors for the quiz
#'
#' Construct a quiz or quiz question 
#' 
#' See dev/example-app.R for current example.
#'
#' @param ... objects of class 'quizQuestions'. See [construct_question()]
#' @param options a list of options generated from [set_quiz_options()]
#' 
#' @keywords internal
#' @seealso [create_question()], [construct_question()], [set_quiz_options()], [construct_messages()]
#'
#' @return an object of class `quiz`
#' @author Joseph Marlo

#' @describeIn construct_quiz Construct a quiz object
construct_quiz <- function(..., options = set_quiz_options()){
  is_all_class_question <- isTRUE(all(purrr::map_lgl(c(...), ~inherits(.x, 'quizQuestion'))))
  if (!is_all_class_question) cli::cli_abort("All items in `questions` should be of class 'quizQuestion'")
  
  verify_options_structure(options)
  
  # make quiz
  quiz <- methods::new('quiz')
  quiz@questions <- c(...) #questions
  quiz@options <- options
  
  verify_quiz_structure(quiz)
  
  return(quiz)
}

#' Set the options for the quiz
#' 
#' These are options to be passed to a `quiz`.
#'
#' @param ns namespace generated from [shiny::NS()]. When using custom namespaces, the individual [create_question()] requires the namespace as well.
#' @param messages an object of class `quizMessages` generated from [create_messages()] containing the messages to show at the end. If not provided, defaults are used.
#' @param sandbox boolean. TBD
#' @param sandbox_resample_n The number of question resamples when in sandbox mode
#' @param end_on_first_wrong Should the quiz immediately end once the user gets one question wrong?
#' @param embed boolean. TBD TODO: remove?
#' @param progress_bar boolean. Show the progress bar UI at the top of the quiz
#' @param progress_bar_color Color code for the progress bar background
#' @param ... other named options to pass to `quiz`
#' 
#' @seealso [construct_quiz()], [construct_messages()]
#'
#' @return a list
#' @export
#' 
#' @examples 
#' quiz <- create_quiz(
#'   create_question(
#'     'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Select nulla.',
#'     add_choice('auctor'),
#'     add_choice('nulla', correct = TRUE)
#'   ),
#'   create_question(
#'     'Mauris congue aliquet dui, ut dapibus lorem porttitor sed. Select 600.',
#'     add_choice('600', correct = TRUE),
#'     add_choice('800')
#'   ),
#'   options = set_quiz_options(sandbox = TRUE)
#' )
#' quiz@options <- set_quiz_options(sandbox = FALSE)
#' @describeIn set_quiz_options Sets the options for a `quiz`
set_quiz_options <- function(ns = shiny::NS('quiz'), messages,  sandbox = NULL, sandbox_resample_n = 50, end_on_first_wrong = !sandbox, embed = FALSE, progress_bar = !sandbox, progress_bar_color = '#609963', ...){
  if(is.null(sandbox))  sandbox <- TRUE
  # set the default messages
  if (!methods::hasArg(messages)) {
    messages <- construct_messages(
      message_correct = "Well done! You got all of them correct.",
      message_wrong = "Hmmm, bummer! You got at least one wrong.",
      message_skipped = "Quiz ended. You can restart it using the button below."
    )
  }
  if (!inherits(messages, 'quizMessages')) cli::cli_abort("`messages` should be of class 'quizMessages'")
  
  quiz_options <- list(
    ns = ns,
    messages = messages,
    sandbox = sandbox,
    sandbox_resample_n = as.integer(sandbox_resample_n),
    logic_end_on_first_wrong = isTRUE(end_on_first_wrong),
    embed = isTRUE(embed),
    progress_bar = isTRUE(progress_bar),
    progress_bar_color = progress_bar_color,
    ...
  )
  
  verify_options_structure(quiz_options)
  
  return(quiz_options)
}

#' @keywords internal
#' @describeIn verify_question_structure Verify the options is the right structure
verify_options_structure <- function(options){
  
  if (!is.list(options)) cli::cli_abort("`options` must be a list")
  
  # check if all required options exist
  req_items <- c('ns', 'messages', 'sandbox', 'embed')
  req_items_in_options <- req_items %in% names(options)
  all_req_items_exist <- isTRUE(all(req_items_in_options))
  if (!all_req_items_exist) cli::cli_abort('Missing in options: {req_items[!req_items_in_options]}')
  
  # check data types
  if (!isTRUE(is.function(options$ns))) cli::cli_abort('`ns` must be a function. Preferably generated from `shiny::NS()`')
  if (!inherits(options$messages, 'quizMessages')) cli::cli_abort('`messages` should be of class `quizMessages`')
  if (!isTRUE(is.logical(options$sandbox))) cli::cli_abort('`sandbox` should be of class `logical`')
  if (!isTRUE(is.logical(options$embed))) cli::cli_abort('`embed` should be of class `logical`')
  
  return(invisible(TRUE))
}

#' @param message_correct a string to be shown at the end of the quiz when the user gets all questions correct
#' @param message_wrong a string to be shown at the end of the quiz when the user gets at least one question wrong
#' @param message_skipped a string to be shown at the end of the quiz when the user skips the quiz or ends it early
#' 
#' @export
#' @return an object of class `quizMessages`
#' @describeIn set_quiz_options Create a messages object
create_messages <- function(message_correct, message_wrong, message_skipped){
  construct_messages(message_correct, message_wrong, message_skipped)
}

#' @param prompt an [htmltools::div] that represents a quiz question
#' @param answerUserPrettifier a function that takes the user answer and prints it neatly. This is wrapped with [purrr::possibly()] to catch any errors.
#' @param answerCorrectPretty a character that prints the correct answer neatly
#' @param grader a function that takes the user answer and determines if it is correct. Must take one argument and return TRUE or FALSE. This is wrapped with [purrr::possibly()] and [base::isTRUE()] to catch any errors.
#' @param ns namespace generated from [shiny::NS()]
#' 
#' @keywords internal
#' @return an object of class `quizQuestion`
#' @describeIn construct_quiz Construct a question object
construct_question <- function(prompt, answerUserPrettifier, answerCorrectPretty, grader, ns){

  if (!isTRUE(inherits(prompt, 'shiny.tag'))) cli::cli_abort("`prompt` must be of class 'shiny.tag'. Preferably generated from `htmltools::div()`")
  if (!isTRUE(is.function(answerUserPrettifier))) cli::cli_abort('`answerUserPrettifier` must be a function with one argument')
  if (!isTRUE(is.character(answerCorrectPretty))) cli::cli_abort('`answerCorrectPretty` must be a string')
  if (!isTRUE(is.function(grader))) cli::cli_abort('`grader` must be a function with one argument')
  if (!isTRUE(is.function(ns))) cli::cli_abort('`ns` must be a function with one argument')
  
  question <- methods::new('quizQuestion')
  question@prompt <- prompt
  question@answerUser = list(NA)
  question@answerUserPrettifier <- answerUserPrettifier
  question@answerCorrectPretty <- answerCorrectPretty
  question@grader <- grader
  question@ns <- ns
  
  verify_question_structure(question)
  
  return(question)
}

#' @param message_correct a string to be shown at the end of the quiz when the user gets all questions correct
#' @param message_wrong a string to be shown at the end of the quiz when the user gets at least one question wrong
#' @param message_skipped a string to be shown at the end of the quiz when the user skips the quiz or ends it early
#' 
#' @keywords internal
#' @return an object of class `quizMessages`
#' @describeIn construct_quiz Construct a messages object
#' @seealso [set_quiz_options()]
construct_messages <- function(message_correct, message_wrong, message_skipped){
  
  if (!isTRUE(is.character(message_correct))) cli::cli_abort('`message_correct` must be class character')
  if (!isTRUE(is.character(message_wrong))) cli::cli_abort('`message_wrong` must be class character')
  if (!isTRUE(is.character(message_skipped))) cli::cli_abort('`message_skipped` must be class character')

  
  messages <- methods::new('quizMessages')
  messages@message_correct <- message_correct
  messages@message_wrong <- message_wrong
  messages@message_skipped <- message_skipped
  
  return(messages)
}

#' Verify quiz elements are the correct format
#'
#' @param question An object of class `quizQuestion`
#' 
#' @keywords internal
#' @return invisible TRUE if all tests passed
#' @author Joseph Marlo

#' @describeIn verify_question_structure Verify a question is the right structure
verify_question_structure <- function(question){
  
  if (!isTRUE(inherits(question, 'quizQuestion'))) cli::cli_abort('`question` must be an S4 object with class quizQuestion')
  
  if (!isTRUE(inherits(question@prompt, 'shiny.tag'))) cli::cli_abort('`question` must be of class shiny.tag. Preferably generated from htmltools::div().')
  
  if (!isTRUE(inherits(question@answerUserPrettifier, 'function'))) cli::cli_abort('`answerUserPrettifier` must be a function that accepts one argument and returns a character.')
  if (!isTRUE(inherits(question@answerCorrectPretty, 'character'))) cli::cli_abort('`answerCorrectPretty` must be a character.')
  if (!isTRUE(inherits(question@grader, 'function'))) cli::cli_abort('`grader` must be a function that accepts one argument and returns a boolean')

  # check to see if there is an input with id "answers"
  verify_input_id(question@prompt)
  
  # verify number of args in functions
  verify_n_args(question@answerUserPrettifier, 1)
  verify_n_args(question@grader, 1)

  return(invisible(TRUE))
}

#' @keywords internal
#' @describeIn verify_question_structure Verify a function has an input with id = 'answers'
verify_input_id <- function(prompt){
  # TODO: this is a bit fragile
  id_detected <- stringr::str_detect(as.character(prompt), "\\banswers\\b")
  if (!isTRUE(id_detected)) cli::cli_abort("'`question` must contain an input with id = 'answers'. This is used to extract the user's answer.")
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
  ns_fns <- purrr::map(quiz@questions, \(x) x@ns)
  if (!isTRUE(length(unique(ns_fns)) == 1)) cli::cli_alert_warning('Multiple unique `ns` provided. Check your `create_question()` calls. Sometimes this is okay.')
  
  verify_messages_structure(quiz@options$messages)
  
  return(invisible(TRUE))
}


# S4 classes --------------------------------------------------------------

# this is purely to satisfy the CMD check warning in `quizQuestion`
setClass('shiny.tag')

#' S4 class for a quiz question
#'
#' @slot prompt shiny.tag. 
#' @slot answerUser list. 
#' @slot answerUserPrettifier function. 
#' @slot answerCorrectPretty character. 
#' @slot grader function.
#'
#' @return none, sets a class
#' @author Joseph Marlo
#' @keywords internal
#'
#' @seealso [construct_question()]
setClass('quizQuestion', slots = list(
  prompt = 'shiny.tag', 
  answerUser = 'list', # initially empty slot that will hold user answerss
  answerUserPrettifier = 'function', # how to print the user answer in the report
  answerCorrectPretty = 'character', # how to print the correct answer in the report
  grader = 'function', # function that compares user answer to the correct answer
  ns = 'function'
  )
)

#' S4 class for a sandbox quiz question
#'
#' @slot prompt shiny.tag. 
#' @slot answerUser list. 
#' @slot answerUserPrettifier function. 
#' @slot answerCorrectPretty character. 
#' @slot grader function.
#'
#' @return none, sets a class
#' @author George Perrett, Joseph Marlo
#' @keywords internal
#'
#' @seealso [construct_question()]
setClass('quizQuestionSandbox', contains = 'quizQuestion')

#' S4 class for a quiz messages to display at the end
#'
#' @slot message_correct character. 
#' @slot message_wrong character. 
#' @slot message_skipped character. 
#'
#' @return none, sets a class
#' @author Joseph Marlo
#' @keywords internal
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
#' @slot options list. a list generated from [set_quiz_options()]
#'
#' @return none, sets a class
#' @author Joseph Marlo
#' @keywords internal
#' 
#' @seealso [construct_quiz()]
setClass('quiz', slots = list(
  questions = 'list',
  options = 'list'
  )
)


# print methods -----------------------------------------------------------

setMethod(
  f = "show",
  signature = "quizQuestion",
  definition = function(object){
    if (shiny::isRunning()){
      return(invisible(NULL))
    } else {
      preview_question(object)
    }
  }
)

setMethod(
  f = "show",
  signature = "quiz",
  definition = function(object){
    if (shiny::isRunning()){
      return(invisible(NULL))
      } else {
        preview_quiz(object)
      }
  }
)
