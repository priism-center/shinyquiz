#' Manage the states of the quiz
#'
#' The quiz has states for each question and a final state for once the quiz ends. Only one state can be active at a time and the question text and answers shown depend on what state is active. 
#' 
#' These are `get` and `set` functions for retrieving state values and setting values. The states are originally created via a `reactiveValues` call within Shiny server (or `list` outside of Shiny; see example below).
#'
#' @param store a list formatted like in the example
#' @param variable one of c('current-question', 'current-answers', 'current-correct-answer', 'next-state', 'current-response')
#' @param state one of c('quiz-question-1', ..., 'quiz-question')
#'
#' @return
#' @export
#'
#' @examples
#' question_1 <- "This is question 1"
#' question_2 <- "This is question 2"
#' question_texts <- list(question_1, question_2)
#' question_prompts <- list(c('1a', '1b'), c('2a', '2b'))
#' correct_answers <- list(c('1a'), c('2b'))
#' # use shiny::reactiveValues() in lieu of list()
#' store <- list( 
#'   state = 'quiz-question-1',
#'   states = c(paste0('quiz-question-', seq_along(question_texts)), 'quiz-complete'),
#'   question_texts = question_texts,
#'   question_prompts = question_prompts,
#'   correct_answers = correct_answers,
#'   responses = c('yes', NA, NA)
#' )
#' get_state(store, 'current-question')
#' @describeIn get_state a getter function for the state machine
get_state <- function(store, variable = NULL, state = NULL){
  if (is.null(state)) state <- store$state
  if (is.null(variable)) return(state)
  if (!(state %in% store$states)) stop('state not in store$states')
  
  if (variable == 'current-question'){
    return(store$question_texts[store$states == state][[1]])
  }
  if (variable == 'current-answers'){
    return(store$question_prompts[store$states == state][[1]])
  }
  if (variable == 'current-correct-answer'){
    return(store$correct_answers[store$states == state][[1]])
  }
  if (variable == 'next-state'){
    return(store$states[min(length(store$question_texts)+1, match(state, store$states) + 1)])
  }
  if (variable == 'current-response'){
    return(store$responses[store$states == state][[1]])
  }
}


#' @describeIn get_state a setter function for the state machine
set_state <- function(store, variable, value, state = NULL){
  if (is.null(state)) state <- get_state(store)
  if (is.null(value)) value <- character(0)
  
  if (variable == 'current-state'){
    store$state <- value
  }
  
  if (variable == 'current-response'){
    store$responses[store$states == state] <- list(value)
  }
  
  return(store)
}

#' @describeIn get_state check that current-response correct
is_current_correct <- function(store){
  current_response <- unname(get_state(store, variable = 'current-response'))
  current_correct_answer <- unname(get_state(store, variable = 'current-correct-answer'))
  identical(current_response, current_correct_answer)
}

#' @describeIn get_state check that all recorded answers are correct
is_all_correct <- function(store) {
  tryCatch({
    # extract responses and correct answers
    responses <- unname(store$responses[-(length(store$question_texts) + 1)])
    correct_answers <- unname(store$correct_answers)
    
    # remove names
    responses <- purrr::map(responses, unname)
    correct_answers <- purrr::map(correct_answers, unname)
    
    # check if they are the same
    is_identical <- identical(responses, correct_answers)
    
    return(is_identical)
  },
  error = function(e) FALSE)
}
