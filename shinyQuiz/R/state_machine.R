#' Get the current state of the question
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
#' questions <- list(question_1, question_2)
#' answers <- list(c('1a', '1b'), c('2a', '2b'))
#' correct_answers <- list(c('1a'), c('2b'))
#' # use shiny::reactiveValues() in lieu of list()
#' store <- list( 
#'   state = 'quiz-question-1',
#'   states = c(paste0('quiz-question-', seq_along(questions)), 'quiz-complete'),
#'   questions = questions,
#'   answers = answers,
#'   correct_answers = correct_answers,
#'   responses = c('yes', NA, NA)
#' )
#' get_state(store, 'current-question')
get_state <- function(store, variable = NULL, state = NULL){
  if (is.null(state)) state <- store$state
  if (is.null(variable)) return(state)
  if (!(state %in% store$states)) stop('state not in store$states')
  
  if (variable == 'current-question'){
    return(store$questions[store$states == state][[1]])
  }
  if (variable == 'current-answers'){
    return(store$answers[store$states == state][[1]])
  }
  if (variable == 'current-correct-answer'){
    return(store$correct_answers[store$states == state][[1]])
  }
  if (variable == 'next-state'){
    return(store$states[min(length(store$questions)+1, match(state, store$states) + 1)])
  }
  if (variable == 'current-response'){
    return(store$responses[store$states == state][[1]])
  }
}

set_response <- function(store, response, state = NULL){
  if (is.null(state)) state <- get_state(store)
  store$responses[store$states == state] <- response
  
  return(store)
}

is_all_correct <- function(store){
  isTRUE(all(store$responses[-(length(store$questions)+1)] == store$correct_answers))
}
