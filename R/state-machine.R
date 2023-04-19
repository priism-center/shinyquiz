### core logic for controlling the quiz ###

#' Functions for managing the quiz state machine
#'
#' The quiz is implemented via a state machine framework. It has states for each question and a final state for once the quiz ends. Only one state can be active at a time. The question text and answers shown depend on what state is active.
#'
#' These are `get` and `set` functions for retrieving state values and setting values. The states are originally created via a `reactiveValues` call within Shiny server (or `list` outside of Shiny; see example below).
#'
#' @param store a list formatted like in the example
#' @param variable one of c('current-question', 'current-correct-answer', 'next-state', 'current-response')
#' @param state one of c('quiz-question-1', ..., 'quiz-question')
#'
#' @return depends on function
#' @export
#'
#' @author Joseph Marlo
#'
#' @examples
#' \dontrun{
#' #TODO: this is the deprecated method; need to update
#' question_1 <- "This is question 1"
#' question_2 <- "This is question 2"
#' #construct_question()
#' #construct_messages()
#' #quiz <- construct_quiz(...)
#' store <- sm_create_reactive_store(quiz, sandbox_mode = FALSE)
#' sm_get_state(store, 'current-question')
#' }
#' @describeIn sm_get_state Getter function for the state machine
sm_get_state <- function(store, variable = NULL, state = NULL){
  if (is.null(state)) state <- store$state
  if (is.null(variable)) return(state)
  if (!(state %in% store$states)) stop('state not in store$states')
  
  if (variable == 'current-question'){
    return(store$questions[store$states == state][[1]]@prompt)
  }
  # if (variable == 'current-answers'){
  #   return(store$question_prompts[store$states == state][[1]])
  # }
  if (variable == 'current-correct-answer'){
    # return(store$correct_answers[store$states == state][[1]])
    return(store$questions[store$states == state][[1]]@answerCorrectDisplay)
  }
  if (variable == 'current-grader'){
    # return(store$graders[store$states == state][[1]])
    return(store$questions[store$states == state][[1]]@grader)
  }
  if (variable == 'current-correct'){
    return(store$is_correct[store$states == state])
  }
  if (variable == 'next-state'){
    return(store$states[min(length(store$states), match(state, store$states) + 1)])
  }
  if (variable == 'current-response'){
    # return(store$responses[store$states == state][[1]])
    return(store$questions[store$states == state][[1]]@answerUser[[1]]) # there's some weird indexing that happens with the lists
  }
  if (variable == 'quiz-skipped'){
    return(store$skipped)
  }
  if (variable == 'sandbox-mode'){
    return(store$sandbox_mode)
  }
}

#' @describeIn sm_get_state Setter function for the state machine
sm_set_state <- function(store, variable, value, state = NULL){
  
  if (is.null(state)) state <- sm_get_state(store)
  if (is.null(value)) value <- character(0)
  
  if (variable == 'current-state'){
    store$state <- value
  }
  if (variable == 'current-response'){
    # store$responses[store$states == state] <- list(value)
    # store$questions[store$states == state][[1]]@answerUser[[1]] <- value
    store$questions[[which(store$states == state)]]@answerUser[[1]] <- value # there's some weird indexing that happens with the lists
  }
  if (variable == 'quiz-skipped'){
    if (!is.logical(value)) stop('value must logical for "quiz-skipped" variable')
    store$skipped <- value
  }
  if (variable == 'current-correct'){
    state_index <- store$states[store$states != 'quiz-complete']
    store$is_correct[state_index == state] <- value
  }
  
  return(store)
}

#' @describeIn sm_get_state Backup function to check that an answer matches a response, agnostic of ordering. Deprecated?
sm_is_answer_correct <- function(answer, key){
  # TODO: deprecated?
  if (length(answer) != length(key)) return(FALSE)
  if(!is.numeric(answer)) is_correct <- purrr::map2_lgl(answer, key, function(resp, key) base::setequal(resp, key))
  if(is.numeric(answer)) is_correct <-  purrr::map2_lgl(answer, key, function(resp, key) dplyr::between(resp, key-.1, key+.1))
  is_correct <- isTRUE(all(is_correct))
  return(is_correct)
}

#' @describeIn sm_get_state Check that current-response is correct
sm_is_current_correct <- function(store){
  current_response <- unname(sm_get_state(store, variable = 'current-response'))
  current_correct_answer <- unname(sm_get_state(store, variable = 'current-correct-answer'))
  
  # if there is a grader function, use it. Otherwise use the generic one defined above
  current_grader <- sm_get_state(store, 'current-grader')
  if (is_truthy(current_grader)){
    is_correct <- current_grader(current_response)
  } else {
    # TODO: deprecated?
    is_correct <- sm_is_answer_correct(current_response, current_correct_answer)
  }
  return(isTRUE(is_correct))
}

#' @describeIn sm_get_state Check that each recorded answer is correct and return a boolean vector
sm_check_is_each_correct <- function(store){
  return(store$is_correct)
}

#' @describeIn sm_get_state Check that all recorded answers are correct
sm_is_all_correct <- function(store) {
  return(isTRUE(all(sm_check_is_each_correct(store))))
}

#' @describeIn sm_get_state Check if the quiz in sandbox mode
sm_quiz_in_sandbox_mode <- function(store){
  isTRUE(sm_get_state(store, 'sandbox-mode'))
}

#' Add headers with question numbers to the quiz questions
#'
#' @param quiz a quiz; see ...?
#'
#' @return quiz
#' @export
#' @author Joseph Marlo
#'
#' @examples
#' #TBD
sm_ui_format_prompts <- function(quiz){
  verify_quiz_structure(quiz)
  
  for (i in seq_along(quiz@questions)){
    quiz@questions[[i]]@prompt <- sm_ui_format_prompt(quiz@questions[[i]]@prompt, i)
  }
  
  return(quiz)
}

#' @describeIn sm_ui_format_prompts Add a header denoting the question number
sm_ui_format_prompt <- function(prompt, i){
  htmltools::div(
    htmltools::h4("Practice what you've learned"),
    htmltools::hr(),
    htmltools::h3(glue::glue("Question {i}")), # h3 required for checkmark/red x placement
    prompt
  )
}

#' @describeIn sm_get_state UI to show once the quiz is completed
sm_ui_quiz_complete <- function(store, ns, messages){
  
  verify_messages_structure(messages)
  
  # render ending message based on if answers are correct
  all_correct <- sm_is_all_correct(store)
  is_skipped <- sm_get_state(store, variable = 'quiz-skipped')
  
  if (is_skipped){
    html_content <- htmltools::tagList(
      htmltools::br(), 
      add_message_skipped(messages@message_skipped)
    )
  } else if (all_correct) {
    html_content <- htmltools::tagList(
      htmltools::br(),
      add_message_correct(messages@message_correct),
      add_confetti()
    )
  } else {
    html_content <- htmltools::tagList(
      htmltools::br(), 
      add_message_wrong(messages@message_wrong)
    )
  }
  
  # render the report table
  grade_report <- sm_ui_complete_report(store)
  
  # render the restart button
  restart_button <- shiny::actionButton(
    inputId = ns('restart_button'),
    label = 'Restart quiz',
    class = 'restart-button'
  )
  
  # put it all together
  html_content <- htmltools::tagList(
    html_content,
    grade_report,
    restart_button,
    htmltools::br(), htmltools::br(), htmltools::hr(), htmltools::br()
  )
  
  return(html_content)
}

#' @describeIn sm_get_state UI to show the score and table of correct answers to display at the end
sm_ui_complete_report <- function(store){
  
  in_sandbox <- sm_quiz_in_sandbox_mode(store)
  
  # grade answers and convert into icons
  icon_right <- fontawesome::fa_i('check') |> as.character()
  icon_wrong <- fontawesome::fa_i('times') |> as.character()
  answers <- sm_check_is_each_correct(store)
  answers_icons <- c(icon_wrong, icon_right)[answers + 1]
  
  # format question labels
  question_label <- paste0('Question ', seq_along(store$questions))
  
  # calculate score and format the score
  # if in sandbox mode, score is only for non skipped items
  answers_user_print <- purrr::map(store$questions, ~.x@answerUserDisplay(.x@answerUser[[1]]))
  answers_user_na <- purrr::map(store$questions, ~.x@answerUser[[1]]) |> is.na() # assumes NAs are skipped questions
  score <- ifelse(
    in_sandbox,
    mean(answers[!answers_user_na]),
    mean(answers)
  )
  if(is.na(score)) score <- 0
  score <- scales::percent_format()(score)
  
  # add skipped label to skipped questions
  skip_label <- '[skipped]'
  answers_user_print[answers_user_na] <- skip_label
  
  # get formatted correct answers
  answers_correct_print <- purrr::map_chr(store$questions, ~.x@answerCorrectDisplay)
  answers_correct_print[answers_user_na] <- skip_label
  
  # put everything in a table
  grade_tbl <- tibble::tibble(
    icon = answers_icons,
    label = question_label,
    `Your Answer` = answers_user_print,
    `Correct Answer` = answers_correct_print
  )
  
  # remove skipped rows if in sandbox mode
  if (sm_quiz_in_sandbox_mode(store)){
    grade_tbl <- dplyr::filter(grade_tbl,`Your Answer` != skip_label)
  }
  
  # convert to reactable
  grade_tbl <- reactable::reactable(
    grade_tbl,
    columns = list(
      icon = reactable::colDef(name = '', html = TRUE, width = 40),
      label = reactable::colDef(name = '', width = 115),
      `Your Answer` = reactable::colDef(align = 'right'),
      `Correct Answer` = reactable::colDef(align = 'right')
    )
  )
  
  # add score to top of table
  grade_report <- htmltools::tagList(
    htmltools::br(),
    htmltools::h4(glue::glue('Score: {score}')),
    grade_tbl
  )
  
  return(grade_report)
}

#' @describeIn sm_get_state UI to show for the current question
sm_ui_question <- function(store, ns){
  
  # render the questions
  html_content <- htmltools::tagList(
    
    # question content
    sm_get_state(store, 'current-question'),
    
    # button to submit answer
    shiny::actionButton(
      inputId = ns('submit_button'),
      label = 'Submit',
      class = 'submit-button'
    ),
    
    # button to skip quiz
    shiny::actionButton(
      inputId = ns('skip_button'),
      label = ifelse(sm_quiz_in_sandbox_mode(store), 'Finish quiz', 'Skip quiz'),
      class = 'skip-button'
    )
  )
  
  return(html_content)
}

#' @param quiz A list of questions of class 'quizQuestion'
#' @param sandbox_mode boolean
#' @param n Number of resamples to make
#'
#' @return questions
#' @export
#' @author Joseph Marlo
#'
#' @examples
#' #TBD
#' @describeIn sm_get_state Create quasi infinite quiz by resampling questions n times
sm_resample_questions_if_sandbox <- function(quiz, sandbox_mode, n = 50){
  if (!(is.numeric(n) && n > 0)) cli::cli_abort('n must be positive integer')
  verify_quiz_structure(quiz)
  
  if (isTRUE(sandbox_mode)){
    indices <- sample(seq_along(quiz@questions), size = n, replace = TRUE)
    quiz@questions <- quiz@questions[indices]
  }
  
  return(quiz)
}


#' @param quiz an object of class 'quiz'
#' @param sandbox_mode boolean
#' 
#' @seealso [shiny::reactiveValues()]
#'
#' @return reactiveValues
#' @export
#' @author Joseph Marlo
#' 
#' @describeIn sm_get_state Create the main store object that handles the state(s)
sm_create_reactive_store <- function(quiz, sandbox_mode){
  verify_quiz_structure(quiz)
  
  # use a static list if not in reactive context (i.e. Shiny)
  list_fn <- shiny::reactiveValues
  if (isFALSE(shiny::isRunning())) list_fn <- base::list
  
  store <- list_fn(
    state = 'quiz-question-1',
    states = c(paste0('quiz-question-', seq_along(quiz@questions)), 'quiz-complete'),
    questions = quiz@questions,
    is_correct = rep(FALSE, length(quiz@questions)),
    ui_html = NULL,
    skipped = FALSE,
    sandbox_mode = isTRUE(sandbox_mode)
  )
  
  return(store)
}
