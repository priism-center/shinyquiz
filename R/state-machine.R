### core logic for controlling the quiz ###

# TODO: all functions that start with 'quiz_' should start with 'sm_'

#' @title Functions for managing the states of the quiz
#'
#' @description The quiz has states for each question and a final state for once the quiz ends. Only one state can be active at a time and the question text and answers shown depend on what state is active.
#'
#' These are `get` and `set` functions for retrieving state values and setting values. The states are originally created via a `reactiveValues` call within Shiny server (or `list` outside of Shiny; see example below).
#'
#' See the post-treatment learning module for a working example.
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
#' question_texts <- list(question_1, question_2)
#' question_prompts <- list(radioButtons(...), radioButtons(...))
#' correct_answers <- list(c('1a'), c('2b'))
#' # use shiny::reactiveValues() in lieu of list()
#' store <- list(
#'   state = 'quiz-question-1',
#'   states = c(paste0('quiz-question-', seq_along(question_texts)), 'quiz-complete'),
#'   question_texts = question_texts,
#'   question_prompts = question_prompts,
#'   correct_answers = correct_answers,
#'   responses = c('yes', NA, NA),
#'   skipped = FALSE,
#'   sandbox_mode = FALSE
#' )
#' quiz_get_state(store, 'current-question')
#' }
#' @describeIn quiz_get_state a getter function for the state machine
quiz_get_state <- function(store, variable = NULL, state = NULL){
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

#' @describeIn quiz_get_state a setter function for the state machine
quiz_set_state <- function(store, variable, value, state = NULL){
  
  if (is.null(state)) state <- quiz_get_state(store)
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

#' @describeIn quiz_get_state Backup function to check that an answer matches a response, agnostic of ordering
quiz_is_answer_correct <- function(answer, key){
  # TODO: deprecated?
  if (length(answer) != length(key)) return(FALSE)
  if(!is.numeric(answer)) is_correct <- purrr::map2_lgl(answer, key, function(resp, key) base::setequal(resp, key))
  if(is.numeric(answer)) is_correct <-  purrr::map2_lgl(answer, key, function(resp, key) dplyr::between(resp, key-.1, key+.1))
  is_correct <- isTRUE(all(is_correct))
  return(is_correct)
}

#' @describeIn quiz_get_state check that current-response is correct
quiz_is_current_correct <- function(store){
  current_response <- unname(quiz_get_state(store, variable = 'current-response'))
  current_correct_answer <- unname(quiz_get_state(store, variable = 'current-correct-answer'))
  
  # if there is a grader function, use it. Otherwise use the generic one defined above
  current_grader <- quiz_get_state(store, 'current-grader')
  if (is_truthy(current_grader)){
    is_correct <- current_grader(current_response)
  } else {
    # TODO: deprecated?
    is_correct <- quiz_is_answer_correct(current_response, current_correct_answer)
  }
  return(isTRUE(is_correct))
}

#' @describeIn quiz_get_state check that recorded answers are correct and return a boolean vector
quiz_check_is_each_correct <- function(store){
  return(store$is_correct)
}

#' @describeIn quiz_get_state check that all recorded answers are correct
quiz_is_all_correct <- function(store) {
  return(isTRUE(all(quiz_check_is_each_correct(store))))
}

#' @describeIn quiz_get_state Check if the quiz in sandbox mode
quiz_in_sandbox_mode <- function(store){
  isTRUE(quiz_get_state(store, 'sandbox-mode'))
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
format_prompts <- function(quiz){
  verify_quiz_structure(quiz)
  
  for (i in seq_along(quiz@questions)){
    quiz@questions[[i]]@prompt <- quiz_format_prompt(quiz@questions[[i]]@prompt, i)
  }
  
  return(quiz)
}

#' @describeIn format_prompts Add a header denoting the question number
quiz_format_prompt <- function(prompt, i){
  htmltools::div(
    htmltools::h4("Practice what you've learned"),
    htmltools::hr(),
    htmltools::h3(glue::glue("Question {i}")), # h3 required for checkmark/red x placement
    prompt
  )
}

#' @describeIn quiz_get_state UI to show once the quiz is completed
quiz_ui_quiz_complete <- function(store, ns, message_correct, message_wrong, message_skipped){
  
  # render ending message based on if answers are correct
  all_correct <- quiz_is_all_correct(store)
  is_skipped <- quiz_get_state(store, variable = 'quiz-skipped')
  
  if (is_skipped){
    html_content <- htmltools::tagList(
      htmltools::br(), 
      add_message_skipped(message_skipped)
    )
  } else if (all_correct) {
    html_content <- htmltools::tagList(
      htmltools::br(),
      add_message_correct(message_correct),
      add_confetti()
    )
  } else {
    html_content <- htmltools::tagList(
      htmltools::br(), 
      add_message_wrong(message_wrong)
    )
  }
  
  # render the report table
  grade_report <- quiz_ui_quiz_complete_report(store)
  
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

#' @describeIn quiz_get_state Quiz score and table of correct answers to show at the end
quiz_ui_quiz_complete_report <- function(store){
  
  in_sandbox <- quiz_in_sandbox_mode(store)
  
  # grade answers and convert into icons
  icon_right <- shiny::icon('check') |> as.character()
  icon_wrong <- shiny::icon('times') |> as.character()
  answers <- quiz_check_is_each_correct(store)
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
  
  # put everything in a table
  grade_tbl <- tibble::tibble(
    icon = answers_icons,
    label = question_label,
    `Your Answer` = answers_user_print,
    `Correct Answer` = answers_correct_print
  ) |>
    dplyr::filter(`Your Answer` != skip_label) |>
    reactable::reactable(
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

#' @describeIn quiz_get_state UI to show for each question
quiz_ui_question <- function(store, ns){
  
  # render the questions
  html_content <- htmltools::tagList(
    
    # question content
    quiz_get_state(store, 'current-question'),
    
    # button to submit answer
    shiny::actionButton(
      inputId = ns('submit_button'),
      label = 'Submit',
      class = 'submit-button'
    ),
    
    # button to skip quiz
    shiny::actionButton(
      inputId = ns('skip_button'),
      label = ifelse(quiz_in_sandbox_mode(store), 'Finish quiz', 'Skip quiz'),
      class = 'skip-button'
    )
  )
  
  return(html_content)
}

#' Create quasi infinite quiz by resampling questions n times
#'
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
resample_questions_if_sandbox <- function(quiz, sandbox_mode, n = 50){
  if (!(is.numeric(n) && n > 0)) cli::cli_abort('n must be positive integer')
  verify_quiz_structure(quiz)
  
  if (isTRUE(sandbox_mode)){
    indices <- sample(seq_along(quiz@questions), size = n, replace = TRUE)
    quiz@questions <- quiz@questions[indices]
  }
  
  return(quiz)
}
