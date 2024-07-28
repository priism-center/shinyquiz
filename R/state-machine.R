### core logic for controlling the quiz ###

#' Functions for managing the quiz state machine
#'
#' The quiz is implemented via a state machine framework. It has states for each question and a final state for once the quiz ends. Only one state can be active at a time. The question text and answers shown depend on which state is active.
#'
#' These are `get` and `set` functions for retrieving state values and setting values. The states are originally created via a `reactiveValues` call within a Shiny server (or `list` outside of Shiny; see example below).
#'
#' @param store a list formatted like in the example
#' @param variable one of c('current-question', 'current-correct-answer', 'next-state', 'current-response')
#' @param state one of c('quiz-question-1', ..., 'quiz-complete')
#' @keywords internal
#' @noRd
#' @return depends on function
#'
#' @author Joseph Marlo
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
#'   )
#' )
#' store <- sm_create_reactive_store(quiz)
#' sm_get_state(store)
#' sm_get_state(store, 'next-state')
#' sm_get_state(store, 'current-question')
#' sm_check_is_each_correct(store)
#' sm_quiz_in_sandbox_mode(store)
#' @describeIn sm_get_state Getter function for the state machine
sm_get_state <- function(store, variable = NULL, state = NULL){
  if (is.null(state)) state <- store$state
  if (is.null(variable)) return(state)
  if (!(state %in% store$states)) cli::cli_abort('state not in store$states')
  
  if (variable == 'current-question'){
    return(store$questions[store$states == state][[1]]@prompt)
  }
  # if (variable == 'current-answers'){
  #   return(store$question_prompts[store$states == state][[1]])
  # }
  if (variable == 'current-correct-answer'){
    # return(store$correct_answers[store$states == state][[1]])
    return(store$questions[store$states == state][[1]]@answerCorrectPretty)
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
  # TODO: is this tested?
  if (variable == 'current-feedback'){
    return(store$questions[store$states == state][[1]]@answerFeedback)
  }
  
  cli::cli_abort('Variable or state not found')
}

#' @keywords internal
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
  
  # TODO: adjusted here
  if (variable == 'current-feedback'){
    store$questions[[which(store$states == state)]]@answerFeedback[[1]] <- value
  }
  
  return(store)
}

#' @keywords internal
#' @describeIn sm_get_state Check that current-response is correct
sm_is_current_correct <- function(store){
  # get the response
  current_response <- unname(sm_get_state(store, variable = 'current-response'))
  current_correct_answer <- unname(sm_get_state(store, variable = 'current-correct-answer'))
  
  # get the grader function
  current_grader <- sm_get_state(store, 'current-grader')
  current_grader <- purrr::possibly(current_grader, otherwise = FALSE)
  
  # grade it
  is_correct <- isTRUE(current_grader(current_response))
  
  return(is_correct)
}

#' @keywords internal
#' @describeIn sm_get_state Check that each recorded answer is correct and return a boolean vector
sm_check_is_each_correct <- function(store){
  answers <- store$is_correct
  answers[is.na(answers)] <- FALSE
  return(answers)
}

#' @keywords internal
#' @describeIn sm_get_state Check that all recorded answers are correct
sm_is_all_correct <- function(store) {
  return(isTRUE(all(sm_check_is_each_correct(store))))
}

sm_get_current_feedback <- function(store){
  quiz@questions[[1]]@choices[[1]]@feedback
  
}

sm_get_all_feedback <- function(store){
  
}

#' @keywords internal
#' @describeIn sm_get_state Check if the quiz in sandbox mode
sm_quiz_in_sandbox_mode <- function(store){
  isTRUE(sm_get_state(store, 'sandbox-mode'))
}

#' @keywords internal
#' @describeIn sm_get_state Check if the quiz is complete
sm_quiz_is_complete <- function(store){
  isTRUE(sm_get_state(store) == 'quiz-complete')
}

#' @keywords internal
#' @describeIn sm_get_state Check if the quiz should end early if user fails a question
sm_logic_end_on_first_wrong <- function(store){
  isTRUE(store$quiz_obj@options$logic_end_on_first_wrong)
}

#' @keywords internal
#' @describeIn sm_get_state Add headers containing the question number to all the questions in a quiz
sm_ui_format_prompts <- function(quiz){
  verify_quiz_structure(quiz)
  
  for (i in seq_along(quiz@questions)){
    quiz@questions[[i]]@prompt <- sm_ui_format_prompt(
      quiz@questions[[i]]@prompt, 
      i, 
      quiz@options$question_heading,
      quiz@options$include_question_title
    )
  }
  
  return(quiz)
}

#' @keywords internal
#' @describeIn sm_get_state Add a header denoting the question number
sm_ui_format_prompt <- function(prompt, i, question_heading, include_question_title){
  htmltools::div(
    htmltools::div(
      class = 'quiz-header',
      htmltools::h4(question_heading),
      htmltools::hr(),
      {if (isTRUE(include_question_title)) htmltools::h3(glue::glue("Question {i}"))} # h3 required for checkmark/red x placement
    ),
    htmltools::div(
        class = 'quiz-prompt',
        prompt
    )
  )
}

#' @keywords internal
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

#' @keywords internal
#' @describeIn sm_get_state UI to show the score and table of correct answers to display at the end
sm_ui_complete_report <- function(store){
  
  # grade answers and convert into icons
  icon_right <- fontawesome::fa_i('check', verify_fa = FALSE) |> as.character()
  icon_wrong <- fontawesome::fa_i('times', verify_fa = FALSE) |> as.character()
  answers <- sm_check_is_each_correct(store)
  answers_icons <- c(icon_wrong, icon_right)[answers + 1]
  
  # format question labels
  question_label <- paste0('Question ', seq_along(store$questions))
  
  # calculate score and format the score
  # if in sandbox mode, score is only for non skipped items
  answers_user_print <- purrr::map(store$questions, ~{
    printer <- purrr::possibly(.x@answerUserPrettifier, otherwise = '[Unable to print user response]')
    printer(.x@answerUser[[1]])
  })
  score <- sm_score_quiz(store)
  score <- scales::percent_format()(score)
  
  # add skipped label to skipped questions
  q_not_answered <- is.na(store$is_correct)
  skip_label <- '[skipped]'
  answers_user_print[q_not_answered] <- skip_label
  
  # get formatted correct answers
  answers_correct_print <- purrr::map_chr(store$questions, ~.x@answerCorrectPretty)
  answers_correct_print[q_not_answered] <- skip_label
  
  # TODO: get feedback
  # browser()
  user_feedback <- purrr::map_chr(store$questions, ~.x@answerFeedback)
  user_feedback[q_not_answered] <- skip_label
  
  # put everything in a table
  grade_tbl <- data.frame(
    icon = answers_icons,
    label = question_label,
    your_answer = unlist(answers_user_print),
    correct_answer = answers_correct_print,
    feedback = user_feedback
  )

  # remove skipped rows if in sandbox mode
  if (sm_quiz_in_sandbox_mode(store)){
    grade_tbl <- grade_tbl[grade_tbl$your_answer != skip_label,]
  }
  
  # convert to reactable
  grade_tbl <- reactable::reactable(
    grade_tbl,
    columns = list(
      icon = reactable::colDef(name = '', html = TRUE, width = 40),
      label = reactable::colDef(name = '', width = 115),
      your_answer = reactable::colDef(name = 'Your Answer', align = 'right'),
      correct_answer = reactable::colDef(name = 'Correct Answer', align = 'right'),
      feedback = reactable::colDef(name = 'Feedback', align = 'left')
    )
    # details = function(index) {
    #   htmltools::div(
    #     style = 'margin: 0.5rem 2rem 2rem 2rem; padding: 0.5rem 1rem 1rem 1rem; background: rgba(230, 230, 230, 1);',
    #     # sm_get_state(store, 'current-question', glue::glue('quiz-question-{index}'))
    #     store$quiz_obj@questions[[index]]@prompt
    #   )
    # }
  )
  
  # add score to top of table
  grade_report <- htmltools::tagList(
    htmltools::br(),
    htmltools::h4(glue::glue('Score: {score}')),
    grade_tbl
  )
  
  return(grade_report)
}

#' @keywords internal
#' @describeIn sm_get_state UI to show for the current question
sm_ui_question <- function(store, ns){
  
  # render the questions
  html_content <- htmltools::tagList(
    
    sm_show_progress(store),
    
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

#' @keywords internal
#' @describeIn sm_get_state Show the progress bar if stipulated
sm_show_progress <- function(store){
  quiz_options <- store$quiz_obj@options
  progress_bar <- htmltools::tagList()
  
  # show_progress_and_not_sandbox <- isTRUE(quiz_options$show_progress) && !quiz_options$sandbox
  show_progress <- isTRUE(quiz_options$progress_bar)
  if(show_progress){
    current_question <- which(store$state == store$states) - 1
    total_questions <- length(store$states) - 1
    progress_percent <- current_question / total_questions
    progress_bar <- add_progress_bar(progress_percent, bg_color = quiz_options$progress_bar_color)
  }
  
  return(progress_bar)
}

#' @keywords internal
#' @param quiz an object of class 'quiz'
#' 
#' @seealso [shiny::reactiveValues()]
#'
#' @return reactiveValues
#' @author Joseph Marlo
#' 
#' @describeIn sm_get_state Create the main store object that handles the state(s)
sm_create_reactive_store <- function(quiz){
  verify_quiz_structure(quiz)
  
  # use a static list if not in reactive context (i.e. Shiny)
  list_fn <- shiny::reactiveValues
  if (isFALSE(shiny::isRunning())) list_fn <- base::list
  
  store <- list_fn(
    state = 'quiz-question-1',
    states = c(paste0('quiz-question-', seq_along(quiz@questions)), 'quiz-complete'),
    questions = quiz@questions,
    is_correct = rep(NA, length(quiz@questions)),
    ui_html = NULL,
    skipped = FALSE,
    sandbox_mode = quiz@options$sandbox,
    quiz_obj = quiz
  )
  
  return(store)
}

#' @keywords internal
#' @describeIn sm_get_state Calculate the percent of questions correct
sm_score_quiz <- function(store){
  # if not in sandbox mode, then NAs should be treated as incorrect

  in_sandbox <- sm_quiz_in_sandbox_mode(store)
  answers <- store$is_correct
  
  score <- ifelse(
    in_sandbox,
    mean(answers, na.rm = TRUE), # this excludes NAs from the calculation
    mean(local({answers[is.na(answers)] <- FALSE; answers})) # this recodes NAs as FALSE
  )
  
  if(is.na(score)) score <- 0
  
  return(score)
}

#' @keywords internal
#' @describeIn sm_get_state Generate a summary of the quiz to output from the Shiny module
sm_summary <- function(store, quiz){
  
  score <- sm_score_quiz(store)
  list(
    score = score,
    score_pretty = scales::percent_format()(score),
    which_correct = store$is_correct,
    skipped_quiz = store$skipped,
    quiz_complete = sm_quiz_is_complete(store),
    quiz_orig_obj = quiz
  )
}
