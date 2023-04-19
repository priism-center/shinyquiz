### full working example app ###

# purrr::walk(list.files('R', full.names = TRUE), source)
devtools::load_all()

library(shiny)


# content -----------------------------------------------------------------

# TODO: can the namespace be handled in a more user-friendly way?
ns_quiz <- shiny::NS('quiz')

# set the text for question 1
question_text_1 <- htmltools::div(
  htmltools::p("You’re tasked with determining if omega-3 fish oil supplements cause a decrease in blood pressure over a 6 month period. You have data from an experiment where participants were randomly assigned to take omega-3 fish oil supplements or a placebo supplement for 6 months. Besides the treatment variable (fish_oil) and the outcome variable (bp_6month) you have the following covariates: "),
  htmltools::tags$ul(
    htmltools::tags$li('Blood pressure measured at the start of the study (bp_baseline)'),
    htmltools::tags$li('Blood pressure measured  3 months into the study (bp_3month)'),
    htmltools::tags$li('Sex measured at the start of the study (sex)'),
    htmltools::tags$li('Height measured at the start of the study (height)')
  ),
  htmltools::p("Which covariates would you include in your analysis? Use the drag-drop below to move variables into the include or exclude bins."),
  
  # set the UI elements for question 1
  sortable::bucket_list(
    header = "Drag the variables to their respective roles",
    group_name = ns_quiz('answers'), # NOTE: this should be 'answers' for mod_quiz to recognize it
    orientation = "horizontal",
    class = 'default-sortable sortable-wide',
    sortable::add_rank_list(
      input_id = ns_quiz('answers_variables'),
      text = "Available",
      labels = c('bp_baseline','bp_3month', 'sex', 'height'),
      options = sortable::sortable_options(multiDrag = TRUE)
    ),
    sortable::add_rank_list(
      input_id = ns_quiz('answers_include'),
      text = "Control for",
      labels = NULL,
      options = sortable::sortable_options(multiDrag = TRUE)
    ),
    sortable::add_rank_list(
      input_id = ns_quiz('answers_treatment'),
      text = "Treatment",
      labels = c('fish_oil'),
      options = sortable::sortable_options(disabled = TRUE)
    ),
    sortable::add_rank_list(
      input_id = ns_quiz('answers_outcome'),
      text = "Outcome",
      labels = c('bp_6month'),
      options = sortable::sortable_options(disabled = TRUE)
    )
  )
)
# preview: htmltools::html_print(question_text_1)

# function to check the answers
grader_1 <- function(user_response){
  
  # its best to catch any errors in these graders
  is_correct <- tryCatch({
    # set the correct answers here
    correct_answers <- list(
      c('bp_3month'),
      c('bp_baseline', 'sex', 'height'),
      c('fish_oil'),
      c('bp_6month')
    )
    
    # this structure is a result of input$'answers' where sortable returns 4 lists
    all_true <- all(
      setequal(user_response[[1]], correct_answers[[1]]),
      setequal(user_response[[2]], correct_answers[[2]]),
      setequal(user_response[[3]], correct_answers[[3]]),
      setequal(user_response[[4]], correct_answers[[4]])
    )
    
    if (isTRUE(all_true)){
      return(TRUE)
    } else {
      return(FALSE)
    }
  },
  error = function(e) return(FALSE)
  )
  
  return(is_correct)
}

answerUserDisplay_1 <- function(x) {
  tryCatch(
    paste0(x[[2]], collapse = ', '),
    error = function(e) 'Cannot print user response'
  )
}
answerCorrectDisplay_1 <- paste0(c('bp_baseline', 'sex', 'height'), collapse = ', ')

# format into a forma question
question_1 <- construct_question(question_text_1, answerUserDisplay_1, answerCorrectDisplay_1, grader_1)



# question 2 --------------------------------------------------------------

question_text_2 <- htmltools::div(
  htmltools::p("Variable X is the only confounder. Given the overlap of X, select all the estimands we could estimate without violating the overlap assumption."),
  htmltools::p("Filler text 1"),
  
  # shiny::renderImage({
  #   list(src = app_sys('app', 'www/learn/estimands2/plots/p21.png'),
  #        contentType = 'image/png',
  #        width = 600,
  #        height = 400)
  # }, deleteFile = F),
  
  shiny::selectInput(
    inputId = ns_quiz('answers'),
    label = NULL,
    choices = c('', 'ATE', 'ATT', 'ATC'),
    selected = NULL
  )
)


answerUserDisplay_2 <- function(x) {
  tryCatch(
    paste0(x[[2]], collapse = ', '),
    error = function(e) 'Cannot print user response'
  )
}
answerCorrectDisplay_2 <- 'test2'
grader_2 <- function(x) TRUE

# format into a forma question
question_2 <- construct_question(question_text_2, answerUserDisplay_2, answerCorrectDisplay_2, grader_2)



# question 3 --------------------------------------------------------------
question_text_3 <- htmltools::div(
  htmltools::p("Variable X is the only confounder. Given the overlap of X, select all the estimands we could estimate without violating the overlap assumption."),
  htmltools::p("Filler text 1"),
  
  # shiny::renderImage({
  #   list(src = app_sys('app', 'www/learn/estimands2/plots/p22.png'),
  #        contentType = 'image/png',
  #        width = 600,
  #        height = 400)
  # }, deleteFile = F),
  
  shiny::selectInput(
    inputId = ns_quiz('answers'),
    label = NULL,
    choices = c('', 'ATE', 'ATT', 'ATC'),
    selected = NULL
  )
)

answerUserDisplay_3 <- function(x) {
  tryCatch(
    paste0(x[[2]], collapse = ', '),
    error = function(e) 'Cannot print user response'
  )
}
answerCorrectDisplay_3 <- 'test3'
grader_3 <- function(x) TRUE

# format into a forma question
question_3 <- construct_question(question_text_3, answerUserDisplay_3, answerCorrectDisplay_3, grader_3)

# set messages
messages <- construct_messages(
  message_correct = "Well done! You got all of them correct. Please read on to learn about the next topic.",
  message_wrong = "Hmmm, bummer! You got at least one wrong.",
  message_skipped = "Quiz skipped. You can restart it using the button below."
)

# create quiz object
quiz <- construct_quiz(c(question_1, question_2, question_3), messages)


# app ---------------------------------------------------------------------

ui <- shiny::fluidPage(
  
  htmltools::div(
    style = 'color: #fff; background: black; padding: 0.5rem; margin-bottom: 5rem; text-align: center;',
    htmltools::h1("EXAMPLE APP")
  ),
  htmltools::div(
    style = "width: 700px",
    quiz_ui(id = 'quiz')
  )
)

server <- function(input, output, session) {

  # run the quiz
  quiz_server(
    id = "quiz", # this should always be quiz
    id_parent = NULL,
    quiz = quiz,
    embed_quiz = TRUE,
    sandbox_mode = FALSE
  )
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
