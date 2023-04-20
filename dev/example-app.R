### fully working example app ###

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

# format into a formal question
question_1 <- construct_question(
  prompt = question_text_1,
  answerUserDisplay = function(x) paste0(x[[2]], collapse = ', '),
  answerCorrectDisplay = paste0(c('bp_baseline', 'sex', 'height'), collapse = ', '),
  grader = grader_1
)
# preview_question(question_1)


# question 2 --------------------------------------------------------------

question_text_2 <- htmltools::div(
  htmltools::p("Variable X is the only confounder. Given the overlap of X, select all the estimands we could estimate without violating the overlap assumption."),
  htmltools::p("Filler text 1"),
  
  shiny::renderPlot(plot(rnorm(10), rnorm(10))),
  
  shiny::selectInput(
    inputId = ns_quiz('answers'),
    label = NULL,
    choices = c('', 'ATE', 'ATT', 'ATC'),
    selected = NULL
  )
)
# preview: htmltools::html_print(question_text_2)

# format into a formal question
question_2 <- construct_question(
  prompt = question_text_2,
  answerUserDisplay = function(x) x,
  answerCorrectDisplay = 'ATE',
  grader = function(x) x == 'ATE'
)


# question 3 --------------------------------------------------------------
question_text_3 <- htmltools::div(
  htmltools::p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
  htmltools::p("Tristique et egestas quis ipsum suspendisse ultrices gravida. Adipiscing enim eu turpis egestas pretium aenean pharetra. Urna porttitor rhoncus dolor purus. Turpis egestas pretium aenean pharetra magna. Proin fermentum leo vel orci. Volutpat odio facilisis mauris sit."),
  
  shiny::selectInput(
    inputId = ns_quiz('answers'),
    label = 'Pick any',
    choices = c('', 'lorem', 'ipsum', 'dolem'),
    selected = NULL
  )
)

# format into a formal question
question_3 <- construct_question(
  prompt = question_text_3,
  answerUserDisplay = function(x) paste0(x[[2]], collapse = ', '),
  answerCorrectDisplay = 'test3',
  grader = function(x) TRUE
)

# create quiz object
quiz <- construct_quiz(
  question_1, 
  question_2, 
  question_3, 
  options = set_quiz_options(sandbox = TRUE)
)
# preview_quiz(quiz)


# app ---------------------------------------------------------------------

ui <- shiny::fluidPage(
  
  htmltools::div(
    style = 'color: #fff; background: black; padding: 0.5rem; margin-bottom: 5rem; text-align: center;',
    htmltools::h1("EXAMPLE APP")
  ),
  htmltools::div(
    style = "max-width: 700px",
    quiz_ui(id = 'quiz')
  )
)

server <- function(input, output, session) {

  # run the quiz
  quiz_server(
    id = "quiz", # TODO: this should always be quiz? does it need to match ns_quiz()?
    id_parent = NULL,
    quiz = quiz
  )
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
