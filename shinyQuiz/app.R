library(shiny)

# TODO: rethink ui_background implementation? ns gets tricky

# Quiz progress until a wrong answer is made is the end of the questions

# ns_id <- 'test'
# ns <- shiny::NS('test')()


# question 1 --------------------------------------------------------------

# set the basic elements of the quiz
question_1 <- tagList(
  h3("Question 1"),
  p("You’re tasked with determining if omega-3 fish oil supplements cause a decrease in blood pressure over a 6 month period. You have data from an experiment where participants were randomly assigned to take omega-3 fish oil supplements or a placebo supplement for 6 months. Besides the treatment variable (fish_oil) and the outcome variable (bp_6month) you have the following covariates: "),
  tags$ul(
    tags$li('Blood pressure measured at the start of the study (bp_baseline)'),
    tags$li('Blood pressure measured  3 months into the study (bp_3month)'),
    tags$li('Sex measured `at the start of the study (sex)'),
    tags$li('Height measured at the start of the study (height)')
  ),
  p("Which covariates would you include in your analysis? Use the drag-drop below to move variables into the include or exclude bins. When your answer is ready click ‘Fit Model’!")
)
# answers_1 <- c('1a', '1b')
question_prompt_1 <- sortable::bucket_list(
  header = "Drag the variables to their respective roles",
  group_name = shiny::NS('test')('answers'),
  orientation = "horizontal",
  class = 'default-sortable sortable-wide',
  sortable::add_rank_list(
    input_id = shiny::NS('test')('answers_variables'),
    text = strong("Variables"),
    labels = c('Hello', 'Goodbye'),
    options = sortable::sortable_options(multiDrag = TRUE)
  ),
  sortable::add_rank_list(
    input_id = shiny::NS('test')('answers_covariates'),
    text = strong("Covariates"),
    labels = NULL,
    options = sortable::sortable_options(multiDrag = TRUE)
  )
)
# answer structure must match structure provided by input$answers
correct_answer_1 <- list(c('Hello', 'Goodbye'),
                         character(0))


# question 2 --------------------------------------------------------------

question_2 <- tagList(
  h3("Question 2"),
  p("A middle school offers an optional meditation class to 8th grade students and you’re tasked with determining if the meditation class causes higher grades at the end of 8th grade. Besides the treatment variable (meditation) and the outcome variable (grades), the school provided you with several other covariates. All covariates were pulled from administrative data at the end of 8th grade. Each covariate is show below:  "),
  tags$ul(
    tags$li('6th grade grades  measured'),
    tags$li('7th grade grades'),
    tags$li('number of detentions during 8th grade '),
    tags$li('Race'),
    tags$li('Parent income at the end of 8th grade')
  ),
  p("Lorem ipsum")
)
question_prompt_2 <- shiny::checkboxGroupInput(
  inputId = shiny::NS('test')('answers'),
  label = NULL,
  choices = c('2a', '2b'),
  selected = NULL
)
correct_answer_2 <- c('2b')

# set quiz structure ------------------------------------------------------

question_texts <- list(question_1, question_2)
question_prompts <- list(question_prompt_1, question_prompt_2)
correct_answers <- list(correct_answer_1, correct_answer_2)
message_correct <- "You got all of them correct"
message_wrong <- "You got at least one wrong"


# UI ----------------------------------------------------------------------

# the UI to show once the quiz is finished
# ui div must have class .learning-content
ui_background <- div(
  class = 'learning-content',
  h3('Welcome to learning module!'),
  p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
  sliderInput(inputId = 'slider_test',
              label = 'Another UI element',
              min = 0,
              max = 1,
              value = 0.5),
  textOutput(outputId = 'text_test')
)

# main UI
ui <- fluidPage(
  includeCSS('www/quiz.css'), 
  ui_quiz(id = 'test'),
  ui_background
)


# server ------------------------------------------------------------------

# main server
server <- function(input, output, session) {
  
  # run the
  server_quiz(
    id = 'test',
    question_texts = question_texts,
    question_prompts = question_prompts,
    correct_answers = correct_answers,
    message_correct = message_correct,
    message_wrong = message_wrong
  )
  
  # example server to run outside of the quiz
  output$text_test <- renderText({
    input$slider_test
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
