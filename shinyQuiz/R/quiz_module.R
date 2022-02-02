require(shiny)

ui_quiz <- function(id) {
  ns <- NS(id)
  uiOutput(outputId = ns('quiz'))
}

server_quiz <- function(id, questions, answers, correct_answers, ui_background) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      validate(need(length(questions) == length(answers),
                    'length(questions) must equal length(answers)'))
      
      # set the current state and potential values
      store <- reactiveValues(
        state = 'quiz-question-1',
        states = c(paste0('quiz-question-', seq_along(questions)), 'quiz-complete'),
        questions = questions,
        answers = answers,
        correct_answers = correct_answers,
        responses = rep(NA, length(questions) + 1)
      )
      
      # on button submit, manage the state
      observeEvent(input$submit_button, {
        
        # record answers
        store <- set_state(store, variable = 'current-response', value = input$answer_buttons)
        
        # is the answer correct
        is_correct <- isTRUE(
          get_state(store, variable = 'current-response') == get_state(store, variable = 'current-correct-answer')
        )
        
        # change the state
        if (is_correct){
          store$state <- get_state(store, 'next-state')
        } else {
          store$state <- 'quiz-complete'
        }
      })
      
      # reset quiz
      observeEvent(input$restart_button, {
        store <- set_state(store, variable = 'current-state', value = 'quiz-question-1')
        store$responses <- rep(NA, length(questions) + 1)
      })
      
      # render the UI
      output$quiz <- renderUI({
        
        if (get_state(store) == 'quiz-complete'){
          html <- ui_background
          
          # add confetti if all answers were correct
          all_correct <- is_all_correct(store)
          if (all_correct) html <- tagList(html, add_confetti())
          
        } else {
          
          # render the questions
          html <- tagList(
            p(get_state(store, 'current-question')),
            radioButtons(
              inputId = ns('answer_buttons'),
              label = NULL,
              choices = get_state(store, 'current-answers'),
              selected = NULL
            ),
            actionButton(inputId = ns('submit_button'),
                         label = 'Submit',
                         class = 'submit-button')
          )
          
          # wrap html in a div
          html <- div(class = 'quiz-container',
                      html)
        }
        
        # add the restart button
        html <- tagList(
          html,
          actionButton(inputId = ns('restart_button'),
                       label = 'Restart quiz',
                       class = 'restart-button')
        )
        
        return(html)
      })
    }
  )
}
