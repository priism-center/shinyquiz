require(shiny)
require(shinyjs)

ui_quiz <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(outputId = ns('quiz'))
  )
}

server_quiz <- function(id, question_texts, question_prompts, correct_answers, message_correct, message_wrong) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      # validate(need(length(questions) == length(answers),
      #               'length(questions) must equal length(answers)'))
      
      # set the current state and potential values
      store <- reactiveValues(
        state = 'quiz-question-1',
        states = c(paste0('quiz-question-', seq_along(question_texts)), 'quiz-complete'),
        question_texts = question_texts,
        question_prompts = question_prompts,
        correct_answers = correct_answers,
        responses = rep(NA, length(question_texts) + 1)
      )
      
      # on button submit, manage the state
      observeEvent(input$submit_button, {
        
        # record answers
        store <- set_state(store, variable = 'current-response', value = input$answers)
        
        # print(input$answers)
        # print(store$responses)
        # print(store$correct_answers)
        
        # is the answer correct
        is_correct <- is_current_correct(store)
        
        # grade it
        if (is_correct){
          # add UI indicator
          add_checkmark(ns = ns)
          
          # change the state
          shinyjs::delay(1000, {
            new_state <- get_state(store, variable = 'next-state')
            store <- set_state(store, variable = 'current-state', value = new_state)
          })
        } else {
          # add UI indicator
          add_red_x(ns = ns)
          
          # change the state
          shinyjs::delay(1000, {
            store <- set_state(store, variable = 'current-state', value = 'quiz-complete')
          })
        }
        
      })
      
      # reset quiz
      observeEvent(input$restart_button, {
        # reset the state to the first question
        store <- set_state(store, variable = 'current-state', value = 'quiz-question-1')
        
        # remove any responses
        store$responses <- rep(NA, length(question_texts) + 1)
        
        # hide content
        shinyjs::runjs('$(".learning-content").hide()')
      })
      
      # render the UI
      output$quiz <- renderUI({
        
        if (get_state(store) == 'quiz-complete'){
          
          # render ending message and confetti
          all_correct <- is_all_correct(store)
          if (all_correct) {
            html_content <- tagList(br(),
                                    add_message_correct(message_correct),
                                    add_confetti())
          } else {
            html_content <- tagList(br(), add_message_wrong(message_wrong))
          }
          
          # make non-quiz content visible
          shinyjs::runjs('$(".learning-content").show()')
          
        } else {
          
          # render the questions
          html_content <- tagList(
            p(get_state(store, 'current-question')),
            get_state(store, 'current-answers'),
            actionButton(inputId = ns('submit_button'),
                         label = 'Submit',
                         class = 'submit-button')
          )
          
          # wrap html in a div
          html_content <- div(class = 'quiz-container',
                              html_content)
        }
        
        # add the restart button
        html_content <- tagList(
          html_content,
          br(), 
          actionButton(inputId = ns('restart_button'),
                       label = 'Restart quiz',
                       class = 'restart-button'),
          br(), hr(), br(), br()
        )
        
        return(html_content)
      })
    }
  )
}
