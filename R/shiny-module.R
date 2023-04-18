### shiny module that implements the state machine ###

#' quiz module
#'
#' @description A shiny Module to implement the quiz
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @author Joseph Marlo
mod_quiz_ui <- function(id){
  ns <- shiny::NS(id)
  htmltools::tagList(
    shinyjs::useShinyjs(),
    htmltools::includeCSS(system.file('shinyQuiz.css', package = "shinyQuiz")),
    htmltools::div(
      id = ns('quiz-container'),
      class = 'quiz-container',
      shiny::uiOutput(outputId = ns('UI_quiz'))
    )
  )
}

#' @describeIn mod_quiz_ui Server side function
mod_quiz_server <- function(id, id_parent = character(0), questions, message_correct, message_wrong, message_skipped, embed_quiz = TRUE, sandbox_mode = FALSE){
  shiny::moduleServer( id, function(input, output, session){
    # ns <- session$ns
    ns <- shiny::NS(shiny::NS(id_parent)(id))
    
    # message(paste0('The quiz module has a namespace id of: ', id))
    
    # add css class to the quiz container if embedding
    # TODO: keep this embedding mode?
    if (isTRUE(embed_quiz)) shinyjs::addClass(id = 'quiz-container', class = 'quiz-embedded')
    
    # resample the questions if in sandbox mode
    # TODO: move to function
    if (isTRUE(sandbox_mode)){
      # number of questions
      n <- 50L
      
      # sample indices for replicating the questions
      indices <- sample(seq_along(questions), size = n, replace = TRUE)
      questions <- questions[indices]
      # question_prompts <- question_prompts[indices]
      # correct_answers <- correct_answers[indices]
    }
    
    # add headers to question texts
    # TODO: move to function
    # question_texts <- quiz_format_question_texts(questions)
    for (i in seq_along(questions)){
      questions[[i]]@question <- quiz_format_question_text(questions[[i]]@question, i)
    }
    
    # set the current state and potential values
    store <- shiny::reactiveValues(
      state = 'quiz-question-1',
      states = c(paste0('quiz-question-', seq_along(questions)), 'quiz-complete'),
      questions = questions,
      is_correct = rep(FALSE, length(questions)),
      ui_html = NULL,
      skipped = FALSE,
      sandbox_mode = isTRUE(sandbox_mode)
    )
    
    # reset quiz
    shiny::observeEvent(input$restart_button, {
      # reset the state to the first question
      store <- quiz_set_state(store, variable = 'current-state', value = 'quiz-question-1')
      
      # remove any responses
      # store$responses <- rep(NA, length(questions) + 1)
      store$questions <- questions
      store <- quiz_set_state(store, variable = 'quiz-skipped', value = FALSE)
      store$is_correct <- rep(FALSE, length(questions))
    })
    
    # skip quiz / finish quiz
    shiny::observeEvent(input$skip_button, {
      store <- quiz_set_state(store, variable = 'current-state', value = 'quiz-complete')
      store <- quiz_set_state(store, variable = 'quiz-skipped', value = TRUE)
    })
    
    # control state behavior
    shiny::observeEvent(store$state, {
      
      # scroll to top of quiz container
      scroll_to_div(ns = ns, id = 'quiz-container')
      
      # make non-quiz content visible (may be re-hidden depending on final state)
      # shinyjs::show(selector = paste0('.', shiny::NS(id_parent)('learning-content')), asis = TRUE)
      
      # state behavior
      if (store$state == 'quiz-complete'){
        # determine the UI
        store$ui_html <- quiz_ui_quiz_complete(
          store,
          ns = ns,
          message_correct = message_correct,
          message_wrong = message_wrong,
          message_skipped = message_skipped
        )
        
        # unblur the text
        # TODO: remove this functionality
        # shinyjs::removeClass(selector = paste0('.', shiny::NS(id_parent)('learning-content-blur')),
        #                      asis = TRUE,
        #                      class = 'learning-content-blur')
        
      } else {
        # determine the UI
        store$ui_html <- quiz_ui_question(store, ns = ns)
        
        # hide non-quiz content
        # TODO: remove this functionality
        # if (!isTRUE(embed_quiz)){
        #   shinyjs::hide(selector = paste0('.', shiny::NS(id_parent)('learning-content')), asis = TRUE)
        # }
      }
    })
    
    # on button submit, record answer and change the state
    shiny::observeEvent(input$submit_button, {
      
      # disable submit button to prevent double clicks
      shinyjs::disable(id = 'submit_button')
      
      # scroll to top of quiz container
      scroll_to_div(ns = ns, id = 'quiz-container')
      
      # record answers
      store <- quiz_set_state(store, variable = 'current-response', value = input$answers)
      
      # is the answer correct and record it
      is_correct <- quiz_is_current_correct(store)
      store <- quiz_set_state(store, 'current-correct', is_correct)
      
      # grade it
      delay_in_ms <- 2000
      if (is_correct){
        # add UI indicator
        add_checkmark(ns = ns, id = 'quiz-container', element = 'h3')
        
        # change the state
        shinyjs::delay(delay_in_ms, {
          new_state <- quiz_get_state(store, variable = 'next-state')
          store <- quiz_set_state(store, variable = 'current-state', value = new_state)
        })
        
      } else {
        # add UI indicator
        add_red_x(ns = ns, id = 'quiz-container', element = 'h3')
        
        # change the state
        # if in sandbox mode, go to next question otherwise end here
        shinyjs::delay(delay_in_ms, {
          if (quiz_in_sandbox_mode(store)){
            new_state <- quiz_get_state(store, variable = 'next-state')
            store <- quiz_set_state(store, variable = 'current-state', value = new_state)
          } else {
            store <- quiz_set_state(store, variable = 'current-state', value = 'quiz-complete')
          }
        })
      }
    })
    
    # render the UI
    output$UI_quiz <- shiny::renderUI(store$ui_html)
  })
}