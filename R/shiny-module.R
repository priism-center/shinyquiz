### shiny module that implements the state machine ###

#' Run a quiz in R Shiny
#'
#' @description A shiny Module to implement a quiz
#'
#' @param id a unique string that is identical across UI and server
#' @author Joseph Marlo
#' @export
#' @describeIn quiz_ui UI side function
quiz_ui <- function(id){
  # shouldn't this also have and id_parent argument?
  ns <- shiny::NS(id)
  htmltools::tagList(
    add_external_resources(),
    htmltools::div(
      id = ns('quiz-container'),
      class = 'quiz-container',
      shiny::uiOutput(outputId = ns('UI_quiz'))
    )
  )
}

#' @param id a unique string that is identical across UI and server
#' @param id_parent if using within a Shiny module, the id of that module
#' @param quiz TBD
#' @param embed_quiz boolean. remove?
#' @param sandbox_mode boolean. Resample questions for quasi infinite mode? 
#' @export
#' 
#' @describeIn quiz_ui Server side function
quiz_server <- function(id, id_parent = character(0), quiz, embed_quiz = TRUE, sandbox_mode = FALSE){
  
  verify_quiz_structure(quiz)
  
  shiny::moduleServer(id, function(input, output, session){
    # ns <- session$ns
    ns <- shiny::NS(shiny::NS(id_parent)(id))
    
    # message(paste0('The quiz module has a namespace id of: ', id))
    
    # add css class to the quiz container if embedding
    # TODO: keep this embedding mode?
    if (isTRUE(embed_quiz)) shinyjs::addClass(id = 'quiz-container', class = 'quiz-embedded')
    
    # resample the questions if in sandbox mode
    quiz <- sm_resample_questions_if_sandbox(quiz, sandbox_mode, n = 50)
    
    # add headers to question texts
    quiz <- sm_ui_format_prompts(quiz)
    
    # set the current state and potential values
    store <- sm_create_reactive_store(quiz, sandbox_mode)
    
    # reset quiz
    shiny::observeEvent(input$restart_button, {
      
      # reset the state to the first question
      store <- sm_set_state(store, variable = 'current-state', value = 'quiz-question-1')
      
      # remove any responses
      store$questions <- quiz@questions
      store <- sm_set_state(store, variable = 'quiz-skipped', value = FALSE)
      store$is_correct <- rep(FALSE, length(quiz@questions))
    })
    
    # skip quiz / finish quiz
    shiny::observeEvent(input$skip_button, {
      store <- sm_set_state(store, variable = 'current-state', value = 'quiz-complete')
      store <- sm_set_state(store, variable = 'quiz-skipped', value = TRUE)
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
        store$ui_html <- sm_ui_quiz_complete(
          store,
          ns = ns,
          messages = quiz@messages
        )
        
        # unblur the text
        # TODO: remove this functionality
        # shinyjs::removeClass(selector = paste0('.', shiny::NS(id_parent)('learning-content-blur')),
        #                      asis = TRUE,
        #                      class = 'learning-content-blur')
        
      } else {
        # determine the UI
        store$ui_html <- sm_ui_question(store, ns = ns)
        
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
      store <- sm_set_state(store, variable = 'current-response', value = input$answers)
      
      # is the answer correct and record it
      is_correct <- sm_is_current_correct(store)
      store <- sm_set_state(store, 'current-correct', is_correct)
      
      # grade it
      delay_in_ms <- 2000
      if (is_correct){
        # add UI indicator
        add_checkmark(ns = ns, id = 'quiz-container', element = 'h3')
        
        # change the state
        shinyjs::delay(delay_in_ms, {
          new_state <- sm_get_state(store, variable = 'next-state')
          store <- sm_set_state(store, variable = 'current-state', value = new_state)
        })
        
      } else {
        # add UI indicator
        add_red_x(ns = ns, id = 'quiz-container', element = 'h3')
        
        # change the state
        # if in sandbox mode, go to next question otherwise end here
        shinyjs::delay(delay_in_ms, {
          if (sm_quiz_in_sandbox_mode(store)){
            new_state <- sm_get_state(store, variable = 'next-state')
            store <- sm_set_state(store, variable = 'current-state', value = new_state)
          } else {
            store <- sm_set_state(store, variable = 'current-state', value = 'quiz-complete')
          }
        })
      }
    })
    
    # render the UI
    output$UI_quiz <- shiny::renderUI(store$ui_html)
  })
}

#' External resources to include in the app 
#'
#' Examples include `shinyjs::useShinyjs` or `fontawesome::fa_html_dependency`. These objects are raised to the head of html document.
#'
#' @return an invisible object of class shiny.tag
#' @noRd
#' @author Joseph Marlo
add_external_resources <- function(){
  htmltools::tags$head(
    shinyjs::useShinyjs(),
    fontawesome::fa_html_dependency(),
    htmltools::includeCSS(system.file('shinyQuiz.css', package = "shinyQuiz")),
  )
}
