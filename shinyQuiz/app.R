library(shiny)

question_1 <- "This is question 1"
question_2 <- "This is question 2"
questions <- list(question_1, question_2)
answers <- list(c('1a', '1b'), c('2a', '2b'))
correct_answers <- list(c('1a'), c('2b'))

ui_background <- tagList(
    p('Quiz finished'),
    sliderInput(inputId = 'test',
                label = 'Another UI element',
                min = 0,
                max = 1,
                value = 0.5)
)

ui <- fluidPage(
    
    includeCSS('www/quiz.css'),

    tagList(
        uiOutput(outputId = 'quiz'),
        actionButton(inputId = 'reload_button',
                     label = 'Reload')
    )

)

server <- function(input, output, session) {

    # set the current state and potentian values
    store <- reactiveValues(
        state = 'quiz-question-1',
        states = c(paste0('quiz-question-', seq_along(questions)), 'quiz-complete'),
        questions = questions,
        answers = answers,
        correct_answers = correct_answers
    )
    
    # on submit, manage the state
    observeEvent(input$submit_button, {
        
        # is the answer correct
        is_correct <- isTRUE(
            input$answer_buttons == get_state(store, variable = 'current-correct-answer')
        )
        
        # change the state
        if (is_correct){
            store$state <- get_state(store, 'next-state')
        } else {
            store$state <- 'quiz-complete'
        }
    })
    
    # reset quiz
    observeEvent(input$reload_button, {
        store$state <- 'quiz-question-1'
    })
    
    # render the UI
    output$quiz <- renderUI({
        
        if (get_state(store) == 'quiz-complete'){
            html <- ui_background
        } else {
            html <- tagList(
                p(get_state(store, 'current-question')),
                shiny::radioButtons(
                    inputId = 'answer_buttons',
                    label = NULL,
                    choices = get_state(store, 'current-answers'),
                    selected = NULL
                ),
                actionButton(inputId = 'submit_button',
                             label = 'Submit')
            )
            
            # wrap html is a div
            html <- div(id = 'quiz-container',
                        html)
        }
        
        return(html)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
