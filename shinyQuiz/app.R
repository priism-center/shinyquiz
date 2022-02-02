library(shiny)

# TODO: rethink ui_background implementation? ns gets tricky

# Quiz progress until a wrong answer is made is the end of the questions

# set the basic elements of the quiz
question_1 <- "This is question 1"
question_2 <- "This is question 2"
questions <- list(question_1, question_2)
answers <- list(c('1a', '1b'), c('2a', '2b'))
correct_answers <- list(c('1a'), c('2b'))

# the UI to show once the quiz is finished
ui_background <- tagList(
    p('Quiz finished'),
    sliderInput(inputId = NS('test')('slider_test'),
                label = 'Another UI element',
                min = 0,
                max = 1,
                value = 0.5),
    textOutput(outputId = NS('test')('text_test'))
)

# main UI
ui <- fluidPage(ui_quiz(id = 'test'))

# main server
server <- function(input, output, session) {
    ns_id <- 'test'
    ns <- NS(ns_id)
    
    server_quiz(
        id = ns_id,
        questions = questions,
        answers = answers,
        correct_answers = correct_answers,
        ui_background = ui_background
    )
    
    output[[ns('text_test')]] <- renderText({
        input[[ns('slider_test')]]
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
