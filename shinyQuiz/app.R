library(shiny)

# TODO: rethink ui_background implementation? ns gets tricky

# Quiz progress until a wrong answer is made is the end of the questions


# set quiz structure ------------------------------------------------------

# set the basic elements of the quiz
question_1 <- tagList(
    h3("Question 1"),
    p("You’re tasked with determining if omega-3 fish oil supplements cause a decrease in blood pressure over a 6 month period. You have data from an experiment where participants were randomly assigned to take omega-3 fish oil supplements or a placebo supplement for 6 months. Besides the treatment variable (fish_oil) and the outcome variable (bp_6month) you have the following covariates: "),
    tags$ul(
        tags$li('Blood pressure measured at the start of the study (bp_baseline)'),
        tags$li('Blood pressure measured  3 months into the study (bp_3month)'),
        tags$li('Sex measured at the start of the study (sex)'),
        tags$li('Height measured at the start of the study (height)')
    ),
    p("Which covariates would you include in your analysis? Use the drag-drop below to move variables into the include or exclude bins. When your answer is ready click ‘Fit Model’!")
)
answers_1 <- c('1a', '1b')
correct_answer_1 <- c('1a')
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
answers_2 <- c('2a', '2b')
correct_answer_2 <- c('2b')
questions <- list(question_1, question_2)
answers <- list(answers_1, answers_2)
correct_answers <- list(correct_answer_1, correct_answer_2)


# UI ----------------------------------------------------------------------

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
ui <- fluidPage(includeCSS('www/quiz.css'), ui_quiz(id = 'test'))


# server ------------------------------------------------------------------

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
