### fully working example app ###

devtools::load_all()


# content -----------------------------------------------------------------

ns_quiz <- shiny::NS('quiz')

quiz <- create_quiz(
  create_question(
    'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed id ornare augue, fringilla molestie metus. Donec eget tortor tincidunt, sagittis dui volutpat, finibus est. Select nulla.',
    add_choice('Nulla vel'),
    add_choice('auctor nulla'),
    add_choice('nulla', correct = TRUE)
  ),
  create_question(
    'Molestie metus. Maecenas tincidunt maximus viverra. Sed non gravida quam. Phasellus at iaculis leo. Mauris congue aliquet dui, ut dapibus lorem porttitor sed.',
    add_choice(500),
    add_choice('600', correct = TRUE),
    add_choice('six hundred'),
    type = 'multiple',
    label = 'Select 600'
  ),
  create_question(
    'Sed non gravida quam. Phasellus at iaculis leo.',
    add_slider(10, 50, 30, correct = 20),
    label = 'Select 20'
  ),
  create_question(
    htmltools::div(
      htmltools::p("A custom question with a plot"),
      shiny::renderPlot(plot(rnorm(100), rnorm(100))),
    ),
    add_slider(10, 50, 30, correct = 20),
    label = 'Select 20'
  ),
  create_question_raw(
    htmltools::div(
      htmltools::p("A custom sortable question"),
      sortable::bucket_list(
        header = "This is a bucket list. You can drag items between the lists.",
        sortable::add_rank_list(
          text = "Drag 'a' and 'b' from here",
          labels = c("a", "b", "c")
        ),
        sortable::add_rank_list(
          input_id = ns_quiz('answers'),
          text = "to here",
          labels = NULL
        )
      )
    ), 
    grader = \(x) setequal(x, c('a', 'b')),
    correct_answer_pretty = 'a, b'
  )
  # create_question_sandbox(
  #   \() {
  #     number <- round(rnorm(1, 30, 10), 0)
  #     rand_prompt <- paste('Is', number, 'an even number?')
  #     
  #     # using create_question inside the function helps to ensure correct class
  #     q <- create_question(prompt = rand_prompt,
  #                          add_choice('Yes, it is even', correct = number%%2 == 0),
  #                          add_choice('No, it is odd', correct = number%%2 != 0))
  #     
  #     return(q)
  #   }
  # )
  # options = set_quiz_options(sandbox = TRUE)
  # options = set_quiz_options(end_on_first_wrong = FALSE)
)


# app ---------------------------------------------------------------------

preview_app(quiz)

