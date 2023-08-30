question <- shinyQuiz::create_question(
  'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Select nulla.',
  shinyQuiz::add_choice('auctor'),
  shinyQuiz::add_choice('nulla', correct = TRUE)
)
quiz <- shinyQuiz::create_quiz(
  question,
  shinyQuiz::create_question(
    'Mauris congue aliquet dui, ut dapibus lorem porttitor sed. Select 600.',
    shinyQuiz::add_choice('600', correct = TRUE),
    shinyQuiz::add_choice('800')
  )
)

p_app <- shinyQuiz:::preview_app(quiz)
p_quiz <- suppressMessages(shinyQuiz:::preview_quiz(quiz))
p_question <- suppressMessages(shinyQuiz:::preview_question(question))

test_that("preview_ functions work", {
  expect_s3_class(p_app, 'shiny.appobj')
  expect_s3_class(p_quiz, 'shiny.tag.list')
  expect_s3_class(p_question, 'shiny.tag')
})
