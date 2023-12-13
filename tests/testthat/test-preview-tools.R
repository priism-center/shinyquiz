question <- shinyquiz::create_question(
  'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Select nulla.',
  shinyquiz::add_choice('auctor'),
  shinyquiz::add_choice('nulla', correct = TRUE)
)
quiz <- shinyquiz::create_quiz(
  question,
  shinyquiz::create_question(
    'Mauris congue aliquet dui, ut dapibus lorem porttitor sed. Select 600.',
    shinyquiz::add_choice('600', correct = TRUE),
    shinyquiz::add_choice('800')
  )
)

p_app <- shinyquiz:::preview_app(quiz)

test_that("preview_app function work", {
  expect_s3_class(p_app, 'shiny.appobj')
})

if (interactive()){
  p_quiz <- suppressMessages(shinyquiz:::preview_quiz(quiz))
  p_question <- suppressMessages(shinyquiz:::preview_question(question))
  test_that("preview_quiz and question functions work", {
    expect_s3_class(p_quiz, 'shiny.tag.list')
    expect_s3_class(p_question, 'shiny.tag')
  })
}
