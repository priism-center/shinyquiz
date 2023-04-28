
test_that('choices works', {
  expect_s4_class(shinyQuiz::add_choice('my option'), 'quizChoice')
  expect_s4_class(shinyQuiz::add_slider(correct = 0.5), 'quizChoiceSlider')
  expect_s4_class(shinyQuiz::add_slider(correct = 0.5), 'quizChoiceSlider')
  
  expect_error(shinyQuiz::add_slider(correct = T))
})

test_that("create_question works", {
  expect_s4_class(shinyQuiz::create_question('My q', shinyQuiz::add_choice('this', TRUE)), 'quizQuestion')
  
  expect_error(shinyQuiz::create_question('My q', shinyQuiz::add_choice('this'), shinyQuiz::add_slider(correct = 1)))
  expect_error(shinyQuiz::create_question('My q', shinyQuiz::add_choice('this')))
})

# test_that("create_question_raw works", {
#   expect_s4_class(shinyQuiz::create_question('My q', shinyQuiz::add_choice('this', TRUE)), 'quizQuestion')
#   expect_error(shinyQuiz::create_question('My q', shinyQuiz::add_choice('this')))
# })

test_that("create_messages works", {
  expect_s4_class(shinyQuiz::create_messages('C', 'W', 'W'), 'quizMessages')
  
  expect_error(shinyQuiz::create_messages(htmltools::p('html'), 'W', 'W'))
})

test_that("create_quiz works", {
  expect_s4_class(
    shinyQuiz::create_quiz(
      shinyQuiz::create_question('My q', shinyQuiz::add_choice('this', TRUE))
    ),
    'quiz'
  )
  expect_error(shinyQuiz::create_question('My q', shinyQuiz::add_choice('this')))
})
