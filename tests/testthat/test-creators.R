
test_that('choices works', {
  expect_s4_class(shinyquiz::add_choice('my option'), 'quizChoice')
  expect_s4_class(shinyquiz::add_slider(correct = 0.5), 'quizChoiceSlider')
  expect_s4_class(shinyquiz::add_slider(correct = 0.5), 'quizChoiceSlider')
  
  expect_error(shinyquiz::add_slider(correct = T))
})

test_that("create_question works", {
  expect_s4_class(shinyquiz::create_question('My q', shinyquiz::add_choice('this', TRUE)), 'quizQuestion')
  
  expect_error(shinyquiz::create_question('My q', shinyquiz::add_choice('this'), shinyquiz::add_slider(correct = 1)))
  expect_error(shinyquiz::create_question('My q', shinyquiz::add_choice('this')))
})

# test_that("create_question_raw works", {
#   expect_s4_class(shinyquiz::create_question('My q', shinyquiz::add_choice('this', TRUE)), 'quizQuestion')
#   expect_error(shinyquiz::create_question('My q', shinyquiz::add_choice('this')))
# })

test_that("create_messages works", {
  expect_s4_class(shinyquiz::create_messages('C', 'W', 'W'), 'quizMessages')
  
  expect_error(shinyquiz::create_messages(htmltools::p('html'), 'W', 'W'))
})

test_that("create_quiz works", {
  expect_s4_class(
    shinyquiz::create_quiz(
      shinyquiz::create_question('My q', shinyquiz::add_choice('this', TRUE))
    ),
    'quiz'
  )
  expect_error(shinyquiz::create_question('My q', shinyquiz::add_choice('this')))
})

test_that("add_text fuzzy works", {
  q1_fuzzy <- create_question('My Label', add_text('hEllo'))
  expect_true(q1_fuzzy@grader(' Héllo'))
  
  q1_exact <- create_question('My Label', add_text('hEllo', exact = TRUE))
  expect_false(q1_exact@grader(' Héllo'))
})
