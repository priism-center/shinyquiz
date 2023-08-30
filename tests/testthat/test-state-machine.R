quiz <- shinyQuiz::create_quiz(
  shinyQuiz::create_question(
    'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Select nulla.',
    shinyQuiz::add_choice('auctor'),
    shinyQuiz::add_choice('nulla', correct = TRUE)
  ),
  shinyQuiz::create_question(
    'Mauris congue aliquet dui, ut dapibus lorem porttitor sed. Select 600.',
    shinyQuiz::add_choice('600', correct = TRUE),
    shinyQuiz::add_choice('800')
  )
)
store <- shinyQuiz:::sm_create_reactive_store(quiz)
state1 <- shinyQuiz:::sm_get_state(store)
state2 <- shinyQuiz:::sm_get_state(store, 'next-state')
state3 <- shinyQuiz:::sm_get_state(store, 'current-question')
correct <- shinyQuiz:::sm_check_is_each_correct(store)
score <- shinyQuiz:::sm_score_quiz(store)

quiz_complete <- shinyQuiz:::sm_quiz_is_complete(store)
store2 <- shinyQuiz:::sm_set_state(store, 'current-state', 'quiz-complete')
state21 <- shinyQuiz:::sm_get_state(store2)
quiz_complete2 <- shinyQuiz:::sm_quiz_is_complete(store2)

test_that('state machine works', {
  expect_type(store, 'list')
  
  expect_type(state1, 'character')
  expect_equal(state1, 'quiz-question-1')
  
  expect_type(state2, 'character')
  expect_equal(state2, 'quiz-question-2')
  
  expect_s3_class(state3, 'shiny.tag')
  
  expect_false(any(correct))
  
  expect_equal(score, 0)
  
  expect_false(quiz_complete)
  expect_equal(state21, 'quiz-complete')
  expect_true(quiz_complete2)
})
