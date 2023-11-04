### user-facing functions for creating questions and quizzes ###

#' S4 class for a quiz choices
#'
#' @slot text character. 
#' @slot correct logical. 
#' 
#' @author Joseph Marlo
#' @keywords internal
#' @return none, sets a class
setClass('quizChoice', slots = list(
  text = 'character',
  correct = 'logical'
  )
)

#' S4 class for a quiz sliders
#'
#' @slot min numeric. 
#' @slot max numeric. 
#' @slot default numeric. 
#' @slot correct numeric. 
#'
#' @author Joseph Marlo
#' @keywords internal
#' @return none, sets a class
setClass('quizChoiceSlider', slots = list(
  min = 'numeric',
  max = 'numeric',
  default = 'numeric',
  correct = 'numeric'
  )
)

#' S4 class for a quiz numeric response
#'
#' @slot correct numeric. 
#'
#' @author George Perrett
#' @keywords internal
#' @return none, sets a class
setClass('quizChoiceNumeric', slots = list(
  correct = 'numeric'
 )
)

#' S4 class for a quiz free form text response
#'
#' @slot correct character 
#'
#' @author Joseph Marlo
#' @keywords internal
#' @return none, sets a class
setClass('quizChoiceText', slots = list(
  correct = 'character',
  exact = 'logical'
)
)


#' Add choices to a question
#' 
#' Add a choice to a quiz question. Used in conjunction with [create_question()] to generate a question 
#'
#' @param text Text of the choice answer
#' @param correct Boolean denoting if this `choice` is correct; numeric for `slider` or `numeric`
#'
#' @return an object of class 'quizChoice'
#' @export
#' 
#' @author Joseph Marlo
#' @seealso [create_question()]
#'
#' @examples
#' add_choice('39')
#' add_choice('39', TRUE)
#' add_slider(0, 1, 0.5, 0.8)
#' add_text('Correct answer')
#' 
#' create_question(
#'  'My question prompt',
#'  add_choice('39'),
#'  add_choice('41', TRUE)
#' )
#' @describeIn add_choice Create a discrete choice
add_choice <- function(text, correct = FALSE){
  if(!isTRUE(is.character(as.character(text)))) cli::cli_abort('`text` must be coercible to a character')
  if(!isTRUE(is.logical(correct))) cli::cli_abort('`correct` must be a logical')
  
  choice <- methods::new('quizChoice')
  choice@text <- as.character(text)
  choice@correct <- correct
  
  return(choice)
}


#' @author George Perrett
#' @return an object of class 'quizChoiceNumeric'
#' @export
#' @describeIn add_choice Create a numeric choice
add_numeric <- function(correct){
  if (is.logical(correct)) cli::cli_abort('`correct` should be a numeric, not logical')
  correct <- as.numeric(correct)
  if (!is_truthy(correct)) cli::cli_abort("`correct` must be coercible to numeric")
  
  numeric <- methods::new('quizChoiceNumeric')
  numeric@correct <- correct
  return(numeric)
}

#' @param min the minimum value of the slider range
#' @param max the maximum value of the slider range
#' @param default_position the default value the slider should take
#' @return an object of class 'quizChoiceSlider'
#' @export
#' @describeIn add_choice Create a slider choice
add_slider <- function(min = 0, max = 1, default_position = 0.5, correct){
  
  if (is.logical(correct)) cli::cli_abort('`correct` should be a numeric, not logical')

  min <- as.numeric(min)
  max <- as.numeric(max)
  default_position <- as.numeric(default_position)
  correct <- as.numeric(correct)
  
  args <- c(min = min, max = max, default_position = default_position, correct = correct)
  is_numeric <- purrr::map_lgl(args, \(x) is.numeric(x) && is_truthy(x))
  if (!isTRUE(all(is_numeric))) cli::cli_abort("{names(args)[!is_numeric]} must be coercible to numeric")
  if(!isTRUE(dplyr::between(correct, min, max))) cli::cli_abort('`correct` must be between `min` and `max`')
  
  slider <- methods::new('quizChoiceSlider')
  slider@min <- min
  slider@max <- max
  slider@default <- default_position
  slider@correct <- correct
  
  return(slider)
}


#' @param exact Boolean denoting if the grader should use exact matching. If FALSE, the user's answer will be compared to the correct answer after trimming whitespace, converting to lower case, and normalizing diacritics. If you wish to use your own normalizing function, please see [create_question_raw()].
#' @author Joseph Marlo
#' @return an object of class 'quizChoiceText'
#' @export
#' @describeIn add_choice Create a free text choice
#' @examples 
#' 
#' q1_fuzzy <- create_question('My Label', add_text(correct = ' hEllo'))
#' q1_fuzzy@grader('Héllo ')
#' q1_exact <- create_question('My Label', add_text(correct = 'hEllo', exact = TRUE))
#' q1_exact@grader('Héllo ')
add_text <- function(correct, exact = FALSE){
  if(!isTRUE(is.character(as.character(correct)))) cli::cli_abort('`correct` must be coercible to a character')
  if(!isTRUE(is.logical(exact))) cli::cli_abort('`exact` must be a logical')
  
  choice <- methods::new('quizChoiceText')
  choice@exact <- as.logical(exact)
  choice@correct <- correct
  
  return(choice)
}

#' @keywords internal
grader_fn_text_fuzzy <- function(text, correct){
  normalize_text <- function(x){
    x <- trimws(x)
    x <- tolower(x)
    x <- gsub("\\s+", " ", x) # trim extra spaces
    x <- stringi::stri_trans_general(x, "Latin-ASCII") # remove non-English standard characters
    return(x)
  }
  is_equal <- setequal(
    normalize_text(text), 
    normalize_text(correct)
  )
  return(is_equal)
}

#' Create a quiz question
#' 
#' Create a single quiz question. Used in conjunction with [create_quiz()] to generate a quiz. 
#'
#' @param prompt Text of the question prompt
#' @param ... Objects of class 'quizChoice' generated from [add_choice()], [add_numeric()], [add_slider()], or [add_text()]. Named options to [shiny::selectInput()] or [shiny::checkboxGroupInput()] can be passed as well.
#' @param type One of c('auto', 'single', 'multiple'). Can the user select only one answer or multiple?
#' @param input One of c('auto', 'select', 'checkbox'). Should the UI for a select object created by [shiny::selectInput()] or checkbox by [shiny::checkboxGroupInput()]?
#' @param shuffle TRUE or FALSE. If TRUE order of choices will be randomly shuffled.
#' @param ns Namespace function generated from [`shiny::NS()`]
#' 
#' @details `create_question` is the default method of creating quiz questions. 
#'
#' @return an object of class `quizQuestion`
#' @export
#' 
#' @author Joseph Marlo, George Perrett
#' @seealso [add_choice()], [add_slider()], [add_numeric()], [add_text()], [create_question_raw()]
#' 
#' @examples
#' \dontrun{
#' q <- create_question(
#'   prompt = 'My prompt explaining what the ATE of this thing should be',
#'   add_choice("34"),
#'   add_choice("59", TRUE),
#'   add_choice("98", TRUE)
#' )
#' preview_question(q)
#' 
#' q2 <- create_question(
#'   prompt = 'My prompt explaining what the ATC of this thing should be',
#'   add_slider(0, 30, 15, correct = 10)
#'  )
#' create_quiz(q, q2)
#' }
#' @describeIn create_question Create a quiz question
create_question <- function(prompt, ..., type = c('auto', 'single', 'multiple'), input = c('auto', 'select', 'checkbox'), shuffle = FALSE, ns = shiny::NS('quiz')){
  
  if (!isTRUE(is.function(ns))) cli::cli_abort('`ns` must be a function. Preferably generated from `shiny::NS()`')
  
  type <- match.arg(type, c('auto', 'single', 'multiple'))
  input <- match.arg(input, c('auto', 'select', 'checkbox'))
  dot_list <- list(...)
  
  # extract sliders
  slider_element <- dot_list[purrr::map_lgl(dot_list, \(x) inherits(x, 'quizChoiceSlider'))]
  
  # extract numeric input
  numeric_element <- dot_list[purrr::map_lgl(dot_list, \(x) inherits(x, 'quizChoiceNumeric'))]
  
  # extract text input
  text_element <- dot_list[purrr::map_lgl(dot_list, \(x) inherits(x, 'quizChoiceText'))]
  
  # extract choices
  choices <- dot_list[purrr::map_lgl(dot_list, \(x) inherits(x, 'quizChoice'))]
  if(isTRUE(shuffle)) choices <- sample(choices)
  
  # quality checks
  if (!isTRUE(is_truthy(slider_element) | is_truthy(choices) | is_truthy(numeric_element) | is_truthy(text_element))) cli::cli_abort('No choices, sliders, numerics, or texts provided')
  if (is_truthy(slider_element) && length(slider_element) > 1) cli::cli_abort('Only one slider can be provided')
  if (is_truthy(numeric_element) && length(numeric_element) > 1) cli::cli_abort('Only one numeric input box can be provided')
  if (is_truthy(slider_element) && is_truthy(choices)) cli::cli_abort('sliders and choices cannot be mixed')
  if (is_truthy(numeric_element) && is_truthy(choices)) cli::cli_abort('numeric input box and choices cannot be mixed')
  if (is_truthy(numeric_element) && is_truthy(slider_element)) cli::cli_abort('numeric input box and sliders cannot be mixed')
  if (is_truthy(text_element) && (is_truthy(slider_element) || is_truthy(numeric_element) || is_truthy(choices))) cli::cli_abort('Text choices cannot be mixed with other types')
  
  # extract extra arguments
  label <- ifelse(is_truthy(dot_list$label), dot_list$label, 'Select answer')
  if(is_truthy(dot_list$selected)){selected <- dot_list$selected} else {selected <- NULL}
  use_slider <- is_truthy(slider_element)
  use_numeric <- is_truthy(numeric_element)
  use_text <- is_truthy(text_element)
  
  if (use_slider){
    # extract extra arguments for slider 
    if(is_truthy(dot_list$step)){step <- dot_list$step} else {step <- NULL}
    round <- ifelse(is_truthy(dot_list$round), dot_list$round, FALSE)
    slider_element <- slider_element[[1]]
    input <- create_question_slider_(slider_element, label, step, round, ns)
  } else if (use_numeric){
    # extract extra arguments for numeric input
    if(is_truthy(dot_list$step)){step <- dot_list$step} else {step <- NA}
    if(is_truthy(dot_list$value)){value <- dot_list$value} else {value <- NULL}
    if(is_truthy(dot_list$min)){min <- dot_list$min} else {min <- NA}
    if(is_truthy(dot_list$max)){max <- dot_list$max} else {max <- NA}
    numeric_element <- numeric_element[[1]]
    input <- create_question_numeric_(numeric_element, label, min, max, value, step, ns)
  } else if (use_text) {
    input <- create_question_text_(text_element[[1]], label, ns)
  } else {
    input <- create_question_input_(dot_list, choices, type, input, label, selected, ns)
  }
  
  # put it in a div
  prompt_html <- htmltools::div(
    htmltools::p(prompt),
    htmltools::br(),
    input$input_html
  )
  
  # set grader function
  grader_fn <- \(x) setequal(x, input$text_correct)
  
  # for free text questions that are not exact, change the grader
  if (use_text && isFALSE(input$is_exact)) grader_fn <- \(x) grader_fn_text_fuzzy(x, input$text_correct)

  # create question of the right class
  q <- construct_question(
    prompt = prompt_html,
    answerUserPrettifier = \(x) paste0(x, collapse = ', '),
    answerCorrectPretty = paste0(input$text_correct, collapse = ', '),
    grader = grader_fn,
    ns = ns
  )
  
  return(q)
}

#' @keywords internal
create_question_numeric_ <- function(numeric, label, min, max, value, step, ns){
  input_html <- shiny::numericInput(
    inputId = ns('answers'), 
    label = label,
    min = min,
    max = max,
    value = value,
    step = step
  )
  
  text_correct <- as.character(numeric@correct)
  
  return(list(input_html = input_html, text_correct = text_correct))
}

#' @keywords internal
create_question_slider_ <- function(slider, label, step, round, ns){
  input_html <- shiny::sliderInput(
    inputId = ns('answers'), 
    label = label,
    min = slider@min,
    max = slider@max,
    value = slider@default,
    step = step,
    round = round
  )
  
  text_correct <- as.character(slider@correct)
  
  return(list(input_html = input_html, text_correct = text_correct))
}

#' @keywords internal
create_question_text_ <- function(text, label, ns){
  input_html <- shiny::textInput(
    inputId = ns('answers'), 
    label = label
  )
  
  text_correct <- as.character(text@correct)
  is_exact <- text@exact
  
  return(list(input_html = input_html, text_correct = text_correct, is_exact = is_exact))
}

#' @keywords internal
create_question_input_ <- function(dot_list, choices, type, input, label, selected, ns){
  
  # extract answer texts
  texts <- purrr::map(choices, \(x) x@text)
  
  # check which choice is correct
  is_correct <- purrr::map_lgl(choices, \(x) x@correct)
  if (sum(is_correct) < 1) cli::cli_abort('Choices must contain at least one correct answer')
  text_correct <- texts[is_correct]
  
  # select type and input type
  if (type == 'auto'){
    type <- ifelse(sum(is_correct) == 1, 'single', 'multiple')
  }
  if (input == 'auto'){
    input <- ifelse(type == 'single', 'select', 'checkbox')
  }
  
  if (sum(is_correct) != 1 && type == 'single') cli::cli_abort('When `type` == "single", choices must contain exactly one correct answer')
  
  if (input == 'select'){
    input_html <- shiny::selectInput(
      inputId = ns('answers'),
      label = label,
      choices = c('', texts),
      selected = selected,
      multiple = isTRUE(type == 'multiple')
    )
  }else {
    input_html <- shiny::checkboxGroupInput(
      inputId = ns('answers'), 
      label = label,
      choices = texts,
      selected = selected
    )
  }
  
  return(list(input_html = input_html, text_correct = text_correct))
}


#' @param prompt Text of the question prompt. Can also be an HTML element such as [htmltools::div()]. 
#' @param grader A function that takes the user answer and determines if it is correct. Must take one argument and return TRUE or FALSE. This is wrapped with [purrr::possibly()] and [base::isTRUE()] to catch any errors.
#' @param correct_answer_pretty A string representing the correct answer that is printed 'pretty'
#' @param user_answer_prettifier A function with one argument that takes the user's answers and prints it 'pretty'
#'
#' @details `create_question_raw()` allows any HTML in the `prompt`. This must contain a shiny input that is accessible via `input$answers`. The namespace also needs care. The default `inputId` is `shiny::NS('quiz')('answers')`. 
#'
#' @return an object of class `quizQuestion`
#' @export
#' @author Joseph Marlo
#'
#' @examples
#' \dontrun{
#' q3 <- create_question_raw(
#'   prompt = htmltools::div(
#'     htmltools::p("my question"),
#'     shiny::selectInput(
#'       inputId = shiny::NS('quiz')('answers'),
#'       label = 'Select 5',
#'       choices = c(4, 5, 6)
#'     )
#'  ),
#'  grader = \(user_input) user_input == '5',
#'  correct_answer_pretty = '5'
#' )
#' create_quiz(q3, q2)
#' }
#' @describeIn create_question Create a quiz question using custom inputs. This is a more flexible function that allows any html. 
create_question_raw <- function(prompt, grader, correct_answer_pretty, user_answer_prettifier = \(user_input) paste0(user_input, collapse = ', ')){
  
  q <- construct_question(
    prompt = htmltools::div(prompt),
    answerUserPrettifier = user_answer_prettifier,
    answerCorrectPretty = correct_answer_pretty,
    grader = grader,
    ns = shiny::NS('quiz') # dummy since ns should be handled directly
  )
  
  return(q)
}

#' Create a quiz
#' 
#' Create a single question comprising of questions generated from [create_question()] and/or [create_question_raw()]. 
#'
#' @param ... objects of class 'quizQuestions'. See [create_question()], [create_question_raw()]
#' @param options a list of options generated from [set_quiz_options()]
#'
#' @return an object of class `quiz`
#' @export
#' @author Joseph Marlo
#' @seealso [set_quiz_options()], [create_question()], [create_question_raw()]
#'
#' @examples
#' quiz <- create_quiz(
#'   create_question(
#'     'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Select nulla.',
#'     add_choice('auctor'),
#'     add_choice('nulla', correct = TRUE)
#'   ),
#'   create_question(
#'     'Mauris congue aliquet dui, ut dapibus lorem porttitor sed. Select 600.',
#'     add_choice('600', correct = TRUE),
#'     add_choice('800')
#'   )
#' )
create_quiz <- function(..., options = set_quiz_options()){
  # create quiz
  quiz <- construct_quiz(..., options = options)
  
  # check if any items are random questions then force into sandbox mode 
  # unless override is triggered
  if (isTRUE(options$overide)) return(quiz)
  dot_list <- list(...)
  is_random <- purrr::map_lgl(unlist(dot_list), \(x) inherits(x, 'quizQuestionRandom'))
  any_random <- isTRUE(any(is_random))
  if (any_random) {
    quiz@options$sandbox <- TRUE
    quiz@options$logic_end_on_first_wrong <- FALSE
    quiz@options$progress_bar <- FALSE
  }
  
  return(quiz)
}


# random questions ------------------------------------------------------

#' Create a random question
#' @param .f a function that outputs an object of class `quizQuestion`. This function can not have any arguments and must be able to produce random permutations of questions. The easiest way to ensure this is by including a `create_question` or `create_question_raw` call inside your function (see example). 
#' @param n a numeric value indicating how many draws of function .f to include in the random question bank. 
#'
#' @description Create questions with inherit randomness. Allows one function to generate many different questions. 
#' @details `create_question_random()` takes any user generated function `.f`. The function passed to  the .`f` argument creates a random prompt along with an updated answer, the function passed to the `.f` argument must return an object of class `quizQuestion`. `create_question_random()` will automatically check to ensure the function passed to `.f` is in the appropriate format. The `n` argument controls how many random draws from  the function passed to `.f` are included in the question bank for the quiz. Higher values of `n` allow more unique questions but extreme values of `n` may also lead to slower performance. To create a quiz with `n` randomly generated questions, `create_question_random()` can be passed as an argument to `create_quiz()`.   
#'
#' @return a list of length n that includes objects of class `quizQuestionRandom`
#' @export
#' @author George Perrett, Joseph Marlo
#'
#' @examples
#' \dontrun{
#' 
#' # a function that generates a random question
#' random_question <- function() {
#'   number <- round(rnorm(1, 30, 10), 0)
#'   rand_prompt <- paste('Is', number, 'an even number?')
#'   
#'   # using create_question inside the function helps to ensure correct class
#'   q <- create_question(
#'     prompt = rand_prompt,
#'     add_choice('Yes, it is even', correct = number %% 2 == 0), 
#'     add_choice('No, it is odd', correct = number %% 2 != 0)
#'   )
#'   
#'   return(q)
#' }
#' 
#' # create a quiz with a question bank of 20 randomly generated questions
#' create_quiz(
#'   create_question_random(.f = random_question, n = 20)
#' )
#' }
create_question_random <- function(.f, n = 50){
  if (!((n %% 1 == 0) & n > 0)) cli::cli_abort('`n` must be a positive integer')
  verify_question_random(.f)
  
  # draw random realizations
  q_bank <- replicate(n, .f(), simplify = 'list')
  
  # set new class for all question from random function
  q_bank <- purrr::map(q_bank, \(x) methods::new('quizQuestionRandom', x))
  
  return(q_bank)
}

#' @describeIn verify_question_structure Verify the input function is the correct structure
#' @keywords internal
verify_question_random <- function(.f){
  
  cli::cli_progress_step(
    'Checking function input',
    msg_done = 'Function inputs good',
    msg_failed = '.f must be a function with no arguments'
  )
  if (!isTRUE(is.function(.f))) cli::cli_abort('')
  verify_n_args(.f, n = 0)
  
  cli::cli_progress_step(
    'Checking function output', 
    msg_done = 'Function output good',
    msg_failed = 'Could not verify function output. Does your function return a question using `create_question` or `create_question_raw`?'
  )
  verify_question_structure(.f())
  
  cli::cli_progress_step(
    'Checking randomness',
    msg_done = 'Randomness detected',
    msg_failed = 'No randomness detected. Function output from multiple calls is identical.'
  )
  if (isTRUE(all.equal(.f(), .f()))) cli::cli_abort('Randomness not detected. Two function calls produced the same output.')
  
  cli::cli_progress_step('All clear! Your random question is looking good!')
  cli::cli_status_clear()
  
  return(invisible(TRUE))
}
