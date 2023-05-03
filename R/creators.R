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



#' Create a choice for a question
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
#' 
#' create_question(
#'  'My question prompt',
#'  add_choice('39'),
#'  add_choice('41', TRUE)
#' )
#' @describeIn add_choice Create a discrete choice
add_choice <- function(text, correct = FALSE){
  # if(!isTRUE(is.character(text))) cli::cli_abort('`text` must be a character')
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
  # if(!is.null(value)) value <- as.numeric(value)
  # if(!is.na(min)) min <- as.numeric(min) else min <- NULL
  # if(!is.na(max)) max <- as.numeric(max) else max <- NULL
  # if(!is.na(step)) step <- as.numeric(step) else step <- NULL
  
  correct <- as.numeric(correct)
 
  args <- c(correct = correct)
  is_numeric <- purrr::map_lgl(args, \(x) is.numeric(x) && is_truthy(x))
  if (!isTRUE(all(is_numeric))) cli::cli_abort("{names(args)[!is_numeric]} must be coercible to numeric")
  
  # if(is.null(min)) min <- NA
  # if(is.null(max)) max <- NA
  # if(is.null(step)) step <- NA
  # 
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

#' Create a quiz question
#'
#' @param prompt Text of the question prompt
#' @param ... Objects of class 'quizChoice' generated from [add_choice()] or [add_slider()]. Named options to [shiny::selectInput] or [shiny::checkboxGroupInput] can be passed as well.
#' @param type One of c('auto', 'single', 'multiple'). How many answers are allowed
#' @param input One of c('auto', 'select', 'checkbox')
#' @param shuffle TRUE or FALSE if TRUE order of choices will be randomly shuffled
#' @param ns Namespace function generated from [`shiny::NS()`]
#' 
#' @details `create_question` is the default method of creating quiz questions. 
#'
#' @return an object of class `quizQuestion`
#' @export
#' 
#' @author Joseph Marlo, George Perrett
#' @seealso [add_choice()], [add_slider()]
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
  
  # extract choices
  choices <- dot_list[purrr::map_lgl(dot_list, \(x) inherits(x, 'quizChoice'))]
  if(isTRUE(shuffle)) choices <- sample(choices)
  
  # quality checks
  if (!isTRUE(is_truthy(slider_element) | is_truthy(choices) | is_truthy(numeric_element))) cli::cli_abort('No choices or sliders provided')
  if (is_truthy(slider_element) && length(slider_element) > 1) cli::cli_abort('Only one slider can be provided')
  if (is_truthy(numeric_element) && length(numeric_element) > 1) cli::cli_abort('Only one numeric input box can be provided')
  if (is_truthy(slider_element) && is_truthy(choices)) cli::cli_abort('sliders and choices cannot be mixed')
  if (is_truthy(numeric_element) && is_truthy(choices)) cli::cli_abort('numeric input box and choices cannot be mixed')
  # if (is_truthy(choices) && is_truthy(choices)) cli::cli_abort('sliders and choices cannot be mixed')
  
  # extract extra arguments
  # browser()
  label <- ifelse(is_truthy(dot_list$label), dot_list$label, 'Select answer')
  if(is_truthy(dot_list$selected)){selected <- dot_list$selected} else {selected <- NULL}
  use_slider <- is_truthy(slider_element)
  use_numeric <- is_truthy(numeric_element)
  
  if (use_slider){
    # extract extra arguments for slider 
    if(is_truthy(dot_list$step)){step <- dot_list$step} else {step <- NULL}
    round <- ifelse(is_truthy(dot_list$round), dot_list$round, FALSE)
    slider_element <- slider_element[[1]]
    input <- create_question_slider_(slider_element, label, step, round, ns)
  } else if(use_numeric){
    # extract extra arguments for numeric input
    if(is_truthy(dot_list$step)){step <- dot_list$step} else {step <- NA}
    if(is_truthy(dot_list$value)){value <- dot_list$value} else {value <- NULL}
    if(is_truthy(dot_list$min)){min <- dot_list$min} else {min <- NA}
    if(is_truthy(dot_list$max)){max <- dot_list$max} else {max <- NA}
    numeric_element <- numeric_element[[1]]
    input <- create_question_numeric_(numeric_element, label, min, max, value, step, ns)
  }else {
    input <- create_question_input_(dot_list, choices, type, input, label, selected, ns)
  }
  
  # put it in a div
  prompt_html <- htmltools::div(
    htmltools::p(prompt),
    htmltools::br(),
    input$input_html
  )
  
  # create question of the right class
  q <- construct_question(
    prompt = prompt_html,
    answerUserPrettifier = \(x) paste0(x, collapse = ', '),
    answerCorrectPretty = paste0(input$text_correct, collapse = ', '),
    grader = \(x) setequal(x, input$text_correct),
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


#' @param prompt Text of the question prompt. Preferably wrapped in [htmltools::div()]. 
#' @param grader A function that takes the user answer and determines if it is correct. Must take one argument and return TRUE or FALSE. This is wrapped with [purrr::possibly()] and [base::isTRUE()] to catch any errors.
#' @param correct_answer_pretty A string representing the correct answer that is printed 'pretty'
#' @param user_answer_prettifier A function with one argument that takes the user's answers and prints it 'pretty'
#'
#' @details `create_question_raw()` allows any html in the `prompt`. This must contain a shiny input that is accessible via `input$answers`. The namespace also needs care. The default `inputId` is `shiny::NS('quiz')('answers')`. 
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
#' @describeIn create_question Create a quiz question using custom inputs. This is a more flexible function. 
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
  # check if any items are sandbox questions then force into sandbox mode 
  # unless override is triggered
  if (!is.null(options$sandbox)){
    quiz@options$sandbox <- options$sandbox
    return(quiz)
  } 
  dot_list <- list(...)
  is_sandbox <- purrr::map_lgl(unlist(dot_list), \(x) inherits(x, 'quizQuestionSandbox'))
  any_sandbox <- isTRUE(any(is_sandbox))
  if (any_sandbox) {
    quiz@options$sandbox <- TRUE
    quiz@options$logic_end_on_first_wrong <- FALSE
    quiz@options$progress_bar <- FALSE
  }
  
  return(quiz)
}


# infinite questions ------------------------------------------------------
#' Create a sandbox question
#' @param .f a function that outputs an object of class `quizQuestion`. This function can not have any arguments and must be able to produce random permutations of questions. The easiest way to ensure this is by including a `create_question` or `create_question_raw` call inside your function (see example). 
#' @param n a numeric value indicating how many draw of function .f to include in the random question bank. 
#'
#' @description Create quasi-infinite questions. 
#' @details `create_question_sandbox()` takes any user generated function `.f`. The function passed to  the .`f` argument creates a random prompt along with an updated answer, the function passed to the `.f` argument must return an object of class `quizQuestion`. `create_question_sandbox()` will automatically check to ensure the function passed to `.f` is in the appropriate format. The `n` argument controls how many random draws from  the function passed to `.f` are included in the question bank for the quiz. Higher values of `n` allow more unique questions but extreme values of `n` may also lead to slower performance. To create a quiz with `n` randomly generated questions, `create_question_sandbox` can be passed as an argument to `create_quiz`.   
#'
#' @importFrom methods new
#' @return n number of objects of class `quizQuestion`
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
#'   q <- create_question(prompt = rand_prompt,
#'                        add_choice('Yes, it is even', correct = number%%2 == 0), 
#'                        add_choice('No, it is odd', correct = number%%2 != 0))
#'   
#'   return(q)
#' }
#' 
#' # create a quiz with a question bank of 20 randomly generated questions
#' create_quiz(
#' create_question_sandbox(.f = random_question, n = 20), 
#' options = set_quiz_options(sandbox = T)
#' )
#' }

create_question_sandbox <- function(.f, n = 50){
  if(!isTRUE(is.numeric(n))) cli::cli_abort('`n` must be coercible to a numeric value')
  verify_fn(.f)
  # draw random realizations
  q_bank <- replicate(n, .f(), simplify = 'list')
  # set new class for all question from sandbox function
  q_bank <- lapply(q_bank,function(x){new('quizQuestionSandbox', x)})
  return(q_bank)
}

#' @keywords internal
verify_fn <- function(.f){
  cli::cli_h1('Checking function')
  cli::cli_h2('Checking function input')
  if (!isTRUE(is.function(.f))) cli::cli_abort('.f must be a function with no arguments')
  verify_n_args(.f, n = 0)
  cli::cli_alert_success('Function inputs good')
  
  cli::cli_h2('Checking function output')
  verify_question_structure(.f())
  cli::cli_alert_success('Function output good')
  
  cli::cli_h2('Checking randomness')
  if (isTRUE(all.equal(.f(), .f()))) cli::cli_abort('No randomness detected. Function output on multiple calls is identical.')
  cli::cli_alert_success('Randomness detected')
  
  cli::cli_h1('')
  cli::cli_alert_success('All clear your sandbox question is looking good!')
  cli::cli_status_clear()
  
  
  # add class ?
  
  return(invisible(.f))
}

