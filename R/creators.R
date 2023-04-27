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

#' Create a choice for a question
#'
#' @param text Text of the choice answer
#' @param correct Boolean denoting if this `choice` is correct; numeric for `slider`
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

#' @param min the minimum value of the slider range
#' @param max the maximum value of the slider range
#' @param default the default value the slider should take
#' @return an object of class 'quizChoiceSlider'
#' @export
#' @describeIn add_choice Create a slider choice
add_slider <- function(min = 0, max = 1, default = 0.5, correct){

  if(!isTRUE(is.numeric(as.numeric(min)))) cli::cli_abort('`min` must be coercible to a numeric')
  if(!isTRUE(is.numeric(as.numeric(max)))) cli::cli_abort('`max` must be coercible to a numeric')
  if(!isTRUE(dplyr::between(correct, min, max))) cli::cli_abort('`correct` must be between `min` and `max`')
  
  slider <- methods::new('quizChoiceSlider')
  slider@min <- min
  slider@max <- max
  slider@default <- default
  slider@correct <- correct
  
  return(slider)
}

#' Create a quiz question
#'
#' @param prompt Text of the question prompt
#' @param ... Objects of class 'quizChoice' generated from [add_choice()] or [add_slider()]. Named options to [shiny::selectInput] or [shiny::checkboxGroupInput] can be passed as well.
#' @param type One of c('auto', 'single', 'multiple'). How many answers are allowed
#' @param input One of c('auto', 'select', 'checkbox')
#' @param id Namespace of the module. Defaults to 'quiz'. This only needs to modified if there are multiple quizzes.
#' @param id_parent Namespace of the parent module, if any. This only needs to modified if the quiz sits within a shiny module.
#'
#' @return an object of class 'quizQuestion'
#' @export
#' 
#' @author Joseph Marlo, George Perrett
#' @seealso [add_choice()]
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
create_question <- function(prompt, ..., type = c('auto', 'single', 'multiple'), input = c('auto', 'select', 'checkbox'), id = 'quiz', id_parent = character(0)){
  
  # TODO: change id/idparent to ns?
  ns <- shiny::NS(shiny::NS(id_parent)(id))
  
  type <- match.arg(type, c('auto', 'single', 'multiple'))
  input <- match.arg(input, c('auto', 'select', 'checkbox'))
  dot_list <- list(...)
  
  # extract sliders
  slider_element <- dot_list[purrr::map_lgl(dot_list, \(x) inherits(x, 'quizChoiceSlider'))]
  
  # extract choices
  choices <- dot_list[purrr::map_lgl(dot_list, \(x) inherits(x, 'quizChoice'))]
  
  # quality checks
  if (!isTRUE(is_truthy(slider_element) | is_truthy(choices))) cli::cli_abort('No choices or sliders provided')
  if (is_truthy(slider_element) && length(slider_element) > 1) cli::cli_abort('Only one slider can be provided')
  if (is_truthy(slider_element) && is_truthy(choices)) cli::cli_abort('sliders and choices cannot be mixed')
  # if (is_truthy(choices) && is_truthy(choices)) cli::cli_abort('sliders and choices cannot be mixed')
  
  # extract extra arguments
  # browser()
  label <- ifelse(is_truthy(dot_list$label), dot_list$label, 'Select answer')
  if(is_truthy(dot_list$selected)){selected <- dot_list$selected} else {selected <- NULL}
  if(is_truthy(dot_list$step)){step <- dot_list$step} else {step <- NULL}
  round <- ifelse(is_truthy(dot_list$round), dot_list$round, FALSE)
  
  use_slider <- is_truthy(slider_element)
  if (use_slider){
    slider_element <- slider_element[[1]]
    input <- create_question_slider_(slider_element, label, step, round, ns)
  } else {
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
    grader = \(x) setequal(x, input$text_correct)
  )
  
  return(q)
}

#' @keywords internal
create_question_slider_ <- function(slider, label, step, round, ns){
  input_html <- shiny::sliderInput(
    inputId = ns('answers'), #TODO: need to figure out how to pass the namespace to here
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
      inputId = ns('answers'), #TODO: need to figure out how to pass the namespace to here
      label = label,
      choices = texts,
      selected = selected,
      multiple = isTRUE(type == 'multiple')
    )
  } else {
    input_html <- shiny::checkboxGroupInput(
      inputId = ns('answers'), #TODO: need to figure out how to pass the namespace to here
      label = label,
      choices = texts,
      selected = selected
    )
  }
  
  return(list(input_html = input_html, text_correct = text_correct))
}

#' Create a quiz question
#' 
#' Create a quiz question using custom inputs. This is a more flexible function that [create_question()]. It allows any html in the `prompt`. ADD REQUIREMENTS
#'
#' @param prompt Text of the question prompt. Preferably wrapped in [htmltools::div()]. 
#' @param grader A function with one argument that takes the user input and returns TRUE/FALSE
#' @param correct_answer_pretty A string representing the correct answer that is printed 'pretty'
#' @param user_answer_prettifier A function with one argument that takes the user's answers and prints it 'pretty'
#'
#' @return a object of class `quizQuestion`
#' @export
#' @author Joseph Marlo
#'
#' @examples
#' \dontrun{
#' q <- create_question_raw(
#'   htmltools::div(
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
#' preview_question(q)
#' }
create_question_raw <- function(prompt, grader, correct_answer_pretty, user_answer_prettifier = \(user_input) paste0(user_input, collapse = ', ')){
  
  # if(!isTRUE(is.character(as.character(text)))) cli::cli_abort('`text` must be coercible to a character')
  
  q <- construct_question(
    prompt = htmltools::div(prompt),
    answerUserPrettifier = user_answer_prettifier,
    answerCorrectPretty = correct_answer_pretty,
    grader = grader
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
#'
#' @examples
#' # TBD
create_quiz <- function(..., options = set_quiz_options()){
  construct_quiz(..., options = options)
}


# infinite questions ------------------------------------------------------

# TODO
# create_question_infinite <- function(.f){
#   
# }
