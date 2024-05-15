### UI-specific utility functions ###

#' Scroll the webpage to a certain div
#'
#' @param ns namespace of the Shiny module
#' @param id id of the div
#'
#' @return called for side effect
#' @author Joseph Marlo
#' @noRd
#' @keywords internal
scroll_to_div <- function(ns = NULL, id = 'quiz-container'){
  if (!is.null(ns)) id <- ns(id)
  js <- glue::glue("$('#{id}')[0].scrollIntoView()")
  shinyjs::runjs(js)
}

#' Add an icon to a div
#'
#' @param ns namespace of the Shiny module
#' @param id id of the div to add the X to
#' @param element element within the div to place the X
#'
#' @return called for side effect
#' @author Joseph Marlo
#' @noRd
#' @keywords internal
add_grade_icon <- function(ns, id, icon, color){
  
  # construct the base selector
  if (!missing(ns)) {
    id <- paste0("#", ns(id))
  } else {
    id <- paste0("#", id)
  }
  
  # add the icon to the h3 element (question title) if it exists, otherwise h4 (question heading)
  js <- glue::glue(
    .open = "{{",
    .close = '}}',
    "var element = $('{{id}} h3').length > 0 ? 'h3' : 'h4'; 
     var selector = '{{id}} ' + element;
     var div_selector = $(selector);
     if (div_selector.children().length === 0) {
       div_selector.append('\\t' + '<span class=\\\"glyphicon glyphicon-{{icon}}\\\" style=\\\"color:{{color}}; font-size: 0.9em;\\\"></span>');
     }"
  )
  
  # run js
  shinyjs::runjs(js)
}

#' @describeIn add_grade_icon
#' @keywords internal
add_checkmark <- function(ns = NULL, id = 'quiz-container'){
  add_grade_icon(ns = ns, id = id, icon = 'ok', color = 'green')
}

#' @describeIn add_grade_icon
#' @keywords internal
add_red_x <- function(ns = NULL, id = 'quiz-container'){
  add_grade_icon(ns = ns, id = id, icon = 'remove', color = 'red')
}

#' Add ending messages to the quiz
#'
#' @param text Message to display
#'
#' @return called for side effect
#' @author Joseph Marlo
#' @noRd
#' @keywords internal
#' @describeIn add_message_correct Message to display when user is correct
add_message_correct <- function(text){
  # this relies on bootstrap css
  htmltools::div(
    class = 'alert alert-success',
    htmltools::p(text)
  )
}

#' @keywords internal
#' @describeIn add_message_correct Message to display when user is wrong
add_message_wrong <- function(text){
  # this relies on bootstrap css
  htmltools::div(
    class = 'alert alert-danger',
    htmltools::p(text)
  )
}

#' @keywords internal
#' @describeIn add_message_correct Message to display when user skipped the quiz
add_message_skipped <- function(text){
  # this relies on bootstrap css
  htmltools::div(
    class = 'alert alert-warning',
    htmltools::p(text)
  )
}


#' Add confetti celebration animation
#' 
#' Requires confetti.css to be in the www/css folder
#'
#' @return div containing divs of class "confetti-piece"
#' @author Joseph Marlo
#' @seealso \url{https://codepen.io/zer0kool/pen/KjZWRW}
#' @noRd
#' @keywords internal
add_confetti <- function(){

  confetti_pieces <- htmltools::div(
    class = 'confetti',
    htmltools::div(class = 'confetti-piece'),
    htmltools::div(class = 'confetti-piece'),
    htmltools::div(class = 'confetti-piece'),
    htmltools::div(class = 'confetti-piece'),
    htmltools::div(class = 'confetti-piece'),
    htmltools::div(class = 'confetti-piece'),
    htmltools::div(class = 'confetti-piece'),
    htmltools::div(class = 'confetti-piece'),
    htmltools::div(class = 'confetti-piece'),
    htmltools::div(class = 'confetti-piece')
  )
  
  # remove after 10 seconds to prevent re-animations when coming back to the page
  shinyjs::delay(
    10*1000,
    shinyjs::hide(selector = '.confetti')
  )
  
  return(confetti_pieces)
}

#' Create a bootstrap progress bar
#'
#' @param percent The percent to fill the progress bar to
#' @param bg_color Background color
#'
#' @return html
#' @author Joseph Marlo
#' @seealso \url{https://getbootstrap.com/docs/4.0/components/progress/}
#' @noRd
#' @keywords internal
add_progress_bar <- function(percent, bg_color) {
  if (percent < 0 | percent > 1) cli::cli_abort('`percent` must be [0, 1]')
  percent <- round(percent * 100, digits = 0)
  if (is.null(bg_color)) bg_color <- "#609963"
    
  htmltools::div(
    class = 'progress',
    htmltools::div(
      class = 'progress-bar',
      role  = 'progressbar',
      style = glue::glue("width: {percent}%; background: {bg_color};"),
      `aria-valuenow` = percent,
      `aria-valuemin` = "0",
      `aria-valuemax` = "100"
    )
  )
}
