# adds a green checkmark to a div
add_checkmark <- function(ns = NULL, div_id = 'submit_button'){
  # ns <- shiny::NS(ns)
  div_id <- ns(div_id)
  shinyjs::runjs({
    div_selector <- paste0('$("#', div_id, '")')
    paste0(
      'if (',
      div_selector,
      '.children().length===0){',
      div_selector,
      '.append("\t" + \'<span class="glyphicon glyphicon-ok" style="color:green"></span>\')}'
    )
  })
}

# adds a red X to a div
add_red_x <- function(ns = NULL, div_id = 'submit_button'){
  # ns <- shiny::NS(ns)
  div_id <- ns(div_id)
  shinyjs::runjs({
    div_selector <- paste0('$("#', div_id, '")')
    paste0(
      'if (',
      div_selector,
      '.children().length===0){',
      div_selector,
      '.append("\t" + \'<span class="glyphicon glyphicon-remove" style="color:red"></span>\')}'
    )
  })
}
