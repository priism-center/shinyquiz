# adds a green checkmark to a div
add_checkmark <- function(ns = NULL, div_id = 'submit_button'){
  # ns <- shiny::NS(ns)
  div_id <- ns(div_id)
  shinyjs::runjs({
    paste0(
      '$("#', div_id, '").append("    " + \'<span class="glyphicon glyphicon-ok" style="color:green"></span>\')'
    )
  })
}

# adds a red X to a div
add_red_x <- function(ns = NULL, div_id = 'submit_button'){
  # ns <- shiny::NS(ns)
  div_id <- ns(div_id)
  shinyjs::runjs({
    paste0(
      '$("#', div_id, '").append("    " + \'<span class="glyphicon glyphicon-remove" style="color:red"></span>\')'
    )
  })
}
