#' Check to see if value is not in a vector
#'
#' This is just a negative of `%in%`
#'
#' @param ... 
#'
#' @return boolean
#' @author Joseph Marlo
#' @return NULL
#' @export
#' 
#' @examples
#' 1 %notin% 2:4
`%notin%` <- Negate(`%in%`)

#' Check if a value is truthy
#'
#' A value is truthy unless it is FALSE, NA, NULL, an empty data.frame, or empty list.
#'
#' Modified from shiny::isTruthy
#'
#' @param x An expression whose truthiness value we want to determine
#'
#' @return boolean
#'
#' @author Joseph Marlo
#' @export
#' @seealso [shiny::isTruthy()]
#'
#' @examples
#' is_truthy(TRUE)
#' is_truthy(FALSE)
#' is_truthy(1)
#' is_truthy(0)
#' is_truthy(NULL)
#' is_truthy(NA)
#' is_truthy(data.frame())
#' is_truthy(data.frame(x = 1))
is_truthy <- function(x){
  
  ##### additions
  if (inherits(x, "data.frame") && nrow(x) == 0)
    return(FALSE)
  if (inherits(x, "list") && length(x) == 0)
    return(FALSE)
  
  ##### shiny::isTruthy
  if (inherits(x, "try-error"))
    return(FALSE)
  if (!is.atomic(x))
    return(TRUE)
  if (is.null(x))
    return(FALSE)
  if (length(x) == 0)
    return(FALSE)
  if (all(is.na(x)))
    return(FALSE)
  if (is.character(x) && !any(nzchar(stats::na.omit(x))))
    return(FALSE)
  if (inherits(x, "shinyActionButtonValue") && x == 0)
    return(FALSE)
  if (is.logical(x) && !any(stats::na.omit(x)))
    return(FALSE)
  return(TRUE)
}


# to satisfy CMD CHECK when using pipe variables
if(getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      'Your Answer'
    )
  )
}
