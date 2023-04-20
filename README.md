# shinyQuiz

<!-- badges: start -->
[![R-CMD-check](https://github.com/priism-center/shinyQuiz/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/priism-center/shinyQuiz/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**STILL IN TESTING**

shinyQuiz is an R package for creating flexible quizzes within R Shiny.

## Example app

There is a example application in `dev/example-app.R`

## Installation

shinyQuiz is currently in development and is available to test by installing via:

``` r
# latest development version
# install.packages("remotes")
remotes::install_github('priism-center/shinyQuiz')
```

## Dev notes

A quiz is managed via a state machine framework. The framework requires an object of S4 class `quiz`. These are constructed using `construct_quiz` and consist of objects of S4 class `question` and `messages`. These have constructor functions as well.

A `quiz` object is created outside of Shiny and passed to a Shiny module. The Shiny module manages the state of the quiz through the central reactive object `store`. The `store` is created via `sm_create_reactive_store` function. The state machine functions (prefixed with `sm_*`) `get` or `set` states. The changing of the `store` object facilitates downstream effects via the Shiny reactive framework. 

For example, the quiz UI is determined by an object in the `store` object. The current state is observed via a `shiny::observeEvent(store$state)`. When the current state changes then a function modifies `store$ui_html`. The UI is rendered via `shiny::renderUI(store$ui_html)` which watches this `ui_html` object.
