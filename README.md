# shinyQuiz

<!-- badges: start -->
[![R-CMD-check](https://github.com/priism-center/shinyQuiz/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/priism-center/shinyQuiz/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

shinyQuiz is an R package for creating simple, flexible quizzes within R Shiny. Easily create quizzes from various pre-built question and choice types or create your own using `htmltools` and `shiny` as building blocks. Integrates with R Shiny applications. Ideal for non-web-developers such as educators, data scientists, and anyone who wants to assess responses interactively.

Learn more with the [Get Started](https://priism-center.github.io/shinyQuiz/articles/get_started.html) article. 

<br>

## Installation

shinyQuiz is currently in development and is available to test by installing via:

``` r
# latest development version
# install.packages("remotes")
remotes::install_github('priism-center/shinyQuiz')
```
<br>

## Usage

``` r
quiz <- create_quiz(
  create_question(
    'What is the capital of Illinois?',
    add_choice('Chicago'),
    add_choice('Paw Paw'),
    add_choice('Spingfield', correct = TRUE)
  ),
  create_question(
    'Which elements are gases at room temperature? Select all that apply.',
    add_choice('Hydrogen', correct = TRUE),
    add_choice('Mercury'),
    add_choice('Nitrogen', correct = TRUE),
    label = 'Select Hydrogen and Nitrogen'
  ),
  create_question(
    'At what temperature does water boil at sea level?',
    add_slider(min = 90, max = 150, default_position = 120, correct = 100),
    label = 'Select 100'
  )
)
preview_app(quiz)
```

<br>
<p align="center">
<a href="https://apsta.shinyapps.io/shinyQuiz-demo/">
<img src="man/figures/README-recording.gif" style="width: 80%; max-width: 400px;">
</a>
</p>

You can view a live version of this app [here](https://apsta.shinyapps.io/shinyQuiz-demo/).
