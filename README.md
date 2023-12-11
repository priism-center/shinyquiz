# shinyquiz

<!-- badges: start -->
[![R-CMD-check](https://github.com/priism-center/shinyQuiz/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/priism-center/shinyquiz/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

shinyquiz is an R package for creating simple, flexible quizzes within R Shiny. Easily create quizzes from various pre-built question and choice types or create your own using [htmltools](https://rstudio.github.io/htmltools/) and [shiny](https://shiny.posit.co/) packages as building blocks. Integrates with Shiny applications. Ideal for non-web-developers such as educators, data scientists, and anyone who wants to assess responses interactively in a small form factor.

shinyquiz is excellent for developing short, informal quizzes with a priority of showing one question at a time. At the end of each question, users are notified if their answer is correct. Once the quiz ends, a summary page shows the user's grade, their answer, and the correct answer. 

The quiz can be a standalone application or integrated into a larger Shiny application.

There is no built-in method for permanently recording users' answers in shinyquiz. By default, the package adheres to R Shiny's session data management. This means that user inputs and responses are held temporarily for the duration of the active session. Once the session ends – typically when the user closes the browser or the app is stopped – these inputs are not retained. If persistent data storage is required, such as for later analysis or record-keeping, users will need to implement their own data persistence logic. This might involve capturing the output of `quiz_server()` and saving responses to a database or writing them to a file.

Learn more with the [Get Started](https://priism-center.github.io/shinyquiz/articles/get_started.html) article. 

<br>

## Installation

shinyquiz is currently in development and is available to test by installing via:

``` r
# latest development version
# install.packages("remotes")
remotes::install_github('priism-center/shinyquiz')
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
