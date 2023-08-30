# shinyQuiz

<!-- badges: start -->
[![R-CMD-check](https://github.com/priism-center/shinyQuiz/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/priism-center/shinyQuiz/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

shinyQuiz is an R package for creating flexible quizzes within R Shiny.

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
    'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed id ornare augue, fringilla molestie metus. Donec eget tortor tincidunt, sagittis dui volutpat, finibus est. Select nulla.',
    add_choice('Nulla vel'),
    add_choice('auctor nulla'),
    add_choice('nulla', correct = TRUE)
  ),
  create_question(
    'Molestie metus. Maecenas tincidunt maximus viverra. Sed non gravida quam. Phasellus at iaculis leo. Mauris congue aliquet dui, ut dapibus lorem porttitor sed.',
    add_choice('ATT'),
    add_choice('ATE', correct = TRUE),
    add_choice('ATC', correct = TRUE),
    label = 'Select ATE and ATC'
  ),
  create_question(
    'Sed non gravida quam. Phasellus at iaculis leo.',
    add_slider(10, 50, 30, correct = 20),
    label = 'Select 20'
  )
)

preview_app(quiz)
```

<br>

## TODO 

- Docs 
  - All functions generally need more thorough explanations 
- Vignettes 
  - Getting started: needs some cleanup; link to functions 
  - Customizing Questions: lots of work 
  - Creating Infinite Questions:  lots of work 
  - Set Up a Quiz in a Module: light review needed 
- Tests 
  - how to test preview functions? 
  - state machine needs many tests 
- CRAN 
  - pass check 
  - pass check on linux, mac, windows 
  - review DESCRIPTION file 
  - review [checklist](https://cran.r-project.org/web/packages/submission_checklist.html) 
  