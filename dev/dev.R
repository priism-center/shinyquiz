## this is a meta script to log the commands that create the package ##

# package setup
renv::activate()
install.packages('devtools')
usethis::create_package('.')
usethis::git_vaccinate()

# imports
usethis::use_package("shiny", min_version = '1.7.4')
usethis::use_package("fontawesome", min_version = '0.5.0')
usethis::use_package("dplyr", min_version = '1.1.1')
usethis::use_package("htmltools", min_version = '0.5.5')
usethis::use_package("reactable", min_version = '0.4.4')
usethis::use_package("shinyjs", min_version = '2.1.0')
usethis::use_package('purrr', min_version = '1.0.1')
usethis::use_package('renv', min_version = NULL)
usethis::use_package('glue', min_version = '1.6.2')
usethis::use_package('cli', min_version = '3.6.1')
usethis::use_package('scales', min_version = '1.2.1')
usethis::use_package('methods')
usethis::use_package('stringr', min_version = '1.5.0')
usethis::use_package('tibble', min_version = '3.2.1')
# usethis::use_package('yesno', min_version  = '0.1.2')
usethis::use_package("sortable", min_version = '0.5.0', type = 'Suggests')
usethis::use_package('renv', type = 'Suggests')
usethis::use_package('testthat', min_version = TRUE, type = 'Suggests')
# renv::snapshot()

# create function files
usethis::use_r('constructors')
usethis::use_r('question-creators')
usethis::use_r('state-machine')
usethis::use_r('shiny-module')
usethis::use_r('preview-tools')
usethis::use_r('example-app')
usethis::use_r('utils-ui')
usethis::use_r('utils')

# create tests
usethis::use_testthat()
usethis::use_test('state-machine')
usethis::use_test('shiny-module')
usethis::use_test('preview-tools')
usethis::use_test('question-creators')

# other
usethis::use_github_action_check_release("R-CMD-check.yaml")
usethis::use_github_actions_badge(name = "R-CMD-check.yaml")
usethis::use_lifecycle_badge(stage = 'experimental')
usethis::use_mit_license()

# pkgdown website
usethis::use_pkgdown()
pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)
usethis::use_github_action("pkgdown")
# go to repo Settings -> Pages -> Source: Deploy from a branch & Branch = 'gh-pages'

# testing
devtools::load_all()
devtools::document()

# build package
usethis::use_build_ignore('dev')
usethis::use_build_ignore('.github')
devtools::check()
devtools::build()
