# =====================================================
# Devtools workflow
# library(devtools)
# =====================================================

devtools::document()          # generate documentation
devtools::check_man()         # check documentation
usethis::use_testthat()      # create test infrastructure
devtools::test()              # run tests
devtools::build_vignettes()   # build vignettes
devtools::build()             # build bundle
devtools::install()           # install package
