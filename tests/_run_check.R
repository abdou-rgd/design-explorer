Sys.setenv(DESIGN_EXPLORER_ROOT = "c:/Users/abdou/Desktop/design-explorer")
library(testthat)
test_file(
  "c:/Users/abdou/Desktop/design-explorer/tests/testthat/test-parse_design_outputs.R",
  reporter = "progress"
)
