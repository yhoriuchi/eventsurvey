test_that("package scaffold loads", {
  expect_identical(
    unname(utils::packageDescription("eventsurvey", fields = "Package")),
    "eventsurvey"
  )
})
