context("Test p5")

test_that("p5 sketches can be created", {
  p5() %>%
    rect(10, 10, 10, 10)
})
