testthat::test_that("ui template returns valid ui class", {
  
  ui <- tagList(
    certara_header("test title"),
    bslib::page_fluid(),
    certara_footer("testURL")
  )
  
  testthat::expect_s3_class(ui, "shiny.tag.list")
})

testthat::test_that("ui elements returns valid ui classes", {
  
  header <- certara_header("test title")
  footer <- certara_footer("testURL")
  
  testthat::expect_s3_class(header, "shiny.tag")
  testthat::expect_s3_class(footer, "shiny.tag")
})

testthat::test_that("theme_certara() returns valid ui class", {
  
  theme <- suppressWarnings(theme_certara())
  testthat::expect_s3_class(theme, "theme")
})