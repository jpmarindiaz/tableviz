context("DT")

test_that("DT",{

  data <- fringe(sampleData("Cat-Num-Yea"))

  #
  tableviz_dt(data)

  tableviz_dt(data, search = FALSE)
  tableviz_dt(data, opts = list(buttons = c("csv", "pdf")))

  tableviz_dt(data, theme = "dark")
  tableviz_dt(data, theme = "light")

})
