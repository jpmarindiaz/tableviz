context("DT")

test_that("DT",{

  data <- fringe(sampleData("Cat-Num-Yea"))
  #
  tableviz_dt(data)

  tableviz_dt(data, rownumbers = TRUE)
  tableviz_dt(data, col_filters = "none")
  tableviz_dt(data, col_filters = "top")
  tableviz_dt(data, col_filters = "bottom")
  tableviz_dt(data, search = FALSE)
  tableviz_dt(data, opts = list(buttons = c("csv", "pdf")))
  tableviz_dt(data, buttons = "")
  tableviz_dt(data, buttons = NULL)
  tableviz_dt(data, buttons = "csv")

  tableviz_dt(data, lang = "en")
  tableviz_dt(data, lang = "es")

  tableviz_dt(data, theme = "dark")
  tableviz_dt(data, theme = "light")

  data <- fringe(sampleData("Cat-Num-Num-Cat-Yea-Cat-Num-Num-Cat-Yea-Cat-Num-Num-Cat-Yea"))
  tableviz_dt(data, scrollX = TRUE)

})
