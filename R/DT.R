

#' @export
tableviz_dt <- function(data, opts = NULL, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  if(is.null(opts)){
    opts <- list(...)
  }else{
    opts <- modifyList(opts, list(...))
  }

  default_opts <- list(
    pagination = TRUE,
    pagination_length = 5,
    rownumbers = TRUE,
    search = TRUE,
    info = TRUE,
    buttons = c(), #c('copy', 'csv', 'excel', 'pdf', 'print'),
    lang = NULL,
    title = "",
    subtitle = "",
    caption = "",
    fixed_header = FALSE,
    col_filters = FALSE,
    theme = "light"
  )
  opts <- modifyList(default_opts, opts)
  str(opts)
  caption <- glue_data(opts, "{caption}")
  dom_str <- "t"
  exts <- character(0)
  rownames <- FALSE
  buttons <- NULL
  lang <- NULL
  col_filters <- "none"
  if(opts$col_filters){
    col_filters <- list(position = col_filters, clear = TRUE, plain = FALSE)
  }
  if(opts$search){
    dom_str <- paste0("f", dom_str)
  }
  if(opts$info){
    dom_str <- paste0(dom_str,"i")
  }
  if(opts$pagination){
    dom_str <- paste0(dom_str,"p")
  }
  if(!is.null(opts$lang)){
    langs <- list(es = "Spanish", de = "German", cn = "Chinese", pt = "Portuguese")
    lang  <- list(url = paste0('//cdn.datatables.net/plug-ins/1.10.11/i18n/'), langs[[opts$lang]], ('.json'))
  }
  if(length(opts$buttons)> 0){
    dom_str <- paste0(dom_str,"B")
    exts <- c(exts, "Buttons")
    buttons <- opts$buttons
  }
  if(opts$rownumbers){
    rownames = 1:nrow(d)
  }
  if(opts$fixed_header){
    exts <- c(exts, "FixedHeader")
  }


  theme_opts <- list(
    light = list(
      header_background = "#ffffff",
      header_color = "#000000"
    ),
    dark = list(
      header_background = "#000000",
      header_color = "#ffffff"
    )
  )
  customJS_tpl <-  "function(settings, json) {
      $(this.api().table().header()).css({'background-color': '<<header_background>>',
                                           'color': '<<header_color>>'});
      }"
  customJS <- glue_data(theme_opts[[opts$theme]], customJS_tpl, .open = "<<", .close = ">>")
  message(customJS)
  str(theme_opts[[opts$theme]])

  dt_opts <- list(
    dom = dom_str,
    pageLength = opts$pagination_length,
    order = list(),
    language = lang,
    buttons = buttons,
    fixedHeader = opts$fixed_header,
    initComplete = JS(customJS)
  )


  datatable(f$data, options = dt_opts,
            extensions = exts,
            filter = col_filters,
            escape = FALSE,
            rownames = rownames,
            caption = caption)


  # datatable(head(iris, 20), options = list(
  #   initComplete = JS(
  #     "function(settings, json) {",
  #     "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  #     "}")
  # ))

  ## Buttons
  # datatable(data, extensions = 'Buttons', options = list(
  #   dom = 'Bfrtip',
  #   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  # ))
  ## Fixed header
  # datatable(
  #   iris, extensions = 'FixedHeader',
  #   options = list(pageLength = 50, fixedHeader = TRUE)
  # )

}
