

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
    rownumbers = FALSE,
    search = TRUE,
    info = TRUE,
    buttons = c(), #c('copy', 'csv', 'excel', 'pdf', 'print'),
    lang = NULL,
    title = "",
    subtitle = "",
    caption = "",
    fixed_header = FALSE,
    col_filters = "none",
    theme = "light",
    scrollX = TRUE
  )
  opts <- modifyList(default_opts, opts)
  str(opts)
  caption <- glue_data(opts, "{caption}")
  dom_str <- "t"
  exts <- character(0)
  rownames <- FALSE
  buttons <- NULL
  lang <- NULL
  #col_filters <- "none"
  # if(opts$col_filters %in% c("top", "bottom")){
  #   col_filters <- list(position = col_filters, clear = TRUE, plain = FALSE)
  # }
  if(opts$search){
    dom_str <- paste0("f", dom_str)
  }
  if(opts$info){
    dom_str <- paste0(dom_str,"i")
  }
  if(opts$pagination){
    dom_str <- paste0(dom_str,"p")
  }
  if(!is.null(opts$lang) && opts$lang != "en"){
    langs <- list(es = "Spanish", de = "German", cn = "Chinese", pt = "Portuguese")
    lang <- langs[[opts$lang]]
    lang  <- list(url = glue('//cdn.datatables.net/plug-ins/1.10.11/i18n/{lang}.json'))
  }
  if(!is.null(opts$buttons)){
    if(opts$buttons == ""){
      buttons <- NULL
    }else if(length(opts$buttons)> 0){
      dom_str <- paste0(dom_str,"B")
      exts <- c(exts, "Buttons")
      buttons <- opts$buttons
    }
  }
  if(opts$rownumbers){
    rownames <- as.character(1:nrow(d))
  }
  if(opts$fixed_header){
    exts <- c(exts, "FixedHeader")
  }
  if(!opts$theme %in% c("light", "dark")){
    opts$theme <- "light"
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

  dt_opts <- list(
    dom = dom_str,
    pageLength = opts$pagination_length,
    order = list(),
    language = lang,
    buttons = c(buttons, NULL),
    fixedHeader = opts$fixed_header,
    initComplete = JS(customJS),
    scrollX = TRUE
  )


  datatable(f$data, options = dt_opts,
            extensions = exts,
            filter = opts$col_filters,
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
