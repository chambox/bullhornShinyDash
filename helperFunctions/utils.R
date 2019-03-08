# generate a random string of 16 characters
randomID <- function() {
  paste(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "")
}

prettyDate <- function(d) {
  suppressWarnings(format(as.POSIXct(gsub("T", " ", d), "%Y-%m-%d %H:%M")))
}

utils_1 <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}



calenderOutput <- function(outputId, width = "100%", height = "auto") {
  htmlwidgets::shinyWidgetOutput(outputId, "fullcalendar", width, height)
}

renderCalender <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, calenderOutput, env, quoted = TRUE)
}

compute<-function(c_start,pl_start,pl_end,count)
{
  if(count[1]>1)
    res<-c(difftime(pl_start[1],c_start[1],units = "days"),
           difftime(pl_end[-count[1]],pl_start[-1],units = "days"),
           difftime(Sys.time(),pl_end[count[1]],units = "days"))
  if((!is.na(pl_end[1]))&count[1]==1)
    res<- c(difftime(pl_start,c_start,units = "days"),
            difftime(Sys.time(),pl_end,units = "days"))
  if(is.na(pl_end[1]) & count[1]==1)
    res<- difftime(Sys.time(),c_start[1],units = "days")
  sum(res[res>0])
}

colfxn<-function(width,id,recruiter){
  column(width = width,
         boxPlus(width = NULL,
                 solidHeader = FALSE,
                 collapsible = FALSE,
                 status = "primary",
                 closable = TRUE,
                 title = recruiter,
                 dropdown_icon = "wrench",
                 enable_dropdown = TRUE,
                 dropdown_menu = dropdownItemList(
                   dropdownItem(url = "http://www.google.com", name = "Link to google"),
                   dropdownItem(url = "#", name = "item 2"),
                   dropdownDivider(),
                   dropdownItem(url = "#", name = "item 3")
                 ),
           kanbanWidgetOutput(id,height = "auto")
         )

  )

}


# all_recruitment_renderUI<-function(){
#     fluidRow(
#       column(
#         width = 12,
#         boxPlus(
#           width = 12,
#           title = "All recruitments",
#           closable = TRUE,
#           status = "warning",
#           solidHeader = FALSE,
#           collapsible = TRUE,
#           #enable_sidebar = TRUE,
#           #sidebar_width = 25,
#          # sidebar_start_open = TRUE,
#           #sidebar_content = tagList(),
#           DT::dataTableOutput("recruitment_table"),
#           br(),
#           valueBoxOutput("prescreen"),
#           valueBoxOutput("firstITV"),
#           valueBoxOutput("secondITV"),
#           valueBoxOutput("thirdITV"),
#           valueBoxOutput("referenceCheck"),
#           valueBoxOutput("proposal"),
#           valueBoxOutput("contracted")
#         )
#         
#       )
#     )
# }
bench_renderUI<-function(){
  tabItem(
    tabName = "bench",
    
    br(),
    
    column(
      width = 12,
      align = "center",
      h1("Bench statistics")
    ),
    br(),
    
    fluidRow(
      # demo enhanced classic boxes from shinydashboard
      boxPlus(
        width = 12,
        title = "Consultants on bench",
        closable = TRUE,
        status = "warning",
        solidHeader = FALSE,
        collapsible = TRUE,
        enable_dropdown = TRUE,
        dropdown_icon = "wrench",
        calenderOutput("calendar")
      )
      )
  )
}







