tabPanel("Placements",
         fluidRow(
           ###Filters
           fluidRow(
             title = "Filters",
             column(width = 4,
                    box(
                      width = NULL,
                      status = "success",
                      dateRangeInput(
                        "filterData_placements",
                        "Select date range:",
                        start = Sys.Date() - 30,
                        end = Sys.Date()
                      )
                    )),
             column(width = 4,
                    box(
                      width = NULL, status = "success",
                      uiOutput("pl_status")
                    )),
             column(width = 4,
                    box(width = NULL,
                        uiOutput("pl_BU")))
           ),
           ##### Timeline
           fluidRow(column(
             width = 12,
             boxPlus(
               title = "Timeline of placements",
               closable = TRUE,
               width = NULL,
               solidHeader = FALSE,
               #collapsible = TRUE,
               enable_dropdown = TRUE,
               collapsed = TRUE,
               status = "warning",
               dropdown_icon = "wrench",
               dropdown_menu = dropdownItemList(
                 dropdownItem(url = "http://www.google.com", name = "Link to google"),
                 dropdownItem(url = "#", name = "item 2"),
                 dropdownDivider(),
                 dropdownItem(url = "#", name = "item 3")
               ),
               timevisOutput("placements")
             )
             
           ))
           
           
           
         ))
