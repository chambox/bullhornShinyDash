tabPanel("Recruitement",
         fluidRow(
           column(width = 6,
                  box(width = NULL,
                      uiOutput("recruitment_BU"))),
           column(width = 6,
                  box(
                    width = NULL,
                    dateRangeInput(
                      "daterange",
                      "Date range:",
                      start = Sys.Date() - 10,
                      end = Sys.Date()
                    )
                  ))
           ,
           column(
             width = 12,
             box(
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               collapsible = TRUE,
               collapsed = TRUE,
               title = "Recruitment statistics",
               DT::dataTableOutput("recruitment_table")
               
             )
             
             
           ),
           
           column(
             width = 12,
             box(
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               collapsible = TRUE,
               collapsed = TRUE,
               title = "Summary statistics",
               valueBoxOutput("prescreen"),
               valueBoxOutput("firstITV"),
               valueBoxOutput("secondITV"),
               valueBoxOutput("thirdITV"),
               valueBoxOutput("referenceCheck"),
               valueBoxOutput("proposal"),
               valueBoxOutput("contracted")
               
             )
             
             
           ),
           uiOutput("kanbans")
         ))
