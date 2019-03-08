tabPanel("Bench",
         fluidRow(
           ## Benchers calendar
           fluidRow(
             column(width=6,
                    box(width = NULL,solidHeader = TRUE,
                        status = "primary",
                        title = "Number of consultants on the bench",
                        collapsible = TRUE,collapsed = TRUE,
                        calenderOutput("calendar")
                    )
             )
           )

)
)
