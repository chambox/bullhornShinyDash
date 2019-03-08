###recruitment body
output$allRecruitmentsBody<-renderUI({
  fluidRow(
    column(width = 12, 
           align = "center", 
           h1("")),
    br(),br(),
    column(
      width = 12,
      boxPlus(
        width = 12,
        title = "All recruitments",
        closable = TRUE,
        status = "warning",
        solidHeader = FALSE,
        collapsible = TRUE,
        DT::dataTableOutput("recruitment_table"),
        br(),
        valueBoxOutput("prescreen"),
        valueBoxOutput("firstITV"),
        valueBoxOutput("secondITV"),
        valueBoxOutput("thirdITV"),
        valueBoxOutput("referenceCheck"),
        valueBoxOutput("proposal"),
        valueBoxOutput("contracted")
      )
      
    ),
    #Statistic column
    column(
      width = 12,
      align = "center",
      box(
        width = 12,
        title = "Recruiter performance",
        status = "warning",
        plotlyOutput("recruitment_perfo")
      )
    )
  )

})

output$perRecruiterBody<-renderUI({
  fluidRow(
    column(width = 12, 
           align = "center", 
           h1("")),
    br(),br(),
    uiOutput("postates")
    )
})