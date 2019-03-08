###recruitment body
output$vacancy_requestBody<-renderUI({
  fluidRow(
    column(width = 12, 
           align = "center", 
           h1("")),
    br(),br(),
    column(
      width = 6,
      tabBox(
        width = 12,
        title = "Requests status",
        tabPanel("Table view", DT::dataTableOutput("vacancy_requestTable")),
        tabPanel("Graph view", plotlyOutput("vacancy_perDpt"))
      )
    ),
    column(
      width = 6,
      tabBox(
        width = 12,
        title = "Placement status",
        tabPanel("Table view", DT::dataTableOutput("vacancy_placementTable")),
        tabPanel("Graph view", plotlyOutput("vacancy_placements"))
      )
    )
  )

})

