

# filter recruiter
output$vacancy_clientCopFilter <- renderUI({
  clientCop <-
    vacancy_rdata$clientCorporation[!vacancy_rdata$customerDpt == "NULL"]
  clientCop <- as.list(unique(clientCop))
  selectInput(
    "vacancy_clientCopFilter",
    "Select client coperation",
    choices = clientCop,
    selected = "BNP Paribas Fortis"
  )
})
#filter business unit
output$vacancy_businessUnitFilter <- renderUI({
  vacancy_rdata$customText11 %>%
    unique() %>%
    c("All business units") %>%
    as.list() -> chs
  selectInput(
    "vacancy_businessUnitFilter",
    "Select business unit",
    choices = chs,
    selected = "All business units"
  )
})
# Data filtering based on date and business unit
# all done here in this reactive setting
vacancy_filteredData <- reactive({
  if (is.null(input$vacancy_businessUnitFilter)) {
    bu <- "All business units"
  }
  else{
    bu <- input$vacancy_businessUnitFilter
  }
  
  if (is.null(input$vacancy_clientCopFilter)) {
    cltCop <- "BNP Paribas Fortis"
  }
  else{
    cltCop <- input$vacancy_clientCopFilter
  }
  
  
  if (is.null(input$daterange)) {
    startEnd = c(Sys.Date() - 30, Sys.Date())
  }
  else{
    startEnd <- input$daterange
  }
  
  if (bu == "All business units")
    subset0 <- rep(TRUE, length(vacancy_rdata$customText11))
  else
    subset0 <- vacancy_rdata$customText11 == bu
  
  subset1 <-
    vacancy_rdata$dateAdded %within% interval(ymd(startEnd[1]),
                                              ymd(startEnd[2]))
  
  subset2 <- vacancy_rdata$clientCorporation == cltCop
  
  subset <- ifelse(is.na(subset0 & subset1 & subset2),
                   FALSE,
                   subset0 & subset1 & subset2)
  r0 <- vacancy_rdata[subset, ]
  list(r0 = r0, bu = bu)
})


#Data table output
output$vacancy_requestTable <- DT::renderDataTable({
  if (!is.null(vacancy_filteredData()$r0))
  {
    if (dim(vacancy_filteredData()$r0)[1] > 0) {
      table(
        gsub("BNPPF_", "", vacancy_filteredData()$r0$customerDpt),
        vacancy_filteredData()$r0$status.y
      ) %>%
        data.frame() %>%
        reshape(
          v.names = "Freq",
          idvar = "Var1",
          timevar = "Var2",
          direction = "wide"
        ) -> tbl
      names(tbl)[1] <- "Tribe"
      names(tbl)[-1] <- gsub("Freq.", "", names(tbl)[-1])
      tbl
    }
    else {
      tbl <- data.frame("No results")
      names(tbl) <-
        paste("Search results for", vacancy_filteredData()$bu)
      tbl
    }
    
  }
  
},
options = list(pageLength = 5, scrollX = TRUE),
escape = FALSE, server = FALSE, selection = 'none')


output$vacancy_placementTable <- DT::renderDataTable({
  if (!is.null(vacancy_filteredData()$r0))
  {
    if (dim(vacancy_filteredData()$r0)[1] > 0) {
      table(
        gsub("BNPPF_", "", vacancy_filteredData()$r0$customerDpt),
        vacancy_filteredData()$r0$status.x
      ) %>%
        data.frame() %>%
        reshape(
          v.names = "Freq",
          idvar = "Var1",
          timevar = "Var2",
          direction = "wide"
        ) -> tbl
      names(tbl)[1] <- "Tribe"
      names(tbl)[-1] <- gsub("Freq.", "", names(tbl)[-1])
      tbl
    }
    else {
      tbl <- data.frame("No results")
      names(tbl) <-
        paste("Search results for", vacancy_filteredData()$bu)
      tbl
    }
    
  }
  
},
options = list(pageLength = 5, scrollX = TRUE),
escape = FALSE, server = FALSE, selection = 'none')

##Vacancy recieved per department
output$vacancy_perDpt <- renderPlotly({
  vacancy_filteredData()$r0 %>%
    select(Status = status.y,
           department = customerDpt) %>%
    mutate(department = gsub("BNPPF_", "", department)) -> data
  p <-
    ggplot(data = data, aes(x = department, fill = Status)) +
    geom_bar(position = "dodge")
  
  ggplotly(p) %>%
    layout(xaxis = list(title = "", tickangle = -45))
})

output$vacancy_placements <- renderPlotly({
  vacancy_filteredData()$r0 %>%
    select(Status = status.x,
           department = customerDpt) %>%
    mutate(department = gsub("BNPPF_", "", department)) -> data
  p <-
    ggplot(data = data, aes(x = department, fill = Status)) +
    geom_bar(position = "dodge")
  
  ggplotly(p) %>%
    layout(xaxis = list(title = "", tickangle = -45))
})




