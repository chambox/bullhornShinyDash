
output$benchTimes <- renderTimevis({
  timevis(data = dataGroups, groups = groups, options = list(editable = TRUE))
})

#Compute placement filters
dataCom <- reactive({
  subset0<-pl_rdata$BizUnit==input$pl_BU
  subset1<-pl_rdata$start%within%
    interval(ymd(input$filterData_placements[1]),
             ymd(input$filterData_placements[2]))
  subset2<-pl_rdata$status==input$pl_status
  list(filter=pl_rdata[
                       subset0&
                       subset1&
                       subset2,])
})

output$pl_status <- renderUI({
  chs<-as.list(unique(pl_rdata$status))
  selectInput("pl_status",
              "Select status",
              choices = chs,
              selected = "In progress"
  )
})



output$placements <- renderTimevis({
  config <- list(
    editable = TRUE,
    align = "center",
    orientation = "top",
    snap = NULL
  )
  timevis(data = dataCom()$filter,
          options = config,height =NULL)
})




#Choices for BUnits
output$pl_BU <- renderUI({

  chs<-as.list(unique(pl_rdata$BizUnit))
  selectInput("pl_BU",
              "Select business unit",
              choices = chs,
              selected = "EBS"
  )
})


#Account owner
# output$pl_acc <- renderUI({
#
#   keyAcc<-as.list(unique(pl_rdata$keyAccountOwner))
#   selectInput("pl_acc",
#               "Select key account owner",
#               choices = keyAcc,
#               selected = keyAcc[[1]]
#   )
# })




