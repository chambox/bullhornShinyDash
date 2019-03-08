#Compute placement filters
dataCom <- reactive({
  subset0<-pl_rdata$BizUnit==input$businessUnit
  subset1<-pl_rdata$start%within%
    interval(ymd(input$daterange[1]),
             ymd(input$daterange[2]))
  list(filter=pl_rdata[
    subset0&
      subset1,])
})


benchCounts <- reactive({
  if(dim(dataCom()$filter)[1]>0)
  {
    t_data<-data.frame(table(substr(dataCom()$filter$end,1,10)))
    t_data%>%mutate(end=Var1,
                    title=paste("Bench ( ",Freq," )" ,sep=""),
                    start=Var1,
                    color=rep("lightsteelblue1",length((Var1))))%>%
      select(-(Var1:Freq))
  }
})

output$calendar<-renderCalender({
  fullcalendar(benchCounts())
})
