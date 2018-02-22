
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# run this: shiny::runApp(host = "10.203.67.102", port=7621,launch.browser=F)
# share this link:  http://nodcjvl0002.no.trinity-health.org:<port#>

library(shiny)
library(pROC)
library(ggplot2)
library(waffle)
library(scales)
#library(extrafont)

shinyServer(function(input, output) {
  
  genData <- reactive({
    n <- ifelse
    rbind(data.frame(actual=1,pred=rlogis(round(as.numeric(input$n)*input$pos_rate,0),location=log(input$auc/(1.0001-input$auc))*1.6)),
          data.frame(actual=0,pred=rlogis(round(as.numeric(input$n)*(1-input$pos_rate),0),location=0)))
  })
  
  genROC <- reactive({
    df <- genData()
    roc(df$actual, df$pred, direction = "<",algorithm=2)
  })
  genCF <- reactive({
    rocD <- genROC()
    TH <- rocD$thresholds[round(as.numeric(input$n)*(1-input$threshold)+1,0)]
    confusion_matrix <- as.data.frame(table(data.frame(rocD$response, rocD$predictor >= TH),exclude=NULL)[1:2,1:2])
    row.names(confusion_matrix) <- c("Actual: NO", "Actual: YES"); names(confusion_matrix) <- c("Predicted: NO", "Predicted: YES")
    confusion_matrix
  })
  output$histplot <- renderPlot({
    df <- genData()
    rocD <- genROC()
    TH <- rocD$thresholds[round(as.numeric(input$n)*(1-input$threshold)+1,0)]
    ggplot(df, aes(x=pred,fill=factor(actual)))+geom_histogram(alpha=.5,position="identity",bins=30) + 
      scale_fill_manual(values = c("#A5D867", "#6E2585")) + geom_vline(xintercept=TH, color ="red", linetype="dashed", size=1.5) +
      scale_y_continuous(name="Observations") +
      theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_text(face="bold", size=16), 
            axis.text.x=element_text(face="bold", size=16), axis.text.y=element_text(face="bold", size=16))
  })
  output$ROCPlot <- renderPlot({
    r <- genROC()
    df <- data.frame(Specificities=r$specificities, Sensitivities=r$sensitivities)
    DP <- df[round(as.numeric(input$n)*(1-input$threshold)+1,0),]
    ggplot(df, aes(x=Specificities, y=Sensitivities)) + geom_point() + geom_path(color="#6E2585", size=2) + 
      geom_abline(slope = 1, intercept = 1, color = "#A5D867",size=1.5) + 
      scale_y_continuous(name = "Sensitivity (True Positive Rate)", labels=percent, expand=c(0,0)) + 
      scale_x_reverse(name = "Specificity (True Negative Rate)", labels=percent,expand=c(0,0)) + coord_fixed() +
      geom_segment(aes(x=Specificities,y=Sensitivities, xend=1,yend=Sensitivities),color="red",size=1.5, linetype="dashed",data=DP) +
      geom_segment(aes(x=Specificities,y=Sensitivities, xend=Specificities,yend=0),color="red",size=1.5, linetype="dashed",data=DP) +
      theme(axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(face="bold", size=16),
            axis.title.y = element_text(face="bold", size=20), axis.text.y  = element_text(face="bold", size=16))
    
  })
  output$conf_table <- renderTable({
    genCF()
  })
  output$wafflePOS <- renderPlot({
    ws <- ifelse(round(log10(as.numeric(input$n)))<=3,1,10^(round(log10(as.numeric(input$n)),0)-3))
    CF <- genCF()
    cfp <- c(`Actual: YES`=CF["Actual: YES","Predicted: YES"], `Actual: NO` = CF["Actual: NO","Predicted: YES"])
    waffle(round(cfp/ws,0),size=.5, title = "Predicted: YES",colors = c("#6E2585", "#A5D867"),
           xlab = paste('1 sq =',ws,'observation(s)'))
  })
  output$waffleNEG <- renderPlot({
    ws <- ifelse(round(log10(as.numeric(input$n)))<=3,1,10^(round(log10(as.numeric(input$n)),0)-3))
    CF <- genCF()
    cfn <- c(`Actual: NO` = CF["Actual: NO","Predicted: NO"], `Actual: YES`=CF["Actual: YES","Predicted: NO"])
    waffle(round(cfn/ws,0),size=.5, title = "Predicted: NO",colors = c("#A5D867", "#6E2585"), reverse = T,
           xlab = paste('1 sq =',ws,'observation(s)'))
  })

})
