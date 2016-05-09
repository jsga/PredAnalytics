## PReface
source('preface.R')


#### SERVER FUNCTION

server <- function(input, output) {

  ###########
  output$Time_series <- renderDygraph({

    # Collect the appropiate data
    ZZ_train =1:(24*7)
    ZZ_test = 24*7 + 1:72
    
    data <- switch(input$Area, 
                   "Denmark West" = data_all[c(ZZ_train,ZZ_test),2],
                   "Denmark East" = data_all[c(ZZ_train,ZZ_test),3],
                   "Whole Denmark" = data_all[c(ZZ_train,ZZ_test),4])
    X_train= data[ZZ_train]
    X_test = data[ZZ_test]
    
    # Collect inputs for the model coefficients
    AR_coef = input$AR
    MA_coef = input$MA
    coef = c(AR_coef,1,MA_coef)
    
    # Make the seasonal dumies
    XXreg = fourier_me(1:tail(ZZ_test,1), terms=input$Dummy,period=48)

    # Fit the model
    model_forecast = arima(X_train,order=coef, xreg = XXreg[ZZ_train,])
    X_fitted = X_train + model_forecast$residuals
    X_pred <- predict(model_forecast,n.ahead=72,newxreg = XXreg[ZZ_test,])
    # Save for output mape
    reactiveValues(Xpred_react = X_pred$pred)
    # Make the plot
    df = data.frame(Load = data,
                    Pred = round(c(X_fitted,X_pred$pred)),
                    Pred_up = round(c(X_fitted + sqrt(model_forecast$sigma2),X_pred$pred + X_pred$se)),
                    Pred_down = round(c(X_fitted - sqrt(model_forecast$sigma2),X_pred$pred - X_pred$se)))
    
  
    tx = xts(df,order.by=data_all$Time[c(ZZ_train,ZZ_test)])
    dygraph(tx) %>% dyRangeSelector() %>%
      dySeries(c("Pred_down", "Pred", "Pred_up"), label = "Forecast") %>%
      dyEvent(data_all$Time[tail(ZZ_train,1)], "Modeling time", labelLoc = "bottom")
    
    
  })

  ###########
  output$Analysis <- renderPlotly({

    #
    if( input$radio == 1){
      df = data.frame(Hour = as.POSIXlt(data_all$Time)$hour[1:2000],
                      Load = data_all$DK1[1:2000])
    }else{
      df = data.frame(Hour = as.POSIXlt(data_all$Time)$hour[1:2000],
                      Load = data_all$DK2[1:2000])
    }
    # Make a boxplot
    ggplot(df, aes(x=factor(Hour), y=Load))+
      geom_boxplot()+
      #facet_grid(.~Hour)+
      labs(x="Hour of the day")+
      theme(axis.text.x=element_text(angle=-45, vjust=0.4,hjust=1))
    ggplotly()
    
    
  })
  
  ############
  output$table <- DT::renderDataTable(DT::datatable({ # First input is an expr, wrap with brackets
     if( input$radio == 1){
      mean_display = as.numeric(lapply(split( data_all$DK1, as.POSIXlt(data_all$Time)$hour),mean,na.rm=T))
      sd_display = as.numeric(lapply(split( data_all$DK1, as.POSIXlt(data_all$Time)$hour),sd,na.rm=T))
    }else{
      mean_display = as.numeric(lapply(split( data_all$DK2, as.POSIXlt(data_all$Time)$hour),mean,na.rm=T))
      sd_display = as.numeric(lapply(split( data_all$DK2, as.POSIXlt(data_all$Time)$hour),sd,na.rm=T))
    }
    data = data.frame(Hour = 1:24,Mean = mean_display, StandardDeviation = sd_display)
    },
    options = list(rownames=FALSE,searching = FALSE,pageLength = 24,lengthMenu = list(c(24))) )
  )
  
  ###########
  output$mape <- renderValueBox({

    
    #### THIS IS NTO EFFICIENT
    ZZ_train =1:(24*7)
    ZZ_test = 24*7 + 1:72
    
    data <- switch(input$Area, 
                   "Denmark West" = data_all[c(ZZ_train,ZZ_test),2],
                   "Denmark East" = data_all[c(ZZ_train,ZZ_test),3],
                   "Whole Denmark" = data_all[c(ZZ_train,ZZ_test),4])
    X_train= data[ZZ_train]
    X_test = data[ZZ_test]
    
    # Collect inputs for the model coefficients
    AR_coef = input$AR
    MA_coef = input$MA
    coef = c(AR_coef,1,MA_coef)
    
    # Make the seasonal dumies
    XXreg = fourier_me(1:tail(ZZ_test,1), terms=input$Dummy,period=48)
    
    # Fit the model
    model_forecast = arima(X_train,order=coef, xreg = XXreg[ZZ_train,])
    X_fitted = X_train + model_forecast$residuals
    X_pred <- predict(model_forecast,n.ahead=72,newxreg = XXreg[ZZ_test,])
    # Save for output mape
    Xpred_react = X_pred$pred
    
    ####
    mape_display = round(mean(abs((X_test - Xpred_react)/X_test)),digits=3)*100
    valueBox(
      paste(mape_display,"%"), "MAPE", icon = icon("bullseye"),
      color = "purple"
    )
  })
}


# Possible improvements:
# 1. PLay around with other graphs
# 2. Import data in real-time
# 3. Create a messaging option to contact support
# 4. Do some king of reporing feature
# 5. What about solving the inverse problem? Something like "online demand calculator"? This would be nice, also to learn simplex with R.

