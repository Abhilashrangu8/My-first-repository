########   Helper function/ Calculation of Forecast
library(shiny)
library(plotly)
library(forecast)
library(tsoutliers)
library(forecast)
library(data.table)
library(polynom)



### Best fir forecasting

y <- function (x){
  
  
  outlier_detection_combo = function(x){
    
    options(warn = -1)
    promo_effects = 0
    promo_periods = 0
    SLS_periods = 0
    SLS_effects = 0
    outlier_detection = function(x){
      l1 = length(x)
      k = 0
      promo_effects = 0
      ### Define a variable for the percentage of zero sales in the last two yearsobf
      if(length(x) > 24){
        perc_zeroes_last_2_yrs = sum(x[(length(x) - 23):length(x)] == 0)/24
      }
      if((length(x) >=6 && length(x) <= 24 && sum(x==0)/length(x) < 0.8) | (length(x) > 24 && perc_zeroes_last_2_yrs < 0.7 && median(x) <= 20)){
        Q1 = quantile(x, c(0.25, 0.833), names = FALSE)[1]
        Q3 = quantile(x, c(0.25, 0.833), names = FALSE)[2]
        U1 = round(Q3 + 1.5*(Q3-Q1),0)
        L1 = round(Q1 - 1.5*(Q3-Q1),0)
        k = which((x > U1)|(x<L1))
        if(length(k) > 0){
          temp = x
          x[x>U1] = U1
          promo_effects =  temp[k] - U1
        }
      }
      if(length(x) > 24 && perc_zeroes_last_2_yrs < 0.7 && median(x) > 20){
        Q1 = quantile(x, c(0.25, 0.75), names = FALSE)[1]
        Q3 = quantile(x, c(0.25, 0.75), names = FALSE)[2]
        U1 = round(Q3 + 1.5*(Q3-Q1),0)
        L1 = round(Q1 - 1.5*(Q3-Q1),0)
        k = which((x > U1)|(x<L1))
        if(median(x) <= 5){
          temp = x
          x[x>U1] = U1
          promo_effects = temp[k] - U1
        }else{
          if(length(k)==0){
            k = 0 
          }
          ###
          if(k[1]!=0){
            start_month = (end(x)[2] + 12 - (length(x)%%12) + 1)%%12
            modelfitsample <- data.table(sales=x,month=rep(1:12,2 + length(x)%/%12)[start_month:(start_month + length(x) - 1)],
                                         promo=as.integer(c(rep(0,length(x)))))
            modelfitsample[k, promo := seq_len(length(k))]
            ###
            xreg = cbind(promo = model.matrix(~as.factor(modelfitsample[,promo])),
                         month = model.matrix(~as.factor(modelfitsample[, month])))
            xreg = xreg[,-1]
            names_xreg = paste("promo", 1:length(k), sep = "_")
            months_xreg = paste("month", 1:12, sep = "_")
            ###
            for (i in 1:ncol(xreg)){
              if (i <= length(k)){
                colnames(xreg)[i] = names_xreg[i]
              }else{
                colnames(xreg)[i] = months_xreg[i - length(k)]
              }
            }
            ###
            xreg = xreg[,-(1+ length(k))]
            fit = tryCatch(auto.arima(x, xreg = xreg), error =function(e) NA) 
            if(sum(!is.na(fit))>=1){
              promo_effects = round(fit$coef,0)[(length(fit$coef) - 11 - length(k) + 1):(length(fit$coef) - 11)]
              x[k] = pmax(0, x[k] - promo_effects)
            }else{
              k = 0
            }
          }
        }
        'if(sum(x[(length(x) - 23):length(x)] ==0)/24 < 0.7){
        brk = breakpoints(x ~ 1, breaks = 1)
        if(!is.na(brk$breakpoints) && (length(x) - brk$breakpoints) > 12){
        x = ts(x[(brk$breakpoints + 1):length(x)], frequency = 12, end = end(x))
        }
        promo_effects = promo_effects[k>(l1 - length(x))]
        k = k[k>(l1 - length(x))]
        if(is.na(k[1])){
        k = 0
        promo_effects = 0
        }
      }'
      }
      if(length(k) == 0){
        k = 0
        promo_effects = 0
      }
      list(x, promo_effects, k)
    }
    ###
    'if(length(x) > 24){
    perc_zeroes_last_2_yrs = sum(x[(length(x) - 23):length(x)] == 0)/24
    x1 = cumsum(rev(x))
    max_end_zeros = max(which(x1 == 0))
    if(max_end_zeros >= 6){
    x = ts(x[(length(x) - (max_end_zeros) + 1): length(x)], frequency = 12, end = end(x))
    }
    }
    y = rle(as.numeric(x))
    z = y$lengths[y$values == 0]
    max_consec_zeros = max(z)
    m = which(z == max_consec_zeros)[1]
    if(!is.na(m)){
    alpha = which(x == 0)[sum(z[1:m])]
    #     if((alpha == length(x)) && length(x) > 24 && max_consec_zeros > 6){
    #       z1 = which(x == 0)
    #       x = x[z1[(length(z1) - max_consec_zeros + 1):length(z1)]]
    #       x = ts(x, end = end(x), frequency = 12)
    #     }
    if((alpha != length(x)) && length(x) > 24 && perc_zeroes_last_2_yrs < 0.7 & max_consec_zeros > 6){
    x = x[(which(x == 0)[sum(z[1:m])] + 1) : length(x)]
    x = ts(x, end = end(x), frequency = 12)
    }
    }'
  od = outlier_detection(x)
    result = od
    x = result[[1]]
    promo_effects = result[[2]] 
    promo_periods = result[[3]]
    SLS_effects = 0
    SLS_periods = 0
    l1 = length(x)
    l = list(x, promo_effects, promo_periods, SLS_effects, SLS_periods)
    return(l)
    }
  #Input Values x and h 
  
  x = outlier_detection_combo(x)[[1]]
  p8 = outlier_detection_combo(x)[[2]]
  p9 = outlier_detection_combo(x)[[3]]
  
  # Cleansed time series
  t = 3                                                                      # Backcasting periods
  h = 12                                                                     # Planning horizon  
  test = ts(x[1:(length(x)-t)],frequency =12 )                           # Cleansed test time series for backcasting
  actuals = (x[(length(x)-(t-1)):length(x)])  # Actuals for FC Error Calculation
  
  ##forecasting functions
  
  #Rolling Simple Moving Average function of order 3
  SMA = function(x,m=3,h,actuals){
    m = min(3, length(x))
    temp = mean(x[(length(x)-m+1):length(x)])
    y = x
    for(i in 1:h){
      temp = mean(y[(length(y)-m+1):length(y)])
      y = c(y,temp) 
    }
    fcastsma <- (y[(length(x)+1):(length(x)+h)])
    acc1 <- accuracy(fcastsma, actuals, test=1)[1,5]
    acc2 <- accuracy(fcastsma, actuals, test=2)[1,5]
    acc3 <- accuracy(fcastsma, actuals, test=3)[1,5]
    acct <- accuracy(fcastsma, actuals)[1,5]
    
    p1= x
    p2 = fcastsma
    p3 = "Simple Moving Average of Order 3"
    p4 = acc1
    p5 = acc2
    p6 = acc3
    p7 = acct
    l <- list("hist"=p1,"fcast"=p2,"method"=p3,"MAPE_lag1"=p4, "MAPE_lag2"=p5,"MAPE_lag3"=p6,"MAPEavg3m"=p7, "promoeffects"=p8, "promoperoiod"=p9)
    return(l)
  } 
  
  #Weighted Moving Average function of order 3
  WMA = function(x,m=3,h,wts=1:min(m,length(x))){
    wma = function(x,m=3, wts=1:min(m,length(x))){
      m = min(3, length(x))
      y = sum(x[(length(x)-m+1):length(x)]*wts)/cumsum(1:min(m,length(x)))[min(m,length(x))]
      return(y)
    }
    y = x
    for(i in 1:h){ 
      temp = wma(y,m)
      y = c(y,temp)
    }
    fcastwma <- (y[(length(x)+1):(length(x)+h)])
    acc1 <- accuracy(fcastwma, actuals, test=1)[1,5]
    acc2 <- accuracy(fcastwma, actuals, test=2)[1,5]
    acc3 <- accuracy(fcastwma, actuals, test=3)[1,5]
    acct <- accuracy(fcastwma, actuals)[1,5]
    
    p1= x
    p2 = fcastwma
    p3 = "Weighted Moving Average of Order 3"
    p4 = acc1
    p5 = acc2
    p6 = acc3
    p7 = acct
    l <- list("hist"=p1,"fcast"=p2,"method"=p3,"MAPE_lag1"=p4, "MAPE_lag2"=p5,"MAPE_lag3"=p6,"MAPEavg3m"=p7, "promoeffects"=p8, "promoperoiod"=p9)
    return(l)
  }
  
  
  #Arima Function
  AR <- function (x, actuals, h) {
    fit <- auto.arima(x)
    fcastarima <- forecast.Arima(fit, h = h) 
    acc1 <- accuracy(fcastarima, actuals, test=1)[1,5]
    acc2 <- accuracy(fcastarima, actuals, test=2)[1,5]
    acc3 <- accuracy(fcastarima, actuals, test=3)[1,5]
    acct <- accuracy(fcastarima, actuals)[2,5]
    
    p1= fcastarima$x
    p2 = fcastarima$mean
    p3 = "ARIMA Method"
    p4 = acc1
    p5 = acc2
    p6 = acc3
    p7 = acct
    l <- list("hist"=p1,"fcast"=p2,"method"=p3,"MAPE_lag1"=p4, "MAPE_lag2"=p5,"MAPE_lag3"=p6,"MAPEavg3m"=p7, "promoeffects"=p8, "promoperoiod"=p9)
    return(l)
  }
  #ETS Function
  ETS <- function(x, actuals, h) {
    fit <- ets(x, model = "AAA", ic = "aic")
    fcastets <- forecast.ets(fit, h = h)
    acc1 <- accuracy(fcastets, actuals, test=1)[1,5]
    acc2 <- accuracy(fcastets, actuals, test=2)[1,5]
    acc3 <- accuracy(fcastets, actuals, test=3)[1,5]
    acct <- accuracy(fcastets, actuals)[2,5]
    
    p1= fcastets$x
    p2 = fcastets$mean
    p3 = "ETS Method"
    p4 = acc1
    p5 = acc2
    p6 = acc3
    p7 = acct
    l <- list("hist"=p1,"fcast"=p2,"method"=p3,"MAPE_lag1"=p4, "MAPE_lag2"=p5,"MAPE_lag3"=p6,"MAPEavg3m"=p7, "promoeffects"=p8, "promoperoiod"=p9)
    return(l)
  }
  
  
  #Croston Function
  CRO <- function (x, actuals, h) {
    fcastcro <- croston(x,h)$mean
    fcastcro
    acc1 <- accuracy(fcastcro, actuals, test=1)[1,5]
    acc2 <- accuracy(fcastcro, actuals, test=2)[1,5]
    acc3 <- accuracy(fcastcro, actuals, test=3)[1,5]
    acct <- accuracy(fcastcro, actuals)[1,5]
    
    p1= x
    p2 = fcastcro
    p3 = "Croston Method"
    p4 = acc1
    p5 = acc2
    p6 = acc3
    p7 = acct
    l <- list("hist"=p1,"fcast"=p2,"method"=p3,"MAPE_lag1"=p4, "MAPE_lag2"=p5,"MAPE_lag3"=p6,"MAPEavg3m"=p7, "promoeffects"=p8, "promoperoiod"=p9)
    return(l)
  }
  
  
  ##best fit forcast
  bestfit <- function(x ,m=3,h,actuals,wts = 1:min(m.length(x))) {
    MAPE_SMA <- SMA(x=test,m=3,h=t,actuals)$MAPEavg3m
    MAPE_WMA <- WMA(x=test,m=3,h=t,wts)$MAPEavg3m
    MAPE_ARIMA <- AR(x=test,actuals, h=t)$MAPEavg3m
    MAPE_ETS <- ETS(x=test,actuals, h=t)$MAPEavg3m
    MAPE_CRO <- CRO(x=test,actuals, h=t)$MAPEavg3m
    
    MAPE_DT <- data.table(row.names = c("SMA","WMA","ARIMA","ETS","CRO"), MAPE=c(MAPE_SMA,MAPE_WMA,MAPE_ARIMA,MAPE_ETS,MAPE_CRO))
    #return(MAPE_DT)
    fit <- MAPE_DT$row.names[which.min(MAPE_DT$MAPE)]
    if(fit=="SMA") {
      return(SMA(x=x,m=3,h=h,actuals))}
    if(fit=="WMA") {
      return(WMA(x=x,m=3,h=h,wts))}
    if(fit=="ARIMA") {
      return(AR(x=x,actuals, h=h))}
    if (fit=="ETS") {
      return(ETS(x=x,actuals, h=h))}
    if (fit== "CRO"){
      return(CRO(x=x,actuals, h=h))}
    
  }
  FORECAST <-bestfit(x,m,h,actuals,wts)
  return(FORECAST)
  }

