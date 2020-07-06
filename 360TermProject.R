# install the required packages first
require(jsonlite)
require(httr)
require(data.table)
require(forecast)

get_token <- function(username, password, url_site){
    
    post_body = list(username=username,password=password)
    post_url_string = paste0(url_site,'/token/')
    result = POST(post_url_string, body = post_body)

    # error handling (wrong credentials)
    if(result$status_code==400){
        print('Check your credentials')
        return(0)
    }
    else if (result$status_code==201){
        output = content(result)
        token = output$key
    }

    return(token)
}

get_data <- function(start_date='2020-03-20', token, url_site){
    
    post_body = list(start_date=start_date,username=username,password=password)
    post_url_string = paste0(url_site,'/dataset/')
    
    header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
    result = GET(post_url_string, header, body = post_body)
    output = content(result)
    data = data.table::rbindlist(output)
    data[,event_date:=as.Date(event_date)]
    data = data[order(product_content_id,event_date)]
    return(data)
}


send_submission <- function(predictions, token, url_site, submit_now=F){
    
    format_check=check_format(predictions)
    if(!format_check){
        return(FALSE)
    }
    
    post_string="list("
    for(i in 1:nrow(predictions)){
        post_string=sprintf("%s'%s'=%s",post_string,predictions$product_content_id[i],predictions$forecast[i])
        if(i<nrow(predictions)){
            post_string=sprintf("%s,",post_string)
        } else {
            post_string=sprintf("%s)",post_string)
        }
    }
    
    submission = eval(parse(text=post_string))
    json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
    submission=list(submission=json_body)
    
    print(submission)
    # {"31515569":2.4,"32939029":2.4,"4066298":2.4,"6676673":2.4,"7061886":2.4,"85004":2.4} 

    if(!submit_now){
        print("You did not submit.")
        return(FALSE)      
    }
    

    header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
    post_url_string = paste0(url_site,'/submission/')
    result = POST(post_url_string, header, body=submission)
    
    if (result$status_code==201){
        print("Successfully submitted. Below you can see the details of your submission")
    } else {
        print("Could not submit. Please check the error message below, contact the assistant if needed.")
    }
    
    print(content(result))
    
}

check_format <- function(predictions){
    
    if(is.data.frame(predictions) | is.data.frame(predictions)){
        if(all(c('product_content_id','forecast') %in% names(predictions))){
            if(is.numeric(predictions$forecast)){
                print("Format OK")
                return(TRUE)
            } else {
                print("forecast information is not numeric")
                return(FALSE)                
            }
        } else {
            print("Wrong column names. Please provide 'product_content_id' and 'forecast' columns")
            return(FALSE)
        }
        
    } else {
        print("Wrong format. Please provide data.frame or data.table object")
        return(FALSE)
    }
    
}

# this part is main code
subm_url = 'http://167.172.183.67'

u_name = "Group23"
p_word = "o0ZgrVebDzjG7bmZ"
submit_now = FALSE

username = u_name
password = p_word

token = get_token(username=u_name, password=p_word, url=subm_url)
datas = get_data(token=token,url=subm_url)


library(caTools)
library(data.table)
library(dplyr)
library(ggplot2)
library(rattle)
library(tidyr)
library(tseries)
library(xts)



Bikini=datas[product_content_id==5926527]
Bikini=Bikini[price>0]
ts.plot(Bikini[,c("sold_count")],main="Daily Sales Bikini",xlab="Time",ylab="Sales Quantity")
plot(Bikini[,c("price","sold_count")],main="Daily Sales Bikini",xlab="Price",ylab="Sales Quantity")
View(Bikini)
Bikini=Bikini[sold_count<20]
ts.plot(Bikini[,c("sold_count")],main="Daily Sales Bikini",xlab="Time",ylab="Sales Quantity")
ts.plot(Bikini[,c("sold_count")],main="Daily Sales Bikini",xlab="Time",ylab="Sales Quantity")
plot(Bikini[,c("price","sold_count")],main="Daily Sales Bikini",xlab="Price",ylab="Sales Quantity")
Bikini_train=Bikini[1:148,]
Bikini_test=Bikini[-(1:148),]
fit_Bikini=lm(sold_count~price,data=Bikini_train)
summary(fit_Bikini)
Bikini_ts=ts(Bikini_train[,c("sold_count")])
arima_model_Bikini=auto.arima(Bikini_ts)
summary(arima_model_Bikini)
arima_model_dynamic_Bikini=auto.arima(Bikini_ts,xreg =Bikini_train[["price"]])
summary(arima_model_dynamic_Bikini)
checkresiduals(fit_Bikini)
checkresiduals(arima_model_Bikini)
checkresiduals(arima_model_dynamic_Bikini)
predict(fit_Bikini,Bikini_test)
predictions_Bikini_auto=forecast(arima_model_Bikini,h=30)
predictions_Bikini_dynamic=forecast(arima_model_dynamic_Bikini,h=30,xreg=Bikini_test[["price"]])


Fakir=datas[product_content_id==7061886]
Fakir=Fakir[price>0]
ts.plot(Fakir[,c("sold_count")],main="Daily Sales Fakir",xlab="Time",ylab="Sales Quantity")
plot(Fakir[,c("price","sold_count")],main="Daily Sales Fakir",xlab=" Price",ylab="Sales Quantity")
Fakir=Fakir[sold_count<200]
Fakir=Fakir[price<250]
ts.plot(Fakir[,c("sold_count")],main="Daily Sales Fakir",xlab="Time",ylab="Sales Quantity")
plot(Fakir[,c("price","sold_count")],main="Daily Sales Fakir",xlab=" Price",ylab="Sales Quantity")
Fakir_train=Fakir[1:265,]
Fakir_test=Fakir[-(1:265),]
fit_Fakir=lm(sold_count~price,data=Fakir_train)
summary(fit_Fakir)
checkresiduals(fit_Fakir)
Fakir_ts=ts(Fakir_train[,c("sold_count")])
arima_model_Fakir=auto.arima(Fakir_ts)
summary(arima_model_Fakir)
checkresiduals(arima_model_Fakir)
arima_model_dynamic_fakir <- auto.arima(Fakir_ts,xreg =Fakir_train[["price"]])
summary(arima_model_dynamic_fakir)
checkresiduals(arima_model_dynamic_fakir)
predict(fit_Fakir,Fakir_test)
predictions_Fakir_auto=forecast(arima_model_Fakir,h=60)
predicitons_Fakir_dynamic=forecast(arima_model_dynamic_fakir,h=60,xreg=Fakir_test[["price"]])




Laroche=datas[product_content_id==85004]
Laroche=Laroche[price>0]
Laroche1=Laroche[1:300,]
Laroche2=Laroche[-(1:300),]
ts.plot(Laroche1[,c("sold_count")],main="Daily Sales Laroche first 300",xlab="Time",ylab="Sales Quantity")
ts.plot(Laroche2[,c("sold_count")],main="Daily Sales Laroche last 126",xlab="Time",ylab="Sales Quantity")
plot(Laroche2[,c("price","sold_count")],main="Daily Sales last 126",xlab=" Price",ylab="Sales Quantity")
plot(Laroche1[,c("price","sold_count")],main="Daily Sales first 300",xlab=" Price",ylab="Sales Quantity")
Laroche1=Laroche1[sold_count<100]
Laroche1_train=Laroche1[1:260,]
Laroche1_test=Laroche1[-(1:260),]
plot(Laroche1[,c("price","sold_count")],main="Daily Sales first 300",xlab=" Price",ylab="Sales Quantity")
ts.plot(Laroche1[,c("sold_count")],main="Daily Sales Laroche first 300",xlab="Time",ylab="Sales Quantity")
Laroche1_ts=ts(Laroche1_train[,c("sold_count")])
arima_model_Laroche1=auto.arima(Laroche1_ts)
arima_model_Laroche1
summary(arima_model_Laroche1)
arima_model_dynamic_laroche1 <- auto.arima(Laroche1_ts,xreg =Laroche1_train[["price"]])
arima_model_dynamic_laroche1
summary(arima_model_dynamic_laroche1)
checkresiduals(arima_model_Laroche1)
checkresiduals(arima_model_dynamic_laroche1)
Laroche2_train=Laroche2[1:106,]
Laroche2_test=Laroche2[-(1:106),]
Laroche2_ts=ts(Laroche2_train[,c("sold_count")])
arima_model_Laroche2=auto.arima(Laroche2_ts)
arima_model_Laroche2
summary(arima_model_Laroche2)
arima_model_dynamic_laroche2 <- auto.arima(Laroche2_ts,xreg =Laroche2_train[["price"]])
arima_model_dynamic_laroche2
summary(arima_model_dynamic_laroche2)
checkresiduals(arima_model_Laroche2)
checkresiduals(arima_model_dynamic_laroche2)

fit_laroche1=lm(sold_count~price,data=Laroche1_train)
fit_laroche2=lm(sold_count~price,data=Laroche2_train)
summary(fit_laroche1)
summary(fit_laroche2)
predict(fit_laroche1,Laroche1_test)
predict(fit_laroche2,Laroche2_test)
predictions_Larohce1_auto=forecast(arima_model_Laroche1,h=40)
predictions_Laroche1_dynamic=forecast(arima_model_dynamic_laroche1,h=40,xreg=Laroche1_test[["price"]])

predictions_Larohce2_auto=forecast(arima_model_Laroche2,h=20)
predictions_Laroche2_dynamic=forecast(arima_model_dynamic_laroche2,h=20,xreg=Laroche2_test[["price"]])






Mont=datas[product_content_id==3904356]
Mont=Mont[price>0]
ts.plot(Mont[,c("sold_count")],main="Daily Sales Mont",xlab="Time",ylab="Sales Quantity")
plot(Mont[,c("price","sold_count")],main="Daily Sales Mont",xlab="Price",ylab="Sales Quantity")
View(Mont)
Mont=Mont[sold_count<20]
ts.plot(Mont[,c("sold_count")],main="Daily Sales Mont",xlab="Time",ylab="Sales Quantity")
plot(Mont[,c("price","sold_count")],main="Daily Sales Mont",xlab="Price",ylab="Sales Quantity")
Mont_train=Mont[1:82,]
Mont_test=Mont[-(1:82),]
fit_Mont=lm(sold_count~price,data=Mont_train)
summary(fit_Mont)
Mont_ts=ts(Mont_train[,c("sold_count")])
arima_model_Mont=auto.arima(Mont_ts)
summary(arima_model_Mont)
arima_model_dynamic_Mont=auto.arima(Mont_ts,xreg=Mont_train[["price"]])
summary(arima_model_dynamic_Mont)
checkresiduals(fit_Mont)
checkresiduals(arima_model_Mont)
checkresiduals(arima_model_dynamic_Mont)
predict(fit_Mont,Mont_test)
predictions_Mont_auto=forecast(arima_model_Mont,h=15)
predictions_Mont_dynamic=forecast(arima_model_dynamic_Mont,h=15,xreg=Mont_test[["price"]])





Oralb=datas[product_content_id==32939029]
Oralb=Oralb[price>0]
Oralb=Oralb[sold_count<600]
ts.plot(Oralb[,c("sold_count")],main="Daily Sales Oralb",xlab="Time",ylab="Sales Quantity")
ts.plot(Oralb[,c("sold_count")],main="Daily Sales Oralb",xlab="Time",ylab="Sales Quantity")
plot(Oralb[,c("price","sold_count")],main="Daily Sales Oralb",xlab="Price",ylab="Sales Quantity")
Oralb_train=Oralb[1:181,]
Oralb_test=Oralb[-(1:181),]
fit_Oralb=lm(sold_count~price,data=Oralb_train)
summary(fit_Oralb)
Oralb_ts=ts(Oralb_train[,c("sold_count")])
arima_model_Oralb=auto.arima(Oralb_ts)
summary(arima_model_Oralb)
arima_model_dynamic_Oralb <- auto.arima(Oralb_ts,xreg =Oralb_train[["price"]])
summary(arima_model_dynamic_Oralb)
checkresiduals(fit_Oralb)
checkresiduals(arima_model_dynamic_Oralb)
predict(fit_Oralb,Oralb_test)
predictions_Oralb_auto=forecast(arima_model_Oralb,h=30)
predictions_Oralb_dynamic=forecast(arima_model_dynamic_Oralb,h=30,xreg=Oralb_test[["price"]])




Sleepy=datas[product_content_id==4066298]
Sleepy=Sleepy[price>0]
ts.plot(Sleepy[,c("sold_count")],main="Daily Sales Sleepy",xlab="Time",ylab="Sales Quantity")
plot(Sleepy[,c("price","sold_count")],main="Daily Sales Sleepy",xlab="Price",ylab="Sales Quantity")
Sleepy1=Sleepy[price<60]
Sleepy2=Sleepy[price>=60]
plot(Sleepy1[,c("price","sold_count")],main="Daily Sales Sleepy under 60",xlab="Price",ylab="Sales Quantity")
ts.plot(Sleepy1[,c("sold_count")],main="Daily Sales Sleepy under 60",xlab="Time",ylab="Sales Quantity")
plot(Sleepy2[,c("price","sold_count")],main="Daily Sales Sleepy above 60",xlab="Price",ylab="Sales Quantity")
Sleepy1_train=Sleepy1[1:39,]
Sleepy1_test=Sleepy1[-(1:39),]
Sleepy2_train=Sleepy2[1:189,]
Sleepy2_test=Sleepy2[-(1:189),]
Sleepy1_ts=ts(Sleepy1_train[,c("sold_count")])
arima_model_Sleepy1=auto.arima(Sleepy1_ts)
summary(arima_model_Sleepy1)
checkresiduals(arima_model_Sleepy1)

arima_model_dynamic_Sleepy1 <- auto.arima(Sleepy1_ts,xreg =Sleepy1_train[["price"]])
summary(arima_model_dynamic_Sleepy1)
checkresiduals(arima_model_dynamic_Sleepy1)

Sleepy2_ts=ts(Sleepy2_train[,c("sold_count")])
arima_model_Sleepy2=auto.arima(Sleepy2_ts)
summary(arima_model_Sleepy2)
checkresiduals(arima_model_Sleepy2)

arima_model_dynamic_Sleepy2 <- auto.arima(Sleepy2_ts,xreg =Sleepy2_train[["price"]])
summary(arima_model_dynamic_Sleepy2)
checkresiduals(arima_model_dynamic_Sleepy2)




fit_Sleepy1=lm(sold_count~price,data=Sleepy1_train)
fit_Sleepy2=lm(sold_count~price,data=Sleepy2_train)

summary(fit_Sleepy1)
summary(fit_Sleepy2)

predict(fit_Sleepy1,Sleepy1_test)
predict(fit_Sleepy2,Sleepy2_test)

predictions_Sleepy1_auto=forecast(arima_model_Sleepy1,h=10)
predictions_Sleepy2_auto=forecast(arima_model_Sleepy2,h=60)

predictions_Sleepy1_dynamic=forecast(arima_model_dynamic_Sleepy1,h=10,xreg=Sleepy1[["price"]])
predictions_Sleepy2_dynamic=forecast(arima_model_dynamic_Sleepy2,h=60,xreg=Sleepy2_test[["price"]])








Tayt=datas[product_content_id==31515569]
Tayt=Tayt[price>0]
ts.plot(Tayt[,c("sold_count")],main="Daily Sales Tayt",xlab="Time",ylab="Sales Quantity")
plot(Tayt[,c("price","sold_count")],main="Daily Sales Tayt",xlab="Price",ylab="Sales Quantity")
Tayt=Tayt[sold_count>20]
plot(Tayt[,c("price","sold_count")],main="Daily Sales Tayt",xlab="Price",ylab="Sales Quantity")
ts.plot(Tayt[,c("sold_count")],main="Daily Sales Tayt",xlab="Time",ylab="Sales Quantity")
Tayt_train=Tayt[1:197,]
Tayt_test=Tayt[-(1:197),]
fit_Tayt=lm(sold_count~price,data=Tayt_train)
summary(fit_Tayt)
Tayt_ts=ts(Tayt_train[,c("sold_count")])
arima_model_Tayt=auto.arima(Tayt_ts)
summary(arima_model_Tayt)
checkresiduals(arima_model_Tayt)

arima_model_dynamic_Tayt=auto.arima(Tayt_ts,xreg=Tayt_train[["price"]])
summary(arima_model_dynamic_Tayt)
checkresiduals(arima_model_dynamic_Tayt)

predict(fit_Tayt,Tayt_test)
predictions_Tayt_auto=forecast(arima_model_Tayt,h=60)
predictions_Tayt_dynamic=forecast(arima_model_dynamic_Tayt,h=60,xreg=Tayt_test[["price"]])










Xiaomi=datas[product_content_id==6676673]
Xiaomi=Xiaomi[price>0]
plot(Xiaomi[,c("price","sold_count")],main="Daily Sales Xiaomi",xlab=" Price",ylab="Sales Quantity")
ts.plot(Xiaomi[,c("sold_count")],main="Daily Sales Xiaomi",xlab="Time",ylab="Sales Quantity")
Xiaomi1=Xiaomi[sold_count<2000]
Xiaomi1_train=Xiaomi[1:311,]
Xiaomi1_test=Xiaomi[-(1:311),]
fit_Xiaomi1=lm(sold_count~price,data=Xiaomi1_train)
summary(fit_Xiaomi1)
Xiaomi1_ts=ts(Xiaomi1_train[,c("sold_count")])
arima_model_Xiaomi1=auto.arima(Xiaomi1_ts)
summary(arima_model_Xiaomi1)
checkresiduals(arima_model_Xiaomi1)

arima_model_dynamic_Xiaomi1 <- auto.arima(Xiaomi1_ts,xreg =Xiaomi1_train[["price"]])
summary(arima_model_dynamic_Xiaomi1)
checkresiduals(arima_model_dynamic_Xiaomi1)



Xiaomi2=Xiaomi1[price<200]
Xiaomi2_train=Xiaomi[1:234,]
Xiaomi2_test=Xiaomi[-(1:234),]

fit_Xiaomi2=lm(sold_count~price,data=Xiaomi2_train)
summary(fit_Xiaomi2)

Xiaomi2_ts=ts(Xiaomi2_train[,c("sold_count")])
arima_model_Xiaomi2=auto.arima(Xiaomi2_ts)
summary(arima_model_Xiaomi2)
checkresiduals(arima_model_Xiaomi2)




arima_model_dynamic_Xiaomi2 <- auto.arima(Xiaomi2_ts,xreg =Xiaomi2_train[["price"]])
summary(arima_model_dynamic_Xiaomi2)

checkresiduals(arima_model_dynamic_Xiaomi2)


predict(fit_Xiaomi1,Xiaomi1_test)
predict(fit_Xiaomi2,Xiaomi2_test)

predictions_Xiaomi1_auto=forecast(arima_model_Xiaomi1,h=60)
predictions_Xiaomi2_auto=forecast(arima_model_Xiaomi2,h=60)

predictions_Xiaomi1_dynamic=forecast(arima_model_dynamic_Xiaomi1,h=60,xreg=Xiaomi1_test[["price"]])
predictions_Xiaomi2_dynamic=forecast(arima_model_dynamic_Xiaomi2,h=60,xreg=Xiaomi2_test[["price"]])