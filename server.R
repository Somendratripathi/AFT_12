library(shiny)
##library(shinyIncubator)
#library(XLConnect)
library(forecast)
library(TTR)
library(stringr)
library(shinyjs)

shinyServer(function(input, output,session) {
#path<-"crap"
mape.map <<- data.frame()
Itp_ds<<-('Waiting to load the data...')
# output$contents <- renderPrint({
   #dynamic select
  output$inSelect <- renderUI({
    selectInput("inSelect", HTML('</br><H6  style="font-family:arial"><CENTER><B>SELECT THE METRIC TO SEE FORECAST</B><CENTER></H6>'),
                choices = Itp_ds )})

  # })

plotfor_all<<-list()
plotfit_all<<-list()
  
 	observeEvent(input$refresh, {
      shinyjs::js$refresh()
    })
		   
observe({ 	
if(input$Pressed==0){return(0)}
		if(input$Pressed==1){   
#############################################################################################################
#################################Module 1 --All forecasting functions########################################
#############################################################################################################

inFile <- input$file1
path <<- inFile$datapath
ptm <- proc.time()
ds <- read.csv(file= path, header = T,sep=",")

colnames(ds)<-str_replace_all(colnames(ds),"[^[:alnum:]]", "")

Itp_ds<<-colnames(ds)
print(paste(colnames(ds)))
print(Itp_ds)
if (is.null(inFile))
 return(NULL)

   #dynamic select
  output$inSelect <- renderUI({
    selectInput("inSelect", HTML('</br><H6  style="font-family:arial"><CENTER><B>SELECT THE METRIC TO SEE FORECAST</B><CENTER></H6>'),
                choices = Itp_ds[2:length(Itp_ds)])})
	  
###add cyc for ets/tbats
#ETS Auto Holtwinters: 15 models (5 trends x 3 seasonal) compared with multiplicative & additive errors 
Somholtwinter <- function(Var_ts,horizon,controlsz){
L_ts<-length(Var_ts)
txn_holt<-ets(Var_ts[1:(L_ts-controlsz)])
txn_adj_holt.for<-forecast(txn_holt, h=(horizon+controlsz),level=85)
list_holt<-list(txn_adj_holt.for$mean[(controlsz+1):length(txn_adj_holt.for$mean)],mape(Var_ts,txn_adj_holt.for,controlsz),txn_adj_holt.for,txn_holt)
return(list_holt)
}


#TBATS for Time-series with complex seasonality (weekly/daily data) 
Somtbats <- function(Var_ts,horizon,controlsz){
L_ts<-length(Var_ts)
txn_holt<-tbats(Var_ts[1:(L_ts-controlsz)])
txn_adj_holt.for<-forecast(txn_holt, h=(horizon+controlsz))
list_holt<-list(txn_adj_holt.for$mean[(controlsz+1):length(txn_adj_holt.for$mean)],mape(Var_ts,txn_adj_holt.for,controlsz),txn_adj_holt.for,txn_holt)
return(list_holt)
}

# Auto Arima: winner model picked basis mininum Akaike Information Critera (AIC) 
SomArima <- function(Var_ts,horizon,controlsz){
L_ts<-length(Var_ts)
txn_holt<-auto.arima(Var_ts[1:(L_ts-controlsz)])
txn_adj_holt.for<-forecast.Arima(txn_holt, h=(horizon+controlsz))
list_holt<-list(txn_adj_holt.for$mean[(controlsz+1):length(txn_adj_holt.for$mean)],mape(Var_ts,txn_adj_holt.for,controlsz),txn_adj_holt.for,txn_holt)
return(list_holt)
}

#Neural networks
Somneural_net <- function(Var_ts,horizon,controlsz){
L_ts<-length(Var_ts)

txn_holt<-nnetar(Var_ts[1:(L_ts-controlsz)],repeats=10)
txn_adj_holt.for<-forecast(txn_holt, h=(horizon+controlsz))
list_holt<-list(txn_adj_holt.for$mean[(controlsz+1):length(txn_adj_holt.for$mean)],mape(Var_ts,txn_adj_holt.for,controlsz),txn_adj_holt.for,txn_holt)
return(list_holt)
}



#Linear Regression with trend & seasonality
Som_lm <- function(Var_ts,horizon,controlsz){

L_ts<-length(Var_ts)
xxx<-ts(Var_ts[1:(L_ts-controlsz)],frequency=12)
txn_holt<-tslm(xxx ~ trend + season)
txn_adj_holt.for<-forecast(txn_holt, h=(horizon+controlsz))
list_holt<-list(txn_adj_holt.for$mean[(controlsz+1):length(txn_adj_holt.for$mean)],mape(Var_ts,txn_adj_holt.for,controlsz),txn_adj_holt.for,txn_holt)
return(list_holt)
}



#data cleaning function
clean_data<-function(Data)
{
Data<-data.frame(Data)
len_DATA<-nrow(Data)
Data_NARM<-data.frame(Data[is.na(Data)==F])
len_DATANARM<- nrow(Data_NARM)
#match the index of the first non NA data point
strt_index<-match(Data_NARM[1,1],Data[,1])
lst_index <-len_DATA- match(Data_NARM[len_DATANARM,1],Data[len_DATA:1,1])+1
Data_clean<-Data[strt_index:lst_index,]
Data_clean[is.na(Data_clean)==T]<-0
return(Data_clean)
}


#good forecasting method will yield residuals with the following properties:
#The residuals are uncorrelated. If there are correlations between residuals, then there is information left in the residuals which should be used in computing forecasts.
#The residuals have zero mean. If the residuals have a mean other than zero, then the forecasts are biased.
#The residuals have constant variance.
#The residuals are normally distributed.

#Calculating mape
mape<- function(time_Series,forecasts,controlsz)
{

#mean(abs(fitted(forecasts)-time_series))

L_ts<-length(time_Series)
x<-sample.int((L_ts-controlsz), size = controlsz, replace = FALSE, prob = NULL)

m_in<- ((abs(fitted(forecasts)[x]-time_Series[x])*1.00)/time_Series[x])*100
m_out<-((abs(forecasts$mean[1:controlsz]-time_Series[(L_ts-controlsz+1):L_ts])*1.00)/time_Series[(L_ts-controlsz+1):L_ts])*100

MAPE<-mean(rbind(m_in,m_out))
return(MAPE)
}


#############################################################################################################
#################################Module 2 -- getting data refining ########################################
#############################################################################################################

print(paste(path))
#Read the data
#print("Please enter the File name along with the path:")
ptm <- proc.time()
#ds <- read.csv(file= path, header = T,sep=",")

plotfor<-list()
plotfit<-list()

#ds[,1]<-as.Date(paste("01-", ds[,1], sep = ""), format = "%d-%b-%y")

fs <- input$Freq
fs<-as.numeric(fs)

#forecast horizon
horizon<-input$slider1


#Number of metrics 
Otp_ds<<- data.frame(c(Month=ds[,1],rep('x',horizon)))
#Otp_ds<<- data.frame(c(Month=1:length(ds[,1]),rep('x',horizon)))
lotp<-length(ds[,1])



#horizon<-out_pts+horizon


#control required or not
#controlreq<-input$controlreq1


list_holt1<-list()
list_tbats<-list()

#if(as.numeric(input$Mvariate) == 1)
#{ v<-1 }
#else 
#{
#ncol : no need to have the first colum for month etc
v<-ncol(ds)-1 
#} # number of forecast vectors


# library(forecast)
# X <- fourier(x, 3)
# m3 <- tslm(x ~ X + trend)
# plot(forecast(m3,newdata=data.frame(X=I(fourierf(x,3,2880))),h=2880))

#http://robjhyndman.com/hyndsight/longseasonality/


#capability to choose what model 
#capability to choose damping 


 withProgress( session,min = 0,max=v, {

 
 for (i in 1:v)
{ 

if(i==1){
setProgress(message = 'Going to start Forecasting',
                  detail = paste('You have',v,'metric(s) to forecast'))
 setProgress(value = i)
 }
 else {
 setProgress(message = paste(i,'metric(s) Done'),
                 value=i)}
 
 
clean_ds<-clean_data(ds[,i+1]) 
 
#Holdout size
if(!length(grep("[^[:digit:]]", as.character(input$Hosize))))
{out_pts<-as.numeric(input$Hosize)}
else
{out_pts<-round(as.numeric(input$Hosize)*length(clean_ds))}
if(out_pts<3)out_pts<-3 
 
 
#r<-ds[,i+1]
if (fs == 7)
{Var_ts<-msts(clean_ds,seasonal.periods=c(7,365.25)) } else
{Var_ts<-ts(clean_ds,frequency=as.numeric(fs)) }

#starting index of the data 
#st_ind<- match(Var_ts[1],r)
if(input$SplineSmoothing==TRUE){
#####Smoothening data 
Var_ts.components<-stl(Var_ts, s.window="periodic", robust=TRUE)
smoothen<- smooth.spline(Var_ts.components$time.series[,3])
reseasonalized <- smoothen$y + Var_ts.components$time.series[,1] + Var_ts.components$time.series[,2]
reseasonalized <-ts(reseasonalized,frequency=as.numeric(fs))
Var_ts<-reseasonalized
}
#####testing for enough data points 
if((length(Var_ts)-out_pts) <=2*as.numeric(fs) ) #cheking data points- out sample 
{
#no data cleansing 
#no out sample testing 


print(paste("Forecasting for ",colnames(ds)[i+1],horizon,out_pts,"Using Auto ETS : Not enough data points"))
list_holt1[[2]]<-c(MAPE=Inf)
list_holt1[[1]]<-rep(NA,horizon - out_pts)
#dim(list_holt1[[2]])<-c(1,1)
#rownames(list_holt1[[2]]) <- c("NsdfULLNAME")

print(paste("Forecasting for ",colnames(ds)[i+1],"Using TBATS : Not enough data points"))
list_tbats[[2]]<-c(MAPE=Inf)
list_tbats[[1]]<-rep(NA,horizon - out_pts)
#dim(list_tbats[[2]])<-c(1,1)
#rownames(list_tbats[[2]]) <- c("NsdfULLNAME")

print(paste("Forecasting for ",colnames(ds)[i+1],"Using Auto Arima : Less data points"))
list_arim1<-SomArima(Var_ts,horizon,out_pts)

print(paste("Forecasting for ",colnames(ds)[i+1],"Using Neural networks : Less data points"))
list_nnet<-Somneural_net(Var_ts,horizon,out_pts)
print(paste("238"))
print(paste("Forecasting for ",colnames(ds)[i+1],"Using Linear Regression : Less data points"))
list_lm<-Som_lm(Var_ts,horizon,out_pts)
print(paste("241"))
}
else
{




####enough data points
print(paste("Forecasting for ",colnames(ds)[i+1],"Using Auto ETS"))
list_holt1<-Somholtwinter(Var_ts,horizon,out_pts)

print(paste("Forecasting for ",colnames(ds)[i+1],"Using TBATS"))
list_tbats<-Somtbats(Var_ts,horizon,out_pts)

print(paste("Forecasting for ",colnames(ds)[i+1],"Using Auto Arima"))
list_arim1<-SomArima(Var_ts,horizon,out_pts)

print(paste("Forecasting for ",colnames(ds)[i+1],"Using Neural networks"))
list_nnet<-Somneural_net(Var_ts,horizon,out_pts)
print(paste("265"))

print(paste("Forecasting for ",colnames(ds)[i+1],"Using Linear Regression"))
list_lm<-Som_lm(Var_ts,horizon,out_pts)
print(paste("268"))
}
print(paste("270"))

if(i==1){
Holt_accuracy<-data.frame(list_holt1[[2]])
tbats_accuracy<-data.frame(list_tbats[[2]])
Arima_accuracy<-data.frame(list_arim1[[2]])
nnet_accuracy<- data.frame(list_nnet[[2]])
lm_accuracy<-data.frame(list_lm[[2]])
}
else
{Holt_accuracy<-rbind(Holt_accuracy,list_holt1[[2]])
tbats_accuracy<-rbind(tbats_accuracy,list_tbats[[2]])
Arima_accuracy<-rbind(Arima_accuracy,list_arim1[[2]])
nnet_accuracy<-rbind(nnet_accuracy,list_nnet[[2]])
lm_accuracy<-rbind(lm_accuracy,list_lm[[2]])
}
print(paste("286"))
rownames(Holt_accuracy)[i] <- paste("ETS",colnames(ds)[i+1])
rownames(tbats_accuracy)[i] <- paste("TBATS",colnames(ds)[i+1])
rownames(Arima_accuracy)[i] <- paste("Auto Arima",colnames(ds)[i+1])
rownames(nnet_accuracy)[i] <- paste("Nnet",colnames(ds)[i+1])
rownames(lm_accuracy)[i] <- paste("LM",colnames(ds)[i+1])

mape.map[i,"ETS"]<<-list_holt1[[2]]
mape.map[i,"TBATS"]<<-list_tbats[[2]]
mape.map[i,"Auto Arima"]<<-list_arim1[[2]]
# print(c(paste("315",list_nnet[[2]])))
# if(list_nnet[[2]]==NULL){
# mape.map[i,"nnet"]<<-999}
# else{
# 
# }
mape.map[i,"nnet"]<<-list_nnet[[2]]
print(paste("321"))

mape.map[i,"lm"]<<-list_lm[[2]]


# plotfor<-list(list_holt1[[3]],list_tbats[[3]],list_arim1[[3]],list_nnet[[3]],list_lm[[3]])
# plotfit<-list(list_holt1[[4]],list_tbats[[4]],list_arim1[[4]],list_nnet[[4]],list_lm[[4]])
#plotbb<-list(plotlist,plotlistb)

min_mape<<-min(mape.map[i,],na.rm=T)

#Check for holt vs. arima vs. nnet vs. 
Data<-ds[i+1]
len_DATA<-nrow(Data)
Data_NARM<-data.frame(Data[is.na(Data)==F])
len_DATANARM<- nrow(Data_NARM)
lst_index <-len_DATA- match(Data_NARM[len_DATANARM,1],Data[len_DATA:1,1])+1
print(paste("308"))

if(min_mape==list_arim1[[2]])
{
print(paste("332"))
Otp_ds<<-cbind(Otp_ds,c(as.numeric(ds[1:lst_index,i+1]),as.numeric(list_arim1[[1]]),rep(0,lotp-lst_index)))
colnames(Otp_ds)[i+1] <<- paste(colnames(ds)[i+1],"Auto Arima")
plotfor_all[[length(plotfor_all)+1]] <<- list_arim1[[3]]
plotfit_all[[length(plotfit_all)+1]] <<- list_arim1[[4]]
next
}
print(c(paste("329"),list_nnet[[2]]))

if(min_mape==list_lm[[2]])
{
print(paste("348"))
Otp_ds<<-cbind(Otp_ds,c(ds[1:lst_index,i+1],as.numeric(list_lm[[1]]),rep(0,lotp-lst_index)))
colnames(Otp_ds)[i+1] <<- paste(colnames(ds)[i+1],"lm")
plotfor_all[[length(plotfor_all)+1]] <<- list_lm[[3]]
plotfit_all[[length(plotfit_all)+1]] <<- list_lm[[4]]
next
}


if(min_mape==list_holt1[[2]]) #&& is.na(list_holt1[[2]])==F)
{
print(paste("316"))
Otp_ds<<-cbind(Otp_ds,c(as.numeric(ds[1:lst_index,i+1]),as.numeric(list_holt1[[1]]),rep(0,lotp-lst_index)))
colnames(Otp_ds)[i+1] <<- paste(colnames(ds)[i+1],"ETS")
# plotfor<-list(list_holt1[[3]])
# plotfit<-list(list_holt1[[4]])
plotfor_all[[length(plotfor_all)+1]] <<- list_holt1[[3]]
plotfit_all[[length(plotfit_all)+1]] <<- list_holt1[[4]]

next
}

if(min_mape==list_tbats[[2]]) #&& is.na(list_tbats[[2]])==F)
{
print(paste("324"))
Otp_ds<<-cbind(Otp_ds,c(as.numeric(ds[1:lst_index,i+1]),as.numeric(list_tbats[[1]]),rep(0,lotp-lst_index)))
colnames(Otp_ds)[i+1] <<- paste(colnames(ds)[i+1],"TBATS")
plotfor_all[[length(plotfor_all)+1]] <<- list_tbats[[3]]
plotfit_all[[length(plotfit_all)+1]] <<- list_tbats[[4]]
next
}


print(paste("384"))
if(min_mape==list_nnet[[2]])
{
print(paste("340"))
Otp_ds<<-cbind(Otp_ds,c(ds[1:lst_index,i+1],as.numeric(list_nnet[[1]]),rep(0,lotp-lst_index)))
colnames(Otp_ds)[i+1] <<- paste(colnames(ds)[i+1],"nnet")
plotfor_all[[length(plotfor_all)+1]] <<- list_nnet[[3]]
plotfit_all[[length(plotfit_all)+1]] <<- list_nnet[[4]]
next
}


}
print(path)
print(paste("344"))

Otp <<- Otp_ds
colnames(Otp) <<- colnames(ds)
# print(colnames(ds))
# print(Itp_ds)
# print(Otp)

#print (colnames(Otp_ds))
#print (colnames(Otp))

names(Otp) <<- gsub('\\.', '', names(Otp))
names(Otp) <<- gsub('\\ ', '', names(Otp))

names(Otp_ds) <<- gsub('\\.', '', names(Otp_ds))
names(Otp_ds) <<- gsub('\\ ', '', names(Otp_ds))







output$plot <- renderPlot({
	# x<- input$inSelect
x<-input$inSelect
#	 ts.plot(x=colnames(Otp)[1], y=colnames(Otp)[match(x, colnames(Otp))], type = 'l' ) 
cvec <- c("gray","pink","red","cyan","black","green")
x<-str_replace_all(x,pattern=" ", repl="")
y<-match(x, colnames(Otp))
 plot(plotfor_all[[y-1]],col=cvec[sample(1:6,1)],type='l')
 lines(fitted(plotfit_all[[y-1]]),col=cvec[sample(1:6,1)],type='o')	
	 # p2$guides(y = list(min = 0, title = ""))
   # p2$guides(x = list(title = "", ticks=""))
   # p2$addParams(height = 300, width = 1000, dom = 'chart2')
   # if(tt=='V2'){
    # plot(1:20,type='l')}else {
	# plot(1:200,type='l')}
 })

setProgress(#message = paste('End of forecasting, time taken:', proc.time() - ptm),
                  detail = paste('Output file has been stored in',path))
			 Sys.sleep(3)	  
				  
setProgress(message = paste('You can now download the file'))				  
				   Sys.sleep(2)	

   return(a)
	
 
				   
})



}


})

# downloadHandler() takes two arguments, both functions.
  # The content function is passed a filename as an argument, and
  #   it should write out data to that filename.
output$downloadData <- downloadHandler(
filename = function() { "output.xls" },

    content = function(file){
         	fname <- paste(file,"xls",sep=".")
	wb <- loadWorkbook(fname, create = TRUE)
      createSheet(wb, name = "Output")
      createSheet(wb, name = "Mape")
      writeWorksheet(wb, Otp_ds, sheet = "Output") # writes output
      writeWorksheet(wb, mape.map, sheet = "Mape") # writes mape
      saveWorkbook(wb)
	  file.rename(fname, file)
	  #input$Pressed=0
	  }
	
	
	)
	}

)
