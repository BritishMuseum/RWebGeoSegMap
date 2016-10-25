#---------Connecting to Adobe Analytics and creating website geosegmentation Map----------------------

#Author: Alice Daish (British Museum adaish@britishmuseum.org)
#September 2016
#https://github.com/BritishMuseum/RWebGeoSegMap

#------------PACKAGE IN USE IS RSITECATALYST-----------------------------------------------------------
#https://github.com/randyzwitch/RSiteCatalyst

#------------INSTALL PACKAGES--------------------------------------------------------------------------
#install.packages("devtools")
library(devtools)
#install_github("randyzwitch/RSiteCatalyst", ref="master")
library(RSiteCatalyst)

#------Install dependant packages-----------------------------------------------------------------------
#install.packages("jsonlite")
#install.packages("plyr")
#install.packages("httr")
#install.packages("stringr")
#install.packages("digest")
#install.packages("base64enc")
#install.packages("RCurl")
library("jsonlite")
library("plyr")
library("httr")
library("stringr")
library("digest")
library("base64enc")
library("RCurl")

#-------Authentication----------------------------------------------------------------------------------
#Find web service credentials in User Management  in the marketing cloud
#Log in for adobe analytics
SCAuth("username:Company", "secretkey")

#----------Explore Report Suite and Metrics, Evars and Props--------------------------------------------
#Report Suites
report_suites <- GetReportSuites()
report_suites

#Find eVars
eVars<-GetEvars("ReportSuiteName")
eVars

#Find metrics
metrics<-GetMetrics("ReportSuiteName")
metrics$id #list of metric id available

metrics[which(metrics$id =="visits"),] #visits
metrics[which(metrics$id =="uniquevisitors"),] #uniquevisitors

#Find elements
elements <- GetElements("ReportSuiteName")
elements$id #list of elements id available

elements[which(elements$id =="geocountry"),] #country

#GetSuccessEvents - pass a single argument or concatentated vector
events <- GetSuccessEvents("ReportSuiteName")
events <- GetSuccessEvents(report_suites$rsid)
events

#GetProps - pass a single argument or concatentated vector
trafficvars <- GetProps("ReportSuiteName")
trafficvars

#-------------- Finding Visits and Unique Website Visitors by Country----------------------------------------------------------------
countryvisits<-QueueRanked("ReportSuiteName",   #Select Report Suite
                        date.from="2015-09-01", #Start date for example "2015-09-01" to
                        date.to="2015-09-01",   #End date for example "2015-09-01"
                        metrics=c("visits","uniquevisitors"), #Visits and Unique Visitors will be returned
                        elements="geocountry",
                        top=300) #Top 300 (all countries)
#View data
View(countryvisits) 

#Save data - best to save as some of the countries Adobe Analytics and rworldmap packages are differently named
write.csv(countryvisits,"20150901_20150831_countriesdata.csv")

#---------------Create a World map of Website GeoSegmentation Visit ----------------------------------------------------------------
#install.packages("rworldmap")
library(rworldmap)

#Data Prep: Edit the data file as some countries names don't match from adobe analytics output to the rworldmap
#Need to remove "Slovakia Republic" from Slovakia
#Need to remove "Province of China" from Taiwan
#Need to remove "n Arab Jamahiriya" from Libya
#Need to remove "Peoples Democratic Republic" from Lao

#Load data
data<-read.csv("20150901_20150831_countriesdata.csv")

#View data
View(data)

#--------------Yellow to Red Scale Map for Visits or Visitors ----------------------------------------------------------------------------
#--------------Plot Visits by Country Map
#Plot numbers on yellow to red colour spectrum map (with automated scale legend)

#Create a map-shaped window
mapDevice('x11')
#Joining the data to a map
map <- joinCountryData2Map(data, joinCode="NAME", nameJoinColumn="name")
#Display the visits map
map1<-mapCountryData(map, nameColumnToPlot="visits",mapTitle="Visits by country",catMethod="fixedWidth",addLegend=T)

#-------------Plot Unique Visitors by Country Map
#Plot numbers on yellow to red colour spectrum map  (with automated scale legend)

#Create a map-shaped window
mapDevice('x11')
#Joining the data to a map
map <- joinCountryData2Map(data, joinCode="NAME", nameJoinColumn="name")
#Display the visits map
map1<-mapCountryData(map, nameColumnToPlot="uniquevisitors",mapTitle="Unique visitors by country",catMethod="fixedWidth",addLegend=T)


#--------------Light to Dark Green Scale Map for Visits or Visitors ----------------------------------------------------------------------------
#--------------Plot Visits by Country Map--------------------
#Create a map-shaped window
mapDevice('x11')

#Joining the data to a map
map <- joinCountryData2Map( data,joinCode = "NAME",nameJoinColumn = "name" )
#Creating a user defined colour palette
op <- palette(c("#ffffcc","#c2e699","#78c679","#238443")) #Light green to dark green colour palette

#Find quartile breaks
cut<- quantile(map@data[["visits"]],na.rm=TRUE)
quantile(map@data[["visits"]],na.rm=TRUE) #Give quantile breaks for labelling 

#Classify the data to a factor
map@data[["number"]] <- cut( map@data[["visits"]],cut, include.lowest=TRUE )

#rename the categories quantiles
levels(map@data[["number"]]) <- c("very low (1-99)","low (100-199)","med (200-299)","high (300-399)") #Type the legend colour quantile breaks

#Create Map
mapCountryData( map, nameColumnToPlot="number",catMethod="categorical",mapTitle="Visits by country",colourPalette="palette", oceanCol="lightblue",missingCountryCol="white")
mtext("Data Source :Adobe Analytics : Website Stats",side=1,line=-1) #Add data source text

#---------Plot Unique Visitors by Country Map-----------------
#Create a map-shaped window
mapDevice('x11')

#Joining the data to a map
map <- joinCountryData2Map( data,joinCode = "NAME",nameJoinColumn = "name" )

#Creating a user defined colour palette
op <- palette(c("#ffffcc","#c2e699","#78c679","#238443")) #Light green to dark green colour palette

#Find quartile breaks
cut<- quantile(map@data[["uniquevisitors"]],na.rm=TRUE)
quantile(map@data[["visits"]],na.rm=TRUE) #Give quantile breaks for labelling 

#Classify the data to a factor
map@data[["number"]] <- cut( map@data[["uniquevisitors"]],cut, include.lowest=TRUE )

#Rename the categories quantiles
levels(map@data[["number"]]) <- c("very low (1-99)","low (100-199)","med (200-299)","high (300-399)")

#Create Map
mapCountryData( map, nameColumnToPlot="number",catMethod="categorical",mapTitle="Unique Visitors by country",colourPalette="palette", oceanCol="lightblue",missingCountryCol="white")
mtext("Data Source : Adobe Analytics : Website Stats ",side=1,line=-1) #Add data source text


#--------------------------------------------END-------------------------------------------------------------------------------------------------




