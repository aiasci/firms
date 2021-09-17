library(data.table)
library(readxl)
library(plotly)


a<- data.table(read.csv("https://raw.githubusercontent.com/aiasci/firms/main/other_var.csv"))
colnames(a)[1] <- "year"

firmtype <- function(x){
  if(x == "A")
    paste0("Agriculture Forestry and Fishing")
  else if(x == "B")
    paste0("Mining and Quarrying")
  else if(x == "C")
    paste0("Manufacturing")
  else if(x == "D")
    paste0("Electricity, Gas, Steam and Air Conditioning Supply")
  else if(x == "E")
    paste0("Water Supply; Sewerage; Waste Management and Remediation Activities")
  else if(x == "F")
    paste0("Construction")
  else if(x == "G")
    paste0("Trade")
  else if(x == "H")
    paste0("Transporting and Storage")
  else if(x == "I")
    paste0("Accommodation and Food Service Activities")
  else if(x == "J")
    paste0("Information and Communication")
  else if(x == "L")
    paste0("Real Estate Activities")
  else if(x == "M")
    paste0("Professional, Scientific and Technical Activities")
  else if(x == "N")
    paste0("Administrative and Support Service Activities")
  else if(x == "P")
    paste0("Education")
  else if(x == "Q")
    paste0("Human Health and Social Work Activities")
  else if(x == "R")
    paste0("Arts, Entertainment and Recreation")
  else if(x == "S")
    paste0("Other Services Activities")
  
}


graphtype = function(x){
  if (x == unique(a$code)[1])
    paste0("exp")
  else if (x == unique(a$code)[2])
    paste0("gr_gp")
  else if (x == unique(a$code)[3])
    paste0("op")
  else if (x == unique(a$code)[4])
    paste0("fxp")
  else if (x == unique(a$code)[5]) 
    paste0("fxl")
  else if (x == unique(a$code)[6]) 
    paste0("nop_s")
  else if (x == unique(a$code)[7]) 
    paste0("nop")
  else if (x == unique(a$code)[8]) 
    paste0("nop_us")
  else if (x == unique(a$code)[9])
    paste0("nep")
  else if (x == unique(a$code)[10]) 
    paste0("ebit")
  else if (x == unique(a$code)[11])
    paste0("p")
  else if (x == unique(a$code)[12])
    paste0("nnp")
  else if (x == unique(a$code)[13]) 
    paste0("tns")
  else if (x == unique(a$code)[14]) 
    paste0("cta")
  else if (x == unique(a$code)[15])
    paste0("ita")
  else if (x == unique(a$code)[16]) 
    paste0("capta")
  else if (x == unique(a$code)[17])
    paste0("strta")
  else if (x == unique(a$code)[18]) 
    paste0("ltrta")
  else if (x == unique(a$code)[19])
    paste0("stdta")
  else if (x == unique(a$code)[20])
    paste0("ltdta")
  else if (x == unique(a$code)[21]) 
    paste0("sorta")
  else if (x == unique(a$code)[22])
    paste0("lorta")
  else if (x == unique(a$code)[23])
    paste0("sodta")
  else if (x == unique(a$code)[24]) 
    paste0("lotda")
  else if (x == unique(a$code)[25])
    paste0("acid")
  else if (x == unique(a$code)[26])
    paste0("cur")
  else if (x == unique(a$code)[27]) 
    paste0("ltrfl")
  else if (x == unique(a$code)[28]) 
    paste0("strfl")
  else if (x == unique(a$code)[29]) 
    paste0("equ")
  else if (x == unique(a$code)[30]) 
    paste0("ebitda")
  else if (x == unique(a$code)[31]) 
    paste0("ebitdad")}



for (i in c(1:length(unique(a$name)))) {
  e=data.table()
  e= a[name == unique(a$name)[i]]
  for (j in c(1:length(unique(e$code)))) {
    d=data.table()
    d = e[code == unique(e$code)[j]]
    assign(paste0(unique(a$name)[i],"_",graphtype(unique(a$code)[j])),
           plot_ly(data=d,type = 'scatter', mod= 'line',mode = 'markers')%>%
             add_trace(x = ~year,y= ~Large, name = "Large", mode = "lines+markers", color = "#b2abd2",visible = T)%>%
             add_trace(x = ~year,y= ~Medium, name = "Medium", mode = "lines+markers", color = "#b2abd1",visible = T)%>%  
             add_trace(x = ~year,y= ~Small, name = "Small", mode = "lines+markers", color = "#b2abd0",visible = T)%>%
             add_trace(x = ~year,y= ~Micro, name = "Micro", mode = "lines+markers", color = "#b2abd3",visible = T)%>%
             layout(legend = list(orientation = 'h'),
                    title = list(text = paste0(unique(e$code)[j], '<br>', '<sup>',
                                               firmtype(unique(a$name)[i]),'</sup>')),
                    xaxis=list(title="Year", tickvals= list(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)),
                    yaxis=list(title="Percentage"),
                    annotations = list(x = 0, y = -0.09, text = "Source:CBRT,Author's Calculations", 
                                       showarrow = F, xref='paper', yref='paper', 
                                       xshift=0, yshift=0,
                                       font=list(size=15, color="black"))), envir = .GlobalEnv) }}






a<- data.table(read.csv("https://raw.githubusercontent.com/aiasci/firms/main/other_var_2.csv"))
colnames(a)[1] <- "year"

firmsize = function(x){
  if (x ==4)
    paste0("l")
  else if (x==5)
    paste0("m")
  else if (x ==6)
    paste0("mm")
  else if (x==7)
    paste0("s")
}

firmsize2 = function(x){
  if (x ==4)
    paste0("Large")
  else if (x==5)
    paste0("Medium")
  else if (x ==6)
    paste0("Micro")
  else if (x==7)
    paste0("Small")
}

for (i in c(1:length(unique(a$name)))) {
  e = a[name == unique(a$name)[i]]
  for (j in c(1:10)) { assign(paste0("e_", j), e[code == unique(e$code)[j]], envir = .GlobalEnv) }
  e_1[,4:7] = (e_1[,4:7]/100)/(1+e_8[,4:7]/100-e_9[,4:7]/100-e_10[,4:7]/100)*100
  e_2[,4:7] = (e_2[,4:7]/100)/(1+e_8[,4:7]/100-e_9[,4:7]/100-e_10[,4:7]/100)*100
  e_3[,4:7] = (e_3[,4:7]/100)/(1+e_8[,4:7]/100-e_9[,4:7]/100-e_10[,4:7]/100)*100
  e_4[,4:7] = (e_4[,4:7]/100)/(1+e_8[,4:7]/100-e_9[,4:7]/100-e_10[,4:7]/100)*100
  e_5[,4:7] = (e_5[,4:7]/100)/(1+e_8[,4:7]/100-e_9[,4:7]/100-e_10[,4:7]/100)*100
  e_6[,4:7] = (e_6[,4:7]/100)/(1+e_8[,4:7]/100-e_9[,4:7]/100-e_10[,4:7]/100)*100
  e_7[,4:7] = (e_7[,4:7]/100)/(1+e_8[,4:7]/100-e_9[,4:7]/100-e_10[,4:7]/100)*100
  e_8[,4:7] = (e_8[,4:7]/100)/(1+e_8[,4:7]/100-e_9[,4:7]/100-e_10[,4:7]/100)*100
  e_9[,4:7] = (e_9[,4:7]/100)/(1+e_8[,4:7]/100-e_9[,4:7]/100-e_10[,4:7]/100)*100
  e_10[,4:7]= (e_10[,4:7]/100)/(1+e_8[,4:7]/100-e_9[,4:7]/100-e_10[,4:7]/100)*100
  for (k in c(4:7)){
    f = data.table()
    f$year       = e_1[,1]
    f$Land       = e_1[,..k]
    f$LandI      = e_2[,..k]
    f$Buildings  = e_3[,..k]
    f$Machinery  = e_4[,..k]
    f$Motor      = e_5[,..k]
    f$Furniture  = e_6[,..k]
    f$Other      = e_7[,..k]
    f$Depreciation= e_8[,..k]
    f$Cons       = e_9[,..k]
    f$Advances   = e_10[,..k]
    assign(paste0(unique(a$name)[i],"_cap_",firmsize(k)),
           plot_ly(data=f,type = 'scatter', mod= 'line',mode = 'markers')%>%
             add_trace(x = ~year,y= ~Land, name = "Land", mode = "lines+markers", color = "#b2abd1",visible = T)%>%  
             add_trace(x = ~year,y= ~LandI, name = "Land Improvments", mode = "lines+markers", color = "#b2abd0",visible = T)%>%
             add_trace(x = ~year,y= ~Buildings, name = "Buildings", mode = "lines+markers", color = "#b2abd3",visible = T)%>%
             add_trace(x = ~year,y= ~Machinery, name = "Machinery,Plant & Equipments", mode = "lines+markers", color = "#b2abd4",visible = T)%>%  
             add_trace(x = ~year,y= ~Motor, name = "Motor Vehicles" , mode = "lines+markers", color = "#b2abd5",visible = T)%>%
             add_trace(x = ~year,y= ~Furniture, name = "Furniture & Fixtures" , mode = "lines+markers", color = "#b2abd6",visible = T)%>%
             add_trace(x = ~year,y= ~Other, name = "Other Tangible Assets" , mode = "lines+markers", color = "#b2abd7",visible = T)%>%  
             layout(legend = list(orientation = 'h'),title = list(text = paste0("Proportion of Tangible Assets",
                                                                                '<br>',
                                                                                '<sup>',
                                                                                paste0(firmsize2(k)," ",firmtype(unique(a$name)[i])),
                                                                                '</sup>')),
                    xaxis=list(title="Year", tickvals= list(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)),
                    yaxis=list(title="Percentage"),
                    annotations = list(x = 0, y = -0.09, text = "Source:CBRT,Author's Calculations", 
                                       showarrow = F, xref='paper', yref='paper', 
                                       xshift=0, yshift=0,
                                       font=list(size=15, color="black"))),envir = .GlobalEnv)
    assign(paste0(unique(a$name)[i],"_cap_",firmsize(k),"_1"),
           plot_ly(data=f,type = 'scatter', mod= 'line',mode = 'markers')%>%
             add_trace(x = ~year,y= ~Depreciation, name = "Accumulated Depreciation", mode = "lines+markers", color = "#b2abd1",visible = T)%>%  
             add_trace(x = ~year,y= ~Cons, name = "Assets in Construction", mode = "lines+markers", color = "#b2abd0",visible = T)%>%
             add_trace(x = ~year,y= ~Advances, name = "Advances Paid", mode = "lines+markers", color = "#b2abd3",visible = T)%>%
             layout(legend = list(orientation = 'h'),title = list(text = paste0("Proportion of Tangible Assets",
                                                                                '<br>',
                                                                                '<sup>',
                                                                                paste0(firmsize2(k)," ",firmtype(unique(a$name)[i])),
                                                                                '</sup>')),
                    xaxis=list(title="Year", tickvals= list(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)),
                    yaxis=list(title="Percentage"),
                    annotations = list(x = 0, y = -0.09, text = "Source:CBRT,Author's Calculations", 
                                       showarrow = F, xref='paper', yref='paper', 
                                       xshift=0, yshift=0,
                                       font=list(size=15, color="black"))),envir = .GlobalEnv)}}


for (i in c(1:length(unique(a$name)))) {
  e = a[name == unique(a$name)[i]]
  for (j in c(11:20)) {
    assign(paste0("e_", j), e[code == unique(e$code)[j]], envir = .GlobalEnv)}
    for (k in c(4:7)) {
      f = data.table()
      f$year       = e_11[,1]
      f$Land       = e_11[,..k]
      f$LandI      = e_12[,..k]
      f$Buildings  = e_13[,..k]
      f$Machinery  = e_14[,..k]
      f$Motor      = e_15[,..k]
      f$Furniture  = e_16[,..k]
      f$Other      = e_17[,..k]
      f$Depreciation= e_18[,..k]
      f$Cons       = e_19[,..k]
      f$Advances   = e_20[,..k]
      assign(paste0(unique(a$name)[i],"_gr_cap_",firmsize(k)),
             plot_ly(data=f,type = 'scatter', mod= 'line',mode = 'markers')%>%
               add_trace(x = ~year,y= ~Land, name = "Land", mode = "lines+markers", color = "#b2abd1",visible = T)%>%  
               add_trace(x = ~year,y= ~LandI, name = "Land Improvments", mode = "lines+markers", color = "#b2abd0",visible = T)%>%
               add_trace(x = ~year,y= ~Buildings, name = "Buildings", mode = "lines+markers", color = "#b2abd3",visible = T)%>%
               add_trace(x = ~year,y= ~Machinery, name = "Machinery,Plant & Equipments", mode = "lines+markers", color = "#b2abd4",visible = T)%>%  
               add_trace(x = ~year,y= ~Motor, name = "Motor Vehicles" , mode = "lines+markers", color = "#b2abd5",visible = T)%>%
               add_trace(x = ~year,y= ~Furniture, name = "Furniture & Fixtures" , mode = "lines+markers", color = "#b2abd6",visible = T)%>%
               add_trace(x = ~year,y= ~Other, name = "Other Tangible Assets" , mode = "lines+markers", color = "#b2abd7",visible = T)%>%  
               layout(legend = list(orientation = 'h'),title = list(text = paste0("Growth Rate of Tangible Assets",
                                                                                  '<br>',
                                                                                  '<sup>',
                                                                                  paste0(firmsize2(k)," ",firmtype(unique(a$name)[i])),
                                                                                  '</sup>')),
                      xaxis=list(title="Year", tickvals= list(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)),
                      yaxis=list(title="Percentage"),
                      annotations = list(x = 0, y = -0.09, text = "Source:CBRT,Author's Calculations", 
                                         showarrow = F, xref='paper', yref='paper', 
                                         xshift=0, yshift=0,
                                         font=list(size=15, color="black"))),envir = .GlobalEnv)
      assign(paste0(unique(a$name)[i],"_gr_cap_",firmsize(k),"_1"),
             plot_ly(data=f,type = 'scatter', mod= 'line',mode = 'markers')%>%
               add_trace(x = ~year,y= ~Depreciation, name = "Accumulated Depreciation", mode = "lines+markers", color = "#b2abd1",visible = T)%>%  
               add_trace(x = ~year,y= ~Cons, name = "Assets in Construction", mode = "lines+markers", color = "#b2abd0",visible = T)%>%
               add_trace(x = ~year,y= ~Advances, name = "Advances Paid", mode = "lines+markers", color = "#b2abd3",visible = T)%>%
               layout(legend = list(orientation = 'h'),title = list(text = paste0("Growth Rate of Tangible Assets",
                                                                                  '<br>',
                                                                                  '<sup>',
                                                                                  paste0(firmsize2(k)," ",firmtype(unique(a$name)[i])),
                                                                                  '</sup>')),
                      xaxis=list(title="Year", tickvals= list(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)),
                      yaxis=list(title="Percentage"),
                      annotations = list(x = 0, y = -0.09, text = "Source:CBRT,Author's Calculations", 
                                         showarrow = F, xref='paper', yref='paper', 
                                         xshift=0, yshift=0,
                                         font=list(size=15, color="black"))),envir = .GlobalEnv)
    }}

