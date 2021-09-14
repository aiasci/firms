

library(data.table)
library(readxl)
library(plotly)


a<- data.table(read_xlsx("/home/ismetovic/Documents/GitHub/firms/other_var.xlsx"))
colnames(a)[1] <- "year"

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
    paste0("equ")}

for (i in c(1:length(unique(a$code)))) {
  c= data.table()
  c$year = a[a$name ==   unique(a$name)[1]][a[a$name == unique(a$name)[1]]$code == unique(a$code)[i]]$year
  c$Large = a[a$name ==  unique(a$name)[1]][a[a$name == unique(a$name)[1]]$code == unique(a$code)[i]]$data
  c$Medium = a[a$name == unique(a$name)[2]][a[a$name == unique(a$name)[2]]$code == unique(a$code)[i]]$data
  c$Micro = a[a$name ==  unique(a$name)[3]][a[a$name == unique(a$name)[3]]$code == unique(a$code)[i]]$data
  c$Small = a[a$name ==  unique(a$name)[4]][a[a$name == unique(a$name)[4]]$code == unique(a$code)[i]]$data
  assign(paste0("a_",graphtype(unique(a$code)[i])),
         plot_ly(data=c,type = 'scatter', mod= 'line',mode = 'markers')%>%
           add_trace(x = ~year,y= ~Large, name = "Large-Sized", mode = "lines+markers", color = "#b2abd2",visible = T)%>%
           add_trace(x = ~year,y= ~Medium, name = "Medium-Sized", mode = "lines+markers", color = "#b2abd1",visible = T)%>%  
           add_trace(x = ~year,y= ~Small, name = "Small-Sized", mode = "lines+markers", color = "#b2abd0",visible = T)%>%
           add_trace(x = ~year,y= ~Micro, name = "Micro-Sized", mode = "lines+markers", color = "#b2abd3",visible = T)%>%
           layout(title = list(text = paste0(unique(a$code)[i],
                                             '<br>',
                                             '<sup>',
                                             "A-Agriculture, forestry and fishing",
                                             '</sup>')),
                  xaxis=list(title="Year", tickvals= list(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)),
                  yaxis=list(title="Percentage"),
                  annotations = list(x = 0, y = -0.07, text = "Source:CBRT, Autors Calculations", 
                                     showarrow = F, xref='paper', yref='paper', 
                                     xshift=0, yshift=0,
                                     font=list(size=15, color="black"))), 
         envir = .GlobalEnv)
  
}








