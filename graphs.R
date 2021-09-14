

library(data.table)
library(readxl)
library(plotly)


a<- data.table(read.csv("https://raw.githubusercontent.com/aiasci/firms/main/other_var.csv"))
a<- a[,c(2:5)]
colnames(a)[1] <- "year"

firmtype = c()
for (i in c(1:16)){
  firmtype[i] = (i-1)*4+1
}

firmname = function(x){
  if (x==1) 
    paste0("A-Agriculture, forestry and fishing")
  else if (x==5)
    paste0("B-Mining and quarrying")
  else if (x==9)
    paste0("C-Manufacturing")
  else if (x==13)
    paste0("D-Electricity, gas, steam and air conditioning supply")
  else if (x==17)
    paste0("E-Water supply; sewerage; waste management and remediation activities")
  else if (x==21)
    paste0("F-Construction")
  else if (x==25)
    paste0("G-Wholesale and retail trade; repair of motor vehicles and motorcycles")
  else if (x==29)
    paste0("H-Transporting and storage")
  else if (x==33)
    paste0("I-Accommodation and food service activities")
  else if (x==37)
    paste0("J-Information and communication")
  else if (x==41)
    paste0("L-Real estate activities")
  else if (x==45)
    paste0("M-Professional, scientific and technical activities")
  else if (x==49)
    paste0("N-Administrative and support service activities")
  else if (x==53)
    paste0("P-Education")
  else if (x==57)
    paste0("Q-Human health and social work activities")
  else if (x==61)
    paste0("R-Arts, entertainment and recreation")
}
firmsign = function(x){
  if (x==1) 
    paste0("a")
  else if (x==5)
    paste0("b")
  else if (x==9)
    paste0("c")
  else if (x==13)
    paste0("d")
  else if (x==17)
    paste0("e")
  else if (x==21)
    paste0("f")
  else if (x==25)
    paste0("g")
  else if (x==29)
    paste0("h")
  else if (x==33)
    paste0("i")
  else if (x==37)
    paste0("j")
  else if (x==41)
    paste0("l")
  else if (x==45)
    paste0("m")
  else if (x==49)
    paste0("n")
  else if (x==53)
    paste0("p")
  else if (x==57)
    paste0("q")
  else if (x==61)
    paste0("r")
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
    paste0("equ")}

for (i in c(1:length(unique(a$code)))) {
  for (j in firmtype){
    c= data.table()
    c$year = a[a$name ==   unique(a$name)[j]][a[a$name == unique(a$name)[j]]$code == unique(a$code)[i]]$year
    c$Large = a[a$name ==  unique(a$name)[j]][a[a$name == unique(a$name)[j]]$code == unique(a$code)[i]]$data
    c$Medium = a[a$name == unique(a$name)[j+1]][a[a$name == unique(a$name)[j+1]]$code == unique(a$code)[i]]$data
    c$Micro = a[a$name ==  unique(a$name)[j+2]][a[a$name == unique(a$name)[j+2]]$code == unique(a$code)[i]]$data
    c$Small = a[a$name ==  unique(a$name)[j+3]][a[a$name == unique(a$name)[j+3]]$code == unique(a$code)[i]]$data
    assign(paste0(firmsign(j),"_",graphtype(unique(a$code)[i])),
           plot_ly(data=c,type = 'scatter', mod= 'line',mode = 'markers')%>%
             add_trace(x = ~year,y= ~Large, name = "Large-Sized", mode = "lines+markers", color = "#b2abd2",visible = T)%>%
             add_trace(x = ~year,y= ~Medium, name = "Medium-Sized", mode = "lines+markers", color = "#b2abd1",visible = T)%>%  
             add_trace(x = ~year,y= ~Small, name = "Small-Sized", mode = "lines+markers", color = "#b2abd0",visible = T)%>%
             add_trace(x = ~year,y= ~Micro, name = "Micro-Sized", mode = "lines+markers", color = "#b2abd3",visible = T)%>%
             layout(title = list(text = paste0(unique(a$code)[i],
                                               '<br>',
                                               '<sup>',
                                               firmname(j),
                                               '</sup>')),
                    xaxis=list(title="Year", tickvals= list(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)),
                    yaxis=list(title="Percentage"),
                    annotations = list(x = 0, y = -0.07, text = "Source:CBRT,Author's Calculations", 
                                       showarrow = F, xref='paper', yref='paper', 
                                       xshift=0, yshift=0,
                                       font=list(size=15, color="black"))), 
           envir = .GlobalEnv)
    
  }
  c= data.table()
  c$year = a[a$name ==   unique(a$name)[65]][a[a$name == unique(a$name)[65]]$code == unique(a$code)[i]]$year
  c$Medium = a[a$name == unique(a$name)[65]][a[a$name == unique(a$name)[65]]$code == unique(a$code)[i]]$data
  c$Micro = a[a$name ==  unique(a$name)[66]][a[a$name == unique(a$name)[66]]$code == unique(a$code)[i]]$data
  c$Small = a[a$name ==  unique(a$name)[67]][a[a$name == unique(a$name)[67]]$code == unique(a$code)[i]]$data
  assign(paste0("s","_",graphtype(unique(a$code)[i])),
         plot_ly(data=c,type = 'scatter', mod= 'line',mode = 'markers')%>%
           add_trace(x = ~year,y= ~Medium, name = "Medium-Sized", mode = "lines+markers", color = "#b2abd1",visible = T)%>%  
           add_trace(x = ~year,y= ~Small, name = "Small-Sized", mode = "lines+markers", color = "#b2abd0",visible = T)%>%
           add_trace(x = ~year,y= ~Micro, name = "Micro-Sized", mode = "lines+markers", color = "#b2abd3",visible = T)%>%
           layout(title = list(text = paste0(unique(a$code)[i],
                                             '<br>',
                                             '<sup>',
                                             "S-Other services activities",
                                             '</sup>')),
                  xaxis=list(title="Year", tickvals= list(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)),
                  yaxis=list(title="Percentage"),
                  annotations = list(x = 0, y = -0.07, text = "Source:CBRT,Author's Calculations", 
                                     showarrow = F, xref='paper', yref='paper', 
                                     xshift=0, yshift=0,
                                     font=list(size=15, color="black"))), 
         envir = .GlobalEnv)
}  

