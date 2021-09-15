

library(data.table)
library(readxl)
library(plotly)


a<- data.table(read.csv("https://raw.githubusercontent.com/aiasci/firms/main/other_var.csv"))
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
    paste0("equ")
  else if (x == unique(a$code)[30]) 
    paste0("ebitda")
  else if (x == unique(a$code)[31]) 
    paste0("ebitdad")}

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
             add_trace(x = ~year,y= ~Large, name = "Large", mode = "lines+markers", color = "#b2abd2",visible = T)%>%
             add_trace(x = ~year,y= ~Medium, name = "Medium", mode = "lines+markers", color = "#b2abd1",visible = T)%>%  
             add_trace(x = ~year,y= ~Small, name = "Small", mode = "lines+markers", color = "#b2abd0",visible = T)%>%
             add_trace(x = ~year,y= ~Micro, name = "Micro", mode = "lines+markers", color = "#b2abd3",visible = T)%>%
             layout(legend = list(orientation = 'h'),title = list(text = paste0(unique(a$code)[i],
                                               '<br>',
                                               '<sup>',
                                               firmname(j),
                                               '</sup>')),
                    xaxis=list(title="Year", tickvals= list(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)),
                    yaxis=list(title="Percentage"),
                    annotations = list(x = 0, y = -0.09, text = "Source:CBRT,Author's Calculations", 
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
           add_trace(x = ~year,y= ~Medium, name = "Medium", mode = "lines+markers", color = "#b2abd1",visible = T)%>%  
           add_trace(x = ~year,y= ~Small, name = "Small", mode = "lines+markers", color = "#b2abd0",visible = T)%>%
           add_trace(x = ~year,y= ~Micro, name = "Micro", mode = "lines+markers", color = "#b2abd3",visible = T)%>%
           layout(legend = list(orientation = 'h'),title = list(text = paste0(unique(a$code)[i],
                                             '<br>',
                                             '<sup>',
                                             "S-Other services activities",
                                             '</sup>')),
                  xaxis=list(title="Year", tickvals= list(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)),
                  yaxis=list(title="Percentage"),
                  annotations = list(x = 0, y = -0.09, text = "Source:CBRT,Author's Calculations", 
                                     showarrow = F, xref='paper', yref='paper', 
                                     xshift=0, yshift=0,
                                     font=list(size=15, color="black"))), 
         envir = .GlobalEnv)
}  



a<- data.table(read.csv("https://raw.githubusercontent.com/aiasci/firms/main/other_var_2.csv"))
colnames(a)[1] <- "year"

firmsign2 = function(x){
  if (x>=1 & x<5) 
    paste0("a")
  else if (x>=5 & x<9)
    paste0("b")
  else if (x>=9 & x<13)
    paste0("c")
  else if (x>=13 & x<17)
    paste0("d")
  else if (x>=17 & x<21)
    paste0("e")
  else if (x>=21 & x<25)
    paste0("f")
  else if (x>=25 & x<29)
    paste0("g")
  else if (x>=29 & x<33)
    paste0("h")
  else if (x>=33 & x<37)
    paste0("i")
  else if (x>=37 & x<41)
    paste0("j")
  else if (x>=41 & x<45)
    paste0("l")
  else if (x>=45 & x<49)
    paste0("m")
  else if (x>=49 & x<53)
    paste0("n")
  else if (x>=53 & x<57)
    paste0("p")
  else if (x>=57 & x<61)
    paste0("q")
  else if (x>=61 & x<65)
    paste0("r")
  else if (x>=65)
    paste0("s")
}

firmsize = function(x){
  if (x%%4 ==1)
    paste0("l")
  else if (x%%4 ==2)
    paste0("m")
  else if (x%%4 ==3)
    paste0("mm")
  else if (x%%4 ==0)
    paste0("s")
}

for (i in c(1:length(unique(a$name)))){
  c= data.table()
  c$year = a[a$name ==   unique(a$name)[i]][a[a$name == unique(a$name)[i]]$code == unique(a$code)[1]]$year[1:11]
  c$Land = a[a$name == unique(a$name)[i]][a[a$name == unique(a$name)[i]]$code == unique(a$code)[1]]$data[1:11]
  c$LandI = a[a$name ==  unique(a$name)[i]][a[a$name == unique(a$name)[i]]$code == unique(a$code)[2]]$data[1:11]
  c$Buildings = a[a$name ==  unique(a$name)[i]][a[a$name == unique(a$name)[i]]$code == unique(a$code)[3]]$data[1:11]
  c$Machinery = a[a$name == unique(a$name)[i]][a[a$name == unique(a$name)[i]]$code == unique(a$code)[4]]$data[1:11]
  c$Motor = a[a$name ==  unique(a$name)[i]][a[a$name == unique(a$name)[i]]$code == unique(a$code)[5]]$data[1:11]
  c$Furniture = a[a$name ==  unique(a$name)[i]][a[a$name == unique(a$name)[i]]$code == unique(a$code)[6]]$data[1:11]
  c$Other = a[a$name == unique(a$name)[i]][a[a$name == unique(a$name)[i]]$code == unique(a$code)[7]]$data[1:11]
  c$Depreciation = a[a$name == unique(a$name)[i]][a[a$name == unique(a$name)[i]]$code == unique(a$code)[8]]$data[1:11]
  c$Cons = a[a$name == unique(a$name)[i]][a[a$name == unique(a$name)[i]]$code == unique(a$code)[9]]$data[1:11]
  c$Advances = a[a$name == unique(a$name)[i]][a[a$name == unique(a$name)[i]]$code == unique(a$code)[10]]$data[1:11]
  c[, re := (1+Depreciation/100-Cons/100-Advances/100)*100]
  c[, Land := Land/re*100]
  c[, LandI := LandI/re*100]
  c[, Buildings := Buildings/re*100]
  c[, Machinery:= Machinery/re*100]
  c[, Motor := Motor/re*100]
  c[, Furniture := Furniture/re*100]
  c[, Other := Other/re*100]
  assign(paste0("fig_",firmsign2(i),"_",firmsize(i)),
         plot_ly(data=c,type = 'scatter', mod= 'line',mode = 'markers')%>%
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
                                                                              unique(a$name)[i],
                                                                              '</sup>')),
                  xaxis=list(title="Year", tickvals= list(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)),
                  yaxis=list(title="Percentage"),
                  annotations = list(x = 0, y = -0.09, text = "Source:CBRT,Author's Calculations", 
                                     showarrow = F, xref='paper', yref='paper', 
                                     xshift=0, yshift=0,
                                     font=list(size=15, color="black"))), 
         envir = .GlobalEnv)
}


for (i in c(1:length(unique(a$name)))){
  c= data.table()
  c$year = a[a$name ==   unique(a$name)[i]][a[a$name == unique(a$name)[i]]$code == unique(a$code)[1]]$year[1:11]
  c$Depreciation = a[a$name == unique(a$name)[i]][a[a$name == unique(a$name)[i]]$code == unique(a$code)[8]]$data[1:11]
  c$Cons = a[a$name == unique(a$name)[i]][a[a$name == unique(a$name)[i]]$code == unique(a$code)[9]]$data[1:11]
  c$Advances = a[a$name == unique(a$name)[i]][a[a$name == unique(a$name)[i]]$code == unique(a$code)[10]]$data[1:11]
  c[, re := (1+Depreciation/100-Cons/100-Advances/100)*100]
  c[, Depreciation := Depreciation/re*100]
  c[, Cons := Cons/re*100]
  c[, Advances := Advances/re*100]
  assign(paste0("fig_",firmsign2(i),"_",firmsize(i),"_1"),
         plot_ly(data=c,type = 'scatter', mod= 'line',mode = 'markers')%>%
           add_trace(x = ~year,y= ~Depreciation, name = "Accumulated Depreciation", mode = "lines+markers", color = "#b2abd1",visible = T)%>%  
           add_trace(x = ~year,y= ~Cons, name = "Assets in Construction", mode = "lines+markers", color = "#b2abd0",visible = T)%>%
           add_trace(x = ~year,y= ~Advances, name = "Advances Paid", mode = "lines+markers", color = "#b2abd3",visible = T)%>%
           layout(legend = list(orientation = 'h'),title = list(text = paste0("Proportion of Tangible Assets",
                                                                              '<br>',
                                                                              '<sup>',
                                                                              unique(a$name)[i],
                                                                              '</sup>')),
                  xaxis=list(title="Year", tickvals= list(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)),
                  yaxis=list(title="Percentage"),
                  annotations = list(x = 0, y = -0.09, text = "Source:CBRT,Author's Calculations", 
                                     showarrow = F, xref='paper', yref='paper', 
                                     xshift=0, yshift=0,
                                     font=list(size=15, color="black"))), 
         envir = .GlobalEnv)
}








