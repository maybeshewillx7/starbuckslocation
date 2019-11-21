library(shiny)
library(leaflet)
library(data.table)
library(dplyr)
library(tigris)
library(DT)

load("./data/subdat.RData")

filter_data <- read.csv("./data/filter_data_used.csv", as.is = T)
table_display <- read.csv("./data/table_display.csv", as.is = T, check.names = FALSE)
starbucks1=read.csv("./data/starbucks1.csv")
starbucks1$Longitude=as.numeric(starbucks1$Longitude)
starbucks1$Latitude=as.numeric(starbucks1$Latitude)
sicon=makeIcon("./imgfile/sicon.png",iconWidth = 18, iconHeight = 18)

shinyServer(function(input, output,session) {
  
  
  output$map <- renderLeaflet({
    m=leaflet() %>%
      addProviderTiles('OpenStreetMap.HOT') %>%
      setView(lng = -73.98097, lat = 40.7562, zoom = 12)%>%
      addMarkers(data=starbucks1,lng= ~Longitude,lat= ~Latitude,icon=~sicon)
    m
  })
  
  observeEvent(input$click_back_button,{
    if(input$click_back_button){
      leafletProxy("map") %>%
        setView(lng = -73.98097, lat = 40.7562, zoom = 12)
    }
  })
  
  areas <- reactive({
    cond.class1 <- if(is.null(input$check2_class)){"is.na(class_1) == FALSE"
    } else if(!("Upper" %in% input$check2_class)){"class_1 == 0"} else {"is.na(class_1) == FALSE"}
    
    cond.class2 <- if(is.null(input$check2_class)){"is.na(class_2) == FALSE"
    }else if(!("Middle" %in% input$check2_class)){"class_2 == 0"} else {"is.na(class_2) == FALSE"}
    
    cond.class3 <- if(is.null(input$check2_class)){"is.na(class_3) == FALSE"
    } else if(!("Working" %in% input$check2_class)){"class_3 == 0"} else {"is.na(class_3) == FALSE"}
    
    cond.class4 <- if(is.null(input$check2_class)){"is.na(class_4) == FALSE"
    } else if(!("Lower" %in% input$check2_class)){"class_4 == 0"} else {"is.na(class_4) == FALSE"}
    
    cond.age1 <- if(is.null(input$check2_age)){"below5 <= 43 | is.na(below5) == TRUE"
    } else if("<5" %in% input$check2_age){"below5 <= 30"
    } else {"below5 <= 43 | is.na(below5) == TRUE"}
    
    cond.age2 <- if(is.null(input$check2_age)){"X5_14 <= 43 | is.na(X5_14) == TRUE"
    } else if("5-14" %in% input$check2_age) {"X5_14 <= 30"
    } else {"X5_14 <= 43 | is.na(X5_14) == TRUE"}
    
    cond.age3 <-  if(is.null(input$check2_age)){"X15_24 <= 43 | is.na(X15_24) == TRUE"
    } else if("15-24" %in% input$check2_age) {"X15_24 <= 30"
    } else {"X15_24 <= 43 | is.na(X15_24) == TRUE"}
    
    cond.age4 <- if(is.null(input$check2_age)){"X25_34 <= 43 | is.na(X25_34) == TRUE"
    } else if("25-34" %in% input$check2_age) {"X25_34 <= 30"
    } else {"X25_34 <= 43 | is.na(X25_34) == TRUE"}
    
    cond.age5 <- if(is.null(input$check2_age)){"X35_44 <= 43 | is.na(X35_44) == TRUE"
    } else if("35-44" %in% input$check2_age) {"X35_44 <= 30"
    } else {"X35_44 <= 43 | is.na(X35_44) == TRUE"}
    
    cond.age6 <- if(is.null(input$check2_age)){"X45_54 <= 43 | is.na(X45_54) == TRUE"
    } else if("45-54" %in% input$check2_age) {"X45_54 <= 30"
    } else {"X45_54 <= 43 |is.na(X45_54) == TRUE"}
    
    cond.age7 <- if(is.null(input$check2_age)){"X55_64 <= 43 | is.na(X55_64) == TRUE"
    } else if("55-64" %in% input$check2_age) {"X55_64 <= 30"
    } else {"X55_64 <= 43 | is.na(X55_64) == TRUE"}
    
    cond.age8 <- if(is.null(input$check2_age)){"X65above <= 43 | is.na(X65above) == TRUE"
    } else if("65+" %in% input$check2_age) {"X65above <= 30"
    } else {"X65above <= 43 | is.na(X65above) == TRUE"}
    
    cond.crime <- if(input$check2_crime == "Safe(<55)"){"crime_level == 'Safe'"
    } else if(input$check2_crime == "Relatively Safe(56~129)"){"crime_level == 'Relatively Safe'"
    } else if(input$check2_crime == "Relatively Dangerous(130~214)"){"crime_level == 'Relatively Dangerous'"
    } else if(input$check2_crime=="Dangerous(>214)"){"crime_level=='Dangerous'"}
    else{"is.na(crime_level)==FALSE"}
    
    market.fil <- if(input$check2_market == "Many"){40} 
    else if(input$check2_market == "A few"){21}
    else {0}
    
    trans.fil <- if(input$check2_trans == "Many"){25}
    else if(input$check2_trans == "A few"){15}
    else {0}
    
    theatre.fil <- if(input$check2_ct == "Many"){10} 
    else if(input$check2_ct == "A few"){5} 
    else {0}
    

    
    areas <- (filter_data %>%
                filter(
                  eval(parse(text = cond.class1)), eval(parse(text = cond.class2)), eval(parse(text = cond.class3)),
                  eval(parse(text = cond.class4)),
                  eval(parse(text = cond.age1)), eval(parse(text = cond.age2)), eval(parse(text = cond.age3)),
                  eval(parse(text = cond.age4)), eval(parse(text = cond.age5)), eval(parse(text = cond.age6)),
                  eval(parse(text = cond.age7)), eval(parse(text = cond.age8)),
                  eval(parse(text = cond.crime)),
                  Transportation.stations >= trans.fil, markets >= market.fil, (Theatre >= theatre.fil | is.na(Theatre) == TRUE)
                ) %>%
                select(zipcode))[,1]
    return(areas)
  })
  
  col_display <- reactive({
    columns <- names(table_display)[22:30]
    
    columns <- c("Zipcode", columns)
    return(columns)
  })
  
  output$table2 <- renderDataTable(table_display[, names(table_display) %in% col_display()] %>% 
                                     filter(
                                       Zipcode %in% areas()), 
                                   options = list("sScrollX" = "100%", "bLengthChange" = FALSE))
  observe({
    if(length(areas())!=0){
      leafletProxy("map")%>%clearGroup(group="new_added")%>%
        addPolygons(data=subset(subdat, subdat$ZIPCODE%in% areas()),
                    weight = 2,
                    color = "#ffffff",
                    fillColor = "#036635",
                    fillOpacity=0.5,
                    group="new_added",
                    noClip = TRUE, label = ~ZIPCODE) 
    }
    else{
      leafletProxy("map")%>%clearGroup(group="new_added")
    }
  })
  
  #############Reset############
  observeEvent(input$button2,{
    updateSelectInput(session, "check2_class",selected = "")
    updateSelectInput(session, "check2_age",selected="")
    updateSelectInput(session, "check2_crime",selected = "")
    updateSelectInput(session, "check2_market",selected = "")
    updateSelectInput(session, "check2_trans",selected = "")
    updateSelectInput(session, "check2_ct",selected = "")
  })
})  