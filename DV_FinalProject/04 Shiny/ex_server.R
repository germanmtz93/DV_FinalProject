# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)
require(DT)
require(extrafont)
library(reshape2)

KPI_High_Max_value = 0.666
KPI_Medium_Max_value = 0.4

# Pull data for Barchart before placing into the shiny server
dfa <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                "select YEARINC, sum_fatalities, sum_total, kpi as ratio, DATEINC,
                                                case
                                                when kpi > "p1" then \\\'01 High Fatality\\\'
                                                when kpi > "p2" then \\\'02 Medium Fatality\\\'
                                                else \\\'03 Low Fatality\\\'
                                                end kpi
                                                from (select YEARINC,
                                                sum(FATALITIES) as sum_fatalities, sum(TOTAL_VICTIMS) as sum_total,
                                                sum(FATALITIES) / sum(TOTAL_VICTIMS) as kpi, DATEINC
                                                from shootingsmass
                                                group by YEARINC, DATEINC)
                                                order by YEARINC;"
                                                ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', 
                                                                  USER='C##cs329e_btb687', PASS='orcl_btb687', MODE='native_mode', 
                                                                  MODEL='model', returnDimensions = 'False', returnFor = 'JSON', 
                                                                  p1=KPI_High_Max_value, p2=KPI_Medium_Max_value), verbose = TRUE))); 

dfa$DATEINC <- as.character(as.Date(dfa$DATEINC,format='%m/%d/%Y'))


# Pull data for crosstab before placing into shiny server
dfb <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                "select YEARINC, RACE, sum_fatalities, sum_total, kpi as ratio,
                                                 case
                                                 when kpi > "p1" then \\\'01 High Fatality\\\'
                                                 when kpi > "p2" then \\\'02 Medium Fatality\\\'
                                                 else \\\'03 Low Fatality\\\'
                                                 end kpi
                                                 from (select YEARINC, RACE,
                                                 sum(FATALITIES) as sum_fatalities, sum(TOTAL_VICTIMS) as sum_total,
                                                 sum(FATALITIES) / sum(TOTAL_VICTIMS) as kpi
                                                 from shootingsmass
                                                 group by YEARINC, RACE)
                                                 order by YEARINC desc;"
                                                 ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', 
                                                                   USER='C##cs329e_btb687', PASS='orcl_btb687', MODE='native_mode', 
                                                                   MODEL='model', returnDimensions = 'False', returnFor = 'JSON', 
                                                                   p1=KPI_High_Max_value, p2=KPI_Medium_Max_value), verbose = TRUE)));

# Pull data for scatter plot before placing into the shiny server
dfc <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from shootingsmass"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_btb687', PASS='orcl_btb687', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ));

dfc.m = melt(dfc, id.vars ="TOTAL_VICTIMS", measure.vars = c("FATALITIES","INJURED"))
dfc.m$VENUE = dfc$VENUE

# Pull data for blending before placing into the shiny server
dfda <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select YEARINC, sum_fatalities from (select YEARINC, sum(FATALITIES) as sum_fatalities from shootingsmass group by YEARINC) order by YEARINC;"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_btb687', PASS='orcl_btb687', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

dfdb <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select YEARINC, HOMICIDES from HOMICIDES"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_btb687', PASS='orcl_btb687', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

dfd <- dplyr::inner_join(dfda, dfdb, by="YEARINC") 


# Begin shiny server code
shinyServer(function(input, output) {
  
  KPI_Low_Max_value <- reactive({input$KPI1})     
  KPI_Medium_Max_value <- reactive({input$KPI2})
  rv <- reactiveValues(alpha = 0.50)
  observeEvent(input$light, { rv$alpha <- 0.50 })
  observeEvent(input$dark, { rv$alpha <- 0.75 })
  
  # Begin code for Barchart
  # Place dfa into reactive function
  df1 <- eventReactive(input$clicks1, {dfa
  })
  
  output$distPlot1 <- renderPlot({             
    plot <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_continuous() +
      labs(title='BarChart') +
      labs(x=paste("YEAR"), y=paste("FATALITY RATIO")) +
      geom_bar(position="dodge") +
      
      layer(data=df1(), 
            mapping=aes(x=DATEINC, y= RATIO, ymax=max(1)*1.05 ), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(colour="black"), 
            position=position_identity()
            
      ) + coord_flip() + 
      #labels
      layer(data=df1(), 
            mapping=aes(x=DATEINC, y=RATIO, label=round(RATIO,4), ymax=max(1)*1.05 ), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=-0.5), 
            position=position_identity()
      ) +
      #Color
      layer(data=df1(), 
            mapping=aes(x=DATEINC, y=RATIO, fill=KPI), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(alpha=0.50), 
            position=position_identity()
      ) +  
      #H-line
      layer(data=df1(), 
            mapping=aes(yintercept = mean(RATIO)), 
            geom="hline",
            geom_params=list(colour="red")
      )
    plot
  })
  
  observeEvent(input$clicks, {
    print(as.numeric(input$clicks))
  })
  
  # Begin code for Second Tab:
  
  df2 <- eventReactive(input$clicks2, {dfb
  })
  
  output$distPlot2 <- renderPlot(height=1000, width=2000, {
    plot1 <- ggplot() + 
      coord_cartesian()  +
      scale_x_discrete() +
      labs(title='Shootings Crosstab\nSUM_TOTAL') +
      labs(x=paste("RACE(group)"), y=paste("YEAR")) +
      layer(data=df2(), 
            mapping=aes(x=RACE, y=YEARINC, label=SUM_TOTAL), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black"), 
            position=position_identity()
      ) +
      layer(data=df2(), 
            mapping=aes(x=RACE, y=YEARINC, fill=KPI), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(alpha=0.50), 
            position=position_identity()
      )
    plot1
  })
  
  # Begin code for Third Tab:
  # Place dfc into reactive function
  df3 <- eventReactive(input$clicks3, {dfc.m
  })
  
  output$distPlot3 <- renderPlot(height=1000, width=2000, {
    plot2 <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_continuous() +
      #facet_grid(~FATALITIES) +
      labs(title='Mass Shootings') +
      labs(x="Venue", y=paste("Count Of Fatalities/Injured")) +
      layer(data=df3(), 
            mapping=aes(x=VENUE, y=value, color=variable), 
            stat="identity", 
            stat_params=list(), 
            geom="point",
            geom_params=list(), 
            #position=position_identity()
            position=position_jitter(width=0.3, height=0)
      ) 
    plot2
  })
  
  # Begin code for Fourth Tab:
  # Place dfd into reactive function
  df4 <- eventReactive(input$clicks4, {dfd
  })
  
  output$distPlot4 <- renderPlot(height=1000, width=2000, {
    plot3 <- ggplot() + 
      coord_cartesian() + 
      #facet_grid(~FATALITIES) +
      labs(title='Mass Shootings of Homicides per Year') +
      labs(x="Year", y=paste("Homicides (per year)")) +
      layer(data=df4(), 
            mapping=aes(x=YEARINC, y=as.numeric(HOMICIDES)), 
            stat="identity", 
            stat_params=list(), 
            geom="line",
            geom_params=list(), 
            position=position_identity()
      ) 
    plot3
  })
  
  # Begin code for Fifth Tab:
  # Use df4 from previous tab
  
  output$distPlot5 <- renderPlot(height=1000, width=2000, {
    plot4 <- ggplot() + 
      coord_cartesian() + 
      #facet_grid(~FATALITIES) +
      labs(title='Mass Shootings of Fatalities per Year') +
      labs(x="Year", y=paste("Fatalities (per year)")) +
      layer(data=df4(), 
            mapping=aes(x=YEARINC, y=SUM_FATALITIES), 
            stat="identity", 
            stat_params=list(), 
            geom="line",
            geom_params=list(), 
            position=position_identity()
      ) 
    plot4
  })
})
  