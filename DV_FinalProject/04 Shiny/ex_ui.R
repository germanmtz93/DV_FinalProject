#ui.R 

library(shiny)

navbarPage(
  title = "Mass Shootings",
  tabPanel(title = "CrossTab",
           sidebarPanel(
             actionButton(inputId = "light", label = "Light"),
             actionButton(inputId = "dark", label = "Dark"),
             sliderInput("KPI1", "KPI_Low_Max_value:", 
                         min = 0, max = 0.4,  value = 0.4),
             sliderInput("KPI2", "KPI_Medium_Max_value:", 
                         min = 0.4, max = 0.6666,  value = 0.6666),
             textInput(inputId = "title", 
                       label = "Crosstab Title",
                       value = "Shootings Crosstab\nSUM_TOTAL"),
             actionButton(inputId = "clicks2",  label = "Click me")
           ),
           
           mainPanel(plotOutput("distPlot2")
           )
  ),
  tabPanel(title = "Barchart",
           sidebarPanel(
             actionButton(inputId = "clicks1",  label = "Click me")
           ),
           mainPanel(plotOutput("distPlot1")
           )
  ),
  tabPanel(title = "ScatterPlot",
           sidebarPanel(
             actionButton(inputId = "clicks3",  label = "Click me")
           ),
           
           mainPanel(plotOutput("distPlot3")
           )
  ),
  tabPanel(title = "Blending 1",
           sidebarPanel(
             actionButton(inputId = "clicks4",  label = "Click me")
           ),
           
           mainPanel(plotOutput("distPlot4")
           )
  ),
  tabPanel(title = "Blending 2",
           sidebarPanel(
             actionButton(inputId = "clicks5",  label = "Click me")
           ),
           
           mainPanel(plotOutput("distPlot5")
           )
  )
)
