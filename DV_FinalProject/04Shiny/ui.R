#ui.R

require(shiny)
require(shinydashboard)
require(leaflet)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Crosstab", tabName = "crosstab", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "crosstab",
              actionButton(inputId = "light", label = "Light"),
              actionButton(inputId = "dark", label = "Dark"),
              sliderInput("KPI1", "KPI_High_Max_value:", 
                          min = 0.6, max = 1,  value = 1),
              sliderInput("KPI2", "KPI_Medium_Max_value:", 
                          min = 0.4, max = 0.6,  value = 0.6),
              textInput(inputId = "title", 
                        label = "Crosstab Title",
                        value = "Team Record Crosstab\nWins / Games"),
              actionButton(inputId = "clicks1",  label = "Click me"),
              plotOutput("distPlot1")
      )
    )
  )
)
