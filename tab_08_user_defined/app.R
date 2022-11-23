rm(list = ls())
gc()

source('./tab_08_user_defined/tab.R')

ui <- tabPanel(
  tab_08_user_defined$ui,
)

server <- tab_08_user_defined$server

shinyApp(ui = ui, server = server)
