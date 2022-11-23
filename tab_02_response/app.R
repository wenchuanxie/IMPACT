#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : app.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-09-05
# @IDE     : RStudio
# @Desc    : 对于免疫治疗队列，分析治疗相应与临床、基因突变、转录表达的相关性
#===============================================================================

source('./tab_02_response/tab.R')

ui <- tabPanel(
  tab_02_response$ui,
)

server <- tab_02_response$server

shinyApp(ui = ui, server = server)
