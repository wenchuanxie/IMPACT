#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : app.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-04-14
# @IDE     : RStudio
# @Desc    : 第一个Tab功能的第一个子功能：生存分析之KM曲线
#===============================================================================

source('tab.R')

ui <- tabPanel(
  tab_01_survival_km$ui,
)

server <- tab_01_survival_km$server

shinyApp(ui = ui, server = server)
