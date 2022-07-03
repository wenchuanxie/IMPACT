#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : app.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-05-14
# @IDE     : RStudio
# @Desc    : 第3个Tab之3.1功能：肿瘤免疫浸润相关性分析：基于基因表达、细胞集ssGSEAscore
#===============================================================================

source('tab.R')

ui <- tabPanel(
  tab_03_tme_corr$ui,
)

server <- tab_03_tme_corr$server

shinyApp(ui = ui, server = server)