#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : app.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-04-14
# @IDE     : RStudio
# @Desc    : 第二个Tab功能：tumor immunogenicity: tmb,neo,genomic scar (hrd),scna et al
#===============================================================================

source('tab.R')

ui <- tabPanel(
  tab_02_tumor_immunogenicity$ui,
)

server <- tab_02_tumor_immunogenicity$server

shinyApp(ui = ui, server = server)