#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : server.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-03-19
# @IDE     : RStudio
# @Desc    : Server
#===============================================================================

# Shiny Server Side -------
server <- function(input, output, session) {

  ###############################################.
  ## 页面快速跳转 Landing page ----\
  observeEvent(input$jump_to_icip, {
    updateTabsetPanel(session, "intabset", selected = "icip")
  })
  observeEvent(input$jump_to_prog, {
    updateTabsetPanel(session, "intabset", selected = "prog")
  })
  observeEvent(input$jump_to_km, {
    updateTabsetPanel(session, "intabset", selected = "km")
  })
  observeEvent(input$jump_to_cox, {
    updateTabsetPanel(session, "intabset", selected = "cox")
  })
  observeEvent(input$jump_to_subgroup, {
    updateTabsetPanel(session, "intabset", selected = "subgroup")
  })
  observeEvent(input$jump_to_im, {
    updateTabsetPanel(session, "intabset", selected = "im")
  })
  observeEvent(input$jump_to_corr, {
    updateTabsetPanel(session, "intabset", selected = "corr")
  })
  observeEvent(input$jump_to_comp, {
    updateTabsetPanel(session, "intabset", selected = "comp")
  })
  observeEvent(input$jump_to_interaction, {
    updateTabsetPanel(session, "intabset", selected = "interaction")
  })
  observeEvent(input$jump_to_mut, {
    updateTabsetPanel(session, "intabset", selected = "mut")
  })
  observeEvent(input$jump_to_data, {
    updateTabsetPanel(session, "intabset", selected = "data")
  })
  observeEvent(input$jump_to_tutorial, {
    updateTabsetPanel(session, "intabset", selected = "tutorial")
  })
  # Temporary until rintroJS fixed
  # observeEvent(input$btn_landing, {
  #   updateTabsetPanel(session, "intabset", selected = "tour")
  # })
  
  ##############################################
  # 0.0 免疫治疗预测分析
  tab_00_ici_explorer$server(input, output,session)
  # 0.1 预后预测
  tab_00_prognosis$server(input, output,session)
  # 1.1 KM分析
  tab_01_survival_km$server(input, output,session)
  # 1.2 Cox分析
  tab_01_survival_cox$server(input, output,session)
  # 1.3亚组分析（interaction）
  tab_01_subgroup_analyses$server(input, output,session)
  # 1.4与治疗交互分析（interaction）
  tab_01_mut_treatment$server(input, output,session)
  # 2 肿瘤免疫原性分析：针对有TMB，Neoantigen，Purity的队列
  tab_02_tumor_immunogenicity$server(input, output,session)
  # 3.1 TME-Corr分析
  tab_03_tme_corr$server(input, output,session)
  # 3.2 TME-Comp分析
  tab_03_tme_comp$server(input, output,session)
  # 4. 肿瘤突变分析
  tab_04_tumor_mutation$server(input, output,session)
  # 5. 数据表
  tab_05_data_resource$server(input, output,session)
  # 6. 关于我
  # tab_06_about$server(input, output,session)
  # 7. FAQ
  # tab_06_about$server(input, output,session)
  
}