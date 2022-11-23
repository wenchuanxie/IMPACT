#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : server.R
# @License : Copyright(C), 
# @Author  : wenchuan.xie
# @Time    : 2022-03-19
# @IDE     : RStudio
# @Desc    : Server
#===============================================================================


# Shiny Server Side -------
server <- function(input, output, session) {
  
  # 页面快速跳转 Landing page ----
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
  
  # Total ICIs cohort (server) ------------------------------------------
  output$total_ici_cohorts <- renderText({
    nrow(dplyr::distinct(dplyr::filter(study.df,study_type == 'ICIs'),study_id,.keep_all = T))
  })
  # Total nonICIs cohort (server) ------------------------------------------
  output$total_tumors <- renderText({
    nrow(distinct(study.df,tumor_detail,.keep_all = T) )
    # valueBox_diy(value = nrow(distinct(study.df,tumor_detail,.keep_all = T) ),
    #              title = '',
    #              subtitle = "Number of Tumors",
    #              color = "blue")
  })
  # Total samples (server) ------------------------------------------
  output$total_samples <- renderText({
    sum(study.df$sample_size,na.rm = T)
    # valueBox_diy(sum(study.df$sample_size,na.rm = T),
    #              title = '',
    #              subtitle = "Number of Samples",
    #              color = "teal")
  })
  # click_js <- JS("function(event) {Shiny.onInputChange('treemapclick', event.point.name);}")
  output$impact_data_sample_size <- renderPlotly({
    
    impact.tree.df <- study.df %>%
      # dplyr::filter(study_source != 'TCGA')  %>%
      # dplyr::group_by(study_type,tumor_detail) %>%
      # dplyr::summarise(study_num = n()) %>%
      dplyr::filter(study_type == 'ICIs') %>%
      dplyr::select(study_id,sample_size) %>%
      dplyr::mutate(study_id = gsub("_","-",study_id)) %>%
      dplyr::group_by(study_id) %>%
      dplyr::summarise(size = sum(sample_size))  %>%
      dplyr::mutate(parents = 'Immunogenomics Studies')
    
    # treemap(impact.tree.df,
    #                    index=c('study_id'),
    #                    vSize="sample_size",
    #           type="index",
    #           palette = "Set2",
    #           bg.labels=c("white"),
    #           align.labels=list(
    #             c("center", "center")
    #             # ,c("center", "center")
    #             ),
    #           title = 'Immunogenomics Studies')
    
    plot_ly(
      impact.tree.df,
      labels = ~ study_id,
      textposition = 'center',
      parents = ~parents,
      values = ~ size,
      type = 'treemap',
      # hovertemplate = "Cohort: %{label}<br>Size: %{value}<extra></extra>",
      domain = list(column=0),
      name = "Immunogenomics Studies",
      textinfo="label+value+percent parent") #%>%
      # layout(title = "Immunogenomics Studies")
    
  })
  output$impact_data_sample_size_nonici <- renderPlotly({
    
    impact.nonici.df <- study.df %>%
      # dplyr::filter(study_source != 'TCGA')  %>%
      dplyr::filter(study_type == 'nonICIs') %>%
      dplyr::select(study_id,sample_size) %>%
      dplyr::mutate(study_id = gsub("_","-",study_id)) %>%
      dplyr::group_by(study_id) %>%
      dplyr::summarise(size = sum(sample_size))  %>%
      dplyr::mutate(parents = 'Non-immunogenomics Studies')
    # sum(impact.nonici.df$size)
    # treemap(impact.nonici.df,
    #         index=c('study_id'),
    #         vSize="sample_size",
    #         type="index",
    #         palette = "Set2",
    #         bg.labels=c("white"),
    #         align.labels=list(c("center", "center")),
    #         title = 'Non-immunogenomics Studies (exclude TCGA)')
    
    plot_ly(
      impact.nonici.df,
      labels = ~ study_id,
      textposition = 'center',
      parents = ~parents,
      values = ~ size,
      type = 'treemap',
      domain = list(column=0),
      name = "Non-immunogenomics Studies",
      textinfo="label+value+percent parent") #%>%
      # layout(title = "Non-immunogenomics Studies")
    
  })
  # 每个功能tab-server -------------------------------------
  # 0.0 免疫治疗预测分析
  tab_00_ici_explorer$server(input, output,session)
  # 0.1 预后预测
  tab_00_prognosis$server(input, output,session)
  # 1.1 KM分析
  tab_01_survival_km$server(input, output,session)
  # 1.2 Cox分析
  tab_01_survival_cox$server(input, output,session)
  # 1.3 亚组分析（interaction）
  tab_01_subgroup_analyses$server(input, output,session)
  # 1.4 Cutoff值筛选
  tab_01_cutpoint_analyses$server(input, output,session)
  
  tab_02_response$server(input, output,session)
  
  # 1.5  与治疗交互分析（interaction）
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
  
  # 8.自定义数据分析
  tab_08_user_defined$server(input, output,session)
  
  
}