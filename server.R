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
  
  # Total ICIs cohort (server) ------------------------------------------
  output$total_ici_cohorts <- renderText({
    nrow(dplyr::distinct(dplyr::filter(study.df,study_type == 'ICIs'),study_id,.keep_all = T))
  })
  # Total nonICIs cohort (server) ------------------------------------------
  output$total_tumors <- renderText({
    nrow(distinct(study.df,tumor_detail,.keep_all = T) )
  })
  # Total samples (server) ------------------------------------------
  output$total_samples <- renderText({
    sum(study.df$sample_size,na.rm = T)
  })
  # click_js <- JS("function(event) {Shiny.onInputChange('treemapclick', event.point.name);}")
  output$impact_data_sample_size <- renderPlotly({
    
    impact.tree.df <- study.df %>%
      dplyr::filter(study_type == 'ICIs') %>%
      dplyr::select(study_id,sample_size) %>%
      dplyr::mutate(study_id = gsub("_","-",study_id)) %>%
      dplyr::group_by(study_id) %>%
      dplyr::summarise(size = sum(sample_size))  %>%
      dplyr::mutate(parents = 'Immunogenomics Studies')
    
    
    plot_ly(
      impact.tree.df,
      labels = ~ study_id,
      textposition = 'center',
      parents = ~parents,
      values = ~ size,
      type = 'treemap',
      domain = list(column=0),
      name = "Immunogenomics Studies",
      textinfo="label+value+percent parent") #%>%
    
  })
  output$impact_data_sample_size_nonici <- renderPlotly({
    
    impact.nonici.df <- study.df %>%
      dplyr::filter(study_type == 'nonICIs') %>%
      dplyr::select(study_id,sample_size) %>%
      dplyr::mutate(study_id = gsub("_","-",study_id)) %>%
      dplyr::group_by(study_id) %>%
      dplyr::summarise(size = sum(sample_size))  %>%
      dplyr::mutate(parents = 'Non-immunogenomics Studies')
    
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
  
  # 8.自定义数据分析
  tab_08_user_defined$server(input, output,session)
  
  
}