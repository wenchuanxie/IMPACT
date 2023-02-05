#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : ui.R
# @License : Copyright(C), 
# @Author  : wenchuan.xie
# @Time    : 2022-03-19
# @IDE     : RStudio
# @Desc    : UI
#===============================================================================

# Shiny UI -------
ui <- fluidPage(
  
  
  # cookie control
  tags$script(src = "cookieControl-9.x.min.js"),
  HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=G-S07KP4MCSN'></script>"),
  tags$script(src = "cookie_control_config.js",style = 'fill:#00a064'),
  
  use_prompt(),
  theme = "bootstrap.css",
  includeCSS("www/styles1.css"),
  
  
  # session 过期样式
  disconnectMessage(
    text = "Your session has timed out!",
    refresh = "Reload now",
    background = "#646464e6",
    size = 36,
    width = "full",
    top = "center",
    colour = "white",
    overlayColour = "#999",
    overlayOpacity = 0.4,
    refreshColour = "#6bba8b"
  ),
  position = c("fixed-top"),
  navbarPage(
    
    # 导航栏：左侧项目标题
    title = "IMPACT",
    
    # 导航栏：几点回到主页
    id = "intabset",
    
    # 项目主题色选择
    ###########################################################################
    ## 首页，demo图片跳转 Landing page ----
    tabPanel(
      title = " Home", icon = icon("house"),
      mainPanel(
        width = 11, style = "margin-left:4%; margin-right:4%",
        rintrojs::introBox(
          fluidRow(
            column(7, p(h3("Welcome to ",strong("IMPACT"), style = "margin-top:0px;"))),
            column(12, 
                   includeMarkdown("tab_00_home/home.Rmd"))
            ),
          data.step = 1,
          data.intro = (p(h4("Welcome to IMPACT resource"),style = "color:0E3E5D; font-size:20px")),
          data.position = "left"
        ),
        rintrojs::introBox(
          fluidRow(
            valueBox_diy2(value = "total_ici_cohorts",
                          subtitle = "ICIs Cohorts",
                          icon = "database",
                          color = "#00A064"),
            valueBox_diy2(value = "total_tumors",
                          subtitle = "Cancers",
                          icon = "file",
                          color = "#00A064"),
            valueBox_diy2(value = "total_samples",
                          subtitle = "Samples",
                          icon = "users",
                          color = "#00A064")
            
          )
        ),
        rintrojs::introBox(
          fluidRow(
            tags$img(src = "Workflow_00A064_upd.png",style = "margin-left:5px;margin-right:5px;margin-bottom:5px;width: 100%;")
          ),
          data.position = "bottom-middle-aligned"
        ),
        rintrojs::introBox(
          fluidRow(
            column(6,div(plotlyOutput("impact_data_sample_size",height="100%",width = '100%')),align = 'left'),
            column(6,div(plotlyOutput("impact_data_sample_size_nonici",height="100%",width = '100%')),align = 'right')
          ),
          data.position = "bottom-middle-aligned"
        ),
        br(),
      )
    ),
    
    # 导航栏：右侧功能标题
    ###########################################################################
    ## 免疫治疗突变数据泛癌分析，探索基因突变对免疫治疗影响 ----
    tabPanel("PredExplore",icon = icon("list-ul"), value = "icip",
             tab_00_ici_explorer$ui
             ),
    ###########################################################################
    ## 非免疫治疗突变数据泛癌分析，探索基因突变对预后影响 ----
    tabPanel("ProgExplore", icon = icon("list-ul"), value = "prog", 
             tab_00_prognosis$ui
             ),
    ###########################################################################
    ## 生存分析和独立性验证 ----
    navbarMenu(
      "Survival Analysis",icon = icon("chart-area"),
      # 下拉框第一个功能：生存分析
      tabPanel("Kaplan-Meier Curve", value = "km",icon = icon("chart-line"),
               tab_01_survival_km$ui),
      # 下拉框第二个功能：独立性验证分析
      tabPanel("Cox Regression", value = "cox",icon = icon("align-center"),
               tab_01_survival_cox$ui
               ), 
      # 下拉框第三个功能：亚组分析
      tabPanel("Subgroup Analysis", value = "subgroup",icon = icon("rectangle-list"),
               tab_01_subgroup_analyses$ui
               ), 
      # 下拉框第四个功能：cutoff值分析，针对连续变量
      tabPanel("Cutpoint Analysis", value = "cutpoint",icon = icon("filter"),
               tab_01_cutpoint_analyses$ui
               ),
      tabPanel("Immunotherapy Response", value = "response",icon = icon("align-center"),
               tab_02_response$ui,
      )
      
    ),
    # 下拉框第四个功能：交互分析
    tabPanel("Interaction Analysis", value = "interaction",icon = icon("chart-line"),
             tab_01_mut_treatment$ui
             ),
    ###########################################################################
    ## 肿瘤免疫原性分析 ---- 
    ## 针对有TMB，Neoantigen，Purity的队列
    tabPanel("Immunogenicity Analysis", icon = icon("sliders"),value = "im",
             tab_02_tumor_immunogenicity$ui
             ),
    ###########################################################################
    ## 肿瘤免疫微环境 ----
    navbarMenu(
      "TME Analysis",icon = icon("braille"),
      # 下拉框第一个功能：免疫相关基因/基因集相关性分析
      tabPanel("TME Correaltion Analysis", value = "corr",icon = icon("list-ul"),
               tab_03_tme_corr$ui
               ),
      
      # 下拉框第二个功能：免疫相关基因/基因集分组比较分析
      tabPanel("TME Comparative Analysis", value = "comp",icon = icon("rectangle-list"),
               tab_03_tme_comp$ui
               )
      ),
    ###########################################################################
    ## 肿瘤突变 ----
    tabPanel("Mutational Profiles",value = "mut", icon = icon("rectangle-list"), 
             tab_04_tumor_mutation$ui
             ),
    ###########################################################################
    ## 用户自定义数据 ----
    tabPanel("User-defined", value = "diy",icon = icon("intercom"),
             tab_08_user_defined$ui
    ),
    ###########################################################################
    ## 数据资源 ----
    tabPanel("Data", value = "data",icon = icon("database"),
             tab_05_data_resource$ui
             ),
    # 整合 Tutorial 和 About
    navbarMenu(
      
      "Info",icon = icon("circle-info"),
      
      tabPanel("Tutorial",value = "tutorial",icon = icon("readme"),
               # tab_07_tutorial$ui
               includeMarkdown("tab_07_tutorial/tutorial.Rmd")
               # includeHTML("tab_07_tutorial/tutorial_icipred.html")
               
      ),
      ## About ----
      tabPanel("About",value = "about",icon = icon("user"),
               # tab_06_about$ui
               includeMarkdown("tab_06_about/about.Rmd")
      )
    )
    
  ),
  div(style = "margin-bottom: 30px;"), # this adds breathing space between content and footer
  
  # 脚注 ----
  hr(style = "border-color: #cbcbcb;"),
  fluidRow(
    column(12,align = "center",
           p(HTML("&bull;"),"IMPACT created by ", tags$a(href = "http://www.cams.ac.cn", 'Chinese Academy of Medical Sciences & Peking Union Medical College', target = '_blank'), " at Beijing", HTML("&bull;"),
             style = "font-size: 100%"),
           p(#tags$a(href = "temp/Cookie Policy.html", 'Cookie policy.', target = '_blank'),
             "Have any question, send an email ", tags$a(href = "mailto:jie_969@163.com", tags$i(class = 'fa fa-envelope', style = 'color:#990000'), target = '_blank'), style = "font-size: 100%"),
           p(tags$em("Version: 2.0. Last updated: November 2022"), style = 'font-size:100%'))
  )
  
)


