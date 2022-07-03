#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : ui.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-03-19
# @IDE     : RStudio
# @Desc    : UI
#===============================================================================

# Shiny UI -------
ui <- fluidPage(
  
  # cookie control
  # ref：https://github.com/DataScienceScotland/shiny_cookies
  # HTML('<script src="https://cc.cdn.civiccomputing.com/9/cookieControl-9.x.min.js" type="text/javascript"></script>'),
  tags$script(src = "cookieControl-9.x.min.js"),
  HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=G-S07KP4MCSN'></script>"),
  tags$script(src = "cookie_control_config.js",style = 'fill:#00a064'),
  
  use_prompt(),
  theme = "bootstrap.css",
  includeCSS("www/styles1.css"),
  
  
  # 输入框颜色控制
  # tags$head(
  #   tags$style(HTML(css))
  # ),

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
    # Pan-Cancer Predictors and Prognostic factors Detection project: PRPPDetector
    # Exploration of Potential predictIve and prognosTic biOMarkers in pan-cancEr: EPITOME
    # IPREX: Immunotherapy and Prognostic biomarkeR EXplorer.
    # IMPEL: a web resource for IMmunotherapy and Predictive biomarker ExpLorer.
    # IMPACT: a web resource for exploration in IMmunotherapy Predictive biomArkers and Cancer prognosTic biomarkers
    
    
    # 导航栏：几点回到主页
    id = "intabset",
    
    # 项目主题色选择
    ###########################################################################
    ## 首页，demo图片跳转 Landing page ----
    tabPanel(
      title = " Home", icon = icon("home"),
      mainPanel(
        width = 11, style = "margin-left:4%; margin-right:4%",
        rintrojs::introBox(
          fluidRow(
            column(7, p(h3("Welcome to ",strong("IMPACT"), style = "margin-top:0px;"))),
            column(12, 
                   # p(h5(strong("IMPACT"),"is a web-based platform for the exploration in ",'IMmunotherapeutic', "Predictive And Cancer prognosTic biomarkers using genomic,
                   #          transcriptomic and proteomic data from in-house, literature and public databases including The Cancer Genome Atlas (TCGA),
                   #          Clinical Proteomic Tumor Analysis Consortium (CPTAC), (International Cancer Genome Consortium (ICGC) and Gene Expression Omnibus (GEO).",
                   #         style = "margin-top:0px;")),
                   includeMarkdown("tab_00_home/home.Rmd"))
            ),
          data.step = 1,
          data.intro = (p(h4("Welcome to IMPACT resource"),style = "color:0E3E5D; font-size:20px")),
          data.position = "left"
        ),
        br(),
        rintrojs::introBox(
          fluidRow(
            tags$img(src = "Workflow_00A064.png",style = "margin-left:5px;margin-right:5px;margin-bottom:5px;width: 100%;")
          ),
          data.position = "bottom-middle-aligned"
        ),
        # banner plot
        # carousel(
        #   width = 12,
        #   id = "omics",
        #   carouselItem(
        #     caption = "PredExplore",
        #     tags$img(src = "Workflow_white_background.png",style = "width: 20%;") # https://figshare.com/ndownloader/files/35523782/preview/35523782/preview.jpg
        #   ),
        #   carouselItem(
        #     caption = "Kaplan–Meier",
        #     tags$img(src = "https://figshare.com/ndownloader/files/35523758/preview/35523758/preview.jpg")
        #   ),
        #   carouselItem(
        #     caption = "Cox Regression",
        #     tags$img(src = "https://figshare.com/ndownloader/files/35523779/preview/35523779/preview.jpg")
        #   ),
        #   carouselItem(
        #     caption = "TME Comparative",
        #     tags$img(src = "https://figshare.com/ndownloader/files/35523776/preview/35523776/preview.jpg")
        #   ),
        #   carouselItem(
        #     caption = "Tumor Mutation Profile",
        #     tags$img(src = "https://figshare.com/ndownloader/files/35523761/preview/35523761/preview.jpg")
        #   )
        # ),
        br(),
        # 1st row
        # 免疫预测因子及预后预测因子通量分析跳转
        # fluidRow(
        #   # Summary box
        #   column(4,
        #          class = "landing-page-column", br(), # spacing
        #          introBox(
        #            lp_main_box(
        #              image_name = "home_predexplore",
        #              button_name = "jump_to_icip", title_box = "Immunotherapy Predictive Biomarkers Exploration",
        #              description = "A tool for identifing potential predictive biomarkers of cancer immunotherapy in pan-cancer"
        #            ),
        #            data.step = 2,
        #            data.intro = h5("The tool allows you to look at a potential predictive biomarker of cancer immunotherapy in multi-cohorts at the same time"),
        #            data.position = "bottom-right-aligned"
        #          )
        #   ),
        #   # Table box
        #   column(4,
        #          class = "landing-page-column",br(), # spacing
        #          introBox( # tour of the tool
        #            lp_main_box(
        #              image_name = "home_progexplore",
        #              button_name = "jump_to_prog", title_box = "Cancer Prognostic Biomarkers Exploration",
        #              description = "A tool for identifing potential prognostic biomarkers of cancer in pan-cancer"
        #            ),
        #            data.step = 3,
        #            data.intro = h5("The tool allows you to look at a potential prognostic biomarker of cancer in multi-cohorts at the same time")
        #          )
        #   ),
        #   # Table box
        #   column(4,
        #          class = "landing-page-column",br(), # spacing
        #          introBox( # tour of the tool
        #            lp_main_box(
        #              image_name = "home_survival_km",
        #              button_name = "jump_to_km", title_box = "Kaplan-Meier Curve",
        #              description = "A high level view of an potential predictive or prognostic biomarker"
        #            ),
        #            data.step = 4,
        #            data.intro = h5("The 'Survival Analysis' window can be used to plot Kaplan-Meier curve")
        #          )
        #   )
        # ),
        # # 2nd ~  row
        # fluidRow(
        #   introBox(
        #     data.step = 5, # tour around the tool
        #     data.intro = h5("Stratified Survival Analysis"),
        #     # 1st column
        #     column(4,
        #            class = "landing-page-column",
        #            lp_about_box(
        #              image_name = "home_survival_cox", button_name = "jump_to_cox",
        #              title_box = "Cox Regression", 
        #              description = "Cox Regression"
        #            ),
        #            lp_about_box(
        #              image_name = "banner02", button_name = "jump_to_im",
        #              title_box = "Immunogenicity Analyses",
        #              description = "Immunogenicity Analyses"
        #            ),
        #            lp_about_box(
        #              image_name = "banner02", button_name = "jump_to_mut",
        #              title_box = "Tumor Mutation Profile",
        #              description = "Tumor mutation profile analyses"
        #            )
        #     ),
        #     # 2nd column
        #     column(4,
        #            class = "landing-page-column",
        #            lp_about_box(
        #              image_name = "banner03", button_name = "jump_to_subgroup",
        #              title_box = "Subgroup Analyses",
        #              description = "Subgroup Analyses"
        #            ),
        #            lp_about_box(
        #              image_name = "banner02", button_name = "jump_to_corr",
        #              title_box = "TME Correaltion Analyses",
        #              description = "Correlation analysis of tumor microenvironment"
        #            ),
        #            lp_about_box(
        #              image_name = "banner02", button_name = "jump_to_data",
        #              title_box = "Data resource",
        #              description = "View and download the data behind this tool"
        #            )
        #     ),
        #     # 3rd column
        #     column(4,
        #            class = "landing-page-column",
        #            lp_about_box(
        #              image_name = "mutation_profiles", button_name = "jump_to_interaction",
        #              title_box = "Interaction Analyses",
        #              description = "Interaction analyses"
        #            ),
        #            lp_about_box(
        #              image_name = "data_resource", button_name = "jump_to_comp",
        #              title_box = "TME Comparative Analyses",
        #              description = "Comparative analysis of tumor microenvironment"
        #            ),
        #            lp_about_box(
        #              image_name = "tutorial", button_name = "jump_to_tutorial",
        #              title_box = "User guide and tutorial",
        #              description = "User guide and tutorial"
        #            )
        #     )
        #   )
        # )
        
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
             tab_00_prognosis$ui),
    ###########################################################################
    ## 生存分析和独立性验证 ----
    navbarMenu(
      "Survival Analyses",icon = icon("chart-area"),
      # 下拉框第一个功能：生存分析
      tabPanel("Kaplan-Meier Curve", value = "km",icon = icon("chart-line"),
               tab_01_survival_km$ui),
      # 下拉框第二个功能：独立性验证分析
      tabPanel("Cox Regression", value = "cox",icon = icon("align-center"),
               tab_01_survival_cox$ui), 
      # 下拉框第三个功能：亚组分析
      tabPanel("Subgroup Analyses", value = "subgroup",icon = icon("list-alt"),
               tab_01_subgroup_analyses$ui)
      
    ),
    # 下拉框第四个功能：交互分析
    tabPanel("Interaction Analyses", value = "interaction",icon = icon("chart-line"),
             tab_01_mut_treatment$ui),
    ###########################################################################
    ## 肿瘤免疫原性分析 ---- 
    ## 针对有TMB，Neoantigen，Purity的队列
    tabPanel("Immunogenicity Analyses", icon = icon("sliders-h"),value = "im",
             tab_02_tumor_immunogenicity$ui),
    ###########################################################################
    ## 肿瘤免疫微环境 ----
    navbarMenu(
      "ImmuneMicroenvironment",icon = icon("braille"),
      # 下拉框第一个功能：免疫相关基因/基因集相关性分析
      tabPanel("TME Correaltion Analyses", value = "corr",icon = icon("list-ul"),
               tab_03_tme_corr$ui),
      
      # 下拉框第二个功能：免疫相关基因/基因集分组比较分析
      tabPanel("TME Comparative Analyses", value = "comp",icon = icon("list-alt"),
               tab_03_tme_comp$ui)
      ),
    ###########################################################################
    ## 肿瘤突变 ----
    tabPanel("MutationalProfiles",value = "mut", icon = icon("list-alt"), 
             tab_04_tumor_mutation$ui),
    ###########################################################################
    ## 数据资源 ----
    tabPanel("Data", value = "data",icon = icon("database"),
             tab_05_data_resource$ui),
    ###########################################################################
    ## Tutorial ----
    tabPanel("Tutorial",value = "tutorial",icon = icon("readme"),
             # tab_07_tutorial$ui
             # includeMarkdown("www/tutorial.md")
             includeMarkdown("tab_07_tutorial/tutorial.Rmd")
    ),
    ###########################################################################
    ## About ----
    tabPanel("About",value = "about",icon = icon("user"),
             # tab_06_about$ui
             includeMarkdown("tab_06_about/about.Rmd")
    )
  ),
  div(style = "margin-bottom: 30px;"), # this adds breathing space between content and footer
  
  # 脚注 ----
  # addResourcePath(prefix = "temp", directoryPath = temp_file_path)
  
  # tags$footer(column(4, "IMPACT v2.0 2022"), 
  #             column(5, tags$a(href="http://www.cams.ac.cn/", 
  #                              tags$b("©Chinese Academy of Medical Sciences & Peking Union Medical College"), 
  #                              class="Chinese Academy of Medical Sciences", 
  #                              style = "color: white; text-decoration: none")), 
  #             # addResourcePath(prefix = "temp", directoryPath = temp_file_path),
  #             column(2, tags$a(href="temp/Cookie Policy.html", 
  #                              tags$b("Cookie Policy!"), 
  #                              class="externallink", 
  #                              target = '_blank',
  #                              style = "color: white; text-decoration: none")), 
  #             style = "
  #             position:fixed;
  #             text-align:center;
  #             left: 0;
  #             bottom:0;
  #             width:100%;
  #             z-index:1000;  
  #             font-size: 12px;
  #             height:30px; /* Height of the footer */
  #             color: white;
  #             padding: 10px;
  #             font-weight: bold;
  #             background-color: #00A064"
  # )
  hr(style = "border-color: #cbcbcb;"),
  fluidRow(
    column(12,align = "center",
           p(HTML("&bull;"),"IMPACT created by ", tags$a(href = "http://www.cams.ac.cn", 'Chinese Academy of Medical Sciences & Peking Union Medical College', target = '_blank'), " at Beijing", HTML("&bull;"),
             style = "font-size: 100%"),
           p(#tags$a(href = "temp/Cookie Policy.html", 'Cookie policy.', target = '_blank'),
             "Have any question, send an email ", tags$a(href = "mailto:jie_969@163.com", tags$i(class = 'fa fa-envelope', style = 'color:#990000'), target = '_blank'), style = "font-size: 100%"),
           p(tags$em("Version: 2.0. Last updated: June 2022"), style = 'font-size:100%'))
  )
  
)


