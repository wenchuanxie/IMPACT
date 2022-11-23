#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : tab.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-07-28
# @IDE     : RStudio
# @Desc    : 连续变量不同临界值对预后的影响
# 队列类型--肿瘤--队列（有表达）--选择基因名称 -- 分析
#===============================================================================

source("./r/func.survivalplot.R")
source("./r/func.cutpointcalc.R")
source("./r/func.cutpointplot.R")
# 输出list
tab_01_cutpoint_analyses <- list()
cat_prefix_cf <- "cutpoint"
# UI - 左侧栏
sidebar <- sidebarPanel(
  id = 'cutpoint_sidebar',
  width = 3,
  h3("Determine the Cutpoint for Continuous Variables"),
  
  # 队列类型
  awesomeRadio(inputId = "cf_study_type",
               label = "Select study types", 
               choices = c("Immunogenomics Studies", 
                           "Non-immunogenomics Studies"),
               selected = "Immunogenomics Studies",
               status = "success"),
  
  # mRNA表达（转录组/蛋白组） 或 临床属性 
  awesomeRadio(inputId = "cf_data_type",
               label = "Select data types",
               choices = c("Clinicopathologicals" = 'Clidx',"mRNA" = 'Expr'),
               selected = "Clidx",
               status = "success"),
  # 瘤种名称
  conditionalPanel('input.cf_data_type != ""',
                   pickerInput(inputId = "cf_cancer_detail",
                               label = "Select one cancer type, E.g. 'LUAD'",
                               choices = NULL,
                               multiple = FALSE,
                               selected = NULL),
                   # 队列
                   conditionalPanel('input.cf_cancer_detail != ""',
                                    pickerInput(inputId = "cf_study_id",
                                                label = "Select one dataset, E.g. 'Hellmann_2018'",
                                                choices = NULL,
                                                multiple = FALSE,
                                                selected = NULL))),

  # 基因或属性
  # conditionalPanel('input.cf_study_id != ""',
                   # 队列数据类型：Mutation,RNA,Protein,Methyl
                   # pickerInput("cf_data_type",
                   #             label = "Select data type, E.g. 'Expr'", 
                   #             choices = NULL,
                   #             selected = NULL, 
                   #             multiple = FALSE),
                   
                  # ),
  # 转录组或者蛋白组时，可选择基因
  conditionalPanel('input.cf_data_type == "Expr"|| input.cf_data_type == "Proteome"',
                   selectizeInput(inputId = "cf_symbol_id", 
                                  label = "Select/Input gene symbols, E.g. 'EGFR STK11'", 
                                  choices = NULL, 
                                  width = '100%',
                                  multiple = T, 
                                  options = list(delimiter = " ", create = T,maxItems = 10,
                                                 placeholder = "No more than 10 genes.",
                                                 plugins = list('remove_button', 'drag_drop')))
  ),
  conditionalPanel('input.cf_data_type == "Clidx"',
                   selectizeInput(inputId = "cf_clidx_id", 
                                  label = "Select/Input Clinical characteristics, E.g. 'TMB'", 
                                  choices = NULL, 
                                  width = '100%',
                                  multiple = T, 
                                  options = list(delimiter = " ", create = T,
                                                 placeholder = "Select Clinical characteristics.",
                                                 plugins = list('remove_button', 'drag_drop')))
  ),
  colourpicker::colourInput(inputId = "cf_color_id", 
                            label = tags$span(
                              add_prompt(tags$span(icon(name = "circle-question")),
                                         message = "Color for line", 
                                         position = "right"),
                              'Color'),  
                            value = "#FF9800", 
                            showColour = c("both", "text","background"), 
                            palette = c("square", "limited"), 
                            allowTransparent = FALSE, 
                            returnName = TRUE),
  # 提交按钮
  # actionButton(inputId = "cf_goButton",
  #              label = "Submit",
  #              class ="btn-primary")
  fluidRow(
    column(2, 
           div(style="display:inline-block",actionButton(inputId = "cf_goButton",label = "Submit",class ="btn-primary"), style="float:left"),
           
    ),
    column(7),
    column(2, 
           div(style="display:inline-block",actionButton("reset_input_cf", "Clear",class="btn-warning"), style="float:left"),
           
    )
  )
  
)
# UI-右侧展示区
mainpage <- mainPanel(
  id = 'cutpoint_mainpage',
  width = 9,
  uiOutput(outputId='cutpoint_maintabs')
)

# UI
tab_01_cutpoint_analyses$ui <- sidebarLayout(sidebar, mainpage)

# Server
tab_01_cutpoint_analyses$server <- function(input, output,session){
  cat("========================= Start cutpoint filter =====================\n")
  # 定义变量存储输出对象
  cf.output.graphic <- reactiveValues() # 存储最终结果
  # middle.dat <- reactiveValues() # 存储中间数据
  
  observeEvent(input[['reset_input_cf']], {
    shinyjs::reset("cutpoint_sidebar")
  })
  observeEvent(input[['reset_input_cf']], {
    output[['cutpoint_maintabs']] <- NULL
  })
  
  # 更新页面参数
  # 0.根据队列类型（ICI或非ICI队列）从数据库获取肿瘤名称名称
  cf.study.df <- eventReactive(c(input$cf_study_type,input$cf_data_type),{
    
    cf_tmp_var <- input$cf_study_type
    cf_study_type_in = case_when(cf_tmp_var == 'Immunogenomics Studies' ~ 'ICIs',
                                 TRUE ~ 'nonICIs')
    # cat("=====> test study type:",cf_study_type_in,"\n")
    cf_data_type_in <- input$cf_data_type
    if(cf_data_type_in == 'Expr'){
      temp.df <- study.df %>%
        dplyr::filter(grepl("Expr",data_type)) %>%
        dplyr::filter(study_type == cf_study_type_in)
      
      if(cf_study_type_in == 'nonICIs'){
        temp.df <- temp.df%>%
          dplyr::arrange(tumor_detail)
      }
    }
    
    if(cf_data_type_in == 'Clidx'){
      temp.df <- study.df %>%
        dplyr::filter(!is.na(genicity_property)) %>%
        dplyr::filter(study_type == cf_study_type_in)
      
      if(cf_study_type_in == 'nonICIs'){
        temp.df <- temp.df%>%
          dplyr::arrange(desc(tumor_detail))
      }
    }
    
    
    temp.df
  })
  # 将获取到的内更新到页面
  observe({
    cf_cancer_detail_list <- unique(cf.study.df()$tumor_detail)
    updatePickerInput(session = session, 
                      inputId = "cf_cancer_detail",
                      choices = cf_cancer_detail_list, 
                      selected = cf_cancer_detail_list[2])
  })
  
  # 1.根据肿瘤名称和队列类型（ICI或非ICI队列）从数据库获取队列名称
  cf.cohort.df <- eventReactive(c(input$cf_cancer_detail,input$cf_data_type),{
    
    cf_data_type_in  <- input$cf_data_type
    cf_in_cancer_detail <- input$cf_cancer_detail
    # browser()
    if(is.null(cf_in_cancer_detail)) cf_in_cancer_detail <- unique(cf.study.df()$tumor_detail)[1]
    temp.study.df <- cf.study.df() %>%
      dplyr::filter(tumor_detail == cf_in_cancer_detail)
    
    if(cf_data_type_in == 'Clidx'){
      temp.study.df <- temp.study.df %>%
        dplyr::filter(genicity_property != ''| !is.na(genicity_property))
    }
    temp.study.df
  })
  # 将获取到的肿瘤队列list更新到页面
  observe({
    updatePickerInput(session = session, inputId = "cf_study_id",
                      choices = unique(cf.cohort.df()$study_id), selected = NULL)
  })
  
  # 2. 根据队列名称更新基因或临床属性，只对转录组
  # cf.data.df <- eventReactive(c(input$cf_study_id),{
  #   
  #   cf_in_study_id <- input$cf_study_id
  #   # 本次过滤后应只有一条记录
  #   temp.df <- cf.cohort.df() %>%
  #     dplyr::filter(study_id %in% cf_in_study_id) 
  #   return(temp.df)
  # })
  # observe({
  #   # 更新数据类型
  #   updatePickerInput(session = session, inputId = "cf_data_type",
  #                     choices = unique(unlist(str_split(cf.data.df()$data_type[1],"/"))), 
  #                     selected = unique(unlist(str_split(cf.data.df()$data_type[1],"/")))[1])
  # })
    
  cf.symbol.list <- eventReactive(c(input$cf_study_id,input$cf_cancer_detail),{
    
    cf_data_type_in  <- input$cf_data_type
    cf_in_study_id <- input$cf_study_id
    
    if(cf_data_type_in == 'Expr'){
      tbl.name <- paste0(tolower(cf_in_study_id),'_expr')
      db.dat.rna <- queryDataFromMySQL(tbl.name)
      symbol_clidx <- db.dat.rna$hugo_symbol
    }
    
    if(cf_data_type_in == 'Clidx'){
      # browser()
      # 本次过滤后应只有一条记录
      temp.df <- cf.cohort.df() %>%
        dplyr::filter(study_id %in% cf_in_study_id) 
      symbol_clidx <- intersect(str_split(temp.df$genicity_property[1],pattern = '#')[[1]],vars.continues)
      symbol_clidx <- toupper(symbol_clidx)
    }
    return(symbol_clidx)
    
  })  
  observe({
    # 更新基因名称
    updateSelectizeInput(session = session,
                         inputId = "cf_symbol_id",
                         choices = unique(cf.symbol.list()),
                         selected = NULL,
                         server = TRUE)
    
    # 更新属性名称
    updateSelectizeInput(session = session,
                         inputId = "cf_clidx_id",
                         choices = unique(cf.symbol.list()),
                         selected = NULL,
                         server = TRUE)
  })
  
  # submit按钮可点击
  observe({
    shinyjs::toggleState("cf_goButton", 
                         !is.null(input$cf_symbol_id) | input$cf_symbol_id != "" )
  })

  
  # 根据前端参数获取后台数据
  db.dat <- eventReactive(c(input$cf_study_id,
                            input$cf_data_type),{
                              cat("查询Clinical/Expr，待分析的研究队列: ",input$cf_study_id,"\n")
                              data_type <- input$cf_data_type
                              study_id <- input$cf_study_id
                              tbl_name <- case_when(data_type == 'Expr' ~ paste0(tolower(study_id),'_expr'),
                                                    data_type == 'Clidx' ~ paste0(tolower(study_id),'_clinical'),
                                                    TRUE ~ '')
                              temp.df <- queryDataFromMySQL(tbl_name)
                              return(temp.df)
                            })
  # 不管是mRNA还是Clinical，都需要获取生存终点信息，因此需查询临床资料
  db.cli <- eventReactive(input$cf_study_id,{
    # browser()
    cat("查询Clinical，待分析的研究队列: ",input$cf_study_id,"\n")
    study_id <- input$cf_study_id
    cancer_id <- input$cf_cancer_detail # cancer_detail
    temp.df <- queryDataFromMySQL(paste0(tolower(study_id),'_clinical'))
    temp.df <- temp.df %>%
      dplyr::filter(cancer_detail %in% cancer_id)
    if("ici_treatment" %in% colnames(temp.df)){
      temp.df <- temp.df %>%
        # 免疫治疗队列有ici_treatment字段，需过滤出免疫治疗人群
        dplyr::filter(ici_treatment == 'Yes') 
    }
    if("sample_type" %in% colnames(temp.df)){
      temp.df <- temp.df %>%
        # GEO队列存在非肿瘤样本，需选择出肿瘤样本
        dplyr::filter(sample_type == 'T') 
    }
    return(temp.df)
  })
  
  # 输入为空验证
  iv_cf <- InputValidator$new()
  iv_cf$add_rule("cf_symbol_id", sv_required())
  iv_cf$add_rule("cf_clidx_id", sv_required())
  iv_cf$enable()
  
  # 点击submit，执行分析
  observeEvent(input$cf_goButton,{
    
    cat("========================= Server cutpoint fill======================\n")
    show_modal_progress_line(
      color = "#00A064", # DF0101
      trail_color = "#eee",
      duration = 90,
      easing = "easeOut",
      text = "Starting cutpoint analysis  ..."
    )
    
    
    # 获取前端参数
    cf_tmp_var <- input$cf_study_type
    cf_study_type_in = case_when(cf_tmp_var == 'Immunogenomics Studies' ~ 'ICIs',
                                 TRUE ~ 'nonICIs')
    cf_data_type_in <- input$cf_data_type
    cf_cancer_detail_in <- input$cf_cancer_detail
    cf_study_id_in <- input$cf_study_id
    cf_line_color <- input$cf_color_id
    
    if(cf_data_type_in == 'Expr'){
      cf_feature_in <- input$cf_symbol_id
      
      # 根据输入参数选择数据
      client.dat <- db.dat() %>%
        dplyr::filter(hugo_symbol %in% cf_feature_in) %>%
        tibble::column_to_rownames(var = 'hugo_symbol') %>%
        t() %>% as.data.frame() %>%
        tibble::rownames_to_column(var = 'sample_id')
      
    }
    if(cf_data_type_in == 'Clidx'){
      cf_feature_in <- tolower(input$cf_clidx_id)
      
      # 根据输入参数选择数据
      client.dat <- db.dat() %>%
        dplyr::select(sample_id,cf_feature_in) 
      
    }
    
    # 根据参数取出时间字段（只分析有终点的数据）
    survival.outcomes <- colnames(db.cli())[grepl("months|status",colnames(db.cli()))]
    
    input.features <- cf_feature_in 
    # 输出到前端
    output$cutpoint_maintabs <- renderUI({
      tabs <- lapply(input.features, function(name) {
        tabPanel(
          title = toupper(name),
          uiOutput(paste0(name,"_cf"))
        )
      })
      do.call(tabsetPanel, tabs)
      
      # bsModal(
      #   id = "kmplotter", title = "Kaplan-Meier plot", trigger = "showkm", size = "large",
      #   plotOutput("selected_row_plot")
      # )
    })
    
    # 逐个特征（临床属性或者mRNA）分析
    n <- length(input.features)
    lapply(seq(1,n), function(i) {
      
      gname <- input.features[i]
      # 进度条
      update_modal_progress(
        value = i / n,
        text =  paste("Analyzing characteristics/gene:", gname,sprintf("(%1.0f%%)", i/n*100)) #paste("Analyzing gene: ", gname)
      )
      # browser()
      # 构造总体分析数据
      server.df <- db.cli() %>%
        dplyr::select(sample_id,survival.outcomes) %>%
        # 合并前端选择的数据
        dplyr::inner_join(client.dat,by='sample_id')
      
      # 从汇总表中取出被选择队列的既定临床终点,为第二层输出tab名称
      temp.outcome.df <- study.df %>%
        dplyr::filter(study_type %in% cf_study_type_in) %>%
        dplyr::filter(tumor_detail %in% cf_cancer_detail_in) %>%
        dplyr::filter(study_id %in% cf_study_id_in )
      second.tab.names <- toupper(unlist(strsplit(temp.outcome.df$outcome[1],split = "#",fixed = F)))
      second.tab.names <- second.tab.names[which(second.tab.names != '')]
      
      # 每个第二层输出tab的内容
      output[[paste0(gname,"_cf")]] <- renderUI({
        tabss <- lapply(second.tab.names, function(cli.name) {
          tabPanel(
            style = "padding-top:15px",
            title = paste0("Survival ",toupper(cli.name)), status = "primary", solidHeader = TRUE, collapsible = TRUE,
            shinycssloaders::withSpinner(plotOutput(paste0(gname, "_plot_cf_",tolower(cli.name)),height = "350px", width = "100%")),
            # h5(HTML(paste("<span style=color:black;font-size:12px;>", "All Queries Genes: mutation of at least one of query genes. 
            #             Mut: mutation, Wt: wild type.\n Hazard ratio (HR) for mutant (Mut) versus wild-type (Wt)  was calculated by Cox regression.", "</span>"))),
            downloadButton(paste0(gname, "_plot_cf_dl_",tolower(cli.name)), 
                           tags$span(
                             "DLGraph",
                             add_prompt(tags$span(icon(name = "circle-question")),
                                        message = "Save plot in a PDF file.", 
                                        position = "left")),
                           style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
            downloadButton(paste0(gname, "_tbl_cf_dl_",tolower(cli.name)), 
                           # "DLTable",
                           tags$span(
                             "DLTable",
                             add_prompt(tags$span(icon(name = "circle-question")),
                                        message = "Save data of this plot in a txt file.", 
                                        position = "left")),
                           style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
            br(),
            DT::dataTableOutput(paste0(gname, "_tbl_cf_",tolower(cli.name)))
          )
        })
        do.call(tabsetPanel, tabss)
      })
      
      # 遍历第二层tab
      lapply(second.tab.names, function(cli.name) {
        # browser()
        # 构造单个终点分析数据
        second.df <- server.df %>%
          dplyr::select(sample_id,gname,
                        times = paste0(tolower(cli.name),"_months"),
                        status = paste0(tolower(cli.name),"_status")) %>%
          dplyr::mutate(times = round(as.numeric(times),2)) %>%
          dplyr::filter(!is.na(times),status != '',!is.na(status))
        # middle.dat[[paste0(gname,"_",cli.name)]]$tbl <- second.df
        
        # 阈值分析
        # 构造函数对每个基因的不同分割点计算Cox回归结果：HR和95%CI
        cutpoint.out.tabl <- do.call(rbind,lapply(c(gname),func.cutpointcalc,second.df))
        cf.output.graphic[[paste0(gname,"_",tolower(cli.name))]]$tbl <- cutpoint.out.tabl
        
        # 对分析结果绘图
        cutpoint.out.plot <- lapply(gname, func.cutpointplot,cutpoint.out.tabl,toupper(cli.name),cf_line_color)
        cf.output.graphic[[paste0(gname,"_",tolower(cli.name))]]$plot <- cutpoint.out.plot
        
        # 输出到相应Tab项
        output[[paste0(gname, "_plot_cf_",tolower(cli.name))]] <- renderPlot({
          print(cutpoint.out.plot)
        })
        output[[paste0(gname, "_tbl_cf_",tolower(cli.name))]] <- renderDataTable({
          datatable(cutpoint.out.tabl, 
                    style = 'bootstrap', 
                    rownames = FALSE, 
                    selection = list(mode = "single", target = "row"),
                    options = list(orderClasses = TRUE,keys = TRUE,dom = 'Bfrtip', searching = F,
                                   language = list(zeroRecords = "No records found matching your selection"),
                                   columnDefs = list(list(className = 'dt-center', targets = '_all'))),
                    colnames = c("Row ID","cutpoint", "Propertion", "HR","95%CI Lower", "95CI% Upper","P value",'Feature','Maxstat cutpoint')
          )
        })
        
        output[[paste0(gname, "_plot_cf_dl_",tolower(cli.name))]] <- downloadHandler(
          filename = function() {
            paste0(prefix_output_file,"_cutpoint_Plot_",toupper(cli.name),"_",gname,"_",Sys.Date(),".pdf", sep = "")
          },
          content = function(file) {
            pdf(file,onefile = F, width = 6, height = 4, pointsize = 10)
            print(cf.output.graphic[[paste0(gname,"_",tolower(cli.name))]]$plot)
            dev.off()
          }
        )
        
        output[[paste0(gname, "_tbl_cf_dl_",tolower(cli.name))]] <- downloadHandler(
          filename = function() {
            paste0(prefix_output_file,"_cutpoint_Data_",toupper(cli.name),"_",gname,"_",Sys.Date(), '.txt', sep='')
          },
          content = function(file) {
            readr::write_delim(x = cf.output.graphic[[paste0(gname,"_",tolower(cli.name))]]$tbl, path = file, delim = "\t")
          }
        )
        
        # 选择数据行弹窗
        # selected.row <- eventReactive(input[[paste0(gname,"_tbl_cf_",tolower(cli.name),"_rows_selected")]],{
        #   slted.row <- input[[paste0(gname,"_tbl_cf_",tolower(cli.name),"_rows_selected")]]
        #   cat(cat_prefix_cf,"-选择的行是:",slted.row,"\n")
        #   return(slted.row)
        # })
        # output[["selected_row_plot"]] <- renderPlot({
        #   cat("Herrr!\n")
        #   # 选择数据行，弹出KM曲线
        #   
        #   
        #   dat.selected.df <- cf.output.graphic[[paste0(gname,"_",tolower(cli.name))]]$tbl[selected.row(),]
        #   
        #   plot.vars <- dat.selected.df$Feature[1]
        #   plot.cutpoint <- dat.selected.df$cutpoint[1]
        #   cat(cat_prefix_ici,"- 选择的cutpoint值:",plot.cutpoint,"\n")
        #   
        #   plot.df <- middle.dat[[paste0(gname,"_",cli.name)]]$tbl
        #   plot.df <- plot.df %>%
        #     dplyr::rename(risk_value = gname) %>%
        #     dplyr::mutate(risk = factor(ifelse(risk_value >= plot.cutpoint,'High','Low'),levels = c('Low','High'))) %>%
        #     dplyr::select(sample_id,times,status,risk)
        #   plot.df %>%
        #     func.survivalplot('',cli.name,
        #                       time.break = time_break_survival(max(.$times,na.rm = T)),
        #                       tbl.loc = 'topright',
        #                       color.dis = c("#2196F3","#FF9800"),
        #                       y.title = case_when(cli.name == 'PFS' ~ 'Progression-free Survival',
        #                                           cli.name == 'DFS' ~ 'Disease-free Survival',
        #                                           cli.name == 'PFI' ~ 'Progression-free Interval',
        #                                           cli.name == 'DFI' ~ 'Disease-free Interal',
        #                                           cli.name == 'DSS' ~ 'Disease-specific Survival',
        #                                           cli.name == 'OS' ~ 'Overall Survival',
        #                                           TRUE ~ 'Survival'))
        # })
        
      })
      
      
    })
    remove_modal_progress()
    
  })
}



