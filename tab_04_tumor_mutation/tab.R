#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : tab.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-05-25
# @IDE     : RStudio
# @Desc    : 肿瘤突变分析，只针对有mutatuon数据的队列
#===============================================================================

source("./r/func.bacthchitest.R")

tab_04_tumor_mutation <- list()
cat_prefix_mut <- 'Tumormutation'

sidebar <- sidebarPanel(
  id = 'tumormutation_sidebar',
  width = 3,
  h3("Tumor Mutation Profile"),
  
  # 研究类型
  awesomeRadio(inputId = "tumormutation_study_type",
               label = "Select study types", 
               choices = c("Immunogenomics Studies", "Non-immunogenomics Studies"),
               selected = "Immunogenomics Studies",
               # inline = TRUE,
               status = "success"),
  # 肿瘤
  conditionalPanel('input.tumormutation_study_type != ""',
                   pickerInput(inputId = "tumormutation_cancer_detail",
                               label = "Select one cancer type, E.g. 'LUAD'",
                               choices = NULL,
                               multiple = FALSE,
                               selected = NULL)),
  # 肿瘤队列
  conditionalPanel('input.tumormutation_cancer_detail != ""',
                   pickerInput(inputId = "tumormutation_study_id",
                               label = "Select one dataset, E.g. 'Hellmann_2018'",
                               choices = NULL,
                               multiple = FALSE,
                               selected = NULL)),
  conditionalPanel('input.tumormutation_study_id != ""',
                   # 队列数据类型：Mutation,RNA,Protein,Methyl
                   pickerInput("tumormutation_data_type",
                               label = "PSeelct data type of grouped genes, E.g. 'Mutation'", 
                               choices = NULL,
                               selected = NULL, 
                               multiple = FALSE)),
  conditionalPanel('input.tumormutation_study_id != "" & input.tumormutation_data_type != ""',
                   # 队列数据的基因list
                   selectizeInput(inputId = "tumormutation_symbol_id", 
                                  label = "Enter grouped gene symbols, E.g. 'EGFR STK11'", 
                                  choices = NULL, 
                                  width = '100%',
                                  multiple = T, 
                                  options = list(delimiter = " ", create = T,
                                                 maxItems = 10,
                                                 placeholder = "No more than 10 genes.",
                                                 plugins = list('remove_button', 'drag_drop'))),
                   conditionalPanel("input.tumormutation_data_type == 'Mutation'" ,
                                    awesomeRadio(
                                      inputId = "tumormutation_logical",
                                      label = "Mutations in all(AND)/any(OR) quried genes",
                                      inline = T,
                                      choices = c("AND" ,"OR"),
                                      selected = "AND",
                                      status = "success",
                                      checkbox = TRUE
                                    )),
                   # 队列数据的基因表达阈值
                   conditionalPanel("input.tumormutation_data_type == 'Expr' || input.tumormutation_data_type == 'Proteome'",
                                    fluidRow(
                                      column(6,numericInput(inputId = "tumormutation_exprcut1_id",
                                                            label = tags$span(
                                                              add_prompt(tags$span(icon(name = "circle-question")),
                                                                         message = "Percentile threshold for RNA data", 
                                                                         position = "right"),
                                                              "Percentile cutoff (%,High):"),
                                                            value = 50,
                                                            min = 0,
                                                            max = 100,step = NA,width = '100%')),
                                      column(5,numericInput(inputId = "tumormutation_exprcut2_id",
                                                            label = "",
                                                            value = 50,
                                                            min = 0,
                                                            max = 100,step = NA,width = '100%'))
                                    )
                   ),
                   selectizeInput(inputId = "tumormutation_display_symbol_id", 
                                  label = "Enter displayed gene symbols, E.g. 'TP53 KEAP1 NFE2L2'", 
                                  choices = NULL, 
                                  width = '100%',
                                  multiple = T, 
                                  options = list(delimiter = " ", create = T,maxItems = 50,
                                                 placeholder = "Less than 50 genes. Top 20 Genes will be selected if NULL!",
                                                 plugins = list('remove_button', 'drag_drop')))),
  conditionalPanel('input.tumormutation_study_id != ""',
                   # 队列数据的基因突变类型
                   pickerInput(inputId = "tumormutation_vartype_id",
                               label = tags$span(
                                 add_prompt(tags$span(icon(name = "circle-question")),
                                            message = "Variant Classification for Mutation data", 
                                            position = "right"),
                                 "Select gene mutation types"),
                               choices = NULL,
                               multiple = TRUE,
                               selected = NULL)
                   
  ),
  # 是否排出高频突变基因
  checkboxInput("exflags", 
                # "Exclude known frequently mutated genes (FLAGs)?",
                HTML("Exclude known frequently mutated genes", 
                     as.character(actionLink(inputId = 'show_flags', label = '(FLAGs)'))),
                TRUE),
  # 分组颜色
  fluidRow(
    column(6,colourpicker::colourInput(inputId = "tumormutation_colorg1_id", 
                                       label = tags$span(
                                         add_prompt(tags$span(icon(name = "circle-question")),
                                                    message = "Color for High/Mut group", 
                                                    position = "right"),
                                         'Color 1'), 
                                       value = "#FF9800", 
                                       showColour = c("both", "text","background"), 
                                       palette = c("square", "limited"), 
                                       allowTransparent = FALSE, 
                                       returnName = TRUE)),
    column(5,colourpicker::colourInput(inputId = "tumormutation_colorg2_id", 
                                       label = tags$span(
                                         add_prompt(tags$span(icon(name = "circle-question")),
                                                    message = "Color for Low/WT group", 
                                                    position = "right"),
                                         'Color 2'),  
                                       value = "#2196F3", 
                                       showColour = c("both", "text","background"), 
                                       palette = c("square", "limited"), 
                                       allowTransparent = FALSE, 
                                       returnName = TRUE))),
  
  # actionLink("flags_list","Detail info of FLAGs genes"),
  bsModal(#"flags_info_popup","FLAGs filter","showdemo",
          id = "flags_info_popup", title = "FLAGs Gene List", trigger = "show_flags",
          includeMarkdown("tab_08_user_defined/flags.md"),
          size="large"),
  br(),
  # 提交按钮
  # actionButton(inputId = "tumormutation_goButton",
  #              label = "Submit",
  #              class ="btn-primary")
  fluidRow(
    column(2, 
           div(style="display:inline-block",actionButton(inputId = "tumormutation_goButton",label = "Submit",class ="btn-primary"), style="float:left"),
           
    ),
    column(7),
    column(2, 
           div(style="display:inline-block",actionButton("reset_input_tumormutation", "Clear",class="btn-warning"), style="float:left"),
           
    )
  )
)

mainpage <- mainPanel(
  id = 'tumormutation_mainpage',
  width = 9,
  uiOutput(outputId='tumormutation_maintabs')
)

tab_04_tumor_mutation$ui <- sidebarLayout(sidebar, mainpage)

tab_04_tumor_mutation$server <- function(input, output,session) {
  cat("========================= Start Tumor Mutation ======================\n")
  
  # 定义变量存储输出对象
  output.graphic <- reactiveValues()
  
  observeEvent(input[['reset_input_tumormutation']], {
    shinyjs::reset("tumormutation_sidebar")
  })
  observeEvent(input$reset_input_tumormutation, {
    output[['tumormutation_maintabs']] <- NULL
  })
  
  # 0.根据队列类型（ICI或非ICI队列）从数据库获取肿瘤名称名称
  tumor.names.df <- eventReactive(input$tumormutation_study_type,{
    study_type <- input$tumormutation_study_type
    # 要求有mutation数据
    temp.df <- study.df %>%
      dplyr::filter(grepl("Mutation",data_type))
    if (study_type == 'Immunogenomics Studies'){
      temp.df <- temp.df %>%
        dplyr::filter(study_type == 'ICIs')
    }else{ 
      temp.df <- temp.df %>%
        dplyr::filter(study_type == 'nonICIs') #%>%
    }
    temp.df
  })
  # 将获取到的内更新到页面
  observe({
    updatePickerInput(session = session, inputId = "tumormutation_cancer_detail",
                      choices = unique(tumor.names.df()$tumor_detail), selected = NULL)
  })
  
  # 1.根据肿瘤名称和队列类型（ICI或非ICI队列）从数据库获取队列名称
  study.names.df <- eventReactive(c(input$tumormutation_cancer_detail),{
    study_type <- input$tumormutation_study_type
    cancer <- input$tumormutation_cancer_detail
    temp.df <- study.df %>%
      dplyr::filter(tumor_detail == cancer) %>%
      dplyr::filter(grepl("Mutation",data_type))
    
    if (study_type == 'Immunogenomics Studies'){
      temp.df <- temp.df %>%
        dplyr::filter(study_type == 'ICIs')
    }else{
      temp.df <- temp.df %>%
        dplyr::filter(study_type == 'nonICIs') 
    }
    temp.df
  })
  # 将获取到的肿瘤队列list更新到页面
  observe({
    updatePickerInput(session = session, inputId = "tumormutation_study_id",
                      choices = unique(study.names.df()$study_id), selected = NULL)
  })
  # 2. 获取数据类型
  data.types.df <- eventReactive(c(input$tumormutation_study_id),{
    
    temp_id <- input$tumormutation_study_type 
    slt_study_type_id <- case_when(temp_id == 'Immunogenomics Studies' ~ 'ICIs',
                                   TRUE  ~ 'nonICIs')
    slt_cancer_id <- input$tumormutation_cancer_detail 
    slt_study_id <- input$tumormutation_study_id
    
    # 本次过滤后应只有一条记录
    temp.df <- study.df %>%
      dplyr::filter(study_type %in% slt_study_type_id) %>%
      dplyr::filter(tumor_detail %in% slt_cancer_id) %>%
      dplyr::filter(study_id %in% slt_study_id) %>%
      dplyr::filter(grepl("Mutation",data_type))
    
    data.types <- unique(unlist(str_split(temp.df$data_type[1],"/")))
    return(data.types)
  })
  # 将获取到数据类型更新到页面
  observe({
    updatePickerInput(session = session, inputId = "tumormutation_data_type",
                      choices = data.types.df(), 
                      selected = data.types.df()[1])
  })
  # 3.1 根据队列名称从数据获取队列内的基因更新分组基因
  study.grp.df <- eventReactive(c(input$tumormutation_data_type,input$tumormutation_study_id),{
    
    study <- input$tumormutation_study_id
    cat(cat_prefix_mut,"-前端返回的队列名称: ",study,"\n")
    
    # data_type = data.types.df()[1]
    data_type = input$tumormutation_data_type
    if(is.null(data_type) || data_type == '') {
      data_type = data.types.df()[1]
    }
    cat(cat_prefix_mut,"-DB返回的数据类型列表: ",data_type,"\n")
    
    db.dat <- data.frame()
    if(data_type == 'Mutation'){
      tbl.name <- paste0(tolower(study),'_mutation')
      db.dat.dna <- queryDataFromMySQL(tbl.name)
      db.dat <- db.dat.dna
    }
    if(data_type == 'Expr'){
      tbl.name <- paste0(tolower(study),'_expr')
      db.dat.rna <- queryDataFromMySQL(tbl.name)
      db.dat.rna$variant_classification <- ''
      db.dat <- db.dat.rna
    }
    if(data_type == 'Proteome'){
      tbl.name <- paste0(tolower(study),'_proteome')
      db.dat.rna <- queryDataFromMySQL(tbl.name)
      db.dat.rna$variant_classification <- ''
      db.dat <- db.dat.rna
    }
    return(db.dat)
  })
  # 将获取到的内更新到页面分组基因
  observe({
    # 更新分组基因名
    updateSelectizeInput(session = session,
                         inputId = "tumormutation_symbol_id",
                         choices = unique(study.grp.df()$hugo_symbol),
                         selected = NULL,
                         server = TRUE)
  })
  # 3.2 根据队列名称从数据获取队列内的基因更新展示基因和突变类型
  study.dis.df <- eventReactive(c(input$tumormutation_study_id),{
    
    study <- input$tumormutation_study_id
    # data_type = data.types.df()[1]
    
    tbl.name <- paste0(tolower(study),'_mutation')
    db.dat.dna <- queryDataFromMySQL(tbl.name)
    db.dat <- db.dat.dna
    
    # if(data_type == 'Expr'){
    #   # 当选者用表达值分组时
    #   tbl.name <- paste0(tolower(study),'_expr')
    #   db.dat.rna <- queryDataFromMySQL(tbl.name)
    #   db.dat <- db.dat.dna %>%
    #     dplyr::filter(hugo_symbol %in% db.dat.rna$hugo_symbol)
    # }
    return(db.dat)
  })
  # 将获取到的内更新到页面分组基因
  observe({
    
    # 更新展示基因名,不超过50个
    updateSelectizeInput(session = session,
                         inputId = "tumormutation_display_symbol_id",
                         choices = unique(study.dis.df()$hugo_symbol),
                         selected = NULL,
                         server = TRUE)
    
    # 如果是突变，则显示突变类型
    # 更新突变类型
    var.types <- unique(study.dis.df()$variant_classification)
    updatePickerInput(session = session, inputId = "tumormutation_vartype_id",
                      # label = "Select/deselect all options",
                      choices = var.types, selected = var.types,
                      options = list(
                        `actions-box` = TRUE))
  })
  
  # 4. 基因表达分组cutoff更新联动
  observe({
    number1 <- input$tumormutation_exprcut1_id  # value 1
    updateNumericInput(session, inputId = "tumormutation_exprcut2_id",
                       label = "Cutoff (%, Low):",
                       value = 100-number1,
                       min = 0, max = 100, step = NA)
  })
  # 若需要cutoff1与cutoff2联动，则取消以下注释；本处为方便设置不同的cutoff，因此不对cutoff1联动
  # observe({
  #   number2 <- input$tumormutation_exprcut2_id  # value 1
  #   updateNumericInput(session, inputId = "tumormutation_exprcut1_id",
  #                      label = "Cutoff (High):",
  #                      value = 100-number2,
  #                      min = 0, max = 100, step = NA)
  # })  
  
  observe({
    shinyjs::toggleState("tumormutation_goButton", 
                         !is.null(input$tumormutation_symbol_id) && input$tumormutation_symbol_id != "" )
  })
  
  # 从DB获取数据
  # 临床信息
  db.dat.cli <- eventReactive(input$tumormutation_study_id,{
    cat(cat_prefix_mut,"-查询Clinical，待分析的研究队列: ",input$tumormutation_study_id,"\n")
    study <- input$tumormutation_study_id
    cancer_id <- input$tumormutation_cancer_detail
    
    # 查 临床信息
    temp.df <- queryDataFromMySQL(paste0(tolower(study),'_clinical'))
    temp.df <- temp.df %>%
      dplyr::filter(cancer_detail %in% cancer_id)
    
    # if("ici_treatment" %in% colnames(temp.df)){
    #   temp.df <- temp.df %>%
    #     # 免疫治疗队列有ici_treatment字段，需过滤出免疫治疗人群
    #     dplyr::filter(ici_treatment == 'Yes') 
    # }
    # 筛选肿瘤样本
    if("sample_type" %in% colnames(temp.df)){
      temp.df <- temp.df %>%
        # GEO队列存在非肿瘤样本，需选择出肿瘤样本
        dplyr::filter(sample_type == 'T') 
    }
    return(temp.df)
  })
  # 查 突变或者表达谱，用于构建分组信息
  db.dat <- eventReactive(c(input$tumormutation_study_id,
                            input$tumormutation_data_type),{
                              cat(cat_prefix_mut,"-查询Mutation/Expr，待分析的研究队列: ",input$tumormutation_study_id,"\n")
                              data_type <- input$tumormutation_data_type
                              study <- input$tumormutation_study_id
                              tbl_name <- case_when(data_type == 'Mutation' ~ paste0(tolower(study),'_mutation'),
                                                    data_type == 'Expr' ~ paste0(tolower(study),'_expr'),
                                                    data_type == 'Proteome' ~ paste0(tolower(study),'_proteome'),
                                                    TRUE ~ '')
                              temp.df <- queryDataFromMySQL(tbl_name)
                              return(temp.df)
                            })
  
  # 本功能中要求队列必须有Mutation数据，本处事先查询出数据,后续用于绘图
  db.dat.mut <- eventReactive(c(input$tumormutation_study_id,input$tumormutation_data_type),{
                              # cat(cat_prefix_mut,"-查询Mutation，待分析的研究队列: ",input$tumormutation_study_id,"\n")
                              data_type <- input$tumormutation_data_type
                              study <- input$tumormutation_study_id
                              tbl_name <- paste0(tolower(study),'_mutation')
                              temp.df <- queryDataFromMySQL(tbl_name)
                              return(temp.df)
                            })
  # 获取宽数据格式
  # db.mut.wide <- reactive({
  #   # 整理突变数据为宽数据格式,要求数据必须要有：hugo_symbol,sample_id,variant_classification 三列
  #   wide.df <- db.dat.mut() %>%
  #     dplyr::select(sample_id,hugo_symbol,type = variant_classification) %>%
  #     dplyr::distinct() 
  #   
  #   return(wide.df)
  # })
  # 获取所有的突变类型变量
  # db.type.df <- reactive({
  #   type.df <- db.dat.mut() %>%
  #     dplyr::select(sample_id,variant_classification) %>%
  #     dplyr::distinct(variant_classification)
  #   return(type.df)
  # })
  
  # 输入为空验证
  iv_tumormutation <- InputValidator$new()
  iv_tumormutation$add_rule("tumormutation_symbol_id", sv_required())
  iv_tumormutation$add_rule("tumormutation_vartype_id", sv_required())
  iv_tumormutation$add_rule("tumormutation_exprcut1_id", sv_gt(9))
  iv_tumormutation$add_rule("tumormutation_exprcut1_id", sv_lt(91))
  iv_tumormutation$add_rule("tumormutation_exprcut2_id", sv_gt(9))
  iv_tumormutation$add_rule("tumormutation_exprcut2_id", sv_lt(91))
  iv_tumormutation$enable()
  
  # 业务层
  observeEvent(input$tumormutation_goButton,{
    cat("===================== Server Tumor Mutation =======================\n")
    # 进度条
    show_modal_progress_line(
      color = "#00A064", # DF0101
      trail_color = "#eee",
      duration = 90,
      easing = "easeOut",
      text = "Starting mutation analysis ..."
    )
    # 获取参数
    selected_study_id = input$tumormutation_study_id
    selected_tumor_detail_id = input$tumormutation_cancer_detail
    cat(cat_prefix_mut,"-选择的研究队列: ", selected_study_id, "\n")
    
    data_type <- input$tumormutation_data_type
    cat(cat_prefix_mut,"-选择的队列数据类型: ", data_type, "\n")
    # 分组基因名
    # validate(need(try(input$tumormutation_symbol_id),"Please enter at least one gene!"))
    symbol <- input$tumormutation_symbol_id
    cat(cat_prefix_mut,"-选择的分组基因名称: ", symbol, "\n")
    # 展示的基因
    symbol_dis <- input$tumormutation_display_symbol_id
    cat(cat_prefix_mut,"-选择的展示基因名称: ", symbol_dis, "\n")
    
    vartype <- input$tumormutation_vartype_id
    cat(cat_prefix_mut,"-选择的基因突变类型: ", vartype, "\n")
    
    exprcut1 <- input$tumormutation_exprcut1_id
    exprcut2 <- input$tumormutation_exprcut2_id
    cat(cat_prefix_mut,"-选择的基因高表达分位数: ", exprcut1, "\n")
    cat(cat_prefix_mut,"-选择的基因低表达分位数: ", exprcut2, "\n")
    
    colorg1 <- input$tumormutation_colorg1_id
    colorg2 <- input$tumormutation_colorg2_id
    cat(cat_prefix_mut,"-选择的样本分组1颜色: ", colorg1, "\n")
    cat(cat_prefix_mut,"-选择的样本分组2颜色: ", colorg2, "\n")
    
    tumormutation_logical_type <- input$tumormutation_logical
    cat(cat_prefix_mut,"-选择的多基因突变逻辑关系: ", tumormutation_logical_type, "\n")
    
    # 数据清理、绘图、输出
    input.genes <- symbol 
    if(data_type == 'Mutation'){
      # input.genes <- c(input.genes,"All Queried Genes")
      if(length(input.genes) >1){
        input.genes <- c(input.genes,"All Queried Genes")
      }
    }
    input.vartype.tumormutation <- vartype
    input.exprcut1 <- exprcut1/100  #转化为分位数函数的输入值
    input.exprcut2 <- exprcut2/100
    # input.color <- c(colorg1,colorg2)
    
    # 写出到页面，每个基因一个tab
    output$tumormutation_maintabs <- renderUI({
      tabs <- lapply(input.genes, function(name) {
        tabPanel(
          title = name,
          uiOutput(paste0(name,"_mut"))
        )
      })
      do.call(tabsetPanel, tabs)
    })
    # 执行数据分析
    # withProgress(message = 'Making plot', value = 0, {
      n <- length(input.genes)
      lapply(seq(1,length(input.genes)), function(i) {
        
        gname <- input.genes[i]
        symbol.name <- gname
        if(gname == 'All Queried Genes') {symbol.name <- setdiff(input.genes,gname)} # EGFR#TP53#ALK
        cat("current gene: ",symbol.name,"\n")
        
        # 构建分组信息
        if(data_type == 'Mutation'){
          if(gname == 'All Queried Genes'){
            # 多个基因时
            if(tumormutation_logical_type == 'AND'){
              # logical 为 AND 时
              
              cat(cat_prefix_mut,"- 逻辑与选择多基因突变模式！\n")
              tmp.mut.samples <- db.dat() %>%
                dplyr::filter(hugo_symbol %in% symbol.name,
                              variant_classification %in% input.vartype.tumormutation) %>%
                dplyr::select(sample_id,hugo_symbol) %>%
                dplyr::distinct(sample_id,hugo_symbol,.keep_all = T) %>%
                dplyr::group_by(sample_id) %>%
                dplyr::summarise(sample_count = n()) %>%
                dplyr::ungroup() %>%
                dplyr::filter(sample_count == length(symbol.name)) # 取
            }
            if(tumormutation_logical_type == 'OR'){
              # logical 为 OR 时
              cat(cat_prefix_mut,"- 逻辑或选择多基因突变模式！\n")
              tmp.mut.samples <- db.dat() %>%
                dplyr::filter(hugo_symbol %in% symbol.name,
                              variant_classification %in% input.vartype.tumormutation) %>%
                dplyr::distinct(sample_id,hugo_symbol,.keep_all = T) %>%
                dplyr::group_by(sample_id) %>%
                dplyr::summarise(sample_count = n()) %>%
                dplyr::ungroup() %>%
                dplyr::filter(sample_count >= 1) # 取
            }
            grp1.df <- db.dat() %>%
              dplyr::filter(hugo_symbol %in% symbol.name,
                            variant_classification %in% input.vartype.tumormutation) %>%
              dplyr::filter(sample_id %in% tmp.mut.samples$sample_id) %>%
              dplyr::select(sample_id,hugo_symbol)
          }else{
            grp1.df <- db.dat() %>%
              dplyr::filter(hugo_symbol %in% symbol.name,
                            variant_classification %in% input.vartype.tumormutation) %>%
              dplyr::select(sample_id,hugo_symbol)
          }
          
          clc.cli <- db.dat.cli() %>%
            dplyr::mutate(group = ifelse(sample_id %in% unique(grp1.df$sample_id),'Mut','WT')) %>%
            dplyr::select(sample_id,group)
          
        }
        if(data_type %in% c('Expr','Proteome')){
          # browser()
          # symbol.name必须为单个基因的名称
          grp1.df <- db.dat() %>%
            dplyr::filter(hugo_symbol %in% symbol.name) %>%
            tibble::column_to_rownames(var = 'hugo_symbol') %>%
            t() %>% as.data.frame() %>%
            tibble::rownames_to_column(var = 'sample_id')
          colnames(grp1.df)[2] <- "sltgene"
          
          cutoff_h <- quantile(as.numeric(grp1.df$sltgene),input.exprcut1,na.rm = T)
          cutoff_l <- quantile(as.numeric(grp1.df$sltgene),input.exprcut2,na.rm = T)
          # 对某些基因的表达值存在75%都想同时的处理
          quantile.25 <- quantile(as.numeric(grp1.df$sltgene),0.25,na.rm = T)
          quantile.75 <- quantile(as.numeric(grp1.df$sltgene),0.75,na.rm = T)
          if(quantile.25 == quantile.75){
            cat("基因：",symbol.name,"的第一分位数和第三分为数相等,无法以75%以内分位值为阈值。系统将以第一个不同的表达值为阈值分组\n")
            cutoff_h <- unique(sort(as.numeric(grp1.df$sltgene)))[2]
            cutoff_l <- unique(sort(as.numeric(grp1.df$sltgene)))[2]
            cat("New Cutoff of Expression: ",cutoff_h," (High); ",cutoff_l," (Low)\n")
          }
          grp1.df <- grp1.df %>%
            dplyr::mutate(sltgene = as.numeric(sltgene)) %>%
            dplyr::mutate(group = ifelse(sltgene >= cutoff_h,"High",
                                         ifelse(sltgene <= cutoff_l,'Low','Middle'))) %>%
            dplyr::filter(group %in% c('Low','High')) %>%
            dplyr::select(sample_id,group)
          # grp1.df$group <- factor(grp1.df$group,levels = c('Low','High'))
          clc.cli <- db.dat.cli() %>%
            dplyr::inner_join(grp1.df,by='sample_id')  %>%
            dplyr::select(sample_id,group)
        }
        
        output[[paste0(gname,"_mut")]] <- renderUI({
          # tabss <- lapply(cli.tab.names, function(cli.name) {
          # })
          # do.call(tabsetPanel, tabss)
          # 热图
          tabPanel(
            style = "padding-top:15px",
            title = paste0(""), status = "primary", solidHeader = TRUE, collapsible = TRUE,
            shinycssloaders::withSpinner(plotOutput(paste0(gname, "_plot_mut"),height = 500)),
            br(),
            # 图片下载按钮
            downloadButton(paste0(gname, "_dl_plot_mut"), 
                           tags$span(
                             "DLGraph",
                             add_prompt(tags$span(icon(name = "circle-question")),
                                        message = "Save plot in a PDF file.", 
                                        position = "left")),
                           style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
            # 数据下载按钮
            downloadButton(paste0(gname, "_dl_tbl_mut"), 
                           tags$span(
                             "DLTable",
                             add_prompt(tags$span(icon(name = "circle-question")),
                                        message = "Save data of heatmap plot in a txt file.", 
                                        position = "left")),
                           style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
            br(),
            # 数据表格
            DT::dataTableOutput(paste0(gname, "_tbl_mut"))
          )
        })
        
        # lapply(cli.tab.names, function(cli.name) {})
        
        # 分组信息提取和样本ID确定
        {
          comm.samples <- intersect(db.dat.mut()$sample_id,clc.cli$sample_id)
          grp.df <- clc.cli %>%
            dplyr::filter(sample_id %in% comm.samples) %>%
            # dplyr::filter(group %in% c('High','Low','Mut','WT')) %>%
            dplyr::arrange(group) 
          mut.df <- db.dat.mut() %>%
            dplyr::filter(sample_id %in% grp.df$sample_id) %>%
            # 分析指定的突变类型的数据
            dplyr::filter(variant_classification %in% input.vartype.tumormutation)
        }
        
        flag_genes <- c("TTN","MUC16","OBSCN","AHNAK2","SYNE1","FLG","MUC5B","DNAH17","PLEC","DST","SYNE2","NEB","HSPG2","LAMA5","AHNAK","HMCN1","USH2A","DNAH11","MACF1","MUC17","DNAH5","GPR98","FAT1","PKD1","MDN1","RNF213","RYR1","DNAH2","DNAH3","DNAH8","DNAH1","DNAH9","ABCA13","SRRM2","CUBN","SPTBN5","PKHD1","LRP2","FBN3","CDH23","DNAH10","FAT4","RYR3","PKHD1L1","FAT2","CSMD1","PCNT","COL6A3","FRAS1","FCGBP","RYR2","HYDIN","XIRP2","LAMA1")
        # 提取高频突变的基因Top20
        if(symbol_dis == '' || is.null(symbol_dis)){
          freq.df <- mut.df %>%
            dplyr::select(sample_id,hugo_symbol,type = variant_classification) %>%
            dplyr::distinct() %>%
            dplyr::group_by(hugo_symbol) %>%
            dplyr::summarise(freq = n()) %>%
            dplyr::ungroup() %>%
            dplyr::filter(hugo_symbol != 'Unknown') %>%
            dplyr::mutate(pct = freq/length(unique(mut.df$sample_id)) * 100 ) %>%
            dplyr::arrange(desc(freq),desc(pct))
          # 过滤FLAGs
          if (input$exflags) {
            freq.df <- freq.df %>%
              dplyr::filter(!hugo_symbol %in% flag_genes)
          }
          
          # 选则高频图片的前20个基因作为展示
          if(nrow(freq.df) >= 20){
            gene.dis <- freq.df$hugo_symbol[1:20]
          }else{
            gene.dis <- freq.df$hugo_symbol
          }
        }else{
          # 否则用客户前端输入的
          gene.dis <- symbol_dis
          # 过滤FLAGs
          if (input$exflags) {
            gene.dis <- setdiff(gene.dis,flag_genes)
          }
        }
        # 宽数据转换
        {
          # 整理突变数据为宽数据格式,要求数据必须要有：hugo_symbol,sample_id,variant_classification 三列
          wide.df <- mut.df %>%
            dplyr::select(sample_id,hugo_symbol,type = variant_classification) %>%
            dplyr::distinct() %>%
            dplyr::filter(hugo_symbol %in% gene.dis)
          
          # 聚合同sample_id下同基因的突变类型
          onco.mat <- aggregate(wide.df[,3],
                                by=list(gene = wide.df$hugo_symbol,
                                        sample=wide.df$sample_id),
                                FUN=paste0)  
          # cat("======>",colnames(onco.mat),"\n")
          colnames(onco.mat)[3] <- 'Variant_Classification'
          plot.mat <- onco.mat %>%
            # dplyr::rename(Variant_Classification = x) %>%
            dplyr::mutate(Variant_Classification = as.character(Variant_Classification)) %>%          # list 转 character
            dplyr::mutate(Variant_Classification = gsub('^[c]',"",Variant_Classification)) %>%        # 消除c
            dplyr::mutate(Variant_Classification = gsub('[(\\"\\")]',"",Variant_Classification)) %>%  # 消除括号、引号
            dplyr::mutate(Variant_Classification = gsub(',',": ",Variant_Classification))  %>%         # 替换 ,
            tidyr::spread(key=sample,value=Variant_Classification) %>%
            tibble::column_to_rownames(var = "gene") %>%
            t() %>% as.data.frame() %>%
            tibble::rownames_to_column(var = "sample_id") %>%
            dplyr::left_join(grp.df,.,by='sample_id') %>%   #grp.df已排序，目的是保持两者序列一致
            dplyr::select(-group) %>%
            tibble::column_to_rownames(var = "sample_id") %>%
            t() # 转置后的数据：行为基因，列为样本
          plot.mat[is.na(plot.mat)] <- ''
        }
        # 对基因在两组的突变做卡方检验
        {
          chi.df <- onco.mat %>%
            dplyr::mutate(ismutant = 1) %>%
            tidyr::pivot_wider(id_cols = 'sample',names_from = 'gene',values_from = 'ismutant') %>%
            dplyr::left_join(dplyr::select(grp.df,sample_id,group),.,by=c('sample_id' = 'sample')) 
          
          calc.exp <- chi.df %>%
            dplyr::filter(!is.na(group))
          calc.exp[is.na(calc.exp)] <- 0
          # browser()
          calc.out <- do.call(rbind,lapply(colnames(calc.exp)[-(1:2)], func.bacthchitest,calc.exp))
          # 修改基因的名称格式为，与plot.mat同样的行顺序
          calc.str <- calc.out[match(rownames(plot.mat),calc.out$characterestics),] %>%
            dplyr::mutate(rname = paste0(characterestics,"\t",p.res.str)) 
        }
        # 颜色设置
        {
          type.df <- wide.df %>%
            dplyr::select(sample_id,type) %>%
            dplyr::distinct(type)
          
          vc_syn <- unique(as.character(type.df$type))
          # cat("all mut type:",vc_syn,"\n")
          # 自定义颜色
          vc_cols1 = RColorBrewer::brewer.pal(n = 9, name = 'Set1')
          vc_cols2 = RColorBrewer::brewer.pal(n = 12, name = 'Set3')
          vc_cols3 = RColorBrewer::brewer.pal(n = 12, name = 'Paired')
          vc_cols <- union(union(vc_cols1,vc_cols2),vc_cols3)[1:length(vc_syn)]
          names(vc_cols) = c(vc_syn)
          # cat("all mut color:",vc_cols,"\n")
          # 构建alter_fun list
          alter_fun = list(
            background = function(x, y, w, h) {
              grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"), 
                        gp = gpar(fill = "#CCCCCC", col = NA))
            }
          )
          for(mut_name in names(vc_cols)) {
            alter_fun[[mut_name]] = local({
              mut_col = vc_cols[[mut_name]]  # define a local `mut_col`
              function(x, y, w, h) {
                grid.rect(x, y, w-unit(0.5, "mm"), h, 
                          gp = gpar(fill = mut_col, col = NA))
              }
            })
          }
          # cat("all mut color length:",length(alter_fun),"\n")
          # legend设置
          heatmap_legend_param = list(title = "Alternations", 
                                      at = vc_syn, labels = vc_syn
                                      ,direction = "horizontal",nrow = 2
                                      )
        }
        
        complexheatmap <- function(){
          
          if(data_type == 'Mutation'){
            col.list <- list(group = c('Mut'= colorg1,'WT' = colorg2))
          }else{
            col.list <- list(group = c('High'= colorg1,'Low' = colorg2))
          }
          
          # left heatmap(突变组/High组)
          onco.cli.left <- grp.df %>%
            dplyr::filter(group %in% c('Mut','High'))
          right_ano <- rowAnnotation(rbar = anno_oncoprint_barplot())
          if(nrow(onco.cli.left) > 1){
            ha.l = HeatmapAnnotation(group = factor(onco.cli.left$group),
                                     col = col.list,
                                     show_legend = c(T), # 左侧图top 注释不显示图例
                                     show_annotation_name = F,simple_anno_size_adjust = T,
                                     annotation_legend_param = list(group = list(title = "Group",
                                                                                 direction = "horizontal",
                                                                                 nrow = 2)))
            # 只有一个样本时，报：Error in : Incorrect type of 'mat'
            p.hl <- oncoPrint(as.matrix(plot.mat[,onco.cli.left$sample_id]),
                              alter_fun_is_vectorized = FALSE,
                              # get_type = function(x) gsub(":.*$", "", strsplit(x, ";")[[1]]), # 两个取一个
                              top_annotation = ha.l,
                              right_annotation = NULL, # 不显示barplot
                              # 右侧加秩和检验P值
                              alter_fun = alter_fun, 
                              pct_digits = 2,
                              # column_split = table(onco.cli$rsgroup)[1], # 切割位置,需对onco.cli按rsgroup排序
                              col = vc_cols, 
                              column_title = NULL, 
                              
                              # 行顺序及分割
                              # row_order = c(seq(1:21)), # 1
                              # row_split = gene.path$Path,
                              
                              remove_empty_columns = T,
                              show_row_names = F,
                              remove_empty_rows = F,
                              show_heatmap_legend = F, # 左侧图不显示legend
                              show_column_names = F,
                              heatmap_legend_param = heatmap_legend_param)
            
            
            right_ano <- rowAnnotation(Chitest = anno_text(calc.str$p.res.str,
                                                           location = 1,
                                                           rot = 0,
                                                           just = "right",
                                                           # show_name = TRUE,
                                                           gp = gpar(fontsize = 10)),
                                       annotation_name_side = 'top')
            
          }
          
          # right heatmap(WT组/LOW组)
          onco.cli.right <- grp.df %>%
            dplyr::filter(group %in% c('WT','Low'))
          if(nrow(onco.cli.right) > 1){
            
            ha.r = HeatmapAnnotation(group = factor(onco.cli.right$group),
                                     col = col.list,
                                     show_legend = c(T), # 左侧图top 注释不显示图例
                                     show_annotation_name = F,simple_anno_size_adjust = T, 
                                     annotation_legend_param = list(group = list(title = "Group",
                                                                                 direction = "horizontal",
                                                                                 nrow = 2)))
            p.hr <- oncoPrint(as.matrix(plot.mat[,onco.cli.right$sample_id]),
                              alter_fun_is_vectorized = FALSE,
                              # get_type = function(x) gsub(":.*$", "", strsplit(x, ";")[[1]]), # 两个取一个
                              top_annotation = ha.r,
                              right_annotation = right_ano,
                              
                              # 右侧加秩和检验P值
                              alter_fun = alter_fun, 
                              pct_digits = 2,
                              # column_split = table(onco.cli$rsgroup)[1], # 切割位置,需对onco.cli按rsgroup排序
                              col = vc_cols, 
                              column_title = NULL, 
                              
                              # 行顺序及分割
                              # row_order = c(seq(1:21)), # 1
                              # row_split = gene.path$Path,
                              
                              remove_empty_columns = T,
                              remove_empty_rows = F,
                              show_heatmap_legend = T, 
                              show_column_names = F,
                              pct_side = 'right',  # 百分比放在右边
                              row_names_side = "left",
                              heatmap_legend_param = heatmap_legend_param)
            
          }
          if(nrow(onco.cli.left) > 1){
            draw(p.hl + p.hr,
                 merge_legend = TRUE,
                 heatmap_legend_side = "bottom",
                 annotation_legend_side = "bottom")
          }else{
            draw(p.hr,
                 merge_legend = TRUE,
                 heatmap_legend_side = "bottom",
                 annotation_legend_side = "bottom")
          }
          # return(p.out)
        }
        
        update_modal_progress(
          value = i / n,
          text =  paste("Analyzing gene:", gname,sprintf("(%1.0f%%)", i/n*100)) #paste("Analyzing gene: ", gname)
        )
        
        # 输出数据整理
        out.mat <- onco.mat %>%
          # dplyr::rename(Variant_Classification = x) %>%
          dplyr::mutate(Variant_Classification = as.character(Variant_Classification)) %>%          # list 转 character
          dplyr::mutate(Variant_Classification = gsub('^[c]',"",Variant_Classification)) %>%        # 消除c
          dplyr::mutate(Variant_Classification = gsub('[(\\"\\")]',"",Variant_Classification)) %>%  # 消除括号、引号
          dplyr::mutate(Variant_Classification = gsub(',',": ",Variant_Classification))  %>%         # 替换 ,
          tidyr::spread(key=sample,value=Variant_Classification) %>%
          tibble::column_to_rownames(var = "gene") %>%
          t() %>% as.data.frame() %>%
          tibble::rownames_to_column(var = "sample_id") %>%
          dplyr::left_join(grp.df,.,by='sample_id') %>%
          tidyr::pivot_longer(cols = !c("sample_id","group"),names_to = 'Hugo_Symbol',values_to = 'Variant_Classification') %>%
          dplyr::filter(!is.na(Variant_Classification),
                        Variant_Classification != '') %>%
          dplyr::select(Sample_ID = sample_id,Hugo_Symbol,Variant_Classification,Group = group)
        
        # out.mat[1:5,]
        
        output.graphic[[paste0(gname)]]$tbl  <- out.mat
        
        output.graphic[[paste0(gname)]]$plot <- complexheatmap()
        
        output[[paste0(gname, "_plot_mut")]] <- renderPlot({
          complexheatmap()
        })
        output[[paste0(gname, "_tbl_mut")]] <- renderDataTable({
          datatable(out.mat, 
                    style = 'bootstrap', 
                    selection = list(mode = "single", target = "row"),
                    rownames = TRUE,
                    options = list(scrollX = TRUE, keys = TRUE,
                                   orderClasses = TRUE,dom = 'tp', language = list(
                      zeroRecords = "No records found matching your selection"),
                      columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                    colnames = c("Sample ID","Hugo Symbol", "Variant Classification", gname))
        })
        output[[paste0(gname, "_dl_plot_mut")]] <- downloadHandler(
          filename = function() {
            if(grepl("#",gname)) gname <- "Comb"
            paste0(prefix_output_file,"_MutationProfile_Plot_",gname,"_",Sys.Date(),".pdf", sep = "")
          },
          content = function(file) {
            pdf(file,onefile = F, pointsize = 10,width = 12)
            print(output.graphic[[paste0(gname)]]$plot)
            dev.off()
          }
        )
        output[[paste0(gname, "_dl_tbl_mut")]] <- downloadHandler(
          filename = function() {
            paste0(prefix_output_file,"_MutationProfile_Data_",gname,"_",Sys.Date(), '.txt', sep='')
          },
          content = function(file) {
            readr::write_delim(x = output.graphic[[paste0(gname)]]$tbl, path = file, delim = "\t")
          }
        )
        
        Sys.sleep(0.01)
      })
      remove_modal_progress()
  })
  
  
}
