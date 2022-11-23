#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : tab.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-04-14
# @IDE     : RStudio
# @Desc    : 第一个Tab功能的第一个子功能：生存分析之KM曲线
#===============================================================================

source("./r/func.survivalplot.R")

# 至少选择一项
# psy_js <- "
# $(document).ready(function(){
#   $('#km_biopsy_status').on('show.bs.select', function(){
#     $('a[role=option]').on('click', function(e){
#       var selections = $('#km_biopsy_status').val();
#       if(selections.length === 1 && $(this).hasClass('selected')){
#         e.stopImmediatePropagation();
#       };
#     });
#   }).on('hide.bs.select', function(){
#     $('a[role=option]').off('click');
#   });
# });"

tab_01_survival_km <- list()
cat_prefix_km <- 'KM'
sidebar <- sidebarPanel(
  # tags$head(tags$script(HTML(psy_js))),
  id = 'km_sidebar',
  width = 3,
  h3("Kaplan-Meier Curve"),
  useShinyjs(),
  div(id = "km_form",
  # 研究类型
  awesomeRadio(inputId = "km_study_type",
               label = "Select study types", 
               choices = c("Immunogenomics Studies", "Non-immunogenomics Studies"),
               selected = "Immunogenomics Studies",
               # inline = TRUE,
               status = "success"),
  # 肿瘤
  conditionalPanel('input.km_study_type != ""',
                   pickerInput(inputId = "km_cancer_detail",
                               label = "Select one cancer type, E.g. 'LUAD'",
                               choices = NULL,
                               multiple = FALSE,
                               selected = NULL)),
  # 肿瘤队列
  conditionalPanel('input.km_cancer_detail != ""',
                   pickerInput(inputId = "km_study_id",
                               label = "Select one dataset, E.g. 'Hellmann_2018'",
                               choices = NULL,
                               multiple = FALSE,
                               selected = NULL)),
  tags$hr(style="border-color: purple;"),
  # h3("Gene parameters", style = "margin-bottom: 25px;color:#00a064"),
  conditionalPanel('input.km_study_id != ""',
                   # 队列数据类型：Mutation,RNA,Protein,Methyl
                   pickerInput("km_data_type",
                               label = "Select data type, E.g. 'Mutation'", 
                               choices = NULL,
                               selected = NULL, 
                               multiple = FALSE),
                   # 队列数据的基因list
                   selectizeInput(inputId = "km_symbol_id", 
                                  label = "Seelct/Input gene symbols, E.g. 'EGFR STK11'", 
                                  choices = NULL, 
                                  width = '100%',
                                  multiple = T, 
                                  options = list(delimiter = " ", create = T,maxItems = 10,
                                                 placeholder = "No more than 10 genes.",
                                                 persist = TRUE,
                                                 plugins = list('remove_button', 'drag_drop')))),
  
  conditionalPanel('input.km_data_type == "Mutation"',
                   # 队列数据的基因突变类型
                   pickerInput(inputId = "km_vartype_id",
                               label = tags$span(
                                  add_prompt(tags$span(icon(name = "circle-question")),
                                             message = "Variant Classification for Mutation data", 
                                             position = "right"),
                                 "Select gene mutation types"),
                               choices = NULL,
                               multiple = TRUE,
                               selected = NULL),
                   awesomeRadio(
                     inputId = "km_logical",
                     label = "Mutations in all(AND)/any(OR) quried genes",
                     inline = T,
                     choices = c("AND" ,"OR"),
                     selected = "AND",
                     status = "success",
                     checkbox = TRUE
                   )),
  conditionalPanel("input.km_data_type == 'Expr' || input.km_data_type == 'Proteome'",
                   # 队列数据的基因表达阈值
                   fluidRow(
                     column(7,numericInput(inputId = "km_exprcut1_id",
                                           label = tags$span(
                                              add_prompt(tags$span(icon(name = "circle-question")),
                                                         message = "Percentile threshold for RNA data", 
                                                         position = "right"),
                                             "Percentile cutoff (%,High):"),
                                           value = 50,
                                           min = 0,
                                           max = 100,step = NA,width = '100%')),
                     column(5,numericInput(inputId = "km_exprcut2_id",
                                           label = "",
                                           value = 50,
                                           min = 0,
                                           max = 100,step = NA,width = '100%'))
                   )),

  conditionalPanel('input.km_study_type == "Immunogenomics Studies"',
                   # ici_treatment_status 免疫治疗取样时间点参数控制
                   tags$hr(style="border-color: purple;"),
                   # h3("Sample parameters", style = "margin-bottom: 25px;color:#00a064"),
                   pickerInput(inputId = "km_biopsy_status",
                               label = tags$span(
                                 add_prompt(tags$span(icon(name = "circle-question")),
                                            message = "Biopsy time of tumor sample", 
                                            position = "right"),
                                 "Biopsy time (Pre-,On- or Post-treatment):"),
                               choices = NULL,
                               multiple = TRUE,
                               selected = NULL),
                   # 免疫治疗药物类型选择
                   # tags$head(tags$script(HTML(js_icitype))),
                   pickerInput(inputId = "km_ici_type",
                               label = tags$span(
                                 add_prompt(tags$span(icon(name = "circle-question")),
                                            message = "Immune checkpoint inhibitors types", 
                                            position = "right"),
                                 "ICIs type:"),
                               choices = NULL,
                               multiple = TRUE,
                               selected = NULL)
                   ),
  tags$hr(style="border-color: purple;"),
  # h3("Graphic parameters", style = "margin-bottom: 25px;color:#00a064"),
  # 分组颜色
  fluidRow(
    column(6,colourpicker::colourInput(inputId = "km_colorg1_id", 
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
    column(1),
    column(5,colourpicker::colourInput(inputId = "km_colorg2_id", 
                                       label = tags$span(
                                          add_prompt(tags$span(icon(name = "circle-question")),
                                                     message = "Color for Low/Wt group", 
                                                     position = "right"),
                                         'Color 2'),  
                                       value = "#2196F3", 
                                       showColour = c("both", "text","background"), 
                                       palette = c("square", "limited"), 
                                       allowTransparent = FALSE, 
                                       returnName = TRUE))
    )),
  awesomeRadio(inputId = "km_tbl_loc",
               label = "Kaplan-Meier Annotation location:", 
               choices = c("Top right" = "topright", "Bottom right" = "bottomright","Bottom left" = "bottomleft"),
               selected = "topright",
               inline = TRUE,
               status = "success"),
  fluidRow(
    column(2, 
           # actionButton(inputId = "km_goButton",label = "Submit",class ="btn-primary")
           div(style="display:inline-block",actionButton(inputId = "km_goButton",label = "Submit",class ="btn-primary"), style="float:left"),
           
    ),
    column(7),
    column(2, 
           div(style="display:inline-block",actionButton("reset_input_km", "Clear",class="btn-warning"), style="float:left"),
           
    )
  )
)

mainpage <- mainPanel(
  useShinyjs(),
  id = 'km_mainpage',
  width = 9,
  uiOutput(outputId='km_maintabs')
)

tab_01_survival_km$ui <- sidebarLayout(sidebar, mainpage)

tab_01_survival_km$server <- function(input, output,session) {
  cat("========================= Start KM ==================================\n")
  
  # 定义变量存储输出对象
  output.graphic <- reactiveValues()
  
  observeEvent(input$reset_input_km, {
    shinyjs::reset("km_sidebar")
  })
  observeEvent(input$reset_input_km, {
    output$km_maintabs <- NULL
  })
  # 0.根据队列类型（ICI或非ICI队列）从数据库获取肿瘤名称名称
  km_study_df <- eventReactive(input$km_study_type,{
    study_type <- input$km_study_type
    temp.df <- study.df 
    if (study_type == 'Immunogenomics Studies'){
      temp.df <- temp.df %>%
        dplyr::filter(study_type == 'ICIs')
    }else{ 
      temp.df <- temp.df %>%
        dplyr::filter(study_type == 'nonICIs') 
    }
    temp.df
    
  })
  
  # 将获取到的内更新到页面
  observe({
    # browser()
    km_cancer_detail_list <- unique(km_study_df()$tumor_detail)
    updatePickerInput(session = session, 
                      inputId = "km_cancer_detail",
                      choices = km_cancer_detail_list, 
                      selected = km_cancer_detail_list[1])
  })
  
  # 1.根据肿瘤名称和队列类型（ICI或非ICI队列）从数据库获取队列名称
  km_study_tumor_df <- eventReactive(c(input$km_cancer_detail),{
    study_type <- input$km_study_type
    km_in_cancer_detail <- input$km_cancer_detail
    if(is.null(km_in_cancer_detail)) km_in_cancer_detail <- unique(km_study_df()$tumor_detail)[1]
    temp.study.df <- km_study_df() %>%
      dplyr::filter(tumor_detail == km_in_cancer_detail)
    temp.study.df
  })
  # 将获取到的肿瘤队列list更新到页面
  observe({
    # browser()
    km_study_id_list <- unique(km_study_tumor_df()$study_id)
    updatePickerInput(session = session, 
                      inputId = "km_study_id",
                      choices = km_study_id_list, 
                      selected = km_study_id_list[1])
  })
  # 2. 获取数据类型
  observeEvent(c(input$km_study_id),{
    temp_id <- input$km_study_type 
    km_in_study_type <- case_when(temp_id == 'Immunogenomics Studies' ~ 'ICIs',
                               TRUE  ~ 'nonICIs')
    
    km_in_cancer_detail <- input$km_cancer_detail 
    km_in_study_id <- input$km_study_id
    cat("KM-前端返回的队列名称: ",km_in_study_id,"\n")
    
    # 本次过滤后应只有一条记录
    temp.df <- km_study_tumor_df() %>%
      # dplyr::filter(study_type %in% km_in_study_type) %>%
      # dplyr::filter(tumor_detail %in% km_in_cancer_detail) %>%
      dplyr::filter(study_id %in% km_in_study_id) 
    # browser()
    data.types <- unique(unlist(str_split(temp.df$data_type[1],"/")))
    # 更新数据类型
    updatePickerInput(session = session, inputId = "km_data_type",
                      choices = data.types, 
                      selected = data.types[1])
    
    # 更新活检时间状态
    biopsy.status <- unique(unlist(str_split(temp.df$ici_study_biopsy[1],"#")))
    updatePickerInput(session = session,
                         inputId = "km_biopsy_status",
                         choices = biopsy.status,
                         selected = biopsy.status)
    
    
    # 更新免疫治疗药物类型
    biopsy.status <- unique(unlist(str_split(temp.df$ici_type[1],",")))
    updatePickerInput(session = session,
                      inputId = "km_ici_type",
                      choices = biopsy.status,
                      selected = biopsy.status)
    
    db.tmp.dat <- data.frame()
    if(data.types[1] == 'Mutation'){
      tbl.name <- paste0(tolower(km_in_study_id),'_mutation')
      db.dat.dna <- queryDataFromMySQL(tbl.name)
      db.tmp.dat <- db.dat.dna
    }
    if(data.types[1] == 'Expr'){
      tbl.name <- paste0(tolower(km_in_study_id),'_expr')
      db.dat.rna <- queryDataFromMySQL(tbl.name)
      db.dat.rna$variant_classification <- ''
      db.tmp.dat <- db.dat.rna
    }
    if(data.types[1] == 'Proteome'){
      tbl.name <- paste0(tolower(km_in_study_id),'_proteome')
      db.dat.rna <- queryDataFromMySQL(tbl.name)
      db.dat.rna$variant_classification <- ''
      db.tmp.dat <- db.dat.rna
    }
    # 更新基因名
    updateSelectizeInput(session = session,
                         inputId = "km_symbol_id",
                         choices = unique(db.tmp.dat$hugo_symbol),
                         selected = NULL,
                         server = TRUE)
    
    # 更新突变类型
    updatePickerInput(session = session, inputId = "km_vartype_id",
                      choices = unique(db.tmp.dat$variant_classification), 
                      selected = unique(db.tmp.dat$variant_classification),
                      options = list(
                        `actions-box` = TRUE))  
    
  })
  
  # 3. 根据队列名称更新基因和突变类型
  observeEvent(c(input$km_data_type),{
    
    km_in_study_id <- input$km_study_id
    
    data_type = input$km_data_type
    cat("KM-前端返回的数据类型: ",data_type,"\n")
    
    db.tmp.dat <- data.frame()
    if(data_type == 'Mutation'){
      tbl.name <- paste0(tolower(km_in_study_id),'_mutation')
      db.dat.dna <- queryDataFromMySQL(tbl.name)
      db.tmp.dat <- db.dat.dna
    }
    if(data_type == 'Expr'){
      tbl.name <- paste0(tolower(km_in_study_id),'_expr')
      db.dat.rna <- queryDataFromMySQL(tbl.name)
      db.dat.rna$variant_classification <- ''
      db.tmp.dat <- db.dat.rna
    }
    if(data_type == 'Proteome'){
      tbl.name <- paste0(tolower(km_in_study_id),'_proteome')
      db.dat.rna <- queryDataFromMySQL(tbl.name)
      db.dat.rna$variant_classification <- ''
      db.tmp.dat <- db.dat.rna
    }
    # return(db.tmp.dat)
    # 更新基因名
    updateSelectizeInput(session = session,
                         inputId = "km_symbol_id",
                         choices = unique(db.tmp.dat$hugo_symbol),
                         selected = NULL,
                         server = TRUE)
    
    # 更新突变类型
    updatePickerInput(session = session, inputId = "km_vartype_id",
                      choices = unique(db.tmp.dat$variant_classification), 
                      selected = unique(db.tmp.dat$variant_classification),
                      options = list(
                        `actions-box` = TRUE))   
  })
  
  
  # 4. 基因表达分组cutoff更新联动
  observe({
    number1 <- input$km_exprcut1_id  # value 1
    updateNumericInput(session, inputId = "km_exprcut2_id",
                       label = "Cutoff (%,Low):",
                       value = 100-number1,
                       min = 0, max = 100, step = NA)
  })
  # 若需要cutoff1与cutoff2联动，则取消以下注释；本处为方便设置不同的cutoff，因此不对cutoff1联动
  # observe({
  #   number2 <- input$km_exprcut2_id  # value 1
  #   updateNumericInput(session, inputId = "km_exprcut1_id",
  #                      label = "Cutoff (High):",
  #                      value = 100-number2,
  #                      min = 0, max = 100, step = NA)
  # })  
  
  
  observe({
    shinyjs::toggleState("km_goButton", 
                         !is.null(input$km_symbol_id) && input$km_symbol_id != "" )
  })
  
  # 从DB获取数据
  # 临床信息
  db.dat.cli <- eventReactive(input$km_study_id,{
    cat("查询Clinical，待分析的研究队列: ",input$km_study_id,"\n")
    study <- input$km_study_id
    cancer_id <- input$km_cancer_detail # cancer_detail
    temp.df <- queryDataFromMySQL(paste0(tolower(study),'_clinical'))
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
  # 突变或者表达谱
  # cat("查询Mutation/Expr，待分析的研究队列: ",study(),"\n")
  # tbl_name <- case_when(data_type() == 'DNA' ~ paste0(tolower(study()),'_mutation'),
  #                       data_type() == 'RNA' ~ paste0(tolower(study()),'_expr'),
  #                       TRUE ~ '')
  # db.dat <- queryDataFromMySQL(tbl_name)

  db.dat <- eventReactive(c(input$km_study_id,
                            input$km_data_type),{
    cat("查询Mutation/Expr，待分析的研究队列: ",input$km_study_id,"\n")
    data_type <- input$km_data_type
    study <- input$km_study_id
    tbl_name <- case_when(data_type == 'Mutation' ~ paste0(tolower(study),'_mutation'),
                          data_type == 'Expr' ~ paste0(tolower(study),'_expr'),
                          data_type == 'Proteome' ~ paste0(tolower(study),'_proteome'),
                          TRUE ~ '')
    temp.df <- queryDataFromMySQL(tbl_name)
    return(temp.df)
  })
  
  # 输入为空验证
  iv_km <- InputValidator$new()
  # iv_km$add_rule("km_cancer_detail", sv_required())
  # iv_km$add_rule("km_study_id", sv_required())
  # iv_km$add_rule("km_data_type", sv_required())
  iv_km$add_rule("km_symbol_id", sv_required())
  iv_km$add_rule("km_vartype_id", sv_required())
  iv_km$add_rule("km_biopsy_status", sv_required())
  iv_km$add_rule("km_ici_type", sv_required())
  iv_km$enable()

  # 业务层：KM分析
  observeEvent(input$km_goButton,{
    cat("=========================== Server KM ==============================\n")
    show_modal_progress_line(
      color = "#00A064", # DF0101
      trail_color = "#eee",
      duration = 90,
      easing = "easeOut",
      text = "Starting survival analysis ..."
    )
    study_type_in <- input$km_study_type
    cat("选择的研究队列类型: ", study_type_in, "\n")
    if (study_type_in == 'Immunogenomics Studies'){
      study_type_in <- 'ICIs'
    }else{
      study_type_in = 'nonICIs'
    }
      
    cancer_detail_in <- input$km_cancer_detail
    cat("选择的研究队列瘤种: ", cancer_detail_in, "\n")
    study_id_in <- input$km_study_id
    cat("选择的研究队列: ", study_id_in, "\n")
    
    
    data_type <- input$km_data_type
    cat("选择的队列数据类型: ", data_type, "\n")
    
    # validate(need(try(input$km_symbol_id),"Please select at least one gene!"))
    symbol <- input$km_symbol_id
    cat("选择的基因名称: ", symbol, "\n")
    
    vartype <- input$km_vartype_id
    cat("选择的基因突变类型: ", vartype, "\n")
    
    # 活检取样时间点
    biopsy_time <- input$km_biopsy_status
    # 免疫治疗药物类型
    ici_type <- input$km_ici_type
    
    exprcut1 <- input$km_exprcut1_id
    exprcut2 <- input$km_exprcut2_id
    cat("选择的基因高表达分位数: ", exprcut1, "\n")
    cat("选择的基因低表达分位数: ", exprcut2, "\n")
    
    colorg1 <- input$km_colorg1_id
    colorg2 <- input$km_colorg2_id
    cat("选择的样本分组1颜色: ", colorg1, "\n")
    cat("选择的样本分组2颜色: ", colorg2, "\n")
    
    input.tbl.loc <- input$km_tbl_loc
    
    km_logical_type <- input$km_logical
    cat(cat_prefix_km,"-选择的多基因突变逻辑关系: ", km_logical_type, "\n")
    
    # 数据清理、绘图、输出
    input.genes <- symbol 
    if(data_type == 'Mutation'){
      # input.genes <- c(input.genes,"All Queried Genes")
      if(length(input.genes) >1){
        input.genes <- c(input.genes,"All Queried Genes")
      }
    }
    input.vartype <- vartype
    input.exprcut1 <- exprcut1/100  #转化为分位数函数的输入值
    input.exprcut2 <- exprcut2/100
    input.color <- c(colorg2,colorg1)
    
    # 写出到页面
    output$km_maintabs <- renderUI({
      tabs <- lapply(input.genes, function(name) {
        tabPanel(
          title = name,
          uiOutput(paste0(name,"_km"))
        )
      })
      do.call(tabsetPanel, tabs)
    })
    # 执行数据分析
    # withProgress(message = 'Making plot', value = 0, {
      n <- length(input.genes)
      lapply(seq(1,n), function(i) {
        gname <- input.genes[i]
        symbol.name <- gname
        if(gname == 'All Queried Genes') {symbol.name <- setdiff(input.genes,gname)} # 从EGFR,TP53,ALK,All Queried Genes中剔除掉“All Queried Genes”
        
        update_modal_progress(
          value = i / n,
          text =  paste("Analyzing gene:", gname,sprintf("(%1.0f%%)", i/n*100)) #paste("Analyzing gene: ", gname)
        )
        
        # 取出时间字段,只分析有终点的
        # survival.colnames <- colnames(db.dat.cli())[grepl("months|status",colnames(db.dat.cli()))]
        
        # 本次过滤后应只有一条记录
        temp.outcome.df <- study.df %>%
          dplyr::filter(study_type %in% study_type_in) %>%
          dplyr::filter(tumor_detail %in% cancer_detail_in) %>%
          dplyr::filter(study_id %in% study_id_in )
        # PFS/OS等生存终点
        cli.tab.names <- toupper(unlist(strsplit(temp.outcome.df$outcome[1],split = "#",fixed = F)))
        cli.tab.names <- cli.tab.names[which(cli.tab.names != '')] # os,pfs,...
        
        selected.colnames <- c(paste0(tolower(cli.tab.names),"_months"),paste0(tolower(cli.tab.names),"_status"))  # os_months,os_status,pfs_months,...
        
        # 免疫治疗相应终点: orr 和 clinical_benefit
        if(study_type_in == 'ICIs'){
          cli.resp.names <- intersect(c('orr','clinical_benefit'),unlist(strsplit(temp.outcome.df$clinical_property[1],split = "#",fixed = F)))
          if(length(cli.resp.names) > 0){
            cli.tab.names <- c(cli.tab.names,toupper(cli.resp.names)) # OS,PFS,...,ORR,CLINICAL_BENEFIT
            selected.colnames <- c(selected.colnames,cli.resp.names) # os_months,os_status,pfs_months,...,orr,clinical_benefit
            }
        }
        
        clc.cli.km <- db.dat.cli()
        # 免疫治疗队列过滤样本
        if(study_type_in == 'ICIs'){
          if(!is.null(biopsy_time)) {
            clc.cli.km <- clc.cli.km %>%
              dplyr::filter(ici_treatment_status %in% biopsy_time)
          }
          if(!is.null(ici_type)) {
            clc.cli.km <- clc.cli.km %>%
              dplyr::filter(str_detect(ici_treatment_type, paste0(ici_type,collapse = "|")))
          }
        }
        
        # for mutation
        if(data_type == 'Mutation'){
          # 临床样本与突变样本取交集
          tumor.samples <- intersect(db.dat()$sample_id,db.dat.cli()$sample_id)
          
          if(gname == 'All Queried Genes'){
            # 多个基因时
            # browser()
            if(km_logical_type == 'AND'){
              # logical 为 AND 时
              cat(cat_prefix_km,"- 逻辑AND选择多基因突变模式！\n")
              # browser()
              tmp.mut.samples <- db.dat() %>%
                dplyr::filter(hugo_symbol %in% symbol.name,
                              variant_classification %in% input.vartype) %>%
                dplyr::select(sample_id,hugo_symbol) %>%
                dplyr::distinct(sample_id,hugo_symbol,.keep_all = T) %>%
                dplyr::group_by(sample_id) %>%
                dplyr::summarise(sample_count = n()) %>%
                dplyr::ungroup() %>%
                dplyr::filter(sample_count == length(symbol.name)) # 取
            }
            if(km_logical_type == 'OR'){
              # logical 为 OR 时
              cat(cat_prefix_km,"- 逻辑OR选择多基因突变模式！\n")
              tmp.mut.samples <- db.dat() %>%
                dplyr::filter(hugo_symbol %in% symbol.name,
                              variant_classification %in% input.vartype)  %>%
                dplyr::distinct(sample_id,hugo_symbol,.keep_all = T) %>%
                dplyr::group_by(sample_id) %>%
                dplyr::summarise(sample_count = n()) %>%
                dplyr::ungroup() %>%
                dplyr::filter(sample_count >= 1) # 取
            }
            
            grp1.df <- db.dat() %>%
              dplyr::filter(hugo_symbol %in% symbol.name,
                            variant_classification %in% input.vartype) %>%
              dplyr::filter(sample_id %in% tmp.mut.samples$sample_id) %>%
              dplyr::select(sample_id,hugo_symbol)
            
            
          }else{
            # 单个基因时
            grp1.df <- db.dat() %>%
              dplyr::filter(hugo_symbol %in% symbol.name,
                            variant_classification %in% input.vartype) %>%
              dplyr::select(sample_id,hugo_symbol)
            
          }
          
          clc.cli <- clc.cli.km %>%
            dplyr::filter(sample_id %in% tumor.samples) %>%
            dplyr::mutate(Group = factor(ifelse(sample_id %in% unique(grp1.df$sample_id),'Mut','Wt'),levels = c('Wt','Mut'))) %>%
            dplyr::select(sample_id,risk = Group,all_of(selected.colnames))
        }
        # for expr
        if(data_type %in% c('Expr','Proteome')){
          # 临床样本与表达谱样本取交集
          tumor.samples <- intersect(colnames(db.dat()),db.dat.cli()$sample_id)
          # symbol.name必须为单个基因的名称
          grp1.df <- db.dat() %>%
            dplyr::select(hugo_symbol,tumor.samples) %>%
            dplyr::filter(hugo_symbol %in% symbol.name) %>%
            tibble::column_to_rownames(var = 'hugo_symbol') %>%
            t() %>% as.data.frame() %>%
            tibble::rownames_to_column(var = 'sample_id')
          colnames(grp1.df)[2] <- "sltgene"
          
          cutoff_h <- quantile(as.numeric(grp1.df$sltgene),input.exprcut1,na.rm = T)
          cutoff_l <- quantile(as.numeric(grp1.df$sltgene),input.exprcut2,na.rm = T)
          
          quantile.25 <- quantile(as.numeric(grp1.df$sltgene),0.25,na.rm = T)
          quantile.75 <- quantile(as.numeric(grp1.df$sltgene),0.75,na.rm = T)
          if(quantile.25 == quantile.75){
            cat("基因：",symbol.name,"的第一分位数和第三分为数相等,无法以75%以内分位值为阈值。系统将以第一个不同的表达值为阈值分组\n")
            cutoff_h <- unique(sort(as.numeric(grp1.df$sltgene)))[2]
            cutoff_l <- unique(sort(as.numeric(grp1.df$sltgene)))[2]
          }
          cat("Cutoff of Expression: ",cutoff_h," (High); ",cutoff_l," (Low)\n")
          
          grp1.df <- grp1.df %>%
            dplyr::mutate(sltgene = as.numeric(sltgene)) %>%
            dplyr::mutate(Group = ifelse(sltgene >= cutoff_h,"High",
                                         ifelse(sltgene <= cutoff_l,'Low','Middle'))) %>%
            dplyr::filter(Group %in% c('Low','High')) %>%
            dplyr::select(sample_id,Group)
          grp1.df$Group <- factor(grp1.df$Group,levels = c('Low','High'))
          clc.cli <- clc.cli.km %>%
            dplyr::filter(sample_id %in% tumor.samples) %>%
            dplyr::inner_join(grp1.df,by='sample_id')  %>%
            dplyr::select(sample_id,risk = Group,all_of(selected.colnames))
        }
        
        # 绘图: KM curve and stackplot
        km.plot.list <- list()
        km.tabl.list <- list()
        for (cli.name in cli.tab.names) {
          
          # cat("====>nrow(clc.cli)",nrow(clc.cli),"\n")
          # browser()
          
          # 生存分析
          if(!is_in(tolower(cli.name),c('orr','clinical_benefit'))){
            km.df <- clc.cli %>%
              # dplyr::mutate(row_id = seq(1,nrow(clc.cli))) %>%
              dplyr::select(sample_id,
                            times = paste0(tolower(cli.name),"_months"),
                            status = paste0(tolower(cli.name),"_status"),
                            risk)%>%
              dplyr::mutate(times = round(as.numeric(times),2)) %>%
              dplyr::filter(!is.na(times),status != '',!is.na(status))
            
            # output.graphic[[paste0(gname,"_",tolower(cli.name))]]$tbl <- km.df
            
            if(length(unique(km.df$risk)) == 1){
              cat("基因 ",gname," 分组只有一组\n")
              km.graphic <- ggsurvplot(survfit(Surv(times, status) ~ risk, 
                                               data = km.df), 
                                       data = km.df,
                                       title = cli.name,
                                       risk.table = T,
                                       risk.table.pos = "out",
                                       risk.table.y.text.col = T,
                                       risk.table.y.text = TRUE,
                                       ncensor.plot = F,
                                       xlim = c(0, max(km.df$times)*1.1),
                                       legend = "none",  # 取消图例，下面legend参数无效
                                       legend.title = '',
                                       color.dis = input.color[1],
                                       break.time.by = time_break_survival(max(km.df$times,na.rm = T)))
              
              km.plotly <- km.graphic
            }else{
              km.graphic <- func.survivalplot(plotdata = km.df,
                                              kw.title = cli.name,
                                              time.break = time_break_survival(max(km.df$times,na.rm = T)),
                                              key.stye = cli.name,
                                              tbl.loc = input.tbl.loc, # km_tbl_loc
                                              color.dis = input.color,
                                              y.title = case_when(cli.name == 'PFS' ~ 'Progression-free Survival',
                                                                  cli.name == 'DFS' ~ 'Disease-free Survival',
                                                                  cli.name == 'PFI' ~ 'Progression-free Interval',
                                                                  cli.name == 'DFI' ~ 'Disease-free Interal',
                                                                  cli.name == 'DSS' ~ 'Disease-specific Survival',
                                                                  cli.name == 'OS' ~ 'Overall Survival',
                                                                  TRUE ~ 'Survival'))
              km.plotly <-  plotly_survival(km.df,'',
                                            time.break = time_break_survival(max(km.df$times,na.rm = T)),
                                            color.dis = input.color,
                                            y.title = case_when(cli.name == 'PFS' ~ 'Progression-free Survival',
                                                                cli.name == 'DFS' ~ 'Disease-free Survival',
                                                                cli.name == 'PFI' ~ 'Progression-free Interval',
                                                                cli.name == 'DFI' ~ 'Disease-free Interal',
                                                                cli.name == 'DSS' ~ 'Disease-specific Survival',
                                                                cli.name == 'OS' ~ 'Overall Survival',
                                                                TRUE ~ 'Survival'))
              
              
            }
            # 执行完绘图， 更新数据列名为当前分析因素
            colnames(km.df) <- c("sample_id", cli.name, paste0(cli.name,".event"), gname)
            km.graphic <- plot_grid(km.graphic$plot + 
                                      theme(plot.title = element_text(hjust = 0,face = "bold")),
                                    km.graphic$table,nrow = 2,rel_heights = c(1,0.3))
          }
          # 响应分析
          if(tolower(cli.name) %in% c('orr','clinical_benefit')){
            if(tolower(cli.name) == 'orr'){
              km.df <- clc.cli %>%
                dplyr::select(sample_id,
                              resp = tolower(cli.name),
                              risk)%>%
                dplyr::filter(resp %in% c('PD','SD','PR','CR')) %>%
                dplyr::mutate(resp =factor(resp,levels = c('PD','SD','PR','CR'))) 
              legendTitle <- "ORR"
            }
            if(tolower(cli.name) == 'clinical_benefit'){
              km.df <- clc.cli %>%
                dplyr::select(sample_id,
                              resp = tolower(cli.name),
                              risk)%>%
                dplyr::filter(resp %in% c('NDB','DCB'))  %>%
                dplyr::mutate(resp =factor(resp,levels = c('NDB','DCB')))
              legendTitle <- "Clinical\nBenefit"
            }
            
            # browser()
            # if(length(unique(km.df$risk)) == 1){
            #   cat("基因 ",gname," 分组只有一组\n")
            #   km.graphic <- ggbarstats(
            #     data = km.df,
            #     x = resp,
            #     y = risk,
            #     type = 'nonparametric',
            #     label = 'counts',
            #     proportion.test = NULL,
            #     title = cli.name,
            #     legend.title = legendTitle,
            #     xlab = ''
            #   )
            #     
            # }else{
              # 关于结果值的解释
              # https://yuzar-blog.netlify.app/posts/2021-12-14-how-to-conduct-chi-square-test-in-r/
            #   km.graphic <- ggbarstats(
            #     data = km.df,
            #     x = resp,
            #     y = risk,
            #     type = 'nonparametric',
            #     label = 'counts',
            #     proportion.test = NULL,
            #     title = cli.name,
            #     legend.title = legendTitle,
            #     xlab = ''
            #   ) 
            # }
            # 调整legend的titile名称
            # km.graphic <- km.graphic +
            #   theme(axis.text = element_text(face = 'bold',size = 12))
            # km.graphic <- as.ggplot(km.graphic)
            
            km.plot.xtbl <- table(km.df$risk,km.df$resp)
            km.plot.pvalue <- chisq.test(km.plot.xtbl)$p.value
            
            km.chiq.df <- km.df %>%
              dplyr::group_by(xname = risk,yname = resp) %>%
              dplyr::summarise(countvalue = n())
            
            km.graphic <- func.stackplot(km.chiq.df,
                                         title = cli.name,
                                         legendTitle = legendTitle,
                                         p.value = km.plot.pvalue,
                                         pvalue.xloc = 1.3,
                                         color.sets = sample(brewer.pal(n = 12, name = "Set3"),
                                                               length(unique(km.df$resp))))
            
            # 执行完绘图， 更新数据列名为当前分析因素
            colnames(km.df) <- c("sample_id", cli.name, gname)
            
          }
          
          output.graphic[[paste0(gname,"_",tolower(cli.name))]]$tbl <- km.df
          output.graphic[[paste0(gname,"_",tolower(cli.name))]]$kmplot <- km.graphic
          # browser()
          km.plot.list[[cli.name]] <- km.graphic
          km.tabl.list[[cli.name]] <- km.df
          
          # output[[paste0(gname, "_plot_km_",tolower(cli.name))]] <- renderPlotly({
          #   ggplotly(km.plotly[[1]])
          # })
          
          # output[[paste0(gname, "_plot_km_",tolower(cli.name))]] <- renderPlot({
          #   print(km.graphic)
          # })
          # output[[paste0(gname, "_tbl_km_",tolower(cli.name))]] <- renderDataTable({
          #   datatable(km.df, 
          #             style = 'bootstrap', 
          #             rownames = FALSE, 
          #             selection = list(mode = "single", target = "row"),
          #             options = list(orderClasses = TRUE,keys = TRUE,dom = 'Bfrtip', searching = F,
          #                            language = list(zeroRecords = "No records found matching your selection"),
          #                            columnDefs = list(list(className = 'dt-center', targets = '_all')))
          #             )
          # })
          # output[[paste0(gname, "_dlplotkm_",tolower(cli.name))]] <- downloadHandler(
          #   filename = function() {
          #     if(grepl("#",gname)) gname <- "Comb"
          #     paste0(prefix_output_file,"_KM_Plot_",toupper(cli.name),"_",gname,"_",Sys.Date(),".pdf", sep = "")
          #   },
          #   content = function(file) {
          #     pdf(file,onefile = F, width = 6, height = 4, pointsize = 10)
          #     print(output.graphic[[paste0(gname,"_",tolower(cli.name))]]$kmplot)
          #     dev.off()
          #   }
          # )
          # output[[paste0(gname, "_dltblkm_",tolower(cli.name))]] <- downloadHandler(
          #   filename = function() {
          #     paste0(prefix_output_file,"_KM_Data_",toupper(cli.name),"_",gname,"_",Sys.Date(), '.txt', sep='')
          #   },
          #   content = function(file) {
          #     readr::write_delim(x = output.graphic[[paste0(gname,"_",tolower(cli.name))]]$tbl, path = file, delim = "\t")
          #   }
          # )
        # })
        }
        # 整合当前输出图表
        {
          # browser()
          # 图
          survival.cli.names <- intersect(toupper(names(km.plot.list)),c("OS","PFS","PFI","DFI",'DFS','DSS'))
          survival.cli.names.length = length(survival.cli.names) # 不会超过4
          
          n.col <- min(5,length(km.plot.list))
          n.row <- max(1,ceiling(length(km.plot.list)/n.col))
          o.widths <- 600*survival.cli.names.length + 400 * (n.col-survival.cli.names.length)
          out.plot <- ggarrange(plotlist = km.plot.list,
                             ncol = n.col,
                             nrow = n.row,
                             # labels = names(km.plot.list),
                             widths = o.widths,
                             heights = 400 * n.row )
          output.width <- o.widths
          output.height <- 400 * n.row
          
          # 表
          tabl.names <- names(km.tabl.list)
          out.tabl <- km.tabl.list[[tabl.names[1]]]
          if(length(tabl.names)>1){
            for (i in c(2:length(km.tabl.list))) {
              tmp.tabl<- km.tabl.list[[tabl.names[i]]]
              out.tabl <- out.tabl  %>%
                dplyr::full_join(tmp.tabl %>%
                                   dplyr::select(-gname),by='sample_id') %>%
                dplyr::select(sample_id,gname,everything())
            }
          }
          
          output.graphic[[paste0(gname,"_km_tabl")]]$tbl <- out.tabl
          output.graphic[[paste0(gname,"_km_plot")]]$kmplot <- out.plot
      }
      {
        output[[paste0(gname,"_km")]] <- renderUI({
          tagList(
            shinycssloaders::withSpinner(plotOutput(paste0(gname, "_km_plot"),height = 400 * n.row,width = 'auto')),
            h5(HTML(paste("<span style=color:black;font-size:12px;>", "All Queried Genes: mutation of at least one of query genes. 
                          Mut: mutation, Wt: wild type.\n Hazard ratio (HR) for mutant (Mut) versus wild-type (Wt)  was calculated by Cox regression.", "</span>"))),
            downloadButton(paste0(gname, "_km_plot_dl"),
                           tags$span(
                             "DLGraph",
                             add_prompt(tags$span(icon(name = "circle-question")),
                                        message = "Save plot in a pdf file.",
                                        position = "left")),
                           style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
            downloadButton(paste0(gname, "_km_tabl_dl"),
                           tags$span(
                             "DLTable",
                             add_prompt(tags$span(icon(name = "circle-question")),
                                        message = "Save data of plot in a txt file.",
                                        position = "left")),
                           style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),  
            br(),
            DT::dataTableOutput(paste0(gname, "_km_tabl"))
          )
          
        })
        
          # 图输出到页面
          output[[paste0(gname,"_km_plot")]] <- renderPlot({
            print(out.plot)
          })
          
          # 表输出到页面
          output[[paste0(gname,"_km_tabl")]] <- renderDataTable({
            datatable(out.tabl, 
                      style = 'bootstrap', 
                      rownames = FALSE, 
                      selection = list(mode = "single", target = "row"),
                      options = list(orderClasses = TRUE,keys = TRUE,dom = 'Bfrtip', searching = F,
                                     language = list(zeroRecords = "No records found matching your selection"),
                                     columnDefs = list(list(className = 'dt-center', targets = '_all'))),
                      colnames = c("Sample ID", colnames(out.tabl)[-1])
            )
          })
          
          # 下载图
          output[[paste0(gname, "_km_plot_dl")]] <- downloadHandler(
            filename = function() {
              if(grepl("#",gname)) gname <- "Comb"
              paste0(prefix_output_file,"_KM_Plot_",gname,"_",Sys.Date(),".pdf", sep = "")
            },
            content = function(file) {
              pdf(file,onefile = F, width = (output.width/100)*0.8, height = output.height/100, pointsize = 10)
              print(output.graphic[[paste0(gname,"_km_plot")]]$kmplot)
              dev.off()
            }
          )
          # 下载表
          output[[paste0(gname, "_km_tabl_dl")]] <- downloadHandler(
            filename = function() {
              paste0(prefix_output_file,"_KM_Data_",gname,"_",Sys.Date(), '.txt', sep='')
            },
            content = function(file) {
              readr::write_delim(x = output.graphic[[paste0(gname,"_km_tabl")]]$tbl, path = file, delim = "\t")
            }
          )
        }
        
        
        Sys.sleep(0.01)
        
      })
      remove_modal_progress()
  })
  
  
}
