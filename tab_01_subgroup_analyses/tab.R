#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : tab.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-04-14
# @IDE     : RStudio
# @Desc    : 第一个Tab功能的第3个子功能：亚组分析+p.interaction
#===============================================================================

source("./r/func.factorization.R")

tab_01_subgroup_analyses <- list()
sidebar <- sidebarPanel(
  id = 'subgroup_sidebar',
  width = 3,
  h3("Subgroup Analyses"),
  awesomeRadio(inputId = "subgroup_study_type",
               label = "Select study types", 
               choices = c("Immunogenomics Studies", "Non-immunogenomics Studies"),
               selected = "Immunogenomics Studies",
               # inline = TRUE,
               status = "success"),
  conditionalPanel('input.subgroup_study_type != ""',
                   pickerInput(inputId = "subgroup_cancer_detail",
                               label = "Select one cancer type, E.g. 'LUAD'",
                               choices = NULL,
                               multiple = FALSE,
                               selected = NULL)),
  conditionalPanel('input.subgroup_cancer_detail != ""',
                   pickerInput(inputId = "subgroup_study_id",
                               label = "Select cne dataset, E.g. 'Hellmann_2018'",
                               choices = NULL,
                               multiple = FALSE,
                               selected = NULL)),
  conditionalPanel('input.subgroup_study_id != ""',
                   # 队列数据类型：Mutation,RNA,Protein,Methyl
                   pickerInput("subgroup_data_type",
                               label = "Select data type, E.g. 'Mutation'", 
                               choices = NULL,
                               selected = NULL, 
                               multiple = FALSE),
                   selectizeInput(inputId = "subgroup_symbol_id", 
                                  label = "Select/Input gene symbols, E.g. 'EGFR STK11'", 
                                  choices = NULL, 
                                  width = '100%',
                                  multiple = T, 
                                  options = list(delimiter = " ", create = T,
                                                 maxItems = 10,placeholder = "No more than 10 genes.",
                                                 plugins = list('remove_button', 'drag_drop'))),
                   # 选择基因还是临床信息
                   awesomeRadio(inputId = "subgroup_yaxis_factor",
                                label = "Pls Select Interaction Factor Type:", 
                                choices = c("Clinicopathologicals", "Gene Symbols"),
                                selected = "Clinicopathologicals",
                                # inline = TRUE,
                                status = "success"),
                   conditionalPanel('input.subgroup_yaxis_factor == "Clinicopathologicals"',
                                    # 用于亚组分析的临床属性
                                    pickerInput(inputId = "subgroup_factor_id",
                                                label = tags$span(
                                                  add_prompt(tags$span(icon(name = "question-circle")),
                                                             message = "Select clinicopathologic characteristics used to Subgroup analyses!", 
                                                         position = "right"),
                                                  "Select one or multiple clinical characteristics"),
                                                choices = NULL,
                                                multiple = TRUE,
                                                selected = NULL)),
                   conditionalPanel('input.subgroup_yaxis_factor == "Gene Symbols"',
                                    # 用与亚组分析的基因
                                    # pickerInput(inputId = "subgroup_factor_gene_id",
                                    #             label = tags$span(
                                    #               add_prompt(tags$span(icon(name = "question-circle")),
                                    #                          message = "Select genes used to Subgroup analyses!", 
                                    #                      position = "right"),
                                    #               "Pls Enter Interaction Genes:"),
                                    #             choices = NULL,
                                    #             options = list(`actions-box` = T, `none-selected-text` = "No more then 20 genes!"),
                                    #             multiple = TRUE,
                                    #             selected = NULL),
                                    selectizeInput(inputId = "subgroup_factor_gene_id", 
                                                   label = tags$span(
                                                     add_prompt(tags$span(icon(name = "question-circle")),
                                                                message = "Select genes used to Subgroup analyses!", 
                                                                position = "right"),
                                                     "Select/Input query gene symbols"), 
                                                   choices = NULL, 
                                                   width = '100%',
                                                   multiple = T, 
                                                   options = list(delimiter = " ", create = T,
                                                                  maxItems = 20,placeholder = "No more then 20 genes!",
                                                                  plugins = list('remove_button', 'drag_drop')))
                                    ),
  ),
  conditionalPanel('input.subgroup_data_type == "Mutation"',
                   pickerInput(inputId = "subgroup_vartype_id",
                               label = tags$span(
                                  add_prompt(tags$span(icon(name = "question-circle")),
                                             message = "Variant Classification for Mutation data", 
                                             position = "right"),
                                 "Select gene mutation types"),
                               choices = NULL,
                               multiple = TRUE,
                               selected = NULL)),
  conditionalPanel("input.subgroup_data_type == 'Expr' || input.subgroup_data_type == 'Proteome'",
                   fluidRow(
                     column(6,numericInput(inputId = "subgroup_exprcut1_id",
                                           label = tags$span(
                                              add_prompt(tags$span(icon(name = "question-circle")),
                                                         message = "Percentile threshold for High/Low group", 
                                                         position = "right"),
                                             "Percentile cutoff (%,High):"),
                                           value = 50,
                                           min = 0,
                                           max = 100,step = NA,width = '100%')),
                     column(5,numericInput(inputId = "subgroup_exprcut2_id",
                                           label = "",
                                           value = 50,
                                           min = 0,
                                           max = 100,step = NA,width = '100%'))
                   )
  ),
  # h5("Forest plot will be displayed when the number of mutant patients more than 5."),
  h5(HTML(paste("<span style=color:red;font-size:10px;>", "Tips: Forest plot will be displayed when the mutant patients more than 5.", "</span>"))),
  # 提交按钮
  actionButton(inputId = "subgroup_goButton",
               label = "Submit",
               class ="btn-primary"
  )
  
)

mainpage <- mainPanel(
  id = 'subgroup_mainpage',
  width = 9,
  uiOutput(outputId='subgroup_maintabs')
)

tab_01_subgroup_analyses$ui <- sidebarLayout(sidebar, mainpage)

tab_01_subgroup_analyses$server <- function(input, output,session) {
  cat("=========================== Start Cox================================\n")
  
  # 定义变量存储输出对象
  output.graphic <- reactiveValues()
  # 0.g根据队列类型（ICI或非ICI队列）从数据库获取肿瘤名称名称
  tumor.names.df <- eventReactive(c(input$subgroup_study_type),{
    study_type <- input$subgroup_study_type
    temp.df <- study.df 
    if (study_type == 'Immunogenomics Studies'){
      temp.df <- temp.df %>%
        dplyr::filter(study_type == 'ICIs')
    }else{
      temp.df <- temp.df %>%
        dplyr::filter(study_type == 'nonICIs') #%>%
        # dplyr::filter(study_id != 'TCGA') %>%
        # dplyr::mutate(study_id = ifelse(study_id == 'TCGA',paste0(study_id,"_",tumor_detail),study_id))
      
    }
    temp.df
  })
  
  # 将获取到的内更新到页面
  observe({

    updatePickerInput(session = session, inputId = "subgroup_cancer_detail",
                      choices = unique(tumor.names.df()$tumor_detail), selected = NULL)
  })
  # 1.根据肿瘤名称和队列类型（ICI或非ICI队列）从数据库获取队列名称
  study.names.df <- eventReactive(c(input$subgroup_cancer_detail),{
    cancer <- input$subgroup_cancer_detail
    study_type <- input$subgroup_study_type
    temp.df <- study.df %>%
      dplyr::filter(tumor_detail == cancer)
    if (study_type == 'Immunogenomics Studies'){
      temp.df <- temp.df %>%
        dplyr::filter(study_type == 'ICIs')
    }else{
      temp.df <- temp.df %>%
        dplyr::filter(study_type == 'nonICIs') #%>%
        # dplyr::filter(study_id != 'TCGA') %>%
        # dplyr::mutate(study_id = ifelse(study_id == 'TCGA',paste0(study_id,"_",tumor_detail),study_id))
      
    }
    temp.df
  })
  # 将获取到的内更新到页面
  observe({
    updatePickerInput(session = session, 
                      inputId = "subgroup_study_id",
                      choices = unique(study.names.df()$study_id), 
                      selected = NULL)
  })
  
  # 2. 获取数据类型
  observeEvent(c(input$subgroup_study_id),{
    # 页面缓冲显示
    show_modal_spinner(spin = "orbit",color = "#6bba8b",text = "Loading data...")
    temp_id <- input$subgroup_study_type 
    subgroup_in_study_type_id <- case_when(temp_id == 'Immunogenomics Studies' ~ 'ICIs',
                                   TRUE  ~ 'nonICIs')
    
    subgroup_in_cancer_id <- input$subgroup_cancer_detail 
    subgroup_in_study_id <- input$subgroup_study_id
    cat("Subgroup-前端返回的队列名称: ",subgroup_in_study_id,"\n")
    
    # 本次过滤后应只有一条记录
    temp.df <- study.df %>%
      dplyr::filter(study_type %in% subgroup_in_study_type_id) %>%
      dplyr::filter(tumor_detail %in% subgroup_in_cancer_id) %>%
      dplyr::filter(study_id %in% subgroup_in_study_id) 
    # return(temp.df)
    
    data.types <- unique(unlist(str_split(temp.df$data_type[1],"/")))
    # return(data.types)
    # 更新数据类型
    updatePickerInput(session = session, inputId = "subgroup_data_type",
                      choices = data.types, 
                      selected = data.types[1])
    
    db.tmp.dat <- data.frame()
    if(data.types[1] == 'Mutation'){
      tbl.name <- paste0(tolower(subgroup_in_study_id),'_mutation')
      db.dat.dna <- queryDataFromMySQL(tbl.name)
      db.tmp.dat <- db.dat.dna
    }
    if(data.types[1] == 'Expr'){
      tbl.name <- paste0(tolower(subgroup_in_study_id),'_expr')
      db.dat.rna <- queryDataFromMySQL(tbl.name)
      db.dat.rna$variant_classification <- ''
      db.tmp.dat <- db.dat.rna
    }
    if(data.types[1] == 'Proteome'){
      tbl.name <- paste0(tolower(subgroup_in_study_id),'_proteome')
      db.dat.rna <- queryDataFromMySQL(tbl.name)
      db.dat.rna$variant_classification <- ''
      db.tmp.dat <- db.dat.rna
    }
    # 更新基因名
    updateSelectizeInput(session = session, 
                         inputId = "subgroup_symbol_id", 
                         choices = unique(db.tmp.dat$hugo_symbol), 
                         selected = NULL, 
                         server = TRUE)
    
    # 更新突变类型
    updatePickerInput(session = session, inputId = "subgroup_vartype_id",
                      choices = unique(db.tmp.dat$variant_classification), 
                      selected = unique(db.tmp.dat$variant_classification),
                      options = list(
                        `actions-box` = TRUE))
    remove_modal_spinner()  # 搜索结束后消除缓冲显示
  })
  
  # 3. 根据队列名称从数据获取队列内的基因、表达、突变、临床信息
  observeEvent(c(input$subgroup_data_type),{
    
    study <- input$subgroup_study_id
    data_type <- input$subgroup_data_type
    cat("Subgroup-前端返回的数据类型: ",data_type,"\n")
    
    
    db.tmp.dat <- data.frame()
    if(data_type == 'Mutation'){
      tbl.name <- paste0(tolower(study),'_mutation')
      db.dat.dna <- queryDataFromMySQL(tbl.name)
      db.tmp.dat <- db.dat.dna
    }
    if(data_type == 'Expr'){
      tbl.name <- paste0(tolower(study),'_expr')
      db.dat.rna <- queryDataFromMySQL(tbl.name)
      db.dat.rna$variant_classification <- ''
      db.tmp.dat <- db.dat.rna
    }
    if(data_type == 'Proteome'){
      tbl.name <- paste0(tolower(study),'_proteome')
      db.dat.rna <- queryDataFromMySQL(tbl.name)
      db.dat.rna$variant_classification <- ''
      db.tmp.dat <- db.dat.rna
    }
    # return(db.tmp.dat)
    # 更新基因名
    updateSelectizeInput(session = session, 
                         inputId = "subgroup_symbol_id", 
                         choices = unique(db.tmp.dat$hugo_symbol), 
                         selected = NULL, 
                         server = TRUE)
    
    # 更新突变类型
    updatePickerInput(session = session, inputId = "subgroup_vartype_id",
                      choices = unique(db.tmp.dat$variant_classification), 
                      selected = unique(db.tmp.dat$variant_classification),
                      options = list(
                        `actions-box` = TRUE))
    
    # 更新interaction属性
    updateSelectizeInput(session = session, 
                      inputId = "subgroup_factor_gene_id",
                      choices = unique(db.tmp.dat$hugo_symbol), 
                      selected = NULL,
                      server = TRUE)
  })
  # 3. 根据队列名称从数据概括表获取临床属性,并更新到页面
  study.clinical.df <- eventReactive(input$subgroup_study_id,{
    # cancer <- input$subgroup_cancer_detail
    selected_study_id <- input$subgroup_study_id
    temp.df <- study.df %>%
      dplyr::filter(study_id == selected_study_id) %>%
      dplyr::distinct(study_id,.keep_all = T)
    
    temp.clinicals <- data.frame(clinical_properties = unlist(strsplit(temp.df$clinical_property,split = "#",fixed = F)))
    temp.clinicals <- temp.clinicals %>%
      dplyr::filter(!clinical_properties %in% c('orr','response'))
    return(temp.clinicals)
  })
  observe({
    # 更新clincal属性
    clinical_properties <- unique(study.clinical.df()$clinical_properties)
    updatePickerInput(session = session, inputId = "subgroup_factor_id",
                      choices = clinical_properties, selected = clinical_properties,
                      options = list(
                        `actions-box` = TRUE))
  })
  
  # 基因表达分组cutoff更新联动
  observe({
    number1 <- input$subgroup_exprcut1_id  # value 1
    updateNumericInput(session, inputId = "subgroup_exprcut2_id",
                       label = "Cutoff (%,Low):",
                       value = 100-number1,
                       min = 0, max = 100, step = NA)
  })
  # 若需要cutoff1与cutoff2联动，则取消以下注释；本处为方便设置不同的cutoff，因此不对cutoff1联动
  # observe({
  #   number2 <- input$subgroup_exprcut2_id  # value 1
  #   updateNumericInput(session, inputId = "subgroup_exprcut1_id",
  #                      label = "Cutoff (High):",
  #                      value = 100-number2,
  #                      min = 0, max = 100, step = NA)
  # })  
  

  # 从DB获取数据
  # 临床信息
  db.dat.cli <- eventReactive(input$subgroup_study_id,{
    cat("查询Clinical，待分析的研究队列: ",input$subgroup_study_id,"\n")
    cat("查询Clinical，待分析的肿瘤（亚型）: ",input$subgroup_cancer_detail,"\n")
    
    study <- input$subgroup_study_id
    cancer_id <- input$subgroup_cancer_detail # cancer_detail
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
  db.dat <- eventReactive(c(input$subgroup_study_id,
                            input$subgroup_data_type),{
                              
                              data_type <- input$subgroup_data_type
                              study <- input$subgroup_study_id
                              cat("查询Mutation/Expr，待分析的研究队列: ",data_type,"\n")
                              
                              tbl_name <- case_when(data_type == 'Mutation' ~ paste0(tolower(study),'_mutation'),
                                                    data_type == 'Expr' ~ paste0(tolower(study),'_expr'),
                                                    data_type == 'Proteome' ~ paste0(tolower(study),'_proteome'),
                                                    TRUE ~ '')
                              temp.df <- queryDataFromMySQL(tbl_name)
                              return(temp.df)
                            })

  # 业务层：分析
  observeEvent(input$subgroup_goButton,{
    cat("========================= Server Subgroup==============================\n")
    show_modal_progress_line(
      color = "#00A064", # DF0101
      trail_color = "#e95420",
      duration = 90,
      easing = "easeOut",
      text = "Starting Subgroup Analyses..."
    )
    ## 数据处理及展示
    
    # study_type <- input$subgroup_study_type
    study_type_in <- input$subgroup_study_type
    cat("选择的研究队列类型: ", study_type_in, "\n")
    if (study_type_in == 'Immunogenomics Studies'){
      study_type_in <- 'ICIs'
    }else{
      study_type_in = 'nonICIs'
    }
    cancer_detail_in <- input$subgroup_cancer_detail
    cat("选择的研究队列瘤种: ", cancer_detail_in, "\n")
    study_id_in <- input$subgroup_study_id
    cat("选择的研究队列: ", study_id_in, "\n")
    
    # 获取页面选择的队列名称
    # study <- input$subgroup_study_id
    # cat("选择的研究队列: ", study, "\n")
    
    data_type <- input$subgroup_data_type
    cat("选择的队列数据类型: ", data_type, "\n")
    
    # validate(need(try(input$subgroup_symbol_id),"Please select one gene at least!"))
    symbol <- input$subgroup_symbol_id
    cat("选择的基因名称: ", symbol, "\n")
    
    yaxis.type <- input$subgroup_yaxis_factor
    cat("选择的临床信息or基因: ",yaxis.type,"\n")
    
    vartype <- input$subgroup_vartype_id
    cat("选择的基因突变类型: ", vartype, "\n")
    
    clinicalproperties <- input$subgroup_factor_id
    cat("选择的临床属性名称: ", clinicalproperties, "\n")
    
    clinicalproperties.genes <- input$subgroup_factor_gene_id
    cat("选择的交互分析基因名称: ", clinicalproperties.genes, "\n")
    
    exprcut1 <- input$subgroup_exprcut1_id
    exprcut2 <- input$subgroup_exprcut2_id
    cat("选择的基因高表达分位数: ", exprcut1, "\n")
    cat("选择的基因低表达分位数: ", exprcut2, "\n")
    
    # 数据清理、绘图、输出
    # cat("Clinical obj:",class(db.dat.cli()),"\n")
    # 提取参数
    input.genes <- symbol
    
    if(data_type == 'Mutation'){
      if(length(input.genes) >1){
        input.genes <- c(input.genes,"All Queries Genes")
      }
      
    }
    input.vartype <- vartype
    
    input.clinpro <- clinicalproperties
    input.clinpro.gene <- clinicalproperties.genes
    # 在interaction list中有与grouped基因重复时，弹出警告
    if(length(intersect(input.genes,input.clinpro.gene)) > 0){
      showNotification("Grouped genes should be excluded from interaction gene list!", 
                       duration = 3,
                       type = c("warning"))
    }
      
    input.exprcut1 <- exprcut1/100  #转化为分位数函数的输入值
    input.exprcut2 <- exprcut2/100
    
    # 第一层tab，按照选择的基因写出到页面
    output$subgroup_maintabs <- renderUI({
      tabs <- lapply(input.genes, function(name) {
        tabPanel(
          title = name,
          uiOutput(paste0(name,'_subgroup'))
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
        if(gname == 'All Queries Genes') {symbol.name <- input.genes} # EGFR#TP53#ALK
        
        update_modal_progress(
          value = i / n,
          text =  paste("Analyzing gene:", gname,sprintf("(%1.0f%%)", i/n*100)) #paste("Analyzing gene: ", gname)
        )
        
        # 取出时间字段,只分析有终点的
        survival.colnames <- colnames(db.dat.cli())[grepl("months|status",colnames(db.dat.cli()))]
        
        #  high expr or mutation sample
        # for mutation
        if(data_type == 'Mutation'){
          # 临床样本与突变样本取交集
          tumor.samples <- intersect(unique(db.dat()$sample_id),db.dat.cli()$sample_id)
          grp1.df <- db.dat() %>%
            dplyr::filter(hugo_symbol %in% symbol.name,
                          variant_classification %in% input.vartype) %>%
            dplyr::select(sample_id,hugo_symbol)
          # 选择临床信息进行亚组分析
          if(yaxis.type == "Clinicopathologicals"){
            clc.cli <- db.dat.cli() %>%
              dplyr::filter(sample_id %in% tumor.samples) %>%
              dplyr::mutate(Group = factor(ifelse(sample_id %in% unique(grp1.df$sample_id),'Mut','Wt'),levels = c('Wt','Mut'))) %>%
              dplyr::select(sample_id,risk = Group,all_of(survival.colnames),all_of(input.clinpro))
            
            interaction.vars <- input.clinpro
          }
          # 选择基因基因亚组分析
          if(yaxis.type == "Gene Symbols"){
            clc.cli.gene <- do.call(cbind,lapply(input.clinpro.gene, function(ggsym){
              # ggsym <- input.clinpro.gene[1]
              # browser()
              grpi.df <- db.dat() %>%
                dplyr::filter(hugo_symbol %in% ggsym,
                              variant_classification %in% input.vartype) %>%
                dplyr::select(sample_id,hugo_symbol)
              
              tmp.cli <- db.dat.cli() %>%
                dplyr::filter(sample_id %in% tumor.samples) %>%
                dplyr::mutate(Group = factor(ifelse(sample_id %in% unique(grpi.df$sample_id),'Mut','Wt'),
                                             levels = c('Wt','Mut'))) %>%
                dplyr::rename(!!quo_name(ggsym) := Group) %>%
                dplyr::select(ggsym) 
              return(tmp.cli)
            }))
            # browser()
            clc.cli <- db.dat.cli() %>%
              dplyr::filter(sample_id %in% tumor.samples) %>%
              dplyr::mutate(Group = factor(ifelse(sample_id %in% unique(grp1.df$sample_id),'Mut','Wt'),levels = c('Wt','Mut'))) %>%
              dplyr::select(sample_id,risk = Group,all_of(survival.colnames)) %>%
              dplyr::bind_cols(clc.cli.gene)
            
            interaction.vars <- colnames(clc.cli.gene)
          }
          
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
          
          # 选择临床信息进行亚组分析
          if(yaxis.type == "Clinicopathologicals"){
            clc.cli <- db.dat.cli() %>%
              dplyr::filter(sample_id %in% tumor.samples) %>%
              dplyr::inner_join(grp1.df,by='sample_id')  %>%
              dplyr::select(sample_id,risk = Group,all_of(survival.colnames),all_of(input.clinpro))
            interaction.vars <- input.clinpro
          }
          # 选择基因基因亚组分析
          if(yaxis.type == "Gene Symbols"){
            clc.cli.gene <- do.call(cbind,lapply(input.clinpro.gene, function(ggsym){
              # ggsym <- input.clinpro.gene[1]
              # symbol.name必须为单个基因的名称
              grpi.df <- db.dat() %>%
                dplyr::select(hugo_symbol,tumor.samples) %>%
                dplyr::filter(hugo_symbol %in% ggsym) %>%
                tibble::column_to_rownames(var = 'hugo_symbol') %>%
                t() %>% as.data.frame() %>%
                tibble::rownames_to_column(var = 'sample_id')
              colnames(grpi.df)[2] <- "intergene"
              
              cutoff_h <- quantile(as.numeric(grpi.df$intergene),input.exprcut1,na.rm = T)
              cutoff_l <- quantile(as.numeric(grpi.df$intergene),input.exprcut2,na.rm = T)
              
              quantile.25 <- quantile(as.numeric(grpi.df$intergene),0.25,na.rm = T)
              quantile.75 <- quantile(as.numeric(grpi.df$intergene),0.75,na.rm = T)
              if(quantile.25 == quantile.75){
                cat("Interaction基因：",ggsym,"的第一分位数和第三分为数相等,无法以75%以内分位值为阈值。系统将以第一个不同的表达值为阈值分组\n")
                cutoff_h <- unique(sort(as.numeric(grpi.df$intergene)))[2]
                cutoff_l <- unique(sort(as.numeric(grpi.df$intergene)))[2]
              }
              tmp.cli <- grpi.df %>%
                dplyr::mutate(intergene = as.numeric(intergene)) %>%
                dplyr::mutate(Group = ifelse(intergene >= cutoff_h,"High",
                                             ifelse(intergene <= cutoff_l,'Low','Middle'))) %>%
                # dplyr::filter(Group %in% c('Low','High')) %>% # 为保证样本顺序一致性，不做筛选
                dplyr::select(Group)
              tmp.cli$Group <- factor(tmp.cli$Group,levels = c('Low','High'))
              colnames(tmp.cli)[1] <- ggsym
              return(tmp.cli)
            }))
            # browser()
            clc.cli <- db.dat.cli() %>%
              dplyr::filter(sample_id %in% tumor.samples) %>%
              dplyr::inner_join(grp1.df,by='sample_id')  %>%
              dplyr::select(sample_id,risk = Group,all_of(survival.colnames)) %>%
              dplyr::bind_cols(clc.cli.gene)
            # browser()
            interaction.vars <- colnames(clc.cli.gene)
          }
          
        }
        
        # 本次过滤后应只有一条记录
        temp.outcome.df <- study.df %>%
          dplyr::filter(study_type %in% study_type_in) %>%
          dplyr::filter(tumor_detail %in% cancer_detail_in) %>%
          dplyr::filter(study_id %in% study_id_in)
        cli.tab.names <- toupper(unlist(strsplit(temp.outcome.df$outcome[1],split = "#",fixed = F)))
        # cat("=========>cli.tab.names:",cli.tab.names,"\n")
        cli.tab.names <- cli.tab.names[which(cli.tab.names != '')]
        
        # 第二层tab：在每个基因下，按照临床终点输出到页面
        output[[paste0(gname,"_subgroup")]] <- renderUI({
          tabss <- lapply(cli.tab.names, function(cli.name) {
            tabPanel(
              style = "padding-top:15px",
              title = paste0(toupper(cli.name)), status = "primary", solidHeader = TRUE, collapsible = TRUE,
              shinycssloaders::withSpinner(uiOutput(paste0(gname, "_plot_subgroup_",tolower(cli.name)))),
              h5(HTML(paste("<span style=color:black;font-size:12px;>", "All Queries Genes: mutation of at least one of query genes.", "</span>"))),
              
              # downloadButton(paste0(gname, "_dl_plot_subgroup_",tolower(cli.name)), 
              #                tags$span(
              #                  "DLGraph",
              #                  add_prompt(tags$span(icon(name = "question-circle")),
              #                             message = "Save plot in a PDF file.", 
              #                             position = "left")),
              #                style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
              downloadButton(paste0(gname, "_dl_tbl_subgroup_",tolower(cli.name)),
                             tags$span(
                               "DLTable",
                               add_prompt(tags$span(icon(name = "question-circle")),
                                          message = "Save source data for Cox analysis in a txt file.",
                                          position = "left")),
                             style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
              br(),
              DT::dataTableOutput(paste0(gname, "_tbl_subgroup_",tolower(cli.name)))
            )
          })
          do.call(tabsetPanel, tabss)
        })
        
        # 亚组分析
        lapply(cli.tab.names, function(cli.name) {
          # browser()
          # cli.name <- 'OS'
          cat("==================== 亚组分析开始 ===================\n")
          cat("亚组分析的属性:",c(interaction.vars),"\n")
          
          # ORR，Reponse等与治疗响应相关结果不纳入subgroup分析
          # input.clinpro <- setdiff(input.clinpro,c('orr','response'))
          km.df <- clc.cli %>%
            dplyr::select(sample_id,
                          times = paste0(tolower(cli.name),"_months"),
                          status = paste0(tolower(cli.name),"_status"),
                          risk,
                          interaction.vars)%>%
            dplyr::mutate(times = round(as.numeric(times),2)) %>%
            dplyr::filter(!is.na(times),status != '',!is.na(status)) %>%
            as_tibble()
          # 因素因子化
          subgroup.df <- func.factorization(km.df,cancer_detail_in)
          print(as_tibble(subgroup.df)[1:10,])
          # browser()
          subgroup.df <- func.removeColsAllNa(subgroup.df)
          subgroup.df <- func.removeColsAllEmpty(subgroup.df)
          interaction.vars <- intersect(interaction.vars,colnames(subgroup.df))
          # 亚组分析
          {
            # browser()
            sub.df <- subgroup.df %>%
              dplyr::mutate(risk = factor(risk,labels = c(0,1))) %>% # 0为WT或Low组
              as.data.frame()
            cox.fit <- coxph(Surv(times,status) ~ risk,data=sub.df)
            sub.out <- subgroupAnalysis(cox.fit,sub.df,treatment="risk",subgroups=interaction.vars) 
            # 将亚组分析结果整理成森林图绘制数据
            data.df <- data.frame(Group = sub.out$subgroups,
                                  Characteristic = as.character(sub.out$level),
                                  Size = as.numeric(sub.out$sample_1) + as.numeric(sub.out$sample_0),
                                  HR = round(sub.out$HazardRatio,2),
                                  L95 = round(sub.out$Lower,2),
                                  U95 = ifelse(sub.out$Upper == Inf,50,round(sub.out$Upper,2)),
                                  P.value = round(sub.out$Pvalue,4),
                                  P.interaction = round(sub.out$pinteraction,2)) %>%
              dplyr::mutate(HR95CI = paste0(HR,'(',L95,'-',U95,')'))
            
            func.table4subplot <- function(cli.names){
              temp.df <- data.df %>%
                dplyr::filter(Group == cli.names) %>%
                dplyr::distinct(Group,.keep_all = T) %>%
                dplyr::select(Characteristic = Group,Size,HR,L95,U95,HR95CI,P.value,P.interaction) %>%
                dplyr::mutate(Size = NA,HR = NA,L95=NA,U95=NA,P.value=NA,HR95CI = NA)
              table.df <- data.df %>%
                dplyr::filter(Group == cli.names) %>%
                dplyr::select(-Group,-P.interaction) %>%
                dplyr::mutate(Characteristic = paste0("  ",Characteristic)) %>%
                dplyr::bind_rows(temp.df,.)
              return(table.df)
            }
            
            table.df <- do.call(rbind,lapply(unique(data.df$Group),func.table4subplot))
            table.df$P.value <- ifelse(table.df$P.value < 0.001,'<0.001',as.character(table.df$P.value))
            # 绘图数据准备完毕
            cat("亚组分析-绘图数据:\n")
            print(table.df)
            # browser()
            pdf_prefix <- ifelse(gname == 'All Queries Genes',"AllQueriesGenes",gname)
            temp.pdfname <- paste0(pdf_prefix,"_",tolower(cli.name),"_",round(runif(1,1000000,10000000)),"_",Sys.Date(),"_forestplot_subgroup.pdf")
            # Plan A：对于突变数目小于5的队列，不展示森林图
            # Plan B：对于突变数目小于5的队列，展示一段默认提示
            # if((length(unique(table.df$L95)) == 1 & is.na(unique(table.df$L95))) || 
            #    (length(unique(table.df$U95)) == 1 & is.na(unique(table.df$U95)))){
            #   cat("Subgroup-分析结果HR的95%上下限均为NA，可能原因是突变样本只有0或1个\n")
            #   use.demo <- TRUE
            # }
            if(nrow(subgroup.df[which(subgroup.df$risk %in% c('Mut','High')),]) < 5){
              cat("Subgroup-分析，基因",gname,"的突变(高表达)样本小于5个，不展示森林图\n")
              show.forester <- FALSE
            }else{
              # browser()
              forester(left_side_data = table.df[,1:2],
                       estimate = table.df$HR,
                       ci_low = table.df$L95,
                       ci_high = table.df$U95,
                       right_side_data = table.df[,6:8],
                       # estimate_col_name = 'HR(95%CI)',
                       ggplot_width = 15,
                       display = FALSE,
                       xlim = c(-0.5, 2),
                       null_line_at = 1,
                       arrows = TRUE,
                       render_as = 'pdf',
                       arrow_labels = c(levels(subgroup.df$risk)[1],levels(subgroup.df$risk)[2]),
                       file_path = here::here(paste0(temp_file_path,"/",temp.pdfname)))
              use.demo <- FALSE
              show.forester <- TRUE
            }
            
          }
          
          output.graphic[[paste0(gname,"_tbl_subgroup_",tolower(cli.name))]]$subgroup_tbl <- subgroup.df
          # output.graphic[[paste0(gname,"_plot_subgroup_",tolower(cli.name))]]$cox.plot <- NULL
          
          addResourcePath(prefix = "temp", directoryPath = temp_file_path)
          resourcePaths()
          output[[paste0(gname, "_plot_subgroup_",tolower(cli.name))]] <- renderUI({
            # if(use.demo){
            #   tags$iframe(style="height:485px; width:100%", 
            #               src = paste0("temp/demo.subgroup.replace.pdf"))
            # }
            if(!show.forester){
              HTML(paste("<span style=color:red;font-size:18px;>", 
                         "Forest plot was not displayed because of there is only ",
                         nrow(subgroup.df[which(subgroup.df$risk %in% c('Mut','High')),]),
                         " patients with ",
                         gname,
                         " mutation or high expression!", "</span>"))
            }else{
              tags$iframe(style="height:485px; width:100%", 
                          src = paste0("temp/",temp.pdfname))
            }
            
          })
          output[[paste0(gname, "_tbl_subgroup_",tolower(cli.name))]] <- renderDataTable({
            # dis.tbl <- table.df %>%
            #   dplyr::select(-HR,-L95,-U95)
            # dis.tbl$Characteristic <- gsub("risk",gname,dis.tbl$Characteristic)
            
            dis.tbl <- subgroup.df
            datatable(dis.tbl, style = 'bootstrap', 
                      selection = list(mode = "single", target = "row"),
                      rownames = FALSE, 
                      options = list(orderClasses = TRUE,dom = 'Bfrtip',searching = F, 
                                     columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                     language = list(zeroRecords = "No records found matching your selection")), 
              # colnames = c("Characteristics",'Sample size', "HR (95%CI)", "P value","P interaction")
              colnames = c("Sample ID",cli.name,paste0(cli.name,'.event'), toupper(gname), toupper(interaction.vars))
              )
          })
          # output[[paste0(gname, "_dl_plot_subgroup_",tolower(cli.name))]] <- downloadHandler(
          #   filename = function() {
          #     if(grepl("#",gname)) gname <- "Comb"
          #     paste0("plot_subgroup_",tolower(cli.name),"_",gname,"_",Sys.Date(),".pdf", sep = "")
          #   },
          #   content = function(file) {
          #     pdf(file,onefile = F,width = 8,height = 6, pointsize = 10)
          #     print(output.graphic[[paste0(gname,"_plot_subgroup_",tolower(cli.name))]]$cox.plot)
          #     dev.off()
          #   }
          # )
          
          output[[paste0(gname, "_dl_tbl_subgroup_",tolower(cli.name))]] <- downloadHandler(
            filename = function() {
              paste0(prefix_output_file,"_Subgroup_Data_",toupper(cli.name),"_",gname,"_",Sys.Date(), '.txt', sep='')
            },
            content = function(file) {
              readr::write_delim(x = output.graphic[[paste0(gname,"_tbl_subgroup_",tolower(cli.name))]]$subgroup_tbl, path = file, delim = "\t")
            }
          )
          cat("==================",paste0(" 基因",gname,": 亚组分析结束 ======================\n"))
        })
        # Increment the progress bar, and update the detail text.
        Sys.sleep(0.01)
      })
      remove_modal_progress()
  })
  
  
}
