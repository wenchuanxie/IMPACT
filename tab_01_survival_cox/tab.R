#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : tab.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-04-14
# @IDE     : RStudio
# @Desc    : 第一个Tab功能的第二个子功能：生存分析之Cox曲线
#===============================================================================

source("./r/func.ggforestplot.R")
cat_prefix_cox <- 'Cox'
tab_01_survival_cox <- list()
sidebar <- sidebarPanel(
  id = 'cox_sidebar',
  width = 3,
  h3("Cox Regression"),
  awesomeRadio(inputId = "cox_study_type",
               label = "Select study types:", 
               choices = c("Immunogenomics Studies", "Non-immunogenomics Studies"),
               selected = "Immunogenomics Studies",
               # inline = TRUE,
               status = "success"),
  conditionalPanel('input.cox_study_type != ""',
                   pickerInput(inputId = "cox_cancer_detail",
                               label = "Select one cancer type, E.g. 'LUAD'",
                               choices = NULL,
                               multiple = FALSE,
                               selected = NULL)),
  conditionalPanel('input.cox_cancer_detail != ""',
                   pickerInput(inputId = "cox_study_id",
                               label = "Select one dataset, E.g. 'Hellmann_2018'",
                               choices = NULL,
                               multiple = FALSE,
                               selected = NULL)),
  tags$hr(style="border-color: purple;"),
  conditionalPanel('input.cox_study_id != ""',
                   # 队列数据类型：Mutation,RNA,Protein,Methyl
                   pickerInput("cox_data_type",
                               label = "Select data type, E.g. 'Mutation'", 
                               choices = NULL,
                               selected = NULL, 
                               multiple = FALSE),
                   selectizeInput(inputId = "cox_symbol_id", 
                                  label = "Select/Input gene symbols, E.g. 'EGFR STK11'", 
                                  choices = NULL, 
                                  width = '100%',
                                  multiple = T, 
                                  options = list(delimiter = " ", create = T,maxItems = 10,
                                                 placeholder = "No more than 10 genes.",
                                                 plugins = list('remove_button', 'drag_drop'))),
                   conditionalPanel('input.cox_data_type == "Mutation"',
                                    awesomeRadio(
                                      inputId = "cox_logical",
                                      label = "Mutations in all(AND)/any(OR) quried genes",
                                      inline = T,
                                      choices = c("AND" ,"OR"),
                                      selected = "AND",
                                      status = "success",
                                      checkbox = TRUE
                                    ),
                                    pickerInput(inputId = "cox_vartype_id",
                                                label = tags$span(
                                                  add_prompt(tags$span(icon(name = "circle-question")),
                                                             message = "Variant Classification for Mutation data", 
                                                             position = "right"),
                                                  "Select gene mutation types"),
                                                choices = NULL,
                                                multiple = TRUE,
                                                selected = NULL)
                                    ),
                   conditionalPanel("input.cox_data_type == 'Expr' || input.cox_data_type == 'Proteome'",
                                    fluidRow(
                                      column(6,numericInput(inputId = "cox_exprcut1_id",
                                                            label = tags$span(
                                                              add_prompt(tags$span(icon(name = "circle-question")),
                                                                         message = "Percentile threshold for High/Low group", 
                                                                         position = "right"),
                                                              "Percentile cutoff (%,High):"),
                                                            value = 50,
                                                            min = 0,
                                                            max = 100,step = NA,width = '100%')),
                                      column(5,numericInput(inputId = "cox_exprcut2_id",
                                                            label = "",
                                                            value = 50,
                                                            min = 0,
                                                            max = 100,step = NA,width = '100%'))
                                    )
                   ),
                   tags$hr(style="border-color: purple;"),
                   # 用于Cox分析的临床属性
                   pickerInput(inputId = "cox_cox_id",
                               label = tags$span(
                                  add_prompt(tags$span(icon(name = "circle-question")),
                                             message = "Response evaluation criterias like ORR and DCR et.al 
                                                         will be exulcued in Cox analysis!", 
                                             position = "right"),
                                 "Select one or mutliple clinical characteristics"),
                               choices = NULL,
                               multiple = TRUE,
                               selected = NULL)),
  # conditionalPanel('input.cox_data_type == "Mutation"',
  #                  pickerInput(inputId = "cox_vartype_id",
  #                              label = tags$span(
  #                                 add_prompt(tags$span(icon(name = "circle-question")),
  #                                            message = "Variant Classification for Mutation data", 
  #                                            position = "right"),
  #                                "Select gene mutation types"),
  #                              choices = NULL,
  #                              multiple = TRUE,
  #                              selected = NULL)),
  # conditionalPanel("input.cox_data_type == 'Expr' || input.cox_data_type == 'Proteome'",
  #                  fluidRow(
  #                    column(6,numericInput(inputId = "cox_exprcut1_id",
  #                                          label = tags$span(
  #                                             add_prompt(tags$span(icon(name = "circle-question")),
  #                                                        message = "Percentile threshold for High/Low group", 
  #                                                        position = "right"),
  #                                            "Percentile cutoff (%,High):"),
  #                                          value = 50,
  #                                          min = 0,
  #                                          max = 100,step = NA,width = '100%')),
  #                    column(5,numericInput(inputId = "cox_exprcut2_id",
  #                                          label = "",
  #                                          value = 50,
  #                                          min = 0,
  #                                          max = 100,step = NA,width = '100%'))
  #                  )
  # ),
  # 提交按钮
  # div(style = "display:inline-block; float:center", actionButton("goButton", "Submit!",styleclass ="primary"))
  # actionButton(inputId = "cox_goButton",
  #              label = "Submit",
  #              class ="btn-primary")
  fluidRow(
    column(2, 
           div(style="display:inline-block",actionButton(inputId = "cox_goButton",label = "Submit",class ="btn-primary"), style="float:left"),
           
    ),
    column(7),
    column(2, 
           div(style="display:inline-block",actionButton("reset_input_cox", "Clear",class="btn-warning"), style="float:left"),
           
    )
  )
  
)

mainpage <- mainPanel(
  id = 'cox_mainpage',
  width = 9,
  uiOutput(outputId='cox_maintabs')
)

tab_01_survival_cox$ui <- sidebarLayout(sidebar, mainpage)

tab_01_survival_cox$server <- function(input, output,session) {
  cat("========================= Start Cox==================================\n")
  
  # 定义变量存储输出对象
  output.graphic <- reactiveValues()
  
  observeEvent(input[['reset_input_cox']], {
    shinyjs::reset("cox_sidebar")
  })
  observeEvent(input$reset_input_cox, {
    output[['cox_maintabs']] <- NULL
  })
  
  # 0.g根据队列类型（ICI或非ICI队列）从数据库获取肿瘤名称名称
  tumor.names.df <- eventReactive(c(input$cox_study_type),{
    study_type <- input$cox_study_type
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

    updatePickerInput(session = session, inputId = "cox_cancer_detail",
                      choices = unique(tumor.names.df()$tumor_detail), selected = NULL)
  })
  # 1.根据肿瘤名称和队列类型（ICI或非ICI队列）从数据库获取队列名称
  study.names.df <- eventReactive(c(input$cox_cancer_detail),{
    cancer <- input$cox_cancer_detail
    study_type <- input$cox_study_type
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
    cox_study_id_list <- unique(study.names.df()$study_id)
    updatePickerInput(session = session, 
                      inputId = "cox_study_id",
                      choices = cox_study_id_list, 
                      selected = cox_study_id_list[1])
  })
  
  # 2. 获取数据类型
  observeEvent(c(input$cox_study_id),{
    # 页面缓冲显示
    show_modal_spinner(spin = "orbit",color = "#6bba8b",text = "Loading data...")
    temp_id <- input$cox_study_type 
    cox_in_study_type_id <- case_when(temp_id == 'Immunogenomics Studies' ~ 'ICIs',
                                   TRUE  ~ 'nonICIs')
    
    cox_in_cancer_id <- input$cox_cancer_detail 
    cox_in_study_id <- input$cox_study_id
    cat("Cox-前端返回的队列名称: ",cox_in_study_id,"\n")
    
    # 本次过滤后应只有一条记录
    temp.df <- study.df %>%
      dplyr::filter(study_type %in% cox_in_study_type_id) %>%
      dplyr::filter(tumor_detail %in% cox_in_cancer_id) %>%
      dplyr::filter(study_id %in% cox_in_study_id) 
    # return(temp.df)
    
    data.types <- unique(unlist(str_split(temp.df$data_type[1],"/")))
    # return(data.types)
    # 更新数据类型
    updatePickerInput(session = session, inputId = "cox_data_type",
                      choices = data.types, 
                      selected = data.types[1])
    
    db.tmp.dat <- data.frame()
    if(data.types[1] == 'Mutation'){
      tbl.name <- paste0(tolower(cox_in_study_id),'_mutation')
      db.dat.dna <- queryDataFromMySQL(tbl.name)
      db.tmp.dat <- db.dat.dna
    }
    if(data.types[1] == 'Expr'){
      tbl.name <- paste0(tolower(cox_in_study_id),'_expr')
      db.dat.rna <- queryDataFromMySQL(tbl.name)
      db.dat.rna$variant_classification <- ''
      db.tmp.dat <- db.dat.rna
    }
    if(data.types[1] == 'Proteome'){
      tbl.name <- paste0(tolower(cox_in_study_id),'_proteome')
      db.dat.rna <- queryDataFromMySQL(tbl.name)
      db.dat.rna$variant_classification <- ''
      db.tmp.dat <- db.dat.rna
    }
    # 更新基因名
    updateSelectizeInput(session = session, 
                         inputId = "cox_symbol_id", 
                         choices = unique(db.tmp.dat$hugo_symbol), 
                         selected = NULL, 
                         server = TRUE)
    
    # 更新突变类型
    updatePickerInput(session = session, inputId = "cox_vartype_id",
                      choices = unique(db.tmp.dat$variant_classification), 
                      selected = unique(db.tmp.dat$variant_classification),
                      options = list(
                        `actions-box` = TRUE))
    remove_modal_spinner()  # 搜索结束后消除缓冲显示
  })
  
  # 3. 根据队列名称从数据获取队列内的基因、表达、突变、临床信息
  observeEvent(c(input$cox_data_type),{
    
    study <- input$cox_study_id
    data_type <- input$cox_data_type
    cat("Cox-前端返回的数据类型: ",data_type,"\n")
    
    
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
                         inputId = "cox_symbol_id", 
                         choices = unique(db.tmp.dat$hugo_symbol), 
                         selected = NULL, 
                         server = TRUE)
    
    # 更新突变类型
    updatePickerInput(session = session, inputId = "cox_vartype_id",
                      choices = unique(db.tmp.dat$variant_classification), 
                      selected = unique(db.tmp.dat$variant_classification),
                      options = list(
                        `actions-box` = TRUE))
  })
  # 3. 根据队列名称从数据概括表获取临床属性,并更新到页面
  study.clinical.df <- eventReactive(input$cox_study_id,{
    # cancer <- input$cox_cancer_detail
    selected_study_id <- input$cox_study_id
    temp.df <- study.df %>%
      dplyr::filter(study_id == selected_study_id) %>%
      dplyr::distinct(study_id,.keep_all = T)
    
    temp.clinicals <- data.frame(clinical_properties = unlist(strsplit(temp.df$clinical_property,split = "#",fixed = F)))
    temp.clinicals <- temp.clinicals %>%
      dplyr::filter(!clinical_properties %in% c('orr','clinical_benefit'))
    return(temp.clinicals)
  })
  observe({
    # 更新clincal属性
    clinical_properties <- unique(study.clinical.df()$clinical_properties)
    clinical_properties <- toupper(clinical_properties)
    updatePickerInput(session = session, inputId = "cox_cox_id",
                      choices = clinical_properties, 
                      selected = clinical_properties,
                      options = list(
                        `actions-box` = TRUE))
  })
  
  # 基因表达分组cutoff更新联动
  observe({
    number1 <- input$cox_exprcut1_id  # value 1
    updateNumericInput(session, inputId = "cox_exprcut2_id",
                       label = "Cutoff (%,Low):",
                       value = 100-number1,
                       min = 0, max = 100, step = NA)
  })
  # 若需要cutoff1与cutoff2联动，则取消以下注释；本处为方便设置不同的cutoff，因此不对cutoff1联动
  # observe({
  #   number2 <- input$cox_exprcut2_id  # value 1
  #   updateNumericInput(session, inputId = "cox_exprcut1_id",
  #                      label = "Cutoff (High):",
  #                      value = 100-number2,
  #                      min = 0, max = 100, step = NA)
  # })  
  observe({
    shinyjs::toggleState("cox_goButton", 
                         !is.null(input$cox_symbol_id) && input$cox_symbol_id != "")
  })

  # 从DB获取数据
  # 临床信息
  db.dat.cli <- eventReactive(input$cox_study_id,{
    cat("查询Clinical，待分析的研究队列: ",input$cox_study_id,"\n")
    cat("查询Clinical，待分析的肿瘤（亚型）: ",input$cox_cancer_detail,"\n")
    
    study <- input$cox_study_id
    cancer_id <- input$cox_cancer_detail # cancer_detail
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
  db.dat <- eventReactive(c(input$cox_study_id,
                            input$cox_data_type),{
                              
                              data_type <- input$cox_data_type
                              study <- input$cox_study_id
                              cat("查询Mutation/Expr，待分析的研究队列: ",data_type,"\n")
                              
                              tbl_name <- case_when(data_type == 'Mutation' ~ paste0(tolower(study),'_mutation'),
                                                    data_type == 'Expr' ~ paste0(tolower(study),'_expr'),
                                                    data_type == 'Proteome' ~ paste0(tolower(study),'_proteome'),
                                                    TRUE ~ '')
                              temp.df <- queryDataFromMySQL(tbl_name)
                              return(temp.df)
                            })

  
  # 输入为空验证
  iv_cox <- InputValidator$new()
  iv_cox$add_rule("cox_symbol_id", sv_required())
  iv_cox$add_rule("cox_vartype_id", sv_required())
  iv_cox$add_rule("cox_cox_id", sv_required())
  iv_cox$add_rule("cox_exprcut1_id", sv_gt(9))
  iv_cox$add_rule("cox_exprcut1_id", sv_lt(91))
  iv_cox$add_rule("cox_exprcut2_id", sv_gt(9))
  iv_cox$add_rule("cox_exprcut2_id", sv_lt(91))
  iv_cox$enable()
  
  # 业务层：KM分析
  observeEvent(input$cox_goButton,{
    cat("========================= Server Cox==============================\n")
    show_modal_progress_line(
      color = "#00A064", # DF0101
      trail_color = "#eee",
      duration = 90,
      easing = "easeOut",
      text = "Starting Cox analysis ..."
    )
    ## 数据处理及展示
    
    # study_type <- input$cox_study_type
    study_type_in <- input$cox_study_type
    cat("选择的研究队列类型: ", study_type_in, "\n")
    if (study_type_in == 'Immunogenomics Studies'){
      study_type_in <- 'ICIs'
    }else{
      study_type_in = 'nonICIs'
    }
    
    cancer_detail_in <- input$cox_cancer_detail
    cat("选择的研究队列瘤种: ", cancer_detail_in, "\n")
    study_id_in <- input$cox_study_id
    cat("选择的研究队列: ", study_id_in, "\n")
    
    # 获取页面选择的队列名称
    # study <- input$cox_study_id
    # cat("选择的研究队列: ", study, "\n")
    
    data_type <- input$cox_data_type
    cat("选择的队列数据类型: ", data_type, "\n")
    
    # validate(need(try(input$cox_symbol_id),"Please select one gene at least!"))
    symbol <- input$cox_symbol_id
    cat("选择的基因名称: ", symbol, "\n")
    
    vartype <- input$cox_vartype_id
    cat("选择的基因突变类型: ", vartype, "\n")
    
    clinicalproperties <- input$cox_cox_id
    clinicalproperties <- tolower(clinicalproperties)
    cat("选择的临床属性名称: ", clinicalproperties, "\n")
    
    exprcut1 <- input$cox_exprcut1_id
    exprcut2 <- input$cox_exprcut2_id
    cat("选择的基因高表达分位数: ", exprcut1, "\n")
    cat("选择的基因低表达分位数: ", exprcut2, "\n")
    
    
    cox_logical_type <- input$cox_logical
    cat(cat_prefix_cox,"-选择的多基因突变逻辑关系: ", cox_logical_type, "\n")
    
    # 数据清理、绘图、输出
    # cat("Clinical obj:",class(db.dat.cli()),"\n")
    # 提取参数
    input.genes <- symbol
    
    if(data_type == 'Mutation'){
      # input.genes <- c(input.genes,"All Queried Genes")
      if(length(input.genes) >1){
        input.genes <- c(input.genes,"All Queried Genes")
      }
    }
    input.vartype <- vartype
    input.clinpro <- clinicalproperties
    input.exprcut1 <- exprcut1/100  #转化为分位数函数的输入值
    input.exprcut2 <- exprcut2/100
    
    # 第一层tab，按照选择的基因写出到页面
    output$cox_maintabs <- renderUI({
      tabs <- lapply(input.genes, function(name) {
        tabPanel(
          title = name,
          uiOutput(paste0(name,'_cox'))
        )
      })
      do.call(tabsetPanel, tabs)
    })
    # 执行数据分析
    # withProgress(message = 'Making plot', value = 0, {
      n <- length(input.genes)
      lapply(seq(1,n), function(i) {
        # browser()
        gname <- input.genes[i]
        symbol.name <- gname
        if(gname == 'All Queried Genes') {symbol.name <- setdiff(input.genes,gname)} # EGFR#TP53#ALK
        
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
          
          if(gname == 'All Queried Genes'){
            
            if(cox_logical_type == 'AND'){
              # logical 为 AND 时
              cat(cat_prefix_cox,"- 逻辑AND选择多基因突变模式！\n")
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
            if(cox_logical_type == 'OR'){
              # logical 为 OR 时
              cat(cat_prefix_cox,"- 逻辑OR选择多基因突变模式！\n")
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
            grp1.df <- db.dat() %>%
              dplyr::filter(hugo_symbol %in% symbol.name,
                            variant_classification %in% input.vartype) %>%
              dplyr::select(sample_id,hugo_symbol)
          }
          
          
          clc.cli <- db.dat.cli() %>%
            dplyr::filter(sample_id %in% tumor.samples) %>%
            dplyr::mutate(Group = factor(ifelse(sample_id %in% unique(grp1.df$sample_id),'Mut','WT'),levels = c('WT','Mut'))) %>%
            dplyr::select(sample_id,risk = Group,all_of(survival.colnames),all_of(input.clinpro))
          
          
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
          
          clc.cli <- db.dat.cli() %>%
            dplyr::filter(sample_id %in% tumor.samples) %>%
            dplyr::inner_join(grp1.df,by='sample_id')  %>%
            dplyr::select(sample_id,risk = Group,all_of(survival.colnames),all_of(input.clinpro))
        }
        
        # 根据临床属性定义输出的tab项
        # cli.name1 <- cli.name2 <- cli.name3 <- ""
        # if("TRUE" %in% grepl("pfs",survival.colnames))  cli.name1 <- 'PFS'
        # if("TRUE" %in% grepl("dfs",survival.colnames))  cli.name2 <- 'DFS'
        # if("TRUE" %in% grepl("os",survival.colnames))  cli.name3 <- 'OS'
        # cli.tab.names <- c(cli.name1,cli.name2,cli.name3)
        # cli.tab.names <- cli.tab.names[which(cli.tab.names != '')]
        # 本次过滤后应只有一条记录
        temp.outcome.df <- study.df %>%
          dplyr::filter(study_type %in% study_type_in) %>%
          dplyr::filter(tumor_detail %in% cancer_detail_in) %>%
          dplyr::filter(study_id %in% study_id_in)
        cli.tab.names <- toupper(unlist(strsplit(temp.outcome.df$outcome[1],split = "#",fixed = F)))
        # cat("=========>cli.tab.names:",cli.tab.names,"\n")
        cli.tab.names <- cli.tab.names[which(cli.tab.names != '')]
        
        # 第二层tab：在每个基因下，按照临床终点输出到页面
        output[[paste0(gname,"_cox")]] <- renderUI({
          tabss <- lapply(cli.tab.names, function(cli.name) {
            tabPanel(
              style = "padding-top:15px",
              title = paste0(toupper(cli.name)), status = "primary", solidHeader = TRUE, collapsible = TRUE,
              # plotOutput(paste0(gname, "_plot_cox_",tolower(cli.name)), height = "350px", width = "100%"),
              shinycssloaders::withSpinner(plotOutput(paste0(gname, "_plot_cox_",tolower(cli.name)), height = "350px", width = "100%")),
              # shinycustomloader::withLoader(plotOutput(paste0(gname, "_plot_cox_",tolower(cli.name)), height = "350px", width = "100%"),type = 'html',loader = 'loader2'),
              h5(HTML(paste("<span style=color:black;font-size:12px;>", "All Queried Genes: mutation of at least one of query genes. 
                        Mut: mutation, WT: wild type.\n Hazard ratio (HR) for mutant (Mut) versus wild-type (WT)  was calculated by Cox regression.", "</span>"))),
              
              downloadButton(paste0(gname, "_dl_plot_cox_",tolower(cli.name)), 
                             tags$span(
                               "DLGraph",
                               add_prompt(tags$span(icon(name = "circle-question")),
                                          message = "Save plot in a PDF file.", 
                                          position = "left")),
                             style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
              downloadButton(paste0(gname, "_dl_tbl_cox_",tolower(cli.name)), 
                             tags$span(
                               "DLTable",
                               add_prompt(tags$span(icon(name = "circle-question")),
                                          message = "Save source data for Cox analysis in a txt file.", 
                                          position = "left")),
                             style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
              br(),
              DT::dataTableOutput(paste0(gname, "_tbl_cox_",tolower(cli.name)))
            )
          })
          do.call(tabsetPanel, tabss)
        })
        
        # Cox单因素和多因素分析
        lapply(cli.tab.names, function(cli.name) {
          # cli.name <- 'OS'
          # ORR，Reponse等与治疗响应相关结果不纳入Cox分析
          input.clinpro <- setdiff(input.clinpro,c('orr','clinical_benefit'))
          input.clinpro <- setdiff(input.clinpro,tolower(gname)) # 如果选择的基因已经在临床信息表中，需要从临床表中剔除该基因
          
          km.df <- clc.cli %>%
            dplyr::select(sample_id,
                          times = paste0(tolower(cli.name),"_months"),
                          status = paste0(tolower(cli.name),"_status"),
                          risk,
                          input.clinpro)%>%
            dplyr::mutate(times = round(as.numeric(times),2)) %>%
            dplyr::filter(!is.na(times),status != '',!is.na(status)) %>%
            as_tibble()
          
          output.graphic[[paste0(gname,"_tbl_cox_",tolower(cli.name))]]$cox_tbl <- km.df
          
          cat("==================== 单/多因素Cox分析开始 ===================\n")
          
          # 因素因子化
          cox.df <- func.factorization(km.df,cancer_detail_in)
          cat("因子化后数据:\n")
          print(as_tibble(cox.df)[1:10,])
          
          if(F){
            cox.df <- km.df 
            if('age' %in% colnames(cox.df)) {
              if(max(as.numeric(cox.df$age),na.rm = T) >= 60) {
                cutoff <- 60
                cox.df$age <- factor(ifelse(cox.df$age >= cutoff, ">=60",
                                            ifelse(cox.df$age < cutoff, "<60","")),levels = c('>=60','<60'))
              }else{
                cutoff <- median(cox.df$age,na.rm = T)
                cox.df$age <- factor(ifelse(cox.df$age >= cutoff, ">=median",
                                            ifelse(cox.df$age < 60, "<median","")),levels = c('>=median','<median'))
              }
            }
            if(!cancer_detail_in %in% c('BRCA','PRAD','BRCA','OV','CESC') &'sex' %in% colnames(cox.df)) {
              cox.df$sex <- factor(Hmisc::capitalize(cox.df$sex),levels = c('Male','Female'))
            }
            # 不纳入种族进行分析，本代码无效
            if('race' %in% colnames(cox.df)) {
              # 针对TCGA队列：White and non-white
              if(grepl("TCGA",study_id_in)){
                cox.df$race <- ifelse(cox.df$race %in% c('','[Not Evaluated]','[Unknow]'),'',
                                      ifelse(cox.df$race == 'WHITE','WHITE','Non-WHITE'))
                cox.df$race <- factor(cox.df$race,levels = c('WHITE','Non-WHITE'))
              }
            }
            if('grade' %in% colnames(cox.df)) {
              cox.df$grade <- factor(ifelse(cox.df$grade %in% c('G1','G2'),'G1_2',
                                            ifelse(cox.df$grade %in% c('G3','G4'),'G3_4','')),levels = c('G3_4','G1_2'))
            }
            if('residual_tumor' %in% colnames(cox.df)) {
              cox.df$residual_tumor <- factor(ifelse(cox.df$residual_tumor %in% c('R0'),'R0',
                                                     ifelse(cox.df$residual_tumor %in% c('R1','R2','RX'),'R1_2','')),
                                              levels = c('R0','R1_2'))
            }
            if('surgical_margins' %in% colnames(cox.df)) {
              cox.df$surgical_margins <- factor(capitalize(tolower(cox.df$surgical_margins)),levels = c('Negative','Positive'))
            }
            if('stage' %in% colnames(cox.df)){
              # 前列腺癌的分期需特殊处理
              cox.df$stage <- factor(ifelse(cox.df$stage %in% c('I','II'),'I_II',
                                            ifelse(cox.df$stage %in% c('III','IV'),'III_IV','')),levels = c('III_IV','I_II'))
            }
            if('smoking' %in% colnames(cox.df)) {
              if("Ever" %in% cox.df$smoking){
                cox.df$smoking <- factor(cox.df$smoking,levels = c('Never','Ever'))
              }
              if("Former" %in% cox.df$smoking){
                cox.df$smoking <- factor(case_when(cox.df$smoking %in% c('Former','Current') ~ "Ever", 
                                                   cox.df$smoking == 'Never' ~ "Never", 
                                                   TRUE ~ ""),levels = c('Never','Ever'))
              }
            }
            if('alcohol' %in% colnames(cox.df)) {
              cox.df$alcohol <- factor(capitalize(tolower(cox.df$alcohol)),levels = c('No','Yes'))
            }
            if('chemo_treatment' %in% colnames(cox.df)) {
              cox.df$chemo_treatment <- factor(capitalize(tolower(cox.df$chemo_treatment)),levels = c('No','Yes'))
            }
            if('rt_treatment' %in% colnames(cox.df)) {
              cox.df$rt_treatment <- factor(capitalize(tolower(cox.df$rt_treatment)),levels = c('No','Yes'))
            }
            if('tki_treatment' %in% colnames(cox.df)) {
              cox.df$tki_treatment <- factor(capitalize(tolower(cox.df$tki_treatment)),levels = c('No','Yes'))
            }
            if('tmb' %in% colnames(cox.df)) {
              cutoff <- median(cox.df$tmb,na.rm = T)
              cox.df$tmb <- factor(ifelse(cox.df$tmb >= cutoff,'>=median',
                                          ifelse(cox.df$tmb < cutoff,'<median','')),levels = c('>=median','<median'))
            }
            if('neoantigen' %in% colnames(cox.df)) {
              cutoff <- median(cox.df$neoantigen,na.rm = T)
              cox.df$neoantigen <- factor(ifelse(cox.df$neoantigen >= cutoff,'>=median',
                                                 ifelse(cox.df$neoantigen < cutoff,'<median','')),levels = c('>=median','<median'))
            }
            if('bmi' %in% colnames(cox.df)) {
              cutoff <- median(cox.df$bmi,na.rm = T)
              cox.df$bmi <- factor(ifelse(cox.df$bmi >= cutoff,'>=median',
                                          ifelse(cox.df$bmi < cutoff,'<median','')),levels = c('>=median','<median'))
            }
            if('bmi' %in% colnames(cox.df)) {
              cutoff <- median(cox.df$bmi,na.rm = T)
              cox.df$bmi <- factor(ifelse(cox.df$bmi >= cutoff,'>=median',
                                          ifelse(cox.df$bmi < cutoff,'<median','')),levels = c('>=median','<median'))
            }
            if('cea' %in% colnames(cox.df)) {
              cutoff <- median(cox.df$cea,na.rm = T)
              cutoff <- 10 # 此处用 10 µg/L 为阈值，待修正
              cox.df$cea <- factor(ifelse(cox.df$cea >= cutoff,'>=10',
                                          ifelse(cox.df$cea < cutoff,'< 10','')),levels = c('>=10','<10'))
            }
            
            # pdl1是分类变量，pdl1_exp是连续变量
            if('pdl1' %in% colnames(cox.df)) {
              # cox.df$pdl1 <- factor(ifelse(cox.df$pdl1_exp < 1,'Negative',
              #                              ifelse(cox.df$pdl1_exp >= 1,'Positive','')),levels = c('Negative','Positive'))
              cox.df$pdl1 <- factor(case_when(cox.df$pdl1 %in% c('Weak','Strong') ~ "Positive", 
                                              cox.df$pdl1 == 'Negative' ~ "Negative", 
                                              TRUE ~ ""),levels = c('Negative','Positive'))
              
            }
            if('msi_status' %in% colnames(cox.df)) {
              cox.df$msi_status <- factor(capitalize(tolower(cox.df$msi_status)),levels = c('MSS','MSI-H'))
            }
            if('tp53' %in% colnames(cox.df)) {
              cox.df$tp53 <- factor(cox.df$tp53,levels = c('WT','Mut'))
            }
            if('egfr' %in% colnames(cox.df)) {
              cox.df$egfr <- factor(cox.df$egfr,levels = c('WT','Mut'))
            }
            if('alk' %in% colnames(cox.df)) {
              cox.df$alk <- factor(cox.df$alk,levels = c('WT','Mut'))
            }
            if('stk11' %in% colnames(cox.df)) {
              cox.df$stk11 <- factor(cox.df$stk11,levels = c('WT','Mut'))
            }
            if('kras' %in% colnames(cox.df)) {
              # cox.df$kras <- factor(capitalize(tolower(cox.df$kras)),levels = c('WT','Mut'))
              cox.df$kras <- factor(cox.df$kras,levels = c('WT','Mut'))
              
            }
          }
          
          # 删除全为NA的列
          # cox.df <- func.removeColsAllNa(cox.df)
          # 删除全为空的列
          # cox.df <- func.removeColsAllEmpty(cox.df)
          
          # 单因素Cox分析
          cat("单因素分析的队列:",nrow(cox.df),"行\n")
          print(as_tibble(cox.df)[1:10,])
          cat("单因素分析的属性:",c(input.clinpro),"\n")
          {
            ezcox.res = ezcox(cox.df, covariates = c(input.clinpro,"risk"),
                              time = 'times',status = 'status',global_method = 'likelihood',return_models = T)
            ezcox.mds <- get_models(ezcox.res)
            # ezcox.pot <- show_models(ezcox.mds,merge_models = T,drop_controls = T)
            # 使用broom提取系数并计算CI
            coef.u <- do.call(rbind,lapply(seq(1:length(ezcox.mds)),function(i){
              y <- as.data.frame(broom::tidy(ezcox.mds[[i]], conf.int = TRUE))
              y <- y %>%
                dplyr::select(estimate,conf.low,conf.high) %>%
                dplyr::mutate(estimate = round(estimate,2),
                              conf.low = round(conf.low,2),
                              conf.high = round(conf.high,2))
              return(y)
            }))
            
            unicox.res <- ezcox.res$res %>%
              dplyr::mutate(Variables = Variable,
                            Characteristic = paste0(Variable,"(",contrast_level," vs. ",ref_level,")"),
                            Level = paste0(contrast_level," vs. ",ref_level),
                            Size = n_contrast + n_ref,
                            HR = round(HR,2),
                            L95 = round(lower_95,2),
                            # Note：HR的U95为Inf,导致 conf.low 和 conf.high 为极端值而无法绘图
                            # 因此，本处做限制为 -10 和 10
                            U95 = ifelse(upper_95 == Inf,100,round(upper_95,2)),
                            HR95CI = paste0(round(HR,2),"(",round(lower_95,2),"-",ifelse(upper_95 == Inf,100,round(upper_95,2)),")"),
                            P = round(p.value,4)) %>%
              dplyr::select(Characteristic,Variables,Level,Size,HR,L95,U95,HR95CI,P) 
            cat("单因素分析结果:\n")
            print(unicox.res)
            # browser()
            # 多因素Cox分析
            cox.sig <- setdiff(unicox.res[unicox.res$P <= 0.05,]$Variables,c('',NA,"risk"))
            if(length(cox.sig) == 0){
              cat("单因素Cox分析未筛选到显著因素，将全部因素纳入多因素分析！\n")
              cox.sig <- input.clinpro
            }
            cat("多因素分析的属性:",cox.sig,"\n")
            ezcox.res = ezcox(cox.df, covariates = 'risk',controls = cox.sig,
                              time = 'times',status = 'status',global_method = 'likelihood',return_models = T)
            ezcox.mds <- get_models(ezcox.res)
            # ezcox.pot <- show_models(ezcox.mds,merge_models = T )
            # 使用broom提取系数并计算CI
            coef.m <- as.data.frame(broom::tidy(ezcox.mds[[1]], conf.int = TRUE))
            coef.m <- coef.m %>%
              dplyr::select(estimate,conf.low,conf.high) %>%
              dplyr::mutate(estimate = round(estimate,2),
                            conf.low = round(conf.low,2),
                            conf.high = round(conf.high,2))
            # browser()
            multicox.res <- ezcox.res$res %>%
              dplyr::mutate(Variables_m = c("risk",cox.sig)) %>%
              dplyr::mutate(Characteristic = paste0(Variables_m,"(",contrast_level," vs. ",ref_level,")"),
                            Level_m = paste0(contrast_level," vs. ",ref_level),
                            HR_m = round(HR,2),
                            L95_m = round(lower_95,2),
                            # Note：HR的U95为Inf,导致 conf.low 和 conf.high 为极端值而无法绘图
                            # 因此，本处做限制为 -10 和 10
                            U95_m = ifelse(upper_95 == Inf,100,round(upper_95,2)),
                            HR95CI_m = paste0(round(HR,2),"(",round(lower_95,2),"-",ifelse(upper_95 == Inf,100,round(upper_95,2)),")"),
                            P_m = round(p.value,4)) %>%
              dplyr::select(Characteristic,Variables_m,Level_m,HR_m,L95_m,U95_m,HR95CI_m,P_m)
            cat("多因素分析结果:\n")
            print(multicox.res)
            
            # 行合并单因素和多因素Cox的结果用于输出表格
            cox.res.tbl <- unicox.res %>%
              dplyr::left_join(multicox.res,by= c('Characteristic')) %>%
              dplyr::select(-Variables,-Level,-Variables_m,-Level_m)
            cat("单/多因素分析结果（4Table）:\n")
            print(cox.res.tbl)
            # output.graphic[[paste0(gname,"_tbl_cox_",tolower(cli.name))]]$cox_tbl <- cox.res.tbl
            
            # 列合并单因素和多因素的Cox结果用于绘图
            cox.res.plot <- data.frame(Characteristic = 'Univariable Cox',Level = '',HR = NA,L95 = NA,U95 = NA,HR95CI='',P=NA,estimate = NA,conf.low = NA,conf.high = NA)  %>%
              dplyr::bind_rows(dplyr::bind_cols(dplyr::select(unicox.res,Characteristic = Variables,Level,HR,L95,U95,HR95CI,P),coef.u)) %>%
              dplyr::bind_rows(data.frame(Characteristic = 'Multivariable Cox',Level = '',HR = NA,L95 = NA,U95 = NA,HR95CI='',P=NA,estimate = NA,conf.low = NA,conf.high = NA)) %>%
              dplyr::bind_rows(multicox.res %>%
                                 dplyr::select(Characteristic = Variables_m,Level = Level_m,HR = HR_m,L95 = L95_m,U95 = U95_m,HR95CI = HR95CI_m,P = P_m) %>%
                                 dplyr::bind_cols(coef.m)) %>%
              dplyr::mutate(Characteristic = ifelse(Characteristic == 'risk',gname,
                                                    ifelse(Characteristic %in% c('Univariable Cox',
                                                                                 'Multivariable Cox'),
                                                           Characteristic,
                                                           toupper(Characteristic))))
            cox.res.plot$stars <- ifelse(is.na(cox.res.plot$P),"",paste0(round(cox.res.plot$P, 3), " ",
                                           ifelse(cox.res.plot$P < 0.05, "*",""),
                                           ifelse(cox.res.plot$P < 0.01, "*",""),
                                           ifelse(cox.res.plot$P < 0.001, "*","")))
            cox.res.plot$stars[which(cox.res.plot$P < 0.001)] = "<0.001 ***"
            
            # Note：HR的U95为Inf,导致 conf.low 和 conf.high 为极端值而无法绘图
            # 因此，本处做限制为 -10 和 10
            cox.res.plot$conf.low <- ifelse(cox.res.plot$conf.low < -100,-100,cox.res.plot$conf.low)
            cox.res.plot$conf.high <- ifelse(cox.res.plot$conf.high > 100,100,cox.res.plot$conf.high)
            
            cat("单/多因素分析结果（4Plot）:\n")
            print(cox.res.plot)
          }
          
          # 绘图
          if(F){
            plot.tbl <- data.frame(Characteristic = 'Univariable Cox',HR = NA,L95 = NA,U95 = NA,HR95CI='',P=NA)%>%
              dplyr::bind_rows(dplyr::select(unicox.res,Characteristic,HR,L95,U95,HR95CI,P)) %>%
              dplyr::bind_rows(data.frame(Characteristic = 'Multivariable Cox',HR = NA,L95 = NA,U95 = NA,HR95CI='',P=NA)) %>%
              dplyr::bind_rows(dplyr::select(multicox.res,Characteristic,HR = HR_m,L95 = L95_m,U95 = U95_m,HR95CI = HR95CI_m,P = P_m)) %>%
              as_tibble()
            plot.tbl$Characteristic <- gsub("risk",gname,plot.tbl$Characteristic)
            cat("绘制森林图的数据:\n")
            print(plot.tbl)
            cox.plot <- forester(left_side_data = plot.tbl[,1],
                                 estimate = plot.tbl$HR,
                                 estimate_precision = 2,
                                 ci_low = plot.tbl$L95,
                                 ci_high = plot.tbl$U95,
                                 right_side_data = plot.tbl[,5:6],
                                 null_line_at = 1,
                                 display = F,
                                 xlim = c(0, max(plot.tbl$U95,na.rm = T)),
                                 arrows = TRUE,
                                 arrow_labels = c("Favours High/Mut ", "Favours Low/WT"),
                                 file_path = here::here(paste0("./temp/forester_plot_",gname,".pdf")))
            
          }
          {
            cox.plot  <- func.ggforestplot(cox.res.plot,main = "Cox Proportional Hazards Model")
            # print(p)
          }
          
          sketch_cox = htmltools::withTags(table(
            class = 'display',
            thead(
              tr(
                th(rowspan = 2, 'Characteristics'),
                th(rowspan = 2, 'Sample Size'),
                th(colspan = 2, 'Univaraible Cox'),
                th(colspan = 2, 'Multivariable Cox')
              ),
              tr(
                lapply(c("HR(95%CI)","P value","HR(95%CI)","P value"), th)
              )
            )
          ))
          
          
          # cox.plot <- ezcox.pot
          output.graphic[[paste0(gname,"_plot_cox_",tolower(cli.name))]]$cox.plot <- cox.plot
        
          output[[paste0(gname, "_plot_cox_",tolower(cli.name))]] <- renderPlot({
            print(cox.plot)
            # output.graphic[[paste0(gname,"_plot_cox_",tolower(cli.name))]]$cox.plot
            # hist(norm(100))
          })
          output[[paste0(gname, "_tbl_cox_",tolower(cli.name))]] <- renderDataTable({
            dis.tbl <- cox.res.tbl %>%
              dplyr::select(Characteristic,Size,HR95CI,P,HR95CI_m,P_m) %>%
              tidyr::separate(col = 'Characteristic',into = c('Vars','Suf'),sep = "[(]",remove = F) %>%
              dplyr::mutate(Vars = toupper(Vars)) %>%
              dplyr::mutate(Characteristic = paste0(Vars,"(",Suf)) %>%
              dplyr::select(-Vars,-Suf)
            dis.tbl$Characteristic <- gsub("RISK|risk",gname,dis.tbl$Characteristic)
            datatable(dis.tbl, 
                      container = sketch_cox,
                      selection = list(mode = "single", target = "row"),
                      style = 'bootstrap', rownames = FALSE, 
                      options = list(orderClasses = TRUE,dom = 'Bfrtip', searching = F,
                                     language = list(zeroRecords = "No records found matching your selection"),
                                     columnDefs = list(list(className = 'dt-center', targets = '_all')))
                      )
          })
          output[[paste0(gname, "_dl_plot_cox_",tolower(cli.name))]] <- downloadHandler(
            filename = function() {
              if(grepl("#",gname)) gname <- "Comb"
              paste0(prefix_output_file,"_COX_Plot_",toupper(cli.name),"_",gname,"_",Sys.Date(),".pdf", sep = "")
            },
            content = function(file) {
              # pdf(file,onefile = F, page = 'a4', pointsize = 10)
              # print(output.graphic[[paste0(gname,"_plot_cox_",tolower(cli.name))]]$cox.plot)
              # dev.off()
              ggsave(filename = file,
                     output.graphic[[paste0(gname,"_plot_cox_",tolower(cli.name))]]$cox.plot,width = 280,height = 100,units = "mm")
            }
          )
          
          output[[paste0(gname, "_dl_tbl_cox_",tolower(cli.name))]] <- downloadHandler(
            filename = function() {
              paste0(prefix_output_file,"_COX_Data_",toupper(cli.name),"_",gname,"_",Sys.Date(), '.txt', sep='')
            },
            content = function(file) {
              readr::write_delim(x = output.graphic[[paste0(gname,"_tbl_cox_",tolower(cli.name))]]$cox_tbl, path = file, delim = "\t")
            }
          )
          cat("==================",paste0(" 基因",gname,": Cox分析结束 ======================\n"))
        })
        # Increment the progress bar, and update the detail text.
        # incProgress(1/n, detail = paste("Analyzing gene: ", gname))
        Sys.sleep(0.01)
      })
      remove_modal_progress()
      # Pause for 0.1 seconds to simulate a long computation.
    #   Sys.sleep(0.1)
    # })
  })
  
  
}
