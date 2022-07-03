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

#sd

tab_01_survival_km <- list()
sidebar <- sidebarPanel(
  id = 'km_sidebar',
  width = 3,
  h3("Kaplan-Meier Curve"),
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
  # 队列数据类型：Mutation,RNA,Protein,Methyl
  # conditionalPanel(
  #   condition = "km_study_id != ''",
  #   pickerInput("km_data_type",
  #               label = 'Pls Select Data Type:', 
  #               choices = NULL,
  #               selected = NULL, 
  #               multiple = FALSE)
  #   ),
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
                                                 plugins = list('remove_button', 'drag_drop')))),
  conditionalPanel('input.km_data_type == "Mutation"',
                   # 队列数据的基因突变类型
                   pickerInput(inputId = "km_vartype_id",
                               label = tags$span(
                                  add_prompt(tags$span(icon(name = "question-circle")),
                                             message = "Variant Classification for Mutation data", 
                                             position = "right"),
                                 "Select gene mutation types"),
                               choices = NULL,
                               multiple = TRUE,
                               selected = NULL)),
  conditionalPanel("input.km_data_type == 'Expr' || input.km_data_type == 'Proteome'",
                   # 队列数据的基因表达阈值
                   fluidRow(
                     column(6,numericInput(inputId = "km_exprcut1_id",
                                           label = tags$span(
                                              add_prompt(tags$span(icon(name = "question-circle")),
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
  # 分组颜色
  fluidRow(
    column(6,colourpicker::colourInput(inputId = "km_colorg1_id", 
                                       label = tags$span(
                                          add_prompt(tags$span(icon(name = "question-circle")),
                                                     message = "Color for High/Mut group", 
                                                     position = "right"),
                                         'Color 1'), 
                                       value = "#FF9800", 
                                       showColour = c("both", "text","background"), 
                                       palette = c("square", "limited"), 
                                       allowTransparent = FALSE, 
                                       returnName = TRUE)),
    column(5,colourpicker::colourInput(inputId = "km_colorg2_id", 
                                       label = tags$span(
                                          add_prompt(tags$span(icon(name = "question-circle")),
                                                     message = "Color for Low/Wt group", 
                                                     position = "right"),
                                         'Color 2'),  
                                       value = "#2196F3", 
                                       showColour = c("both", "text","background"), 
                                       palette = c("square", "limited"), 
                                       allowTransparent = FALSE, 
                                       returnName = TRUE))
    ),
  # 提交按钮
  actionButton(inputId = "km_goButton",
               label = "Submit",
               class ="btn-primary"
               # size = 'big',
  )
  # actionButton('km_reset','Reset',class ="primary")
  
)

mainpage <- mainPanel(
  id = 'km_mainpage',
  width = 9,
  uiOutput(outputId='km_maintabs')
)

tab_01_survival_km$ui <- sidebarLayout(sidebar, mainpage)

tab_01_survival_km$server <- function(input, output,session) {
  cat("=========================== Start KM ================================\n")
  
  # 定义变量存储输出对象
  output.graphic <- reactiveValues()

  # 0.根据队列类型（ICI或非ICI队列）从数据库获取肿瘤名称名称
  tumor.names.df <- eventReactive(input$km_study_type,{
    study_type <- input$km_study_type
    temp.df <- study.df 
    if (study_type == 'Immunogenomics Studies'){
      temp.df <- temp.df %>%
        dplyr::filter(study_type == 'ICIs')
    }else{ #  if(study_type == 'GEO/Paper Studies')
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
                      inputId = "km_cancer_detail",
                      choices = unique(tumor.names.df()$tumor_detail), 
                      selected = NULL)
  })
  
  # 1.根据肿瘤名称和队列类型（ICI或非ICI队列）从数据库获取队列名称
  study.names.df <- eventReactive(c(input$km_cancer_detail),{
    study_type <- input$km_study_type
    km_in_cancer_detail <- input$km_cancer_detail
    temp.study.df <- study.df %>%
      dplyr::filter(tumor_detail == km_in_cancer_detail)
    if (study_type == 'Immunogenomics Studies'){
      temp.study.df <- temp.study.df %>%
        dplyr::filter(study_type == 'ICIs')
    }else{
      temp.study.df <- temp.study.df %>%
        dplyr::filter(study_type == 'nonICIs')
    }
    temp.study.df
  })
  # 将获取到的肿瘤队列list更新到页面
  observe({
    updatePickerInput(session = session, inputId = "km_study_id",
                      choices = unique(study.names.df()$study_id), selected = NULL)
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
    temp.df <- study.df %>%
      dplyr::filter(study_type %in% km_in_study_type) %>%
      dplyr::filter(tumor_detail %in% km_in_cancer_detail) %>%
      dplyr::filter(study_id %in% km_in_study_id) 
    
    data.types <- unique(unlist(str_split(temp.df$data_type[1],"/")))
    # 更新数据类型
    updatePickerInput(session = session, inputId = "km_data_type",
                      choices = data.types, 
                      selected = data.types[1])
    
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

  # 业务层：KM分析
  observeEvent(input$km_goButton,{
    cat("=========================== Server KM ==============================\n")
    show_modal_progress_line(
      color = "#00A064", # DF0101
      trail_color = "#e95420",
      duration = 90,
      easing = "easeOut",
      text = "Starting Survival Computation..."
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
    
    exprcut1 <- input$km_exprcut1_id
    exprcut2 <- input$km_exprcut2_id
    cat("选择的基因高表达分位数: ", exprcut1, "\n")
    cat("选择的基因低表达分位数: ", exprcut2, "\n")
    
    colorg1 <- input$km_colorg1_id
    colorg2 <- input$km_colorg2_id
    cat("选择的样本分组1颜色: ", colorg1, "\n")
    cat("选择的样本分组2颜色: ", colorg2, "\n")
    
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
        if(gname == 'All Queried Genes') {symbol.name <- input.genes} # EGFR#TP53#ALK
        
        update_modal_progress(
          value = i / n,
          text =  paste("Analyzing gene:", gname,sprintf("(%1.0f%%)", i/n*100)) #paste("Analyzing gene: ", gname)
        )
        
        # 取出时间字段,只分析有终点的
        survival.colnames <- colnames(db.dat.cli())[grepl("months|status",colnames(db.dat.cli()))]
        
        if(data_type == 'Mutation'){
          # 临床样本与突变样本取交集
          tumor.samples <- intersect(db.dat()$sample_id,db.dat.cli()$sample_id)
          grp1.df <- db.dat() %>%
            dplyr::filter(hugo_symbol %in% symbol.name,
                          variant_classification %in% input.vartype) %>%
            dplyr::select(sample_id,hugo_symbol)
          
          clc.cli <- db.dat.cli() %>%
            dplyr::filter(sample_id %in% tumor.samples) %>%
            dplyr::mutate(Group = factor(ifelse(sample_id %in% unique(grp1.df$sample_id),'Mut','Wt'),levels = c('Wt','Mut'))) %>%
            dplyr::select(sample_id,risk = Group,all_of(survival.colnames))
          
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
            dplyr::select(sample_id,risk = Group,all_of(survival.colnames))
        }
        
        # 根据临床属性定义输出的tab项
        # cli.name1 <- cli.name2 <- cli.name3 <- ""
        # if("TRUE" %in% grepl("pfs",survival.colnames))  cli.name1 <- 'PFS'
        # if("TRUE" %in% grepl("dfs",survival.colnames))  cli.name2 <- 'DFS'
        # if("TRUE" %in% grepl("os",survival.colnames))  cli.name3 <- 'OS'
        # cli.tab.names <- c(cli.name1,cli.name2,cli.name3)
        
        # 本次过滤后应只有一条记录
        temp.outcome.df <- study.df %>%
          dplyr::filter(study_type %in% study_type_in) %>%
          dplyr::filter(tumor_detail %in% cancer_detail_in) %>%
          dplyr::filter(study_id %in% study_id_in )
        cli.tab.names <- toupper(unlist(strsplit(temp.outcome.df$outcome[1],split = "#",fixed = F)))
        # cat("=========>cli.tab.names:",cli.tab.names,"\n")
        cli.tab.names <- cli.tab.names[which(cli.tab.names != '')]
        
        output[[paste0(gname,"_km")]] <- renderUI({
          tabss <- lapply(cli.tab.names, function(cli.name) {
            tabPanel(
              style = "padding-top:15px",
              title = paste0("Survival ",toupper(cli.name)), status = "primary", solidHeader = TRUE, collapsible = TRUE,
              # shinycustomloader::withLoader(plotlyOutput(paste0(gname, "_plot_km_",tolower(cli.name)), height = "350px", width = "100%"),type = 'html',loader = 'loader2'),
              # shinycssloaders::withSpinner(plotlyOutput(paste0(gname, "_plot_km_",tolower(cli.name)),height = "350px", width = "100%")),
              shinycssloaders::withSpinner(plotOutput(paste0(gname, "_plot_km_",tolower(cli.name)),height = "350px", width = "100%")),
              h5(HTML(paste("<span style=color:black;font-size:12px;>", "All Queries Genes: mutation of at least one of query genes. 
                        Mut: mutation, Wt: wild type.\n Hazard ratio (HR) for mutant (Mut) versus wild-type (Wt)  was calculated by Cox regression.", "</span>"))),
              downloadButton(paste0(gname, "_dlplotkm_",tolower(cli.name)), 
                             tags$span(
                               "DLGraph",
                               add_prompt(tags$span(icon(name = "question-circle")),
                                          message = "Save plot in a PDF file.", 
                                          position = "left")),
                             style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
              downloadButton(paste0(gname, "_dltblkm_",tolower(cli.name)), 
                             # "DLTable",
                             tags$span(
                               "DLTable",
                               add_prompt(tags$span(icon(name = "question-circle")),
                                          message = "Save data of survival plot in a txt file.", 
                                          position = "left")),
                             style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
              br(),
              DT::dataTableOutput(paste0(gname, "_tbl_km_",tolower(cli.name)))
            )
          })
          do.call(tabsetPanel, tabss)
        })
        
        # 绘KM图
        lapply(cli.tab.names, function(cli.name) {
          # cat("====>nrow(clc.cli)",nrow(clc.cli),"\n")
          # browser()
          km.df <- clc.cli %>%
            dplyr::mutate(row_id = seq(1,nrow(clc.cli))) %>%
            dplyr::select(row_id,sample_id,
                          times = paste0(tolower(cli.name),"_months"),
                          status = paste0(tolower(cli.name),"_status"),
                          risk)%>%
            dplyr::mutate(times = round(as.numeric(times),2)) %>%
            dplyr::filter(!is.na(times),status != '',!is.na(status))
          
          output.graphic[[paste0(gname,"_",tolower(cli.name))]]$tbl <- km.df
          
          if(length(unique(km.df$risk)) == 1){
            cat("基因 ",gname," 分组只有一组\n")
            km.graphic <- ggsurvplot(survfit(Surv(times, status) ~ risk, 
                                             data = km.df), 
                                     data = km.df,
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
            km.graphic <- func.survivalplot(km.df,'',cli.name,
                                            time.break = time_break_survival(max(km.df$times,na.rm = T)),
                                            tbl.loc = 'topright',
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
          output.graphic[[paste0(gname,"_",tolower(cli.name))]]$km <- km.graphic

          # output[[paste0(gname, "_plot_km_",tolower(cli.name))]] <- renderPlotly({
          #   ggplotly(km.plotly[[1]])
          # })
          output[[paste0(gname, "_plot_km_",tolower(cli.name))]] <- renderPlot({
            print(km.graphic)
          })
          output[[paste0(gname, "_tbl_km_",tolower(cli.name))]] <- renderDataTable({
            datatable(km.df, 
                      style = 'bootstrap', 
                      rownames = FALSE, 
                      selection = list(mode = "single", target = "row"),
                      options = list(orderClasses = TRUE,keys = TRUE,dom = 'Bfrtip', searching = F,
                                     language = list(zeroRecords = "No records found matching your selection"),
                                     columnDefs = list(list(className = 'dt-center', targets = '_all'))),
                      colnames = c("Row ID","Sample ID", cli.name, paste0(cli.name,".event"), gname)
                      )
          })
          output[[paste0(gname, "_dlplotkm_",tolower(cli.name))]] <- downloadHandler(
            filename = function() {
              if(grepl("#",gname)) gname <- "Comb"
              paste0(prefix_output_file,"_KM_Plot_",toupper(cli.name),"_",gname,"_",Sys.Date(),".pdf", sep = "")
            },
            content = function(file) {
              pdf(file,onefile = F, width = 6, height = 4, pointsize = 10)
              print(output.graphic[[paste0(gname,"_",tolower(cli.name))]]$km)
              dev.off()
            }
          )
          output[[paste0(gname, "_dltblkm_",tolower(cli.name))]] <- downloadHandler(
            filename = function() {
              paste0(prefix_output_file,"_KM_Data_",toupper(cli.name),"_",gname,"_",Sys.Date(), '.txt', sep='')
            },
            content = function(file) {
              readr::write_delim(x = output.graphic[[paste0(gname,"_",tolower(cli.name))]]$tbl, path = file, delim = "\t")
            }
          )
        })
        # Increment the progress bar, and update the detail text.
        # incProgress(1/n, detail = paste("Analyzing gene: ", gname))
        Sys.sleep(0.01)
        # if("pfs_months" %in% colnames(clc.cli)) {
        #   km.pfs.df <- clc.cli %>%
        #     dplyr::select(sample_id,times = pfs_months,status = pfs_status,risk)%>%
        #     dplyr::mutate(times = round(as.numeric(times),2)) %>%
        #     dplyr::filter(!is.na(times),status != '')
        #   output.graphic[[gname]]$tbl.pfs <- km.pfs.df
        #   km.graphic <- func.survivalplot(km.pfs.df,'','PFS',time.break = time_break_survival(max(km.pfs.df$times,na.rm = T)),
        #                              tbl.loc = 'topright',
        #                              color.dis = input.color,y.title = 'Progression-free Survival')
        #   output.graphic[[gname]]$km.pfs <- km.graphic
        #   km.pfs.plotly <-  plotly_survival(km.pfs.df,'',time.break = time_break_survival(max(km.pfs.df$times,na.rm = T)),color.dis = input.color,y.title = 'Progression-free Survival')
        #   
        #   
        #   output[[paste0(gname, "_sur_pfs")]] <- renderPlotly({
        #     ggplotly(km.pfs.plotly[[1]])
        #   })
        #   output[[paste0(gname, "_tbl_pfs")]] <- renderDataTable({
        #     datatable(km.pfs.df, style = 'bootstrap', rownames = FALSE, options = list(orderClasses = TRUE,dom = 'tp', language = list(
        #       zeroRecords = "No records found matching your selection")), 
        #       colnames = c("Sample_ID", "Times", "Status", gname))
        #   })
        #   output[[paste0(gname, "_dl_pfs")]] <- downloadHandler(
        #     filename = function() {
        #       if(grepl("#",gname)) gname <- "Comb"
        #       paste("km_pfs_",gname,"_",Sys.Date(),".pdf", sep = "")
        #     },
        #     content = function(file) {
        #       pdf(file,onefile = F, width = 6, height = 4, pointsize = 10)
        #       print(output.graphic[[gname]]$km.pfs)
        #       dev.off()
        #     }
        #   )
        #   output[[paste0(gname, "_dltbl_pfs")]] <- downloadHandler(
        #     filename = function() {
        #       paste("data_pfs_",gname,"_",Sys.Date(), '.txt', sep='')
        #     },
        #     content = function(file) {
        #       readr::write_delim(x = output.graphic[[gname]]$tbl.pfs, path = file, delim = "\t")
        #     }
        #   )
        # }
        # if("os_months" %in% colnames(clc.cli)) {
        #   km.os.df <- clc.cli %>%
        #     dplyr::select(sample_id,times = os_months,status = os_status,risk) %>%
        #     dplyr::mutate(times = round(as.numeric(times),2)) %>%
        #     dplyr::filter(!is.na(times),status != '')
        #   output.graphic[[gname]]$tbl.os <- km.os.df
        #   km.graphic <- func.survivalplot(km.os.df,'','OS',time.break = time_break_survival(max(km.os.df$times,na.rm = T)),
        #                              tbl.loc = 'topright',
        #                              color.dis = input.color,y.title = 'Overall Survival')
        #   output.graphic[[gname]]$km.os <- km.graphic
        #   km.os.plotly <-  plotly_survival(km.os.df,'',time.break = time_break_survival(max(km.os.df$times,na.rm = T)),color.dis = input.color,y.title = 'Overall Survival')
        #   # output[[paste0(gname, "_sur_os")]] <- renderPlot({
        #   #   km.graphic
        #   # })
        #   output[[paste0(gname, "_sur_os")]] <- renderPlotly({
        #     ggplotly(km.os.plotly[[1]])
        #   })
        #   output[[paste0(gname, "_tbl_os")]] <- renderDataTable({
        #     datatable(km.os.df, style = 'bootstrap', rownames = FALSE, options = list(orderClasses = TRUE,dom = 'tp', language = list(
        #       zeroRecords = "No records found matching your selection")), 
        #       colnames = c("Sample_ID", "Times", "Status", gname))
        #   })
        #   output[[paste0(gname, "_dl_os")]] <- downloadHandler(
        #     filename = function() {
        #       if(grepl("#",gname)) gname <- "Comb"
        #       paste("km_os_",gname,"_",Sys.Date(),".pdf", sep = "")
        #     },
        #     content = function(file) {
        #       pdf(file,onefile = F, width = 6, height = 4, pointsize = 10)
        #       print(output.graphic[[gname]]$km.os)
        #       dev.off()
        #     }
        #   )
        #   output[[paste0(gname, "_dltbl_os")]] <- downloadHandler(
        #     filename = function() {
        #       paste("data_os_",gname,"_",Sys.Date(), '.txt', sep='')
        #     },
        #     content = function(file) {
        #       readr::write_delim(x = output.graphic[[gname]]$tbl.os, path = file, delim = "\t")
        #     }
        #   )
        # }
        # if("dfs_months" %in% colnames(clc.cli)) {
        #   km.dfs.df <- clc.cli %>%
        #     dplyr::select(sample_id,times = dfs_months,status = dfs_status,risk)%>%
        #     dplyr::mutate(times = round(as.numeric(times),2)) %>%
        #     dplyr::filter(!is.na(times),status != '')
        #   output.graphic[[gname]]$tbl.dfs <- km.dfs.df
        #   km.graphic <- func.survivalplot(km.dfs.df,'','DFS',time.break = time_break_survival(max(km.dfs.df$times,na.rm = T)),
        #                              tbl.loc = 'topright',
        #                              color.dis = input.color,y.title = 'Disease-free Survival')
        #   output.graphic[[gname]]$km.dfs <- km.graphic
        #   km.dfs.plotly <-  plotly_survival(km.dfs.df,'',time.break = time_break_survival(max(km.dfs.df$times,na.rm = T)),color.dis = input.color,y.title = 'Progression-free Survival')
        #   output[[paste0(gname, "_sur_dfs")]] <- renderPlotly({
        #     ggplotly(km.dfs.plotly[[1]])
        #   })
        #   output[[paste0(gname, "_tbl_dfs")]] <- renderDataTable({
        #     datatable(km.dfs.df, style = 'bootstrap', rownames = FALSE, options = list(orderClasses = TRUE,dom = 'tp', language = list(
        #       zeroRecords = "No records found matching your selection")), 
        #       colnames = c("Sample_ID", "Times", "Status", gname))
        #   })
        #   output[[paste0(gname, "_dl_dfs")]] <- downloadHandler(
        #     filename = function() {
        #       if(grepl("#",gname)) gname <- "Comb"
        #       paste("km_dfs_",gname,"_", Sys.Date(),".pdf", sep = "")
        #     },
        #     content = function(file) {
        #       pdf(file,onefile = F, width = 6, height = 4, pointsize = 10)
        #       print(output.graphic[[gname]]$km.dfs)
        #       dev.off()
        #     }
        #   )
        #   output[[paste0(gname, "_dltbl_dfs")]] <- downloadHandler(
        #     filename = function() {
        #       paste('data_dfs_',gname,"_", Sys.Date(), '.txt', sep='')
        #     },
        #     content = function(file) {
        #       readr::write_delim(x = output.graphic[[gname]]$tbl.dfs, path = file, delim = "\t")
        #     }
        #   )
        # }
        
        
      })
      remove_modal_progress()
      # Pause for 0.1 seconds to simulate a long computation.
    #   Sys.sleep(0.1)
    # })
  })
  
  
}
