#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : tab.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-04-19
# @IDE     : RStudio
# @Desc    : 第二个Tab功能：tumor immunogenicity: tmb,neo,genomic scar (hrd),scna et al
#===============================================================================

source("./r/func.ggboxplot.R")
source("./r/func.beeswarmplot.R")
source("./r/func.scatterbwplot.R")


tab_02_tumor_immunogenicity <- list()
cat_prefix_img <- 'Immunogenicity'
sidebar <- sidebarPanel(
  id = 'immunogenicity_sidebar',
  width = 3,
  h3("Immunogenicity Analyses"),
  # 研究类型
  awesomeRadio(inputId = "immunogenicity_study_type",
               label = "Select study types", 
               choices = c("Immunogenomics Studies", "Non-immunogenomics Studies"),
               selected = "Immunogenomics Studies",
               # inline = TRUE,
               status = "success"),
  # 肿瘤
  conditionalPanel('input.immunogenicity_study_type != ""',
                   pickerInput(inputId = "immunogenicity_cancer_detail",
                               label = "Select one cancer type, E.g. 'LUAD'",
                               choices = NULL,
                               multiple = FALSE,
                               selected = NULL)),
  # 肿瘤队列
  conditionalPanel('input.immunogenicity_cancer_detail != ""',
                   pickerInput(inputId = "immunogenicity_study_id",
                               label = "Select one dataset, E.g. 'Hellmann_2018'",
                               choices = NULL,
                               multiple = FALSE,
                               selected = NULL)),
  conditionalPanel('input.immunogenicity_study_id != ""',
                   # 队列数据类型：Mutation,RNA,Protein,Methyl
                   pickerInput("immunogenicity_data_type",
                               label = "Select data type, E.g. 'Mutation'", 
                               choices = NULL,
                               selected = NULL, 
                               multiple = FALSE),
                   # 队列数据的基因list
                   selectizeInput(inputId = "immunogenicity_symbol_id", 
                                  label = "Select/Input gene symbols, E.g. 'EGFR STK11'", 
                                  choices = NULL, 
                                  width = '100%',
                                  multiple = T, 
                                  options = list(delimiter = " ", create = T,
                                                 maxItems = 10,
                                                 placeholder = "No more than 10 genes.",
                                                 plugins = list('remove_button', 'drag_drop')))),
  conditionalPanel("input.immunogenicity_data_type == 'Mutation'",
                   # 队列数据的基因突变类型
                   pickerInput(inputId = "immunogenicity_vartype_id",
                               label = tags$span(
                                  add_prompt(tags$span(icon(name = "question-circle")),
                                             message = "Variant Classification for Mutation data", 
                                             position = "right"),
                                 "Select gene mutation types"),
                               choices = NULL,
                               multiple = TRUE,
                               selected = NULL)),
  conditionalPanel(condition = "input.immunogenicity_data_type == 'Expr' || input.immunogenicity_data_type == 'Proteome'",
                   # 队列数据的基因表达阈值
                   fluidRow(
                     column(6,numericInput(inputId = "immunogenicity_exprcut1_id",
                                           label = tags$span(
                                             add_prompt(tags$span(icon(name = "question-circle")),
                                                        message = "Percentile threshold for RNA data", 
                                                        position = "right"),
                                             "Percentile cutoff (%,High)"),
                                           value = 50,
                                           min = 0,
                                           max = 100,step = NA,width = '100%')),
                     column(5,numericInput(inputId = "immunogenicity_exprcut2_id",
                                           label = "",
                                           value = 50,
                                           min = 0,
                                           max = 100,step = NA,width = '100%'))
                   )),
  # 分组颜色
  fluidRow(
    column(6,colourpicker::colourInput(inputId = "immunogenicity_colorg1_id", 
                                       label = tags$span(
                                          add_prompt(tags$span(icon(name = "question-circle")),
                                                     message = "Color for Mut/High group", 
                                                     position = "right"),
                                         'Color 1'), 
                                       value = "#FF9800", 
                                       showColour = c("both", "text","background"), 
                                       palette = c("square", "limited"), 
                                       allowTransparent = FALSE, 
                                       returnName = TRUE)),
    column(5,colourpicker::colourInput(inputId = "immunogenicity_colorg2_id", 
                                       label = tags$span(
                                         add_prompt(tags$span(icon(name = "question-circle")),
                                                    message = "Color for WT/Low group", 
                                                    position = "right"),
                                         'Color 2'),  
                                       value = "#2196F3", 
                                       showColour = c("both", "text","background"), 
                                       palette = c("square", "limited"), 
                                       allowTransparent = FALSE, 
                                       returnName = TRUE))
    ),
  h5(HTML(paste("<span style=color:black;font-size:14px;>", "Tips: The results of TMB, neoantigen, tumor_purity, ploidy, wGII, ITH,and PD-L1 protein will be shown if available.", "</span>"))),
  
  # 提交按钮
  actionButton(inputId = "immunogenicity_goButton",
               label = "Submit",
               class ="btn-primary"
  )
  # actionButton('immunogenicity_reset','Reset',class ="primary")
  
)

mainpage <- mainPanel(
  id = 'immunogenicity_mainpage',
  width = 9,
  uiOutput(outputId='immunogenicity_maintabs')
)

tab_02_tumor_immunogenicity$ui <- sidebarLayout(sidebar, mainpage)

tab_02_tumor_immunogenicity$server <- function(input, output,session) {
  cat("========================= Start Immunogenicity ======================\n")
  
  # 定义变量存储输出对象
  output.graphic <- reactiveValues()
  
  # 0.根据队列类型（ICI或非ICI队列）从数据库获取肿瘤名称名称
  tumor.names.df <- eventReactive(input$immunogenicity_study_type,{
    study_type <- input$immunogenicity_study_type
    # 要求genicity_property有值
    temp.df <- study.df %>%
      dplyr::filter(genicity_property != '',
                    !is.na(genicity_property))
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
    updatePickerInput(session = session, inputId = "immunogenicity_cancer_detail",
                      choices = unique(tumor.names.df()$tumor_detail), selected = NULL)
  })
  
  # 1.根据肿瘤名称和队列类型（ICI或非ICI队列）从数据库获取队列名称
  study.names.df <- eventReactive(c(input$immunogenicity_cancer_detail),{
    study_type <- input$immunogenicity_study_type
    cancer <- input$immunogenicity_cancer_detail
    # 要求genicity_property有值
    temp.df <- study.df %>%
      dplyr::filter(tumor_detail == cancer) %>%
      dplyr::filter(genicity_property != '',
                    !is.na(genicity_property))
    
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
  # 将获取到的肿瘤队列list更新到页面
  observe({
    updatePickerInput(session = session, inputId = "immunogenicity_study_id",
                      choices = unique(study.names.df()$study_id), selected = NULL)
  })
  # 2. 获取数据类型
  observeEvent(c(input$immunogenicity_study_id),{
    # 页面缓冲显示
    show_modal_spinner(spin = "orbit",color = "#6bba8b",text = "Loading data...")
    imm_in_study_type_id <- case_when(input$immunogenicity_study_type  == 'Immunogenomics Studies' ~ 'ICIs',
                                      TRUE  ~ 'nonICIs')
    
    imm_in_cancer_id <- input$immunogenicity_cancer_detail 
    imm_in_study_id <- input$immunogenicity_study_id
    cat(cat_prefix_img,"-前端返回的队列名称: ",imm_in_study_id,"\n")
    
    # 本次过滤后应只有一条记录
    temp.df <- study.df %>%
      dplyr::filter(study_type %in% imm_in_study_type_id) %>%
      dplyr::filter(tumor_detail %in% imm_in_cancer_id) %>%
      dplyr::filter(study_id %in% imm_in_study_id) 
    data.types <- unique(unlist(str_split(temp.df$data_type[1],"/")))
    # return(data.types)
    # 更新数据类型
    updatePickerInput(session = session, inputId = "immunogenicity_data_type",
                      choices = data.types, 
                      selected = data.types[1])
    
    db.tmp.dat <- data.frame()
    if(data.types[1] == 'Mutation'){
      tbl.name <- paste0(tolower(imm_in_study_id),'_mutation')
      db.dat.dna <- queryDataFromMySQL(tbl.name)
      db.tmp.dat <- db.dat.dna
    }
    if(data.types[1] == 'Expr'){
      tbl.name <- paste0(tolower(imm_in_study_id),'_expr')
      db.dat.rna <- queryDataFromMySQL(tbl.name)
      db.dat.rna$variant_classification <- ''
      db.tmp.dat <- db.dat.rna
    }
    if(data.types[1] == 'Proteome'){
      tbl.name <- paste0(tolower(imm_in_study_id),'_proteome')
      db.dat.rna <- queryDataFromMySQL(tbl.name)
      db.dat.rna$variant_classification <- ''
      db.tmp.dat <- db.dat.rna
    }
    # 更新基因名
    updateSelectizeInput(session = session,
                         inputId = "immunogenicity_symbol_id",
                         choices = unique(db.tmp.dat$hugo_symbol),
                         selected = NULL,
                         server = TRUE)
    # 更新突变类型
    updatePickerInput(session = session, inputId = "immunogenicity_vartype_id",
                      # label = "Select/deselect all options",
                      choices = unique(db.tmp.dat$variant_classification), 
                      selected = unique(db.tmp.dat$variant_classification),
                      options = list(`actions-box` = TRUE))
    remove_modal_spinner()  # 搜索结束后消除缓冲显示
  })
  
  # 3. 根据队列名称从数据获取队列内的基因、表达、突变、临床信息
  observeEvent(c(input$immunogenicity_data_type),{
    
    study <- input$immunogenicity_study_id
    data_type = input$immunogenicity_data_type
    cat(cat_prefix_img,"-前端返回的数据类型: ",data_type,"\n")
    
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
                         inputId = "immunogenicity_symbol_id",
                         choices = unique(db.tmp.dat$hugo_symbol),
                         selected = NULL,
                         server = TRUE)
    # 更新突变类型
    updatePickerInput(session = session, inputId = "immunogenicity_vartype_id",
                      # label = "Select/deselect all options",
                      choices = unique(db.tmp.dat$variant_classification), 
                      selected = unique(db.tmp.dat$variant_classification),
                      options = list(`actions-box` = TRUE))
  })

  # 4. 基因表达分组cutoff更新联动
  observe({
    number1 <- input$immunogenicity_exprcut1_id  # value 1
    updateNumericInput(session, inputId = "immunogenicity_exprcut2_id",
                       label = "Cutoff (%,Low):",
                       value = 100-number1,
                       min = 0, max = 100, step = NA)
  })
  # 若需要cutoff1与cutoff2联动，则取消以下注释；本处为方便设置不同的cutoff，因此不对cutoff1联动
  # observe({
  #   number2 <- input$immunogenicity_exprcut2_id  # value 1
  #   updateNumericInput(session, inputId = "immunogenicity_exprcut1_id",
  #                      label = "Cutoff (High):",
  #                      value = 100-number2,
  #                      min = 0, max = 100, step = NA)
  # })  
  
  # 重置输入
  # observeEvent(input$immunogenicity_reset, {
  #   shinyjs::reset('immunogenicity_sidebar')
  #   shinyjs::reset('immunogenicity_mainpage')
  # })
  # 从DB获取数据
  # 临床信息
  db.dat.cli <- eventReactive(input$immunogenicity_study_id,{
    cat(cat_prefix_img,"-查询Clinical，待分析的研究队列: ",input$immunogenicity_study_id,"\n")
    study <- input$immunogenicity_study_id
    cancer_id <- input$immunogenicity_cancer_detail # cancer_detail
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
  
  db.dat <- eventReactive(c(input$immunogenicity_study_id,
                            input$immunogenicity_data_type),{
                              cat(cat_prefix_img,"-查询Mutation/Expr，待分析的研究队列: ",input$immunogenicity_study_id,"\n")
                              data_type <- input$immunogenicity_data_type
                              study <- input$immunogenicity_study_id
                              tbl_name <- case_when(data_type == 'Mutation' ~ paste0(tolower(study),'_mutation'),
                                                    data_type == 'Expr' ~ paste0(tolower(study),'_expr'),
                                                    data_type == 'Proteome' ~ paste0(tolower(study),'_proteome'),
                                                    TRUE ~ '')
                              temp.df <- queryDataFromMySQL(tbl_name)
                              return(temp.df)
                            })
  
  # 业务层：KM分析
  observeEvent(input$immunogenicity_goButton,{
    cat("===================== Server Immunogenicity =======================\n")
    show_modal_progress_line(
      color = "#00A064", # DF0101
      trail_color = "#eee",
      duration = 90,
      easing = "easeOut",
      text = "Starting Immunogenicity Computation..."
    )
    selected_study_id = input$immunogenicity_study_id
    selected_tumor_detail_id = input$immunogenicity_cancer_detail
    cat(cat_prefix_img,"-选择的研究队列: ", selected_study_id, "\n")
    
    data_type <- input$immunogenicity_data_type
    cat(cat_prefix_img,"-选择的队列数据类型: ", data_type, "\n")
    
    # validate(need(try(input$immunogenicity_symbol_id),"Please select at least one gene!"))
    symbol <- input$immunogenicity_symbol_id
    cat(cat_prefix_img,"-选择的基因名称: ", symbol, "\n")
    
    vartype <- input$immunogenicity_vartype_id
    cat(cat_prefix_img,"-选择的基因突变类型: ", vartype, "\n")
    
    exprcut1 <- input$immunogenicity_exprcut1_id
    exprcut2 <- input$immunogenicity_exprcut2_id
    cat(cat_prefix_img,"-选择的基因高表达分位数: ", exprcut1, "\n")
    cat(cat_prefix_img,"-选择的基因低表达分位数: ", exprcut2, "\n")
    
    colorg1 <- input$immunogenicity_colorg1_id
    colorg2 <- input$immunogenicity_colorg2_id
    cat(cat_prefix_img,"-选择的样本分组1颜色: ", colorg1, "\n")
    cat(cat_prefix_img,"-选择的样本分组2颜色: ", colorg2, "\n")
    
    # 数据清理、绘图、输出
    input.genes <- symbol 
    if(data_type == 'Mutation'){
      # input.genes <- c(input.genes,"All Queries Genes")
      if(length(input.genes) >1){
        input.genes <- c(input.genes,"All Queries Genes")
      }
    }
    input.vartype <- vartype
    input.exprcut1 <- exprcut1/100  #转化为分位数函数的输入值
    input.exprcut2 <- exprcut2/100
    # input.color <- c(colorg1,colorg2)
    
    # 根据队列名称确定待分析免疫原性指标（事先存储于总表）
    temp.df <- study.df %>%
      dplyr::filter(study_id == selected_study_id,
                    tumor_detail == selected_tumor_detail_id) 
    temp.immunogenicities <- unlist(strsplit(temp.df$genicity_property,split = "#",fixed = F))
    # 在study_info中，PD-L1标识为pdl1，在每个队列中，pdl1_exp是连续性变量，pdl1是分组变量
    if('pdl1' %in% temp.immunogenicities){ 
      temp.immunogenicities <- gsub("pdl1",'pdl1_exp',temp.immunogenicities)
      }
    
    # 写出到页面
    output$immunogenicity_maintabs <- renderUI({
      tabs <- lapply(input.genes, function(name) {
        tabPanel(
          title = name,
          uiOutput(paste0(name,"_imgen"))
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
        if(gname == 'All Queries Genes') {symbol.name <- input.genes} # EGFR#TP53#ALK
        
        update_modal_progress(
          value = i / n,
          text =  paste("Analyzing gene:", gname,sprintf("(%1.0f%%)", i/n*100)) #paste("Analyzing gene: ", gname)
        )
        
        if(data_type == 'Mutation'){
          # 临床样本与突变样本取交集
          tumor.samples <- intersect(unique(db.dat()$sample_id),db.dat.cli()$sample_id)
          grp1.df <- db.dat() %>%
            dplyr::filter(hugo_symbol %in% symbol.name,
                          variant_classification %in% input.vartype) %>%
            dplyr::select(sample_id,hugo_symbol)
          clc.cli <- db.dat.cli() %>%
            dplyr::filter(sample_id %in% tumor.samples) %>%
            dplyr::mutate(group = ifelse(sample_id %in% unique(grp1.df$sample_id),'Mut','Wt')) %>%
            # 本处给突变类型时，初始化为一个很大的值，但后续绘图时，突变数据只绘制蜂群图，不绘制相关系数图
            dplyr::mutate(sltgene = 100000) %>%
            dplyr::select(sample_id,group,sltgene,all_of(temp.immunogenicities))
          
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
            dplyr::mutate(group = ifelse(sltgene >= cutoff_h,"High",
                                         ifelse(sltgene <= cutoff_l,'Low','Middle'))) %>%
            dplyr::filter(group %in% c('Low','High')) %>%
            dplyr::select(sample_id,group,sltgene)
          # grp1.df$group <- factor(grp1.df$group,levels = c('Low','High'))
          clc.cli <- db.dat.cli() %>%
            dplyr::filter(sample_id %in% tumor.samples) %>%
            dplyr::inner_join(grp1.df,by='sample_id')  %>%
            dplyr::select(sample_id,group,sltgene,all_of(temp.immunogenicities))
        }
        
        # 根据临床属性定义输出的tab项
        cli.tab.names <- temp.immunogenicities
        cli.tab.names <- cli.tab.names[which(cli.tab.names != '')]
        cat(cat_prefix_img,"-待分析的免疫原性指标: ",cli.tab.names,"\n")
        # print(clc.cli[1:5,])
        
        
        output[[paste0(gname,"_imgen")]] <- renderUI({
          tabss <- lapply(cli.tab.names, function(cli.name) {
            tabPanel(
              style = "padding-top:15px",
              title = paste0("",toupper(cli.name)), status = "primary", solidHeader = TRUE, collapsible = TRUE,
              # plotlyOutput(paste0(gname, "_plot_img_",tolower(cli.name)), height = "350px", width = "100%"),
              # plotOutput(paste0(gname, "_plot_img_",tolower(cli.name)), height = "350px", width = "100%"),
              shinycssloaders::withSpinner(plotOutput(paste0(gname, "_plot_img_",tolower(cli.name)), height = "350px", width = "100%")),
              h5(HTML(paste("<span style=color:black;font-size:12px;>", "All Queries Genes: mutation of at least one of query genes. Mut: mutation,Wt: wild-type.", "</span>"))),
              
              downloadButton(paste0(gname, "_dl_plot_img_",tolower(cli.name)), 
                             # "DLGraph",
                             tags$span(
                               "DLGraph",
                               add_prompt(tags$span(icon(name = "question-circle")),
                                          message = "Save plot in a PDF file.", 
                                          position = "left")),
                             style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
              downloadButton(paste0(gname, "_dl_tbl_img_",tolower(cli.name)), 
                             # "DLTable",
                             tags$span(
                               "DLTable",
                               add_prompt(tags$span(icon(name = "question-circle")),
                                          message = "Save data of beeswarm plot in a txt file.", 
                                          position = "left")),
                             style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
              br(),
              DT::dataTableOutput(paste0(gname, "_tbl_img_",tolower(cli.name)))
              # dt_output("",paste0(gname, "_tbl_",tolower(cli.name)))
            )
          })
          do.call(tabsetPanel, tabss)
        })
        
        # 绘Boxplot图
        lapply(cli.tab.names, function(cli.name) {
          
          # cli.tab.names 为不同的变异原性指标：TMB，neoantigen,PDL1，tumor_purity等均为
          # print(clc.cli[1:20,])
          km.df <- clc.cli %>%
            dplyr::mutate(row_id = seq(1,nrow(clc.cli))) %>%
            dplyr::select(row_id,sample_id,img_name = cli.name,group,sltgene)%>%
            dplyr::filter(!is.na(img_name))  %>% # 删除NA的记录
            dplyr::mutate(img_name = as.numeric(img_name))
          
          output.graphic[[paste0(gname,"_",tolower(cli.name))]]$tbl <- km.df
          # print(km.df)
          km.graphic <- func.scatterbwplot(km.df,cli.name,gname,colorg2,colorg1)
          
          output.graphic[[paste0(gname,"_",tolower(cli.name))]]$km <- km.graphic
          
          # output[[paste0(gname, "_plot_img_",tolower(cli.name))]] <- renderPlotly({
          #   ggplotly(km.graphic)
          # })
          output[[paste0(gname, "_plot_img_",tolower(cli.name))]] <- renderPlot({
            km.graphic
          })
          output[[paste0(gname, "_tbl_img_",tolower(cli.name))]] <- renderDataTable({
            if("high" %in% tolower(km.df$group)){
              out.df <- km.df
              datatable(out.df, style = 'bootstrap', rownames = FALSE,
                        options = list(orderClasses = TRUE,dom = 'tp', language = list(
                          zeroRecords = "No records found matching your selection"),
                          columnDefs = list(list(className = 'dt-center', targets = 0:4))),
                        colnames = c("Row_ID","Sample_ID", toupper(cli.name), paste0(gname,"_Group") ,gname))
            }else{
              out.df <- km.df[,c(1,2,3,4)]
              datatable(out.df, style = 'bootstrap', rownames = FALSE,
                        options = list(orderClasses = TRUE,dom = 'tp', language = list(
                          zeroRecords = "No records found matching your selection"),
                          columnDefs = list(list(className = 'dt-center', targets = 0:3))),
                        colnames = c("Row_ID","Sample_ID", toupper(cli.name), gname))
            }
          })
          # output[[paste0(gname, "_tbl_img_",tolower(cli.name))]]  <- render_dt(km.df, 'cell', FALSE)
          output[[paste0(gname, "_dl_plot_img_",tolower(cli.name))]] <- downloadHandler(
            filename = function() {
              if(grepl("#",gname)) gname <- "Comb"
              paste0(prefix_output_file,"_Immunnogenicity_Plot_",toupper(cli.name),"_",gname,"_",Sys.Date(),".pdf", sep = "")
            },
            content = function(file) {
              pdf(file,onefile = F, width = 6, height = 4, pointsize = 10)
              print(output.graphic[[paste0(gname,"_",tolower(cli.name))]]$km)
              dev.off()
            }
          )
          output[[paste0(gname, "_dl_tbl_img_",tolower(cli.name))]] <- downloadHandler(
            filename = function() {
              paste0(prefix_output_file,"_Immunnogenicity_Data_",toupper(cli.name),"_",gname,"_",Sys.Date(), '.txt', sep='')
            },
            content = function(file) {
              readr::write_delim(x = output.graphic[[paste0(gname,"_",tolower(cli.name))]]$tbl, path = file, delim = "\t")
            }
          )
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
