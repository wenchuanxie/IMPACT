#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : tab.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-05-06
# @IDE     : RStudio
# @Desc    : 第3个Tab之3.2功能：肿瘤免疫浸润比较分析：基于基因表达、细胞集ssGSEAscore,只针对mRNA数据进行分析
#===============================================================================

source("./r/func.bacthwilcoxtest.R")

tab_03_tme_comp <- list()
cat_prefix_tme_comp <- 'TME-Comp'
sidebar <- sidebarPanel(
  id = 'comp_sidebar',
  width = 3,
  h3("TME Comparative Analysis"),
  # 研究类型
  awesomeRadio(inputId = "comp_study_type",
               label = "Select study types", 
               choices = c("Immunogenomics Studies", "Non-immunogenomics Studies"),
               selected = "Immunogenomics Studies",
               # inline = TRUE,
               status = "success"),
  # 肿瘤
  conditionalPanel('input.comp_study_type != ""',
                   pickerInput(inputId = "comp_cancer_detail",
                               label = "Select one cancer type, E.g. 'LUAD'",
                               choices = NULL,
                               multiple = FALSE,
                               selected = NULL)),
  # 肿瘤队列(只输出有表达谱（或者Protein/Methy等）的)
  conditionalPanel('input.comp_cancer_detail != ""',
                   pickerInput(inputId = "comp_study_id",
                               label = "Select one dataset, E.g. 'Hellmann_2018'",
                               choices = NULL,
                               multiple = FALSE,
                               selected = NULL)),
  tags$hr(style="border-color: purple;"),
  conditionalPanel('input.comp_study_id != ""',
                   # 队列数据的基因list
                   selectizeInput(inputId = "comp_symbol_id", 
                                  label = "Select/Input gene symbol, E.g. 'EGFR STK11'", 
                                  choices = NULL, 
                                  width = '100%',
                                  multiple = T, 
                                  options = list(delimiter = " ", create = T,
                                                 maxItems = 10,
                                                 placeholder = "No more than 10 genes.",
                                                 plugins = list('remove_button', 'drag_drop')))),
  # 队列数据的基因表达阈值
  fluidRow(
    column(6,numericInput(inputId = "comp_exprcut1_id",
                          label = tags$span(
                            add_prompt(tags$span(icon(name = "circle-question")),
                                       message = "Percentile threshold for RNA data", 
                                       position = "right"),
                            "Percentile cutoff (%,High):"),
                          value = 50,
                          min = 0,
                          max = 100,step = NA,width = '100%')),
    column(5,numericInput(inputId = "comp_exprcut2_id",
                          label = "",
                          value = 50,
                          min = 0,
                          max = 100,step = NA,width = '100%'))
  ),
  # 基因类型：RNA,Protein,Methyl
  pickerInput("comp_gene_type",
              label = 'Immunomodulator/Signature Type:', 
              choices = NULL,
              selected = NULL, 
              multiple = FALSE),
  
  # 分组颜色
  fluidRow(
    column(6,colourpicker::colourInput(inputId = "comp_colorg1_id", 
                                       label = tags$span(
                                         add_prompt(tags$span(icon(name = "circle-question")),
                                                    message = "Color for positive correlation", 
                                                    position = "right"),
                                         'Color 1'), 
                                       value = "#FF9800", 
                                       showColour = c("both", "text","background"), 
                                       palette = c("square", "limited"), 
                                       # allowedCols,
                                       allowTransparent = FALSE, 
                                       # transparentText, 
                                       returnName = TRUE)),
    column(5,colourpicker::colourInput(inputId = "comp_colorg2_id", 
                                       label = tags$span(
                                         add_prompt(tags$span(icon(name = "circle-question")),
                                                    message = "Color for nagative correlation", 
                                                    position = "right"),
                                         'Color 2'),  
                                       value = "#2196F3", 
                                       showColour = c("both", "text","background"), 
                                       palette = c("square", "limited"), 
                                       # allowedCols,
                                       allowTransparent = FALSE, 
                                       # transparentText, 
                                       returnName = TRUE))
    ),
  # 提交按钮
  # actionButton(inputId = "comp_goButton",
  #              label = "Submit",
  #              class ="btn-primary")
  fluidRow(
    column(2, 
           div(style="display:inline-block",actionButton(inputId = "comp_goButton",label = "Submit",class ="btn-primary"), style="float:left"),
           
    ),
    column(7),
    column(2, 
           div(style="display:inline-block",actionButton("reset_input_comp", "Clear",class="btn-warning"), style="float:left"),
           
    )
  )
  
  
)

mainpage <- mainPanel(
  id = 'comp_mainpage',
  width = 9,
  uiOutput(outputId='comp_maintabs')
)

tab_03_tme_comp$ui <- sidebarLayout(sidebar, mainpage)

tab_03_tme_comp$server <- function(input, output,session) {
  cat("========================= Start Comparative ======================\n")
  # 定义变量存储输出对象
  output.graphic <- reactiveValues()
  
  observeEvent(input[['reset_input_comp']], {
    shinyjs::reset("comp_sidebar")
  })
  observeEvent(input$reset_input_comp, {
    output[['comp_maintabs']] <- NULL
  })
  
  # 0.根据队列类型（ICI或非ICI队列）从数据库获取肿瘤名称名称
  tumor.names.df <- eventReactive(input$comp_study_type,{
    study_type <- input$comp_study_type
    # 要求genicity_property有值
    temp.df <- study.df %>%
      # dplyr::filter(genicity_property != '',
      #               !is.na(genicity_property))%>%
      # 过滤出有Expr的队列
      dplyr::filter(grepl("Expr",data_type))
    
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
    updatePickerInput(session = session, inputId = "comp_cancer_detail",
                      choices = unique(tumor.names.df()$tumor_detail), selected = NULL)
  })
  
  # 1.根据肿瘤名称和队列类型（ICI或非ICI队列）从数据库获取队列名称
  study.names.df <- eventReactive(c(input$comp_cancer_detail),{
    study_type <- input$comp_study_type
    cancer <- input$comp_cancer_detail
    # 要求genicity_property有值
    temp.df <- study.df %>%
      dplyr::filter(tumor_detail == cancer) %>%
      # dplyr::filter(genicity_property != '',
      #               !is.na(genicity_property)) %>%
      # 过滤出有Expr的队列
      dplyr::filter(grepl("Expr",data_type))
    
    if (study_type == 'Immunogenomics Studies'){
      temp.df <- temp.df %>%
        dplyr::filter(study_type == 'ICIs')
    }else{
      temp.df <- temp.df %>%
        dplyr::filter(study_type == 'nonICIs') #%>%
        # dplyr::filter(study_id != 'TCGA') %>%
        # dplyr::mutate(study_id = ifelse(study_id == 'TCGA',paste0(study_id,"_",tumor_detail),study_id))
      
    }
    # print(temp.df)
    
    temp.df
  })
  # 将获取到的肿瘤队列list更新到页面
  observe({
    updatePickerInput(session = session, inputId = "comp_study_id",
                      choices = unique(study.names.df()$study_id), selected = NULL)
  })
  # 2. 获取数据类型，只显示Expr等表达谱数据（目前只有RNA，以后增加Protein等）
  data.types.df <- eventReactive(c(input$comp_study_id),{
    temp_id <- input$comp_study_type 
    slt_study_type_id <- case_when(temp_id == 'Immunogenomics Studies' ~ 'ICIs',
                                   TRUE  ~ 'nonICIs')
    
    slt_cancer_id <- input$comp_cancer_detail 
    
    slt_study_id <- input$comp_study_id
    # if(grepl("TCGA_",slt_study_id)) slt_study_id <- "TCGA"
    # 本次过滤后应只有一条记录
    temp.df <- study.df %>%
      dplyr::filter(study_type %in% slt_study_type_id) %>%
      dplyr::filter(tumor_detail %in% slt_cancer_id) %>%
      dplyr::filter(study_id %in% slt_study_id) 
    # return(temp.df)
    
    data.types <- unique(unlist(str_split(temp.df$data_type[1],"/")))
    # 不展示mutation
    data.types <- setdiff(data.types,"Mutation")
    return(data.types)
    
  })
  
  # 将获取到数据类型更新到页面
  # observe({
  #   updatePickerInput(session = session, inputId = "comp_data_type",
  #                     choices = data.types.df(), 
  #                     selected = data.types.df()[1])
  # })
  
  # 3. 根据队列名称从数据获取队列内的基因、表达、突变、临床信息
  study.content.df <- eventReactive(c(input$comp_study_id),{
    
    study <- input$comp_study_id
    cat(cat_prefix_tme_comp,"-前端返回的队列名称: ",study,"\n")
    
    data_type = data.types.df()[1]
    cat(cat_prefix_tme_comp,"-DB返回的数据类型列表: ",data_type,"\n")
    
    db.dat <- data.frame()
    # if(data_type == 'Mutation'){
    #   tbl.name <- paste0(tolower(study),'_mutation')
    #   db.dat.dna <- queryDataFromMySQL(tbl.name)
    #   db.dat <- db.dat.dna
    # }
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
  # 将获取到的内更新到页面
  observe({
    # 更新基因名
    updateSelectizeInput(session = session,
                         inputId = "comp_symbol_id",
                         choices = unique(study.content.df()$hugo_symbol),
                         selected = NULL,
                         server = TRUE)
    
    # 如果是突变，则显示突变类型
    # 更新突变类型
    # var.types <- unique(study.content.df()$variant_classification)
    # updatePickerInput(session = session, inputId = "comp_vartype_id",
    #                   # label = "Select/deselect all options",
    #                   choices = var.types, selected = var.types,
    #                   options = list(
    #                     `actions-box` = TRUE))
  })
  
  # 4. 基因表达分组cutoff更新联动
  observe({
    number1 <- input$comp_exprcut1_id  # value 1
    updateNumericInput(session, inputId = "comp_exprcut2_id",
                       label = "Cutoff (%, Low):",
                       value = 100-number1,
                       min = 0, max = 100, step = NA)
  })
  # 若需要cutoff1与cutoff2联动，则取消以下注释；本处为方便设置不同的cutoff，因此不对cutoff1联动
  # observe({
  #   number2 <- input$comp_exprcut2_id  # value 1
  #   updateNumericInput(session, inputId = "comp_exprcut1_id",
  #                      label = "Cutoff (High):",
  #                      value = 100-number2,
  #                      min = 0, max = 100, step = NA)
  # })  
  
  # 5. 获取TME基因分类
  # 将获取到数据类型更新到页面
  observe({
    # db_choices <- c(tme.gene.corr,tme.cell.corr)
    db_choices.df <- data.frame('sigtype' = c(rep('Gene',length(tme.gene.corr)),rep('Pathway',length(tme.cell.corr))),
                                'sigvalue' = c(tme.gene.corr,tme.cell.corr))
    db_choices <- lapply(split(db_choices.df$sigvalue, db_choices.df$sigtype), as.list)
    updatePickerInput(session = session, inputId = "comp_gene_type",
                      choices = db_choices, 
                      selected = db_choices[[1]][[1]])
  })
  
  observe({
    shinyjs::toggleState("comp_goButton", 
                         !is.null(input$comp_symbol_id) && input$comp_symbol_id != "" )
  })
  
  # 重置输入
  # observeEvent(input$comp_reset, {
  #   shinyjs::reset('comp_sidebar')
  #   shinyjs::reset('comp_mainpage')
  # })
  
  # 从DB获取数据
  # 临床信息
  db.dat.cli <- eventReactive(input$comp_study_id,{
    cat(cat_prefix_tme_corr,"-查询Clinical，待分析的研究队列: ",input$comp_study_id,"\n")
    study <- input$comp_study_id
    cancer_id <- input$comp_cancer_detail # cancer_detail
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
  # 表达谱信息
  db.dat <- eventReactive(c(input$comp_study_id,
                            input$comp_data_type),{
                              cat(cat_prefix_tme_corr,"-查询Expr，待分析的研究队列: ",input$comp_study_id,"\n")
                              # data_type <- input$comp_data_type
                              data_type <- 'Expr' # Hardcode：查询expr表
                              study <- input$comp_study_id
                              tbl_name <- case_when(data_type == 'Mutation' ~ paste0(tolower(study),'_mutation'),
                                                    data_type == 'Expr' ~ paste0(tolower(study),'_expr'),
                                                    data_type == 'Proteome' ~ paste0(tolower(study),'_proteome'),
                                                    TRUE ~ '')
                              temp.df <- queryDataFromMySQL(tbl_name)
                              return(temp.df)
                            })
  
  # 输入为空验证
  iv_comp <- InputValidator$new()
  iv_comp$add_rule("comp_symbol_id", sv_required())
  iv_comp$add_rule("comp_gene_type", sv_required())
  iv_comp$add_rule("comp_exprcut1_id", sv_gt(9))
  iv_comp$add_rule("comp_exprcut1_id", sv_lt(91))
  iv_comp$add_rule("comp_exprcut2_id", sv_gt(9))
  iv_comp$add_rule("comp_exprcut2_id", sv_lt(91))
  iv_comp$enable()
  
  # 业务层：KM分析
  observeEvent(input$comp_goButton,{
    cat("===================== Server Comparative =======================\n")
    show_modal_progress_line(
      color = "#00A064", # DF0101
      trail_color = "#eee",
      duration = 90,
      easing = "easeOut",
      text = "Starting comparative analysis ..."
    )
    selected_study_id = input$comp_study_id
    selected_tumor_detail_id = input$comp_cancer_detail
    cat(cat_prefix_tme_comp,"-选择的研究队列: ", selected_study_id, "\n")
    
    data_type <- input$comp_data_type
    cat(cat_prefix_tme_comp,"-选择的队列数据类型: ", data_type, "\n")
    
    # validate(need(try(input$comp_symbol_id),"Please select at least one gene!"))
    symbol <- input$comp_symbol_id
    cat(cat_prefix_tme_comp,"-选择的基因名称: ", symbol, "\n")
    
    # vartype <- input$comp_vartype_id
    # cat(cat_prefix_tme_comp,"-选择的基因突变类型: ", vartype, "\n")
    
    exprcut1 <- input$comp_exprcut1_id
    exprcut2 <- input$comp_exprcut2_id
    cat(cat_prefix_tme_comp,"-选择的基因高表达分位数: ", exprcut1, "\n")
    cat(cat_prefix_tme_comp,"-选择的基因低表达分位数: ", exprcut2, "\n")
    
    colorg1 <- input$comp_colorg1_id
    colorg2 <- input$comp_colorg2_id
    cat(cat_prefix_tme_comp,"-选择的样本分组1颜色: ", colorg1, "\n")
    cat(cat_prefix_tme_comp,"-选择的样本分组2颜色: ", colorg2, "\n")
    
    genetype <- input$comp_gene_type
    cat(cat_prefix_tme_corr,"-选择的基因/细胞集类型: ", genetype, "\n")
    
    # 数据清理、绘图、输出
    input.genes <- symbol 
    # if(data_type == 'Mutation'){
    #   input.genes <- c(input.genes,"Combined")
    # }
    # input.vartype <- vartype
    input.exprcut1 <- exprcut1/100  #转化为分位数函数的输入值
    input.exprcut2 <- exprcut2/100
    # input.color <- c(colorg1,colorg2)
    
    input.genetype <- genetype
    
    use.ssGSEA = FALSE
    if(input.genetype %in% tme.gene.corr){
      # 根据选择的input.genetype确定分析的基因列表，并与待分析的队列的基因名list取交集
      tme.df <- tme.gene.df %>%
        dplyr::filter(type == input.genetype)
    }else{
      # 根据选择的input.genetype确定分析的细胞列表，准备提取基因，及其表达谱进行ssGSEA分析计算score
      tme.df <- tme.cell.df %>%
        dplyr::filter(main_category == input.genetype)
      use.ssGSEA = TRUE
    }
    
    # 写出到页面
    output$comp_maintabs <- renderUI({
      # 外层Tab：基因
      tabs <- lapply(input.genes, function(name) {
        tabPanel(
          title = name,
          uiOutput(paste0(name,"_comp"))
        )
      })
      do.call(tabsetPanel, tabs)
    })
    # 执行数据分析
    # withProgress(message = 'Making plot', value = 0, {
      # 外层Tab：基因
      n <- length(input.genes)
      lapply(seq(1,length(input.genes)), function(i) {
        
        gname <- input.genes[i]
        symbol.name <- gname
        # cat(cat_prefix_tme_comp,"-基因: ", gname, "分析开始!\n")
        
        update_modal_progress(
          value = i / n,
          text =  paste("Analyzing gene:", gname,sprintf("(%1.0f%%)", i/n*100)) #paste("Analyzing gene: ", gname)
        )
        
        # if(gname == 'Combined') {symbol.name <- input.genes} # EGFR#TP53#ALK
        
        # if(data_type == 'Mutation'){
        #   # 临床样本与突变样本取交集
        #   tumor.samples <- intersect(unique(db.dat()$sample_id),db.dat.cli()$sample_id)
        #   grp1.df <- db.dat() %>%
        #     dplyr::filter(hugo_symbol %in% symbol.name,
        #                   variant_classification %in% input.vartype) %>%
        #     dplyr::select(sample_id,hugo_symbol)
        #   clc.cli <- db.dat.cli() %>%
        #     dplyr::filter(sample_id %in% tumor.samples) %>%
        #     dplyr::mutate(group = ifelse(sample_id %in% unique(grp1.df$sample_id),'Mut','WT')) %>%
        #     # 本处给突变类型时，初始化为一个很大的值，但后续绘图时，突变数据只绘制蜂群图，不绘制相关系数图
        #     dplyr::mutate(sltgene = 100000) %>%
        #     dplyr::select(sample_id,group,sltgene,all_of(temp.immunogenicities))
        #   
        # }
        # for expr
        {
          # symbol.name必须为单个基因的名称
          grp1.df <- db.dat() %>%
            dplyr::filter(hugo_symbol %in% symbol.name) %>%
            tibble::column_to_rownames(var = 'hugo_symbol') %>%
            t() %>% as.data.frame() %>%
            tibble::rownames_to_column(var = 'sample_id')
          colnames(grp1.df)[2] <- "sltgene"
          # browser()
          cutoff_h <- quantile(as.numeric(grp1.df$sltgene),input.exprcut1,na.rm = T)
          cutoff_l <- quantile(as.numeric(grp1.df$sltgene),input.exprcut2,na.rm = T)
          
          quantile.25 <- quantile(as.numeric(grp1.df$sltgene),0.25,na.rm = T)
          quantile.75 <- quantile(as.numeric(grp1.df$sltgene),0.75,na.rm = T)
          if(quantile.25 == quantile.75){
            cat("基因：",symbol.name,"的第一分位数和第三分为数相等,无法以75%以内分位值为阈值。系统将以第一个不同的表达值为阈值分组\n")
            cutoff_h <- unique(sort(as.numeric(grp1.df$sltgene)))[2]
            cutoff_l <- unique(sort(as.numeric(grp1.df$sltgene)))[2]
          }
          cat(cat_prefix_tme_comp,"Cutoff of Expression: ",cutoff_h," (High); ",cutoff_l," (Low)\n")
          
          grp1.df <- grp1.df %>%
            dplyr::mutate(sltgene = as.numeric(sltgene)) %>%
            dplyr::mutate(group = ifelse(sltgene >= cutoff_h,"High",
                                         ifelse(sltgene <= cutoff_l,'Low','Middle'))) %>%
            dplyr::filter(group %in% c('Low','High')) %>%
            dplyr::select(sample_id,group,sltgene) %>%
            dplyr::arrange(group,desc(sltgene))
          # 准备计算Cor的数据
          # 用户选择的基因（single in list）
          dat.x <- grp1.df %>%
            tibble::column_to_rownames(var = 'sample_id') %>%
            dplyr::select(sltgene)
          # 用户选择的基因list类型或者cell signature
          # TME的基因的表达谱
          if(isFALSE(use.ssGSEA)){
            # 直接提取基因的表达谱
            dat.y <- db.dat() %>%
              dplyr::filter(hugo_symbol %in% tme.df$symbol) %>%
              tibble::column_to_rownames(var = 'hugo_symbol') %>%
              t() %>% as.data.frame() 
            legend.title <- 'Expr'
          }else{
            # 提取基因的表达谱，计算ssGSEAscore
            cell.sets <- split(tme.df$gene,       # gene
                               tme.df$short_name) # pathway name
            gene.expr <- db.dat() %>%             # db.dat()行为基因列为样本，
              tibble::column_to_rownames(var = 'hugo_symbol') %>%
              t() %>% 
              scale() %>%  # 转置后将表达谱进行Z-score处理
              t()    
            ssgsea.df <- gsva(expr = as.matrix(gene.expr),
                              gset.idx.list = cell.sets,
                              kcdf = "Gaussian",
                              parallel.sz = 4,
                              method = "ssgsea",
                              mx.diff = TRUE)
            
            dat.y <- ssgsea.df %>% # ssgsea.df：行为细胞，列为样本
              t() %>% as.data.frame() 
            legend.title <- 'ssGSEA score'
          }
          # 计算相关系数
          corr.df <- psych::corr.test(dat.x,dat.y,method = 'spearman',adjust = 'fdr')
          
          # 准备计算组间差异的数据，前三列是sample_id,基因表达的分组变量（group）,基因表达的连续变量
          dat.g <- grp1.df %>%
            dplyr::inner_join(dat.y %>%
                                tibble::rownames_to_column(var = 'sample_id'),by='sample_id')
          # 计算两组间差异
          calc.out <- data.frame()
          if(length(unique(dat.g$group)) > 1){
            calc.out <- do.call(rbind,lapply(colnames(dat.g)[-(1:3)], func.bacthwilcoxtest,dat.g))
          }
        }
        
        output[[paste0(gname,"_comp")]] <- renderUI({
          # 内层Tab，此功能无多个特征分析，因此取消lapply
          # tabss <- lapply(cli.tab.names, function(cli.name) {})
          # do.call(tabsetPanel, tabss)
          cli.name <- 'comp'
          tabPanel(
            style = "padding-top:15px",
            title = paste0("",toupper(cli.name)), status = "primary", solidHeader = TRUE, collapsible = TRUE,
            # plotlyOutput(paste0(gname, "_plot_comp_",tolower(cli.name)), height = "350px", width = "100%"),
            shinycssloaders::withSpinner(plotOutput(paste0(gname, "_plot_comp_",tolower(cli.name)), height = "350px", width = "100%")),
            br(),
            downloadButton(paste0(gname, "_dl_plot_comp_",tolower(cli.name)), 
                           tags$span(
                             "DLGraph",
                             add_prompt(tags$span(icon(name = "circle-question")),
                                        message = "Save plot in a PDF file.", 
                                        position = "left")),
                           style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
            downloadButton(paste0(gname, "_dl_tbl_comp_",tolower(cli.name)), 
                           tags$span(
                             "DLTable",
                             add_prompt(tags$span(icon(name = "circle-question")),
                                        message = "Save data of heatmap plot in a txt file.", 
                                        position = "left")),
                           style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
            br(),
            DT::dataTableOutput(paste0(gname, "_tbl_comp_",tolower(cli.name)))
          )
        })
        
        # 绘heatmap图，# 内层Tab，此功能无多个特征分析，因此取消lapply(或设置为与输出一样的固定tab项，但在页面不显示)
        cli.tab.names <- 'comp'
        lapply(cli.tab.names, function(cli.name) {
          # 提取相关系数、P值及Q值
          # browser()
          corr.long <- func.psychcorrwide2long(corr.df)
          # 组合表达谱、差异比较分析、相关系数绘制热图
          {
            # 基因注释
            # 列注释
            col.ano <- dat.g %>%
              dplyr::select(sample_id, group,sltgene) %>%
              tibble::column_to_rownames(var = 'sample_id')
            # 行注释
            # 先整合比较和相关性结果与基因/ssGSEA矩阵，用于输出表格
            dat.exp <- dat.g %>%
              dplyr::select(-group,-sltgene) %>%
              tibble::column_to_rownames(var = 'sample_id') %>%
              t() %>% as.data.frame()
            dat.all <- corr.long %>%
              # 选择相关性结果：左注释
              dplyr::select(calc_obs = xname,Rho = r,P.value.corr = p,P.adj.corr = padj)
            if(nrow(calc.out) > 0){
              dat.all <- dat.all %>%
                # 合并秩和检验结果：右注释
                dplyr::left_join(dplyr::select(calc.out,calc_obs,P.value.str.comp = p.value.str),by='calc_obs') %>%
                # 合并绘图数据，确定行顺序一致
                dplyr::left_join(dat.exp %>%
                                   tibble::rownames_to_column(var = 'calc_obs'),by='calc_obs') %>%
                dplyr::arrange(desc(Rho))
              # 提取行注释
              row.ano <- dat.all %>%
                dplyr::select(calc_obs,Rho,P.value.corr,P.adj.corr,P.value.str.comp)
            }else{
              dat.all <- dat.all %>%
                dplyr::mutate(P.value.str.comp = '') %>%
                # 合并绘图数据，确定行顺序一致
                dplyr::left_join(dat.exp %>%
                                   tibble::rownames_to_column(var = 'calc_obs'),by='calc_obs') %>%
                dplyr::arrange(desc(Rho))
              
              # 提取行注释
              row.ano <- dat.all %>%
                dplyr::select(calc_obs,Rho,P.value.corr,P.adj.corr,P.value.str.comp) #%>%
                # dplyr::mutate(P.value.str.comp = NA)
            }
            # browser()
            # 绘图
            # heatmap.df <- t(scale(t(dat.exp)))  # 当基因在各个样本下表达值想同时， scale后是NaN
            # Normalize data
            # heatmap.df <- apply(as.matrix(dat.exp), 1, function(x){x/mean(x)}) # 对特征（基因）进行标准化
            heatmap.df <- dat.exp
            col_fun = colorRamp2(c(min(heatmap.df,na.rm = T),0,max(heatmap.df,na.rm = T)), c('blue', "white", "red"))
            vc_cols = c(colorg1,colorg2)
            # browser()
            names(vc_cols) <- levels(dat.g$group)
            lower_upper <- ifelse(abs(min(col.ano$sltgene)) < abs(max(col.ano$sltgene)),
                                  floor(abs(min(col.ano$sltgene))),
                                  ceiling(abs(max(col.ano$sltgene))))
            top_annotation <- HeatmapAnnotation(Group = factor(col.ano$group),
                                                Obs = col.ano$sltgene,
                                                col = list(Group = c("High" = colorg1,"Low" = colorg2),
                                                           Obs = colorRamp2(c(floor(min(col.ano$sltgene)),(min(col.ano$sltgene) + max(col.ano$sltgene))/2 , ceiling(max(col.ano$sltgene))), 
                                                                            c(colorg2,"white", colorg1))),
                                                # annotation_legend_param = list(
                                                #   Group = list(direction = "horizontal",nrow = 1),
                                                #   Obs = list(direction = "horizontal",nrow = 1)),
                                                annotation_label = c("Group",gname),
                                                show_legend = T,
                                                height = unit(2, "cm"),
                                                annotation_name_side = 'right')
            left_annotation <- rowAnnotation(Rho = row.ano$Rho,
                                             Sig = anno_text(round(row.ano$P.adj.corr,4),
                                                             location = 0.5,
                                                             rot = 0,
                                                             just = "center",
                                                             gp = gpar(fontsize = 8)),
                                             col = list(Rho = colorRamp2(c(-0.5,0,0.5), c("#FFC107", "white", "#F44336"))),
                                             # annotation_legend_param = list(
                                             #   Rho = list(direction = "horizontal",nrow =1)
                                             # ),
                                             # annotation_label = c("Type",'Roh','Roh.p'),
                                             show_annotation_name = c(T,T),
                                             show_legend = c(T,T),
                                             annotation_name_side = 'bottom'
            )
            if(nrow(calc.out) == nrow(heatmap.df)){
              p.heatm <- Heatmap(heatmap.df[row.ano$calc_obs,],
                                 # heatmap_legend_param = list(direction = "horizontal"),
                                 top_annotation = top_annotation,
                                 # 左侧加相关性分析热图
                                 left_annotation = left_annotation,
                                 # 右侧加秩和检验P值
                                 right_annotation = rowAnnotation(Sig = anno_text(row.ano$P.value.str.comp,
                                                                                  location = 1,
                                                                                  rot = 0,
                                                                                  just = "right",
                                                                                  gp = gpar(fontsize = 8))),
                                 row_names_side = 'right',
                                 cluster_columns = F,
                                 cluster_rows = F,
                                 use_raster = F, # 样本数超过了2k，所以定义为F
                                 name = legend.title,
                                 # column_split = col.ano$group,
                                 col = col_fun,
                                 row_names_gp = grid::gpar(fontsize = 8),
                                 show_column_names = F,
                                 column_names_gp = grid::gpar(fontsize = 10))
            }else{
              # 一组时，无秩和检验
              p.heatm <- Heatmap(heatmap.df[row.ano$calc_obs,],
                                 # heatmap_legend_param = list(direction = "horizontal"),
                                 top_annotation = top_annotation,
                                 # 左侧加相关性分析热图
                                 left_annotation = left_annotation,
                                 # 只有一组，无秩和检验P值
                                 # right_annotation = rowAnnotation(Sig = anno_text(row.ano$P.value.str.comp,
                                 #                                                  location = 1,
                                 #                                                  rot = 0,
                                 #                                                  just = "right",
                                 #                                                  gp = gpar(fontsize = 8))),
                                 row_names_side = 'right',
                                 cluster_columns = F,
                                 cluster_rows = F,
                                 use_raster = F, # 样本数超过了2k，所以定义为F
                                 name = legend.title,
                                 # column_split = col.ano$group,
                                 col = col_fun,
                                 row_names_gp = grid::gpar(fontsize = 8),
                                 show_column_names = F,
                                 column_names_gp = grid::gpar(fontsize = 10))
            }
            
            
          }
          
          output.graphic[[paste0(gname,"_",tolower(cli.name))]]$tbl <- dat.all
          output.graphic[[paste0(gname,"_",tolower(cli.name))]]$plot <- p.heatm
          
          sketch_cmp = htmltools::withTags(table(
            class = 'display',
            thead(
              tr(
                th(rowspan = 2, 'Gene Symbol'),
                th(colspan = 3, 'Correaltion'),
                th(colspan = 1, 'Wilcoxon test')
              ),
              tr(
                lapply(c("Rho","P value","FDR","P value"), th)
              )
            )
          ))
          
          output[[paste0(gname, "_plot_comp_",tolower(cli.name))]] <- renderPlot({
            draw(p.heatm)
          })
          output[[paste0(gname, "_tbl_comp_",tolower(cli.name))]] <- renderDataTable({
            datatable(dat.all %>%
                        dplyr::select(calc_obs,Rho,P.value.corr,P.adj.corr,P.value.str.comp) %>%
                        dplyr::rename(`Gene Symbol` = calc_obs,
                                      `P value (Rho)` = P.value.corr,
                                      `FDR (Rho)` = P.adj.corr,
                                      `P value (wilcox.test)` = P.value.str.comp), 
                      style = 'bootstrap',
                      container = sketch_cmp,
                      selection = list(mode = "single", target = "row"),
                      rownames = FALSE,
                      options = list(scrollX = TRUE, keys = TRUE,
                                     orderClasses = TRUE,
                                     dom = 'Bfrtip', searching = F,
                                     language = list(zeroRecords = "No records found matching your selection"),
                        columnDefs = list(list(className = 'dt-center', targets = '_all'))))
          })
          output[[paste0(gname, "_dl_plot_comp_",tolower(cli.name))]] <- downloadHandler(
            filename = function() {
              if(grepl("#",gname)) gname <- "Comb"
              paste0(prefix_output_file,"_Comp_Plot_",gname,"_",Sys.Date(),".pdf", sep = "")
            },
            content = function(file) {
              pdf(file,onefile = F, width = 12, height = 5, pointsize = 10)
              draw(output.graphic[[paste0(gname,"_",tolower(cli.name))]]$plot, 
                   heatmap_legend_side = "right", 
                   annotation_legend_side = "right")
              dev.off()
            }
          )
          output[[paste0(gname, "_dl_tbl_comp_",tolower(cli.name))]] <- downloadHandler(
            filename = function() {
              paste0(prefix_output_file,"_Comp_Data_",gname,"_",Sys.Date(), '.txt', sep='')
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
