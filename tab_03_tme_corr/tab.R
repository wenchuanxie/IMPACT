#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : tab.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-04-19
# @IDE     : RStudio
# @Desc    : 第3个Tab之3.1功能：肿瘤免疫浸润相关性分析：基于基因表达、细胞集ssGSEAscore
#===============================================================================

source("./r/func.bubbleplot.R")
source("./r/func.triangleheatmap.R")
source("./r/func.psychcorrwide2long.R")

tab_03_tme_corr <- list()
cat_prefix_tme_corr <- 'TME-Corr'
sidebar <- sidebarPanel(
  id = 'corr_sidebar',
  width = 3,
  h3("TME Correlation Analyses"),
  # 研究类型
  awesomeRadio(inputId = "corr_study_type",
               label = "Select study types", 
               choices = c("Immunogenomics Studies", "Non-immunogenomics Studies"),
               selected = "Immunogenomics Studies",
               # inline = TRUE,
               status = "success"),
  # 肿瘤
  conditionalPanel('input.corr_study_type != ""',
                   pickerInput(inputId = "corr_cancer_detail",
                               label = "Select one cancer type, E.g. 'LUAD'",
                               choices = NULL,
                               multiple = FALSE,
                               selected = NULL)),
  # 肿瘤队列(只输出有表达谱（或者Protein/Methy等）的)
  conditionalPanel('input.corr_cancer_detail != ""',
                   pickerInput(inputId = "corr_study_id",
                               label = "Select one dataset, E.g. 'Hellmann_2018'",
                               choices = NULL,
                               multiple = FALSE,
                               selected = NULL)),
  conditionalPanel('input.corr_study_id != ""',
                   # 队列数据的基因list
                   selectizeInput(inputId = "corr_symbol_id", 
                                  label = "Select/Input gene symbols, E.g. 'EGFR STK11'", 
                                  choices = NULL, 
                                  width = '100%',
                                  multiple = T, 
                                  options = list(delimiter = " ", create = T,
                                                 maxItems = 10,
                                                 placeholder = "No more than 10 genes.",
                                                 plugins = list('remove_button', 'drag_drop')))),
  # 分组颜色
  fluidRow(
    column(6,colourpicker::colourInput(inputId = "corr_colorg1_id", 
                                       label = tags$span(
                                          add_prompt(tags$span(icon(name = "question-circle")),
                                                     message = "Color for positive correlation", 
                                                     position = "right"),
                                         'Color 1'), 
                                       value = "#FF9800", 
                                       showColour = c("both", "text","background"), 
                                       palette = c("square", "limited"), 
                                       allowTransparent = FALSE, 
                                       returnName = TRUE)),
    column(5,colourpicker::colourInput(inputId = "corr_colorg2_id", 
                                       label = tags$span(
                                          add_prompt(tags$span(icon(name = "question-circle")),
                                                     message = "Color for nagative correlation", 
                                                     position = "right"),
                                         'Color 2'),  
                                       value = "#2196F3", 
                                       showColour = c("both", "text","background"), 
                                       palette = c("square", "limited"), 
                                       allowTransparent = FALSE, 
                                       returnName = TRUE))),
  # 基因类型：RNA,Protein,Methyl
  pickerInput("corr_gene_type",
              label = 'Select immune genes or signatures', 
              choices = NULL,
              selected = NULL, 
              multiple = FALSE),
  # 输出的Cor 类型
  awesomeRadio(
    inputId = "corr_type",
    label = "Method:",
    inline = T,
    choices = c(
      "pearson"  = "pearson",
      "spearman" = "spearman",
      "kendall" = "kendall"
    ),
    selected = "spearman",
    status = "success",
    checkbox = TRUE
  ),
  # 输出的图片格式
  awesomeRadio(
    inputId = "corr_figure_type",
    label = "Figure Type:",
    inline = T,
    choices = c(
      "Bubble"  = "bubble",
      "Heatmap" = "theatmap"
    ),
    selected = "bubble",
    status = "success",
    checkbox = TRUE
    ),
  # 提交按钮
  actionButton(inputId = "corr_goButton",
               label = "Submit",
               class ="btn-primary")
  
)

mainpage <- mainPanel(
  id = 'corr_mainpage',
  width = 9,
  uiOutput(outputId='corr_maintabs')
)

tab_03_tme_corr$ui <- sidebarLayout(sidebar, mainpage)

tab_03_tme_corr$server <- function(input, output,session) {
  cat("========================= Start Correaltion  ======================\n")
  
  # 定义变量存储输出对象
  output.graphic <- reactiveValues()
  
  # 0.根据队列类型（ICI或非ICI队列）从数据库获取肿瘤名称名称
  tumor.names.df <- eventReactive(input$corr_study_type,{
    study_type <- input$corr_study_type
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
    updatePickerInput(session = session, inputId = "corr_cancer_detail",
                      choices = unique(tumor.names.df()$tumor_detail), selected = NULL)
  })
  
  # 1.根据肿瘤名称和队列类型（ICI或非ICI队列）从数据库获取队列名称
  study.names.df <- eventReactive(c(input$corr_cancer_detail),{
    study_type <- input$corr_study_type
    cancer <- input$corr_cancer_detail
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
    updatePickerInput(session = session, inputId = "corr_study_id",
                      choices = unique(study.names.df()$study_id), selected = NULL)
  })
  # 2. 获取数据类型，只显示Expr等表达谱数据（目前只有RNA，以后增加Protein等）
  data.types.df <- eventReactive(c(input$corr_study_id),{
    temp_id <- input$corr_study_type 
    slt_study_type_id <- case_when(temp_id == 'Immunogenomics Studies' ~ 'ICIs',
                                   TRUE  ~ 'nonICIs')
    
    slt_cancer_id <- input$corr_cancer_detail 
    
    slt_study_id <- input$corr_study_id
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
  #   updatePickerInput(session = session, inputId = "corr_data_type",
  #                     choices = data.types.df(), 
  #                     selected = data.types.df()[1])
  # })
  
  # 3. 根据队列名称从数据获取队列内的基因、表达、突变、临床信息
  study.content.df <- eventReactive(c(input$corr_study_id),{
    
    study <- input$corr_study_id
    cat(cat_prefix_tme_corr,"-前端返回的队列名称: ",study,"\n")
    
    data_type = data.types.df()[1]
    cat(cat_prefix_tme_corr,"-DB返回的数据类型列表: ",data_type,"\n")
    
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
    return(db.dat)
  })
  # 将获取到的内更新到页面
  observe({
    # 更新基因名
    updateSelectizeInput(session = session,
                         inputId = "corr_symbol_id",
                         choices = unique(study.content.df()$hugo_symbol),
                         selected = NULL,
                         server = TRUE)
    
    # 如果是突变，则显示突变类型
    # 更新突变类型
    # var.types <- unique(study.content.df()$variant_classification)
    # updatePickerInput(session = session, inputId = "corr_vartype_id",
    #                   # label = "Select/deselect all options",
    #                   choices = var.types, selected = var.types,
    #                   options = list(
    #                     `actions-box` = TRUE))
  })
  
  # 4. 基因表达分组cutoff更新联动
  # observe({
  #   number1 <- input$corr_exprcut1_id  # value 1
  #   updateNumericInput(session, inputId = "corr_exprcut2_id",
  #                      label = "Cutoff (Low):",
  #                      value = 100-number1,
  #                      min = 0, max = 100, step = NA)
  # })
  # 若需要cutoff1与cutoff2联动，则取消以下注释；本处为方便设置不同的cutoff，因此不对cutoff1联动
  # observe({
  #   number2 <- input$corr_exprcut2_id  # value 1
  #   updateNumericInput(session, inputId = "corr_exprcut1_id",
  #                      label = "Cutoff (High):",
  #                      value = 100-number2,
  #                      min = 0, max = 100, step = NA)
  # })  
  
  # 5. 获取TME基因分类
  # 将获取到数据类型更新到页面
  observe({
    db_choices.df <- data.frame('sigtype' = c(rep('Gene',length(tme.gene.corr)),rep('Pathway',length(tme.cell.corr))),
                                'sigvalue' = c(tme.gene.corr,tme.cell.corr))
    db_choices <- lapply(split(db_choices.df$sigvalue, db_choices.df$sigtype), as.list)
    
    updatePickerInput(session = session, inputId = "corr_gene_type",
                      choices = db_choices, 
                      selected = db_choices[[1]][[1]])
  })
  
  
  # 重置输入
  # observeEvent(input$corr_reset, {
  #   shinyjs::reset('corr_sidebar')
  #   shinyjs::reset('corr_mainpage')
  # })
  
  # 从DB获取数据
  # 临床信息
  db.dat.cli <- eventReactive(input$corr_study_id,{
    cat(cat_prefix_tme_corr,"-查询Clinical，待分析的研究队列: ",input$corr_study_id,"\n")
    study <- input$corr_study_id
    cancer_id <- input$corr_cancer_detail # cancer_detail
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
  db.dat <- eventReactive(c(input$corr_study_id,
                            input$corr_data_type),{
                              cat(cat_prefix_tme_corr,"-查询Mutation/Expr，待分析的研究队列: ",input$corr_study_id,"\n")
                              # data_type <- input$corr_data_type
                              data_type <- 'Expr' # Hardcode：查询expr表
                              study <- input$corr_study_id
                              tbl_name <- case_when(data_type == 'Mutation' ~ paste0(tolower(study),'_mutation'),
                                                    data_type == 'Expr' ~ paste0(tolower(study),'_expr'),
                                                    TRUE ~ '')
                              temp.df <- queryDataFromMySQL(tbl_name)
                              return(temp.df)
                            })
  
  # 业务层：Corr分析
  observeEvent(input$corr_goButton,{
    cat("===================== Server Correaltion =======================\n")
    show_modal_progress_line(
      color = "#00A064", # DF0101
      trail_color = "#eee",
      duration = 90,
      easing = "easeOut",
      text = "Starting Correaltion Computation..."
    )
    # browser()
    selected_study_id = input$corr_study_id
    selected_tumor_detail_id = input$corr_cancer_detail
    cat(cat_prefix_tme_corr,"-选择的研究队列: ", selected_study_id, "\n")
    
    # data_type <- input$corr_data_type
    # cat(cat_prefix_tme_corr,"-选择的队列数据类型: ", data_type, "\n")
    
    # validate(need(try(input$corr_symbol_id),"Please select at least one gene!"))
    symbol <- input$corr_symbol_id
    cat(cat_prefix_tme_corr,"-选择的基因名称: ", symbol, "\n")
    
    # vartype <- input$corr_vartype_id
    # cat(cat_prefix_tme_corr,"-选择的基因突变类型: ", vartype, "\n")
    
    # exprcut1 <- input$corr_exprcut1_id
    # exprcut2 <- input$corr_exprcut2_id
    # cat(cat_prefix_tme_corr,"-选择的基因高表达分位数: ", exprcut1, "\n")
    # cat(cat_prefix_tme_corr,"-选择的基因低表达分位数: ", exprcut2, "\n")
    
    colorg1 <- input$corr_colorg1_id
    colorg2 <- input$corr_colorg2_id
    cat(cat_prefix_tme_corr,"-选择的样本分组1颜色: ", colorg1, "\n")
    cat(cat_prefix_tme_corr,"-选择的样本分组2颜色: ", colorg2, "\n")
    
    genetype <- input$corr_gene_type
    cat(cat_prefix_tme_corr,"-选择的基因/细胞集类型: ", genetype, "\n")
    
    corrtype <- input$corr_type
    cat(cat_prefix_tme_corr,"-选择的想关性方法: ", corrtype, "\n")
    
    figuretype <- input$corr_figure_type
    cat(cat_prefix_tme_corr,"-选择的输出图样式: ", figuretype, "\n")
    
    # 数据清理、绘图、输出
    input.genes <- symbol 
    # if(data_type == 'Mutation'){
    #   input.genes <- c(input.genes,"Combined")
    # }
    # input.vartype <- vartype
    # input.exprcut1 <- exprcut1/100  #转化为分位数函数的输入值
    # input.exprcut2 <- exprcut2/100
    input.genetype <- genetype
    input.corrtype <- corrtype
    input.figuretype <- figuretype
    
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
    output$corr_maintabs <- renderUI({
      # 输出的面板Tab名称
      tabs <- lapply("Correlation Analysis", function(name) {
        tabPanel(
          title = name,
          uiOutput(paste0("corr","_tmegene"))
        )
      })
      do.call(tabsetPanel, tabs)
    })
    # 执行数据分析
    # withProgress(message = 'Making plot', value = 0, {
      n <- length(input.genes)
      for(j in 1:n){
        update_modal_progress(
          value = j / n,
          text =  paste("Process",sprintf("(%1.0f%%)", j/n*100)) #paste("Analyzing gene: ", gname)
        )
      }
      
      lapply(1, function(i) {
        
        gname <- input.genes[i]
        
        cli.name <- 'correlation'
        # for expr
        {
          # 选择的基因名的表达谱
          dat.x <- db.dat() %>%
            dplyr::filter(hugo_symbol %in% input.genes) %>%
            tibble::column_to_rownames(var = 'hugo_symbol') %>%
            t() %>% as.data.frame() 
          # TME的基因的表达谱
          if(isFALSE(use.ssGSEA)){
            # 直接提取基因的表达谱
            dat.y <- db.dat() %>%
              dplyr::filter(hugo_symbol %in% tme.df$symbol) %>%
              tibble::column_to_rownames(var = 'hugo_symbol') %>%
              t() %>% as.data.frame() 
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
          }
          # 计算相关系数
          corr.df <- psych::corr.test(dat.x,dat.y,method = input.corrtype,adjust = 'fdr')
        }
        
        # 一个Tab输出
        output[[paste0("corr","_tmegene")]] <- renderUI({
          tabPanel(
            style = "padding-top:15px",
            title = paste0("",cli.name), status = "primary", solidHeader = TRUE, collapsible = TRUE,
            # plotlyOutput(paste0(gname, "_plot_corr_",tolower(cli.name)), height = "350px", width = "100%"),
            # plotOutput(paste0(gname, "_plot_corr_",tolower(cli.name)), height = "350px", width = "100%"),
            shinycssloaders::withSpinner(plotOutput(paste0(gname, "_plot_corr_",tolower(cli.name)), height = "350px", width = "100%")),
            downloadButton(paste0(gname, "_dl_plot_corr_",tolower(cli.name)), 
                           tags$span(
                             "DLGraph",
                             add_prompt(tags$span(icon(name = "question-circle")),
                                        message = "Save plot in a PDF file.", 
                                        position = "left")),
                           style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
            downloadButton(paste0(gname, "_dl_tbl_corr_",tolower(cli.name)), 
                           tags$span(
                             "DLTable",
                             add_prompt(tags$span(icon(name = "question-circle")),
                                        message = "Save data of corr plot in a txt file.", 
                                        position = "left")),
                           style = "display:inline-block;float:right;color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "),
            br(),
            DT::dataTableOutput(paste0(gname, "_tbl_corr_",tolower(cli.name)))
          )
        })
        
        # 绘制相关系数图：气泡图或者三角热图
        lapply(1, function(k) {
          cli.name <- 'correlation'
          # 提取相关系数
          # corr.r <- corr.df$r
          # corr.p <- corr.df$p
          # corr.padj <- corr.df$p.adj
          # 
          # corr.r.long <- corr.r %>%
          #   as.data.frame() %>%
          #   tibble::rownames_to_column(var = 'xname') %>%
          #   tidyr::pivot_longer(cols = -xname,names_to = 'yname',values_to = 'corr')
          # corr.p.long <- corr.p %>%
          #   as.data.frame() %>%
          #   tibble::rownames_to_column(var = 'xname') %>%
          #   tidyr::pivot_longer(cols = -xname,names_to = 'yname',values_to = 'p')
          # corr.padj.long <- corr.padj %>%
          #   as.data.frame() %>%
          #   tibble::rownames_to_column(var = 'xname') %>%
          #   tidyr::pivot_longer(cols = -xname,names_to = 'yname',values_to = 'padj')
          # corr.long <- cbind(corr.r.long,corr.p.long$p,corr.padj.long$padj)
          # colnames(corr.long) <- c('yname','xname','r','p','padj')
          # 
          # corr.long <- corr.long %>%
          #   dplyr::mutate_at(3, ~ round(.,2)) %>%  # 将corr值（行列名）四舍五入
          #   dplyr::mutate_at(4:5, ~ round(.,6))  # 将corr值（行列名）四舍五入
          corr.long <- func.psychcorrwide2long(corr.df)
          # 绘图
          if(input.figuretype == 'bubble'){
            cat(cat_prefix_tme_corr,"-绘制相关系数气泡图\n")
            p.corr <- func.bubbleplot(corr.long,
                                      color1 = colorg1,
                                      color2 = colorg2,
                                      xlevel = colnames(dat.y),
                                      ylevel = colnames(dat.x))
          }
          if(input.figuretype == 'theatmap'){
            cat(cat_prefix_tme_corr,"-绘制相关系数三角热图\n")
            p.corr <- func.triangleheatmap(corr.long,
                                           color1 = colorg1,
                                           color2 = colorg2)
          }
          # 存储输出表和图
          output.graphic[[paste0(gname,"_",tolower(cli.name))]]$tbl <- corr.long
          output.graphic[[paste0(gname,"_",tolower(cli.name))]]$plot <- p.corr
          
          # output[[paste0(gname, "_plot_corr_",tolower(cli.name))]] <- renderPlotly({
          #   ggplotly(km.graphic)
          # })
          output[[paste0(gname, "_plot_corr_",tolower(cli.name))]] <- renderPlot({
            p.corr
          })
          output[[paste0(gname, "_tbl_corr_",tolower(cli.name))]] <- renderDataTable({
            datatable(corr.long, 
                      selection = list(mode = "single", target = "row"),
                      style = 'bootstrap', 
                      rownames = FALSE,
                      options = list(orderClasses = TRUE,dom = 'tp', 
                                     language = list(zeroRecords = "No records found matching your selection"),
                                     columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                      colnames = c("Queried gene",input.genetype, 'Corr','P value','FDR'))
          })
          # output[[paste0(gname, "_tbl_corr_",tolower(cli.name))]]  <- render_dt(km.df, 'cell', FALSE)
          output[[paste0(gname, "_dl_plot_corr_",tolower(cli.name))]] <- downloadHandler(
            filename = function() {
              paste0("corr_",tolower(cli.name),"_",gname,"_",Sys.Date(),".pdf", sep = "")
            },
            content = function(file) {
              pdf(file,onefile = F, width = 12, height = 5, pointsize = 10)
              print(output.graphic[[paste0(gname,"_",tolower(cli.name))]]$plot)
              dev.off()
            }
          )
          output[[paste0(gname, "_dl_tbl_corr_",tolower(cli.name))]] <- downloadHandler(
            filename = function() {
              paste0("data_",tolower(cli.name),"_",gname,"_",Sys.Date(), '.txt', sep='')
            },
            content = function(file) {
              readr::write_delim(x = output.graphic[[paste0(gname,"_",tolower(cli.name))]]$tbl, path = file, delim = "\t")
            }
          )
        })
        
        # Increment the progress bar, and update the detail text.
        # incProgress(1/n, detail = paste("Analyzing correlation......"))
      })
      remove_modal_progress()
      # Pause for 0.1 seconds to simulate a long computation.
    #   Sys.sleep(0.1)
    # })
  })
  
  
}
