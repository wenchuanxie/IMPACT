#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : tab.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-05-23
# @IDE     : RStudio
# @Desc    : 第5个Tab功能：数据 表格
#===============================================================================


tab_05_data_resource <- list()

tab_05_data_resource$ui <- fluidPage(
  id = 'dataresource_mainpage',
  h3(strong("Data Resources in IMPACT"),align="center"),
  hr(),
  width = 10,
  uiOutput(outputId='dataresource_maintabs'),
  textOutput('myText')
)

tab_05_data_resource$server <- function(input, output,session) {
  cat("========================= Start Data Resource  ======================\n")
  
  selectedData <- reactiveValues(dldata = '')
  ns = session$ns
  # 输出到页面
  output$dataresource_maintabs <- renderUI({
    # 输出的面板Tab名称
    # tabs <- lapply("Data Resource", function(name) {
    #   tabPanel(
    #     title = name,
    #     uiOutput(paste0("dataresource"))
    #   )
    # })
    # do.call(tabsetPanel, tabs)
    uiOutput(paste0("dataresource"))
  })

  # 输出Tab
  output[["dataresource"]] <- renderUI({
    # tabPanel(
    #   style = "padding-top:15px",
    #   title = paste0(""), status = "primary", solidHeader = TRUE, collapsible = TRUE,
    #   DT::dataTableOutput(paste0("datatbl"))
    # )
    shinycssloaders::withSpinner(DT::dataTableOutput(paste0("datatbl")))
  })
  
  # 定义函数
  shinyInput <- function(FUN, len, id, ns, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(ns(id), i), ...))
    }
    inputs
  }
  
  # 页面缓冲显示
  # show_modal_spinner(spin = "orbit",color = "#6bba8b",text = "Downloading files...")
  #  查询出表格后进行处理
  data.out <- study.df %>%
    # dplyr::filter(study_source != 'TCGA') %>% # TCGA数据源不提供下载入口
    dplyr::arrange(study_type,desc(study_source),study_id) %>%
    dplyr::mutate(study_type = ifelse(study_type == 'ICIs','ICIs Cohort','non ICIs Cohort')) %>%
    dplyr::select(study_type,study_source,study_id,genomic_platform,tumor_detail,sample_size,data_type,pmid) 
  # 为方便根据关键词（study_id，data_type等）下载数据，且避免hyperlink，因此新建一个有hyperlink的对象用于前端展示
  # 要求data.dis 和 data.out的列信息和行顺序一致（在data.dis不可以有行过滤）
  data.dis <- data.out %>%
    dplyr::mutate(pmid = ifelse(is.na(pmid),
                                "",
                                paste0("<a href=\"https://pubmed.ncbi.nlm.nih.gov/", pmid,"/\" target=\"_blank\">", pmid,"</a>")))%>%
    dplyr::mutate(study_id = ifelse(startsWith(study_id,"GSE"),
                                    paste0("<a href=\"https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", tolower(study_id),"\" target=\"_blank\">", study_id,"</a>"),
                                    study_id)) %>% # https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=gse13213
    dplyr::mutate(genomic_platform = ifelse(startsWith(genomic_platform,"GPL"),
                                    paste0("<a href=\"https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", tolower(genomic_platform),"\" target=\"_blank\">", genomic_platform,"</a>"),
                                    genomic_platform)) %>%  # https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GPL6480
    dplyr::mutate(Actions = ifelse(study_source %in% c('In-house'),"Contact Author",
                                   ifelse(study_source %in% c('TCGA'),
                                          paste0('<a href=\"https://portal.gdc.cancer.gov/\" target=\"_blank\">', 'GDC Data Portal','</a>'),
                                          shinyInput(downloadButton, 
                                                     nrow(data.out),
                                                     'button_',
                                                     ns = ns,
                                                     label = "Download",
                                                     onclick = sprintf("Shiny.setInputValue('%s', this.id)",ns("select_button"))
                                          )))
                  )
  # remove_modal_spinner()
  
  output[["datatbl"]] <- renderDataTable({
    datatable(
      {data.dis},
      # 隐藏下载功能
      # {data.dis %>%
      #     dplyr::select(-Actions)},
    style = 'bootstrap', 
    selection = list(mode = "single", target = "row"), # 单行选择，target = "cell"：选择单元格
    class = 'cell-border stripe', # 单元格加边线
    # class = "display",
    # filter = 'top', # 顶部列筛选
    rownames = FALSE,
    escape = FALSE, # hyperlink
    # extensions = 'Buttons',# 下载链接
    options = list(orderClasses = TRUE,
                   searching = F,
                   # buttons = c('csv'), # 配合extensions的显示的内容
                   pageLength  = 10,
                   language = list(zeroRecords = "No records found matching your selection"),
                   columnDefs = list(list(className = 'dt-center',targets="_all")),
                   paging = TRUE,   # 分页
                   scrollX = TRUE,  # 横向滚动轴
                   ordering = TRUE, 
                   dom = 'Bfrtip',
                   preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                   drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
                   lengthMenu=c(3,5,10)
                   # buttons = I('colvis')  # 列显示控制
                   ),
    colnames = c("Cohort Type","Source","Cohort ID", 'Platform','Cancer','Sample Size','Data Type','PMID'
                 ,'Actions'  # 隐藏下载功能
                 )
    )
  })
  
  
  # 获取点击的“download”按钮所在的row id，根据row id获取data.out中的行
  observeEvent(input$select_button, {
    
    # 页面缓冲显示
    show_modal_spinner(spin = "orbit",color = "#6bba8b",text = "Downloading files...")
    
    # cat("input$select_button:",input$select_button,"\n")  # 值为“button_n”，其中“button_”是Actions列中shinyInput函数中我们自定义的第三个参数值，n表示第n行
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    
    tmp.study_id <- data.out[selectedRow,]$study_id
    tmp.tumor_detail <- data.out[selectedRow,]$tumor_detail
    tmp.data_type <- data.out[selectedRow,]$data_type
    cat("DataResource - 当前下载的数据ID: ",tmp.study_id,", ",tmp.tumor_detail,"\n")
    
    # 查 临床信息
    data.clin <- queryDataFromMySQL(paste0(tolower(tmp.study_id),'_clinical'))
    data.clin <- data.clin %>%
      dplyr::filter(cancer_detail == tmp.tumor_detail)
    # 查 表达/突变/蛋白数据
    data.dna <- data.frame()
    if(grepl("Mutation",tmp.data_type)){
      data.dna <- queryDataFromMySQL(paste0(tolower(tmp.study_id),'_mutation'))
      data.dna <- data.dna %>%
        dplyr::filter(sample_id %in% data.clin$sample_id)
    }
    data.rna <- data.frame()
    if(grepl("Expr",tmp.data_type)){
      data.rna <- queryDataFromMySQL(paste0(tolower(tmp.study_id),'_expr'))
      data.rna <- data.rna %>%
        dplyr::select(hugo_symbol,intersect(colnames(data.rna),data.clin$sample_id))
    }
    data.pro <- data.frame()
    if(grepl("Proteome",tmp.data_type)){
      data.pro <- queryDataFromMySQL(paste0(tolower(tmp.study_id),'_proteome'))
      data.pro <- data.pro %>%
        dplyr::select(hugo_symbol,intersect(colnames(data.pro),data.clin$sample_id))
    }
    # update_modal_progress()
    data.list <- list("Clinical" = data.clin,
                      "Mutation" = data.dna,
                      "RNAseq" = data.rna,
                      "Proteome" = data.pro)
    selectedData$dldata <- data.list
    
  })
  # 下载数据
  lapply(1:nrow(data.out), function(i){
    # cat("DataResource - 当前行数:",i,"\n")
    tmp.study_id <- data.out[i,]$study_id
    tmp.tumor_detail <- data.out[i,]$tumor_detail
    
    output[[paste0("button_",i)]] <- downloadHandler(
      filename = function() {
        paste0(prefix_output_file,"_Data_",tmp.study_id,'_',tmp.tumor_detail,".xlsx", sep = "")
      },
      content = function(filename) {
        cat("DataResource - 数据对象:",filename,"\n")
        openxlsx::write.xlsx(selectedData$dldata,file = filename,overwrite = T)
        remove_modal_spinner()  # 写出xlsx后才消除缓冲显示
      }
    )
  })
  
}
