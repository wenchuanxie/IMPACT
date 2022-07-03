#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : dataimport.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-03-19
# @IDE     : RStudio
# @Desc    : 从数据库获取数据
#===============================================================================


# 数据库链接 ----
{
  # 关闭多有sql连接,注释是否会影响别的项目？
  killDbConnections <- function () {
    all_cons <- dbListConnections(MySQL())
    print(all_cons)
    for(con in all_cons)
      +  dbDisconnect(con)
    print(paste(length(all_cons), " connections killed."))
  }
  killDbConnections()
  # 数据库查询
  queryDataFromMySQL <- function(dbName){
    host <- "127.0.0.1"     # dev
    # host <- "47.104.15.105" # prod
    dbname <- "icbcohorts"
    user <- "icbuser"
    password <- "ICB2021"       # dev
    # password <- "ICB2021#1qaz"  # prod
    con <- dbConnect(RMariaDB::MariaDB(),
                     host=host,dbname=dbname,user=user,password=password)
    sql <- paste0("SELECT * FROM ",dbName)
    sql.res <- dbSendQuery(con,sql)
    query.df <- dbFetch(sql.res, n = -1) # n = -1 表示提取所有
    dbClearResult(sql.res)
    dbDisconnect(con)
    return(query.df)
  }
}

# 队列信息查询 ----
{
  # 初始化时读取数据，减少数据库连接
  ## 初始化时获取队列信息
  study.all <- queryDataFromMySQL('study_info')
  study.df <- study.all
  
  ## 获取TME基因分类信息
  tme.gene.df <- queryDataFromMySQL('tme_gene_info')
  ### 前端展示用的基因分类信息
  tme.gene.corr <- unique(tme.gene.df$type)
  ## 获取TME细胞分类信息
  tme.cell.df <- queryDataFromMySQL('tme_cell_info')
  ### 前端展示用的细胞分类信息
  tme.cell.corr <- unique(tme.cell.df$main_category)
}

# Interaction分析使用的数据，预先加载
{
  
  tbl.name <- 'Checkmate025'
  cm025.cli <- queryDataFromMySQL(paste0(tbl.name,"_clinical"))
  cm025.mut <- queryDataFromMySQL(paste0(tbl.name,"_mutation"))
  
}





# 功能测试脚本
if(F){
  # 数据一致性测试 ----
  source("./r/func.ggboxplot.R")
  tbl.name <- 'Checkmate025'
  clin.df <- queryDataFromMySQL(paste0(tbl.name,"_clinical"))
  muta.df <- queryDataFromMySQL(paste0(tbl.name,"_mutation"))
  expr.df <- queryDataFromMySQL(paste0(tbl.name,"_expr"))
  
  # km.df <- km.df %>%
  #   dplyr::filter(cancer_detail %in% c("Melanoma")) %>%
  #   dplyr::filter(sample_id %in% unique(muta.df$sample_id))
  # km.df$smoking <- factor(km.df$smoking,levels = c('Never','Ever'))
  # km.df2 <- func.removeColsAllNa(km.df)
  
  gene <- c('KEAP1')
  # var.types <- c('missense','synonymous','splice','nonsense')
  var.types <- unique(muta.df$variant_classification) # 默认为所有
  muta.df <- muta.df %>%
    dplyr::filter(hugo_symbol %in% gene) %>%
    dplyr::filter(variant_classification %in% var.types) %>%
    dplyr::distinct(sample_id,hugo_symbol,variant_classification)
  length(unique(muta.df$sample_id))
  
  table(clin.df$cancer_detail)
  clin.df <- clin.df %>%
    dplyr::filter(ici_treatment == 'Yes') %>%
    # dplyr::filter(cancer_detail == 'NSCLC') %>%
    dplyr::mutate(group = ifelse(sample_id %in% muta.df$sample_id,'Mut','Wt'))
  table(clin.df$group)
  
  clin.df %>%
    dplyr::select(sample_id,group,img_name = neoantigen) %>%
    dplyr::mutate(img_name = as.numeric(img_name)) %>%
    func.ggboxplot(group1 = "Wt",group2 = 'Mut',color1 = "red",color2 = 'green',ytitle = 'Neoantigen')
  
  
  # 相关性分析测试 ----
  dat.x <- expr.df %>%
    dplyr::filter(hugo_symbol %in% c('EGFR','TP53','ALK')) %>%
    tibble::column_to_rownames(var = 'hugo_symbol') %>%
    t() %>% as.data.frame()
  dat.y <- expr.df %>%
    dplyr::filter(hugo_symbol %in% unique(tme.gene.df[which(tme.gene.df$type =='Chemokine'),]$symbol)) %>%
    tibble::column_to_rownames(var = 'hugo_symbol') %>%
    t() %>% as.data.frame()
  corr <- psych::corr.test(dat.x,dat.y,method = 'spearman',alpha=.05,adjust = 'fdr')
  corr.r <- corr$r %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = 'xname') %>%
    tidyr::pivot_longer(cols = -xname,names_to = 'yname',values_to = 'corr')
  corr.p <- corr$p %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = 'xname') %>%
    tidyr::pivot_longer(cols = -xname,names_to = 'yname',values_to = 'p')
  corr.padj <- corr$p.adj %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = 'xname') %>%
    tidyr::pivot_longer(cols = -xname,names_to = 'yname',values_to = 'padj')
  corr.long <- cbind(corr.r,corr.p$p,corr.padj$padj)
  colnames(corr.long) <- c('yname','xname','r','p','padj')
  
  source("./r/func.bubbleplot.R")
  func.bubbleplot(corr.long,color1 = "#2165B0",color2 = "#B11F2C",
                  xlevel = unique(tme.gene.df[which(tme.gene.df$type =='Chemokine'),]$symbol),
                  ylevel = c('EGFR','TP53','ALK'))
  
  
  source("./r/func.triangleheatmap.R")
  tri.df <- corr.long %>%
    dplyr::select(xname,yname,padj = p,r) 
  func.triangleheatmap(tri.df,color1 = "#2165B0",color2 = "#B11F2C")
  
  
  # TME细胞集测试 ----
  tbl.name <- 'tme_cell_info'
  tme.cell.df <- queryDataFromMySQL(paste0(tbl.name))
  
  tbl.name <- 'tme_gene_info'
  tme.gene.df <- queryDataFromMySQL(paste0(tbl.name))
  
  # 秩和检验测试
  source("./r/func.bacthwilcoxtest.R")
  tbl.name <- 'checkmate009'
  expr.df <- queryDataFromMySQL(paste0(tbl.name,"_expr"))
  
  grp1.df <- expr.df %>%
    dplyr::filter(hugo_symbol %in% c('EGFR')) %>%
    tibble::column_to_rownames(var = 'hugo_symbol') %>%
    t() %>% as.data.frame() %>%
    tibble::rownames_to_column(var = 'sample_id')
  colnames(grp1.df)[2] <- "sltgene"
  
  grp1.df <- grp1.df %>%
    dplyr::mutate(sltgene = as.numeric(sltgene)) %>%
    dplyr::mutate(group = ifelse(sltgene >= median(sltgene),"High",
                                 ifelse(sltgene <= median(sltgene),'Low','Middle'))) %>%
    dplyr::filter(group %in% c('Low','High')) %>%
    dplyr::select(sample_id,group,sltgene) %>%
    dplyr::arrange(group,desc(sltgene))
  
  dat.y <- expr.df %>%
    dplyr::filter(hugo_symbol %in% tme.gene.df[which(tme.gene.df$type == 'Chemokine'),]$symbol) %>%
    tibble::column_to_rownames(var = 'hugo_symbol') %>%
    t() %>% as.data.frame()
  
  dat.g <- grp1.df %>%
    dplyr::inner_join(dat.y %>%
                        tibble::rownames_to_column(var = 'sample_id'),by='sample_id')
  # 计算两组间差异
  calc.out <- do.call(rbind,lapply(colnames(dat.g)[-(1:3)], func.bacthwilcoxtest,dat.g))
  
}
