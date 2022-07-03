# -*- coding: utf-8 -*-
# @File    : func.factorization.R
# @License : Copyright(C), WEN
# @Author  : gzlyc1228@163.com
# @Time    : 2020/07/21 15:00 
# @IDE     : RStudio
# @Desc    : 因子化操作

func.factorization = function(input.clinical.df,cancer.detail.list){
  
  cox.df <- input.clinical.df
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
  if(!cancer.detail.list %in% c('BRCA','PRAD','BRCA','OV','CESC') &'sex' %in% colnames(cox.df)) {
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
  if('visceral_pleural_invasion' %in% colnames(cox.df)) {
    cox.df$visceral_pleural_invasion <- factor(capitalize(tolower(cox.df$visceral_pleural_invasion)),levels = c('No','Yes'))
  }
  if('lymphvascular_invasion' %in% colnames(cox.df)) {
    cox.df$lymphvascular_invasion <- factor(capitalize(tolower(cox.df$lymphvascular_invasion)),levels = c('No','Yes'))
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
    cox.df$tp53 <- factor(capitalize(tolower(cox.df$tp53)),levels = c('Wt','Mut'))
  }
  if('egfr' %in% colnames(cox.df)) {
    cox.df$egfr <- factor(capitalize(tolower(cox.df$egfr)),levels = c('Wt','Mut'))
  }
  if('alk' %in% colnames(cox.df)) {
    cox.df$alk <- factor(capitalize(tolower(cox.df$alk)),levels = c('Wt','Mut'))
  }
  if('stk11' %in% colnames(cox.df)) {
    cox.df$stk11 <- factor(capitalize(tolower(cox.df$stk11)),levels = c('Wt','Mut'))
  }
  if('kras' %in% colnames(cox.df)) {
    cox.df$kras <- factor(capitalize(tolower(cox.df$kras)),levels = c('Wt','Mut'))
  }
  return(cox.df)
}