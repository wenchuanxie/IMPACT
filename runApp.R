

# -*- coding: utf-8 -*-
# @File    : runApp.R
# @License : Copyright(C), Yucc  
# @Author  : wenchuan.xie
# @Time    : 2022/04/10
# @IDE     : 启动Shiny


rm(list = ls())
gc()

library(shiny)
runApp("../IMPACT/",launch.browser = T)

