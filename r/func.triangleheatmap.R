#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : func.bubbleplot.R
# @License : Copyright(C), Yucc
# @Author  : yuchen.liao
# @Time    : 2022-05-02
# @IDE     : RStudio
# @Desc    : 相关系数三角坐标图绘制
# @ref     : https://github.com/dxsbiocc/learn/blob/main/R/plot/triangle_heatmap.R
#===============================================================================

#函数说明
# @ data     : 相关系数矩阵，长数据格式。四列：yname,xname,padj,r
# @ color1   : 高值的颜色
# @ color2   : 低值的颜色

# 相关系数和p值进行绘图
func.triangleheatmap <- function(data,color1,color2){
  
  corr.in <- data %>%
    dplyr::select(yname,xname,p,r) %>%
    mutate_at(1:2, ~ as.factor(.)) # 将前两列（行列名）称转为factor
  
  corr.tri <- triangle_data(corr.in)
  
  # 替换NA为0
  corr.df <- mutate(corr.tri, 
                    r = replace_na(r, 0),
                    p = replace_na(p, 1)) 
  # 添加起始位置，和 偏移点
  tmp <- corr.df %>% transmute(across(where(is.factor), as.numeric)) %>%
    `names<-`(c("y", "x")) %>%
    cbind(corr.df, .) %>%
    as.data.frame()
  
  # P值显著的点
  points <- do.call(rbind, apply(tmp, 1, function(row) {
    p = as.numeric(row['p'])
    x = as.numeric(row['x'])
    y = as.numeric(row['y'])
    df = data.frame()
    if (p < 0.001) {
      df = rbind(df, data.frame(x = x + 0.9, y = y + 0.5))
    }
    if (p < 0.01) {
      df = rbind(df, data.frame(x = x + 0.9, y = y + 0.3))
    }
    if (p < 0.05) {
      df = rbind(df, data.frame(x = x + 0.9, y = y + 0.1))
    }
    df
  }))
  p.out <- ggplot(corr.tri) +
    # 上三角绘图：Rho值
    geom_polygon(aes(lower.y, lower.x, fill = r, group = group), colour = "grey50") +
    scale_fill_gradientn(name = "Rho", 
                         breaks =  c(-1,-0.5,0.5,1),
                         labels =  c(-1,-0.5,0.5,1),
                         limits = c(-1,1),
                         colours = colorRampPalette(c(color2, "white",color1))(5),
                         na.value = 'white') + # colors = colorRampPalette(c("#1E3163", "#00C1D4", "#FFED99","#FF7600"))(10)
    new_scale("fill") +
    # 下三角绘图：P值
    geom_polygon(aes(upper.y, upper.x, fill = p, group = group)) +
    scale_fill_gradientn(name = "P.adj", 
                         trans = "log",
                         breaks =  c(0.001,0.01,0.05,0.1),
                         labels =  c(0.001,0.01,0.05,0.1),
                         # limits = c(1E-100,0.1),
                         colours=c("red","orange","yellow","grey90"),
                         na.value = 'white')
    # scale_fill_gradientn(values = c(0,0.05,0.2),breaks = c(0,0.05,0.2),labels=c(0,0.05,0.2),
    #                      colours = c( "red", "white",'green'),na.value = 'white') # RColorBrewer::brewer.pal(5, "YlGnBu")
  if(nrow(points) > 0){
    # P值统计学星号
    p.out <- p.out + geom_point(data = points, aes(x, y), shape=8,size = 0.8)
  }
  # 添加横纵坐标label
  p.out <- p.out + 
    scale_x_continuous(breaks = c(1:length(unique(corr.in[[2]]))) + 0.5, expand = c(0,0),
                       labels = sort(unique(corr.in[[2]]))) +
    scale_y_continuous(expand = c(0, 0), breaks = c(1:length(unique(corr.in[[1]]))) + 0.5,
                       labels = sort(unique(corr.in[[1]])), sec.axis = dup_axis()) +
    theme(
      plot.margin = margin(0.5,0.01,0.5,0.01, "cm"),
      axis.title = element_blank(),
      axis.text.y.left = element_blank(),
      axis.ticks.y.left = element_blank(),
      axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.5),
      legend.position = 'bottom'
    )
  
  return(p.out)
}

# 根据配对列表生成上、下三角坐标
triangle <- function(pairs, type = "up") {
  # 默认的上三角坐标基
  x = c(0, 0, 1)
  y = c(0, 1, 1)
  # 下三角的坐标基
  if (type == "lower") {
    x = c(0, 1, 1)
    y = c(0, 0, 1)
  }
  # 生成三角矩阵
  mat = do.call(
    rbind,
    apply(pairs, 1, function (row) {
      a = row[1]
      b = row[2]
      data.frame(
        x = x + a,
        y = y + b,
        group = paste(a, b, sep = "-")
      )
    }))
  return(mat)
}

triangle_data <- function(data, row = 1, col = 2) {
  # 这里设置的 row 和 col 表示要指定的行列变量所在列
  # 生成所有组合
  rows = length(unique(data[[row]]))
  cols = length(unique(data[[col]]))
  pairs = merge(1:rows, 1:cols)
  # 获取上三角坐标
  upper <- triangle(pairs)
  colnames(upper) <- c(paste0("upper.", colnames(upper)[1:2]), "group")
  # 获取下三角坐标
  lower <- triangle(pairs, type = "lower")[1:2]
  colnames(lower) <- paste0("lower.", colnames(lower))
  # 合并坐标
  upper_lower = cbind(upper, lower)
  # 根据分组信息将坐标连接到数据中
  data %>% transmute(across(where(is.factor), ~ as.character(as.numeric(.)))) %>%
    unite("group", row:col, sep = "-") %>%
    cbind(data, .) %>%
    right_join(upper_lower, by = "group")
}