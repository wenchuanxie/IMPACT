
# 先进行 χ2 分布的期望值准则验证
# ref: https://huanghaicheng1024.github.io/applied-nonparametric-statistics/chisq-test.html
# 输入: 二维列联表矩阵
# 返回list：
# n 总和
# expectation 期望频数矩阵
# rule 是否满足准则
func.chisq.expectation <- function(x){
  x <- xtabs(~ TICS + ORR, data = temp.df)
  res <- list()
  r <- nrow(x)
  c <- ncol(x)
  rs <- apply(x,1,sum) # 每行求和
  cs <- apply(x,2,sum) # 每列求和
  as <- sum(x)         # 总和
  res$n <- as
  res$expectation <- rs%*%t(cs)/as  # 期望频数矩阵
  if(r==2&c==1|r==1&c==2){
    res$rule <- !sum(res$expectation<5)
  }else if(r==2&c==2){
    if(as<40 | min(res$expectation)<1){
        # n＜40 或 最小理论数T＜1,Fisher确切概率法
        res$rule <-FALSE
      }else if(min(res$expectation)>=1 & min(res$expectation)<5) {
        # n >= 40 且 最小理论数1<=T<5,Yates校正卡方检验,此处不做方法选择，返回FALSE时，默认用Fisher
        res$rule <-FALSE
      }else{
        #  n >= 40 且 最小理论数T>5.卡方检验
        res$rule <-TRUE
      }
  }else{
    #  R×C表卡方检验条件：（1）理论数小于5的格子不能超过1/5；（2） 不能有小于1的理论数；
    #  以上两个之一不满足时，用Fisher确切概率法
    res$rule <- (mean(res$expectation<5)<0.2 & min(res$expectation) >= 1)
  }
  res
}
