#--------------Code Description------------------------------------------------#
# Notes:
# ver1.0, date: 20180824, By MaoYan
# 
# 1.分析妈湾出口匝道匝道B的行驶速度、加速度、制动、加速等。
#------------------------------------------------------------------------------#


# 调用DataInput.R----
source(file = "E:/R/MaWan/MawanDS/DataInput.R", encoding = "utf-8")


# 调用R Packages----
library(ggplot2)
library(magrittr)
library(plyr)


# 数据切分----
df.zdb <- subset(x = df.dsdata,
                 dsScen == "ZDB" | dsScen == "ZDBwithTraffic")  # 匝道B数据


