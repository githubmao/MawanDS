#--------------Code Description------------------------------------------------#
# Notes:
# ver1.0, date: 20180816, By MaoYan
# 
# 1.分析妈湾出口匝道匝道5的行驶速度、加速度、制动、加速等。(轿车only)
#------------------------------------------------------------------------------#


# 调用DataInput.R----
source(file = "E:/R/MaWan/MawanDS/DataInput.R", encoding = "utf-8")


# 调用R Packages----
library(ggplot2)
library(magrittr)
library(plyr)


# 数据切分----
df.zd5 <- subset(x = df.dsdata, dsScen == "ZD5")  # 匝道5数据

df.zd5$disTravelled <- ifelse(test = df.zd5$roadName == "MainRD_Right",
                              df.zd5$disFromRoadStart - 2800,
                              df.zd5$disFromRoadStart + 2776)  # 桩号标定


# 1 匝道5，行驶速度分析----
plot.zd5speed <- ggplot(data = df.zd5,
                        aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
#  scale_x_continuous(name = "桩号", limits = c(500, 3600),
#                     breaks = c(500, 1500, 2428, 2478, 2621, 3410),
#                     labels = c("LK4+160", "LK3+160", "", "",
#                                "S2K0+144.039", "S2K0+956.173")) +
#  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100),
#                     breaks = seq(0, 100, by = 20)) +
#  annotate(geom = "rect", xmin = 500, xmax = 3410,
#           ymin = 0, ymax = 100, alpha = 0.2) +
#  annotate(geom = "text", x = 3410, y = 100, label = "S2匝道洞口") +
#  annotate(geom = "segment", x = 2428, xend = 2478, y = 95, yend = 95,
#           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
#  annotate(geom = "text", x = 2400, y = 100, label = "渐变段") +
#  annotate(geom = "segment", x = 2478, xend = 2621, y = 95, yend = 95,
#           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
#  annotate(geom = "text", x = 2600, y = 100, label = "减速段") +
#  theme(legend.position = "none", 
#        axis.text.x = element_text(face = "bold", size = 10),
#        axis.text.y = element_text(face = "bold", size = 10),
#        axis.title.x = element_text(face = "bold", size = 12),
#        axis.title.y = element_text(face = "bold", size = 12),
#        panel.grid.major.x = element_line(linetype = "dashed",
#                                          colour = "black", size = 0.5))


plot.zd5speed

# 2 匝道5，轨迹分析----
# 3 匝道5，车道跨越点分析----
# 4 匝道5，加速度分析----
# 5 匝道5，制动踏板位移分析----
# 6 匝道5，油门踏板位移分析----
# 7 关键位置速度分布分析----
# 8 开始制动位置分布----
# 9 开始加速位置分布----
# 10 大减速度分布分析----
# 10.1 大减速度，个体比较----
# 10.2 大减速度，群体比较----