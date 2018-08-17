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
  scale_x_continuous(name = "桩号", limits = c(500, 3100),
                     breaks = c(500, 1500, 2556, 2636, 2776, 3100),
                     labels = c("RK2+330", "RK3+330", "", "",
                                "S2K0+142.248", "S5K0+466")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100),
                     breaks = seq(0, 100, by = 20)) +
  annotate(geom = "rect", xmin = 500, xmax = 2921,
           ymin = 0, ymax = 100, alpha = 0.2) +
  annotate(geom = "text", x = 2921, y = 100, label = "S5匝道洞口") +
  annotate(geom = "segment", x = 2556, xend = 2636, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2596, y = 100, label = "渐变段") +
  annotate(geom = "segment", x = 2636, xend = 2776, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2706, y = 100, label = "减速段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd5speed


# 2 匝道5，轨迹分析----
df.zd5traj <- CalcBatchTraj(data = df.zd5,
                            kDriverID = c("S0101", "S0201", "S0301", "S0401",
                                          "S0501", "S0601", "S0701", "S0801",
                                          "S0901", "S1001", "S1101", "S1201",
                                          "S1301", "S1401", "S1501", "S1601",
                                          "S1701", "S1801", "S1901", "S2001"),
                            is.main2ramp = TRUE,
                            is.disdecrease = FALSE)

plot.zd5traj <- ggplot(data = df.zd5traj,
                       aes(x = disTravelled, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1)

plot.zd5traj


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





























