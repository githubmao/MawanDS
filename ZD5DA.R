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
# 2.1 匝道5，轨迹分析1----
df.zd5traj <- CalcBatchTraj(data = df.zd5,
                            kDriverID = c("S0101", "S0201", "S0301", "S0401",
                                          "S0501", "S0601", "S0701", "S0801",
                                          "S0901", "S1001", "S1101", "S1201",
                                          "S1301", "S1401", "S1501", "S1601",
                                          "S1701", "S1801", "S1901", "S2001"),
                            is.main2ramp = TRUE,
                            is.disdecrease = FALSE)

plot.zd5traj1 <- ggplot(data = df.zd5traj,
                        aes(x = disTravelled, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 3100, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 2556, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(3100, 3100),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 2638, xend = 3100, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 2776, xend = 3100, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 2776, xend = 2776, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 2776, xmax = 3100, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 2638, xend = 2776, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(2673, 2708, 2743), xend = c(2776, 2776, 2776),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 2556, xend = 2638, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 2776, xend = 3100, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 2638, xend = 2776, y = 18.65, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 2636, xend = 2638, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 2556, xend = 2636, y = 11.45, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(2636, 2638, 2776), xend = c(2638, 2776, 3100),
           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 2921,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 2921, y = 1.5,
           fontface="bold", size = 4,
           label = "S5匝道洞口") +
  annotate(geom = "segment",
           x = 2556, xend = 2636, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2596, 2596), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "50.04m")) +
  annotate(geom = "segment",
           x = 2636, xend = 2776, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2706, 2706), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "142.65m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 3100),
                     breaks = c(500, 1500, 2556, 2636, 2776, 3100),
                     labels = c("RK2+330", "RK3+330", "", "",
                                "S2K0+142.248", "S5K0+466")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd5traj1


# 2.2 匝道5，轨迹分析2----
plot.zd5traj2 <- ggplot(data = df.zd5traj,
                        aes(x = disTravelled, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 2000, xend = 3100, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 2000, xend = 2556, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(2000, 2000), xend = c(3100, 3100),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 2638, xend = 3100, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 2776, xend = 3100, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 2776, xend = 2776, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 2776, xmax = 3100, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 2638, xend = 2776, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(2673, 2708, 2743), xend = c(2776, 2776, 2776),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 2556, xend = 2638, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 2776, xend = 3100, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 2638, xend = 2776, y = 18.65, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 2636, xend = 2638, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 2556, xend = 2636, y = 11.45, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(2636, 2638, 2776), xend = c(2638, 2776, 3100),
           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 2000, xmax = 2921,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 2921, y = 1.5,
           fontface="bold", size = 4,
           label = "S5匝道洞口") +
  annotate(geom = "segment",
           x = 2556, xend = 2636, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2596, 2596), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "50.04m")) +
  annotate(geom = "segment",
           x = 2636, xend = 2776, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2706, 2706), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "142.65m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(2000, 3100),
                     breaks = c(2556, 2636, 2776, 3100),
                     labels = c("", "",
                                "S2K0+142.248", "S5K0+466")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd5traj2


# 3 匝道5，车道跨越点分析----
df.zd5LCpoints <- CalcBatchLCPoint(data = df.zd5traj,
                                   kDriverID = c("S0101", "S0201", "S0301",
                                                 "S0401", "S0501", "S0601",
                                                 "S0701", "S0801", "S0901",
                                                 "S1001", "S1101", "S1201",
                                                 "S1301", "S1401", "S1501",
                                                 "S1601", "S1701", "S1801",
                                                 "S1901", "S2001"),
                                   kLatDis = 11.45,
                                   is.main2ramp = TRUE,
                                   is.disdecrease = FALSE)

median(df.zd5LCpoints$disTravelled)

plot.zd5LCpoints <- ggplot(data = df.zd5LCpoints,
                           aes(x = disTravelled, y = drivingTraj)) +
  geom_jitter(shape = 21, fill = "white", size = 2, stroke = 2) +
  # 平均换道位置
  geom_vline(xintercept = median(df.zd5LCpoints$disTravelled),
             col = "red", size = 1) +
  # 平均换道位置标注
  annotate(geom = "text", x = median(df.zd5LCpoints$disTravelled) + 150,
           y = 11.45 - 2,
           fontface = "bold", colour = "red", size = 4,
           label = c("换道位置中位值")) +
  annotate(geom = "segment",
           x = median(df.zd5LCpoints$disTravelled),
           xend = median(df.zd5LCpoints$disTravelled) + 80,
           y = 11.45, yend = 11.45 - 2,
           size = 1, colour = "red",
           arrow = arrow(ends = "last", angle = 30,
                         length = unit(0.2, "cm"))) +
  annotate(geom = "segment",
           x = median(df.zd5LCpoints$disTravelled), xend = 2556,
           y = 20, yend = 20, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2550, y = 18,
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段起点25m")) +
  annotate(geom = "segment",
           x = median(df.zd5LCpoints$disTravelled), xend = 2636,
           y = 20, yend = 20, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2620, y = 22,
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段终点55m")) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 2300, xend = 3100, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 2300, xend = 2556, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(2300, 2300), xend = c(3100, 3100),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 2638, xend = 3100, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 2776, xend = 3100, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 2776, xend = 2776, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 2776, xmax = 3100, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 2638, xend = 2776, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(2673, 2708, 2743), xend = c(2776, 2776, 2776),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 2556, xend = 2638, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 2776, xend = 3100, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 2638, xend = 2776, y = 18.65, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 2636, xend = 2638, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 2556, xend = 2636, y = 11.45, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(2636, 2638, 2776), xend = c(2638, 2776, 3100),
           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 2300, xmax = 2921,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 2921, y = 1.5,
           fontface="bold", size = 4,
           label = "S5匝道洞口") +
  annotate(geom = "segment",
           x = 2556, xend = 2636, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2596, 2596), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "50.04m")) +
  annotate(geom = "segment",
           x = 2636, xend = 2776, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2706, 2706), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "142.65m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(2300, 3100),
                     breaks = c(2556, 2636, 2776, 3100),
                     labels = c("", "",
                                "S2K0+142.248", "S5K0+466")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd5LCpoints


# 4 匝道5，加速度分析----
plot.zd5acc <- ggplot(data = df.zd5,
                      aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = c(-2, 2), colour = "orange",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 3100),
                     breaks = c(500, 1500, 2556, 2636, 2776, 3100),
                     labels = c("RK2+330", "RK3+330", "", "",
                                "S2K0+142.248", "S5K0+466")) +
  scale_y_continuous(name = "加速度（m/s2）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 0, xmax = 2921,
           ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 2921, y = 5, label = "S5匝道洞口") +
  annotate(geom = "segment", x = 2556, xend = 2636, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2596, y = 5, label = "渐变段") +
  annotate(geom = "segment", x = 2636, xend = 2776, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2706, y = 5, label = "减速段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd5acc


# 5 匝道5，制动踏板位移分析----
plot.zd5brakepedal <- ggplot(data = df.zd5,
                             aes(x = disTravelled, y = appBrake)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 3100),
                     breaks = c(500, 1500, 2556, 2636, 2776, 3100),
                     labels = c("RK2+330", "RK3+330", "", "",
                                "S2K0+142.248", "S5K0+466")) +
  scale_y_continuous(name = "制动踏板位移）", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 0, xmax = 2921,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 2921, y = 1, label = "S5匝道洞口") +
  annotate(geom = "segment", x = 2556, xend = 2636, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2596, y = 1, label = "渐变段") +
  annotate(geom = "segment", x = 2636, xend = 2776, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2706, y = 1, label = "减速段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd5brakepedal


# 6 匝道5，油门踏板位移分析----
plot.zd5gaspedal <- ggplot(data = df.zd5,
                           aes(x = disTravelled, y = appGasPedal)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 3100),
                     breaks = c(500, 1500, 2556, 2636, 2776, 3100),
                     labels = c("RK2+330", "RK3+330", "", "",
                                "S2K0+142.248", "S5K0+466")) +
  scale_y_continuous(name = "油门踏板位移）", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 0, xmax = 2921,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 2921, y = 1, label = "S5匝道洞口") +
  annotate(geom = "segment", x = 2556, xend = 2636, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2596, y = 1, label = "渐变段") +
  annotate(geom = "segment", x = 2636, xend = 2776, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 2706, y = 1, label = "减速段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd5gaspedal


# 7 关键位置速度分布分析----
df.zd5spotspeed <- CalcBatchSpotSpeed(data = df.zd5,
                                      kDriverID = c("S0101", "S0201", "S0301",
                                                    "S0401", "S0501", "S0601",
                                                    "S0701", "S0801", "S0901",
                                                    "S1001", "S1101", "S1201",
                                                    "S1301", "S1401", "S1501",
                                                    "S1601", "S1701", "S1801",
                                                    "S1901", "S2001"),
                                      kDis = c(2556, 2636, 2776),
                                      is.disdecrease = FALSE,
                                      kTag = c("transSTRT",
                                               "transEnd/decSTRT",
                                               "decEnd"),
                                      kDisType = "Dis")  # 特征位置速度

df.zd5spotspeed <- subset(df.zd5LCpoints,
                          select = c("rowNo", "disTravelled",
                                     "speedKMH", "driverID")) %>%
  transform(disTag = factor("LCPoint")) %>%
  rbind(df.zd5spotspeed)

# 特征点速度统计量计算
plotdf.zd5spotspeed <- ddply(df.zd5spotspeed,
                             .(disTag),
                             summarize,
                             meanSpeed = mean(speedKMH),
                             sdSpeed = sd(speedKMH))

plot.zd5spotspeed <- ggplot(data = plotdf.zd5spotspeed,
                            aes(x = disTag, y = meanSpeed)) +
  geom_bar(stat = "identity", width = 0.15) +
  geom_errorbar(aes(ymax = meanSpeed + sdSpeed, ymin = meanSpeed - sdSpeed),
                width = 0.1, size =1) +
  scale_x_discrete(limits = c("transSTRT", "LCPoint",
                              "transEnd/decSTRT", "decEnd"),
                   labels = c("渐变段起点", "换道位置",
                              "渐变段终点\n(减速段起点)", "减速段终点")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100)) +
  theme(axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 12))

plot.zd5spotspeed


# 8 开始制动位置分布----
df.zd5appbrake <- CalcBatchAppBrake(data = df.zd5traj,
                                    kDriverID = c("S0101", "S0201", "S0301",
                                                  "S0401", "S0501", "S0601",
                                                  "S0701", "S0801", "S0901",
                                                  "S1001", "S1101", "S1201",
                                                  "S1301", "S1401", "S1501",
                                                  "S1601", "S1701", "S1801",
                                                  "S1901", "S2001"),
                                    is.disdecrease = FALSE,
                                    kPedalMod = 0.01,
                                    kCalcBrakeType = "BrakePedal")

plot.zd5appbrake <- ggplot(data = df.zd5appbrake,
                           aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appBrake)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 3100, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 2556, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(3100, 3100),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 2638, xend = 3100, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 2776, xend = 3100, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 2776, xend = 2776, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 2776, xmax = 3100, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 2638, xend = 2776, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(2673, 2708, 2743), xend = c(2776, 2776, 2776),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 2556, xend = 2638, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 2776, xend = 3100, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 2638, xend = 2776, y = 18.65, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 2636, xend = 2638, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 2556, xend = 2636, y = 11.45, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(2636, 2638, 2776), xend = c(2638, 2776, 3100),
           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 2921,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 2921, y = 1.5,
           fontface="bold", size = 4,
           label = "S5匝道洞口") +
  annotate(geom = "segment",
           x = 2556, xend = 2636, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2596, 2596), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "50.04m")) +
  annotate(geom = "segment",
           x = 2636, xend = 2776, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2706, 2706), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "142.65m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 3100),
                     breaks = c(500, 1500, 2556, 2636, 2776, 3100),
                     labels = c("RK2+330", "RK3+330", "", "",
                                "S2K0+142.248", "S5K0+466")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd5appbrake


# 9 开始加速位置分布----
df.zd5appgas <- CalcBatchAppGas(data = df.zd5traj,
                                kDriverID = c("S0101", "S0201", "S0301",
                                              "S0401", "S0501", "S0601",
                                              "S0701", "S0801", "S0901",
                                              "S1001", "S1101", "S1201",
                                              "S1301", "S1401", "S1501",
                                              "S1601", "S1701", "S1801",
                                              "S1901", "S2001"),
                                is.disdecrease = FALSE,
                                kPedalMod = 0.01)

plot.zd5appgas <- ggplot(data = df.zd5appgas,
                         aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appGasPedal)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 3100, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 2556, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(3100, 3100),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 2638, xend = 3100, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 2776, xend = 3100, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 2776, xend = 2776, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 2776, xmax = 3100, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 2638, xend = 2776, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(2673, 2708, 2743), xend = c(2776, 2776, 2776),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 2556, xend = 2638, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 2776, xend = 3100, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 2638, xend = 2776, y = 18.65, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 2636, xend = 2638, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 2556, xend = 2636, y = 11.45, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(2636, 2638, 2776), xend = c(2638, 2776, 3100),
           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 2921,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 2921, y = 1.5,
           fontface="bold", size = 4,
           label = "S5匝道洞口") +
  annotate(geom = "segment",
           x = 2556, xend = 2636, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2596, 2596), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "50.04m")) +
  annotate(geom = "segment",
           x = 2636, xend = 2776, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2706, 2706), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "142.65m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 3100),
                     breaks = c(500, 1500, 2556, 2636, 2776, 3100),
                     labels = c("RK2+330", "RK3+330", "", "",
                                "S2K0+142.248", "S5K0+466")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd5appgas


# 10 大减速度分布分析----
# 10.1 大减速度，个体比较----
df.zd5decoutlier1 <- CalcBatchAccOutliers(data = df.zd5traj,
                                          kDriverID = c("S0101", "S0201",
                                                        "S0301", "S0401",
                                                        "S0501", "S0601",
                                                        "S0701", "S0801",
                                                        "S0901", "S1001",
                                                        "S1101", "S1201",
                                                        "S1301", "S1401",
                                                        "S1501", "S1601",
                                                        "S1701", "S1801",
                                                        "S1901", "S2001"),
                                          kQuantile = 0.05,
                                          kAccLimit = NA,
                                          kOutliersType = "Dec",
                                          is.indv = TRUE)

plot.zd5decoutlier1 <- ggplot(data = df.zd5decoutlier1,
                              aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.5) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 3100, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 2556, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(3100, 3100),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 2638, xend = 3100, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 2776, xend = 3100, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 2776, xend = 2776, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 2776, xmax = 3100, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 2638, xend = 2776, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(2673, 2708, 2743), xend = c(2776, 2776, 2776),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 2556, xend = 2638, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 2776, xend = 3100, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 2638, xend = 2776, y = 18.65, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 2636, xend = 2638, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 2556, xend = 2636, y = 11.45, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(2636, 2638, 2776), xend = c(2638, 2776, 3100),
           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 2921,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 2921, y = 1.5,
           fontface="bold", size = 4,
           label = "S5匝道洞口") +
  annotate(geom = "segment",
           x = 2556, xend = 2636, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2596, 2596), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "50.04m")) +
  annotate(geom = "segment",
           x = 2636, xend = 2776, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2706, 2706), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "142.65m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 3100),
                     breaks = c(500, 1500, 2556, 2636, 2776, 3100),
                     labels = c("RK2+330", "RK3+330", "", "",
                                "S2K0+142.248", "S5K0+466")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd5decoutlier1


# 10.2 大减速度，群体比较----
df.zd5decoutlier2 <- CalcBatchAccOutliers(data = df.zd5traj,
                                          kDriverID = c("S0101", "S0201",
                                                        "S0301", "S0401",
                                                        "S0501", "S0601",
                                                        "S0701", "S0801",
                                                        "S0901", "S1001",
                                                        "S1101", "S1201",
                                                        "S1301", "S1401",
                                                        "S1501", "S1601",
                                                        "S1701", "S1801",
                                                        "S1901", "S2001"),
                                          kQuantile = NA,
                                          kAccLimit = -3.0,
                                          kOutliersType = "Dec",
                                          is.indv = FALSE)

plot.zd5decoutlier2 <- ggplot(data = df.zd5decoutlier2,
                              aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.5) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 3100, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 2556, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(3100, 3100),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 2638, xend = 3100, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 2776, xend = 3100, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 2776, xend = 2776, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 2776, xmax = 3100, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 2638, xend = 2776, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(2673, 2708, 2743), xend = c(2776, 2776, 2776),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 2556, xend = 2638, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 2776, xend = 3100, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 2638, xend = 2776, y = 18.65, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 2636, xend = 2638, y = 18.65, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 2556, xend = 2636, y = 11.45, yend = 18.65,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(2636, 2638, 2776), xend = c(2638, 2776, 3100),
           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 0, xmax = 2921,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 2921, y = 1.5,
           fontface="bold", size = 4,
           label = "S5匝道洞口") +
  annotate(geom = "segment",
           x = 2556, xend = 2636, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2596, 2596), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("渐变段", "50.04m")) +
  annotate(geom = "segment",
           x = 2636, xend = 2776, y = 3, yend = 3, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(2706, 2706), y = c(1.5, 4.5),
           fontface = "bold", size = 4,
           label = c("减速段", "142.65m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 3100),
                     breaks = c(500, 1500, 2556, 2636, 2776, 3100),
                     labels = c("RK2+330", "RK3+330", "", "",
                                "S2K0+142.248", "S5K0+466")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd5decoutlier2





