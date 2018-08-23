#--------------Code Description------------------------------------------------#
# Notes:
# ver1.0, date: 20180822, By MaoYan
# 
# 1.分析妈湾入口匝道匝道6的行驶速度、加速度、制动、加速等。（轿车only）
#------------------------------------------------------------------------------#


# 调用DataInput.R----
source(file = "E:/R/MaWan/MawanDS/DataInput.R", encoding = "utf-8")


# 调用R Packages----
library(ggplot2)
library(magrittr)
library(plyr)


# 数据切分----
df.zd6 <- subset(x = df.dsdata,
                 dsScen == "ZD6" | dsScen == "ZD6withTraffic")  # 匝道6数据

# 标定匝道6数据
df.zd6$disTravelled <- ifelse(test = df.zd6$roadName == "ZD_6",
                              df.zd6$disFromRoadStart,
                              df.zd6$disFromRoadStart - 2546 + 387)  # 桩号标定

df.zd6WOT <- subset(x = df.zd6, dsScen == "ZD6")  # 匝道6，无交通流
df.zd6WT <- subset(x = df.zd6, dsScen == "ZD6withTraffic")  # 匝道6，有交通流


# 1 匝道6，行驶速度分析----
# 1.1 轿车，行驶速度，无交通流----
plot.zd6WOTspeed <- ggplot(data = df.zd6WOT,
                           aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1300),
                     breaks = c(0, 361, 581, 661),
                     labels = c("S6K0+000", "S1K0+361", "", "")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100),
                     breaks = seq(0, 100, by = 20)) +
  annotate(geom = "rect", xmin = 220, xmax = 1300,
           ymin = 0, ymax = 100, alpha = 0.2) +
  annotate(geom = "text", x = 220, y = 100, label = "S6匝道隧道起点") +
  annotate(geom = "segment", x = 361, xend = 581, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 470, y = 100, label = "加速段") +
  annotate(geom = "segment", x = 581, xend = 661, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 621, y = 100, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd6WOTspeed


# 1.2 轿车，行驶速度，有交通流----
plot.zd6WTspeed <- ggplot(data = df.zd6WT,
                          aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1300),
                     breaks = c(0, 361, 581, 661),
                     labels = c("S6K0+000", "S1K0+361", "", "")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100),
                     breaks = seq(0, 100, by = 20)) +
  annotate(geom = "rect", xmin = 220, xmax = 1300,
           ymin = 0, ymax = 100, alpha = 0.2) +
  annotate(geom = "text", x = 220, y = 100, label = "S6匝道隧道起点") +
  annotate(geom = "segment", x = 361, xend = 581, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 470, y = 100, label = "加速段") +
  annotate(geom = "segment", x = 581, xend = 661, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 621, y = 100, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd6WTspeed


# 2 匝道6，轨迹分析----
# 2.1 轿车，轨迹，无交通流----
df.zd6WOTtraj <- CalcBatchTraj(data = df.zd6WOT,
                               kDriverID = c("S0101", "S0201", "S0301",
                                             "S0401", "S0501", "S0601",
                                             "S0701", "S0801", "S0901",
                                             "S1001", "S1101", "S1201",
                                             "S1301", "S1401", "S1501",
                                             "S1601", "S1701", "S1801",
                                             "S1901", "S2001"),
                               is.main2ramp = FALSE,
                               is.disdecrease = FALSE)

plot.zd6WOTtraj <- ggplot(data = df.zd6WOTtraj,
                          aes(x = disTravelled, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1300, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 687, xend = 1300, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(1300, 1300),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 0, xend = 497, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 0, xend = 361, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 361, xend = 361, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 361, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 361, xend = 497, y = 16.14, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(361, 361, 361), xend = c(395, 429, 463),
           y = c(12.62, 13.79, 14.96), yend = c(11.45, 11.45, 11.45),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 497, xend = 687, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 0, xend = 361, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 361, xend = 498, y = 23.35, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 498, xend = 607, y = 18.75, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 607, xend = 687, y = 18.75, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(0, 361, 498), xend = c(361, 498, 607),
           y = c(19.82, 19.82, 15.05), yend = c(19.82, 15.05, 15.05),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 220, xmax = 1300,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 220, y = 1.5,
           fontface="bold", size = 4,
           label = "S6匝道隧道起点") +
  annotate(geom = "segment",
           x = 361, xend = 581, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(475, 475), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 581, xend = 687, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(634, 634), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1300),
                     breaks = c(0, 361, 581, 687),
                     labels = c("S6K0+000", "S1K0+361", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd6WOTtraj


# 2.2 轿车，轨迹，有交通流----
df.zd6WTtraj <- CalcBatchTraj(data = df.zd6WT,
                              kDriverID = c("S0101", "S0201", "S0301",
                                            "S0401", "S0501", "S0601",
                                            "S0701", "S0801", "S0901",
                                            "S1001", "S1101", "S1201",
                                            "S1301", "S1401", "S1501",
                                            "S1601", "S1701", "S1801",
                                            "S1901", "S2001"),
                              is.main2ramp = FALSE,
                              is.disdecrease = FALSE)

plot.zd6WTtraj <- ggplot(data = df.zd6WTtraj,
                         aes(x = disTravelled, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1300, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 687, xend = 1300, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(1300, 1300),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 0, xend = 497, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 0, xend = 361, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 361, xend = 361, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 361, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 361, xend = 497, y = 16.14, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(361, 361, 361), xend = c(395, 429, 463),
           y = c(12.62, 13.79, 14.96), yend = c(11.45, 11.45, 11.45),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 497, xend = 687, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 0, xend = 361, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 361, xend = 498, y = 23.35, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 498, xend = 607, y = 18.75, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 607, xend = 687, y = 18.75, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(0, 361, 498), xend = c(361, 498, 607),
           y = c(19.82, 19.82, 15.05), yend = c(19.82, 15.05, 15.05),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 220, xmax = 1300,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 220, y = 1.5,
           fontface="bold", size = 4,
           label = "S6匝道隧道起点") +
  annotate(geom = "segment",
           x = 361, xend = 581, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(475, 475), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 581, xend = 687, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(634, 634), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1300),
                     breaks = c(0, 361, 581, 687),
                     labels = c("S6K0+000", "S1K0+361", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd6WTtraj


# 3 匝道6，车道跨越点分析----
# 3.1 轿车，车道跨越点，无交通流----
df.zd6WOTLCpoints <- CalcBatchLCPoint(data = df.zd6WOTtraj,
                                      kDriverID = c("S0101", "S0201", "S0301",
                                                    "S0401", "S0501", "S0601",
                                                    "S0701", "S0801", "S0901",
                                                    "S1001", "S1101", "S1201",
                                                    "S1301", "S1401", "S1501",
                                                    "S1601", "S1701", "S1801",
                                                    "S1901", "S2001"),
                                      kLatDis = 11.45,
                                      is.main2ramp = FALSE,
                                      is.disdecrease = FALSE)

plot.zd6WOTLCpoints <- ggplot(data = df.zd6WOTLCpoints,
                              aes(x = disTravelled, y = drivingTraj)) +
  geom_jitter(shape = 21, fill = "white", size = 2, stroke = 2) +
  # 平均换道位置
  geom_vline(xintercept = median(df.zd6WOTLCpoints$disTravelled),
             col = "red", size = 1) +
  # 平均换道位置标注
  annotate(geom = "text", x = median(df.zd6WOTLCpoints$disTravelled) + 200,
           y = 11.45 - 2,
           fontface = "bold", colour = "red", size = 4,
           label = c("换道位置中位值")) +
  annotate(geom = "segment",
           x = median(df.zd6WOTLCpoints$disTravelled),
           xend = median(df.zd6WOTLCpoints$disTravelled) + 80,
           y = 11.45, yend = 11.45 - 2,
           size = 1, colour = "red",
           arrow = arrow(ends = "last", angle = 30,
                         length = unit(0.2, "cm"))) +
  annotate(geom = "segment",
           x = median(df.zd6WOTLCpoints$disTravelled), xend = 581,
           y = 3.8, yend = 3.8, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(581, 581), y = c(2, 5),
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段起点", "8m")) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1300, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 687, xend = 1300, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(1300, 1300),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 0, xend = 497, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 0, xend = 361, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 361, xend = 361, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 361, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 361, xend = 497, y = 16.14, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(361, 361, 361), xend = c(395, 429, 463),
           y = c(12.62, 13.79, 14.96), yend = c(11.45, 11.45, 11.45),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 497, xend = 687, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 0, xend = 361, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 361, xend = 498, y = 23.35, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 498, xend = 607, y = 18.75, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 607, xend = 687, y = 18.75, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(0, 361, 498), xend = c(361, 498, 607),
           y = c(19.82, 19.82, 15.05), yend = c(19.82, 15.05, 15.05),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 220, xmax = 1300,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 220, y = 1.5,
           fontface="bold", size = 4,
           label = "S6匝道隧道起点") +
  annotate(geom = "segment",
           x = 361, xend = 581, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(475, 475), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 581, xend = 687, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(634, 634), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1300),
                     breaks = c(0, 361, 581, 687),
                     labels = c("S6K0+000", "S1K0+361", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd6WOTLCpoints


# 3.2 轿车，车道跨越点，有交通流----
df.zd6WTLCpoints <- CalcBatchLCPoint(data = df.zd6WTtraj,
                                     kDriverID = c("S0101", "S0201", "S0301",
                                                   "S0401", "S0501", "S0601",
                                                   "S0701", "S0801", "S0901",
                                                   "S1001", "S1101", "S1201",
                                                   "S1301", "S1401", "S1501",
                                                   "S1601", "S1701", "S1801",
                                                   "S1901", "S2001"),
                                     kLatDis = 11.45,
                                     is.main2ramp = FALSE,
                                     is.disdecrease = FALSE)

plot.zd6WTLCpoints <- ggplot(data = df.zd6WTLCpoints,
                             aes(x = disTravelled, y = drivingTraj)) +
  geom_jitter(shape = 21, fill = "white", size = 2, stroke = 2) +
  # 平均换道位置
  geom_vline(xintercept = median(df.zd6WTLCpoints$disTravelled),
             col = "red", size = 1) +
  # 平均换道位置标注
  annotate(geom = "text", x = median(df.zd6WTLCpoints$disTravelled) + 200,
           y = 11.45 - 2,
           fontface = "bold", colour = "red", size = 4,
           label = c("换道位置中位值")) +
  annotate(geom = "segment",
           x = median(df.zd6WTLCpoints$disTravelled),
           xend = median(df.zd6WTLCpoints$disTravelled) + 80,
           y = 11.45, yend = 11.45 - 2,
           size = 1, colour = "red",
           arrow = arrow(ends = "last", angle = 30,
                         length = unit(0.2, "cm"))) +
  annotate(geom = "segment",
           x = median(df.zd6WTLCpoints$disTravelled), xend = 581,
           y = 3.8, yend = 3.8, size = 1, colour = "blue",
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(581, 581), y = c(2, 5),
           fontface = "bold", size = 4, colour = "blue",
           label = c("距离渐变段起点", "30m")) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1300, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 687, xend = 1300, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(1300, 1300),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 0, xend = 497, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 0, xend = 361, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 361, xend = 361, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 361, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 361, xend = 497, y = 16.14, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(361, 361, 361), xend = c(395, 429, 463),
           y = c(12.62, 13.79, 14.96), yend = c(11.45, 11.45, 11.45),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 497, xend = 687, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 0, xend = 361, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 361, xend = 498, y = 23.35, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 498, xend = 607, y = 18.75, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 607, xend = 687, y = 18.75, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(0, 361, 498), xend = c(361, 498, 607),
           y = c(19.82, 19.82, 15.05), yend = c(19.82, 15.05, 15.05),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 220, xmax = 1300,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 220, y = 1.5,
           fontface="bold", size = 4,
           label = "S6匝道隧道起点") +
  annotate(geom = "segment",
           x = 361, xend = 581, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(475, 475), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 581, xend = 687, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(634, 634), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1300),
                     breaks = c(0, 361, 581, 687),
                     labels = c("S6K0+000", "S1K0+361", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd6WTLCpoints


# 4 匝道6，加速度分析----
# 4.1 轿车，加速度，无交通流----
plot.zd6WOTacc <- ggplot(data = df.zd6WOT,
                         aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = c(-2, 2), colour = "orange",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1300),
                     breaks = c(0, 361, 581, 661),
                     labels = c("S6K0+000", "S1K0+361", "", "")) +
  scale_y_continuous(name = "加速度（m/s2）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 220, xmax = 1300,
           ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 220, y = 5, label = "S6匝道隧道起点") +
  annotate(geom = "segment", x = 361, xend = 581, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 470, y = 5, label = "加速段") +
  annotate(geom = "segment", x = 581, xend = 661, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 621, y = 5, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd6WOTacc


# 4.2 轿车，加速度，有交通流----
plot.zd6WTacc <- ggplot(data = df.zd6WT,
                        aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = c(-2, 2), colour = "orange",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1300),
                     breaks = c(0, 361, 581, 661),
                     labels = c("S6K0+000", "S1K0+361", "", "")) +
  scale_y_continuous(name = "加速度（m/s2）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 220, xmax = 1300,
           ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 220, y = 5, label = "S6匝道隧道起点") +
  annotate(geom = "segment", x = 361, xend = 581, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 470, y = 5, label = "加速段") +
  annotate(geom = "segment", x = 581, xend = 661, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 621, y = 5, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd6WTacc


# 5 匝道6， 制动踏板位移分析----
# 5.1 轿车，制动踏板位移，无交通流----
plot.zd6WOTbrakepedal <- ggplot(data = df.zd6WOT,
                                aes(x = disTravelled, y = appBrake)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1300),
                     breaks = c(0, 361, 581, 661),
                     labels = c("S6K0+000", "S1K0+361", "", "")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 220, xmax = 1300,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 220, y = 1, label = "S6匝道隧道起点") +
  annotate(geom = "segment", x = 361, xend = 581, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 470, y = 1, label = "加速段") +
  annotate(geom = "segment", x = 581, xend = 661, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 621, y = 1, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd6WOTbrakepedal


# 5.2 轿车，制动踏板位移，有交通流----
plot.zd6WTbrakepedal <- ggplot(data = df.zd6WT,
                               aes(x = disTravelled, y = appBrake)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1300),
                     breaks = c(0, 361, 581, 661),
                     labels = c("S6K0+000", "S1K0+361", "", "")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 220, xmax = 1300,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 220, y = 1, label = "S6匝道隧道起点") +
  annotate(geom = "segment", x = 361, xend = 581, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 470, y = 1, label = "加速段") +
  annotate(geom = "segment", x = 581, xend = 661, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 621, y = 1, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd6WTbrakepedal


# 6 匝道6，油门踏板位移分析----
# 6.1 轿车，油门踏板位移，无交通流----
plot.zd6WOTgaspedal <- ggplot(data = df.zd6WOT,
                              aes(x = disTravelled, y = appGasPedal)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1300),
                     breaks = c(0, 361, 581, 661),
                     labels = c("S6K0+000", "S1K0+361", "", "")) +
  scale_y_continuous(name = "油门踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 220, xmax = 1300,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 220, y = 1, label = "S6匝道隧道起点") +
  annotate(geom = "segment", x = 361, xend = 581, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 470, y = 1, label = "加速段") +
  annotate(geom = "segment", x = 581, xend = 661, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 621, y = 1, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd6WOTgaspedal


# 6.2 轿车，油门踏板位移，有交通流----
plot.zd6WTgaspedal <- ggplot(data = df.zd6WT,
                             aes(x = disTravelled, y = appGasPedal)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1300),
                     breaks = c(0, 361, 581, 661),
                     labels = c("S6K0+000", "S1K0+361", "", "")) +
  scale_y_continuous(name = "油门踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 220, xmax = 1300,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 220, y = 1, label = "S6匝道隧道起点") +
  annotate(geom = "segment", x = 361, xend = 581, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 470, y = 1, label = "加速段") +
  annotate(geom = "segment", x = 581, xend = 661, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 621, y = 1, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd6WTgaspedal


# 7 关键位置速度分布分析----
# 7.1 轿车，关键位置速度，无交通流----
df.zd6WOTspotspeed <- CalcBatchSpotSpeed(data = df.zd6WOT,
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
                                         kDis = c(361, 581, 661),
                                         is.disdecrease = FALSE,
                                         kTag = c("accSTRT",
                                                  "accEnd/transSTRT",
                                                  "transEnd"),
                                         kDisType = "Dis")

df.zd6WOTspotspeed <- subset(df.zd6WOTLCpoints,
                             select = c("rowNo", "disTravelled",
                                        "speedKMH", "driverID")) %>%
  transform(disTag = factor("LCPoint")) %>%
  rbind(df.zd6WOTspotspeed)

df.zd6WOTspotspeed <- transform(df.zd6WOTspotspeed,
                                scenType = "WOT")  # 增加对比场景标签

# 特征点速度统计量计算
plotdf.zd6WOTspotspeed <- ddply(df.zd6WOTspotspeed,
                                .(disTag),
                                summarize,
                                meanSpeed = mean(speedKMH),
                                sdSpeed = sd(speedKMH))

plot.zd6WOTspotspeed <- ggplot(data = plotdf.zd6WOTspotspeed,
                               aes(x = disTag, y = meanSpeed)) +
  geom_bar(stat = "identity", width = 0.15) +
  geom_errorbar(aes(ymax = meanSpeed + sdSpeed, ymin = meanSpeed - sdSpeed),
                width = 0.1, size =1) +
  scale_x_discrete(limits = c("accSTRT", "accEnd/transSTRT",
                              "LCPoint", "transEnd"),
                   labels = c("加速段起点", "加速段终点\n(渐变段起点)",
                              "换道位置", "渐变段终点")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100)) +
  theme(axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 12))

plot.zd6WOTspotspeed


# 7.2 轿车，关键位置速度，有交通流----
df.zd6WTspotspeed <- CalcBatchSpotSpeed(data = df.zd6WT,
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
                                        kDis = c(361, 581, 661),
                                        is.disdecrease = FALSE,
                                        kTag = c("accSTRT",
                                                 "accEnd/transSTRT",
                                                 "transEnd"),
                                        kDisType = "Dis")

df.zd6WTspotspeed <- subset(df.zd6WTLCpoints,
                            select = c("rowNo", "disTravelled",
                                       "speedKMH", "driverID")) %>%
  transform(disTag = factor("LCPoint")) %>%
  rbind(df.zd6WTspotspeed)

df.zd6WTspotspeed <- transform(df.zd6WTspotspeed,
                               scenType = "WT")  # 增加对比场景标签

# 特征点速度统计量计算
plotdf.zd6WTspotspeed <- ddply(df.zd6WTspotspeed,
                               .(disTag),
                               summarize,
                               meanSpeed = mean(speedKMH),
                               sdSpeed = sd(speedKMH))

plot.zd6WTspotspeed <- ggplot(data = plotdf.zd6WTspotspeed,
                              aes(x = disTag, y = meanSpeed)) +
  geom_bar(stat = "identity", width = 0.15) +
  geom_errorbar(aes(ymax = meanSpeed + sdSpeed, ymin = meanSpeed - sdSpeed),
                width = 0.1, size =1) +
  scale_x_discrete(limits = c("accSTRT", "accEnd/transSTRT",
                              "LCPoint", "transEnd"),
                   labels = c("加速段起点", "加速段终点\n(渐变段起点)",
                              "换道位置", "渐变段终点")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100)) +
  theme(axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 12))

plot.zd6WTspotspeed


# 7.3 轿车，关键位置速度，无交通流 vs 有交通流----
# 特征点速度统计量计算，WOT vs WT
df.zd6spotspeed <- rbind(df.zd6WOTspotspeed,
                         df.zd6WTspotspeed)

plotdf.zd6spotspeed <- ddply(df.zd6spotspeed,
                             .(disTag, scenType),
                             summarize,
                             meanSpeed = mean(speedKMH),
                             sdSpeed = sd(speedKMH))

plot.zd6spotspeed <- ggplot(data = plotdf.zd6spotspeed,
                            aes(x = disTag, y = meanSpeed,
                                fill = scenType)) +
  geom_bar(stat = "identity", position = position_dodge(),
           width = 0.3, colour = "black") +
  geom_errorbar(aes(ymax = meanSpeed + sdSpeed, ymin = meanSpeed - sdSpeed),
                position = position_dodge(0.3), width = 0.2) +
  scale_x_discrete(limits = c("accSTRT", "accEnd/transSTRT",
                              "LCPoint", "transEnd"),
                   labels = c("加速段起点", "加速段终点\n(渐变段起点)",
                              "换道位置", "渐变段终点")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100)) +
  scale_fill_discrete(breaks=c("WOT", "WT"),
                      labels=c("w/o Traffic", "w/ Traffic")) +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(face = "bold", size = 10),
        legend.key.size = unit(0.3, "cm"),
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 12))

plot.zd6spotspeed


# 8 开始制动位置分布----
# 8.1 轿车，开始制动位置分布，无交通流----
df.zd6WOTappbrake <- CalcBatchAppBrake(data = df.zd6WOTtraj,
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

plot.zd6WOTappbrake <- ggplot(data = df.zd6WOTappbrake,
                              aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appBrake)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1300, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 687, xend = 1300, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(1300, 1300),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 0, xend = 497, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 0, xend = 361, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 361, xend = 361, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 361, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 361, xend = 497, y = 16.14, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(361, 361, 361), xend = c(395, 429, 463),
           y = c(12.62, 13.79, 14.96), yend = c(11.45, 11.45, 11.45),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 497, xend = 687, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 0, xend = 361, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 361, xend = 498, y = 23.35, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 498, xend = 607, y = 18.75, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 607, xend = 687, y = 18.75, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(0, 361, 498), xend = c(361, 498, 607),
           y = c(19.82, 19.82, 15.05), yend = c(19.82, 15.05, 15.05),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 220, xmax = 1300,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 220, y = 1.5,
           fontface="bold", size = 4,
           label = "S6匝道隧道起点") +
  annotate(geom = "segment",
           x = 361, xend = 581, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(475, 475), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 581, xend = 687, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(634, 634), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1300),
                     breaks = c(0, 361, 581, 687),
                     labels = c("S6K0+000", "S1K0+361", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd6WOTappbrake


# 8.2 轿车，开始制动位置分布，有交通流----
df.zd6WTappbrake <- CalcBatchAppBrake(data = df.zd6WTtraj,
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

plot.zd6WTappbrake <- ggplot(data = df.zd6WTappbrake,
                             aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appBrake)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1300, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 687, xend = 1300, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(1300, 1300),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 0, xend = 497, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 0, xend = 361, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 361, xend = 361, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 361, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 361, xend = 497, y = 16.14, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(361, 361, 361), xend = c(395, 429, 463),
           y = c(12.62, 13.79, 14.96), yend = c(11.45, 11.45, 11.45),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 497, xend = 687, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 0, xend = 361, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 361, xend = 498, y = 23.35, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 498, xend = 607, y = 18.75, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 607, xend = 687, y = 18.75, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(0, 361, 498), xend = c(361, 498, 607),
           y = c(19.82, 19.82, 15.05), yend = c(19.82, 15.05, 15.05),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 220, xmax = 1300,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 220, y = 1.5,
           fontface="bold", size = 4,
           label = "S6匝道隧道起点") +
  annotate(geom = "segment",
           x = 361, xend = 581, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(475, 475), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 581, xend = 687, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(634, 634), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1300),
                     breaks = c(0, 361, 581, 687),
                     labels = c("S6K0+000", "S1K0+361", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd6WTappbrake


# 9 开始加速位置分布----
# 9.1 轿车，开始制动位置分布，无交通流----
df.zd6WOTappgas <- CalcBatchAppGas(data = df.zd6WOTtraj,
                                   kDriverID = c("S0101", "S0201", "S0301",
                                                 "S0401", "S0501", "S0601",
                                                 "S0701", "S0801", "S0901",
                                                 "S1001", "S1101", "S1201",
                                                 "S1301", "S1401", "S1501",
                                                 "S1601", "S1701", "S1801",
                                                 "S1901", "S2001"),
                                   is.disdecrease = FALSE,
                                   kPedalMod = 0.01)

plot.zd6WOTappgas <- ggplot(data = df.zd6WOTappgas,
                            aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appGasPedal)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1300, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 687, xend = 1300, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(1300, 1300),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 0, xend = 497, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 0, xend = 361, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 361, xend = 361, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 361, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 361, xend = 497, y = 16.14, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(361, 361, 361), xend = c(395, 429, 463),
           y = c(12.62, 13.79, 14.96), yend = c(11.45, 11.45, 11.45),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 497, xend = 687, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 0, xend = 361, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 361, xend = 498, y = 23.35, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 498, xend = 607, y = 18.75, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 607, xend = 687, y = 18.75, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(0, 361, 498), xend = c(361, 498, 607),
           y = c(19.82, 19.82, 15.05), yend = c(19.82, 15.05, 15.05),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 220, xmax = 1300,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 220, y = 1.5,
           fontface="bold", size = 4,
           label = "S6匝道隧道起点") +
  annotate(geom = "segment",
           x = 361, xend = 581, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(475, 475), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 581, xend = 687, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(634, 634), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1300),
                     breaks = c(0, 361, 581, 687),
                     labels = c("S6K0+000", "S1K0+361", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd6WOTappgas


# 9.2 轿车，开始制动位置分布，有交通流----
df.zd6WTappgas <- CalcBatchAppGas(data = df.zd6WTtraj,
                                  kDriverID = c("S0101", "S0201", "S0301",
                                                "S0401", "S0501", "S0601",
                                                "S0701", "S0801", "S0901",
                                                "S1001", "S1101", "S1201",
                                                "S1301", "S1401", "S1501",
                                                "S1601", "S1701", "S1801",
                                                "S1901", "S2001"),
                                  is.disdecrease = FALSE,
                                  kPedalMod = 0.01)

plot.zd6WTappgas <- ggplot(data = df.zd6WTappgas,
                           aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = appGasPedal)) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1300, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 687, xend = 1300, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(1300, 1300),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 0, xend = 497, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 0, xend = 361, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 361, xend = 361, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 361, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 361, xend = 497, y = 16.14, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(361, 361, 361), xend = c(395, 429, 463),
           y = c(12.62, 13.79, 14.96), yend = c(11.45, 11.45, 11.45),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 497, xend = 687, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 0, xend = 361, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 361, xend = 498, y = 23.35, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 498, xend = 607, y = 18.75, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 607, xend = 687, y = 18.75, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(0, 361, 498), xend = c(361, 498, 607),
           y = c(19.82, 19.82, 15.05), yend = c(19.82, 15.05, 15.05),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 220, xmax = 1300,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 220, y = 1.5,
           fontface="bold", size = 4,
           label = "S6匝道隧道起点") +
  annotate(geom = "segment",
           x = 361, xend = 581, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(475, 475), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 581, xend = 687, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(634, 634), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1300),
                     breaks = c(0, 361, 581, 687),
                     labels = c("S6K0+000", "S1K0+361", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd6WTappgas


# 10 大减速度分布分析----
# 10.1 轿车，大减速度，无交通流----
# 10.1.1 轿车，大减速度，无交通流，个体比较----
df.zd6WOTdecoutlier1 <- CalcBatchAccOutliers(data = df.zd6WOTtraj,
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

plot.zd6WOTdecoutlier1 <- ggplot(data = df.zd6WOTdecoutlier1,
                                 aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.2)+
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1300, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 687, xend = 1300, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(1300, 1300),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 0, xend = 497, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 0, xend = 361, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 361, xend = 361, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 361, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 361, xend = 497, y = 16.14, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(361, 361, 361), xend = c(395, 429, 463),
           y = c(12.62, 13.79, 14.96), yend = c(11.45, 11.45, 11.45),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 497, xend = 687, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 0, xend = 361, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 361, xend = 498, y = 23.35, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 498, xend = 607, y = 18.75, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 607, xend = 687, y = 18.75, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(0, 361, 498), xend = c(361, 498, 607),
           y = c(19.82, 19.82, 15.05), yend = c(19.82, 15.05, 15.05),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 220, xmax = 1300,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 220, y = 1.5,
           fontface="bold", size = 4,
           label = "S6匝道隧道起点") +
  annotate(geom = "segment",
           x = 361, xend = 581, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(475, 475), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 581, xend = 687, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(634, 634), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1300),
                     breaks = c(0, 361, 581, 687),
                     labels = c("S6K0+000", "S1K0+361", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd6WOTdecoutlier1


# 10.1.2 轿车，大减速度，无交通流，群体比较----
df.zd6WOTdecoutlier2 <- CalcBatchAccOutliers(data = df.zd6WOTtraj,
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

plot.zd6WOTdecoutlier2 <- ggplot(data = df.zd6WOTdecoutlier2,
                                 aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.2)+
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1300, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 687, xend = 1300, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(1300, 1300),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 0, xend = 497, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 0, xend = 361, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 361, xend = 361, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 361, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 361, xend = 497, y = 16.14, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(361, 361, 361), xend = c(395, 429, 463),
           y = c(12.62, 13.79, 14.96), yend = c(11.45, 11.45, 11.45),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 497, xend = 687, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 0, xend = 361, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 361, xend = 498, y = 23.35, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 498, xend = 607, y = 18.75, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 607, xend = 687, y = 18.75, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(0, 361, 498), xend = c(361, 498, 607),
           y = c(19.82, 19.82, 15.05), yend = c(19.82, 15.05, 15.05),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 220, xmax = 1300,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 220, y = 1.5,
           fontface="bold", size = 4,
           label = "S6匝道隧道起点") +
  annotate(geom = "segment",
           x = 361, xend = 581, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(475, 475), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 581, xend = 687, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(634, 634), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1300),
                     breaks = c(0, 361, 581, 687),
                     labels = c("S6K0+000", "S1K0+361", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd6WOTdecoutlier2


# 10.2 轿车，大减速度，有交通流----
# 10.2.1 轿车，大减速度，有交通流，个体比较----
df.zd6WTdecoutlier1 <- CalcBatchAccOutliers(data = df.zd6WTtraj,
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

plot.zd6WTdecoutlier1 <- ggplot(data = df.zd6WTdecoutlier1,
                                aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.2)+
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1300, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 687, xend = 1300, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(1300, 1300),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 0, xend = 497, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 0, xend = 361, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 361, xend = 361, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 361, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 361, xend = 497, y = 16.14, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(361, 361, 361), xend = c(395, 429, 463),
           y = c(12.62, 13.79, 14.96), yend = c(11.45, 11.45, 11.45),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 497, xend = 687, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 0, xend = 361, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 361, xend = 498, y = 23.35, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 498, xend = 607, y = 18.75, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 607, xend = 687, y = 18.75, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(0, 361, 498), xend = c(361, 498, 607),
           y = c(19.82, 19.82, 15.05), yend = c(19.82, 15.05, 15.05),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 220, xmax = 1300,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 220, y = 1.5,
           fontface="bold", size = 4,
           label = "S6匝道隧道起点") +
  annotate(geom = "segment",
           x = 361, xend = 581, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(475, 475), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 581, xend = 687, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(634, 634), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1300),
                     breaks = c(0, 361, 581, 687),
                     labels = c("S6K0+000", "S1K0+361", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd6WTdecoutlier1


# 10.2.2 轿车，大减速度，有交通流，群体比较----
df.zd6WTdecoutlier2 <- CalcBatchAccOutliers(data = df.zd6WTtraj,
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

plot.zd6WTdecoutlier2 <- ggplot(data = df.zd6WTdecoutlier2,
                                aes(x = disTravelled, y = drivingTraj)) +
  geom_point(aes(colour = factor(driverID), size = accZMS2), alpha = 0.2)+
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 1300, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 687, xend = 1300, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(1300, 1300),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 0, xend = 497, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 0, xend = 361, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 361, xend = 361, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 0, xmax = 361, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 361, xend = 497, y = 16.14, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(361, 361, 361), xend = c(395, 429, 463),
           y = c(12.62, 13.79, 14.96), yend = c(11.45, 11.45, 11.45),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 497, xend = 687, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 0, xend = 361, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线3
  annotate(geom = "segment", x = 361, xend = 498, y = 23.35, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线4
  annotate(geom = "segment", x = 498, xend = 607, y = 18.75, yend = 18.75,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线5
  annotate(geom = "segment", x = 607, xend = 687, y = 18.75, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道分隔线
  annotate(geom = "segment",
           x = c(0, 361, 498), xend = c(361, 498, 607),
           y = c(19.82, 19.82, 15.05), yend = c(19.82, 15.05, 15.05),
           size = 1, colour = "black", linetype = "dashed") +
  # 隧道起点、加速段、渐变段位置
  annotate(geom = "rect", xmin = 220, xmax = 1300,
           ymin = 0, ymax = 23.35, alpha = 0.2) +
  annotate(geom = "text", x = 220, y = 1.5,
           fontface="bold", size = 4,
           label = "S6匝道隧道起点") +
  annotate(geom = "segment",
           x = 361, xend = 581, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(475, 475), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("加速段", "220m")) +
  annotate(geom = "segment",
           x = 581, xend = 687, y = 23.35, yend = 23.35, size = 1,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = c(634, 634), y = c(22, 24),
           fontface = "bold", size = 4,
           label = c("渐变段", "80m")) +
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 1300),
                     breaks = c(0, 361, 581, 687),
                     labels = c("S6K0+000", "S1K0+361", "", "")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zd6WTdecoutlier2







