#--------------Code Description------------------------------------------------#
# Notes:
# ver1.0, date: 20180823, By MaoYan
# 
# 1.分析妈湾出口匝道匝道A的行驶速度、加速度、制动、加速等。
#------------------------------------------------------------------------------#


# 调用DataInput.R----
source(file = "E:/R/MaWan/MawanDS/DataInput.R", encoding = "utf-8")


# 调用R Packages----
library(ggplot2)
library(magrittr)
library(plyr)


# 数据切分----
df.zda <- subset(x = df.dsdata, dsScen == "ZDA")  # 匝道A数据

df.zda$disTravelled <- ifelse(test = df.zda$roadName == "MainRD_Right",
                              df.zda$disFromRoadStart - 4000,
                              df.zda$disFromRoadStart + 3644.64)

df.zdasedan <- subset(x = df.zda, dsVehicleType == "Sedan")  # 匝道A，轿车
df.zdatruck <- subset(x = df.zda, dsVehicleType == "Truck")  # 匝道A，货车


# 1 匝道A，行驶速度分析----
# 1.1 轿车，行驶速度----
df.zdasedan <- subset(df.zdasedan, driverID != "S1301")  # 提出严重超速样本

plot.zdasedanspeed <- ggplot(data = df.zdasedan,
                             aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(500, 4100),
                     breaks = c(500, 1500, 2500, 3445, 3525, 3645, 4100),
                     labels = c("RK3+530", "RK4+530", "RK5+530", "", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100),
                     breaks = seq(0, 100, by = 20)) +
  annotate(geom = "rect", xmin = 500, xmax = 3902,
           ymin = 0, ymax = 100, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 100, label = "A匝道洞口") +
  annotate(geom = "segment", x = 3445, xend = 3525, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 3485, y = 100, label = "渐变段") +
  annotate(geom = "segment", x = 3525, xend = 3645, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 3585, y = 100, label = "减速段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zdasedanspeed


# 1.2 货车，行驶速度----
plot.zdatruckspeed <- ggplot(data = df.zdatruck,
                             aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(500, 4100),
                     breaks = c(500, 1500, 2500, 3445, 3525, 3645, 4100),
                     labels = c("RK3+530", "RK4+530", "RK5+530", "", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100),
                     breaks = seq(0, 100, by = 20)) +
  annotate(geom = "rect", xmin = 500, xmax = 3902,
           ymin = 0, ymax = 100, alpha = 0.2) +
  annotate(geom = "text", x = 3902, y = 100, label = "A匝道洞口") +
  annotate(geom = "segment", x = 3445, xend = 3525, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 3485, y = 100, label = "渐变段") +
  annotate(geom = "segment", x = 3525, xend = 3645, y = 95, yend = 95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 3585, y = 100, label = "减速段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zdatruckspeed


# 2 匝道A，轨迹分析----
# 2.1 轿车，轨迹1----
df.zdasedantraj <- CalcBatchTraj(data = df.zdasedan,
                                 kDriverID = c("S0101", "S0201", "S0301",
                                               "S0401", "S0501", "S0601",
                                               "S0701", "S0801", "S0901",
                                               "S1001", "S1101", "S1201",
                                               "S1401", "S1501", "S1601",
                                               "S1701", "S1801", "S1901",
                                               "S2001"),
                                 is.main2ramp = TRUE,
                                 is.disdecrease = FALSE)

plot.zdasedantraj <- ggplot(data = df.zdasedantraj,
                            aes(x = disTravelled, y = drivingTraj)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  # 主线车道边缘线1
  annotate(geom = "segment", x = 0, xend = 4100, y = 0, yend = 0,
           size = 3, colour = "black", linetype = "solid") +
  # 主线车道边缘线2
  annotate(geom = "segment", x = 0, xend = 3445, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 主线车道车道分隔线
  annotate(geom = "segment",
           x = c(0, 0), xend = c(4100, 4100),
           y = c(3.82, 7.64), yend = c(3.82, 7.64),
           size = 1, colour = "black", linetype = "dashed") +
  # 主线车道边缘线3&渠化线1
  annotate(geom = "segment", x = 3520, xend = 4100, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "solid") +
  # 匝道车道边缘线1
  annotate(geom = "segment", x = 3645, xend = 4100, y = 16.14, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 鼻端
  annotate(geom = "segment", x = 3645, xend = 3645, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 主线&匝道分隔
  annotate(geom = "rect", xmin = 3645, xmax = 4100, ymin = 11.45, ymax = 16.14,
           colour = "black") +
  # 渠化线2
  annotate(geom = "segment", x = 3520, xend = 3645, y = 11.45, yend = 16.14,
           size = 1, colour = "black", linetype = "solid") +
  # 其他渠化线
  annotate(geom = "segment",
           x = c(3551, 3582, 3613), xend = c(3645, 3645, 3645),
           y = c(11.45, 11.45, 11.45), yend = c(14.96, 13.79, 12.62),
           size = 1, colour = "black", linetype = "solid") +
  # 匝道与主线车道分割线
  annotate(geom = "segment", x = 3445, xend = 3520, y = 11.45, yend = 11.45,
           size = 1, colour = "black", linetype = "dashed") +
  # 匝道车道边缘线2
  annotate(geom = "segment", x = 3645, xend = 4100, y = 23.35, yend = 23.35,
           size = 1, colour = "black", linetype = "solid") +
#  # 匝道车道边缘线3
#  annotate(geom = "segment", x = 2526, xend = 2621, y = 18.65, yend = 23.35,
#           size = 1, colour = "black", linetype = "solid") +
#  # 匝道车道边缘线4
#  annotate(geom = "segment", x = 2478, xend = 2526, y = 18.65, yend = 18.65,
#           size = 1, colour = "black", linetype = "solid") +
#  # 匝道车道边缘线5
#  annotate(geom = "segment", x = 2428, xend = 2478, y = 11.45, yend = 18.65,
#           size = 1, colour = "black", linetype = "solid") +
#  # 匝道车道分隔线
#  annotate(geom = "segment",
#           x = c(2478, 2526, 2621), xend = c(2526, 2621, 3600),
#           y = c(15.05, 15.05, 19.82), yend = c(15.05, 19.82, 19.82),
#           size = 1, colour = "black", linetype = "dashed") +
#  # 隧道起点、加速段、渐变段位置
#  annotate(geom = "rect", xmin = 0, xmax = 3410,
#           ymin = 0, ymax = 23.35, alpha = 0.2) +
#  annotate(geom = "text", x = 3480, y = 1.5,
#           fontface="bold", size = 4,
#           label = "S2匝道洞口") +
#  annotate(geom = "segment",
#           x = 2428, xend = 2478, y = 3, yend = 3, size = 1,
#           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
#  annotate(geom = "text", x = c(2400, 2400), y = c(1.5, 4.5),
#           fontface = "bold", size = 4,
#           label = c("渐变段", "50.04m")) +
#  annotate(geom = "segment",
#           x = 2478, xend = 2621, y = 3, yend = 3, size = 1,
#           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm"))) +
#  annotate(geom = "text", x = c(2600, 2600), y = c(1.5, 4.5),
#           fontface = "bold", size = 4,
#           label = c("减速段", "142.65m")) +
  
  # 坐标轴及图例等修改
  scale_x_continuous(name = NULL, limits = c(0, 4100),
                     breaks = c(500, 1500, 2500, 3445, 3525, 3645, 4100),
                     labels = c("RK3+530", "RK4+530", "RK5+530", "", "",
                                "AK0+199.102", "AK0+654")) +
  scale_y_reverse(name = NULL, breaks = NULL, labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed",
                                        colour = "black", size = 0.5),
        panel.grid.minor = element_blank())

plot.zdasedantraj


# 2.2 轿车，轨迹2----
# 2.3 货车，轨迹1----
# 2.4 货车，轨迹2----


# 3 匝道A，车道跨越点分析----
# 3.1 轿车，车道跨越点----
# 3.2 货车，车道跨越点L3->L2----
# 3.3 货车，车道跨越点L2->L1----
# 3.4 货车，车道跨越点L1->Ramp----



# 4 匝道A，加速度分析----
# 4.1 轿车，加速度----
# 4.2 货车，加速度----



# 5 匝道A，制动踏板位移分析----
# 5.1 轿车，制动踏板位移----
# 5.2 货车，制动踏板位移----



# 6 匝道A，油门踏板位移分析----
# 6.1 轿车，油门踏板位移----
# 6.2 货车，油门踏板位移----



# 7 匝道A，关键位置速度分布分析----
# 7.1 轿车，关键位置速度分布----
# 7.2 货车，关键位置速度分布----



# 8 匝道A，开始制动位置分布分析----
# 8.1 轿车，开始制动位置分布----
# 8.2 货车，开始制动位置分布----



# 9 匝道A，开始加速位置分布分析----
# 9.1 轿车，开始加速位置分布----
# 9.2 货车，开始加速位置分布----



# 10 匝道A，大减速度分布分析----
# 10.1 轿车，大减速度分布----
# 10.1.1 轿车，大减速度，个体比较----
# 10.1.2 轿车，大减速度，群体比较----
# 10.2 货车，大减速度分布----
# 10.2.1 货车，大减速度，个体比较----
# 10.2.2 货车，大减速度，群体比较----