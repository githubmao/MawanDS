#--------------Code Description------------------------------------------------#
# Notes:
# ver1.0, date: 20180705, By MaoYan
# 
# 1.分析妈湾入口匝道匝道1的行驶速度、加速度、制动、加速等。
#------------------------------------------------------------------------------#


# 调用DataInput.R----
source(file = "E:/R/MaWan/MawanDS/DataInput.R", encoding = "utf-8")


# 调用R Packages----
library(ggplot2)


# 数据切分----
df.zd1 <- subset(x = df.dsdata,
                 dsScen == "ZD1" | dsScen == "ZD1withTraffic")  # 匝道1数据

# 标定匝道1数据
df.zd1$disTravelled <- ifelse(test = df.zd1$roadName == "ZD_1/3",
                              df.zd1$disFromRoadStart,
                              df.zd1$disFromRoadStart + 582 - 2975)

df.zd1WOT <- subset(x = df.zd1, dsScen == "ZD1")  # 匝道1，无交通流
df.zd1WT <- subset(x = df.zd1, dsScen == "ZD1withTraffic")  # 匝道1，有交通流

df.zd1WOTsedan <- subset(x = df.zd1WOT,
                         dsVehicleType == "Sedan")  # 匝道1轿车，无交通流
df.zd1WOTtruck <- subset(x = df.zd1WOT,
                         dsVehicleType == "Truck")  # 匝道1货车，无交通流

df.zd1WTsedan <- subset(x = df.zd1WT,
                        dsVehicleType == "Sedan")  # 匝道1轿车，有交通流
df.zd1WTtruck <- subset(x = df.zd1WT,
                        dsVehicleType == "Truck")  # 匝道1货车，有交通流


# 1 匝道1，行驶速度分析----
# 1.1 轿车，行驶速度----
# 1.1.1 轿车，行驶速度，无交通流----
plot.zd1WOTsedanspeed <- ggplot(data = df.zd1WOTsedan,
                                aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 120),
                     breaks = seq(0, 120, by = 20)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 120, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 120, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 115, yend = 115,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 120, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 115, yend = 115,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 120, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WOTsedanspeed


# 1.1.2 轿车，行驶速度，有交通流----
plot.zd1WTsedanspeed <- ggplot(data = df.zd1WTsedan,
                               aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 120),
                     breaks = seq(0, 120, by = 20)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 120, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 120, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 115, yend = 115,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 120, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 115, yend = 115,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 120, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WTsedanspeed

# 1.2 货车，行驶速度----
plot.zd1WOTtruckspeed <- ggplot(data = df.zd1WOTtruck,
                                aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 120),
                     breaks = seq(0, 120, by = 20)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 120, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 120, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 115, yend = 115,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 120, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 115, yend = 115,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 120, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WOTtruckspeed


# 2 匝道1，轨迹分析----
# 2.1 轿车，轨迹----
# 2.1.1 轿车，轨迹，无交通流----
# 2.1.2 轿车，轨迹，有交通流----
# 2.2 货车，轨迹----


# 3 匝道1，车道跨越点分析----
# 3.1 轿车，车道跨越点----
# 3.1.1 轿车，车道跨越点，无交通流----
# 3.1.2 轿车，车道跨越点，有交通流----
# 3.2 货车，车道跨越点----


# 4 匝道1，加速度分析----
# 4.1 轿车，加速度----
# 4.1.1 轿车，加速度，无交通流----
plot.zd1WOTsedanacc <- ggplot(data = df.zd1WOTsedan,
                              aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size =1) +
  geom_hline(yintercept = c(-2, 2), colour = "orange",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "加速度（m/s2）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 5, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 5, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 5, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WOTsedanacc


# 4.1.2 轿车，加速度，有交通流----
plot.zd1WTsedanacc <- ggplot(data = df.zd1WTsedan,
                             aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size =1) +
  geom_hline(yintercept = c(-2, 2), colour = "orange",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "加速度（m/s2）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 5, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 5, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 5, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WTsedanacc


# 4.2 货车，加速度----
plot.zd1WOTtrucksacc <- ggplot(data = df.zd1WOTtruck,
                               aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size =1) +
  geom_hline(yintercept = c(-2, 2), colour = "orange",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "加速度（m/s2）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 5, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 5, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 4.5, yend = 4.5,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 5, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WOTtrucksacc


# 5 匝道1，制动踏板位移分析----
# 5.1 轿车，制动踏板位移----
# 5.1.1 轿车，制动踏板位移，无交通流----
plot.zd1WOTsedanbrakepedal <- ggplot(data = df.zd1WOTsedan,
                                     aes(x = disTravelled, y = appBrake)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "制动踏板位移）", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 1, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 1, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 1, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WOTsedanbrakepedal


# 5.1.2 轿车，制动踏板位移，有交通流----
plot.zd1WTsedanbrakepedal <- ggplot(data = df.zd1WTsedan,
                                    aes(x = disTravelled, y = appBrake)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "制动踏板位移）", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 1, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 1, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 1, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WTsedanbrakepedal


# 5.2 货车，制动踏板位移----
plot.zd1WOTtruckbrakepedal <- ggplot(data = df.zd1WOTtruck,
                                      aes(x = disTravelled, y = appBrake)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "制动踏板位移）", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 1, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 1, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 1, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WOTtruckbrakepedal


# 6 匝道1，油门踏板位移分析----
# 6.1 轿车，油门踏板位移----
# 6.1.1 轿车，油门踏板位移，无交通流----
plot.zd1WOTsedangaspedal <- ggplot(data = df.zd1WOTsedan,
                                   aes(x = disTravelled, y = appGasPedal)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "油门踏板位移）", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 1, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 1, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 1, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WOTsedangaspedal


# 6.1.2 轿车，油门踏板位移，有交通流----
plot.zd1WTsedangaspedal <- ggplot(data = df.zd1WTsedan,
                                  aes(x = disTravelled, y = appGasPedal)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "油门踏板位移）", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 1, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 1, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 1, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WTsedangaspedal


# 6.2 货车，油门踏板位移----
plot.zd1WOTtruckgaspedal <- ggplot(data = df.zd1WOTtruck,
                                   aes(x = disTravelled, y = appGasPedal)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(0, 1400),
                     breaks = c(0, 582, 802, 852),
                     labels = c("S1K0+000", "S1K0+581.612", "", "")) +
  scale_y_continuous(name = "油门踏板位移）", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 211.6, xmax = 1400,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 211.6, y = 1, label = "S1匝道隧道起点") +
  annotate(geom = "segment", x = 582, xend = 802, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 692, y = 1, label = "加速段") +
  annotate(geom = "segment", x = 802, xend = 852, y = 0.95, yend = 0.95,
           arrow = arrow(ends = "both", angle = 45, length = unit(0.2, "cm"))) +
  annotate(geom = "text", x = 827, y = 1, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed",
                                          colour = "black", size = 0.5))

plot.zd1WOTtruckgaspedal

































