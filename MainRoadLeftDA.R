#--------------Code Description------------------------------------------------#
# Notes:
# ver1.0, date: 20180703, By MaoYan
# 
# 1.分析妈湾主线左线的行驶速度、加速度、制动、加速等。
#------------------------------------------------------------------------------#


# 调用DataInput.R----
source(file = "E:/R/MaWan/MawanDS/DataInput.R", encoding = "utf-8")


# 调用R Packages----
library(ggplot2)


# 数据切分----
df.mainleft <- subset(x = df.dsdata, dsScen == "ZX")  # 左线数据
df.mainleft <- df.mainleft[df.mainleft$roadName != "MainRD_Right",] 

# 标定左线桩号
df.mainleft$disTravelled <- ifelse(df.mainleft$roadName == "ZD_B",
                                   df.mainleft$disFromRoadStart + 1374.6 - 486,
                                   df.mainleft$disFromRoadStart)

df.mainleftsedan <- subset(x = df.mainleft,
                           dsVehicleType == "Sedan")  # 左线轿车
df.mainlefttruck <- subset(x = df.mainleft,
                           dsVehicleType == "Truck")  # 左线货车


# 1 左线，行驶速度分析----
# 1.1 轿车，行驶速度----
plot.MLsedanspeed <- ggplot(data = df.mainleftsedan,
                            aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("RK8", "RK7", "RK6", "RK5", "RK4",
                                "RK3", "RK2", "RK1", "RK0")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 120),
                     breaks = seq(0, 120, by = 20)) +
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = 0, ymax = 120, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 120, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 120, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MLsedanspeed


# 1.2 货车，行驶速度----
plot.MLtruckspeed <- ggplot(data = df.mainlefttruck,
                            aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("RK8", "RK7", "RK6", "RK5", "RK4",
                                "RK3", "RK2", "RK1", "RK0")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 120),
                     breaks = seq(0, 120, by = 20)) +
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = 0, ymax = 120, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 120, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 120, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MLtruckspeed


# 2 左线，加速度分析----
# 2.1 轿车，加速度----
plot.MLsedanacc <- ggplot(data = df.mainleftsedan,
                          aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size =1) +
  geom_hline(yintercept = c(-2, 2), colour = "orange",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("RK8", "RK7", "RK6", "RK5", "RK4",
                                "RK3", "RK2", "RK1", "RK0")) +
  scale_y_continuous(name = "加速度（m/s2）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 5, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 5, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MLsedanacc


# 2.2 货车，加速度----
plot.MLtruckacc <- ggplot(data = df.mainlefttruck,
                          aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size =1) +
  geom_hline(yintercept = c(-2, 2), colour = "orange",
             linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("RK8", "RK7", "RK6", "RK5", "RK4",
                                "RK3", "RK2", "RK1", "RK0")) +
  scale_y_continuous(name = "加速度（m/s2）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 5, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 5, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MLtruckacc


# 3 左线，制动踏板位移分析----
# 3.1 轿车，制动踏板位移----
plot.MLsedanbrakepedal <- ggplot(data = df.mainleftsedan,
                                 aes(x = disTravelled, y = appBrake)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("RK8", "RK7", "RK6", "RK5", "RK4",
                                "RK3", "RK2", "RK1", "RK0")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 1, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 1, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MLsedanbrakepedal


# 3.2 货车，制动踏板位移----
plot.MLtruckbrakepedal <- ggplot(data = df.mainlefttruck,
                                 aes(x = disTravelled, y = appBrake)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("RK8", "RK7", "RK6", "RK5", "RK4",
                                "RK3", "RK2", "RK1", "RK0")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 1, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 1, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MLtruckbrakepedal


# 4 左线，油门踏板位移分析----
# 4.1 轿车，油门踏板位移----
plot.MLsedangaspedal <- ggplot(data = df.mainleftsedan,
                               aes(x = disTravelled, y = appGasPedal)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("RK8", "RK7", "RK6", "RK5", "RK4",
                                "RK3", "RK2", "RK1", "RK0")) +
  scale_y_continuous(name = "油门踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 1, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 1, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MLsedangaspedal


# 4.2 货车，油门踏板位移----
plot.MLtruckgaspedal <- ggplot(data = df.mainlefttruck,
                               aes(x = disTravelled, y = appGasPedal)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red",
             linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(50, 8050),
                     breaks = seq(50, 8050, by = 1000),
                     labels = c("RK8", "RK7", "RK6", "RK5", "RK4",
                                "RK3", "RK2", "RK1", "RK0")) +
  scale_y_continuous(name = "油门踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 529, xmax = 6369,
           ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 529, y = 1, label = "主线隧道洞口") +
  annotate(geom = "text", x = 6369, y = 1, label = "主线隧道洞口") +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MLtruckgaspedal






