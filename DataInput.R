#--------------Code Description------------------------------------------------#
# Notes:
# ver1.0, date: 20180626, By MaoYan
# 
# 1.导入妈湾（施工图阶段/初设图纸）驾驶模拟试验得到的所有数据，包括小客车和货车
# 2.依据车辆调研结果，驾模试验Trucksim选择的货车的比功率为7.0kW/T
#------------------------------------------------------------------------------#


# 调用RFunctions.R----
source(file = "E:/R/MaWan/MawanDS/RFunctions.R", encoding = "utf-8")


# 调用R Packages----
library(data.table)


# 设置数据所在文件夹路径为工作目录----
setwd(dir = "E:/R/MaWan/MawanDS/Data")  # 工作目录
kFileList <- list.files(pattern = "*.csv")  # 数据文件名
kDataName <- gsub(".csv", "", kFileList)  # 单个数据集名
df.dsdata <- data.frame()  # 创建用于存放所有数据的数据框

# 数据导入----
for (kFileIndex in 1:length(kFileList)) {
  
  tmpdf.file2data <- fread(input = kFileList[kFileIndex],
                           header = TRUE,
                           sep = "auto",
                           stringsAsFactors = FALSE,
                           data.table = FALSE,
                           select = c(1:89))
  
  tmpdf.file2data <- RenameSimDataV12(tmpdf.file2data)  # 变量重命名
  
  tmpdf.file2data$dsVehicleType <- strsplit(x = kDataName[kFileIndex],
                                            split = "_")[[1]][2]  # 模拟车类型
  tmpdf.file2data$PW <- strsplit(x = kDataName[kFileIndex],
                                 split = "_")[[1]][3]  # 模拟车比功率
  tmpdf.file2data$dsScen <- strsplit(x = kDataName[kFileIndex],
                                     split = "_")[[1]][4]  # 模拟场景
  tmpdf.file2data$driverID <- strsplit(x = kDataName[kFileIndex],
                                       split = "_")[[1]][5]  # 被试驾驶人ID
  tmpdf.file2data$driverAge <- strsplit(x = kDataName[kFileIndex],
                                        split = "_")[[1]][6]  # 被试驾驶人年龄
  tmpdf.file2data$driverSex <- strsplit(x = kDataName[kFileIndex],
                                        split = "_")[[1]][7]  # 被试驾驶人性别
  tmpdf.file2data$dringYears <- strsplit(x = kDataName[kFileIndex],
                                         split = "_")[[1]][8]  # 被试驾驶人驾龄
  
  df.dsdata <- rbind(tmpdf.file2data, df.dsdata)  # 合并导入的数据
}

options(scipen = 200)


# 数据类型转换----
df.dsdata$speedXMS <- as.numeric(df.dsdata$speedXMS)
df.dsdata$speedYMS <- as.numeric(df.dsdata$speedYMS)
df.dsdata$speedZMS <- as.numeric(df.dsdata$speedZMS)
df.dsdata$speedMS <- as.numeric(df.dsdata$speedMS)
df.dsdata$speedKMH <- as.numeric(df.dsdata$speedKMH)

df.dsdata$accXMS2 <- as.numeric(df.dsdata$accXMS2)
df.dsdata$accYMS2 <- as.numeric(df.dsdata$accYMS2)
df.dsdata$accZMS2 <- as.numeric(df.dsdata$accZMS2)

df.dsdata$disTravelled <- as.numeric(df.dsdata$disTravelled)

df.dsdata$steeringValue <- as.numeric(df.dsdata$steeringValue)
df.dsdata$appSteering <- as.numeric(df.dsdata$appSteering)

df.dsdata$gasPedal <- as.numeric(df.dsdata$gasPedal)
df.dsdata$appGasPedal <- as.numeric(df.dsdata$appGasPedal)

df.dsdata$brakePedal <- as.numeric(df.dsdata$brakePedal)
df.dsdata$appBrake <- as.numeric(df.dsdata$appBrake)

df.dsdata$disFromRoadStart <- as.numeric(df.dsdata$disFromRoadStart)

df.dsdata$disToLeftBorder <- as.numeric(df.dsdata$disToLeftBorder)
df.dsdata$disToRightBorder <- as.numeric(df.dsdata$disToRightBorder)

df.dsdata$carriagewayWidth <- as.numeric(df.dsdata$carriagewayWidth)

df.dsdata$roadOffset <- as.numeric(df.dsdata$roadOffset)
df.dsdata$laneOffset <- as.numeric(df.dsdata$laneOffset)

df.dsdata$laneWidth <- as.numeric(df.dsdata$laneWidth)





