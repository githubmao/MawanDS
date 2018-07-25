#--------------Code Description------------------------------------------------#
# Notes:
# ver1.0, date: 20180626, By MaoYan
# 
# 1.妈湾（施工图阶段/初设图纸）驾驶模拟数据分析所用函数汇总
#------------------------------------------------------------------------------#


# UC-winRoad V12数据重命名----
RenameSimDataV12 <- function(data){
  # 重命名UC-winRoad V12版本输出数据变量名。
  #
  # 输入：UC-winRoad V12版本输出数据。
  # 输出：重命名后的数据框。
  
  kSimDataNewName <- c("simTime",         # Time
                       "logTime",         # TimeStamp
                       "type",            # Type
                       "carModel",        # Model
                       "logID",           # ID
                       "customID",        # customID
                       "logDescription",  # description
                       "positionX",       # position X
                       "positionY",       # position Y
                       "positionZ",       # position Z
                       "yawAngle",        # yawAngle
                       "pitchAngle",      # pitchAngle
                       "rollAngle",       # rollAngle
                       "directionX",      # direction X
                       "directionY",      # direction Y
                       "directionZ",      # direction Z
                       "bodyPitchAngle",  # bodyPitchAngle
                       "bodyRollAngle",   # bodyRollAngle
                       "RPM",             # RPM
                       "gearNumber",      # gearNumber
                       "speedXMS",        # speedVectInMetresPerSecond X
                       "speedYMS",        # speedVectInMetresPerSecond Y
                       "speedZMS",        # speedVectInMetresPerSecond Z
                       "speedKMH",        # speedInKmPerHour
                       "speedMS",         # speedInMetresPerSecond
                       "accXMS2",         # localAccelInMetresPerSecond2 X
                       "accYMS2",         # localAccelInMetresPerSecond2 Y
                       "accZMS2",         # localAccelInMetresPerSecond2 Z
                       "bodyRotSpeedYawRS",   # bodyRotSpeedInRadsPerSecond Yaw
                       "bodyRotSpeedPitchRS", # bodyRotSpeedInRadsPerSecond Pitch
                       "bodyRotSpeedRollRS",  # bodyRotSpeedInRadsPerSecond Roll
                       "bodyRotAccYawRS2",    # bodyRotAccelInRadsPerSecond Yaw
                       "bodyRotAccPitchRS2",  # bodyRotAccelInRadsPerSecond Pitch
                       "bodyRotAccRollRS2",   # bodyRotAccelInRadsPerSecond Roll
                       "rotSpeedYawRS",       # rotSpeedInRadsPerSecond Yaw
                       "rotSpeedPitchRS",     # rotSpeedInRadsPerSecond Pitch
                       "rotSpeedRollRS",      # rotSpeedInRadsPerSecond Roll
                       "rotAccYawRS2",        # rotAccelInRadsPerSecond Yaw
                       "rotAccPitchRS2",      # rotAccelInRadsPerSecond Pitch
                       "rotAccRollRS2",       # rotAccelInRadsPerSecond Roll
                       "disTravelled",        # distanceTravelled
                       "steeringValue",       # steering
                       "appSteering",         # appliedSteering
                       "steeringVelocity",    # steeringVelocity
                       "turningCurvature",    # turningCurvature
                       "gasPedal",            # throttle
                       "pedalTorque",         # pedalTorque
                       "appGasPedal",         # appliedThrottle
                       "brakePedal",          # brake
                       "appBrake",            # appliedBrake
                       "lightState",          # lightState
                       "automaticControl",    # automaticControl
                       "dragForce",           # dragForce
                       "carMass",             # mass
                       "carWheelBase",        # wheelBase
                       "centerOfGravityHeight",    # centerOfGravityHeight
                       "centerOfGravityPosition",  # centerOfGravityPosition
                       "rollAxisHeight",           # rollAxisHeight
                       "trailerState",             # trailer
                       "trailerAngle",             # trailerAngle
                       "trailerPitchAngle",        # trailerPitchAngle
                       "trailerWheelbase",         # trailerWheelbase
                       "isInIntersection",         # inIntersection
                       "roadName",                 # road
                       "disFromRoadStart",         # distanceAlongRoad
                       "latestRoad",               # latestRoad
                       "disFromLatestRoadStart",   # distanceAnlongLatestRoad
                       "disToLeftBorder",          # distanceToLeftBorder
                       "disToRightBorder",         # distanceToRightBorder
                       "carriagewayWidth",         # carriagewayWidth
                       "roadOffset",               # offsetFromRoadCenter
                       "laneOffset",               # offsetFromLaneCenter
                       "logitudinalSlope",         # roadLongitudinalSlop
                       "lateralSlope",             # roadLateralSlop
                       "laneNumber",               # laneNumber
                       "laneWidth",                # laneWidth
                       "laneDirectionX",           # laneDirection X
                       "laneDirectionY",           # laneDirection Y
                       "laneDirectionZ",           # laneDirection Z
                       "laneCurvature",            # laneCurvature
                       "isDrivingForward",         # drivingForwards
                       "speedLimit",               # speedLimit
                       "isSpeedOver",              # speedOver
                       "leftLaneOverlap",          # leftLaneOverLap
                       "rightLaneOverlap",         # rightLaneOverLap
                       "collisionWithUser",        # collicionWithUser
                       "pedestrianNumber",         # pedestriansNumber
                       "roadSurface",              # surface
                       "averageFlux")              # averageFlux
  
  names(data) <- kSimDataNewName
  return(data)
}


# 计算行车轨迹----
CalcDrivingTraj <- function(data,
                            is.main2ramp = NA,
                            is.disdecrease = NA){
  # 依据 dsToLeftBorder 计算模拟车行车轨迹。
  #
  # 输入：
  # data：重命名后的数据框，含 disToLeftBorder, roadName等数据。
  # is.main2ramp：主线进入匝道为 TRUE，匝道进入主线为 FALSE，默认为NA。
  # is.disdecrease：data中的disTraveled变量是否为递减趋势，默认为NA。
  #
  # 输出：含行车轨迹变量 drivingTraj 的数据框。
  
  if (is.na(is.main2ramp)) {
    stop("Please input the 'is.main2ramp'.")  # 没有输入is.main2ramp
  } else if (is.na(is.disdecrease)) {
    stop("Please input the 'is.disdecrease'.")  # 没有输入is.disdecrease
  } else if (is.logical(is.main2ramp) & is.logical(is.disdecrease)) {
    
    data <- data[order(data$disTravelled,
                       decreasing = is.disdecrease),]  # data按桩号排列
    
    kRoadName <- unique(data$roadName)  # 数据所含道路名称
    
    if (length(kRoadName) != 2) {
      stop("The number of 'road name' is not equal to 2.")  # 异常情况
    } else {
      
      tmp.subdata1 <- subset(x = data, subset = data$roadName == kRoadName[1])
      tmp.subdata2 <- subset(x = data, subset = data$roadName == kRoadName[2])
      
      kData1Length <- length(tmp.subdata1$disToLeftBorder)  # tmp.subdata1行数
      
      # 计算主线与匝道Traj计算差
      kDeltaDisBorder <- abs(tmp.subdata1[kData1Length,]$disToLeftBorder -
                               tmp.subdata2[1,]$disToLeftBorder)
      
      # 计算行车轨迹drivingTraj
      if (is.main2ramp) {
        tmp.subdata1$drivingTraj <- tmp.subdata1$disToLeftBorder  # 主线轨迹
        tmp.subdata2$drivingTraj <- tmp.subdata2$disToLeftBorder +
          kDeltaDisBorder  # 匝道轨迹
      } else {
        tmp.subdata1$drivingTraj <- tmp.subdata1$disToLeftBorder +
          kDeltaDisBorder  # 匝道轨迹
        tmp.subdata2$drivingTraj <- tmp.subdata2$disToLeftBorder  # 主线轨迹
      }
      
      return(rbind(tmp.subdata1, tmp.subdata2))
    }
  } else {
    stop("Please check the input 'data', 'is.main2ramp' and 'is.disdecrease'.\
  The 'data' should be a Rioh 8dof driving simulator data.\
  The 'is.main2ramp' should be a logical variable.\
  The 'is.disdecrease' should be a logical variable.")
  }
}


# 批量计算行车轨迹----
CalcBatchTraj <- function(data,
                          kDriverID = NA,
                          is.main2ramp = NA,
                          is.disdecrease = NA){
  # 依据CalcDrivingTraj函数，批量计算行车轨迹。
  #
  # 输入：
  # data：重命名后的数据框，含 disToLeftBorder, roadName等数据。
  # kDriverID：需要计算的驾驶人ID向量集，默认为NA。
  # is.main2ramp：CalcDrivingTraj函数中的is.main2ramp参数值，默认为NA。
  # is.disdecrease：CalcDrivingTraj函数中的is.disdecrease参数值，默认为NA。
  #
  # 输出：含行车轨迹变量 drivingTraj 的数据框。
  
  if (is.character(kDriverID)) {
    
    df.traj <- data.frame()  # 输出数据框
    
    for (kIDidx in 1:length(kDriverID)) {  # 依次计算各个驾驶人的DrivingTraj
      
      tmp.df <- data[data$driverID == kDriverID[kIDidx],]
      tmpdf.calctraj <- CalcDrivingTraj(data = tmp.df,
                                        is.main2ramp = is.main2ramp,
                                        is.disdecrease = is.disdecrease)
      
      df.traj <- rbind(df.traj, tmpdf.calctraj)
    }
    
    return(df.traj)
    
  } else {
    stop("Please check the input 'kDriverID'.\
  The 'kDriverID' should be a character vector variable.")
  }
}


# 计算车道跨越点----
CalcLCPoint <- function(data,
                        kLatDis = NA,
                        is.main2ramp = NA,
                        is.disdecrease = NA){
  # 计算车道跨越点的数据行行号，车道跨越判定原则：依据DisToLeftBorder输出值，即
  # drivingTraj值，设定阈值，模拟车从主线进入匝道时，车道跨越位置为前一时刻该输
  # 出值小于阈值、且下一时刻该输出值大于或等于阈值的位置点（匝道进入主线反之）。
  #
  # 输出：
  # data：重命名且经过CalcDrivingTraj函数计算的数据框。
  # kLatDis：车道跨越阈值，默认为NA。
  # is.main2ramp：主线进入匝道为 TRUE，匝道进入主线为 FALSE，默认为NA。
  # is.disdecrease：data中的disTraveled变量是否为递减趋势，默认为NA。
  #
  # 输出：车道跨越点的位置行号，有多个满足判定条件的位置点（即反复换道时），输出
  #       为一组数值向量。
  
  if (is.na(kLatDis)) {
    stop("Please input the 'kLatDis'.")  # 没有输入kLatDis
  } else if (is.na(is.main2ramp)) {
    stop("Please input the 'is.main2ramp'.")  # 没有输入is.main2ramp
  } else if (is.na(is.disdecrease)) {
    stop("Please input the 'is.disdecrease'.")  # 没有输入is.disdecrease
  } else if (is.numeric(kLatDis) & is.logical(is.main2ramp) &
             is.logical(is.disdecrease)) {
    
    data <- data[order(data$disTravelled,
                       decreasing = is.disdecrease),]  # 按桩号排列数据
    
    # 生成前一时刻的drivingTraj
    tmp.data <- transform(data,
                          drivingTrajB4 = c(drivingTraj[1],
                                            drivingTraj[-length(drivingTraj)]))
    
    # 计算车道跨越点数据行号
    if (is.main2ramp) {
      kLCRowNo <- which(tmp.data$drivingTraj > kLatDis &
                          tmp.data$drivingTrajB4 <= kLatDis)
    } else {
      kLCRowNo <- which(tmp.data$drivingTraj < kLatDis &
                          tmp.data$drivingTrajB4 >= kLatDis)
    }
    
    return(kLCRowNo)
  } else {
    stop("Please check the input 'data', 'kLatDis', 'is.main2ramp'\
  and 'is.disdecrease'.\
  The 'data' should be a Rioh 8dof driving simulator data.\
  The 'kLatDis' should be a numeric variable.\
  The 'is.main2ramp' should be a logical variable.\
  The 'is.disdecrease' should be a logical variable.")
  }
}


# 批量计算车道跨越点----
CalcBatchLCPoint <- function(data,
                             kDriverID = NA,
                             kLatDis = NA,
                             is.main2ramp = NA,
                             is.disdecrease = NA){
  # 依据CalcLCPoint函数，批量计算车道跨越点。
  #
  # 输入：
  # data：重命名且经过CalcDrivingTraj函数计算的数据框据。
  # kDriverID：需要计算的驾驶人ID向量集，默认为NA。
  # is.main2ramp：CalcLCPoint函数中的is.main2ramp参数值，默认为NA。
  # is.disdecrease：CalcLCPoint函数中的is.disdecrease参数值，默认为NA。
  #
  # 输出：含车道跨越点行号、桩号、驾驶轨迹（即距离道路左侧距离）、驾驶人ID的
  #       数据框。
  
  if (is.character(kDriverID)) {
    
    df.LCpoints <- data.frame()  # 输出数据框
    
    for (kIDidx in 1:length(kDriverID)) {  # 依次计算各个驾驶人的LC Point
      
      tmp.df <- data[data$driverID == kDriverID[kIDidx],]
      kLCPoint <- CalcLCPoint(data = tmp.df,
                              kLatDis = kLatDis,
                              is.main2ramp = is.main2ramp,
                              is.disdecrease = is.disdecrease)
      
      tmpdf.LCpoint <- data.frame(rowNo = kLCPoint,
                                  disTravelled = tmp.df$disTravelled[kLCPoint],
                                  drivingTraj = tmp.df$drivingTraj[kLCPoint],
                                  driverID = tmp.df$driverID[kLCPoint],
                                  speedKMH = tmp.df$speedKMH[kLCPoint])
      
      df.LCpoints <- rbind(df.LCpoints, tmpdf.LCpoint)
    }
    
    return(df.LCpoints)
    
  } else {
    stop("Please check the input 'kDriverID'.\
  The 'kDriverID' should be a character vector variable.")
  }
}


# 计算特征位置的车速----
CalcSpotSpeed <- function(data,
                          kDis = NA,
                          is.disdecrease = NA,
                          kTag = NA,
                          kDisType = "Dis"){
  # 计算特征位置的车速。
  #
  # 输入：
  # data：重命名后的数据框。
  # kDis：特征位置的桩号或行号，可以为一组向量，默认为NA。
  # is.disdecrease：data中的disTraveled变量是否为递减趋势，默认为NA。
  # kTag：特征位置的标签值，默认为NA。
  # kDisType：计算模式，kDis为特征位置的桩号时，选"Dis"模式；kDis为特征位置的行
  #           号时，选"RowNo"模式；默认为"Dis"模式。
  #
  # 输出：含特征位置行号、桩号、速度、驾驶人ID、特征标签值的数据框。
  
  if (is.na(kDis[1])) {
    stop("Please input the 'kDis'.")  # 没有输入kDis
  } else if (is.na(is.disdecrease)) {
    stop("Please input the 'is.disdecrease'.")  # 没有输入is.disdecrease
  } else if (is.na(kTag)[1]) {
    stop("Please input the 'kTag'.")  # 没有输入kTag
  } else if (is.numeric(kDis) & is.logical(is.disdecrease) &
             is.character(kTag) & is.character(kDisType)) {
    
    if (kDisType == "Dis") {  # kDis为特征位置的桩号
      
      data <- data[order(data$disTravelled,
                         decreasing = is.disdecrease),]  # 按桩号排列数据
      
      kDisRowNo <- c()  # 特征位置行号输出向量
      
      for (kDisidx in 1:length(kDis)) {  # 依次计算各特征行号
        kDisRowNo <- append(kDisRowNo,
                            which.min(abs(data$disTravelled - kDis[kDisidx])))
      }
      
      return(data.frame(rowNo = kDisRowNo,
                        disTravelled = data$disTravelled[kDisRowNo],
                        speedKMH = data$speedKMH[kDisRowNo],
                        driverID = data$driverID[kDisRowNo],
                        disTag = kTag))
      
      
    } else if (kDisType == "RowNo") {  # kDis为特征位置的数据行号
      
      data <- data[order(data$disTravelled,
                         decreasing = is.disdecrease),]  # 按桩号排列数据
      
      return(data.frame(rowNo = kDis,
                        disTravelled = data$disTravelled[kDis],
                        speedKMH = data$speedKMH[kDis],
                        driverID = data$driverID[kDis],
                        disTag = kTag))
    } else {
      stop("Please check the input 'kDisType'.\
  The 'kDisType' is the calculation method to be used. The default 'Dis' implies\
  the input 'kDis' is the 'Distance From Road Start'. The other method is 'RowNo'\
  which means the input 'kDis' is the 'Row Number'.")
    }
    
  } else {
    stop("Please check the input 'data', 'kDis', 'is.disdecrease', and 'kTag'.\
  The 'data' should be a Rioh 8dof driving simulator data.\
  The 'kDis' should be a numeric variable.\
  The 'is.disdecrease' should be a logical variable.\
  The 'kTag' should be a character variable.")
  }
}


# 批量计算特征位置的车速----
CalcBatchSpotSpeed <- function(data,
                               kDriverID = NA,
                               kDis = NA,
                               is.disdecrease = NA,
                               kTag = NA,
                               kDisType = NA){
  # 依据CalcSpotSpeed函数，批量计算特征位置的车速。
  #
  # 输入：
  # data：重命名后的数据框。
  # kDriverID：需要计算的驾驶人ID向量集，默认为NA。
  # kDis：CalcSpotSpeed函数中的kDis，默认为NA。
  # is.disdecrease：CalcSpotSpeed函数中的is.disdecrease，默认为NA。
  # kTag：CalcSpotSpeed函数中的kTag，默认为NA。
  # kDisType：CalcSpotSpeed函数中的kDisType，默认为"Dis"。
  #
  # 输出：含车道跨越点行号、桩号、驾驶轨迹（即距离道路左侧距离）、驾驶人ID的
  #       数据框。
  
  if (is.character(kDriverID)) {
    
    df.spotspeed <- data.frame()  # 输出数据框
    
    for (kIDidex in 1:length(kDriverID)) {  # 依次计算各个驾驶人的特征位置速度
      
      tmp.df <- data[data$driverID == kDriverID[kIDidex],]
      tmpdf.spotspeed <- CalcSpotSpeed(data = tmp.df,
                                       kDis = kDis,
                                       is.disdecrease = is.disdecrease,
                                       kTag = kTag,
                                       kDisType = kDisType)
      
      df.spotspeed <- rbind(df.spotspeed, tmpdf.spotspeed)
    }
    
    return(df.spotspeed)
    
  } else {
    stop("Please check the input 'kDriverID'.\
  The 'kDriverID' should be a character vector variable.")
  }
}


# 计算加速度or减速度较大的点----
CalcAccOutliers <- function(data,
                            kQuantile = NA,
                            kAccLimit = NA,
                            kOutliersType = "Dec",
                            is.indv = TRUE){
  # 计算加速度or减速度较大的点。
  #
  # 输入：
  # data：重命名后的数据框，经过CalcDrivingTraj计算行车轨迹。
  # kQuantile：计算的百分位数，默认为NA。
  # kAccLimit: 计算的加速度or减速度边界值，默认为NA。
  # kOutliersType：计算模式，计算加速度较大的点选"Acc"模式，计算减速度较大的点选
  #                "Dec"模式，默认为"Dec"模式。
  # 注意：（1）kQuantile为0.85、kOutliersType为"Acc"时，输出为计算得到的加速度
  #           大于行车数据中85分位数加速度的数据；
  #       （2）kQuantile为0.15、kOutliersType为"Dec"时，输出为计算得到的减速度
  #           小于行车数据中15分位数（即减速度绝对值大于85分位数）减速度的数据。
  # is.indv: 默认为TRUE，表示每个驾驶人的加速度or减速度边界值单独计算；
  #          FALSE表示所有驾驶人的加速度or减速度边界值为同一数值。
  #
  # 输出：加速度or减速度较大的数据组成的数据框。
  
  if (is.na(is.indv)) {
    stop("Please input the 'is.indv'.")  # 没有输入is.indv
  } else if (is.logical(is.indv)) {
    
    
    if (is.indv) {  # 个体特征边界点计算
      if (is.na(kQuantile)) {
        stop("Please input the 'kQuantile'.")  # 没有输入kQuantile
      } else if (is.numeric(kQuantile) & kQuantile >= 0 & kQuantile <= 1) {
        
        if (kOutliersType == "Dec") {
          tmp.data <- subset(data, accZMS2 <= 0)  # 减速位置点数据
          kAccLimit <- quantile(x = tmp.data$accZMS2,
                                probs = kQuantile,
                                names = FALSE)  # 加速度边界点
          return(subset(tmp.data, accZMS2 <= kAccLimit))  # 返回Outlier点数据
          
        } else if (kOutliersType == "Acc") {
          tmp.data <- subset(data, accZMS2 >= 0)  # 加速位置点数据
          kAccLimit <- quantile(x = tmp.data$accZMS2,
                                probs = kQuantile,
                                names = FALSE)  # 加速度边界值
          return(subset(tmp.data, accZMS2 >= kAccLimit))  # 返回Outlier点数据
          
        } else {
          stop("Please check the input 'kOutliersType'.\
  The 'kOutliersType' is the calculation method to be used. The default 'Dec'\
               implies the input 'kOutliersType' is the deceleration outliers calculation.\
               The other method is 'Acc', which means the input 'kOutliersType' is the\
               acceleration outliers calculation.")
        }
        
      } else {
        stop("Please check the input 'data', and 'kQuantile'.\
  The 'data' should be a Rioh 8dof driving simulator data.\
  The 'kQuantile' should be a numeric variable ranged from 0 to 1.")
      }
      
      
    } else {  # 群体同一边界点计算
      if (is.na(kAccLimit)) {
        stop("Please input the 'kAccLimit'.")  # 没有输入kAccLimit
      } else if (is.numeric(kAccLimit)) {
        
        if (kOutliersType == "Dec") {
          return(subset(data, accZMS2 <= kAccLimit))  # 返回Outlier点数据
        } else if (kOutliersType == "Acc") {
          return(subset(data, accZMS2 >= kAccLimit))  # 返回Outlier点数据
        } else {
          stop("Please check the input 'kOutliersType'.\
  The 'kOutliersType' is the calculation method to be used. The default 'Dec'\
  implies the input 'kOutliersType' is the deceleration outliers calculation.\
  The other method is 'Acc', which means the input 'kOutliersType' is the\
  acceleration outliers calculation.")
        }
        
      } else {
        stop("Please check the input 'data', and 'kAccLimit'.\
  The 'data' should be a Rioh 8dof driving simulator data.\
  The 'kAccLimit' should be a numeric variable.")
      }
    }
    
    
  } else {
    stop("Please check the input 'is.indv'.\
  The 'is.indv' is a logical variable. The default 'TRUE' implies every driver\
  has his or her own acceleration or deceleration limit, while the 'FALSE' means\
  all drivers have the same accleration or deceleration limit.")
  }
}


# 批量计算加速度or减速度较大的点
CalcBatchAccOutliers <- function(data,
                                 kDriverID = NA,
                                 kQuantile = NA,
                                 kAccLimit = NA,
                                 kOutliersType = "Dec",
                                 is.indv = NA){
  # 批量计算加速度or减速度较大的点。
  #
  # 输入：
  # data：重命名后的数据框，经过CalcDrivingTraj计算行车轨迹。
  # kDriverID：需要计算的驾驶人ID向量集，默认为NA。
  # kQuantile：CalcAccOutliers中的kQuantile，默认为NA。
  # kAccLimit: CalcAccOutliers中的kAccLimit，默认为NA。
  # kOutliersType：CalcAccOutliers中的kOutliersType，默认为NA。
  # is.indv: CalcAccOutliers中的is.indv，默认为NA。
  #
  # 输出：加速度or减速度较大的数据组成的数据框。
  
  if (is.character(kDriverID)) {
    
    df.accoutliers <- data.frame()  # 输出数据框
    
    # 依次计算各个驾驶人加速度or减速度较大的点
    for (kIDidex in 1:length(kDriverID)) {
      
      tmp.df <- data[data$driverID == kDriverID[kIDidex],]
      tmpdf.accoutliers <- CalcAccOutliers(data = tmp.df,
                                           kQuantile = kQuantile,
                                           kAccLimit = kAccLimit,
                                           kOutliersType = kOutliersType,
                                           is.indv = is.indv)
      
      df.accoutliers <- rbind(df.accoutliers, tmpdf.accoutliers)
    }
    
    return(df.accoutliers)
    
  } else {
    stop("Please check the input 'kDriverID'.\
         The 'kDriverID' should be a character vector variable.")
  }
}





















