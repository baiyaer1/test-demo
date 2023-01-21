##中文名：决策树 英文名：ClassificationAndRegressionTree
#
#
#### 运行环境：R.4.2
#### install.packages(c(NMF","tinyarray","grDevices"))
## 卸除R包，避免程序冲突：UnlinkRPackage
UnlinkRPackage = function(){
  RPackage = setdiff((.packages()),c("base","methods","utils"))
  if(length(RPackage) > 0){
    for(i in 1:length(RPackage)){
      pkg <- paste0("package:",RPackage[i])
      print(pkg)
      detach(pkg, character.only = TRUE)
    }
    print("R package removal completed!")
  }else{
    print("No R package removal!")
  }
}
Dir = "./"
### 主函数
ClassificationAndRegressionTree <- function(Dir){
  setwd(Dir)
  options(stringsAsFactors = F)
  rm(list=ls())
  gc()
  UnlinkRPackage()
  
  ## 以下为功能函数  
  options(warn = -1)
  library(rattle)
  library(rpart)
  library(rpart.plot)
  library(party)
  library(partykit)
  library(C50)
  library(export)
  library(data.table)
  # 创建目录
  dir.create("./Result/CART/PNG/",recursive = T)
  dir.create("./Result/CART/PDF/",recursive = T)
  dir.create("./Result/ID3/PNG/",recursive = T)
  dir.create("./Result/ID3/PDF/",recursive = T)
  dir.create("./Result/C4.5/PNG/",recursive = T)
  dir.create("./Result/C4.5/PDF/",recursive = T)
  dir.create("./Result/C5.0/PNG/",recursive = T)
  dir.create("./Result/C5.0/PDF/",recursive = T)
  # 用户输入区
  InputData <- fread("./TempData/DataInput.txt",data.table = F)# 输入数据
  Title <- read.table("./TempData/Title.txt",fill = T,sep = "\t")# 读入标题说明数据类型
  Author <- read.table("./TempData/Author.txt",fill = T,sep = "\t")# 说明作者
  Width = read.table("./TempData/Width.txt",sep = "\t",header = T, quote = "")
  Height = read.table("./TempData/Height.txt",sep = "\t",header = T, quote = "")
  
  # 预处理
  Title <- as.character(Title)
  Author <- as.character(Author)
  InputData$Tag <- as.factor(InputData$Tag)
  YNum <- ncol(InputData)# 获取数据集列数-->class.id以确定分类变量
  #--------------------------------------#
  # 划分训练集、测试集、验证集
  #--------------------------------------#
  set.seed(10086) #随机抽样设置种子
  TrainIndex <- sample(nrow(InputData),0.8*nrow(InputData)) #抽样函数
  DataTrain <- InputData[ TrainIndex,] #生成训练集
  DataTest  <- InputData[-TrainIndex,] #生成测试集
  ##### CART ####
  if (T) {
    #--------------------------------------#
    #    > 1 < 【 CART算法 】
    #    using 'rpart' package
    #--------------------------------------#
    ModelCart <- rpart(Tag ~.,
                       data = DataTrain,
                       method = "class",
                       parms = list(split="gini")# 使用CART算法的时候, split = “gini”
    )
    
    Model <- ModelCart
    # 训练集准确性
      TrainPre <-  predict(Model,newdata = DataTrain,type = "class")# 提取预测模型标签
      MixMatrix <- as.data.frame.array(table(DataTrain[,YNum], TrainPre))# 混淆矩阵 行为预测，列为真实值
      TrainCorrect <- sum(as.numeric(TrainPre) == as.numeric(as.factor(DataTrain[,YNum])))/nrow(DataTrain)# 测试集准确性
      write.table(MixMatrix,"./Result/CART/TrainSetMixMatrix.txt",sep = "\t",quote = F) 
      
      # 测试集验证
      TestPre <-  predict(Model,newdata = DataTest,type = "class")# 提取预测模型标签
      MixMatrix <- as.data.frame.array(table(DataTest[,YNum], TestPre))# 混淆矩阵 行为预测，列为真实值
      TestCorrect <- sum(as.numeric(TestPre) == as.numeric(as.factor(DataTest[,YNum])))/nrow(DataTest)# 测试集准确性
      write.table(MixMatrix,"./Result/CART/TestSetMixMatrix.txt",sep = "\t",quote = F) 
      
      # AUC For Test
      if(T){
        library(pROC)
        TestPreToProbability <- predict(Model,newdata = DataTest)# 返回模型标签预测概率
        Max <- unlist(apply(TestPreToProbability,1,max))
        Tag <- DataTest$Tag
        TestPreToProbability <- cbind(TestPreToProbability,Max,Tag)
        Roc <- roc(TestPreToProbability[,4], TestPreToProbability[,3], smooth=TRUE)
        png("./Result/CART/PNG/TestRoc.png")
        print(plot(Roc,
                   add = FALSE,
                   col = 'red', 
                   legacy.axes = TRUE,
                   xlab = "1-Specificity",
                   print.auc =TRUE) )
        dev.off()
        
        pdf("./Result/CART/PDF/TestRoc.pdf",width = Width$width*2,height = Height$height*2,onefile = FALSE)
        print(plot(Roc,
                   add = FALSE,
                   col = 'red', 
                   legacy.axes = TRUE,
                   xlab = "1-Specificity",
                   print.auc =TRUE,
                   main = "The Roc of TestSet"))
        dev.off()
      }
      
      
      
      # Tree 
      png("./Result/CART/PNG/Cart.png")
      fancyRpartPlot(Model,
                     main = paste("CART of", Title,
                                  "\n(",Sys.time(),")",sep = " ","\n",
                                  "TrainSetVal = ", scales::percent(TrainCorrect, 0.0001), 
                                  "TestSetVal = ", scales::percent(TestCorrect, 0.0001)
                                  
                     ),
                     sub = paste("Creat by",Author,sep = " ")
      )
      dev.off()
      
      pdf("./Result/CART/PDF/Cart.pdf",width = Width$width*2,height = Height$height*2,onefile = FALSE)
      print(fancyRpartPlot(Model,
                           main = paste("CART of", Title,
                                        "\n(",Sys.time(),")",sep = " ","\n",
                                        "TrainSetVal = ", scales::percent(TrainCorrect, 0.0001), 
                                        "TestSetVal = ", scales::percent(TestCorrect, 0.0001)
                                        
                           ),
                           sub = paste("Creat by",Author,sep = " ")
      ))
      dev.off()
      
      # 自动选择xerror最小时候对应的cp值来剪枝
      Complexity <- printcp(ModelCart) # 计算复杂度
      index <- which.min(Complexity[,4])[[1]][1] # 查找最优化节点数
      FixTree <- prune(ModelCart,cp = Complexity[index,1]) # 根据最优化节点cp值修剪决策树
      Model <- FixTree
      
      
      # 训练集准确性
      TrainPre <-  predict(Model,newdata = DataTrain,type = "class")# 提取预测模型标签
      MixMatrix <- as.data.frame.array(table(DataTrain[,YNum], TrainPre))# 混淆矩阵 行为预测，列为真实值
      TrainCorrect <- sum(as.numeric(TrainPre) == as.numeric(as.factor(DataTrain[,YNum])))/nrow(DataTrain)# 测试集准确性
      write.table(MixMatrix,"./Result/CART/TrainSetMixMatrixFixtree.txt",sep = "\t",quote = F) 
      
      # 测试集验证
      TestPre <-  predict(Model,newdata = DataTest,type = "class")# 提取预测模型标签
      MixMatrix <- as.data.frame.array(table(DataTest[,YNum], TestPre))# 混淆矩阵 行为预测，列为真实值
      TestCorrect <- sum(as.numeric(TestPre) == as.numeric(as.factor(DataTest[,YNum])))/nrow(DataTest)# 测试集准确性
      write.table(MixMatrix,"./Result/CART/TestSetMixMatrixFixtree.txt",sep = "\t",quote = F) 
      
      # AUC For Test
      if(T){
        library(pROC)
        TestPreToProbability <- predict(Model,newdata = DataTest)# 返回模型标签预测概率
        Max <- unlist(apply(TestPreToProbability,1,max))
        Tag <- DataTest$Tag
        TestPreToProbability <- cbind(TestPreToProbability,Max,Tag)
        Roc <- roc(TestPreToProbability[,4], TestPreToProbability[,3], smooth=TRUE)
        png("./Result/CART/PNG/TestFixtreeRoc.png")
        print(plot(Roc,
                   add = FALSE,
                   col = 'red', 
                   legacy.axes = TRUE,
                   xlab = "1-Specificity",
                   print.auc =TRUE) )
        dev.off()
        
        pdf("./Result/CART/PDF/TestFixtreeRoc.pdf",width = Width$width*2,height = Height$height*2,onefile = FALSE)
        print(plot(Roc,
                   add = FALSE,
                   col = 'red', 
                   legacy.axes = TRUE,
                   xlab = "1-Specificity",
                   print.auc =TRUE,
                   main = "The Roc of TestSet"))
        dev.off()
      }
      
      
      
      # Tree 
      png("./Result/CART/PNG/CartFixtree.png")
      fancyRpartPlot(Model,
                     main = paste("CART of", Title,
                                  "\n(",Sys.time(),")",sep = " ","\n",
                                  "TrainSetVal = ", scales::percent(TrainCorrect, 0.0001), 
                                  "TestSetVal = ", scales::percent(TestCorrect, 0.0001)
                                  
                     ),
                     sub = paste("Creat by",Author,sep = " ")
      )
      dev.off()
      
      pdf("./Result/CART/PDF/CartFixtree.pdf",width = Width$width*2,height = Height$height*2,onefile = FALSE)
      print(fancyRpartPlot(Model,
                           main = paste("CART of", Title,
                                        "\n(",Sys.time(),")",sep = " ","\n",
                                        "TrainSetVal = ", scales::percent(TrainCorrect, 0.0001), 
                                        "TestSetVal = ", scales::percent(TestCorrect, 0.0001)
                                        
                           ),
                           sub = paste("Creat by",Author,sep = " ")
      ))
      dev.off()
    
  }
  ##### ID3 #####
  if (T) {
    #--------------------------------------#
    #    > 2 <  【 ID3算法 】 
    #    using 'rpart' package
    #--------------------------------------#
    ModelID3 <- rpart(Tag ~ .,
                      data = DataTrain,
                      method = "class",
                      parms = list(split="information")#使用ID3算法时候, split = “information”
    )
    # 未修剪树
    if (T) {
      
      # 训练集准确性
      TrainPre <-  predict(ModelID3,newdata = DataTrain,type = "class")# 提取预测模型标签
      MixMatrix <- as.data.frame.array(table(DataTrain[,YNum], TrainPre))# 混淆矩阵 行为预测，列为真实值
      TrainCorrect <- sum(as.numeric(TrainPre) == as.numeric(as.factor(DataTrain[,YNum])))/nrow(DataTrain)# 测试集准确性
      write.table(MixMatrix,"./Result/ID3/TrainSetMixMatrix.txt",sep = "\t",quote = F) 
      
      # 测试集验证
      TestPre <-  predict(ModelID3,newdata = DataTest,type = "class")# 提取预测模型标签
      MixMatrix <- as.data.frame.array(table(DataTest[,YNum], TestPre))# 混淆矩阵 行为预测，列为真实值
      TestCorrect <- sum(as.numeric(TestPre) == as.numeric(as.factor(DataTest[,YNum])))/nrow(DataTest)# 测试集准确性
      write.table(MixMatrix,"./Result/ID3/TestSetMixMatrix.txt",sep = "\t",quote = F) 
      
      # AUC For Test
      if(T){
        library(pROC)
        TestPreToProbability <- predict(ModelID3,newdata = DataTest)# 返回模型标签预测概率
        Max <- unlist(apply(TestPreToProbability,1,max))
        Tag <- DataTest$Tag
        TestPreToProbability <- cbind(TestPreToProbability,Max,Tag)
        Roc <- roc(TestPreToProbability[,4], TestPreToProbability[,3], smooth=TRUE)
        png("./Result/ID3/PNG/TestRoc.png")
        print(plot(Roc,
                   add = FALSE,
                   col = 'red', 
                   legacy.axes = TRUE,
                   xlab = "1-Specificity",
                   print.auc =TRUE) )
        dev.off()
        
        pdf("./Result/ID3/PDF/TestRoc.pdf",width = Width$width*2,height = Height$height*2,onefile = FALSE)
        print(plot(Roc,
                   add = FALSE,
                   col = 'red', 
                   legacy.axes = TRUE,
                   xlab = "1-Specificity",
                   print.auc =TRUE,
                   main = "The Roc of TestSet"))
        dev.off()
      }
      # Tree
      png("./Result/ID3/PNG/ID3.png")
      print(fancyRpartPlot(ModelID3,
                           main = paste("ID3 of", Title,
                                        "\n(",Sys.time(),")",sep = " ","\n",
                                        "TrainSetVal = ", scales::percent(TrainCorrect, 0.0001), 
                                        "TestSetVal = ", scales::percent(TestCorrect, 0.0001)
                                        
                           ),
                           sub = paste("Creat by",Author,sep = " ")
      ))
      dev.off()
      
      pdf("./Result/ID3/PDF/ID3.pdf",width = Width$width*2,height = Height$height*2)
      print(fancyRpartPlot(ModelID3,
                           main = paste("ID3 of", Title,
                                        "\n(",Sys.time(),")",sep = " ","\n",
                                        "TrainSetVal = ", scales::percent(TrainCorrect, 0.0001), 
                                        "TestSetVal = ", scales::percent(TestCorrect, 0.0001)
                                        
                           ),
                           sub = paste("Creat by",Author,sep = " ")
      ))
      dev.off()
    }
    # 自动选择xerror最小时候对应的cp值来剪枝
    Complexity <- printcp(ModelID3) # 计算复杂度
    index <- which.min(Complexity[,4])[[1]][1] # 查找最优化节点数
    FixTree <- prune(ModelID3,cp = Complexity[index,1]) # 根据最优化节点cp值修剪决策树
    
    if (T) {
      # 训练集准确性
      TrainPre <-  predict(FixTree,newdata = DataTrain,type = "class")# 提取预测模型标签
      MixMatrix <- as.data.frame.array(table(DataTrain[,YNum], TrainPre))# 混淆矩阵 行为预测，列为真实值
      TrainCorrect <- sum(as.numeric(TrainPre) == as.numeric(as.factor(DataTrain[,YNum])))/nrow(DataTrain)# 测试集准确性
      write.table(MixMatrix,"./Result/ID3/TrainSetMixMatrixFixtree.txt",sep = "\t",quote = F) 
      
      # 测试集验证
      TestPre <-  predict(FixTree,newdata = DataTest,type = "class")# 提取预测模型标签
      MixMatrix <- as.data.frame.array(table(DataTest[,YNum], TestPre))# 混淆矩阵 行为预测，列为真实值
      TestCorrect <- sum(as.numeric(TestPre) == as.numeric(as.factor(DataTest[,YNum])))/nrow(DataTest)# 测试集准确性
      write.table(MixMatrix,"./Result/ID3/TestSetMixMatrixFixtree.txt",sep = "\t",quote = F) 
      
      # AUC For Test
      if(T){
        library(pROC)
        TestPreToProbability <- predict(FixTree,newdata = DataTest)# 返回模型标签预测概率
        Max <- unlist(apply(TestPreToProbability,1,max))
        Tag <- DataTest$Tag
        TestPreToProbability <- cbind(TestPreToProbability,Max,Tag)
        Roc <- roc(TestPreToProbability[,4], TestPreToProbability[,3], smooth=TRUE)
        png("./Result/ID3/PNG/TestFixtreeRoc.png")
        print(plot(Roc,
                   add = FALSE,
                   col = 'red', 
                   legacy.axes = TRUE,
                   xlab = "1-Specificity",
                   print.auc =TRUE) )
        dev.off()
        
        pdf("./Result/ID3/PDF/TestFixtreeRoc.pdf",width = Width$width*2,height = Height$height*2,onefile = FALSE)
        print(plot(Roc,
                   add = FALSE,
                   col = 'red', 
                   legacy.axes = TRUE,
                   xlab = "1-Specificity",
                   print.auc =TRUE,
                   main = "The Roc of TestSet"))
        dev.off()
      }
      # Tree
      png("./Result/ID3/PNG/ID3Fixtree.png")
      print(fancyRpartPlot(FixTree,
                           main = paste("ID3 of", Title,
                                        "\n(",Sys.time(),")",sep = " ","\n",
                                        "TrainSetVal = ", scales::percent(TrainCorrect, 0.0001), 
                                        "TestSetVal = ", scales::percent(TestCorrect, 0.0001)
                                        
                           ),
                           sub = paste("Creat by",Author,sep = " ")
      ))
      dev.off()
      
      pdf("./Result/ID3/PDF/ID3Fixtree.pdf",width = Width$width*2,height = Height$height*2)
      print(fancyRpartPlot(FixTree,
                           main = paste("ID3 of", Title,
                                        "\n(",Sys.time(),")",sep = " ","\n",
                                        "TrainSetVal = ", scales::percent(TrainCorrect, 0.0001), 
                                        "TestSetVal = ", scales::percent(TestCorrect, 0.0001)
                                        
                           ),
                           sub = paste("Creat by",Author,sep = " ")
      ))
      dev.off()
    }
  }
  ##### C4.5 ####
  if (T) {
    #--------------------------------------#
    #    > 3 <  【 C4.5算法 】
    #    using 'sampling' 'party' package
    #--------------------------------------#
    ModelC4.5 <- ctree(Tag ~ .,data = DataTrain)
    # 未修剪树
    if (T) {
      # 训练集准确性
      TrainPre <-  predict(ModelC4.5,newdata = DataTrain)# 提取预测模型标签
      MixMatrix <- as.data.frame.array(table(DataTrain[,YNum], TrainPre))# 混淆矩阵 行为预测，列为真实值
      TrainCorrect <- sum(as.numeric(TrainPre) == as.numeric(as.factor(DataTrain[,YNum])))/nrow(DataTrain)# 测试集准确性
      write.table(MixMatrix,"./Result/C4.5/TrainSetMixMatrix.txt",sep = "\t",quote = F) 
      
      # 测试集验证
      TestPre <-  predict(ModelC4.5,newdata = DataTest)# 提取预测模型标签
      MixMatrix <- as.data.frame.array(table(DataTest[,YNum], TestPre))# 混淆矩阵 行为预测，列为真实值
      TestCorrect <- sum(as.numeric(TestPre) == as.numeric(as.factor(DataTest[,YNum])))/nrow(DataTest)# 测试集准确性
      write.table(MixMatrix,"./Result/C4.5/TestSetMixMatrix.txt",sep = "\t",quote = F) 
      
      
      pdf("./Result/C4.5/PDF/C4.5_Plot_1.pdf",width = Width$width*2,height = Height$height*2,onefile = FALSE)
      plot(ModelC4.5,
           main = paste("C4.5 of", Title,
                        "\n(",Sys.time(),")",sep = " ","\n",
                        "TrainSetVal = ", scales::percent(TrainCorrect, 0.0001), 
                        "TestSetVal = ", scales::percent(TestCorrect, 0.0001)))
      dev.off()
      pdf("./Result/C4.5/PDF/C4.5_Plot_2.pdf",width = Width$width*2,height = Height$height*2,onefile = FALSE)
      plot(ModelC4.5,
           type="simple",
           main = paste("C4.5 of", Title,
                        "\n(",Sys.time(),")",sep = " ","\n",
                        "TrainSetVal = ", scales::percent(TrainCorrect, 0.0001), 
                        "TestSetVal = ", scales::percent(TestCorrect, 0.0001)))
      dev.off()
    }
    
  }
  ##### C5.0 #####
  if (T) {
    #--------------------------------------#
    #    > 4 <  【 C5.0算法 】
    #    using 'C50' package
    #--------------------------------------#
    Tc <- C5.0Control(subset = T,winnow = F,
                      noGlobalPruning = T,
                      minCases = 20) #CF=0.25
    ModelC5.0 <- C5.0(Tag ~ .,
                      data = DataTrain,
                      rules = F,
                      control = Tc
    )
    
    # 训练集准确性
    TrainPre <-  predict(ModelC5.0,newdata = DataTrain,type = "class")# 提取预测模型标签
    MixMatrix <- as.data.frame.array(table(DataTrain[,YNum], TrainPre))# 混淆矩阵 行为预测，列为真实值
    TrainCorrect <- sum(as.numeric(TrainPre) == as.numeric(as.factor(DataTrain[,YNum])))/nrow(DataTrain)# 测试集准确性
    write.table(MixMatrix,"./Result/C5.0/TrainSetMixMatrix.txt",sep = "\t",quote = F) 
    
    # 测试集验证
    TestPre <-  predict(ModelC5.0,newdata = DataTest,type = "class")# 提取预测模型标签
    MixMatrix <- as.data.frame.array(table(DataTest[,YNum], TestPre))# 混淆矩阵 行为预测，列为真实值
    TestCorrect <- sum(as.numeric(TestPre) == as.numeric(as.factor(DataTest[,YNum])))/nrow(DataTest)# 测试集准确性
    write.table(MixMatrix,"./Result/C5.0/TestSetMixMatrix.txt",sep = "\t",quote = F) 
    
    # Tree
    pdf("./Result/C5.0/PDF/C50.pdf",width = Width$width*2,height = Height$height*2,onefile = FALSE)
    print(plot(ModelC5.0,
               main =  paste("C5.0 of", Title,
                             "\n(",Sys.time(),")",sep = " ","\n",
                             "TrainSetVal = ", scales::percent(TrainCorrect, 0.0001), 
                             "TestSetVal = ", scales::percent(TestCorrect, 0.0001))
    ))
    dev.off()
    
    png("./Result/C5.0/PNG/C50.png",width = Width$width*100,height = Height$height*100)
    print(plot(ModelC5.0,
               main =  paste("C5.0 of", Title,
                             "\n(",Sys.time(),")",sep = " ","\n",
                             "TrainSetVal = ", scales::percent(TrainCorrect, 0.0001), 
                             "TestSetVal = ", scales::percent(TestCorrect, 0.0001))
    ))
    dev.off()
  }
  # 主函数结束
}
get_Result = function(Dir){
  rm(list=ls())
  gc()
  Time = system.time({Result = ClassificationAndRegressionTree(Dir)})
  print(Time)
  Time_log = data.frame(user.self = Time[1],sys.self = Time[2],elapsed = Time[3])
  Running_log = data.frame(Running_log = "Run successfully",Time_log)# 待测试
  write.table(Running_log,"./Result/Running_log.txt",row.names=F,sep = "\t",quote=F)
  return(print("Run successfully!"))
  UnlinkRPackage()
  .rs.restartR()  ## 重启R
}

