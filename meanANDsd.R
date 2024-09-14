
# 导入文件
data = read.csv('EOM_DWI_values.csv', header = TRUE, sep = ',')
#填补缺失值
max_length = max(sapply(data, length))
data_full = as.data.frame(sapply(data, function(x) {length(x) = max_length; return(x)}))

#统计均值和标准差
col_Means = colMeans(data, na.rm = TRUE)
std_devs = sapply(data, function(col) sd(col, na.rm = TRUE))

#正态性检验
pValuesOfShapiroTest = sapply(data_full, function(col) shapiro.test(col))

#创建结果数据框
data_sum = data.frame(t(col_Means))
rownames(data_sum)[1] <- "mean"
data_sum = rbind(data_sum, data.frame(t(std_devs)))
rownames(data_sum)[2] <- "sd"
data_sum = rbind(data_sum, data.frame(t(pValuesOfShapiroTest[2,])))
rownames(data_sum)[3] <- "pValuesOfShapiroTest"

#不同组的t检验
#1 内直肌正前方注视
MR_Cen_unpaired = wilcox.test(data_full$IXT.MR.Cen, data_full$HC.MR.Cen, paired = FALSE)
#Wilcoxon统计量W值
MR_Cen_unpaired[["statistic"]][["W"]]
#Wilconxon的p值
MR_Cen_unpaired[["p.value"]] 

#2 内直肌集合注视
MR_Con_unpaired = t.test(data_full$IXT.MR.Con, data_full$HC.MR.Con, var.equal = TRUE, paired = FALSE)
#2-samples t-test 的t值
MR_Con_unpaired[["statistic"]][["t"]]
#2-samples t-test 的p值
MR_Con_unpaired[["p.value"]]

#3 外直肌中央注视
LR_Cen_unpaired = t.test(data_full$IXT.LR.Cen, data_full$HC.LR.Cen, var.equal = TRUE, paired = FALSE)
#2-samples t-test 的t值
LR_Cen_unpaired[["statistic"]][["t"]]
#2-samples t-test 的p值
LR_Cen_unpaired[["p.value"]]

#4 外直肌集合注视
LR_Con_unpaired = t.test(data_full$IXT.LR.Con, data_full$HC.LR.Con, var.equal = TRUE, paired = FALSE)
#2-samples t-test 的t值
LR_Con_unpaired[["statistic"]][["t"]]
#2-samples t-test 的p值
LR_Con_unpaired[["p.value"]]


#不同注视状态的配对t检验
#1 IXT组内直肌
IXT_MR_paired = wilcox.test(data_full$IXT.MR.Cen, data_full$IXT.MR.Con, paired = TRUE)
#输出IXT组 MR 在两种状态下配对检验的结果（非参数）
#Wilcoxon统计量V值
IXT_MR_paired[["statistic"]][["V"]]
#Wilconxon的p值
IXT_MR_paired[["p.value"]] 

#2 IXT组外直肌
IXT_LR_paired = t.test(data_full$IXT.LR.Cen, data_full$IXT.LR.Con, paired = TRUE)
#Paired t test T value
IXT_LR_paired[["statistic"]][["t"]]
#Paired t test p value
IXT_LR_paired[["p.value"]]

#3 HC组内直肌
HC_MR_paired = t.test(data_full$HC.MR.Cen, data_full$HC.MR.Con, paired = TRUE)
#Paired t test T value
HC_MR_paired[["statistic"]][["t"]]
#Paired t test p value
HC_MR_paired[["p.value"]]

#4 HC组外直肌
HC_LR_paired = t.test(data_full$HC.LR.Cen, data_full$HC.LR.Con, paired = TRUE)
#Paired t test T value
HC_LR_paired[["statistic"]][["t"]]
#Paired t test p value
HC_LR_paired[["p.value"]]


#相关性分析
#Data准备
IXT_EOM_Difference = data.frame(
  MR_diff = data_full$IXT.MR.Con - data_full$IXT.MR.Cen,
  LR_diff = data_full$IXT.LR.Con - data_full$IXT.LR.Cen)
IXT_clinicalData = read.csv("ClinicalData.csv", header = TRUE)
#正态性检验
pValuesOfShapiroTestofIXT_EOM_Difference = sapply(IXT_EOM_Difference, function(col) shapiro.test(col))
pValuesOfShapiroTestOfClinicalData = sapply(IXT_clinicalData, function(col) shapiro.test(col))
combined_data = cbind(IXT_EOM_Difference, IXT_clinicalData$Angle33cm)
colnames(combined_data)[3] = "Angle33cm"
#均为正态分布，采用皮尔逊相关得到相关矩阵
source("correlation_pearson_matrix.R")
results = correlation_pearson_matrix(combined_data)
results$p_matrix

