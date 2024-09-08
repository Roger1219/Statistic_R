
# 倒入文件
data = read.csv('EOM_DWI_values.csv', header = TRUE, sep = ',')
#填补缺失值
max_length = max(sapply(data, length))
data_full = as.data.frame(sapply(data, function(x) {length(x) <- max_length; return(x)}))

#统计均值和标准差
col_Means = colMeans(data, na.rm = TRUE)
std_devs <- sapply(data, function(col) sd(col, na.rm = TRUE))

#创建结果数据框
data_sum = data.frame(t(col_Means))
rownames(data_sum)[1] <- "mean"
data_sum = rbind(data_sum, data.frame(t(std_devs)))
rownames(data_sum)[2] <- "sd"
