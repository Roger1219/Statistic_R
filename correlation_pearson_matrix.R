#本函数输入一个数据框，每一列是一组数据，自动计算任意两列间的相关系数矩阵r_matrix和p值矩阵p_matrix



correlation_pearson_matrix = function(inputdata) {
  cor_results = cor(inputdata, use = "complete.obs")
  #p值矩阵
  cor_pValues = combn(names(inputdata), 2, function(cols) {
    col1 = inputdata[[cols[1]]]
    col2 = inputdata[[cols[2]]]
    test = cor.test(col1, col2)
    c(cols[1], cols[2], test$p.value)
  })
  
  #将数组cor_pValues转换成p values 数据框
  row_names <- cor_pValues[1,]
  col_names <- cor_pValues[2,]
  values <- as.numeric(cor_pValues[3,])
  cor_pValues_dataframe = data.frame(matrix(NA, nrow = length(unique(c(row_names, col_names))), ncol = length(unique(c(row_names, col_names)))))
  rownames(cor_pValues_dataframe) = unique(c(row_names, col_names))
  colnames(cor_pValues_dataframe) = unique(c(row_names, col_names))
  for (i in 1:ncol(cor_pValues)) {
    cor_pValues_dataframe[cor_pValues[1, i], cor_pValues[2, i]] = cor_pValues[3, i]
    cor_pValues_dataframe[cor_pValues[2, i], cor_pValues[1, i]] = cor_pValues[3, i]
    cor_pValues_dataframe[cor_pValues[1, i], cor_pValues[1, i]] = 1 #对角线
    cor_pValues_dataframe[cor_pValues[2, i], cor_pValues[2, i]] = 1 #对角线
  }
  results = list(r_matrix = cor_results, p_matrix = cor_pValues_dataframe)
  return(results)
}