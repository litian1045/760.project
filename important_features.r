data <- read.csv("d:\\760.project\\main_fill.csv")
cor_result <- c()
name_list <- names(data)
for (i in 1:length(data))
	cor_result = c(cor_result, cor(data[,i], data["TARGET"]))
dl = data.frame(name = name_list[order(abs(cor_result),decreasing=TRUE)], cor= cor_result[order(abs(cor_result),decreasing=TRUE)])
write.csv(dl, "d:\\760.project\\cor_list.csv")


data <- read.csv("d:\\760.project\\main_fill.csv")
list1 <- read.csv("d:\\760.project\\list_f.csv")['Feature_Name']
for (i in 1:20) {
	for (j in i:20) {
		if (list1[i,] != list1[j,]) {
			data[paste(list1[i,],"_plus_",list1[j,], sep='')] = data[list1[i,]] + data[list1[j,]]
			data[paste(list1[i,],"_minus_",list1[j,], sep='')] = data[list1[i,]] - data[list1[j,]]
			data[paste(list1[i,],"_multi_",list1[j,], sep='')] = data[list1[i,]] * data[list1[j,]]
			if (sum(data[list1[j,]] == 0) == 0)
				data[paste(list1[i,],"_division_",list1[j,], sep='')] = data[list1[i,]] / data[list1[j,]]
			}
		}
	}
write.csv(data, "d:\\760.project\\main_lr_less.csv",row.names=FALSE)

for (i in 1:length(data))
	if (sum(is.na(data[,i])) > 0)
		print(c(i,sum(is.na(data[,i]))))
