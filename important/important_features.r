data <- read.csv("d:\\760.project\\main_fill.csv")
cor_result <- c()
name_list <- names(data)
for (i in 1:length(data))
	cor_result = c(cor_result, cor(data[,i], data["TARGET"]))
dl = data.frame(name = name_list[order(abs(cor_result),decreasing=TRUE)], cor= cor_result[order(abs(cor_result),decreasing=TRUE)])
write.csv(dl, "d:\\760.project\\cor_list.csv")

data <- read.csv("d:\\760.project\\main\\main_lr.csv")
list1 <- read.csv("d:\\760.project\\important\\cor_list.csv")['Feature_Name']
for (i in 1:20) {
	for (j in i:20) {
		if (list1[i,] != list1[j,]) {
			data[paste(list1[i,],"_plus_",list1[j,], sep='')] = data[,which(names(data) == list1[i,])] + data[,which(names(data) == list1[j,])]
			data[paste(list1[i,],"_minus_",list1[j,], sep='')] = data[,which(names(data) == list1[i,])] - data[,which(names(data) == list1[j,])]
			data[paste(list1[i,],"_multi_",list1[j,], sep='')] = data[,which(names(data) == list1[i,])] * data[,which(names(data) == list1[j,])]
			if (sum(data[list1[j,]] == 0) == 0)
				data[paste(list1[i,],"_division_",list1[j,], sep='')] = data[,which(names(data) == list1[i,])] / data[,which(names(data) == list1[j,])]
			}
		}
	}
write.csv(data, "d:\\760.project\\main\\main_lr2.csv",row.names=FALSE)

l1 <- c()
for (i in 1:length(data))
	if (sum(is.na(data[,i])) > 0) {
		print(c(i,sum(is.na(data[,i]))))
		l1 = c(l1, i)}

for (i in 1:length(oot))
	if (sum(is.na(oot[,i])) > 0)
		print(c(i,sum(is.na(oot[,i]))))
		
for (i in 1:length(mrv))
	if (sum(is.na(mrv[,i])) > 0)
		print(c(i,sum(is.na(mrv[,i]))))

for (i in 1:length(data))
	if (sum(data[,i]==Inf) > 0)
		print(c(i,sum(data[,i]==Inf)))
		
l1 <- c()
for (i in 1:length(data))
	if (sum(data[,i]==-Inf) > 0) {
		print(c(i,sum(data[,i]==-Inf)))
		l1 = c(l1, i)}
l1 <- c()
for (i in 1:length(data))
	if (sum(data[,i]==Inf) > 0) {
		print(c(i,sum(data[,i]==Inf)))
		l1 = c(l1, i)}
l1 <- c()
for (i in 1:length(mrv))
	if (sum(is.na(mrv[,i])) > 0) {
		print(c(i,sum(is.na(mrv[,i]))))
		l1 = c(l1, i)}

data <- read.csv("d:\\760.project\\learnning_curve.csv")
plot(1:1500,data[,1], col="white", main="Learning Curve of XGBoost", ylab="AUC", xlab="n")
lines(1:1500,data[,1])