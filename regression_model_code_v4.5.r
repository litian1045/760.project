library(parallel)
library(ROCR)
library(Information)

cutpoint <- function(data, bins) {
  bins <- min(length(unique(data[which(!is.na(data))])),bins)
  if (bins==1)
    return(c(min(data, na.rm=TRUE),max(data, na.rm=TRUE)))
  if (bins==0)
    return(NULL)
  else {
    level <- quantile(data,1/bins,na.rm=TRUE)
    return(unique(c(min(data,na.rm=TRUE),cutpoint(data[which(data>level)],bins-1),max(data,na.rm=TRUE)))) }}

is_order <- function(data){
  data <- data[which(!is.na(data))]
  if (length(data)<=1)
    return(0)
  t_in <- 1
  t_de <- -1
  for (i in 2:length(data)) {
    if (!(data[i] >= data[i-1]))
	  t_in <- 0
	if (!(data[i] <= data[i-1]))
	  t_de <- 0}
  if (t_in==1)
    return(t_in)
  if (t_de==-1)
    return(t_de)
  return(0)}
	
psi <- function(data1, data2, bins, zoom=1000) {
  p1 <- data1[which(!is.na(data1))]*zoom
  p2 <- data2[which(!is.na(data2))]*zoom
  cutpoints <- cutpoint(p1,bins)
  if (length(unique(p1))==2)
    cutpoints <- c(cutpoints,max(cutpoints))
  min_list <- cutpoints[1:length(cutpoints)-1]
  max_list <- cutpoints[-1]
  binned <- paste("(",round(min_list,4),",",round(max_list,4),"]",sep="")
  bins_t <- length(max_list);max_list[bins_t]<-Inf
  t <- rep(0,bins_t);o <- rep(0,bins_t);tp <- rep(0,bins_t);op <- rep(0,bins_t);psi <- rep(0,bins_t)
  for (i in 1:bins_t) {
    t[i] <- length(which(p1<max_list[i]))-sum(t[1:i]);o[i] <- length(which(p2<max_list[i]))-sum(o[1:i])
	tp[i] <- t[i]/length(p1);op[i] <- o[i]/length(p2)
	psi[i] <- round((op[i]-tp[i]) * log(op[i]/tp[i]),4)}
  data_psi <- sum(psi)
  return(list(data.frame(binned,t,o,tp,op,psi),data_psi))}
  
ks <- function(lr=object, data, bad_flg=character(), bins, zoom=1000){
  p <- predict(lr,data,type='response')
  pred1 <- prediction(p,data[,which(names(data)==bad_flg)])
  perf1 <- performance(pred1, "tpr", "fpr")
  p <- p*zoom
  min_list <- p[order(p,decreasing = TRUE)[round((1:bins)/bins*length(p))]]
  max_list <- p[order(p,decreasing = TRUE)[round((0:(bins-1))/bins*length(p))+1]]
  level <- paste("(",round(min_list),",",round(max_list),"]",sep="")
  min_list[bins] <- min_list[bins]-1
  flg_0 <- rep(0,bins);flg_1 <- rep(0,bins);flg_0_cumsum <- rep(0,bins);flg_1_cumsum <- rep(0,bins)
  for (i in 1:bins) {
    flg_0[i] <- length(which(p>min_list[i]&data[,which(names(data)==bad_flg)]==0))-sum(flg_0[1:i])
    flg_1[i] <- length(which(p>min_list[i]&data[,which(names(data)==bad_flg)]==1))-sum(flg_1[1:i])
	flg_0_cumsum[i] <- sum(flg_0[1:i]);flg_1_cumsum[i] <- sum(flg_1[1:i])}
  rate <- round(flg_1/(flg_0+flg_1),3)
  data_ks <- round(max(perf1@'y.values'[[1]][round((1:bins)/bins*length(perf1@'y.values'[[1]]))]-perf1@'x.values'[[1]][round((1:bins)/bins*length(perf1@'x.values'[[1]]))])*100,2)
  return (list(data.frame(level,flg_0,flg_1,rate,flg_0_cumsum,flg_1_cumsum),data_ks))}
  
lplot <- function(data1, data2, bins) {
  p1 <- data1[which(!is.na(data1))]
  p2 <- data2[which(!is.na(data1))]
  cutpoints <- cutpoint(p1,bins)
  if (length(unique(p1))==2)
    cutpoints <- c(cutpoints,max(cutpoints))
  max_list <- cutpoints[-1]
  bins_t <- length(max_list)
  max_list[bins_t] <- max_list[bins_t]+1
  o <- rep(0,bins_t)
  t <- rep(0,bins_t)
  for (i in 1:bins_t){
    o[i] <- sum(p2[which(p1<max_list[i])])-sum(o[1:i])
	t[i] <- length(p2[which(p1<max_list[i])])-sum(t[1:i])}
  o <- o/t
  if (bins > bins_t)
    o <- c(o,rep(NA,bins-bins_t))
  if (bins_t == 0)
    o <- rep(NA,bins)
  return(o)}

data_info <- function(trn,oot,mrv,target,psi_bins=5,plot_bins=5,IV_bins=5,cal_iv=TRUE) {
  F_name <- names(trn)
  NA_N_trn <- c()
  NA_N_oot <- c()
  NA_N_mrv <- c()
  F_min <- c()
  F_p1 <- c()
  F_p99 <- c()
  F_max <- c()
  F_mean <- c()
  Unique_N <- c()
  psi_oot <- c()
  psi_mrv <- c()
  lplot_array <- array(NA,c(length(trn),plot_bins))
  lplot_order <- c()
  for (i in 1:length(trn)) {
    NA_N_trn[i] <- length(which(is.na(trn[i])))
	NA_N_oot[i] <- length(which(is.na(oot[i])))
	NA_N_mrv[i] <- length(which(is.na(mrv[i])))
    F_min[i] <- tryCatch({min(trn[,i],na.rm=TRUE)},error=function(e){NA},warning=function(w){NA})
    F_max[i] <- tryCatch({max(trn[,i],na.rm=TRUE)},error=function(e){NA},warning=function(w){NA})
    F_mean[i] <- tryCatch({mean(trn[,i],na.rm=TRUE)},error=function(e){NA},warning=function(w){NA})
	F_p1[i] <- tryCatch({quantile(trn[,i],0.01,na.rm=TRUE)},error=function(e){NA},warning=function(w){NA})
	F_p99[i] <- tryCatch({quantile(trn[,i],0.99,na.rm=TRUE)},error=function(e){NA},warning=function(w){NA})
    Unique_N[i] <- length(unique(trn[!is.na(trn[,i]),i]))
    psi_oot[i] <- tryCatch({psi(trn[,i],oot[,i],psi_bins)[[2]]},error=function(e){NA},warning=function(w){NA})
    psi_mrv[i] <- tryCatch({psi(trn[,i],mrv[,i],psi_bins)[[2]]},error=function(e){NA},warning=function(w){NA})
    lplot_array[i,1:plot_bins] <- tryCatch({lplot(trn[,i],trn[,which(names(trn)==target)],plot_bins)},error=function(e){rep(NA,plot_bins)})
	lplot_order[i] <- is_order(lplot_array[i,])}
  NA_rate_trn <- NA_N_trn / length(trn[,1])
  NA_rate_oot <- NA_N_oot / length(oot[,1])
  NA_rate_mrv <- NA_N_mrv / length(mrv[,1])
  F_IV <- rep(NA,length(trn))
  if (cal_iv==TRUE) {
    IV <- create_infotables(data=trn, y=target, bins=IV_bins)
    for (i in 1:length(IV$Summary[,1]))
      F_IV[which(names(trn)==IV$Summary[i,1])] <- IV$Summary[i,2]}
  F_info <- data.frame(F_name,NA_N_trn,NA_rate_trn,NA_rate_oot,NA_rate_mrv,F_min,F_p1,F_mean,F_p99,F_max,Unique_N,psi_oot,psi_mrv,F_IV,lplot_order)
  for (i in 1:plot_bins) {
    F_info <- cbind(F_info,lplot_array[,i])
    names(F_info)[length(F_info)] <- paste("lplot_",i,sep="")  }
  return(F_info)}
  
# function CV1
CV1 <- function(nf=n, bfn, cv, wk, seed, auc_diff, ks_diff, psi_diff, max_pv, max_cor, ks_bins, psi_bins, ks_startnmu, plot_order) {
  load("Flist.RData") 
  if (length(which(Flist==nf))>0 | nf == bfn)
    return(0)

  load("nonlist.RData") 
  if (length(which(nonlist==nf))>0)
    return(0)

  load(paste("tr_", bfn, ".RData", sep=""))
  tr1 = temp
  if (length(which(Flist>0))>0) {
    for (ii in 1:length(which(Flist>0))) {
      load(paste("tr_", Flist[ii], ".RData", sep=""))
	  tr1 <- cbind(tr1, temp) }}
  load(paste("tr_", nf, ".RData", sep=""))
  if (length(tr1)>1)
    for (ii in 2:length(tr1))
      if (abs(cor(tr1[,ii],temp))>max_cor)
        return(0)
  tr1 <- cbind(tr1, temp) 
  
  names(tr1)[1] <- "cv_target" 
  wcv <- rep(1,length(tr1[,1]))
  wcv[tr1[,1]==1] <- wk
  lrcv <- glm(cv_target~., data=tr1, weights=wcv, family=binomial())  
  if (length(which(summary(lrcv)$coef[,4]>=max_pv))>0)
    return(0)
	
  if (plot_order==TRUE){
    load("F_info.RData") 
    for (ii in 2:length(tr1))
      if (summary(lrcv)$coef[ii,1]/F_info$lplot_order[which(F_info$F_name==names(tr1[ii]))]<0 | F_info$lplot_order[which(F_info$F_name==names(tr1[ii]))]==0)
        return(0)}
  
  library(ROCR)
  load("psi_cv.RData")
  load("oot.RData")
  load("mrv.RData") 
  names(oot)[bfn] <- "cv_target" 
  names(mrv)[bfn] <- "cv_target" 
  trn_p <- predict(lrcv, tr1, type='response')
  oot_p <- predict(lrcv, oot, type='response')
  mrv_p <- predict(lrcv, mrv, type='response')
  if (psi_cv(trn_p,mrv_p,psi_bins)/psi_cv(trn_p,oot_p,psi_bins)>=psi_diff | is.na(psi_cv(trn_p,mrv_p,psi_bins)/psi_cv(trn_p,oot_p,psi_bins)))
	return(0)
  
  pred1 <- prediction(trn_p, tr1$cv_target)
  perf1 <- performance(pred1, "tpr", "fpr")
  auc_trn <- performance(pred1, "auc")@'y.values'[[1]] 
  ks_trn <- max(perf1@'y.values'[[1]][round((1:ks_bins)/ks_bins*length(perf1@'y.values'[[1]]))]-perf1@'x.values'[[1]][round((1:ks_bins)/ks_bins*length(perf1@'x.values'[[1]]))])
  
  pred1 <- prediction(oot_p, oot$cv_target)
  perf1 <- performance(pred1, "tpr", "fpr")
  auc_oot <- performance(pred1, "auc")@'y.values'[[1]] 
  ks_oot <- max(perf1@'y.values'[[1]][round((1:ks_bins)/ks_bins*length(perf1@'y.values'[[1]]))]-perf1@'x.values'[[1]][round((1:ks_bins)/ks_bins*length(perf1@'x.values'[[1]]))])
  
  #pred1 <- prediction(mrv_p, mrv$cv_target)
  #perf1 <- performance(pred1, "tpr", "fpr")
  #auc_mrv <- performance(pred1, "auc")@'y.values'[[1]]  
  #ks_mrv <- max(perf1@'y.values'[[1]][round((1:ks_bins)/ks_bins*length(perf1@'y.values'[[1]]))]-perf1@'x.values'[[1]][round((1:ks_bins)/ks_bins*length(perf1@'x.values'[[1]]))])
  
  t1 <- sum(auc_trn,auc_oot)/2
  m1 <- sum(ks_trn,ks_oot)/2
  t2 <- sum(abs(auc_trn-t1),abs(auc_oot-t1))
  t3 <- sum(abs(ks_trn-m1),abs(ks_oot-m1))  
  
  if (t2<auc_diff & (t3<ks_diff | length(tr1)<=ks_startnmu)) {
	op <- 0
	# Cross Validation
	tr1_good <- tr1[which(tr1[,1]==0),]
	tr1_bad <- tr1[which(tr1[,1]==1),]
    set.seed(seed)
    sample1_good <- sample(1:length(tr1_good[, 1]), length(tr1_good[, 1])-length(tr1_good[, 1])%%cv)
	set.seed(seed)
	sample1_bad <- sample(1:length(tr1_bad[, 1]), length(tr1_bad[, 1])-length(tr1_bad[, 1])%%cv)
    dis_good=length(sample1_good)/cv
	dis_bad=length(sample1_bad)/cv
    for (k in 1:cv) {
      trcv <- rbind(tr1_good[-sample1_good[((k-1)*dis_good+1):(k*dis_good)],],tr1_bad[-sample1_bad[((k-1)*dis_bad+1):(k*dis_bad)],])
      tecv <- rbind(tr1_good[+sample1_good[((k-1)*dis_good+1):(k*dis_good)],],tr1_bad[+sample1_bad[((k-1)*dis_bad+1):(k*dis_bad)],])
	  wcv <- rep(1,length(trcv[,1]))
	  wcv[trcv[,1]==1] <- wk
      lrcv <- glm(cv_target~., data=trcv, weights=wcv, family=binomial())
	  trcv_p <- predict(lrcv, tecv, type='response')
	  pred1 <- prediction(trcv_p, tecv[,1])
      auc_trcv <- performance(pred1, "auc")@'y.values'[[1]] 
	  op <- op + auc_trcv}
    return(op/cv)}
  else 
    return(0)}

psi_cv <- function(data1,data2,bins) {
  p1 <- data1[which(!is.na(data1))]
  p2 <- data2[which(!is.na(data2))]
  max_list <- quantile(data1,(1:bins)/bins,na.rm=TRUE)
  max_list[bins] <- max_list[bins]+1
  t <- rep(0,bins);o <- rep(0,bins);tp <- rep(0,bins);op <- rep(0,bins);psi <- rep(0,bins)
  for (i in 1:bins) {
    t[i] <- length(which(p1<max_list[i]))-sum(t[1:i]);o[i] <- length(which(p2<max_list[i]))-sum(o[1:i])
	tp[i] <- t[i]/length(p1);op[i] <- o[i]/length(p2)
	psi[i] <- round((op[i]-tp[i]) * log(op[i]/tp[i]),4)}
  data_psi <- sum(psi)
  return(data_psi)}

	
lr_stepwise <- function(tr=data.frame, oot=data.frame, mrv=data.frame, bad_flg=character(), F_info, nround, cv_num=3, seed_value=round(runif(1)*100000000), nonlist=c(),
                        weight_value=1, cl_num=1, auc_diff=0.03, ks_diff=0.03, psi_diff=Inf, max_pv=0.05, max_cor=0.3, ks_bins=5, psi_bins=5, ks_startnmu=5, plot_order=TRUE) {
  # save features
  print(paste("seed = ",seed_value,sep=""))
  for (i in 1:length(tr)) { 
    temp <- tr[i]
    save(temp, file=paste("tr_", i, ".RData", sep=""))}
  save(oot, file="oot.RData")
  save(mrv, file="mrv.RData")
  save(psi_cv, file="psi_cv.RData")
  save(F_info, file="F_info.RData")
  save(nonlist, file="nonlist.RData")
  bad_flg_num <- which(names(tr)==bad_flg) 
  # variable selection with greatest AUC
  kn <- nround
  Flist <- rep(0, kn)
  Nlist <- rep("", kn)
  Alist <- rep(0, kn)
  cl <- makeCluster(cl_num)
  for (i in 1:kn) {
    save(Flist, file="Flist.RData")
    plist <- c(1:length(tr))
    results <- parLapply(cl, plist, CV1, bfn=bad_flg_num, cv=cv_num, wk=weight_value, seed=seed_value,
	                     auc_diff=auc_diff, ks_diff=ks_diff, psi_diff=psi_diff, max_pv=max_pv, max_cor=max_cor,
						 ks_bins=ks_bins, psi_bins=psi_bins, ks_startnmu=ks_startnmu, plot_order=plot_order)
    temp1 <- do.call('rbind', results)
	if (max(temp1)>0) {
      Flist[i] <- min(which(temp1==max(temp1)))
      Nlist[i] <- names(tr[Flist[i]])
      Alist[i] <- round(max(temp1),3)
      print(paste("round = ",i,"  AUC = ",Alist[i],"  Feature Number = ",Flist[i],"  Feature Name = ",Nlist[i],sep=""))}
	else {
	  print(paste("round = ",i,"  NULL",sep=""))}
	}
  stopCluster(cl)
  Olist <- data.frame(Feature_Number=Flist,Feature_Name=Nlist,AUC=Alist)
  return(Olist)}

data <- read.csv("d:\\760.project\\main_cor.csv")
  
trn <- data_all[which(data_all$request_month>=201703 & data_all$request_month<=201709 & data_all$filter_flg==0 & data_all$investor_id==76 & data_all$new_customer_flg==0),]
oot <- data_all[which(data_all$request_month>=201710 & data_all$request_month<=201710 & data_all$filter_flg==0 & data_all$investor_id==76 & data_all$new_customer_flg==0),]
mrv <- data_all[which(data_all$request_month>=201711 & data_all$request_month<=201711 & data_all$filter_flg==0 & data_all$investor_id==76 & data_all$new_customer_flg==0),]

trn <- data_all[which(data_all$request_month>=201703 & data_all$request_month<=201710),]
oot <- data_all[which(data_all$request_month>=201711 & data_all$request_month<=201711),]
mrv <- data_all[which(data_all$request_month>=201712 & data_all$request_month<=201712),]

test_index <- read.csv("d:\\760.project\\test_set_index.csv", head=FALSE) + 1
trn <- data[-test_index[,1],]
oot_index <- sample(length(trn[,1]),size=30000)
oot <- trn[oot_index,]
trn <- trn[-oot_index,]
mrv <- data[test_index[,1],]

# Target variable must be binary
# psi_bins default 5
# plot_bins default 5
# IV_bins default 5
# cal_iv should the IV be calculated, default TRUE
F_info <- data_info(trn,oot,mrv,target="TARGET",plot_bins=5,cal_iv=FALSE)
write.csv(F_info,"d:\\760.project\\F_info_cor.csv",row.names=FALSE)

var_list <- c("TARGET",
"FLAG_IS_OLD_CUSTOMER",
"DAYS_BIRTH",
"DAYS_ID_PUBLISH",
"FLAG_EMP_PHONE",
"FLAG_WORK_PHONE",
"FLAG_CONT_MOBILE",
"FLAG_PHONE",
"FLAG_EMAIL",
"REG_REGION_NOT_LIVE_REGION",
"REG_REGION_NOT_WORK_REGION",
"LIVE_REGION_NOT_WORK_REGION",
"REG_CITY_NOT_LIVE_CITY",
"REG_CITY_NOT_WORK_CITY",
"LIVE_CITY_NOT_WORK_CITY",
"EXT_SOURCE_1",
"EXT_SOURCE_2",
"EXT_SOURCE_3",
"LIVINGAREA_MODE",
"OBS_30_CNT_SOCIAL_CIRCLE",
"DEF_30_CNT_SOCIAL_CIRCLE",
"FLAG_DOCUMENT_2",
"FLAG_DOCUMENT_3",
"FLAG_DOCUMENT_4",
"FLAG_DOCUMENT_5",
"FLAG_DOCUMENT_6",
"FLAG_DOCUMENT_7",
"FLAG_DOCUMENT_8",
"FLAG_DOCUMENT_9",
"FLAG_DOCUMENT_11",
"FLAG_DOCUMENT_13",
"FLAG_DOCUMENT_14",
"FLAG_DOCUMENT_15",
"FLAG_DOCUMENT_16",
"FLAG_DOCUMENT_17",
"FLAG_DOCUMENT_18",
"FLAG_DOCUMENT_19",
"FLAG_DOCUMENT_20",
"FLAG_DOCUMENT_21",
"main_NAME_CONTRACT_TYPE_Cash.loans_count",
"main_NAME_CONTRACT_TYPE_Cash.loans_count_norm",
"main_NAME_CONTRACT_TYPE_Revolving.loans_count",
"main_NAME_CONTRACT_TYPE_Revolving.loans_count_norm",
"main_CODE_GENDER_F_count",
"main_CODE_GENDER_F_count_norm",
"main_CODE_GENDER_M_count",
"main_CODE_GENDER_M_count_norm",
"main_FLAG_OWN_CAR_N_count",
"main_FLAG_OWN_CAR_N_count_norm",
"main_FLAG_OWN_CAR_Y_count",
"main_FLAG_OWN_CAR_Y_count_norm",
"main_FLAG_OWN_REALTY_N_count",
"main_FLAG_OWN_REALTY_N_count_norm",
"main_FLAG_OWN_REALTY_Y_count",
"main_FLAG_OWN_REALTY_Y_count_norm",
"main_NAME_TYPE_SUITE_Children_count",
"main_NAME_TYPE_SUITE_Children_count_norm",
"main_NAME_TYPE_SUITE_Family_count",
"main_NAME_TYPE_SUITE_Family_count_norm",
"main_NAME_TYPE_SUITE_Group.of.people_count",
"main_NAME_TYPE_SUITE_Group.of.people_count_norm",
"main_NAME_TYPE_SUITE_Other_A_count",
"main_NAME_TYPE_SUITE_Other_A_count_norm",
"main_NAME_TYPE_SUITE_Other_B_count",
"main_NAME_TYPE_SUITE_Other_B_count_norm",
"main_NAME_TYPE_SUITE_Spouse..partner_count",
"main_NAME_TYPE_SUITE_Spouse..partner_count_norm",
"main_NAME_TYPE_SUITE_Unaccompanied_count",
"main_NAME_TYPE_SUITE_Unaccompanied_count_norm",
"main_NAME_INCOME_TYPE_Commercial.associate_count",
"main_NAME_INCOME_TYPE_Commercial.associate_count_norm",
"main_NAME_INCOME_TYPE_Pensioner_count",
"main_NAME_INCOME_TYPE_Pensioner_count_norm",
"main_NAME_INCOME_TYPE_State.servant_count",
"main_NAME_INCOME_TYPE_State.servant_count_norm",
"main_NAME_INCOME_TYPE_Student_count",
"main_NAME_INCOME_TYPE_Student_count_norm",
"main_NAME_INCOME_TYPE_Unemployed_count",
"main_NAME_INCOME_TYPE_Unemployed_count_norm",
"main_NAME_INCOME_TYPE_Working_count",
"main_NAME_INCOME_TYPE_Working_count_norm",
"main_NAME_EDUCATION_TYPE_Academic.degree_count",
"main_NAME_EDUCATION_TYPE_Academic.degree_count_norm",
"main_NAME_EDUCATION_TYPE_Higher.education_count",
"main_NAME_EDUCATION_TYPE_Higher.education_count_norm",
"main_NAME_EDUCATION_TYPE_Incomplete.higher_count",
"main_NAME_EDUCATION_TYPE_Incomplete.higher_count_norm",
"main_NAME_EDUCATION_TYPE_Lower.secondary_count",
"main_NAME_EDUCATION_TYPE_Lower.secondary_count_norm",
"main_NAME_EDUCATION_TYPE_Secondary...secondary.special_count",
"main_NAME_EDUCATION_TYPE_Secondary...secondary.special_count_norm",
"main_NAME_FAMILY_STATUS_Civil.marriage_count",
"main_NAME_FAMILY_STATUS_Civil.marriage_count_norm",
"main_NAME_FAMILY_STATUS_Married_count",
"main_NAME_FAMILY_STATUS_Married_count_norm",
"main_NAME_FAMILY_STATUS_Separated_count",
"main_NAME_FAMILY_STATUS_Separated_count_norm",
"main_NAME_FAMILY_STATUS_Single...not.married_count",
"main_NAME_FAMILY_STATUS_Single...not.married_count_norm",
"main_NAME_FAMILY_STATUS_Widow_count",
"main_NAME_FAMILY_STATUS_Widow_count_norm",
"main_NAME_HOUSING_TYPE_Co.op.apartment_count",
"main_NAME_HOUSING_TYPE_Co.op.apartment_count_norm",
"main_NAME_HOUSING_TYPE_House...apartment_count",
"main_NAME_HOUSING_TYPE_House...apartment_count_norm",
"main_NAME_HOUSING_TYPE_Municipal.apartment_count",
"main_NAME_HOUSING_TYPE_Municipal.apartment_count_norm",
"main_NAME_HOUSING_TYPE_Office.apartment_count",
"main_NAME_HOUSING_TYPE_Office.apartment_count_norm",
"main_NAME_HOUSING_TYPE_Rented.apartment_count",
"main_NAME_HOUSING_TYPE_Rented.apartment_count_norm",
"main_NAME_HOUSING_TYPE_With.parents_count",
"main_NAME_HOUSING_TYPE_With.parents_count_norm",
"main_OCCUPATION_TYPE_Accountants_count",
"main_OCCUPATION_TYPE_Accountants_count_norm",
"main_OCCUPATION_TYPE_Cleaning.staff_count",
"main_OCCUPATION_TYPE_Cleaning.staff_count_norm",
"main_OCCUPATION_TYPE_Cooking.staff_count",
"main_OCCUPATION_TYPE_Cooking.staff_count_norm",
"main_OCCUPATION_TYPE_Core.staff_count",
"main_OCCUPATION_TYPE_Core.staff_count_norm",
"main_OCCUPATION_TYPE_Drivers_count",
"main_OCCUPATION_TYPE_Drivers_count_norm",
"main_OCCUPATION_TYPE_HR.staff_count",
"main_OCCUPATION_TYPE_HR.staff_count_norm",
"main_OCCUPATION_TYPE_High.skill.tech.staff_count",
"main_OCCUPATION_TYPE_High.skill.tech.staff_count_norm",
"main_OCCUPATION_TYPE_IT.staff_count",
"main_OCCUPATION_TYPE_IT.staff_count_norm",
"main_OCCUPATION_TYPE_Laborers_count",
"main_OCCUPATION_TYPE_Laborers_count_norm",
"main_OCCUPATION_TYPE_Low.skill.Laborers_count",
"main_OCCUPATION_TYPE_Low.skill.Laborers_count_norm",
"main_OCCUPATION_TYPE_Managers_count",
"main_OCCUPATION_TYPE_Managers_count_norm",
"main_OCCUPATION_TYPE_Medicine.staff_count",
"main_OCCUPATION_TYPE_Medicine.staff_count_norm",
"main_OCCUPATION_TYPE_Private.service.staff_count",
"main_OCCUPATION_TYPE_Private.service.staff_count_norm",
"main_OCCUPATION_TYPE_Realty.agents_count",
"main_OCCUPATION_TYPE_Realty.agents_count_norm",
"main_OCCUPATION_TYPE_Sales.staff_count",
"main_OCCUPATION_TYPE_Sales.staff_count_norm",
"main_OCCUPATION_TYPE_Secretaries_count",
"main_OCCUPATION_TYPE_Secretaries_count_norm",
"main_OCCUPATION_TYPE_Security.staff_count",
"main_OCCUPATION_TYPE_Security.staff_count_norm",
"main_OCCUPATION_TYPE_Waiters.barmen.staff_count",
"main_OCCUPATION_TYPE_Waiters.barmen.staff_count_norm",
"main_WEEKDAY_APPR_PROCESS_START_FRIDAY_count",
"main_WEEKDAY_APPR_PROCESS_START_FRIDAY_count_norm",
"main_WEEKDAY_APPR_PROCESS_START_MONDAY_count",
"main_WEEKDAY_APPR_PROCESS_START_MONDAY_count_norm",
"main_WEEKDAY_APPR_PROCESS_START_SATURDAY_count",
"main_WEEKDAY_APPR_PROCESS_START_SATURDAY_count_norm",
"main_WEEKDAY_APPR_PROCESS_START_SUNDAY_count",
"main_WEEKDAY_APPR_PROCESS_START_SUNDAY_count_norm",
"main_WEEKDAY_APPR_PROCESS_START_THURSDAY_count",
"main_WEEKDAY_APPR_PROCESS_START_THURSDAY_count_norm",
"main_WEEKDAY_APPR_PROCESS_START_TUESDAY_count",
"main_WEEKDAY_APPR_PROCESS_START_TUESDAY_count_norm",
"main_WEEKDAY_APPR_PROCESS_START_WEDNESDAY_count",
"main_WEEKDAY_APPR_PROCESS_START_WEDNESDAY_count_norm",
"main_ORGANIZATION_TYPE_Advertising_count",
"main_ORGANIZATION_TYPE_Advertising_count_norm",
"main_ORGANIZATION_TYPE_Agriculture_count",
"main_ORGANIZATION_TYPE_Agriculture_count_norm",
"main_ORGANIZATION_TYPE_Bank_count",
"main_ORGANIZATION_TYPE_Bank_count_norm",
"main_ORGANIZATION_TYPE_Business.Entity.Type.1_count",
"main_ORGANIZATION_TYPE_Business.Entity.Type.1_count_norm",
"main_ORGANIZATION_TYPE_Business.Entity.Type.2_count",
"main_ORGANIZATION_TYPE_Business.Entity.Type.2_count_norm",
"main_ORGANIZATION_TYPE_Business.Entity.Type.3_count",
"main_ORGANIZATION_TYPE_Business.Entity.Type.3_count_norm",
"main_ORGANIZATION_TYPE_Cleaning_count",
"main_ORGANIZATION_TYPE_Cleaning_count_norm",
"main_ORGANIZATION_TYPE_Construction_count",
"main_ORGANIZATION_TYPE_Construction_count_norm",
"main_ORGANIZATION_TYPE_Culture_count",
"main_ORGANIZATION_TYPE_Culture_count_norm",
"main_ORGANIZATION_TYPE_Electricity_count",
"main_ORGANIZATION_TYPE_Electricity_count_norm",
"main_ORGANIZATION_TYPE_Emergency_count",
"main_ORGANIZATION_TYPE_Emergency_count_norm",
"main_ORGANIZATION_TYPE_Government_count",
"main_ORGANIZATION_TYPE_Government_count_norm",
"main_ORGANIZATION_TYPE_Hotel_count",
"main_ORGANIZATION_TYPE_Hotel_count_norm",
"main_ORGANIZATION_TYPE_Housing_count",
"main_ORGANIZATION_TYPE_Housing_count_norm",
"main_ORGANIZATION_TYPE_Industry..type.1_count",
"main_ORGANIZATION_TYPE_Industry..type.1_count_norm",
"main_ORGANIZATION_TYPE_Industry..type.10_count",
"main_ORGANIZATION_TYPE_Industry..type.10_count_norm",
"main_ORGANIZATION_TYPE_Industry..type.11_count",
"main_ORGANIZATION_TYPE_Industry..type.11_count_norm",
"main_ORGANIZATION_TYPE_Industry..type.12_count",
"main_ORGANIZATION_TYPE_Industry..type.12_count_norm",
"main_ORGANIZATION_TYPE_Industry..type.13_count",
"main_ORGANIZATION_TYPE_Industry..type.13_count_norm",
"main_ORGANIZATION_TYPE_Industry..type.2_count",
"main_ORGANIZATION_TYPE_Industry..type.2_count_norm",
"main_ORGANIZATION_TYPE_Industry..type.3_count",
"main_ORGANIZATION_TYPE_Industry..type.3_count_norm",
"main_ORGANIZATION_TYPE_Industry..type.4_count",
"main_ORGANIZATION_TYPE_Industry..type.4_count_norm",
"main_ORGANIZATION_TYPE_Industry..type.5_count",
"main_ORGANIZATION_TYPE_Industry..type.5_count_norm",
"main_ORGANIZATION_TYPE_Industry..type.6_count",
"main_ORGANIZATION_TYPE_Industry..type.6_count_norm",
"main_ORGANIZATION_TYPE_Industry..type.7_count",
"main_ORGANIZATION_TYPE_Industry..type.7_count_norm",
"main_ORGANIZATION_TYPE_Industry..type.8_count",
"main_ORGANIZATION_TYPE_Industry..type.8_count_norm",
"main_ORGANIZATION_TYPE_Industry..type.9_count",
"main_ORGANIZATION_TYPE_Industry..type.9_count_norm",
"main_ORGANIZATION_TYPE_Insurance_count",
"main_ORGANIZATION_TYPE_Insurance_count_norm",
"main_ORGANIZATION_TYPE_Kindergarten_count",
"main_ORGANIZATION_TYPE_Kindergarten_count_norm",
"main_ORGANIZATION_TYPE_Legal.Services_count",
"main_ORGANIZATION_TYPE_Legal.Services_count_norm",
"main_ORGANIZATION_TYPE_Medicine_count",
"main_ORGANIZATION_TYPE_Medicine_count_norm",
"main_ORGANIZATION_TYPE_Military_count",
"main_ORGANIZATION_TYPE_Military_count_norm",
"main_ORGANIZATION_TYPE_Mobile_count",
"main_ORGANIZATION_TYPE_Mobile_count_norm",
"main_ORGANIZATION_TYPE_Other_count",
"main_ORGANIZATION_TYPE_Other_count_norm",
"main_ORGANIZATION_TYPE_Police_count",
"main_ORGANIZATION_TYPE_Police_count_norm",
"main_ORGANIZATION_TYPE_Postal_count",
"main_ORGANIZATION_TYPE_Postal_count_norm",
"main_ORGANIZATION_TYPE_Realtor_count",
"main_ORGANIZATION_TYPE_Realtor_count_norm",
"main_ORGANIZATION_TYPE_Religion_count",
"main_ORGANIZATION_TYPE_Religion_count_norm",
"main_ORGANIZATION_TYPE_Restaurant_count",
"main_ORGANIZATION_TYPE_Restaurant_count_norm",
"main_ORGANIZATION_TYPE_School_count",
"main_ORGANIZATION_TYPE_School_count_norm",
"main_ORGANIZATION_TYPE_Security_count",
"main_ORGANIZATION_TYPE_Security_count_norm",
"main_ORGANIZATION_TYPE_Security.Ministries_count",
"main_ORGANIZATION_TYPE_Security.Ministries_count_norm",
"main_ORGANIZATION_TYPE_Self.employed_count",
"main_ORGANIZATION_TYPE_Self.employed_count_norm",
"main_ORGANIZATION_TYPE_Services_count",
"main_ORGANIZATION_TYPE_Services_count_norm",
"main_ORGANIZATION_TYPE_Telecom_count",
"main_ORGANIZATION_TYPE_Telecom_count_norm",
"main_ORGANIZATION_TYPE_Trade..type.1_count",
"main_ORGANIZATION_TYPE_Trade..type.1_count_norm",
"main_ORGANIZATION_TYPE_Trade..type.2_count",
"main_ORGANIZATION_TYPE_Trade..type.2_count_norm",
"main_ORGANIZATION_TYPE_Trade..type.3_count",
"main_ORGANIZATION_TYPE_Trade..type.3_count_norm",
"main_ORGANIZATION_TYPE_Trade..type.4_count",
"main_ORGANIZATION_TYPE_Trade..type.4_count_norm",
"main_ORGANIZATION_TYPE_Trade..type.5_count",
"main_ORGANIZATION_TYPE_Trade..type.5_count_norm",
"main_ORGANIZATION_TYPE_Trade..type.6_count",
"main_ORGANIZATION_TYPE_Trade..type.6_count_norm",
"main_ORGANIZATION_TYPE_Trade..type.7_count",
"main_ORGANIZATION_TYPE_Trade..type.7_count_norm",
"main_ORGANIZATION_TYPE_Transport..type.1_count",
"main_ORGANIZATION_TYPE_Transport..type.1_count_norm",
"main_ORGANIZATION_TYPE_Transport..type.2_count",
"main_ORGANIZATION_TYPE_Transport..type.2_count_norm",
"main_ORGANIZATION_TYPE_Transport..type.3_count",
"main_ORGANIZATION_TYPE_Transport..type.3_count_norm",
"main_ORGANIZATION_TYPE_Transport..type.4_count",
"main_ORGANIZATION_TYPE_Transport..type.4_count_norm",
"main_ORGANIZATION_TYPE_University_count",
"main_ORGANIZATION_TYPE_University_count_norm",
"main_ORGANIZATION_TYPE_XNA_count",
"main_ORGANIZATION_TYPE_XNA_count_norm",
"main_FONDKAPREMONT_MODE_not.specified_count",
"main_FONDKAPREMONT_MODE_not.specified_count_norm",
"main_FONDKAPREMONT_MODE_org.spec.account_count",
"main_FONDKAPREMONT_MODE_org.spec.account_count_norm",
"main_FONDKAPREMONT_MODE_reg.oper.account_count",
"main_FONDKAPREMONT_MODE_reg.oper.account_count_norm",
"main_FONDKAPREMONT_MODE_reg.oper.spec.account_count",
"main_FONDKAPREMONT_MODE_reg.oper.spec.account_count_norm",
"main_HOUSETYPE_MODE_block.of.flats_count",
"main_HOUSETYPE_MODE_block.of.flats_count_norm",
"main_HOUSETYPE_MODE_specific.housing_count",
"main_HOUSETYPE_MODE_specific.housing_count_norm",
"main_HOUSETYPE_MODE_terraced.house_count",
"main_HOUSETYPE_MODE_terraced.house_count_norm",
"main_WALLSMATERIAL_MODE_Block_count",
"main_WALLSMATERIAL_MODE_Block_count_norm",
"main_WALLSMATERIAL_MODE_Mixed_count",
"main_WALLSMATERIAL_MODE_Mixed_count_norm",
"main_WALLSMATERIAL_MODE_Monolithic_count",
"main_WALLSMATERIAL_MODE_Monolithic_count_norm",
"main_WALLSMATERIAL_MODE_Others_count",
"main_WALLSMATERIAL_MODE_Others_count_norm",
"main_WALLSMATERIAL_MODE_Panel_count",
"main_WALLSMATERIAL_MODE_Panel_count_norm",
"main_WALLSMATERIAL_MODE_Stone..brick_count",
"main_WALLSMATERIAL_MODE_Stone..brick_count_norm",
"main_WALLSMATERIAL_MODE_Wooden_count",
"main_WALLSMATERIAL_MODE_Wooden_count_norm",
"main_EMERGENCYSTATE_MODE_No_count",
"main_EMERGENCYSTATE_MODE_No_count_norm",
"main_EMERGENCYSTATE_MODE_Yes_count",
"main_EMERGENCYSTATE_MODE_Yes_count_norm",
"client_bureau_balance_MONTHS_BALANCE_count_mean",
"client_bureau_balance_MONTHS_BALANCE_count_max",
"client_bureau_balance_MONTHS_BALANCE_count_min",
"client_bureau_balance_MONTHS_BALANCE_mean_mean",
"client_bureau_balance_MONTHS_BALANCE_mean_max",
"client_bureau_balance_MONTHS_BALANCE_mean_min",
"client_bureau_balance_MONTHS_BALANCE_mean_sum",
"client_bureau_balance_MONTHS_BALANCE_max_sum",
"client_bureau_balance_MONTHS_BALANCE_min_mean",
"client_bureau_balance_MONTHS_BALANCE_min_max",
"client_bureau_balance_MONTHS_BALANCE_min_min",
"client_bureau_balance_MONTHS_BALANCE_min_sum",
"client_bureau_balance_MONTHS_BALANCE_sum_mean",
"client_bureau_balance_MONTHS_BALANCE_sum_min",
"client_bureau_balance_MONTHS_BALANCE_sum_sum",
"client_bureau_balance_csv_STATUS_0_count_mean",
"client_bureau_balance_csv_STATUS_0_count_norm_mean",
"client_bureau_balance_csv_STATUS_0_count_norm_max",
"client_bureau_balance_csv_STATUS_1_count_mean",
"client_bureau_balance_csv_STATUS_1_count_max",
"client_bureau_balance_csv_STATUS_1_count_norm_mean",
"client_bureau_balance_csv_STATUS_1_count_norm_max",
"client_bureau_balance_csv_STATUS_1_count_norm_sum",
"client_bureau_balance_csv_STATUS_2_count_norm_sum",
"client_bureau_balance_csv_STATUS_3_count_sum",
"client_bureau_balance_csv_STATUS_4_count_norm_sum",
"client_bureau_balance_csv_STATUS_5_count_max",
"client_bureau_balance_csv_STATUS_C_count_mean",
"client_bureau_balance_csv_STATUS_C_count_max",
"client_bureau_balance_csv_STATUS_C_count_norm_mean",
"client_bureau_balance_csv_STATUS_X_count_max",
"cc_AMT_BALANCE_min",
"cc_AMT_CREDIT_LIMIT_ACTUAL_sum",
"flag_pay_on_time_sum",
"flag_pay_early_sum",
"flag_pay_late_sum",
"flag_pay_same_sum",
"flag_pay_more_sum",
"flag_pay_less_sum",
"AMT_INSTALMENT_sum",
"AMT_PAYMENT_sum",
"days_pay_days_mean",
"flag_pay_on_time_mean",
"flag_pay_early_mean",
"flag_pay_same_mean",
"flag_pay_more_mean",
"flag_pay_less_mean",
"count_payment",
"AMT_payment_instalment_p",
"SK_DPD_DEF_max",
"SK_DPD_mean",
"SK_DPD_DEF_sum",
"per_NAME_CONTRACT_TYPE_Cash.loans_count",
"per_NAME_CONTRACT_TYPE_Revolving.loans_count",
"per_NAME_CONTRACT_TYPE_Revolving.loans_count_norm",
"per_NAME_CONTRACT_TYPE_XNA_count",
"per_NAME_CONTRACT_STATUS_Approved_count",
"per_NAME_CONTRACT_STATUS_Approved_count_norm",
"per_NAME_CONTRACT_STATUS_Canceled_count",
"per_NAME_CONTRACT_STATUS_Canceled_count_norm",
"per_NAME_CONTRACT_STATUS_Refused_count",
"per_NAME_PAYMENT_TYPE_Cash.through.the.bank_count_norm",
"per_NAME_PAYMENT_TYPE_XNA_count",
"per_CODE_REJECT_REASON_HC_count",
"per_CODE_REJECT_REASON_HC_count_norm",
"per_CODE_REJECT_REASON_LIMIT_count_norm",
"per_CODE_REJECT_REASON_SCO_count_norm",
"per_CODE_REJECT_REASON_SCOFR_count",
"per_CODE_REJECT_REASON_SCOFR_count_norm",
"per_CODE_REJECT_REASON_XAP_count_norm",
"per_CODE_REJECT_REASON_XNA_count",
"per_NAME_TYPE_SUITE_Unaccompanied_count_norm",
"per_NAME_PORTFOLIO_Cards_count",
"per_NAME_PORTFOLIO_Cards_count_norm",
"per_NAME_PORTFOLIO_POS_count_norm",
"per_NAME_PORTFOLIO_XNA_count",
"per_NAME_PRODUCT_TYPE_XNA_count_norm",
"per_NAME_PRODUCT_TYPE_walk.in_count",
"per_NAME_PRODUCT_TYPE_walk.in_count_norm",
"per_NAME_YIELD_GROUP_XNA_count",
"per_NAME_YIELD_GROUP_high_count",
"per_NAME_YIELD_GROUP_high_count_norm",
"per_NAME_YIELD_GROUP_low_action_count_norm",
"per_NAME_YIELD_GROUP_low_normal_count_norm",
"per_AMT_ANNUITY_mean",
"per_AMT_ANNUITY_min",
"per_AMT_APPLICATION_mean",
"per_AMT_APPLICATION_min",
"per_AMT_DOWN_PAYMENT_mean",
"per_AMT_DOWN_PAYMENT_sum",
"per_HOUR_APPR_PROCESS_START_mean",
"per_HOUR_APPR_PROCESS_START_max",
"per_RATE_DOWN_PAYMENT_mean",
"per_RATE_DOWN_PAYMENT_sum",
"per_DAYS_DECISION_mean",
"per_DAYS_DECISION_max",
"per_DAYS_DECISION_min",
"per_DAYS_DECISION_sum",
"per_SELLERPLACE_AREA_mean",
"per_SELLERPLACE_AREA_max",
"per_SELLERPLACE_AREA_sum",
"per_DAYS_FIRST_DRAWING_count",
"per_DAYS_FIRST_DRAWING_sum",
"per_DAYS_FIRST_DUE_count",
"per_DAYS_FIRST_DUE_min",
"per_DAYS_LAST_DUE_1ST_VERSION_count",
"per_DAYS_LAST_DUE_1ST_VERSION_mean",
"per_DAYS_LAST_DUE_1ST_VERSION_min",
"per_DAYS_LAST_DUE_count",
"per_DAYS_LAST_DUE_min",
"per_DAYS_TERMINATION_count",
"per_DAYS_TERMINATION_min",
"per_NFLAG_INSURED_ON_APPROVAL_count",
"per_NFLAG_INSURED_ON_APPROVAL_max",
"per_NFLAG_INSURED_ON_APPROVAL_min",
"per_NFLAG_INSURED_ON_APPROVAL_sum",
"EXT_SOURCE_2_plus_EXT_SOURCE_3",
"EXT_SOURCE_2_multi_EXT_SOURCE_3",
"EXT_SOURCE_2_plus_EXT_SOURCE_1",
"EXT_SOURCE_2_plus_DAYS_BIRTH",
"EXT_SOURCE_2_multi_DAYS_BIRTH",
"EXT_SOURCE_2_minus_per_NAME_CONTRACT_STATUS_Refused_count_norm",
"EXT_SOURCE_2_multi_per_NAME_CONTRACT_STATUS_Refused_count_norm",
"EXT_SOURCE_2_plus_flag_pay_late_mean",
"EXT_SOURCE_2_multi_flag_pay_late_mean",
"EXT_SOURCE_2_plus_flag_pay_on_time_mean",
"EXT_SOURCE_2_multi_flag_pay_on_time_mean",
"EXT_SOURCE_2_minus_per_NAME_CONTRACT_STATUS_Approved_count_norm",
"EXT_SOURCE_2_plus_REGION_RATING_CLIENT_W_CITY",
"EXT_SOURCE_2_plus_flag_pay_less_mean",
"EXT_SOURCE_2_multi_flag_pay_less_mean",
"EXT_SOURCE_2_minus_per_NAME_PRODUCT_TYPE_walk.in_count",
"EXT_SOURCE_2_plus_main_NAME_INCOME_TYPE_Working_count",
"EXT_SOURCE_2_multi_main_NAME_INCOME_TYPE_Working_count",
"EXT_SOURCE_2_plus_main_NAME_INCOME_TYPE_Working_count_norm",
"EXT_SOURCE_2_multi_main_NAME_INCOME_TYPE_Working_count_norm",
"EXT_SOURCE_2_plus_main_NAME_EDUCATION_TYPE_Higher.education_count",
"EXT_SOURCE_2_multi_main_NAME_EDUCATION_TYPE_Higher.education_count",
"EXT_SOURCE_2_plus_main_NAME_EDUCATION_TYPE_Higher.education_count_norm",
"EXT_SOURCE_2_multi_main_NAME_EDUCATION_TYPE_Higher.education_count_norm",
"EXT_SOURCE_2_minus_per_NAME_PRODUCT_TYPE_walk.in_count_norm",
"EXT_SOURCE_2_plus_DAYS_LAST_PHONE_CHANGE",
"EXT_SOURCE_2_multi_DAYS_LAST_PHONE_CHANGE",
"EXT_SOURCE_3_plus_EXT_SOURCE_1",
"EXT_SOURCE_3_plus_DAYS_BIRTH",
"EXT_SOURCE_3_multi_DAYS_BIRTH",
"EXT_SOURCE_3_minus_per_NAME_CONTRACT_STATUS_Refused_count_norm",
"EXT_SOURCE_3_multi_per_NAME_CONTRACT_STATUS_Refused_count_norm",
"EXT_SOURCE_3_plus_flag_pay_late_mean",
"EXT_SOURCE_3_multi_flag_pay_late_mean",
"EXT_SOURCE_3_plus_flag_pay_on_time_mean",
"EXT_SOURCE_3_multi_flag_pay_on_time_mean",
"EXT_SOURCE_3_minus_per_NAME_CONTRACT_STATUS_Approved_count_norm",
"EXT_SOURCE_3_plus_REGION_RATING_CLIENT_W_CITY",
"EXT_SOURCE_3_plus_flag_pay_less_mean",
"EXT_SOURCE_3_multi_flag_pay_less_mean",
"EXT_SOURCE_3_minus_per_NAME_PRODUCT_TYPE_walk.in_count",
"EXT_SOURCE_3_plus_main_NAME_INCOME_TYPE_Working_count",
"EXT_SOURCE_3_multi_main_NAME_INCOME_TYPE_Working_count",
"EXT_SOURCE_3_plus_main_NAME_INCOME_TYPE_Working_count_norm",
"EXT_SOURCE_3_multi_main_NAME_INCOME_TYPE_Working_count_norm",
"EXT_SOURCE_3_plus_main_NAME_EDUCATION_TYPE_Higher.education_count",
"EXT_SOURCE_3_multi_main_NAME_EDUCATION_TYPE_Higher.education_count",
"EXT_SOURCE_3_plus_main_NAME_EDUCATION_TYPE_Higher.education_count_norm",
"EXT_SOURCE_3_multi_main_NAME_EDUCATION_TYPE_Higher.education_count_norm",
"EXT_SOURCE_3_minus_per_NAME_PRODUCT_TYPE_walk.in_count_norm",
"EXT_SOURCE_3_plus_DAYS_LAST_PHONE_CHANGE",
"EXT_SOURCE_3_multi_DAYS_LAST_PHONE_CHANGE",
"EXT_SOURCE_1_plus_DAYS_BIRTH",
"EXT_SOURCE_1_multi_DAYS_BIRTH",
"EXT_SOURCE_1_minus_per_NAME_CONTRACT_STATUS_Refused_count_norm",
"EXT_SOURCE_1_multi_per_NAME_CONTRACT_STATUS_Refused_count_norm",
"EXT_SOURCE_1_plus_flag_pay_late_mean",
"EXT_SOURCE_1_multi_flag_pay_late_mean",
"EXT_SOURCE_1_plus_flag_pay_on_time_mean",
"EXT_SOURCE_1_multi_flag_pay_on_time_mean",
"EXT_SOURCE_1_minus_per_NAME_CONTRACT_STATUS_Approved_count_norm",
"EXT_SOURCE_1_plus_REGION_RATING_CLIENT_W_CITY",
"EXT_SOURCE_1_plus_flag_pay_less_mean",
"EXT_SOURCE_1_multi_flag_pay_less_mean",
"EXT_SOURCE_1_plus_main_NAME_INCOME_TYPE_Working_count",
"EXT_SOURCE_1_multi_main_NAME_INCOME_TYPE_Working_count",
"EXT_SOURCE_1_plus_main_NAME_INCOME_TYPE_Working_count_norm",
"EXT_SOURCE_1_multi_main_NAME_INCOME_TYPE_Working_count_norm",
"EXT_SOURCE_1_plus_main_NAME_EDUCATION_TYPE_Higher.education_count",
"EXT_SOURCE_1_plus_main_NAME_EDUCATION_TYPE_Higher.education_count_norm",
"EXT_SOURCE_1_multi_main_NAME_EDUCATION_TYPE_Higher.education_count_norm",
"EXT_SOURCE_1_minus_per_NAME_PRODUCT_TYPE_walk.in_count_norm",
"EXT_SOURCE_1_plus_DAYS_LAST_PHONE_CHANGE",
"EXT_SOURCE_1_multi_DAYS_LAST_PHONE_CHANGE",
"DAYS_BIRTH_minus_per_NAME_CONTRACT_STATUS_Refused_count_norm",
"DAYS_BIRTH_plus_flag_pay_late_mean",
"DAYS_BIRTH_plus_flag_pay_on_time_mean",
"DAYS_BIRTH_plus_per_NAME_CONTRACT_STATUS_Refused_count",
"DAYS_BIRTH_minus_per_NAME_CONTRACT_STATUS_Approved_count_norm",
"DAYS_BIRTH_plus_REGION_RATING_CLIENT_W_CITY",
"DAYS_BIRTH_plus_flag_pay_less_mean",
"DAYS_BIRTH_plus_per_NAME_PRODUCT_TYPE_walk.in_count",
"DAYS_BIRTH_plus_main_NAME_INCOME_TYPE_Working_count",
"DAYS_BIRTH_plus_main_NAME_INCOME_TYPE_Working_count_norm",
"DAYS_BIRTH_multi_main_NAME_INCOME_TYPE_Working_count_norm",
"DAYS_BIRTH_plus_main_NAME_EDUCATION_TYPE_Higher.education_count",
"DAYS_BIRTH_plus_main_NAME_EDUCATION_TYPE_Higher.education_count_norm",
"DAYS_BIRTH_plus_DAYS_LAST_PHONE_CHANGE",
"per_NAME_CONTRACT_STATUS_Refused_count_norm_minus_per_CODE_REJECT_REASON_XAP_count_norm",
"per_NAME_CONTRACT_STATUS_Refused_count_norm_minus_flag_pay_late_mean",
"per_NAME_CONTRACT_STATUS_Refused_count_norm_minus_per_NAME_CONTRACT_STATUS_Refused_count",
"per_NAME_CONTRACT_STATUS_Refused_count_norm_plus_REGION_RATING_CLIENT_W_CITY",
"per_NAME_CONTRACT_STATUS_Refused_count_norm_minus_REGION_RATING_CLIENT_W_CITY",
"per_NAME_CONTRACT_STATUS_Refused_count_norm_multi_per_NAME_PRODUCT_TYPE_walk.in_count",
"per_NAME_CONTRACT_STATUS_Refused_count_norm_minus_per_NAME_PRODUCT_TYPE_walk.in_count_norm",
"per_CODE_REJECT_REASON_XAP_count_norm_plus_REGION_RATING_CLIENT_W_CITY",
"per_CODE_REJECT_REASON_XAP_count_norm_minus_REGION_RATING_CLIENT_W_CITY",
"per_CODE_REJECT_REASON_XAP_count_norm_plus_main_NAME_INCOME_TYPE_Working_count_norm",
"per_CODE_REJECT_REASON_XAP_count_norm_plus_per_NAME_PRODUCT_TYPE_walk.in_count_norm",
"flag_pay_late_mean_plus_flag_pay_on_time_mean",
"flag_pay_late_mean_plus_per_NAME_CONTRACT_STATUS_Refused_count",
"flag_pay_late_mean_minus_per_NAME_CONTRACT_STATUS_Approved_count_norm",
"flag_pay_late_mean_plus_REGION_RATING_CLIENT_W_CITY",
"flag_pay_late_mean_plus_flag_pay_less_mean",
"flag_pay_late_mean_plus_main_NAME_INCOME_TYPE_Working_count",
"flag_pay_late_mean_multi_main_NAME_INCOME_TYPE_Working_count",
"flag_pay_late_mean_plus_main_NAME_INCOME_TYPE_Working_count_norm",
"flag_pay_late_mean_multi_main_NAME_INCOME_TYPE_Working_count_norm",
"flag_pay_late_mean_plus_main_NAME_EDUCATION_TYPE_Higher.education_count",
"flag_pay_late_mean_plus_main_NAME_EDUCATION_TYPE_Higher.education_count_norm",
"flag_pay_late_mean_plus_DAYS_LAST_PHONE_CHANGE",
"flag_pay_late_mean_multi_DAYS_LAST_PHONE_CHANGE",
"flag_pay_on_time_mean_minus_per_NAME_CONTRACT_STATUS_Refused_count",
"flag_pay_on_time_mean_multi_per_NAME_CONTRACT_STATUS_Refused_count",
"flag_pay_on_time_mean_minus_per_NAME_CONTRACT_STATUS_Approved_count_norm",
"flag_pay_on_time_mean_plus_REGION_RATING_CLIENT_W_CITY",
"flag_pay_on_time_mean_plus_flag_pay_less_mean",
"flag_pay_on_time_mean_minus_per_NAME_PRODUCT_TYPE_walk.in_count",
"flag_pay_on_time_mean_plus_main_NAME_INCOME_TYPE_Working_count",
"flag_pay_on_time_mean_multi_main_NAME_INCOME_TYPE_Working_count",
"flag_pay_on_time_mean_plus_main_NAME_INCOME_TYPE_Working_count_norm",
"flag_pay_on_time_mean_multi_main_NAME_INCOME_TYPE_Working_count_norm",
"flag_pay_on_time_mean_plus_main_NAME_EDUCATION_TYPE_Higher.education_count",
"flag_pay_on_time_mean_plus_main_NAME_EDUCATION_TYPE_Higher.education_count_norm",
"flag_pay_on_time_mean_minus_per_NAME_PRODUCT_TYPE_walk.in_count_norm",
"flag_pay_on_time_mean_plus_DAYS_LAST_PHONE_CHANGE",
"per_NAME_CONTRACT_STATUS_Refused_count_plus_REGION_RATING_CLIENT_W_CITY",
"per_NAME_CONTRACT_STATUS_Refused_count_minus_REGION_RATING_CLIENT_W_CITY",
"per_NAME_CONTRACT_STATUS_Refused_count_plus_flag_pay_less_mean",
"per_NAME_CONTRACT_STATUS_Refused_count_minus_flag_pay_less_mean",
"per_NAME_CONTRACT_STATUS_Refused_count_plus_per_NAME_PRODUCT_TYPE_walk.in_count",
"per_NAME_CONTRACT_STATUS_Refused_count_multi_per_NAME_PRODUCT_TYPE_walk.in_count",
"per_NAME_CONTRACT_STATUS_Refused_count_plus_main_NAME_INCOME_TYPE_Working_count",
"per_NAME_CONTRACT_STATUS_Refused_count_minus_main_NAME_INCOME_TYPE_Working_count",
"per_NAME_CONTRACT_STATUS_Refused_count_plus_main_NAME_EDUCATION_TYPE_Higher.education_count",
"per_NAME_CONTRACT_STATUS_Refused_count_minus_main_NAME_EDUCATION_TYPE_Higher.education_count",
"per_NAME_CONTRACT_STATUS_Refused_count_plus_main_NAME_EDUCATION_TYPE_Higher.education_count_norm",
"per_NAME_CONTRACT_STATUS_Refused_count_minus_main_NAME_EDUCATION_TYPE_Higher.education_count_norm",
"per_NAME_CONTRACT_STATUS_Refused_count_multi_per_NAME_PRODUCT_TYPE_walk.in_count_norm",
"per_NAME_CONTRACT_STATUS_Refused_count_plus_DAYS_LAST_PHONE_CHANGE",
"per_NAME_CONTRACT_STATUS_Refused_count_minus_DAYS_LAST_PHONE_CHANGE",
"per_NAME_CONTRACT_STATUS_Approved_count_norm_plus_REGION_RATING_CLIENT_W_CITY",
"per_NAME_CONTRACT_STATUS_Approved_count_norm_minus_REGION_RATING_CLIENT_W_CITY",
"REGION_RATING_CLIENT_W_CITY_plus_flag_pay_less_mean",
"REGION_RATING_CLIENT_W_CITY_minus_flag_pay_less_mean",
"REGION_RATING_CLIENT_W_CITY_plus_per_NAME_PRODUCT_TYPE_walk.in_count",
"REGION_RATING_CLIENT_W_CITY_minus_per_NAME_PRODUCT_TYPE_walk.in_count",
"REGION_RATING_CLIENT_W_CITY_multi_per_NAME_PRODUCT_TYPE_walk.in_count",
"REGION_RATING_CLIENT_W_CITY_plus_main_NAME_INCOME_TYPE_Working_count",
"REGION_RATING_CLIENT_W_CITY_minus_main_NAME_INCOME_TYPE_Working_count",
"REGION_RATING_CLIENT_W_CITY_plus_main_NAME_INCOME_TYPE_Working_count_norm",
"REGION_RATING_CLIENT_W_CITY_minus_main_NAME_INCOME_TYPE_Working_count_norm",
"REGION_RATING_CLIENT_W_CITY_multi_main_NAME_INCOME_TYPE_Working_count_norm",
"REGION_RATING_CLIENT_W_CITY_plus_main_NAME_EDUCATION_TYPE_Higher.education_count",
"REGION_RATING_CLIENT_W_CITY_minus_main_NAME_EDUCATION_TYPE_Higher.education_count",
"REGION_RATING_CLIENT_W_CITY_plus_main_NAME_EDUCATION_TYPE_Higher.education_count_norm",
"REGION_RATING_CLIENT_W_CITY_minus_main_NAME_EDUCATION_TYPE_Higher.education_count_norm",
"REGION_RATING_CLIENT_W_CITY_plus_per_NAME_PRODUCT_TYPE_walk.in_count_norm",
"REGION_RATING_CLIENT_W_CITY_multi_per_NAME_PRODUCT_TYPE_walk.in_count_norm",
"REGION_RATING_CLIENT_W_CITY_plus_DAYS_LAST_PHONE_CHANGE",
"REGION_RATING_CLIENT_W_CITY_minus_DAYS_LAST_PHONE_CHANGE",
"flag_pay_less_mean_plus_per_NAME_PRODUCT_TYPE_walk.in_count",
"flag_pay_less_mean_plus_main_NAME_INCOME_TYPE_Working_count",
"flag_pay_less_mean_plus_main_NAME_INCOME_TYPE_Working_count_norm",
"flag_pay_less_mean_multi_main_NAME_INCOME_TYPE_Working_count_norm",
"flag_pay_less_mean_plus_main_NAME_EDUCATION_TYPE_Higher.education_count",
"flag_pay_less_mean_plus_main_NAME_EDUCATION_TYPE_Higher.education_count_norm",
"flag_pay_less_mean_plus_DAYS_LAST_PHONE_CHANGE",
"per_NAME_PRODUCT_TYPE_walk.in_count_minus_main_NAME_INCOME_TYPE_Working_count",
"per_NAME_PRODUCT_TYPE_walk.in_count_plus_main_NAME_INCOME_TYPE_Working_count_norm",
"per_NAME_PRODUCT_TYPE_walk.in_count_multi_main_NAME_INCOME_TYPE_Working_count_norm",
"per_NAME_PRODUCT_TYPE_walk.in_count_plus_main_NAME_EDUCATION_TYPE_Higher.education_count",
"per_NAME_PRODUCT_TYPE_walk.in_count_minus_main_NAME_EDUCATION_TYPE_Higher.education_count",
"per_NAME_PRODUCT_TYPE_walk.in_count_plus_main_NAME_EDUCATION_TYPE_Higher.education_count_norm",
"per_NAME_PRODUCT_TYPE_walk.in_count_minus_main_NAME_EDUCATION_TYPE_Higher.education_count_norm",
"per_NAME_PRODUCT_TYPE_walk.in_count_multi_per_NAME_PRODUCT_TYPE_walk.in_count_norm",
"per_NAME_PRODUCT_TYPE_walk.in_count_plus_DAYS_LAST_PHONE_CHANGE",
"per_NAME_PRODUCT_TYPE_walk.in_count_minus_DAYS_LAST_PHONE_CHANGE",
"main_NAME_INCOME_TYPE_Working_count_plus_main_NAME_INCOME_TYPE_Working_count_norm",
"main_NAME_INCOME_TYPE_Working_count_plus_main_NAME_EDUCATION_TYPE_Higher.education_count",
"main_NAME_INCOME_TYPE_Working_count_plus_main_NAME_EDUCATION_TYPE_Higher.education_count_norm",
"main_NAME_INCOME_TYPE_Working_count_multi_main_NAME_EDUCATION_TYPE_Higher.education_count_norm",
"main_NAME_INCOME_TYPE_Working_count_plus_DAYS_LAST_PHONE_CHANGE",
"main_NAME_INCOME_TYPE_Working_count_norm_plus_main_NAME_EDUCATION_TYPE_Higher.education_count",
"main_NAME_INCOME_TYPE_Working_count_norm_multi_main_NAME_EDUCATION_TYPE_Higher.education_count",
"main_NAME_INCOME_TYPE_Working_count_norm_plus_main_NAME_EDUCATION_TYPE_Higher.education_count_norm",
"main_NAME_INCOME_TYPE_Working_count_norm_multi_main_NAME_EDUCATION_TYPE_Higher.education_count_norm",
"main_NAME_INCOME_TYPE_Working_count_norm_plus_DAYS_LAST_PHONE_CHANGE",
"main_NAME_INCOME_TYPE_Working_count_norm_multi_DAYS_LAST_PHONE_CHANGE",
"main_NAME_EDUCATION_TYPE_Higher.education_count_plus_main_NAME_EDUCATION_TYPE_Higher.education_count_norm",
"main_NAME_EDUCATION_TYPE_Higher.education_count_minus_per_NAME_PRODUCT_TYPE_walk.in_count_norm",
"main_NAME_EDUCATION_TYPE_Higher.education_count_plus_DAYS_LAST_PHONE_CHANGE",
"main_NAME_EDUCATION_TYPE_Higher.education_count_norm_plus_DAYS_LAST_PHONE_CHANGE")

trn <- trn[var_list]
oot <- oot[var_list]
mrv <- mrv[var_list]

write.csv(trn,"d:\\760.project\\main_cor_trn.csv",row.names=FALSE)
write.csv(oot,"d:\\760.project\\main_cor_oot.csv",row.names=FALSE)
write.csv(mrv,"d:\\760.project\\main_cor_mrv.csv",row.names=FALSE)

trn <- read.csv("d:\\760.project\\main_cor_trn.csv")
oot <- read.csv("d:\\760.project\\main_cor_oot.csv")
mrv <- read.csv("d:\\760.project\\main_cor_mrv.csv")

trn <- trn[-1]
oot <- oot[-1]
mrv <- mrv[-1]

list1 <- array(0,c(length(trn),7))
for (i in 2:length(trn)) {
  if (length(unique(trn[, i]))>3) {
    dt1 <- trn[order(trn[,i]),i]
    dt1 <- dt1[which(!is.na(dt1))]
    len1 <- length(dt1)
    list1[i,1] <- cor(dt1,c(1:len1))
    list1[i,2] <- cor(log(dt1+1),c(1:len1))
    list1[i,3] <- cor(sqrt(dt1),c(1:len1))
    list1[i,4] <- cor((dt1)^2,c(1:len1))
    list1[i,5] <- cor(1/(dt1+1),c(1:len1))
    list1[i,6] <- max(abs(list1[i,1:3]),na.rm=TRUE)
    list1[i,7] <- min(which(abs(list1[i,1:3])==list1[i,6]),na.rm=TRUE)
  }}

for (i in 2:length(trn)) {
if (list1[i,7]==2 & list1[i,1]<=.8){
  new_name <- paste(names(trn[i]),'_t_log',sep='')
  trn[!is.na(trn[,i]),i] <- log(trn[!is.na(trn[,i]),i]+1)
  oot[!is.na(oot[,i]),i] <- log(oot[!is.na(oot[,i]),i]+1)
  mrv[!is.na(mrv[,i]),i] <- log(mrv[!is.na(mrv[,i]),i]+1)
  names(trn)[i]<-new_name;names(oot)[i]<-new_name;names(mrv)[i]<-new_name}
if (list1[i,7]==3 & list1[i,1]<=.8){
  new_name <- paste(names(trn[i]),'_t_sqrt',sep='')
  trn[!is.na(trn[,i]),i] <- sqrt(trn[!is.na(trn[,i]),i])
  oot[!is.na(oot[,i]),i] <- sqrt(oot[!is.na(oot[,i]),i])
  mrv[!is.na(mrv[,i]),i] <- sqrt(mrv[!is.na(mrv[,i]),i])
  names(trn)[i]<-new_name;names(oot)[i]<-new_name;names(mrv)[i]<-new_name}
if (list1[i,7]==4 & list1[i,1]<=.8){
  new_name <- paste(names(trn[i]),'_t_square',sep='')
  trn[!is.na(trn[,i]),i] <- (trn[!is.na(trn[,i]),i])^2
  oot[!is.na(oot[,i]),i] <- (oot[!is.na(oot[,i]),i])^2
  mrv[!is.na(mrv[,i]),i] <- (mrv[!is.na(mrv[,i]),i])^2
  names(trn)[i]<-new_name;names(oot)[i]<-new_name;names(mrv)[i]<-new_name}
if (list1[i,7]==5 & list1[i,1]<=.8){
  new_name <- paste(names(trn[i]),'_t_reciprocal',sep='')
  trn[!is.na(trn[,i]),i] <- 1/(trn[!is.na(trn[,i]),i]+1)
  oot[!is.na(oot[,i]),i] <- 1/(oot[!is.na(oot[,i]),i]+1)
  mrv[!is.na(mrv[,i]),i] <- 1/(mrv[!is.na(mrv[,i]),i]+1)
  names(trn)[i]<-new_name;names(oot)[i]<-new_name;names(mrv)[i]<-new_name}}
  
# drop variables with less than 0.8 cor
list2 <- which(list1[,6]<0.9&list1[,6]!=0)
trn <- trn[, -list2]
oot <- oot[, -list2]
mrv <- mrv[, -list2]

F_info <- data_info(trn,oot,mrv,target="TARGET",plot_bins=5,cal_iv=FALSE)
# replace NA
for (i in 2:length(trn))
  if (length(which(is.na(trn[, i])))>0 | length(which(is.na(oot[, i])))>0 | length(which(is.na(mrv[, i])))>0) {
	trn[which(is.na(trn[, i])), i] <- round(mean(trn[, i], na.rm=TRUE),1)
	oot[which(is.na(oot[, i])), i] <- round(mean(trn[, i], na.rm=TRUE),1)
	mrv[which(is.na(mrv[, i])), i] <- round(mean(trn[, i], na.rm=TRUE),1)}

# tr: train set
# oot: oot set
# mrv: mrv set
# bad_flg: character, name of target feature
# F_info: create by function "data_info", contain the order flag for Logitplot
# nround: number of rounds
# cv_num: number cross-validation folds, default 3
# seed_value: random seed, default randomly
# nonlist: a vector that include which features should not be added into the model, default NULL
# weight_value: weights for bad target, default 1
# cl_num: number of clusters, default 1
# auc_diff: maximum difference between AUCs allowed, default 0.03
# ks_diff: maximum difference between KSs allowed, default 0.03
# psi_diff: maximum times between PSIs allowed, default Inf
# max_pv=: maximum p value allowed, default 0.05
# max_cor: maximum correlation allowed, default 0.3
# ks_bins: number of bins for KS, default 5
# psi_bins: number of bins for PSI, default 5
# ks_startnmu: the number of rounds that starts to consider the difference between PSIs, default 5
# plot_order: should the lplot and its estimate order? default TRUE
list_f <- lr_stepwise(trn, oot, mrv, bad_flg="TARGET", F_info, nround=15, cv_num=10, weight_value=11, cl_num=8) 

w1 <- rep(1,length(trn[,1]));w1[trn[,1]==1] <- 11
lr1 <- glm(TARGET~., data=trn[c(1,list_f$Feature_Number)], weights=w1, family=binomial())
ks(lr1,trn,bad_flg="TARGET",5);ks(lr1,oot,bad_flg="TARGET",5)
trn_p <- predict(lr1, trn, type='response');oot_p <- predict(lr1, oot, type='response');mrv_p <- predict(lr1, mrv, type='response')
psi(trn_p,oot_p,5);psi(trn_p,mrv_p,5)

for(i in 1:20){
  lr1 <- glm(initial_M1Plus_in_1m_flg~., data=trn[c(1,list_f$Feature_Number[c(1:i)])], weights=w1, family=binomial())
  print(c(ks(lr1,trn,bad_flg="initial_M1Plus_in_1m_flg",5)[[2]],ks(lr1,oot,bad_flg="initial_M1Plus_in_1m_flg",5)[[2]]))}

write.csv(summary(lr1)$coef,"C:\\Users\\Administrator\\Desktop\\12.21\\summary.csv",row.names=TRUE)
write.csv(list_f,"d:\\760.project\\list_f_lr.csv",row.names=FALSE)

list_f <- read.csv("d:\\760.project\\list_f_lr.csv")
trn$new_score <- predict(lr1, trn, type='response');oot$new_score <- predict(lr1, oot, type='response');mrv$new_score <- predict(lr1, mrv, type='response')
write.csv(oot,"C:\\Users\\Administrator\\Desktop\\12.21\\oot_score.csv",row.names=FALSE)

test <- read.csv("C:\\Users\\Administrator\\Desktop\\12.21\\1201_1220_apply.csv")
for (i in 1:length(test))
  if (length(which(is.na(test[, i])))>0)
	test[which(is.na(test[, i])), i] <- round(F_info$F_mean[which(F_info$F_name==names(test[i]))],1)
test$new_score <- predict(lr1, test, type='response')
write.csv(test,"C:\\Users\\Administrator\\Desktop\\12.21\\test.csv",row.names=FALSE)

for (i in 1:length(data_all2)){
  if (length(unique(data_all2[,i]))>1) {
    figure=paste("C:\\Users\\Administrator\\Desktop\\1.18\\",names(data_all2[i]),".jpg",sep="")
    jpeg(file=figure)
    plot(data_all2[,i],main=names(data_all2[i]))
    dev.off()}
}

plot(data_all2[,3])
data_all2[which(data_all2[,3]==outlier(data_all2[,3])),3]<-NA



list_f <- lr_stepwise(trn, oot, mrv, bad_flg="dpd30p_4m", F_info, nround=20, cv_num=10, weight_value=4, cl_num=4, auc_diff=.04, ks_diff=.04, max_cor=.4, seed_value=70114399)


for (i in c(8:27)) {
  data1 <- data_all[which(data_all$request_month>=201703 & data_all$request_month<=201711),]
  data2 <- data_all[which(data_all$request_month>=201703 & data_all$request_month<=201712),]
  pred1 <- prediction(data1[i], data1$dpd30p_3m)
  pred2 <- prediction(data2[i], data2$dpd30p_1m)
  perf1 <- performance(pred1, "tpr", "fpr");perf2 <- performance(pred2, "tpr", "fpr")
  auc1 <- performance(pred1, "auc")@'y.values'[[1]];auc2 <- performance(pred2, "auc")@'y.values'[[1]]
  if(auc1<0.5){auc1=1-auc1};if(auc2<0.5){auc2=1-auc2}
  print(c(auc1,auc2))
}

var_list <- c("is_fpd30p","f_1","f_2","f_3","f_4","f_6","f_7","f_11","f_13","f_18","f_21","var_525","var_528","var_530","var_533","var_534","var_537","var_538","var_539","var_540","var_554","var_555","var_556","var_557","var_558","var_561","var_562","var_563","var_565","var_568","var_571","var_572","var_574","var_575","var_576","var_580","var_583","var_585","var_587","var_596","var_597","var_599","var_602","var_605","var_609","var_610","var_611","var_613","var_619","var_620","var_621","var_624","var_625","var_626","var_627","var_628","var_639","var_640","var_644","var_646","var_650","var_657","var_659","var_660","var_667","var_668","var_670","var_671","var_677","var_678","var_680","var_681","var_688","var_689","var_691","var_693","var_959","numnighttime","smallbalancedays","numdaytime","overl8m8amt","overl5m5amt","numdaytime1113","numdaytime1112","l1amt_workdayweekendratio","maxminusmin","new_times","vix","avgfundvol1year","m1_sumamt","avgfundvol6m","l1_sumamt","all_1_sumamt","all_1_workdayamt","m1_workdayamt","m1_avgnumamt","all_1_weekdayamt","all_1_amt10","all_1_num10","all_1_amt12","all_1_amtlevelamt5","all_1_amtlevelnum5","m1_avgworkdaynumamt","all_1_num12","all_1_amt11","all_1_avgweekdaynumamt","l1_num12","l1_weekdayamt","l1_amt10","l1_avgworkdaynumamt","l1_amt12","m8_sumamt","all_1_amt09","all_1_avgnumamt10","l1_num10","all_1_amtlevelnum6","all_1_amtlevelnum4","all_8_sumamt","all_1_amtlevelamt4","l8_sumamt","all_1_amtlevelamt6","all_1_amt13","l1_amtlevelnum5","all_1_workdaynum","all_1_sumnum","l1_amtlevelamt4","all_1_num11","l1_amtlevelnum4","l1_amtlevelamt5","all_1_amtlevelnum7","all_1_avgnumamt09","all_1_amtlevelamt7","m1_weekdayamt","l1_sumnum","l1_amt11","m1_workdaynum","all_1_amt14","l1_avgnumamt12","m1_amtlevelnum7","l1_workdaynum","l1_avgnumamt10","m1_amtlevelamt7","m1_sumnum","m8_avgnumamt","l1_amt09","all_1_avgweekdaydayamt","all_1_weekdaynum","m1_avgweekdaynumamt","l1_num11","all_1_avgworkdaydayamt","l1_amtlevelnum7","purchase_last_6_month_cnt","purchase_last_3_month_cnt","pur_last_6_month_amt","pur_3m_workday_ratio","pur_6m_workday_ratio","pur_12m_9_12_ratio","pur_12m_12_14_ratio","pur_12m_23_2_ratio","loan_last_6_month_cnt","loan_last_12_month_amt","loan_w_1m_ratio","loan_w_12m_ratio","bal_1m_avg_amt","bal_3m_avg_amt","bal_6m_avg_amt","bal_12m_avg_amt","bal_12m_avg_std_amt","bal_avg_amt","bal_lastest_amt","bal_max_amt","first_opentime","yu_e_all_rj","yu_e360d_rj","yu_e180d_rj","yu_e_all_sd","yu_e_all_maxyue","sg_all_10_17h_amount","sg_jy_all_amount","sgxftx_all_1_7h_num_ratio","xf_all_0_6h_num_ratio","sg_bj_all_amount","xf_all_10_14h_num","sg_all_weekend_amount","tx_jy_all_amount","xf_jy_90d_amount","sg_all_10_14h_num","tx_all_9_16h_amount","yuebao_acct_num","device_price","app_stability_180d","top_90d","top_180d","tail_90d","finance_90d","finance_7d","entertainment_90d","entertainment_180d","loan_90d","loan_180d","ym9002_02","ym9002_04","ym9002_08","ym9002_12","ym9002_13","ym9002_16","ym9002_19","ym9002_20","ym9002_21","ym9002_22","ym9002_23","ym9002_29","ym9002_30","ym9002_37","ym9002_39","ym9002_40","ym9002_42","ym9002_44","ym9002_47","ym9002_55","ym9002_56")

list_f <- lr_stepwise(trn, oot, mrv, bad_flg="dpd30p_3m", F_info, nround=20, cv_num=10, weight_value=5, cl_num=4) 
w1 <- rep(1,length(trn[,1]));w1[trn[,1]==1] <- 5
lr1 <- glm(dpd30p_3m~., data=trn[c(1,list_f$Feature_Number[c(1:13)])], weights=w1, family=binomial())
ks(lr1,trn,bad_flg="dpd30p_3m",5);ks(lr1,oot,bad_flg="dpd30p_3m",5)
trn_p <- predict(lr1, trn, type='response');oot_p <- predict(lr1, oot, type='response');mrv_p <- predict(lr1, mrv, type='response')
psi(trn_p,oot_p,5);psi(trn_p,mrv_p,5)
pred1 <- prediction(oot_p, oot$dpd30p_3m)
perf1 <- performance(pred1, "tpr", "fpr")
auc1 <- performance(pred1, "auc")@'y.values'[[1]]

list_f <- lr_stepwise(trn, oot, mrv, bad_flg="dpd30p_3m", F_info, nround=20, cv_num=195, weight_value=5, cl_num=4, auc_diff=.04, ks_diff=.04, max_cor=.4, seed_value=84480745, nonlist=c(2:10))
lr2 <- glm(dpd30p_3m~., data=trn[c(1,list_f$Feature_Number[c(1:13)])], weights=w1, family=binomial())
ks(lr2,trn,bad_flg="dpd30p_3m",5);ks(lr2,oot,bad_flg="dpd30p_3m",5)
oot_p <- predict(lr2, oot, type='response')
pred2 <- prediction(oot_p, oot$dpd30p_3m)
perf2 <- performance(pred2, "tpr", "fpr")
auc2 <- performance(pred2, "auc")@'y.values'[[1]]

plot(perf1,main='ROC Logistic Regression')
plot(perf2,col='red',lty=2,add=TRUE)
legend(0.6, 0.6, c('with HangLv', 'without HangLv'), col=c('black', 'red'), lwd=2)