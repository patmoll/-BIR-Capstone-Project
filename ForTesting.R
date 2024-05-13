library("readxl")
library("data.table")
library("dplyr")
library("tidyr")
library("e1071") # for svm
library("isotree") # for isolation forest
library("nnet") # for multinomial
library("randomForest") # for random forest

###############################
# Helper functions
###############################

filter_regdata_by_industry <- function(reg_data, industry) {
  reg_data = reg_data%>%distinct(TIN, .keep_all = TRUE)
  reg_data = filter(reg_data, INDUSTRY_CLASSIFICATION_DESC == industry)
  return(reg_data)
}

filter_slsp_by_tins <- function(slsp, tin_df) {
  slsp_filtered =filter(slsp, owner_tin %in% unlist(c(tin_df)))
  return(slsp_filtered)
}

get_sales_cols <- function(old_sls) {
  new = subset(old_sls, select = c(owner_tin, tax_year, qtr, sls_taxable_sales))
  return(new)
}

get_purch_cols <- function(old_slp) {
  new = subset(old_slp, select = c(owner_tin, tax_year, qtr, gross_taxable_purchases))
  return(new)
}

aggregate_industry_sales <- function(sales_data) {
  sales = sales_data %>% group_by(owner_tin , tax_year, qtr) %>% summarise(total_Sales = sum(sls_taxable_sales) , .groups = 'drop')
  sales = sales %>% unite(col = "dummy", c("owner_tin","tax_year", "qtr"), sep = "-", remove=FALSE)
  return(sales)
}

aggregate_industry_purchases <- function(purchase_data) {
  purch = purchase_data %>% group_by(owner_tin , tax_year, qtr) %>% summarise(total_purch = sum(gross_taxable_purchases
  ) , .groups = 'drop')
  purch = purch %>% unite(col = "dummy", c("owner_tin","tax_year", "qtr"), sep = "-", remove=FALSE)
  return(purch)
}

keep_latest <- function(vat) {
  #vat = filter(vat , AMENDED_YN == "Y")
  vat = vat %>% unite(col = "dummy", c("owner_tin","tax_year", "qtr"), sep = "-", remove=FALSE)
  vat = vat %>% arrange(desc(DATE_FILED)) 
  vat = vat %>% distinct(dummy, .keep_all = TRUE)
  return(vat)
}

get_vat_cols <- function(old_vat) {
  new = old_vat["DATE_FILED"]
  new["owner_tin"] = old_vat["TIN"]
  new["tax_year"]= old_vat["YEAR"]
  new["qtr"]= old_vat["QTR"]
  new["NET_PAYABLE"]= old_vat["NET_PAYBLE"]
  new["AMENDED_YN"] = old_vat["AMENDED_YN"]
  
  return(new)
}

get_ratio_sales_to_purchases <- function(agg_sales, agg_purch) {
  merged_df = inner_join(agg_sales, agg_purch, by = c("owner_tin","tax_year","qtr"))
  merged_df["Sales_to_Purch"] = merged_df["total_Sales"] / merged_df["total_purch"]
  merged_df["Purch_to_Sales"] = 1 / merged_df["Sales_to_Purch"]
  ind_med_Sales_to_Purch <- median(as.numeric(merged_df$Sales_to_Purch) , na.rm =T)
  ind_med_Purch_to_Sales <- median(as.numeric(merged_df$Purch_to_Sales) , na.rm =T)
  return(c(ind_med_Sales_to_Purch, ind_med_Purch_to_Sales))
}

get_ratio_vat_to_sales <- function(agg_sales , all_vat) {
  merged_df = inner_join(agg_sales, all_vat, by = c("owner_tin","tax_year","qtr"))
  merged_df["VAT_to_Sales"] = merged_df["NET_PAYABLE"] / merged_df["total_Sales"]
  merged_df["Sales_to_VAT"] = merged_df["VAT_to_Sales"]
  ind_med_vat_to_sales <- median(as.numeric(merged_df$VAT_to_Sales), na.rm =T)
  ind_med_sales_to_vat <- median(as.numeric(merged_df$Sales_to_VAT), na.rm =T)
  return(c(ind_med_vat_to_sales,ind_med_sales_to_vat))
}

get_ratio_vat_to_purch <- function(agg_purch , all_vat) {
  merged_df = inner_join(agg_purch, all_vat, by = c("owner_tin","tax_year","qtr"))
  merged_df["VAT_to_Purch"] = merged_df["NET_PAYABLE"] / merged_df["total_purch"]
  merged_df["Purch_to_VAT"] = 1 / merged_df["VAT_to_Purch"]  
  ind_med_vat_to_purch <- median(as.numeric(merged_df$VAT_to_Purch), na.rm =T)
  ind_med_purch_to_vat <- median(as.numeric(merged_df$Purch_to_VAT), na.rm =T)
  
  return(c(ind_med_vat_to_purch,ind_med_purch_to_vat))
}

#Function for geeting the complete list of tins with years and qtrs 

year_qtr_df <- function(start_year, end_year, start_qtr, end_qtr) {
  year = start_year
  qtr = start_qtr 
  dat = data_frame()
  for (x in 1:((end_year-start_year+1)*4 + (1-4) - start_qtr + end_qtr)){
    dat[x,"tax_year"]= as.character(year)
    dat[x,"qtr"] = paste("Qtr", as.character(qtr))
    qtr = qtr + 1
    if (qtr %% 4 == 1){
      year = year + 1 
      qtr = 1
    }
  }
  return(dat)
}

add_year_qtr <- function(reg_tin , time_table) {
  
  time_table["TIN"] = as.character(reg_tin[1,"TIN"])
  df = time_table
  
  for (x in 2:nrow(reg_tin)){
    y = as.character(reg_tin[x,"TIN"])
    time_table["TIN"] = y
    df = rbind(df, time_table)
  }
  return( df )
}

cross_checking <- function(cross_check_df , agg_sales , agg_purch , all_vat) {
  for (x in 1:nrow(cross_check_df)){
    y = as.character(cross_check_df[x,"dummy"])
    
    #SLSP
    if ( y %in% agg_sales$dummy){
      cross_check_df[x,"Sales"] = TRUE
    } else {cross_check_df[x,"Sales"] = FALSE}
    if ( y %in% agg_purch$dummy){
      cross_check_df[x,"Purch"] = TRUE
    } else {cross_check_df[x,"Purch"] = FALSE}
    
    #Vats
    if (y %in% all_vat$dummy){
      cross_check_df[x,"VAT"] = TRUE
    } else {cross_check_df[x,"VAT"] = FALSE}
    
  }
  cross_check_df["count_true"] = cross_check_df["Sales"] + cross_check_df["Purch"] + cross_check_df["VAT"]
  return(cross_check_df)
}

add_with_values <- function(validity, agg_sales , agg_purch, all_vat , ind_sales, ind_purch, ind_vat) {
  for (x in 1:nrow(validity)){
    y = as.character(validity[x,"dummy"])
    
    if (validity[x,"count_true"] == 1) {
      
      if (validity[x, "Sales"] == TRUE){
        z = filter(agg_sales, dummy == y)
        validity[x,"total_Sales"] = z["total_Sales"]
        med_purch = get_ratio_sales_to_purchases(ind_sales , ind_purch)
        validity[x,"total_purch"] = validity[x,"total_Sales"] * med_purch[2]
        med_vat = get_ratio_vat_to_sales(ind_sales, ind_vat)
        validity[x,"NET_PAYABLE"] = validity[x,"total_Sales"] * med_vat[2]
      }
      
      else if (validity[x, "Purch"] == TRUE){
        z = filter(agg_purch, dummy == y)
        validity[x,"total_purch"] = z["total_purch"]
        med_sales = get_ratio_sales_to_purchases(ind_sales , ind_purch)
        validity[x,"total_Sales"] = validity[x,"total_purch"] * med_sales[1]
        med_vat = get_ratio_vat_to_purch(ind_purch, ind_vat)
        validity[x,"NET_PAYABLE"] = validity[x,"total_purch"] * med_vat[1]
      }
      
      else if (validity[x, "VAT"] == TRUE){
        z = filter(all_vat, dummy == y)
        validity[x,"NET_PAYABLE"] = z["NET_PAYABLE"]
        med_sales = get_ratio_vat_to_sales(ind_sales , ind_vat)
        validity[x,"total_Sales"] = validity[x,"NET_PAYABLE"] * med_sales[2]
        med_purch = get_ratio_vat_to_purch(ind_purch, ind_vat)
        validity[x,"total_purch"] = validity[x,"NET_PAYABLE"] * med_purch[2]
      }
      
    }
    
    else if (validity[x,"count_true"] == 2) {
      
      if (validity[x, "Sales"] == FALSE){
        v = filter(all_vat, dummy == y)
        p = filter(agg_purch, dummy == y)
        validity[x,"NET_PAYABLE"] = v["NET_PAYABLE"]
        validity[x,"total_purch"] = p["total_purch"]
        validity[x,"total_Sales"] = validity[x,"NET_PAYABLE"] / 0.12 + validity[x,"total_purch"]
      }
      
      else if (validity[x, "Purch"] == FALSE){
        v = filter(all_vat, dummy == y)
        s = filter(agg_sales, dummy == y)
        validity[x,"NET_PAYABLE"] = v["NET_PAYABLE"]
        validity[x,"total_Sales"] = s["total_Sales"]
        validity[x,"total_purch"] = validity[x,"total_Sales"] - validity[x,"NET_PAYABLE"] / 0.12
      }
      
      else if (validity[x, "VAT"] == FALSE){
        p = filter(agg_purch, dummy == y)
        s = filter(agg_sales, dummy == y)
        validity[x,"total_purch"] = p["total_purch"]
        validity[x,"total_Sales"] = s["total_Sales"]
        validity[x,"NET_PAYABLE"] = (validity[x,"total_Sales"] - validity[x,"total_purch"] ) * 0.12
      }
      
    }
    
    else if (validity[x,"count_true"] == 3) {
      v = filter(all_vat, dummy == y)
      p = filter(agg_purch, dummy == y)
      s = filter(agg_sales, dummy == y)
      validity[x,"NET_PAYABLE"] = v["NET_PAYABLE"]
      validity[x,"total_purch"] = p["total_purch"]
      validity[x,"total_Sales"] = s["total_Sales"]
    }
    
  }
  return(validity)
}

update_with_latest <- function(valid, distinct_tins) {
  
  for (x in distinct_tins) {
    x.data = filter(valid, TIN == x)
    x.data = x.data %>% unite(col = "dummy_2", c("tax_year", "qtr"), sep = "-", remove=FALSE)
    x.data = x.data %>% arrange(desc(dummy_2)) 
    x.dummy = as.character(x.data[1,"dummy"])
    row = which(valid$dummy==x.dummy)
    x.data[1,"tax_year"] = as.character(taxyear)
    x.data[1,"qtr"] = taxqtr
    valid[row,] = x.data[1,c(1,3:13)] 
    #x.data %>% distinct(TIN, .keep_all = TRUE)
  }
  return(valid)
}

######################################################################################

######################
# Z-Score Functions
######################

zscore_sales <- function(agg_sales, taxyear, taxqtr, ind_med_sales, ind_mad_sales) {
  
  ###Getting the zscore of sales using entire industry history.
  
  #getting each qtr's absolute difference from the median
  curr_sales = filter(agg_sales, tax_year == taxyear & qtr == taxqtr)
  #z-score of the current year's qtr in relation to entire industry history
  curr_sales["sales_ind_zscore"] = (curr_sales$total_Sales - ind_med_sales)/ind_mad_sales
  
  ###getting the z-scores of sales in relation to personal company's history
  
  #getting a company's personal median based on its own personal history
  per_med_sales = agg_sales %>% group_by(TIN) %>% summarise(med_Sales = median(total_Sales) , .groups = 'drop')
  #adding the median to a sales table
  merged_sales = merge(agg_sales, per_med_sales, by = "TIN")
  #getting each qtr's absolute difference from the median
  per_diff_sales = abs(merged_sales$total_Sales - merged_sales$med_Sales)
  per_mad_sales = median(per_diff_sales)
  per_curr_sales = filter(merged_sales, tax_year == taxyear & qtr == taxqtr)
  
  #z-score of the current year's qtr in relation to entire industry history
  curr_sales["sales_per_zscore"] = (per_curr_sales$total_Sales - per_curr_sales$med_Sales)/per_mad_sales
  
  return(curr_sales)
}

zscore_purchases <- function(agg_purch, taxyear, taxqtr, ind_med_purch, ind_mad_purch) {
  
  #getting each qtr's absolute difference from the median
  curr_purch = filter(agg_purch, tax_year == taxyear & qtr == taxqtr)
  #z-score of the current year's qtr in relation to entire industry history
  curr_purch["purch_ind_zscore"] = (curr_purch$total_purch - ind_med_purch)/ind_mad_purch
  
  ###getting the z-scores of purchases in relation to personal company's history 
  
  #getting a company's personal median based on its own personal history
  per_med_purch = agg_purch %>% group_by(TIN) %>% summarise(med_purch = median(total_purch) , .groups = 'drop')
  #adding the median to a purch table
  merged_purch = merge(agg_purch, per_med_purch, by = "TIN")
  #getting each qtr's absolute difference from the median
  per_diff_purch = abs(merged_purch$total_purch - merged_purch$med_purch)
  per_mad_purch = median(per_diff_purch)
  per_curr_purch = filter(merged_purch, tax_year == taxyear & qtr == taxqtr)
  
  #z-score of the current year's qtr in relation to entire industry history
  curr_purch["purch_per_zscore"] = (per_curr_purch$total_purch - per_curr_purch$med_purch)/per_mad_purch
  
  return(curr_purch)
}

zscore_vat <- function(vat, taxyear, taxqtr, ind_med_vat, ind_mad_vat) {
  
  #getting only the qtr # since vat qtr is formatted different from SLSP qtr
  #taxqtr <- gsub('Qtr ' , '' , taxqtr)
  
  ###Getting the zscore of vat using entire industry history.
  
  #getting each qtr's absolute difference from the median
  curr_vat = filter(vat, tax_year == taxyear & qtr == taxqtr)
  #z-score of the current year's qtr in relation to entire industry history
  curr_vat["vat_ind_zscore"] = (curr_vat$NET_PAYABLE - ind_med_vat)/ind_mad_vat
  
  ###getting the z-scores of sales in relation to personal company's history
  
  #getting a company's personal median based on its own personal history
  per_med_vat = vat %>% group_by(TIN) %>% summarise(med_vat = median(NET_PAYABLE) , .groups = 'drop')
  #adding the median to a sales table
  merged_vat = merge(vat, per_med_vat, by = "TIN")
  #getting each qtr's absolute difference from the median
  per_diff_vat = abs(merged_vat$NET_PAYABLE - merged_vat$med_vat)
  per_mad_vat = median(per_diff_vat)
  per_curr_vat = filter(merged_vat, tax_year == taxyear & qtr == taxqtr)
  
  #z-score of the current year's qtr in relation to entire industry history
  curr_vat["vat_per_zscore"] = (per_curr_vat$NET_PAYABLE - per_curr_vat$med_vat)/per_mad_vat
  
  return(curr_vat)
}


###########################

create_indept_vars <- function(valid.full, sls.full, slp.full , valid.ind.med) {
  # This function no longer creates the Benford's Analysis result variables.
  ind_med_sales = median(as.numeric(valid.ind.med$total_Sales))
  ind_diff_sales = abs(valid.ind.med$total_Sales - ind_med_sales)
  ind_mad_sales = median(ind_diff_sales)
  z_sales = zscore_sales(valid.full,taxyear, taxqtr, ind_med_sales, ind_mad_sales)
  
  ind_med_purch = median(as.numeric(valid.ind.med$total_purch))
  ind_diff_purch = abs(valid.ind.med$total_purch - ind_med_purch)
  ind_mad_purch = median(ind_diff_purch)
  z_purch = zscore_purchases(valid.full, taxyear, taxqtr, ind_med_purch, ind_mad_purch)
  
  z_sales = z_sales[,c(4:8,12:14)]
  z_purch = z_purch[,c(4:8,12:14)]
  
  ind_med_vat = median(as.numeric(valid.ind.med$NET_PAYABLE))
  ind_diff_vat = abs(valid.ind.med$NET_PAYABLE - ind_med_vat)
  ind_mad_vat = median(ind_diff_vat)
  z_vat = zscore_vat(valid.full, taxyear, taxqtr, ind_med_vat, ind_mad_vat)
  
  z_vat = z_vat[,c(4:8,12:14)]
  
  main.full = z_sales %>% inner_join ( z_purch , by = c("TIN", "Sales", "Purch" , "VAT" , "count_true" , "Label"))
  
  main.full = main.full %>% inner_join ( z_vat , by = c("TIN", "Sales", "Purch" , "VAT" , "count_true" , "Label"))

  return(main.full)
}

#############################
#Data processing compilation
#############################
data_processing_for_training_set <- function (reg.f, sls.f, slp.f, ebir.f, efps.f,
                             reg.2, sls.2, slp.2, ebir.2, efps.2) {
  
  sls.full = rbind(sls.f,sls.2)
  slp.full = rbind(slp.f,slp.2)
  
  sales.f = aggregate_industry_sales(sls.f)
  purch.f = aggregate_industry_purchases(slp.f)
  
  sales.2 = aggregate_industry_sales(sls.2)
  purch.2 = aggregate_industry_purchases(slp.2)
  
  ebir.f$qtr <- sub("^","Qtr ", ebir.f$qtr)  
  efps.f$qtr <- sub("^","Qtr ", efps.f$qtr)
  ebir.2$qtr <- sub("^","Qtr ", ebir.2$qtr)  
  efps.2$qtr <- sub("^","Qtr ", efps.2$qtr)

  
  vat.f = rbind(ebir.f, efps.f)
  vat.2 = rbind(ebir.2, efps.2)
  
  print(c("vat.f", nrow(vat.f)))
  print(c("vat.2", nrow(vat.2)))
  
  vat.f = keep_latest(vat.f)
  vat.2 = keep_latest(vat.2)
  
  print(c("vat.f", nrow(vat.f)))
  print(c("vat.2", nrow(vat.2)))
  
  reg_tin.f = reg.f["TIN"] %>%distinct(TIN, .keep_all = TRUE)
  reg_tin.2 = reg.2["TIN"] %>%distinct(TIN, .keep_all = TRUE)
  
  cross_checker.f = add_year_qtr (reg_tin.f , time_table)
  cross_checker.2 = add_year_qtr (reg_tin.2 , time_table)
  
  cross_checker.f = cross_checker.f %>% unite(col = "dummy", c("TIN","tax_year", "qtr"), sep = "-", remove=FALSE)
  cross_checker.2 = cross_checker.2 %>% unite(col = "dummy", c("TIN","tax_year", "qtr"), sep = "-", remove=FALSE)
  
  validity.f = cross_checking(cross_checker.f , sales.f , purch.f, vat.f)
  validity.2 = cross_checking(cross_checker.2 , sales.2 , purch.2, vat.2)
  
  print(c("validity.f",count(validity.f$count_true == 0)))
  print(c("validity.2",count(validity.2$count_true == 0)))
  
  
  val.f = add_with_values(validity.f, sales.f, purch.f, vat.f, sales.f, purch.f, vat.f)
  valid.f = tidyr::drop_na(val.f)
  valid.f["Label"] = "Known Fraud" 
  
  print(c("val.f ",count(val.f$count_true == 0)))
  print(c("valid.f ",nrow(valid.f)))
  
  val.2 = add_with_values(validity.2, sales.2, purch.2, vat.2, sales.2, purch.2, vat.2)
  valid.2 = tidyr::drop_na(val.2)
  valid.2["Label"] = "Unlabeled" 
  
  print(c("val.2 ",count(val.2$count_true == 0)))
  print(c("valid.2 ",nrow(valid.2)))
  
  valid.full.unupdated = rbind(valid.f , valid.2)
  
  print(c("valid.full.unupdated ",nrow(valid.full.unupdated)))
  
  distinct_tins = valid.full.unupdated %>% distinct(TIN, .keep_all = TRUE)
  distinct_tins = distinct_tins$TIN
  valid.full = update_with_latest(valid.full.unupdated, distinct_tins)
  
  return(valid.full)
}


data_processing_for_testing_set <- function(validation_set_label = FALSE, 
                                               reg.v = FALSE, sales.v = FALSE, purch.v = FALSE, ebir.v = FALSE, efps.v = FALSE) {
  # This function is similar to the data_processing_for_validation_set.
  ebir.v$qtr <- sub("^","Qtr ", ebir.v$qtr)  
  efps.v$qtr <- sub("^","Qtr ", efps.v$qtr)
  
  vat.v = rbind(ebir.v, efps.v)
  
  vat.v = keep_latest(vat.v)
  #print(vat.v)
  reg_tin.v = reg.v["TIN"] %>%distinct(TIN, .keep_all = TRUE)
  
  cross_checker.v = add_year_qtr(reg_tin.v ,time_table)
  
  cross_checker.v = cross_checker.v %>% unite(col = "dummy", c("TIN","tax_year", "qtr"), sep = "-", remove=FALSE)
  
  validity.v = cross_checking(cross_checker.v , sales.v , purch.v, vat.v)
  #print(validity.v)
  val.v = add_with_values(validity.v, sales.v, purch.v, vat.v, sales.v, purch.v, vat.v)
  valid.v = tidyr::drop_na(val.v)
  valid.v["Label"] = validation_set_label
  
  valid.v.unupdated = valid.v
  
  distinct_tins.v = valid.v %>% distinct(TIN, .keep_all = TRUE)
  distinct_tins.v= distinct_tins.v$TIN
  valid.v = update_with_latest(valid.v.unupdated, distinct_tins.v)
  
  return(valid.v) 
}
#############################
#Validation data splitting
#############################

data_splitting <- function (main_fraud, main_unlabeled , seed, train = 0.7, valid = 0.3) {
  df = main_fraud
  set.seed(seed)
  
  # Input 2. Set the fractions of the dataframe you want to split into training, 
  # validation, and test.
  fractionTraining   <- train
  fractionValidation <- valid
  
  # Compute sample sizes.
  sampleSizeTraining   <- fractionTraining   * nrow(df)
  sampleSizeValidation <- fractionValidation * nrow(df)
  
  
  # Create the randomly-sampled indices for the dataframe. Use setdiff() to
  # avoid overlapping subsets of indices.
  indicesTraining    <- sort(sample(seq_len(nrow(df)), size=sampleSizeTraining))
  indicesNotTraining <- setdiff(seq_len(nrow(df)), indicesTraining)
  indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))
  
  # Finally, output the three dataframes for training, validation and test.
  dfTraining   <- df[indicesTraining, ]
  dfValidation <- df[indicesValidation, ]
  
  fraud_train = dfTraining
  fraud_val = dfValidation
  
  main_train.full = rbind(fraud_train, main_unlabeled)
  return(list(fraud_val, main_train.full))
}

################################
# Model Function
################################

umodel <- function(train_fraud, unlabeled, u_method) {
  x = subset(train_fraud, select = -c(Label, TIN, VAT))
  
  if (u_method == "svm") {
    u.mod = svm(x, y=NULL, type = "one-classification")
    u.pred = predict(u.mod, subset(unlabeled, select = -c(Label, TIN, VAT)))
    
    svm.unlabeled = unlabeled
    svm.unlabeled["Label"] = u.pred
    svm.unlabeled = svm.unlabeled %>% mutate(Label = if_else(Label==TRUE, true="Likely Fraud", false="Likely Legitimate"))
    
    u.data = rbind(train_fraud, svm.unlabeled)
    
  } else if (u_method == "iso") {
    u.mod = isolation.forest(x, ndim=1, ntrees=200, nthreads=1)
    u.pred = predict.isolation_forest(u.mod, subset(unlabeled, select = -c(Label, TIN, VAT)))
    #we only want ones with high confidence in prediction
    
    iso.unlabeled = unlabeled
    iso.unlabeled["Label"] = u.pred
    iso.unlabeled = iso.unlabeled %>% mutate(Label = if_else(Label>0.50, true="Likely Fraud", false="Likely Legitimate"))
    
    u.data = rbind(train_fraud, iso.unlabeled)
  }
  return(u.data)
}

ssmodel <- function(u.data, s_method) {
  if (s_method == "multi") {
    if ("Likely Legitimate" %in% u.data$Label){
      u.data$Label = relevel(factor(u.data$Label), ref="Likely Legitimate")
      
      ss.mod = multinom(formula = Label ~ Sales + Purch + VAT + count_true + sales_ind_zscore + sales_per_zscore + 
                          purch_ind_zscore + purch_per_zscore + vat_ind_zscore + vat_per_zscore, data = u.data)
    } else {
      print ("No Likely Legitimate Labels after Unsupervised Learning, switching to Random Forest")
      ss.mod = randomForest(x=subset(u.data, select = -c(Label, TIN)), y = factor(u.data$Label), ntree=500)
    }
  } else if (s_method == "rf") {
    ss.mod = randomForest(x=subset(u.data, select = -c(Label, TIN)), y = factor(u.data$Label), ntree=500)
  }
  return(ss.mod)
}

#######################################################################################################################
setwd("C:/Data for OJT")

taxyear = 2022 #Latest year
taxqtr = 'Qtr 4'
industry = "WHOLESALE OF OTHER WASTE AND SCRAP AND PRODUCTS, N.E.C."

time_table = year_qtr_df(2018, 2022,1,4)

# SEC Fraud and Unlabeled 1
sls.f = get_sales_cols(read_excel("Summary List of Sales.xls"))
slp.f = get_purch_cols(read_excel("Summary List of Purchases.xls"))

sls.2 = get_sales_cols(read_excel("Summary List of Sales_46699.xls"))
slp.2 = get_purch_cols(read_excel("Summary List of Purchases_46699.xls"))

reg.f = filter_regdata_by_industry(read_excel("REG.xls"), industry)
reg.2 = filter_regdata_by_industry(read_excel("TRS_REG.xls"), industry)

ebir.f = get_vat_cols(read_excel("VAT_RETURNS_EBIRFORMS.xls"))

efps.f = get_vat_cols(read_excel("VAT_RETURNS_EFPS.xls"))

ebir.2 = get_vat_cols(read_excel("VAT_RETURNS_EBIRFORMS_46699.xls"))
efps.2 = get_vat_cols(read_excel("VAT_RETURNS_EFPS_46699.xls"))

seeds = c(7,10,30,77,14,62,68,95,82,87,75,51)

######################################################
#Loading BIR fraud Data

sls.f.bir = get_sales_cols(read_excel("SLS_2018_2022.xlsx"))
slp.f.bir = get_purch_cols(read_excel("SLP_2018_2022.xlsx"))

reg.f.bir = filter_regdata_by_industry(read_excel("TRS.xlsx"),industry)

ebir.f.bir = read_excel("VAT_RETURNS_EBIR.xls")[,-5]
ebir.f.bir = ebir.f.bir %>% rename(TIN = TIN...2)
ebir.f.bir = get_vat_cols(ebir.f.bir)


efps.f.bir = ebir.f.bir[0,]
efps.f.bir


######################################################
# Loading batch 2 of unlabeled data
sls_2018 = get_sales_cols(read_excel("C:\\Data for OJT\\Data Request 03202024_Random 200 Scraps\\SLS_2018.xlsx"))
sls_2019 = get_sales_cols(read_excel("C:\\Data for OJT\\Data Request 03202024_Random 200 Scraps\\SLS_2019.xlsx"))
sls_2020 = get_sales_cols(read_excel("C:\\Data for OJT\\Data Request 03202024_Random 200 Scraps\\SLS_2020.xlsx"))
sls_2021 = get_sales_cols(read_excel("C:\\Data for OJT\\Data Request 03202024_Random 200 Scraps\\SLS_2021.xlsx"))
sls_2022 = get_sales_cols(read_excel("C:\\Data for OJT\\Data Request 03202024_Random 200 Scraps\\SLS_2022.xlsx"))

slp_2018 = read_excel("C:\\Data for OJT\\Data Request 03202024_Random 200 Scraps\\SLP_2018.xlsx")
slp_2019 = read_excel("C:\\Data for OJT\\Data Request 03202024_Random 200 Scraps\\SLP_2019.xlsx")
slp_2020 = read_excel("C:\\Data for OJT\\Data Request 03202024_Random 200 Scraps\\SLP_2020.xlsx")
slp_2021 = read_excel("C:\\Data for OJT\\Data Request 03202024_Random 200 Scraps\\SLP_2021.xlsx")
slp_2022 = read_excel("C:\\Data for OJT\\Data Request 03202024_Random 200 Scraps\\SLP_2022.xlsx")

sls.3 = rbind(sls_2018,sls_2019,sls_2020,sls_2021,sls_2022)
slp.3 = rbind(slp_2018,slp_2019,slp_2020,slp_2021,slp_2022)
slp.3 = get_purch_cols(slp.3)

reg.3 = filter_regdata_by_industry(read_excel("C:\\Data for OJT\\Data Request 03202024_Random 200 Scraps\\REG_IRIS.xlsx"),industry)

ebir.3 = get_vat_cols(read_excel("C:\\Data for OJT\\Data Request 03202024_Random 200 Scraps\\VAT_Returns_EBIR.xlsx"))
efps.3 = get_vat_cols(read_excel("C:\\Data for OJT\\Data Request 03202024_Random 200 Scraps\\VAT_Returns_EFPS.xlsx"))


#######################################################
# Load data for testing. Note that this code is problematic as there was a large volume of data in the
# SLS and SLP.

for (i in 1:17) {
  if (i == 1) {
    sls.test = get_sales_cols(read_excel(paste0("Data Request 04112024/SLS/sls",i, ".xlsx")))
  } else {
    sls.test = rbind(sls.test, get_sales_cols(read_excel(paste0("Data Request 04112024/SLS/sls",i, ".xlsx"))))
  }
}

openxlsx::write.xlsx(sls.test,file="C:\\Data for OJT\\sls_test.xlsx") 

for (i in 1:9) {
  if (i == 1) {
    slp.test = get_purch_cols(read_excel(paste0("Data Request 04112024/SLP/slp",i, ".xlsx")))
  } else {
    slp.test = rbind(slp.test, get_purch_cols(read_excel(paste0("Data Request 04112024/SLP/slp",i, ".xlsx"))))
  }
}


reg.test = read_excel("Data Request 04112024/REG.xlsx")%>%distinct(TIN, .keep_all = FALSE)

slp.test.a = aggregate_industry_purchases(slp.test)

sls.test = read_excel("C:\\Data for OJT\\sls_test.xlsx")

#This loop was terminated early on
for (i in 1:17) {
 if (i == 1) {
   sls.test.a = aggregate_industry_sales(read_excel(paste0("Data Request 04112024/SLS/sls",i, ".xlsx")))
 } else {
   sls.test.a = rbind(sls.test.a, aggregate_industry_sales(read_excel(paste0("Data Request 04112024/SLS/sls",i, ".xlsx"))))
 }
}


reg.test = sls.test.a %>% distinct(owner_tin, .keep_all = FALSE)
colnames(reg.test) = "TIN"
slp.test.a = filter_slsp_by_tins(slp.test.a, reg.test)

slp.test.a = read_excel("C:\\Data for OJT\\slp_test.xlsx")
slp.test.a = slp.test.a %>% group_by(dummy, owner_tin , tax_year, qtr) %>% summarise(total_purch = sum(total_purch), .groups = "keep" )

sls.test.a = read_excel("C:\\Data for OJT\\sls_test.xlsx")
sls.test.a = sls.test.a %>% group_by(dummy, owner_tin , tax_year, qtr) %>% summarise(total_Sales = sum(total_Sales), .groups = "keep" )

openxlsx::write.xlsx(sls.test.a,file="C:\\Data for OJT\\sls_test.xlsx") 
openxlsx::write.xlsx(slp.test.a,file="C:\\Data for OJT\\slp_test.xlsx") 

ebir.test = get_vat_cols(read_excel("Data Request 04112024/VAT_RETURNS_EBIR_forms.xlsx"))
efps.test = get_vat_cols(read_excel("Data Request 04112024/VAT_RETURNS_EFPS.xlsx"))

ebir.test = ebir.test %>% filter(owner_tin %in% reg.test$owner_tin)
efps.test = efps.test %>% filter(owner_tin %in% reg.test$owner_tin)


#############################
# Model Testing
#############################
final_test <- function( main_train.full, test_set, u.method, s.method){
  # This function trains the selected unsupervised and supervised methods on the chosen dataset,
  # then uses the supervised model to predict the labels of a test data.
  
  unsup_fraud = filter(main_train.full , Label == "Known Fraud")
  unlabeled = filter(main_train.full , Label == "Unlabeled")
  
  test_data = umodel(unsup_fraud, unlabeled, u.method)
  test_model = ssmodel(test_data, s.method)
  
  test_pred = predict(test_model, subset(test_set, select = -c(Label, TIN)))
  
  TINS = test_set %>% distinct(TIN, .keep_all = FALSE)
  
  pred_df = data.frame(TINS)
  pred_df["Label"] = test_pred
  
  return (pred_df)
}

#to train: SVM + RF, SEC + UNL 1

sls.f_sec.unl_2 = rbind(sls.f , sls.2)
slp.f_sec.unl_2 = rbind(slp.f , slp.2)

#training set
valid.f_sec.unl_2 = as.data.frame(data_processing_for_training_set (reg.f, sls.f, slp.f, ebir.f, efps.f,
                                                                    reg.2, sls.2, slp.2, ebir.2, efps.2))

main_full.f_sec.unl_2 = create_indept_vars(valid.f_sec.unl_2, sls.f_sec.unl_2, slp.f_sec.unl_2 , valid.f_sec.unl_2)

sls.test.a = tidyr::drop_na(sls.test.a)
slp.test.a = tidyr::drop_na(slp.test.a)

#test set
valid.test = as.data.frame(data_processing_for_testing_set (validation_set_label = "Unlabeled", 
                                                                reg.test, sls.test.a, slp.test.a, ebir.test, efps.test))

main_full.test = create_indept_vars(valid.test, sls.test.a, slp.test.a , valid.f_sec.unl_2)

p.SVM_RF_SEC_UNL1 = final_test( main_full.f_sec.unl_2, main_full.test, "svm", "rf")
openxlsx::write.xlsx(p.SVM_RF_SEC_UNL1,file="C:\\Data for OJT\\SVM_RF_SEC_UNL1.xlsx") 
test_pred = p.SVM_RF_SEC_UNL1$Label
perc_kf = sum(test_pred == "Known Fraud")/length(test_pred)
perc_lf = sum(test_pred == "Likely Fraud")/length(test_pred)
perc_ll = sum(test_pred == "Likely Legitimate")/length(test_pred)
perc_f = perc_kf + perc_lf

plist.svm.rf.SEC.UNL1 = c(perc_f, perc_kf, perc_ll)


#############################################################################
# SVM + MULTI , SEC + UNL 1
p.SVM_MULTI_SEC_UNL1 = final_test( main_full.f_sec.unl_2, main_full.test, "svm", "multi")
openxlsx::write.xlsx(p.SVM_MULTI_SEC_UNL1,file="C:\\Data for OJT\\SVM_MULTI_SEC_UNL1.xlsx") 
test_pred = p.SVM_MULTI_SEC_UNL1$Label

perc_kf = sum(test_pred == "Known Fraud")/length(test_pred)
perc_lf = sum(test_pred == "Likely Fraud")/length(test_pred)
perc_ll = sum(test_pred == "Likely Legitimate")/length(test_pred)
perc_f = perc_kf + perc_lf

plist.svm.multi.SEC.UNL1 = c(perc_f, perc_kf, perc_ll)

# ISO + MULTI , BIR + UNL 2

#training set

sls.f_bir.unl_3 = rbind(sls.f.bir , sls.3)
slp.f_bir.unl_3 = rbind(slp.f.bir , slp.3)

valid.f_bir.unl_3 = as.data.frame(data_processing_for_training_set (reg.f.bir, sls.f.bir, slp.f.bir, ebir.f.bir, efps.f.bir,
                                                                    reg.3, sls.3, slp.3, ebir.3, efps.3))

main_full.f_bir.unl_3 = create_indept_vars(valid.f_bir.unl_3, sls.f_bir.unl_3, slp.f_bir.unl_3 , valid.f_bir.unl_3)

main_full.test = create_indept_vars(valid.test, sls.test.a, slp.test.a , valid.f_bir.unl_3)

p.ISO_MULTI_BIR_UNL2 = final_test( main_full.f_bir.unl_3, main_full.test, "iso", "multi")
openxlsx::write.xlsx(p.ISO_MULTI_BIR_UNL2,file="C:\\Data for OJT\\ISO_MULTI_BIR_UNL2.xlsx") 
test_pred = p.ISO_MULTI_BIR_UNL2$Label

perc_kf = sum(test_pred == "Known Fraud")/length(test_pred)
perc_lf = sum(test_pred == "Likely Fraud")/length(test_pred)
perc_ll = sum(test_pred == "Likely Legitimate")/length(test_pred)
perc_f = perc_kf + perc_lf

plist.iso.multi.BIR.UNL2 = c(perc_f, perc_kf, perc_ll)


