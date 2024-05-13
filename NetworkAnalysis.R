library("readxl")
library("data.table")
library("dplyr")
library("benford.analysis")
library("matrixStats")
library("tidyr")
library("e1071") # for svm
library("isotree") # for isolation forest
library("nnet") # for multinomial
library("Metrics")
library("ConfusionTableR")
library("randomForest")

reg_data = read_excel("C:\\Data for OJT\\Data Request 03212024_SLSP Batch 3\\REG-IRIS.xlsx")
slp2018_b3 = read_excel("C:\\Data for OJT\\Data Request 03212024_SLSP Batch 3\\SLP_2018.xlsx")
slp2019_b3 = read_excel("C:\\Data for OJT\\Data Request 03212024_SLSP Batch 3\\SLP_2019.xlsx")
slp2020_b3 = read_excel("C:\\Data for OJT\\Data Request 03212024_SLSP Batch 3\\SLP_2020.xlsx")
slp2021_b3 = read_excel("C:\\Data for OJT\\Data Request 03212024_SLSP Batch 3\\SLP_2021.xlsx")
slp2022_b3 = read_excel("C:\\Data for OJT\\Data Request 03212024_SLSP Batch 3\\SLP_2022.xlsx")

summarize_purchases <- function(slp) {
  # This function counts the number of transactions in each owner TIN and seller TIN in the SLP data.
  
  summary = as.data.frame(table(slp$owner_tin, slp$reg_tin))
  colnames(summary) = c("owner_tin", "seller_tin", "frequency")
  summary = summary[which(summary$frequency>= 1),]
  return(summary)
}

get_seller_deg <- function(sum_purch, freq = 4) {
  # This function gets the number of sellers which had at least four buyers during a specified period of time.
  
  seller_deg = data.frame(seller_tin = NA, degree=NA)
  for (tin in sum_purch$seller_tin) {
    seller_deg[tin,c("seller_tin","degree")] = c(tin, nrow(filter(sum_purch, sum_purch$seller_tin == tin)))
  }
  seller_deg = drop_na(seller_deg)
  rownames(seller_deg) <- NULL
  seller_deg["degree"] = as.numeric(seller_deg$degree)
  seller_deg = seller_deg %>% arrange(desc(degree))
  seller_deg = seller_deg[which(seller_deg$degree > freq),]
  return(seller_deg)
}

sum_filt2018 = summarize_purchases(slp2018_b3)
seller_deg2018 = get_seller_deg(sum_filt2018)
seller_deg2019 = get_seller_deg(summarize_purchases(slp2019_b3))
seller_deg2020 = get_seller_deg(summarize_purchases(slp2020_b3))
seller_deg2021 = get_seller_deg(summarize_purchases(slp2021_b3))
seller_deg2022 = get_seller_deg(summarize_purchases(slp2022_b3))

merged = merge(x = seller_deg2018, y = seller_deg2019, by = "seller_tin", all = TRUE)
colnames(merged) <- c("seller_tin", "2018", "2019")
merged = merge(x = merged, y = seller_deg2020, by = "seller_tin", all = TRUE)
colnames(merged) <- c("seller_tin", "2018", "2019", "2020")
merged = merge(x = merged, y = seller_deg2021, by = "seller_tin", all = TRUE)
colnames(merged) <- c("seller_tin", "2018", "2019", "2020", "2021")
merged = merge(x = merged, y = seller_deg2022, by = "seller_tin", all = TRUE)
colnames(merged) <- c("seller_tin", "2018", "2019", "2020", "2021", "2022")
names(merged)
#seller_tins = rbind(seller_deg2018,seller_deg2019,seller_deg2020,seller_deg2021,seller_deg2022)["seller_tin"] %>% distinct(seller_tin)
openxlsx::write.xlsx(merged,file="C:\\Data for OJT\\seller_tins.xlsx") 
