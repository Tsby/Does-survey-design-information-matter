#####################################################
############# Code for the Case Study 1 #############
#####################################################

## Author: Aleksandr Tsybakin
## Date:   28/03/2021


################## 1. Introduction ##################

# Import packages
library(mi)
library(missForest)
library(VIM)
library(survey)
library(lattice)
lattice.options(default.theme = standard.theme(color = FALSE))

# Upload the data
df = read.csv("./chms_2018.csv")
head(df[1:8], 5)

sprintf("Number of rows: %d", nrow(df))
sprintf("Number of columns: %d", ncol(df))

# Define categorical and numerical variables
cat_cols = c("SMK_12","CLC_SEX", "HIGHBP")
num_cols = !(colnames(df) %in% cat_cols)

# Convert numbers into categories for smoking factor
df[(!is.na(df$SMK_12)) & (df$SMK_12 == 1), "SMK_12"] = "daily" 
df[(!is.na(df$SMK_12)) & (df$SMK_12 == 2), "SMK_12"] = "occasional" 
df[(!is.na(df$SMK_12)) & (df$SMK_12 == 3), "SMK_12"] = "non-smoker" 
df$SMK_12 = factor(df$SMK_12)

# Convert numbers into categories for sex factor
df$CLC_SEX = ifelse(test=df$CLC_SEX==1, yes="male", no="female")
df$CLC_SEX = factor(df$CLC_SEX)

# Convert numbers into categories for target variable
df$HIGHBP = ifelse(test=df$HIGHBP==1, yes="yes", no="no")
df$HIGHBP = factor(df$HIGHBP)

# Convert the rest columns to numeric type
df[, num_cols] = sapply(df[, num_cols], function(x) as.numeric(x))
head(df[1:8], 5)



################### 2. Exploratory Data Analysis ####################

################# 2.1. Exploring the missing values #################

# Get missing values per column
sort_nan_cols = sort(colSums(is.na(df[2:8])), decreasing=TRUE)

# Plot a bar chart
bp = barplot(sort_nan_cols / nrow(df), 
             ylim=c(0, 0.03),
             ylab="Number of missing values",
             main="Histogram of missing values in columns",
             las=2)
text(bp, sort_nan_cols / nrow(df), labels=sort_nan_cols, cex=1, pos=3)

# Get the ratio of rows with missing values
sum(!complete.cases(df)) / nrow(df)


################# 2.2. Imputation of missing values #################

MAPE = function(y_pred, y_true) {
  # This function calculates the MAPE error
  return(mean(abs((y_pred - y_true) / y_true)))
}

mi_imputation = function(df_check_mi) {
  # This function imputes the missing values using Multiple imputations
  # from the mi package
  
  # Define the Missing Data Frame
  mdf = missing_data.frame(df_check_mi)
  
  # Change the method to Predictive Mean Matching
  mdf = change(mdf, y=missing_vars, what="method", to="pmm")
  
  # Impute missing values with  1 chain and 30 iterations
  imputations = mi(mdf, n.iter=30, n.chains=1, max.minutes=15, verbose=FALSE)
  
  # Get the imputed data
  imputed_df = complete(imputations)
  imputed_df = imputed_df[, main_vars]
  return(imputed_df)
}

# Define the columns of interest
main_vars = c("HWMDBMI", "LAB_BHG", "LAB_BCD", "SMK_12", "CLC_SEX", "CLC_AGE")
missing_vars = c("HWMDBMI", "LAB_BHG", "LAB_BCD", "SMK_12")

# Remove unnecessary rows
df_check = df[complete.cases(df[, main_vars]), main_vars]
df_check = df_check[(df_check[, "LAB_BCD"] != 999.5) & (df_check[, "LAB_BHG"] != 999.5),]

# Define the number of simulations
N_SIM = 25 

# Define an Error matrix
errors = matrix(0, ncol=length(missing_vars), nrow=3)

for (k in 1:N_SIM) {
  # Initialize store lists
  df_check_cur = data.frame(df_check)
  imputed_dfs = list()
  inds = list()
  
  # Generate missing values
  inds[[1]] = sample(1:nrow(df_check), sort_nan_cols["HWMDBMI"])
  inds[[2]] = sample(1:nrow(df_check), sort_nan_cols["LAB_BHG"])
  inds[[3]] = sample(1:nrow(df_check), sort_nan_cols["LAB_BCD"])
  inds[[4]] = sample(1:nrow(df_check), sort_nan_cols["SMK_12"])
  
  df_check_cur[inds[[1]], "HWMDBMI"] = NA
  df_check_cur[inds[[2]], "LAB_BHG"] = NA
  df_check_cur[inds[[3]], "LAB_BCD"] = NA
  df_check_cur[inds[[4]], "SMK_12"] = NA
  
  # Impute missing values using 3 methods
  imputed_dfs[[1]] = mi_imputation(df_check_cur)
  imputed_dfs[[2]] = missForest(df_check_cur, maxiter=10, ntree=100, verbose=FALSE)$ximp
  imputed_dfs[[3]] = kNN(df_check_cur, k=3)
  
  # Calculate the errors for each column
  for (i in 1:3) {
    for (j in 1:length(missing_vars)) {
      cur_imp_df = imputed_dfs[[i]]
      cur_col = missing_vars[j]
      cur_rows = inds[[j]]
      
      if (cur_col == "SMK_12") {
        # Calculate Accuracy
        cur_error = (cur_imp_df[cur_rows, cur_col] == df_check[cur_rows, cur_col])
        errors[i, j] = errors[i, j] + cur_error
      } else {
        # Calculate MAPE
        cur_MAPE = MAPE(cur_imp_df[cur_rows, cur_col], df_check[cur_rows, cur_col])
        errors[i, j] = errors[i, j] + cur_MAPE
      }
    }
  }
}

# Get an average errors 
errors_df = as.data.frame(errors / N_SIM, row.names=c("MI", "MissForest", "KNN"))
colnames(errors_df) = c("HWMDBMI", "LAB_BHG", "LAB_BCD", "SMK_12")
errors_df

# Remove all rows with missing values
df = df[complete.cases(df), ]


################### 2.3. Below-the-limit values ###################

get_below_the_limit_vals = function(col) {
  # This function count below-the-limit values in the given column
  col_without_na = df[, col]
  inds = sapply(col_without_na, function(x) if(x == 999.5){1} else {0})
  return(sum(inds))
} 

# Get the below-the-limit values
for_hist = c(get_below_the_limit_vals("LAB_BHG"), get_below_the_limit_vals("LAB_BCD"))

# Plot a bar chart 
bp = barplot(for_hist / nrow(df), 
             ylim=c(0, 1),
             ylab="Number of of the below-the-limit values",
             main="Histogram of the below-the-limit values",
             names.arg=c("LAB_BCD", "LAB_BHG"),
             las=2)
text(bp, for_hist / nrow(df), labels=round(for_hist / nrow(df), 4), cex=1, pos=3)

# Change below-the-limit values to the minimum values in columns
df$LAB_BCD = sapply(df$LAB_BCD, function(x) if(!(is.na(x)) & (x == 999.5)){0.71} else {x})
df$LAB_BHG = sapply(df$LAB_BHG, function(x) if(!(is.na(x)) & (x == 999.5)){2.1}  else {x})


####################### 2.4. Duplicates #######################

# Check duplicates
any(duplicated(df[2:8]) | duplicated(df[2:8], fromLast=TRUE))


####################### 2.5. Visualization ####################

# Smoking factor factorized by the hypertension indicator
histogram(~ SMK_12  | HIGHBP, data=df)

# Sex factor factorized by the hypertension indicator
histogram(~ CLC_SEX | HIGHBP, data=df)

# Age factorized by the hypertension indicator
histogram(~ CLC_AGE | HIGHBP, data=df)

# Body mass index factorized by the hypertension indicator
histogram(~ HWMDBMI | HIGHBP, data=df)

# Blood cadmium factorized by the hypertension indicator
histogram(~ LAB_BCD | HIGHBP, data=df)

# Blood mercury factorized by the hypertension indicator
histogram(~ LAB_BHG | HIGHBP, data=df)


#################### 3. Model Analysis ####################

#################### 3.1 Simple models #################### 

######## 3.1.1. Models without survey information ######### 

# Define a simple model:
# 1. Set the formula that includes all variables.
# 2. Determine the data.
# 3. Set the binomial distribution, logit link function by default.
model_1 = glm(HIGHBP ~ SMK_12 + CLC_SEX + CLC_AGE + HWMDBMI + LAB_BCD + LAB_BHG, 
              data=df,                                                           
              family="binomial")                                                 
summary(model_1)


########## 3.1.1. Models with survey information ##########

# Define a survey design:
# 1. Determine the data.
# 2. Set one stage of sampling design.
# 3. Determine the weights.
# 4. Determine the variables of interest. 
dsg = svydesign(data=df,
                id=~1, 
                weights=~WGT_FULL, 
                variables=df[,c("SMK_12","CLC_SEX","CLC_AGE","HWMDBMI",
                                "LAB_BCD","LAB_BHG", "HIGHBP")])

# Define a model with survey design
# 1. Set the formula that includes all variables.
# 2. Determine the design of the survey.
# 3. Set the 'quasibinomial' distribution as proposed by tutorials to avoid warnings.
model_2 = svyglm(HIGHBP ~ SMK_12 + CLC_SEX + CLC_AGE + HWMDBMI + LAB_BCD + LAB_BHG, 
                 design=dsg,
                 family="quasibinomial")                                             
summary(model_2)

# Get the summary for 11 degrees of freedom
summary(model_2, df.resid=11)

