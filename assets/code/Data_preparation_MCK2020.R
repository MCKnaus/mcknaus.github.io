###############################################################################
#               Michael C. Knaus, St. Gallen, 12.10.2020                      #
# This file produces the dataset that underlies the analysis of the paper     #
# "Double machine learning based program evaluation under unconfoundedness"   #
# The raw dataset "Swiss_ALMP.csv" can be requested via the FORS database     #
# (https://forsbase.unil.ch/project/study-public-overview/17035/1/). It is    #
# not a public use file but researchers can request access via the infra-     #
# structure of the FORS database.                                             #
# After receiving the data, save the file "Swiss_ALMP.csv" in a folder of     #
# your choice, change the working directory in line 16 and run the file.      #
# If you encounter problems, feel free to contact me.                         #
###############################################################################


# Set working directory to the folder where the Swiss_ALMP.csv file is stored
setwd("C:/Users/Administrator/switchdrive/Papers/Multi Treat Labor/Paper/Data")

# Load packages to be used
library(tidyverse)
library(grf)

set.seed(1234)

# Read data
db = read.csv("Swiss_ALMP.csv")

# Remove non German speaking cantons as well as employment and personality programs
db = db %>% filter(canton_german == 1) %>% filter(!(treatment6 == "employment" | treatment6 == "personality"))

# Get covariate matrix
x = as.matrix(select(db,age,canton_moth_tongue,city_big,city_medium,city_no,cw_age,cw_cooperative,
                     cw_educ_above_voc,cw_educ_tertiary,cw_female,cw_missing,cw_own_ue,cw_tenure,
                     cw_voc_degree,emp_share_last_2yrs,emp_spells_5yrs,employability,female,foreigner_b,foreigner_c,
                     gdp_pc,married,other_mother_tongue,past_income,prev_job_manager,prev_job_sec_mis,prev_job_sec1,
                     prev_job_sec2,prev_job_sec3,prev_job_self,prev_job_skilled,prev_job_unskilled,qual_semiskilled,
                     qual_degree,qual_unskilled,qual_wo_degree,swiss,ue_cw_allocation1,ue_cw_allocation2,ue_cw_allocation3,
                     ue_cw_allocation4,ue_cw_allocation5,ue_cw_allocation6,ue_spells_last_2yrs,unemp_rate))

# Define variable labels
label_x = c("Age","Mother tongue in canton's language","Lives in big city","Lives in medium city",
            "Lives in no city","Caseworker age","Caseworker cooperative","Caseworker education: above vocational training",
            "Caseworker education: tertiary track","Caseworker female","Missing caseworker characteristics",
            "Caseworker has own unemployemnt experience","Caseworker tenure","Caseworker education: vocational degree",
            "Fraction of months employed last 2 years","Number of employment spells last 5 years","Employability",
            "Female","Foreigner with temporary permit","Foreigner with permanent permit","Cantonal GDP p.c.",
            "Married","Mother tongue other than German, French, Italian","Past income","Previous job: manager",
            "Missing sector","Previous job in primary sector","Previous job in secondary sector","Previous job in tertiary sector",
            "Previous job: self-employed","Previous job: skilled worker","Previous job: unskilled worker","Qualification: semiskilled",
            "Qualification: some degree","Qualification: unskilled","Qualification: skilled without degree","Swiss citizen",
            "Allocation of unemployed to caseworkers: by industry","Allocation of unemployed to caseworkers: by occupation",
            "Allocation of unemployed to caseworkers: by age","Allocation of unemployed to caseworkers: by employability",
            "Allocation of unemployed to caseworkers: by region","Allocation of unemployed to caseworkers: other",
            "Number of unemployment spells last 2 years","Cantonal unemployment rate (in %)")

### Assign pseudo random starting
# Estimate probability to start ALMP in month 4-6 after unemployment
rf_late = regression_forest(x[db$treatment6 != "no program",],db$start_q2[db$treatment6 != "no program"],tune.parameters = "all",seed=1234)
p_late = predict(rf_late,x[db$treatment6 == "no program",])
# Generate pseudo starting point for those in no program
db$elap = db$start_q2
db$elap[db$treatment6 == "no program"] = 0 + rbernoulli(sum(db$treatment6 == "no program"),p_late)
table(db$elap)

### Remove those that are employed at the pseudo starting point
db = db %>% filter(!(db$elap == 1 & (db$employed1 == 1 | db$employed2 == 1 | db$employed3 == 1)))

# Create outcome
emp = matrix(NA,nrow(db),31)
emp[db$elap == 0,] = as.matrix(db[db$elap == 0,c(sprintf("employed%s",seq(3,33)))])
emp[db$elap == 1,] = as.matrix(db[db$elap == 1,c(sprintf("employed%s",seq(6,36)))])
y = rowSums(emp)

# Update covariate matrix
x = as.matrix(select(db,age,canton_moth_tongue,city_big,city_medium,city_no,cw_age,cw_cooperative,
                     cw_educ_above_voc,cw_educ_tertiary,cw_female,cw_missing,cw_own_ue,cw_tenure,
                     cw_voc_degree,emp_share_last_2yrs,emp_spells_5yrs,employability,female,foreigner_b,foreigner_c,
                     gdp_pc,married,other_mother_tongue,past_income,prev_job_manager,prev_job_sec_mis,prev_job_sec1,
                     prev_job_sec2,prev_job_sec3,prev_job_self,prev_job_skilled,prev_job_unskilled,qual_semiskilled,
                     qual_degree,qual_unskilled,qual_wo_degree,swiss,ue_cw_allocation1,ue_cw_allocation2,ue_cw_allocation3,
                     ue_cw_allocation4,ue_cw_allocation5,ue_cw_allocation6,ue_spells_last_2yrs,unemp_rate))

# Create treatment
w = db$treatment6
w = factor(w,c("no program","job search","vocational","computer","language"))
label_w = c("No program","Job search",  "Vocational",  "Computer",  "Language")

# Create heterogeneity variables
z_female = x[,"female"]
z_foreigner = 
z_employ = x[,"employability"]
z_age = x[,"age"]
z_inc = x[,"past_income"]
z_blp = data.frame("female" = x[,"female"],
                   "age" = x[,"age"],
                   "foreigner" = x[,"foreigner_b"] + x[,"foreigner_c"],
                   "employability" = factor(x[,"employability"]),
                   "past_incomein10000" = x[,"past_income"]/10000)

# Create optimal policy variables
x_pt_low = cbind(z_age,z_employ,z_female,z_foreigner,z_inc)
colnames(x_pt_low) = c("Age","Employability","Female","Foreigner","Past income")
x_pt_high = x[,c(15:16,24:36,44)]
colnames(x_pt_high) = label_x[c(15:16,24:36,44)]

save(y,w,x,z_blp,x_pt_low,x_pt_high,label_x,label_w,file="Database_SALMP_MCK2020.Rdata")