
# coding: utf-8

# # Import the libraries

# In[157]:

import pandas as pd
import numpy as np
import os
import requests
import zipfile
import io


# In[158]:

#s = requests.session()
#payload = {'username': 'jain.yo@husky.neu.edu', 'password': '0_iSRa}e'}
#url = 'https://freddiemac.embs.com/FLoan/secure/auth.php'
#a = s.post(url, data=payload)

#values = {'accept': 'Yes','acceptSubmit':'Continue','action': 'acceptTandC'}
#url1 = 'https://freddiemac.embs.com/FLoan/Data/download.php'
#b = s.post(url1,data=values)


# In[159]:

#for year in range(1999,2002):
#    url3 = 'https://freddiemac.embs.com/FLoan/Data/sample_'+str(year)+'.zip'
#    file = s.get(url3)
#    m=zipfile.ZipFile(io.BytesIO(file.content))
#    m.extractall()


# # Change the directory to the location of source file

# os.getcwd()
# tar = os.chdir(os.getcwd()+'/'+'SampleFile')

# # Load each performance file and read using pandas

# In[160]:

performance_files = [file for file in os.listdir() if file.startswith('sample_svcg')]


# In[165]:

print (performance_files[:])       ### performance files from 1999-2016


# # Load the data

# In[177]:

def load_data(file):
    df = pd.read_table(file,delimiter='|', header=None)
    return df


# In[167]:

#df.head()


# # Name the columns

# In[178]:

def name_column(df):

    df.columns=['LOAN_SEQUENCE_NUMBER','MONTHLY_REPORTING_PERIOD','CURRENT_ACTUAL_UPB',
                     'CURRENT_LOAN_DELINQUENCY_STATUS','LOAN_AGE','REMAINING_MONTHS_TO_LEGAL_MATURITY',
                     'REPURCHASE_FLAG','MODIFICATION_FLAG','ZERO_BALANCE_CODE','ZERO_BALANCE_EFFECTIVE_DATE',
                     'CURRENT_INTEREST_RATE','CURRENT_DEFERRED_UPB','DUE_DATE_OF_LAST_PAID_INSTALLMENT_DDLPI',
                     'MI_RECOVERIES','NET_SALES_PROCEEDS','NON_MI_RECOVERIES','EXPENSES','Legal_Costs',
                     'Maintenance_and_Preservation_Costs','Taxes_and_Insurance','Miscellaneous_Expenses',
                     'Actual_Loss_Calculation']
    numberofrows = len(df.index)
    print(numberofrows)
    return df


# # Check the DataTypes

# In[12]:

#perfor_data.dtypes


# # Validate the columns

# # 1) LOAN SEQUENCE NUMBER  
# 
# It's a Unique identifier assigned to each loan. 
# It is of length 12 and alpha-numeric type.
# 
# valid values: F1YYQnXXXXXX
# F1 = product (Fixed Rate Mortgage);
# YYQn = origination
# year and quarter; and,
# XXXXXX = randomly
# assigned digits

# In[106]:

#perfor_data['LOAN_SEQUENCE_NUMBER'].unique()


# In[13]:

## check first and last five loan seq number

#print(pd.value_counts(perfor_data['LOAN_SEQUENCE_NUMBER'].values, sort=True).head())

#print(pd.value_counts(perfor_data['LOAN_SEQUENCE_NUMBER'].values,sort=True).tail())


# # validation pending... for loan

# # 2) MONTHLY REPORTING PERIOD 
# The as-of month for loan information contained in the loan record.
# 
# Valid Values: YYYYMM
# Type: Date 
# length : 6

# Check if there are null values in the column and
# if there are any replace it with : 199701 

# In[136]:

def validate_monthly_report(df):

    if any(df['MONTHLY_REPORTING_PERIOD'].isnull()):
        df['MONTHLY_REPORTING_PERIOD'].fillna('199701')
    return df
    
    


# # 3) CURRENT ACTUAL UPB 
# 
# The Current Actual UPB reflects the mortgage ending balance as reported by the servicer for the corresponding monthly reporting period.
# For fixed rate mortgages, this UPB is derived from the mortgage balance as reported by the servicer 
# and includes any scheduled and unscheduled principal reductions applied to the mortgage.
# 
# Calculation: (interest bearing UPB) + (non- interest bearing UPB)
# Numeric Literal decimal
# length : 2

# In[137]:

def validate_upb(df):
    if any(df['CURRENT_ACTUAL_UPB'].isnull()):
        df['CURRENT_ACTUAL_UPB'].fillna('00')
    return df


# # 4) CURRENT LOAN DELINQUENCY STATUS 
# 
# A value corresponding to the number of days the borrower is delinquent, based on the due date of last paid installment (“DDLPI”) reported by
# ervicers to Freddie Mac, and is calculated under the Mortgage Bankers Association (MBA) method.
# If a loan has been acquired by REO, then the Current Loan Delinquency Status will reflect the value corresponding to that status (instead of the value corresponding to the number of days the borrower is delinquent).

#  XX = Unknown
#  0 = Current, or less
# than 30 days past due
#  1 = 30-59 days delinquent
#  2=60–89days delinquent
#  3=90–119days delinquent
#  And so on...
#  R = REO Acquisition
#  Space (3) =
# Unavailable

# In[117]:

#print(pd.value_counts(perfor_data['CURRENT_LOAN_DELINQUENCY_STATUS'].values, sort=False))


# In[129]:

#if perfor_data['CURRENT_LOAN_DELINQUENCY_STATUS'].isnull().values.any() == True:
#    df.CURRENT_LOAN_DELINQUENCY_STATUS.replace(np.NaN, 'XX', inplace=True)


# In[106]:

def loan_delinquency_status(df):
    if any(df['CURRENT_LOAN_DELINQUENCY_STATUS'].isnull()):
        df['CURRENT_LOAN_DELINQUENCY_STATUS'].fillna('U')
            
        df['CURRENT_LOAN_DELINQUENCY_STATUS'] = df.CURRENT_LOAN_DELINQUENCY_STATUS.replace('   ','U')
    return df


# # 5)LOAN AGE
# 
# from the columns Monthly Reporting Period : if its 201602 - then loan age is 0, 201603 is 1 and 201604 is 2 and so one.
# 
# should i put a range from (0,999)

# In[138]:

def validate_loan_age(df):
    if any(df['LOAN_AGE'].isnull()):
        df['LOAN_AGE'] = df.LOAN_AGE.fillna('NA')
        
    if any(df['LOAN_AGE']) not in range(0,1000):
        print(" loan age value out of range")
    return df


# # 6 REMAINING MONTHS TO LEGAL MATURITY
# 
# Since its numneric. what to fill na with??
# 

# In[139]:

def validate_remaining_months_to_legal_maturity(df):
    if any(df['REMAINING_MONTHS_TO_LEGAL_MATURITY'].isnull()):
        df['REMAINING_MONTHS_TO_LEGAL_MATURITY'] = df.REMAINING_MONTHS_TO_LEGAL_MATURITY.fillna('NA')
        
    if any(df['REMAINING_MONTHS_TO_LEGAL_MATURITY']) not in range(0,1000):
        print(" loan age value out of range")
    
    return df


# # 7 REPURCHASE FLAG

# Null values replaced with U (Unknown)

# In[186]:

def validate_repurchase_flag(df):
    if any(df['REPURCHASE_FLAG'].isnull()):
        df['REPURCHASE_FLAG'] = df.REPURCHASE_FLAG.fillna('U')
   
    df['REPURCHASE_FLAG'] = df.REPURCHASE_FLAG.replace(' ','U')
    return df


# # 8 MODIFICATION FLAG 

# Replaced Null as Unknown and Replaced ' ' with N

# In[187]:

def validation_modification_flag(df):
    if any(df['MODIFICATION_FLAG'].isnull()):
        df['MODIFICATION_FLAG'] = df.MODIFICATION_FLAG.fillna('U')
    
    df['MODIFICATION_FLAG'] = df.MODIFICATION_FLAG.replace(' ','N')
    
    return df


# # 9 ZERO BALANCE CODE
#   01 = Prepaid or Matured (Voluntary Payoff)
#   03 = Foreclosure Alternative Group (Short Sale, Third Party Sale, Charge Off or Note Sale)
#   06 = Repurchase prior to Property Disposition
#   09 = REO Disposition
#   Space(2) = Not (NA)
# Applicable

# In[199]:

def validate_zero_balance_code(df):
    if any(df['ZERO_BALANCE_CODE'].isnull()):
        df['ZERO_BALANCE_CODE'] = df.ZERO_BALANCE_CODE.fillna('NA')
        
    
    df['ZERO_BALANCE_CODE'] = df.ZERO_BALANCE_CODE.replace('  ','NA')
        
    #f any(df['ZERO_BALANCE_CODE']) != 1.0 or any(df['ZERO_BALANCE_CODE']) != 6.0 or any(df['ZERO_BALANCE_CODE']) != 3.0 or any(df['ZERO_BALANCE_CODE']) != 9.0:
    if 1.0 not in df['ZERO_BALANCE_CODE'] or 3.0 not in df['ZERO_BALANCE_CODE'] or 6.0 not in df['ZERO_BALANCE_CODE'] or 9.0 not in df['ZERO_BALANCE_CODE']:
        print("Not a 1 or 3 or 6 or 9 zero balance code")
    return df


# In[196]:

#hhh = validate_zero_balance_code(df)


# In[195]:

#hhh['ZERO_BALANCE_CODE'].unique()


# if any(df['ORIGINAL_COMBINED_LOAN_TO_VALUE_CLTV'].isnull()):
#         df['ORIGINAL_COMBINED_LOAN_TO_VALUE_CLTV'] = df.ORIGINAL_COMBINED_LOAN_TO_VALUE_CLTV.fillna(999)
#         print (sum(cltv_count['ORIGINAL_COMBINED_LOAN_TO_VALUE_CLTV'].count()))
#         return df

# # 10 ZERO BALANCE EFFECTIVE DATE
# 
# YYYYMM
#   Space(6) = Not
# Applicable
# 
# Assumption : replacing nan with 199701. since the year strats from 1999.
# nan = 199701

# In[143]:

def validate_zero_balance_effective_date(df):
    if any(df['ZERO_BALANCE_EFFECTIVE_DATE'].isnull()):
        df['ZERO_BALANCE_EFFECTIVE_DATE'] = df.ZERO_BALANCE_EFFECTIVE_DATE.fillna('199701')
    
    df['ZERO_BALANCE_EFFECTIVE_DATE'] = df.ZERO_BALANCE_CODE.replace('      ','000000')
    
    return df


# # 11 CURRENT INTEREST RATE
# 
# Reflects the current interest rate on the mortgage note, taking into account any loan modifications.
# 
# Numeric 
# Literal 
# Decimal

# In[144]:

def validate_current_interest_rate(df):
    if any(df['CURRENT_INTEREST_RATE'].isnull()):
        df['CURRENT_INTEREST_RATE'] = df.CURRENT_INTEREST_RATE.fillna('NA')
        #print(pd.value_counts(df['CURRENT_INTEREST_RATE'].values, sort=True).head())
    return df


# # 12 CURRENT DEFERRED UPB
# 
# The current non-interest bearing UPB of the modified mortgage.
# $ Amount. Non-Interest Bearing UPB.
# 
# The column has only zero as value.. validation?? 

# In[145]:

def validate_current_deferred_upb(df):
    return df


# # 13 DUE DATE OF LAST PAID INSTALLMENT (DDLPI)
# 
# YYYYMM
# 
# fill na : 199701

# In[204]:

def validate_ddpli(df):
    if any(df['DUE_DATE_OF_LAST_PAID_INSTALLMENT_DDLPI'].isnull()):
        df['DUE_DATE_OF_LAST_PAID_INSTALLMENT_DDLPI'] = df.DUE_DATE_OF_LAST_PAID_INSTALLMENT_DDLPI.fillna('199701')
        
    return df


# # 14 MI RECOVERIES

# In[147]:

def validate_MI_recoveries(df):
    if any(df['MI_RECOVERIES'].isnull()):
        df['MI_RECOVERIES'] = df.MI_RECOVERIES.fillna('0')
        
    return df


# # 15 NET SALES PROCEEDS
# 
# $ Amount. Gross Sale Proceeds – Allowable Selling Expenses.
# C = Covered U = Unknown
# 
# Alpha- numeric Literal Decimal

# In[148]:

def validate_net_sales_proceeds(df):
    if any(df['NET_SALES_PROCEEDS'].isnull()):
         df['NET_SALES_PROCEEDS'] = df.NET_SALES_PROCEEDS.fillna('0')
        
    return df


# # 16 NON MI RECOVERIES
# 

# In[149]:

def validate_non_MI_recoveries(df):
    if any(df['NON_MI_RECOVERIES'].isnull()):
        df['NON_MI_RECOVERIES']=df.NET_SALES_PROCEEDS.fillna('0')
    return df


# # 17 EXPENSES

# In[150]:

def validate_expenses(df):
    if any(df['EXPENSES'].isnull()):
        df['EXPENSES']=df.EXPENSES.fillna('NA')
    return df


# # 18 Legal Costs 

# In[151]:

def validate_legal_costs(df):
    if any(df['Legal_Costs'].isnull()):
        df['Legal_Costs']=df.Legal_Costs.fillna('NA')
    return df


# # 19 Maintenance and Preservation Costs

# In[152]:

def validate_maintenance_preservation_costs(df):
    if any(df['Maintenance_and_Preservation_Costs'].isnull()):
        df['Maintenance_and_Preservation_Costs']=df.Maintenance_and_Preservation_Costs.fillna('NA')
    return df


# 20 Taxes and Insurance Data

# In[153]:

def validate_taxes_and_insurance(df):
    if any(df['Taxes_and_Insurance'].isnull()):
        df['Taxes_and_Insurance']=df.Taxes_and_Insurance.fillna('NA')
    return df


# # 21 Miscellaneous Expenses

# In[154]:

def validate_miscellaneous_expenses(df):
    return df


# # 22 Actual Loss Calculation

# In[155]:

def validate_actual_loss_cal(df):
    if (df['REPURCHASE_FLAG'] == 'Y').values.any() or (df['NET_SALES_PROCEEDS']== 'C').values.any() or (df['NET_SALES_PROCEEDS']== 'U').values.any() :
#or any(perfor_data['NET_SALES_PROCEEDS']) == 'C' or any(perfor_data['NET_SALES_PROCEEDS']) == 'U':
        print('here')
        df['Actual_Loss_Calculation'] = df.Actual_Loss_Calculation.fillna(0)
    return df


# # 23 Modification Cost 
# 

# In[156]:

def validate_modification_cost(df):
    return df


# MAIN VALIDATION FUNCTIONS AND CODE

# In[205]:

for i in range(len(performance_files)):
    df = pd.DataFrame()
    print (performance_files[i])
    df = load_data(performance_files[i])
    
    df = name_column(df)
    
    #1)
    
    df = validate_monthly_report(df)
    df = validate_upb(df)
    df = loan_delinquency_status(df)
    df = validate_loan_age(df)
    df = validate_remaining_months_to_legal_maturity(df)
    df = validate_repurchase_flag(df)
    df = validation_modification_flag(df)
    df = validate_zero_balance_code(df)
    df = validate_zero_balance_effective_date(df)
    df = validate_current_interest_rate(df)
    df = validate_current_deferred_upb(df)
    df = validate_ddpli(df)
    df = validate_MI_recoveries(df)
    df = validate_net_sales_proceeds(df)
    df = validate_non_MI_recoveries(df)
    df = validate_expenses(df)
    df = validate_legal_costs(df)
    df = validate_maintenance_preservation_costs(df)
    df = validate_taxes_and_insurance(df)
    df = validate_miscellaneous_expenses(df)
    df = validate_actual_loss_cal(df)
    df = validate_modification_cost(df)
    
    df.to_csv('Sample_Performance_Validated_'+str(performance_files[i][12:-4])+'.csv', index=None)
    


# In[ ]:

print('Performance file validation done')

