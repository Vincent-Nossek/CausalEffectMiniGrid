clear all

cd "C:\Users\vinnosse\OneDrive - Université Clermont Auvergne\Documents\Chapter1\Econometrie\Input"


use FullPanel.dta
drop if ControlDum == 1
drop treated

*Generate Time variables
destring ControlDum, replace
mdesc StartDate if ControlDum == 0
gen Day = 28
destring Year Month, replace
gen Time = mdy(Month, Day, Year)
format Time %d
gen TimeMo = mofd(Time)
format TimeMo %tm


gen fill_st_date = StartDate if ControlDum == 0
tostring StartYear, replace
*Impute June 30 if missing exact month of launch
replace fill_st_date = "30.06."  + StartYear if StartDate=="" & ControlDum == 0
replace fill_st_date = subinstr(fill_st_date, "/", ".", .)

gen eStartDate = date(fill_st_date, "DMY")
format eStartDate %d
gen eStartMonth = mofd(eStartDate)
format eStartMonth %tm

*Generate Treated var
gen treated = (TimeMo == eStartMonth ) if ControlDum==0
tab treated
*if treated = 144 -> OK
drop treated

gen eStartYear = yofd(eStartDate)
drop if eStartYear == Year

gen beforeT = TimeMo < eStartMonth if ControlDum==0
gen deltaTmo = TimeMo - eStartMonth 
gen delta_yr = Year - eStartYear


egen UniqueID_num = group(UniqueID)
xtset UniqueID_num TimeMo

*************************************
*** Compute Monthly trend values ****
sum UniqueID_num
forvalues proj = 1/`r(max)' {
qui: reg NTL_viirs Year i.Month if UniqueID_num == `proj' & beforeT == 1 
predict LP`proj' if UniqueID_num == `proj'
predict STDP`proj' if UniqueID_num == `proj', stdp
 
gen beta_sign`proj' = 2*ttail(e(df_r),abs(_b[Year]/_se[Year])) if UniqueID_num == `proj'  
}

egen LP_consolid = rowmax(LP*)
egen STDP_consolid = rowmax(STDP*)
egen Beta_sign_consolid = rowmax(beta_sign*)
drop LP1-LP144 STDP1-STDP144 beta_sign1-beta_sign144

*Replace Trend prediction by pre-avg if trend is not significant
bysort UniqueID_num : egen before_avg = mean(NTL_viirs) if beforeT == 1
bysort UniqueID_num : egen before_avg_ID = mean(before_avg)
replace LP_consolid = before_avg_ID if Beta_sign_consolid > 0.1 & beforeT == 0

***********************
*** Compute ST-Dev ****
gen Dev = NTL_viirs - LP_consolid if delta_yr > 0 & delta_yr <= 3 

bysort UniqueID : egen ST_DEVIATION = mean(Dev)

gen LPS_sq = STDP_consolid^2
bysort UniqueID : egen sum_LPS_sq =  sum(LPS_sq) if delta_yr > 0 & delta_yr <= 3
bysort UniqueID : egen N_period = max(deltaTmo) if sum_LPS_sq != . 
bysort UniqueID : gen ST_STD = sqrt(sum_LPS_sq/N_period) if delta_yr > 0 & delta_yr <= 3
drop LPS_sq sum_LPS_sq 

drop Dev* 

*Compute Degrees of freedom
collapse (sum) beforeT (mean) ST_DEVIATION* ST_STD*  , by(id_projet Country ControlDum UniqueID)

****************************
*** Compute Significance ***
gen ddl = beforeT - 1
gen ST_STUDENT = ST_DEVIATION / ST_STD

gen ST_SIGN = t(ddl, ST_STUDENT)

gen SuccessTrend = 0
replace SuccessTrend = 1 if ST_SIGN >= 0.9 & ST_DEVIATION	>= 0


keep UniqueID id_projet ST_SIGN* SuccessTrend

save ProjectsSuccess.dta, replace


