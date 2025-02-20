  
*********************************************
*** Load Full panel and make yearly panel ***
clear
cd "C:\Users\vinnosse\OneDrive - Université Clermont Auvergne\Documents\Chapter1\Econometrie\Input"
use FullPanel.dta
drop treated


*Generate Time variables
gen Day = 28
destring Year Month, replace
gen Time = mdy(Month, Day, Year)
format Time %d

gen fill_st_date = StartDate if ControlDum == 0
tostring StartYear, replace
*Impute June 30 if missing exact month of launch
replace fill_st_date = "30.06."  + StartYear if StartDate=="" & ControlDum == 0
replace fill_st_date = subinstr(fill_st_date, "/", ".", .)

gen eStartDate = date(fill_st_date, "DMY")
format eStartDate %d

*Generate Treated var
gen treated = (Time >= eStartDate & Time<= eStartDate+30) if ControlDum==0
tab treated
*if treated = 144 -> OK

*Generate firt time treated
gen FT = Time if treated==1
bysort UniqueID: egen FirstTDate = max(FT)
format FirstTDate %d
replace FirstTDate=0 if FirstTDate==.
drop FT

sort Country ControlDum UniqueID Year Month

egen ID_num = group(UniqueID)


*Collapse data to yearly data
destring StartYear, replace
gen NTL_median = NTL_viirs
gen FirstYear = StartYear if ControlDum == 0
replace FirstYear = 0 if ControlDum== 1 & FirstYear == .

collapse (mean)  StartYear Surface Altitude Pop DistGrandeVille PV Gradient  NTL_viirs FirstYear Distance corrNTL rank* Grid_dist (max)Probably_Stopped Project_added_ABER Connected_to_GRID Local_Production  ///
 (median) NTL_median , by(Country ControlDum id_projet UniqueID ID_num Year Technology Ownershipmodel CapacityinMW )

save YearlyFullPanel.dta , replace


*************************
*** Yearly Estimation ***
clear
capture log close

cd "C:\Users\vinnosse\OneDrive - Université Clermont Auvergne\Documents\Chapter1\Econometrie\Input"
use YearlyFullPanel.dta , clear


egen CountryNum = group(Country)
egen id_cluster = group(id_projet)
egen UniqueIDnum = group(UniqueID)


foreach var of varlist Probably_Stopped Project_added_ABER Connected_to_GRID Local_Production{
bysort id_projet: egen c_`var' = max(`var')
}

gen treated = 0
replace treated = 1 if ControlDum == 0 & Year >= StartYear
gen delta_yr = Year - StartYear

drop Probably_Stopped Project_added_ABER Connected_to_GRID Local_Production


*Number of obs
foreach x in 1 3 5 10 {
tab ControlDum Country if rank_dm <= `x'
}

*Number of treated and controls
foreach x in 1 3 5 10 {
tab ControlDum Country if rank_dm <= `x' & delta_yr == 0
}

*NTL mean
foreach x in 1 3 5 10 {
sum NTL_viirs if rank_dm <= `x'
}

*R squared from TWFE
xtset UniqueIDnum Year
foreach x in 1 3 5 10 {
quietly: xtreg NTL_viirs treated i.Year if rank_dm <= `x' & delta_yr < 4, fe vce(cluster UniqueIDnum) 
display  e(r2_o)
}

cd "C:\Users\vinnosse\OneDrive - Université Clermont Auvergne\Documents\Chapter1\Econometrie\OutputLimitedT"
log using EstimationLog, replace text 


*** Estimation w/ Mahalanobis Dist.
/*
foreach x in 1 3 5 10  {
preserve
keep if rank_dm <= `x'

did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if CountryNum==1, effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal save_results(dcm_BFA_DM`x')  graph_off

scalar t_stat = e(Av_tot_effect)/e(se_avg_total_effect)
scalar p_val = 2*normal(-abs(t_stat))
di t_stat, p_val
scalar t_stat = e(Effect_1)/e(se_effect_1)
scalar p_val = 2*normal(-abs(t_stat))
di t_stat, p_val
scalar t_stat = e(Effect_2)/e(se_effect_2)
scalar p_val = 2*normal(-abs(t_stat))
di t_stat, p_val
scalar t_stat = e(Effect_3)/e(se_effect_3)
scalar p_val = 2*normal(-abs(t_stat))
di t_stat, p_val
scalar t_stat = e(Effect_4)/e(se_effect_4)
scalar p_val = 2*normal(-abs(t_stat))
di t_stat, p_val

did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if CountryNum==2, effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal save_results(dcm_MDG_DM`x')  graph_off

scalar t_stat = e(Av_tot_effect)/e(se_avg_total_effect)
scalar p_val = 2*normal(-abs(t_stat))
di t_stat, p_val
scalar t_stat = e(Effect_1)/e(se_effect_1)
scalar p_val = 2*normal(-abs(t_stat))
di t_stat, p_val
scalar t_stat = e(Effect_2)/e(se_effect_2)
scalar p_val = 2*normal(-abs(t_stat))
di t_stat, p_val
scalar t_stat = e(Effect_3)/e(se_effect_3)
scalar p_val = 2*normal(-abs(t_stat))
di t_stat, p_val
scalar t_stat = e(Effect_4)/e(se_effect_4)
scalar p_val = 2*normal(-abs(t_stat))
di t_stat, p_val

did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated , controls(CountryNum) effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal save_results(dcm_ALL_DM`x')  graph_off

scalar t_stat = e(Av_tot_effect)/e(se_avg_total_effect)
scalar p_val = 2*normal(-abs(t_stat))
di t_stat, p_val
scalar t_stat = e(Effect_1)/e(se_effect_1)
scalar p_val = 2*normal(-abs(t_stat))
di t_stat, p_val
scalar t_stat = e(Effect_2)/e(se_effect_2)
scalar p_val = 2*normal(-abs(t_stat))
di t_stat, p_val
scalar t_stat = e(Effect_3)/e(se_effect_3)
scalar p_val = 2*normal(-abs(t_stat))
di t_stat, p_val
scalar t_stat = e(Effect_4)/e(se_effect_4)
scalar p_val = 2*normal(-abs(t_stat))
di t_stat, p_val

restore
}

*/

*Table all
did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if rank_dm <= 1, controls(CountryNum) effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal graph_off
outreg2 using dcmALL.xls, replace ctitle(MD1)
foreach x in 3 5 10  {
did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if rank_dm <= `x', controls(CountryNum) effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal graph_off
outreg2 using dcmALL.xls, append ctitle(MD`x') 
}

*Graph
did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if rank_dm <= 3, controls(CountryNum) effects(4) placebo(4) cluster(UniqueIDnum)

*** By countries
*NTL mean
foreach x in 3 5 {
sum NTL_viirs if rank_dm <= `x' & Country == "Burkina Faso"
}
foreach x in 3 5 {
sum NTL_viirs if rank_dm <= `x' & Country == "Madagascar"
}
*R squared from TWFE
xtset UniqueIDnum Year
foreach x in 3 5 {
quietly: xtreg NTL_viirs treated i.Year if rank_dm <= `x' & delta_yr < 4 & Country == "Burkina Faso", fe vce(cluster UniqueIDnum) 
display  e(r2_o)
}

foreach x in 3 5 {
quietly: xtreg NTL_viirs treated i.Year if rank_dm <= `x' & delta_yr < 4 & Country == "Madagascar", fe vce(cluster UniqueIDnum) 
display  e(r2_o)
}

*DiD tables by countries
did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if CountryNum==1 & rank_dm <= 3, effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal graph_off
outreg2 using dcmCountry.xls, replace ctitle(BFA MD3)

did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if CountryNum==1 & rank_dm <= 5, effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal graph_off
outreg2 using dcmCountry.xls, append ctitle(BFA MD5) 

did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if CountryNum==2 & rank_dm <= 3, effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal graph_off
outreg2 using dcmCountry.xls, append ctitle(MDG MD3)

did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if CountryNum==2 & rank_dm <= 5, effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal graph_off
outreg2 using dcmCountry.xls, append ctitle(MDG MD5) 

capture log close






*********************************************
*** Comapre successful to failed projects ***
clear
capture log close
cd "C:\Users\vinnosse\OneDrive - Université Clermont Auvergne\Documents\Chapter1\Econometrie\Input"
use YearlyFullPanel.dta , clear

merge m:1 id_projet using ProjectsSuccess.dta
drop _merge

*graph box ST_SIGN if ControlDum == 0 & Country == "Burkina Faso", over(Project_added_ABER)


tab Country if ControlDum == 0 & Year == 2013
tab StartYear

egen CountryNum = group(Country)
egen id_cluster = group(id_projet)
egen UniqueIDnum = group(UniqueID)

gen treated = 0
replace treated = 1 if ControlDum == 0 & Year >= StartYear

gen delta_yr = Year - StartYear


cd "C:\Users\vinnosse\OneDrive - Université Clermont Auvergne\Documents\Chapter1\Econometrie\OutputSuccessOnly"


*** Regression with Success defined by linear trend test ***
tab SuccessTrend Country if delta_yr == 0 & ControlDum ==0

foreach x in 1 3 5 10 {
tab SuccessTrend ControlDum if rank_dm <= `x' 
}

*Number of treated and controls
tab ControlDum SuccessTrend if rank_dm <= 3 & delta_yr == 0
*Mean NTL 
sum NTL_viirs if rank_dm <= 3 & SuccessTrend == 1
sum NTL_viirs if rank_dm <= 3 & SuccessTrend == 0
*R squared from TWFE
xtset UniqueIDnum Year
quietly: xtreg NTL_viirs treated i.Year if rank_dm <= 3 & delta_yr < 4 & SuccessTrend==1, fe vce(cluster UniqueIDnum) 
display  e(r2_o)
quietly: xtreg NTL_viirs treated i.Year if rank_dm <= 3 & delta_yr < 4 & SuccessTrend==0, fe vce(cluster UniqueIDnum) 
display  e(r2_o)

*Table Success and failed
did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if SuccessTrend==1 & rank_dm <= 3 , controls(CountryNum) effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal graph_off
outreg2 using dcm_SuccessAndFail.xls, replace ctitle(Successful)

did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if SuccessTrend==0 & rank_dm <= 3 , controls(CountryNum) effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal graph_off
outreg2 using dcm_SuccessAndFail.xls, append ctitle(Failed)




*** Regression by technology ***
gen Tek = 0 
replace Tek = 1 if Technology == "Solar PV" | Technology == "Hybrid Solar PV / Diesel"
bysort id_projet: egen TekSolar = max(Tek)
drop Tek

table Country TekSolar if delta_yr ==0

tab TekSolar Country if rank_dm <= 3 

gen Tek = 0
replace Tek = 1 if Technology == "Hydro"
bysort id_projet: egen TekHydro = max(Tek)
drop Tek

table Country TekHydro if delta_yr ==0
tab TekHydro Country if rank_dm <= 3 

*Number of treated and controls
tab ControlDum  if delta_yr == 0 & rank_dm <= 3 & TekSolar == 1 
tab ControlDum  if delta_yr == 0 & rank_dm <= 3 & TekHydro == 1
tab ControlDum  if delta_yr == 0 & rank_dm <= 3 & TekHydro == 1 & CountryNum==2
*Mean of NTL
sum NTL_viirs if delta_yr == 0 & rank_dm <= 3 & TekSolar == 1 
sum NTL_viirs if delta_yr == 0 & rank_dm <= 3 & TekHydro == 1
sum NTL_viirs if delta_yr == 0 & rank_dm <= 3 & TekHydro == 1 & CountryNum==2
*R squared from TWFE
xtset UniqueIDnum Year
quietly: xtreg NTL_viirs treated i.Year if rank_dm <= 3 & TekSolar == 1 & delta_yr < 4 , fe vce(cluster UniqueIDnum) 
display  e(r2_o)
quietly: xtreg NTL_viirs treated i.Year if rank_dm <= 3 & TekHydro == 1  & delta_yr < 4 , fe vce(cluster UniqueIDnum) 
display  e(r2_o)
quietly: xtreg NTL_viirs treated i.Year if rank_dm <= 3 & CountryNum==2 & TekHydro == 1 & delta_yr < 4 , fe vce(cluster UniqueIDnum) 
display  e(r2_o)


did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if rank_dm <= 3 & TekSolar == 1, controls(CountryNum) effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal graph_off
outreg2 using dcm_tableTek2.xls, replace ctitle("Solar powered")
did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if rank_dm <= 3 & TekHydro == 1, controls(CountryNum) effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal graph_off
outreg2 using dcm_tableTek2.xls, append ctitle("Hydro powered")
did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if rank_dm <= 3 & CountryNum==2 & TekHydro == 1, effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal graph_off
outreg2 using dcm_tableTek2.xls, append ctitle("Hydro powered Madagascar only")



*** Regression by power level and local production unit ***

replace CapacityinMW = subinstr(CapacityinMW, ",", ".",.) 
destring CapacityinMW, replace

gen PowerLvl = 0 if Country == "Madagascar" & CapacityinMW != .
sum CapacityinMW if Country == "Madagascar", detail
bysort id_projet: replace PowerLvl = 1 if CapacityinMW >= r(p50) & Country == "Madagascar" & CapacityinMW != .
bysort id_projet: egen PowerLvlc = max(PowerLvl)
drop PowerLvl

replace Connected_to_GRID = 0 if Country == "Madagascar"
table Country Connected_to_GRID if delta_yr == 0

table Connected_to_GRID Country if rank_dm <= 3 & Country=="Burkina Faso"

tab PowerLvlc Country if rank_dm <= 3 & Country == "Madagascar"

replace Local_Production = 1 if Country == "Madagascar"
 table Country Local_Production if delta_yr ==0
 tab Local_Production Country if rank_dm <= 3 
 
bysort id_projet:  egen c_Connected_to_GRID = max(Connected_to_GRID)
bysort id_projet:  egen c_Local_Production = max(Local_Production)
 
*Number of treated and controls
tab ControlDum if rank_dm <= 3 & delta_yr == 0 & PowerLvlc == 1 & CountryNum==2
tab ControlDum if rank_dm <= 3 & delta_yr == 0 & PowerLvlc == 0 & CountryNum==2
tab ControlDum if rank_dm <= 3 & delta_yr == 0 & c_Connected_to_GRID == 1 & CountryNum==1
tab ControlDum if rank_dm <= 3 & delta_yr == 0 & c_Local_Production == 0 & CountryNum==1
*Mean NTL 
sum NTL_viirs if rank_dm <= 3 &  PowerLvlc == 1 & CountryNum==2
sum NTL_viirs if rank_dm <= 3 &  PowerLvlc == 0 & CountryNum==2
sum NTL_viirs if rank_dm <= 3 & c_Connected_to_GRID == 1  & CountryNum==1
sum NTL_viirs if rank_dm <= 3 & c_Local_Production == 0  & CountryNum==1
*R squared from TWFE
xtset UniqueIDnum Year
quietly: xtreg NTL_viirs treated i.Year if CountryNum==2 & rank_dm <= 3 & PowerLvlc == 1 & delta_yr < 4 , fe vce(cluster UniqueIDnum) 
display  e(r2_o)
quietly: xtreg NTL_viirs treated i.Year if CountryNum==2 & rank_dm <= 3 & PowerLvlc == 0 & delta_yr < 4 , fe vce(cluster UniqueIDnum) 
display  e(r2_o)
quietly: xtreg NTL_viirs treated i.Year if CountryNum==1 & rank_dm <= 3 & c_Connected_to_GRID == 1 & delta_yr < 4 , fe vce(cluster UniqueIDnum) 
display  e(r2_o)
quietly: xtreg NTL_viirs treated i.Year if CountryNum==1 & rank_dm <= 3 & c_Local_Production == 0 & delta_yr < 4 , fe vce(cluster UniqueIDnum) 
display  e(r2_o)


did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if CountryNum==2 & rank_dm <= 3 & PowerLvlc == 1, effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal  graph_off
outreg2 using dcm_tableTek1.xls, replace ctitle("High Power")
did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if CountryNum==2 & rank_dm <= 3 & PowerLvlc == 0, effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal  graph_off
outreg2 using dcm_tableTek1.xls, append ctitle("Low Power")
did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if CountryNum==1 & rank_dm <= 3 & c_Connected_to_GRID == 1, effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal graph_off
outreg2 using dcm_tableTek1.xls, append ctitle("Connected to grid")
 did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if CountryNum==1 & rank_dm <= 3 & c_Local_Production == 0, effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal graph_off
outreg2 using dcm_tableTek1.xls, append ctitle("No local Production")
 

 
 
capture log close
