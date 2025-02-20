
**** Estimation de Chaisemartin and D’Haultfœuille *****


*************************
*** Yearly Estimation ***
clear
set matsize 2000
cd "C:\Users\vinnosse\OneDrive - Université Clermont Auvergne\Documents\Chapter1\Econometrie\Input"
use YearlyFullPanel.dta , clear

capture log close

egen CountryNum = group(Country)
egen id_cluster = group(id_projet)
egen UniqueIDnum = group(UniqueID)

gen treated = 0
replace treated = 1 if ControlDum == 0 & Year >= StartYear

gen delta_yr = Year - StartYear

cd"C:\Users\vinnosse\OneDrive - Université Clermont Auvergne\Documents\Chapter1\Econometrie\Robustness"

*R squared from TWFE
xtset UniqueIDnum Year
foreach x in 1 3 5 10 {
quietly: xtreg NTL_viirs treated i.Year if rank_dm <= `x' , fe vce(cluster UniqueIDnum) 
display  e(r2_o)
}

*Table all periods
did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if rank_dm <= 1, controls(CountryNum) effects(6) placebo(4) cluster(UniqueIDnum)  effects_equal graph_off
outreg2 using dcmALL_6p.xls, replace ctitle(MD1)
foreach x in 3 5 10  {
did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if rank_dm <= `x', controls(CountryNum) effects(6) placebo(4) cluster(UniqueIDnum)  effects_equal graph_off
outreg2 using dcmALL_6p.xls, append ctitle(MD`x') 
}

xtset UniqueIDnum Year
xtreg NTL_viirs  i.Year treated if rank_dm <= 3, fe



xtset UniqueIDnum Year
foreach x in 1 3 5 10  {
if `x' == 1 {
xtreg NTL_viirs  i.Year treated if rank_dm <= `x', fe
outreg2 using TWFE_DMx.doc, replace ctitle(MD`x') keep(treated) title("All projects, TWFE estimates")
}
else{
xtreg NTL_viirs  i.Year treated if rank_dm <= `x', fe
outreg2 using TWFE_DMx.doc, append ctitle(MD`x') keep(treated)
}

}


*** Estimation w/ Euclidean Dist.
*Number of treated and controls
foreach x in 1 3 5 10 {
tab ControlDum Country if rank_de <= `x' & delta_yr == 0
}

*R squared from TWFE
xtset UniqueIDnum Year
foreach x in 1 3 5 10 {
quietly: xtreg NTL_viirs treated i.Year if rank_de <= `x' & delta_yr < 4, fe vce(cluster UniqueIDnum) 
display  e(r2_o)
}

did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if rank_de <= 1 , controls(CountryNum) effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal  graph_off
outreg2 using dcmALL_ED.xls, replace ctitle(ED1)

foreach x in 3 5 10 {
did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if rank_de <= `x' , controls(CountryNum) effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal graph_off
outreg2 using dcmALL_ED.xls, append ctitle(ED`x')
}





*** Estimation w/ Geographic distance
replace rank_GeoDist = 0 if rank_GeoDist == . & ControlDum == 0
foreach x in 1 3 5 10 {
preserve
keep if rank_GeoDist <= `x'

did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if CountryNum==1, effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal save_results(dcm_BFA_GD`x')  graph_off

did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if CountryNum==2, effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal save_results(dcm_MDG_GD`x')  graph_off

did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated , controls(CountryNum) effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal save_results(dcm_ALL_GD`x')  graph_off

restore
}


*** Estimation w/ NTL in log 
gen logNTL = log(NTL_viirs + 0.01)

*NTL mean
foreach x in 1 3 5 10 {
sum logNTL if rank_dm <= `x'
}

*R squared from TWFE
xtset UniqueIDnum Year
foreach x in 1 3 5 10 {
quietly: xtreg logNTL treated i.Year if rank_dm <= `x' & delta_yr < 4, fe vce(cluster UniqueIDnum) 
display  e(r2_o)
}

did_multiplegt_dyn logNTL UniqueIDnum Year treated if rank_dm <= 1 ,  effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal  graph_off
outreg2 using dcmALL_logNTL.xls, replace ctitle(MD1)

foreach x in 3 5 10 {
did_multiplegt_dyn logNTL UniqueIDnum Year treated if rank_dm <= `x' , effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal graph_off
outreg2 using dcmALL_logNTL.xls, append ctitle(MD`x')
}



*** Regular TWFE estimation
 
xtset UniqueIDnum Year
*Post Average
xtreg NTL_viirs treated i.Year if rank_dm <= 1 & delta_yr < 4, fe vce(cluster UniqueIDnum) 
outreg2 using Post_avg_TWFE_ALL_MD.xls, replace ctitle(MD1) keep(treated) 
foreach x in 3 5 10 {
xtreg NTL_viirs treated i.Year if rank_dm <= `x' & delta_yr < 4, fe vce(cluster UniqueIDnum) 
outreg2 using Post_avg_TWFE_ALL_MD.xls, append ctitle(MD`x') keep(treated) 
}

*Event study
preserve
drop if delta_yr < 0 | delta_yr > 4
xtreg NTL_viirs i.Year treated##i.delta_yr if rank_dm <= 1 & delta_yr>=0, fe vce(cluster UniqueIDnum)
outreg2 using TWFE_ALL_MD.xls, replace ctitle(MD1) keep(treated#delta_yr)
foreach x in 3 5 10 {
xtreg NTL_viirs i.Year treated##i.delta_yr if rank_dm <= `x' & delta_yr>=0, fe vce(cluster UniqueIDnum)
outreg2 using TWFE_ALL_MD.xls, append ctitle(MD`x') keep(treated#delta_yr)	
}
restore

*Robust to heterogeneity from stata
/*
quietly: xthdidregress ra (NTL_viirs) (treated) if rank_dm <= 1, group(UniqueIDnum) vce(cluster UniqueIDnum)
estat aggregation, dynamic (-5/3) 
eventbaseline, pre(5) post(3) baseline(-1) 
outreg2 using TWFE_ALL_MD.xls, replace ctitle(MD1)
foreach x in 3 5 10 {
xthdidregress ra (NTL_viirs) (treated) if rank_dm <= `x', group(UniqueIDnum) vce(cluster UniqueIDnum)
eventbaseline, pre(5) post(3) baseline(-1) 
outreg2 using TWFE_ALL_MD.xls, append ctitle(MD`x')
}
*/



*** Country-year interaction
egen ctry_yr = group(CountryNum Year)

xtreg NTL_viirs treated i.Year i.ctry_yr if rank_dm <= 1 & delta_yr < 4,fe  vce(cluster UniqueIDnum) 
outreg2 using Post_avg_interact_MD.xls, replace ctitle(MD1) keep(treated) addtext(Country Year interaction, YES)
foreach x in 3 5 10 {
xtreg NTL_viirs treated i.Year i.ctry_yr  if rank_dm <= `x' & delta_yr < 4, fe vce(cluster UniqueIDnum) 
outreg2 using Post_avg_interact_MD.xls, append ctitle(MD`x') keep(treated) addtext(Country Year interaction, YES)
}

preserve
drop if delta_yr < 0 | delta_yr > 4
xtreg NTL_viirs i.Year i.ctry_yr treated##i.delta_yr if rank_dm <= 1 & delta_yr>=0, fe vce(cluster UniqueIDnum)
outreg2 using TWFE_interacrt_MD.xls, replace ctitle(MD1) keep(treated#delta_yr) addtext(Country Year interaction, YES)
foreach x in 3 5 10 {
xtreg NTL_viirs i.Year i.ctry_yr treated##i.delta_yr if rank_dm <= `x' & delta_yr>=0, fe vce(cluster UniqueIDnum)
outreg2 using TWFE_interacrt_MD.xls, append ctitle(MD`x') keep(treated#delta_yr) addtext(Country Year interaction, YES)
}
restore


*******************************************
*** Robustness with Raw NTL (<0 values) ***


clear
cd "C:\Users\vinnosse\OneDrive - Université Clermont Auvergne\Documents\Chapter1\Econometrie\Input"
use ProjectsBFA_panel.dta
append using ProjectsMada_panel.dta, force
append using ControlBFA_panel.dta, force
append using ControlMada_panel.dta, force

*** Generate unique ID
*desc
tostring id_village, replace
tostring ControlDum, replace
gen UniqueID = id_projet +"_" + ControlDum + "_" + id_village
destring ControlDum, replace


*** Remove project near Industrial Mine
drop if id_projet == "BFAproj112" 

*** NTL correction outlyer points
replace NTL_viirs = 0 if id_projet == "2" & ControlDum==0 & Month == "09" & Year == "2016"
replace NTL_viirs = 12.62 if id_projet == "82" & ControlDum==0 & Month == "09" & Year == "2017"

*Projets 12 6 60 67 79 99 112 -> NTL w/ outlier values replaced by value of previous month
replace NTL_viirs = 0.61 if id_projet == "BFAproj12" & ControlDum==0 & Month == "12" & Year == "2015"
replace NTL_viirs = 1.72 if id_projet == "BFAproj6"  & ControlDum==0 & Month == "11" & Year == "2013"
replace NTL_viirs = 0.82 if id_projet == "BFAproj6"  & ControlDum==0 & Month == "02" & Year == "2015"
replace NTL_viirs = 1.59 if id_projet == "BFAproj6"  & ControlDum==0 & Month == "12" & Year == "2015"
replace NTL_viirs = 0.15 if id_projet == "BFAproj60" & ControlDum==0 & Month == "11" & Year == "2013"
replace NTL_viirs = 0    if id_projet == "BFAproj67" & ControlDum==0 & Month == "12" & Year == "2014"
replace NTL_viirs = 1.8  if id_projet == "BFAproj79" & ControlDum==0 & Month == "03" & Year == "2016"
replace NTL_viirs = 3.38 if id_projet == "BFAproj99" & ControlDum==0 & Month == "11" & Year == "2017"
*replace NTL_viirs = 30   if id_projet == "BFAproj112" & ControlDum==0 & Month == "04" & Year == "2020"

replace NTL_viirs = NTL_viirs[_n-1] if ControlDum == 0 & Month == "07" & Year == "2022"

*Compute mean NTL
rename NTL_viirs NTLsum
gen pix = round(Surface/(400*400))
gen NTL_viirs = NTLsum/pix


*** Merge ranking w/ Unique ID on control to filter before diff in diff.
merge m:1 UniqueID using Cross_rankings.dta 
keep if _merge == 3
drop _merge

sort Country ControlDum UniqueID Year Month 

collapse (mean) StartYear Surface Altitude Pop DistGrandeVille PV Gradient  NTL_viirs Distance rank* Grid_dist, by(Country ControlDum id_projet UniqueID Year Technology Ownershipmodel CapacityinMW )
destring Year, replace
cd"C:\Users\vinnosse\OneDrive - Université Clermont Auvergne\Documents\Chapter1\Econometrie\Robustness"

save YealryPanel_rawNTL.dta, replace

use YealryPanel_rawNTL.dta, clear


gen FirstYear = StartYear if ControlDum == 0
replace FirstYear = 0 if ControlDum== 1 & FirstYear == .



egen CountryNum = group(Country)
egen id_cluster = group(id_projet)
egen UniqueIDnum = group(UniqueID)


gen treated = 0
replace treated = 1 if ControlDum == 0 & Year >= StartYear
gen delta_yr = Year - StartYear


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


*Table raw NTL
did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if rank_dm <= 1, controls(CountryNum) effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal graph_off
outreg2 using dcmRawNTL.xls, replace ctitle(MD1)
foreach x in 3 5 10  {
did_multiplegt_dyn NTL_viirs UniqueIDnum Year treated if rank_dm <= `x', controls(CountryNum) effects(4) placebo(4) cluster(UniqueIDnum)  effects_equal graph_off
outreg2 using dcmRawNTL.xls, append ctitle(MD`x') 
}



