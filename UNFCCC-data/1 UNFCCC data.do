cd "C:\Users\VENMANSF\OneDrive - London School of Economics\Research projects\EU ETS emissions\"
//import EEA emissions data
//check if there is a separate file for UK data
import delimited using "EEAemissions.csv", varnames(1) clear
replace v6 = subinstr(v6, " ", "", .) // remove all spaces
destring v6, replace force
rename v6 Emissions
//replace Emissions=Emissions/1000
rename etsinformation sector
replace sector="ETS_VerifiedEmissions" if sector=="2. Verified emissions"
replace sector="ETS_EEAcorrection" if sector=="3. Estimate to reflect current ETS scope for allowances and emissions"
keep year sector country Emissions
//add up verified emissions and correction term
preserve
collapse (sum) Emissions*, by(year country) //take out "country" argument to add up EU27 and UK, keep if we use country by country data. 
gen sector = "ETS_EEA"
tempfile Temp
save `Temp'
restore
append using `Temp'
save "EEAemissions", replace

//import which UNFCCC sector names are ETS. 
//ETSsectors indicates 1 of ETS, -1 for non-ETS, 0 for mixed. It is created manually
import delimited using "ETSsectors.csv", varnames(1) clear
save "ETSsectors.dta", replace

//import download from UNFCCC website
//di.unfccc.int/flex_annex1 => Flexible queries => Annex1 excluding EIT, CO2eq; all years.
import delimited "UNFCCC_total_GHG.csv", varnames(1) clear
drop if country==""
//merge with ETS indicator
merge m:1 sector using "ETSsectors"
drop if _merge==1 //observations which were comments in csv file
drop _merge


reshape long v, i(country sector) j(year)
destring v, replace force
drop if v==.
rename v Emissions
replace year=year+1987
 
//append eea emission data
append using "EEAemissions"

//declare panel
gen country_sector=country+"_"+sector
egen id=group(country_sector)
xtset id year

//create a variable with level of sectors
gen justdots = ustrregexra(sector,"[^.]","")
gen sector_level = ustrlen(justdots)
drop justdots


tab country
replace country="UK" if regexm(country,"^United Kingdom of Great Britain and N")
replace country="EU" if regexm(country,"^European Union")

//create EU28 indicator
gen EU28 = country=="Austria"| country=="Belgium"| country=="Bulgaria"| country=="Croatia"| country=="Cyprus"| country=="Czechia"| country=="Denmark"| country=="Estonia"| country=="Finland"| country=="France"| country=="Germany"| country=="Greece"| country=="Hungary"| country=="Ireland"| country=="Italy"| country=="Latvia"| country=="Lithuania"| country=="Luxembourg"| country=="Malta"| country=="Netherlands"| country=="Poland"| country=="Portugal"| country=="Romania"| country=="Slovakia"| country=="Slovenia"| country=="Spain"| country=="Sweden"| country=="UK"

gen EU27 = country=="Austria"| country=="Belgium"| country=="Bulgaria"| country=="Croatia"| country=="Cyprus"| country=="Czechia"| country=="Denmark"| country=="Estonia"| country=="Finland"| country=="France"| country=="Germany"| country=="Greece"| country=="Hungary"| country=="Ireland"| country=="Italy"| country=="Latvia"| country=="Lithuania"| country=="Luxembourg"| country=="Malta"| country=="Netherlands"| country=="Poland"| country=="Portugal"| country=="Romania"| country=="Slovakia"| country=="Slovenia"| country=="Spain"| country=="Sweden"


// make a few merged sectors 
//EU27 and EU28
foreach i in EU27 EU28 {
	preserve
	keep if `i'==1 
	collapse (sum) Emissions*, by(sector year) //sum emissions by sector and year 
	gen country = "`i'"
	tempfile Temp
	save `Temp'
	restore
	append using `Temp'
}


//all ETS sectors
preserve
keep if ets==1 
collapse (sum) Emissions*, by(country year) //sum emissions by country and year 
gen sector = "ETS"
tempfile Temp
save `Temp'
restore
append using `Temp'

//all nonETS sectors
preserve
keep if ets==-1 
collapse (sum) Emissions*, by(country year) 
gen sector = "nonETS"
tempfile Temp
save `Temp'
restore
append using `Temp'

//energy and nonenergy from Iron&steel 
preserve
keep if regexm(sector, "1.A.2.a") | regexm(sector, "2.C.1") 
collapse (sum) Emissions*, by(country year) 
tempfile ironsteel
save `ironsteel'
restore
append using `ironsteel'

//Alternative: but not easier.
/*expand 2 if regexm(sector, "1.A.2.a"), gen(duplicate)
replace sector = "Iron_Steel" if duplicate==1
bys country year: gen emissionTemp1=emission if regexm(sector, "1.A.2.a") | regexm(sector, "2.C.1")
by country year: egen emissionTemp2 = total(emissionTemp1) //treats missing as zero
//or: by country year: egen emissionTemp2 = total(cond(regexm(sector, "1.A.2.a") | regexm(sector, "2.C.1"),emission,.))
replace emissions=emissionTemp2 if sector == "Iron_Steel"
*/


//Create emissions relative to 2004
gen Emissions2004 = .
bysort country sector (year): replace Emissions2004 = Emissions if year == 2004 | year==2005 & sector=="ETS_EEA"
bysort country sector (year): egen Emissions2004temp=min(Emissions2004)
gen Emissions_pct=Emissions/Emissions2004temp*100
drop Emissions2004*


save "UNFCCC_total_GHG.dta", replace

