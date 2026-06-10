//Create graphs
use "UNFCCC_total_GHG.dta", replace
//ETS sectors
twoway ///
  (line Emissions_pct year if regexm(sector, "1.A.1.a") & country=="EU",lpattern(solid)) ///   lcolor(blue)
  (line Emissions_pct year if regexm(sector, "1.A.1.b") & country=="EU",lpattern(solid)) ///   lcolor(green) ) ///
  (line Emissions_pct year if regexm(sector, "1.A.2.a") & country=="EU",lpattern(solid)) ///   lcolor(red) ) ///
  (line Emissions_pct year if regexm(sector, "1.A.2.b") & country=="EU",lpattern(solid)) ///  
  (line Emissions_pct year if regexm(sector, "1.A.2.c") & country=="EU",lpattern(dash)) ///   
  (line Emissions_pct year if regexm(sector, "1.A.2.d") & country=="EU",lpattern(dash)) ///   
  (line Emissions_pct year if regexm(sector, "1.A.2.f") & country=="EU",lpattern(dash)) ///   (vline 2004, lcolor(gs8)) ///
  , ///
  xline(2004, lcolor(gs8)) ///
  legend(label(1 "Electricity") label(2 "Refining") label(3 "Iron & Steel") label(4 "Non-ferrous metals") label(5 "Chemicals") label(6 "Paper") label(7 "Cement, lime, glass, ceramics")) ///
  ytitle("Emissions") ///
  xlabel(2004 "2004" 1990(10)2020) ///
  xtitle("Year") ///
  graphregion(color(white)) ///
  bgcolor(white)
  graph export "ETSsectors_nonProcess.png", replace
 

//ETS vs non-ETS
twoway ///
  (line Emissions_pct year if sector=="ETS" & country=="EU",lpattern(solid)) ///   
  (line Emissions_pct year if sector=="ETS_EEA" & country=="EU27",lpattern(dash) lwidth(thick)) ///
  (line Emissions_pct year if sector=="nonETS" & country=="EU",lpattern(solid)) ///
  , ///
  xline(2004, lcolor(gs8)) ///
  xlabel(2004 "2004" 1990(10)2020) ///
  legend(label(1 "ETS proxy") label(2 "ETS") label(3 "non-ETS") ) ///
  ytitle("Emissions") ///
  xtitle("Year") ///
  graphregion(color(white)) ///
  bgcolor(white)
  graph export "ETS_nonETS.png", replace
//ETS vs non-ETS for detailed sectors.  
twoway ///
  (line Emissions_pct year if sector=="ETS" & country=="EU",lpattern(solid) lwidth(thick)) ///   
  (line Emissions_pct year if regexm(sector, "^1.A.3 ") & country=="EU",lpattern(dash)) ///
  (line Emissions_pct year if regexm(sector, "^1.A.4.b ") & country=="EU",lpattern(solid)) ///
  (line Emissions_pct year if regexm(sector, "^1.A.4.c ") & country=="EU",lpattern(solid)) ///
  (line Emissions_pct year if regexm(sector, "^1.B ") & country=="EU",lpattern(dash)) ///
  (line Emissions_pct year if regexm(sector, "^3. ") & country=="EU",lpattern(solid)) ///   //(line Emissions_pct year if regexm(sector, "^2.E ") & country=="EU",lpattern(solid)) ///  add:2.E Electronics, 2.D non-energy from fuels and solvents, 2.G other (2.F sustbitutes from Ozon depleting substances starts at zero in 1990)
  , ///
  xline(2004, lcolor(gs8)) ///
  legend(label(1 "ETS") label(2 "Transport") label(3 "Residential") label(4 "Agriculture,forestry,fishing energy") label(5 "Fugitive from fuels") label(6 "Agriculture non-energy") ) ///  //label(7 "Electronics")
  xlabel(2004 "2004" 1990(10)2020) ///
  ytitle("Emissions") ///
  xtitle("Year") ///
  graphregion(color(white)) ///
  bgcolor(white)
  graph export "ETS_nonETSsectors.png", replace
  
//ETS sectors in EU and other countries
twoway ///
  (line Emissions_pct year if sector=="ETS" & country=="EU",lpattern(solid) lwidth(thick)) ///
  (line Emissions_pct year if sector=="ETS" & country=="United States of America",lpattern(solid)) ///
  (line Emissions_pct year if sector=="ETS" & country=="Japan",lpattern(solid) ) ///
  (line Emissions_pct year if sector=="ETS" & country=="Australia",lpattern(solid)) ///
  (line Emissions_pct year if sector=="ETS" & country=="New Zealand",lpattern(solid)) ///
  , ///
  xline(2004, lcolor(gs8)) ///
  legend(label(1 "ETS EU") label(2 "USA")  label(3 "Japan") label(4 "Australia") label(5 "New Zealand")) ///
  ytitle("Emissions") ///
  xtitle("Year") ///
  xlabel(2004 "2004" 1990(10)2020) ///
  graphregion(color(white)) ///
  bgcolor(white)
  graph export "ETS_EU_OtherCountries.png", replace