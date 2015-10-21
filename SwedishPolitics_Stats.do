
* Swedish trade flows, 1960-2000
********************************

* Source of data: OECD.Stat `Monthly Statistics of International Trade' (MSIT)

cd "${no}/Uni/PhD Lund/Teaching/Swedish Politics"

* Imports
import excel using "SE_Tradeflows_OECD.Stat.xlsx", sheet("Import") ///
	cellrange(A6:R52) clear first
	
foreach x of varlist _all {
	rename `x' imp_`x'
} 
ren imp_Year year

foreach x of varlist imp_Australia-imp_UnitedStates {
	replace `x'="." if `x'==".."
	}
	
foreach x of varlist imp_Australia-imp_UnitedStates {
	destring `x', replace
	}
	
sort year
	compress
	save se_import.dta, replace
	
* EU15
import excel using "SE_Tradeflows_OECD.Stat.xlsx", sheet("EU15") ///
	cellrange(A6:C60) clear first
	ren Import imp_EU15
	ren Export exp_EU15
	ren Year year
	
sort year
	compress
	save se_eu15.dta, replace
	
* Exports
import excel using "SE_Tradeflows_OECD.Stat.xlsx", sheet("Export") ///
	cellrange(A6:R52) clear first
	
foreach x of varlist _all {
	rename `x' exp_`x'
} 
ren exp_Year year

foreach x of varlist exp_Australia-exp_UnitedStates {
	replace `x'="." if `x'==".."
	}
	
foreach x of varlist exp_Australia-exp_UnitedStates {
	destring `x', replace
	}
	
sort year
	compress

* Merging
merge 1:1 year using se_import.dta
	drop _merge
	
merge 1:1 year using se_eu15.dta
	drop _merge
	
destring year, replace
	drop if year>2000
	sort year
	tsset year

	
* CREATING VARIABLES:

* Total trade:
foreach k in Australia Austria Belgium Canada Denmark Finland France ///
	Germany Ireland Italy Japan Netherlands NewZealand Norway ///
	UnitedKingdom UnitedStates World EU15 {
	gen trade_`k'=exp_`k'+imp_`k'
	label var trade_`k' "Total trade, `k'"
	}

* Country groupings:
gen trade_EU=trade_Belgium+trade_France+trade_Germany+trade_Italy+ ///
	trade_Netherlands
	label var trade_EU "Total trade, EU founding members (excl. Lux.)"
	
gen trade_Nor=trade_Denmark+trade_Finland+trade_Norway
	label var trade_Nor "Total trade, Nordics"

gen trade_Pac=trade_Japan+trade_NewZealand+trade_Australia
	label var trade_Pac "Total trade, Australasia"

for var trade_EU trade_Nor trade_Pac trade_UnitedKingdom ///
	trade_UnitedStates trade_EU15: ///
	gen pcX=(X/trade_World)*100
	label var pctrade_EU "EU5 (BE, NL, FR, DE)"
	label var pctrade_Nor "Nordic (DK, NO, FI)"
	label var pctrade_Pac "AUS, NZL, JAP"
	label var pctrade_UnitedKingdom "United Kingdom"
	label var pctrade_UnitedStates "United States"
	label var pctrade_EU15 "EU 15"

gr tw (line pctrade_EU year, lp(solid) lc(black)) ///
	(line pctrade_Nor year, lp(dash_dot) lc(black)) ///
	(line pctrade_UnitedKingdom year, lp(shortdash) lc(black)) ///
	(line pctrade_UnitedStates year, lp(longdash) lc(black)), ///
	ytitle("Percent of total trade" "(export+import)") ///
	leg(r(2)) xline(1957, lc(red) lp(dot)) ///
	xline(1995, lc(red) lp(solid)) xline(1991, lc(red) lp(longdash)) ///
	xline(1973, lc(red) lp(shortdash)) xline(1959, lc(red) lp(dot)) ///
	text(30 1957 "Treaty of Rome" "(1957)", place(w) c(red) si(vsmall)) ///
	text(30 1959 "EFTA" "(1959)", place(e) c(red) si(vsmall)) ///
	text(35 1973 "FTA & EEC enlargement" "(1973)", c(red) place(e) si(vsmall)) ///
	text(30 1995 "Membership" "(1995)", place(e) c(red) si(vsmall)) ///
	text(30 1991 "Application" "(1991)", place(w) c(red) si(vsmall))
	gr export SE_trade.pdf, replace
	
gr tw (line pctrade_EU15 year if year>1959 & year<2000, lc(black) lp(solid)), ///
	ylabel(50(5)70)
	

* ECJ Preliminary rulings
*************************

use "${no}/Uni/PhD Lund/Teaching/Swedish Politics/Stone Sweet_Brunell/Art_234.dta", ///
	clear
	
label define cntry 1 "Austria" 2 "Belgium" 3 "Denmark" 4 "Finland" 5 "France" ///
	6 "Germany" 7 "Greece" 8 "Ireland" 9 "Italy" 10 "Luxembourg"  ///
	11 "Netherlands" 12 "Portugal" 13 "Spain" 14 "Sweden" 15 "United Kingdom" ///
	16 "Cyprus" 17 "Estonia" 18 "Hungary" 19 "Latvia" 20 "Lithuania" ///
	21 "Malta" 22 "Poland" 23 "Czech Rep." 24 "Slovakia" 25 "Slovenia"
	label values country cntry
	label var country "Country"
	
	ren filedate year
	label var year "File date"
	
sort country year
	
* Only cases with a decision
*keep if decision==1 | decision==2

collapse (sum) agri (sum) freemove (sum) compet (sum) external ///
	 (sum) socsec (sum) socpriv (sum) environ (sum) estab ///
	  (sum) tax (sum) transprt (sum) commpolc  (sum) apprxlaw, ///
	  by(country year)
	  
drop if country==. | year==.
	sort country year
	xtset country year

egen allcases=rowtotal(agri freemove compet external ///
	socsec socpriv environ estab tax transprt commpolc apprxlaw)
	
gr tw (line allcases year if country==14, lp(solid) lw(medthick)) ///
	(spike socsec year if country==14, lc(red) lw(thick)) ///
	(spike tax year if country==14, lc(gold) lw(thick)) ///
	(spike environ year if country==14, lc(green) lw(thick)), ///
	leg(r(2) order(1 "Total" 2 "Social security" ///
	3 "Taxation" 4 "Environment")) ///
	ytitle("Number of ECJ Art. 234 references") ///
	xtitle(" ")
	gr export ecj234_se.pdf, replace
	
* Overall number of cases per year
use "${no}/Uni/PhD Lund/Teaching/Swedish Politics/Stone Sweet_Brunell/Art_234.dta", ///
	clear
	
label define cntry 1 "Austria" 2 "Belgium" 3 "Denmark" 4 "Finland" 5 "France" ///
	6 "Germany" 7 "Greece" 8 "Ireland" 9 "Italy" 10 "Luxembourg"  ///
	11 "Netherlands" 12 "Portugal" 13 "Spain" 14 "Sweden" 15 "United Kingdom" ///
	16 "Cyprus" 17 "Estonia" 18 "Hungary" 19 "Latvia" 20 "Lithuania" ///
	21 "Malta" 22 "Poland" 23 "Czech Rep." 24 "Slovakia" 25 "Slovenia"
	label values country cntry
	label var country "Country"
	
	ren filedate year
	label var year "File date"
	
drop if country==. | year==.
	sort country year
	
*keep if decision==1 | decision==2

gen allcases=1
	
collapse (sum) allcases, by(year)

gr tw line allcases year if year<2007, ///
	ytitle("Number of ECJ Art. 234 references per year") ///
	xtitle(" ")
	gr export ecj234.pdf, replace

	
* Kšnig/Luetgert BJPS Compliance dataset
****************************************
use "${no}/Uni/PhD Lund/Teaching/Swedish Politics/KoenigLuetgert.dta", ///
	clear	
	drop b-sw
	
sort country year sector	

gen simcat=category
	recode simcat (1 2 3 4 5 = 1) (6=2) (7=3), copyr
	label define simcat 1 "Timely" 2 "Delayed" 3 "Failed"
	label values simcat simcat
	
ta country simcat, r nof

ta simcat, gen(simcat)
	su simcat1
	local mean=r(mean)
	di `mean'

gr bar simcat1 simcat2 simcat3, over(country, lab(ang(45))) ///
	stack percent leg(r(1) order(1 "Timely" 2 "Delayed" 3 "Failed")) ///
	ytitle("Percent of all directives") ///
	blabel(bar, pos(center) format(%3.1f) si(vsmall) c(black)) ///
	bar(1, c(green)) bar(2, c(gold)) bar(3, c(red))
	gr export eudirtransp.pdf, replace
	

* Manifesto Project Data -- Swedish party positions on EU
*********************************************************
use "${rc}/General/Data/Comparative Manifesto Project/cmp_2014b.dta", clear

keep if countryname=="Sweden"
	keep country countryname eumember edate date party partyname per108 per110
	
gen year=year(edate)
	
sort party year
	xtset party year
	
gen euinteg=per108-per110
	label var euinteg "Number of references to EU (positive minus negative)"
	gen euissue=per108+per110
	label var euissue "Percent devoted to EU issues"
	
bysort year: egen aveuissue=mean(euissue)
	
gr tw (line aveuissue year if party==11320 & (year>=1960 & year<=2010), lc(black) lw(medthick)), ///
	xline(1995, lc(gray) lp(solid)) ///
	xline(1991, lc(gray) lp(longdash)) ///
	xline(1973, lc(gray) lp(shortdash)) ///
	text(6 1991 "Application" "(1991)", c(gray) place(w) si(small)) ///
	text(6 1995 "Membership" "(1995)", c(gray) place(e) si(small)) ///
	text(6 1973 "FTA & EEC enlargement" "(1973)", c(gray) place(w) si(small)) ///
	ytitle("Percent") ///
	title("Avg. share of party manifestos devoted to EU matters") ///
	xtitle(" ")
	gr export se_cmp.pdf, replace
	
* Party abbreviations
label define parabr 11110 "Greens" 11220 "Left party" 11320 "Social Democrats" ///
	11420 "Liberals" 11520 "Christian democrats" 11620 "Moderates" ///
	11810 "Center party", replace
	label values party parabr
	
gr tw (line euinteg year) if year>=1960 & year<=2010 ///
	& party!=11710 & party!=11951, by(party) xtitle(" ") ///
	xline(1995, lc(red) lp(shortdash)) yline(0, lc(gray) lp(shortdash)) ///
	ytitle("Support for EU" "(% positive minus % negative references)")
	gr export se_proeu.pdf, replace

* EU NŠmnden, number of documents
*********************************

clear
set obs 59

gen period=_n in 1/3
	label define period 1 "1990-99" 2 "2000-09" 3 "2010-15"
	label values period period
	
gen docs=.
	replace docs=1 if period==1
	replace docs=445 if period==2
	replace docs=718 if period==3
	
gr bar docs, over(period) ///
	ytitle(Number of documents) ///
	blabel(bar, pos(center) format(%3.0f) c(white) box fc(black))
	gr export eunamnden.pdf, replace
	

* Eurobarometer
***************

use "${no}/Data/Eurobarometer/ma_eurobar.dta", clear
	keep study_id id year eb nation* membrshp benefit ecimp
	ren membrshp memb
	
for var memb benefit ecimp: bysort nation2 year: ///
	egen avX=mean(X)
	
for var memb benefit ecimp: bysort year: egen m_X=mean(X) if nation2!=14
	
sort nation2 year
	by nation2 year: gen no=_n // observation identifier
	
gr tw (line avmemb year if nation2==17 & no==1, sort lp(solid) lc(black)) ///
	(line m_memb year if nation2==17 & no==1, lp(longdash) lc(black)), ///
	ylabel(1 "Good" 2 "Neither" 3 "Bad", angle(45)) ///
	leg(order(1 "Sweden" 2 "EU Average (excl. SE)")) xtitle(" ") yscale(reverse)
	gr export eb_mb.pdf, replace
	
gr tw (line avbenefit year if nation2==17 & no==1, sort lp(solid) lc(black)) ///
	(line m_benefit year if nation2==17 & no==1, lp(longdash) lc(black)), ///
	ylabel(1 "Benefited" 2 "Not benefited", angle(45)) yscale(reverse) ///
	leg(order(1 "Sweden" 2 "EU Average (excl. SE)")) xtitle(" ")
	gr export eb_ben.pdf, replace
	
* CPDS I (Swedish economic data)
********************************

use "${rc}/General/Data/CPDS I-III/CPDS I/CPDSI1960-2012stata.dta", clear

for var openc unemp debt: ///
	bysort year: egen oecd_X=mean(X) if country!="Sweden"

keep if country=="Sweden" | country=="Iceland"
	xtset countryn year
	
* Sweden
gr tw (line realgdpgr year if country=="Sweden" & year>=1985 & year<2000, lc(black)), ///
	xline(1991, lc(red) lp(shortdash)) xline(1995, lc(red) lp(longdash)) ///
	text(4 1991 "Application" "(1991)", c(gray) place(w) si(small)) ///
	text(4 1995 "Membership" "(1995)", c(gray) place(e) si(small)) ///
	ytitle("Real GDP Growth (%)") xtitle(" ")
	gr export se_gdpgr.pdf, replace
	
gr tw (line unemp year if country=="Sweden" & year>=1985 & year<2000, lc(black)), ///
	xline(1991, lc(red) lp(shortdash)) xline(1995, lc(red) lp(longdash)) ///
	text(4 1991 "Application" "(1991)", c(gray) place(w) si(small)) ///
	text(4 1995 "Membership" "(1995)", c(gray) place(e) si(small)) ///
	ytitle("Unemployment (%)") xtitle(" ")
	gr export se_unemp.pdf, replace

gr tw (line deficit year if country=="Sweden" & year>=1985 & year<2000, lc(black)), ///
	xline(1991, lc(red) lp(shortdash)) xline(1995, lc(red) lp(longdash)) ///
	text(-5 1991 "Application" "(1991)", c(gray) place(w) si(small)) ///
	text(-5 1995 "Membership" "(1995)", c(gray) place(e) si(small)) ///
	ytitle("Government Budget Balance (% GDP)") xtitle(" ")
	gr export se_deficit.pdf, replace
	
	
gr tw (line unemp year if country=="Sweden", lc(black)) ///
	(line oecd_unemp year, lc(black) lp(longdash)), ///
	xline(1991, lc(gray) lp(shortdash)) xline(1995, lc(gray) lp(longdash)) ///
	ytitle("Unemployment (%)") xtitle(" ") ///
	leg(order(1 "Sweden" 2 "OECD (excl. SE)"))
	gr export se_unemphist.pdf, replace
	
gr tw (line debt year  if country=="Sweden", lc(black)) ///
	(line oecd_debt year, lc(black) lp(longdash)), ///
	xline(1991, lc(gray) lp(shortdash)) xline(1995, lc(gray) lp(longdash)) ///
	ytitle("Gross government debt (% GDP)") xtitle(" ") ///
	leg(order(1 "Sweden" 2 "OECD (excl. SE)"))
	gr export se_debt.pdf, replace

gr tw (line openc year  if country=="Sweden", lc(black)) ///
	(line oecd_openc year, lc(black) lp(longdash)), ///
	xline(1991, lc(red) lp(shortdash)) xline(1995, lc(red) lp(longdash)) ///
	ytitle("Openness to trade (export + import, % GDP)") xtitle("Year") ///
	leg(order(1 "Sweden" 2 "OECD (excl. SE)"))

* Iceland
gr tw (line realgdpgr year if country=="Iceland" & year>=2007, lc(black)), ///
	xline(2009, lc(red) lp(shortdash)) ///
	text(5 2009 "Application for EU membership", c(red) si(small) place(e)) ///
	ytitle("Real GDP Growth (%)") xtitle(" ")
	gr export ice_gdpgr.pdf, replace


* Quality of Government
***********************

use "${no}/Data/Quality of Government/qog_std_ts_jan15.dta", clear
	* see also http://www.qogdata.pol.gu.se/data/qog_std_jan15.pdf

sort ccode year
	xtset ccode year
	drop if year<1950 | year>2010

for var wdi_gdppccon wdi_armedfper wdi_expmilgdp wdi_expmilgexp: bysort year:  ///
	egen av_X=mean(X) if cname!="Sweden" ///
	
for var wdi_gdppccon: bysort year: egen rank_gdpc=rank(X), field


gr tw (line wdi_gdppccon year if cname=="Sweden", lc(black)) ///
	(line av_wdi_gdppccon year if cname=="Austria", lc(black) lp(longdash)), ///
	xline(1991, lc(gray) lp(shortdash)) xline(1995, lc(gray) lp(longdash)) ///
	ytitle("Real GDP per capita (constant 2005 prices)") xtitle(" ") ///
	leg(order(1 "Sweden" 2 "World (excl. SE)")) xlabel(1950(10)2010)
	gr export se_gdppc.pdf, replace
	
gr tw (line rank_gdpc year if cname=="Sweden", yscale(reverse)), ///
	xline(1991, lc(gray) lp(shortdash)) xline(1995, lc(gray) lp(longdash)) ///
	ytitle("Sweden's ranking (per-capita GDP)") xtitle(" ") ///
	xlabel(1950(10)2010)
	gr export se_gdprank.pdf, replace
	
