********************************************************************************
* Preamble, globals, and import
********************************************************************************
clear all

* Insert paths to data folder and figure folder here
global import "/data/"
global export "/figs/"

********************************************************************************
* Define categories: Breitenstein
********************************************************************************
use "${import}/choosing_crook_clean.dta", replace

* Create categories
decode nperformance, gen(economy)
decode nqualities, gen(experience)
decode samep, gen(copartisan)

* Create categories
gen category = .
replace category = 1 if economy == "bad" & experience == "low" & copartisan == "Different party"
replace category = 2 if economy == "bad" & experience == "high" & copartisan == "Different party"
replace category = 3 if economy == "bad" & experience == "low" & copartisan == "Same party"
replace category = 4 if economy == "bad" & experience == "high" & copartisan == "Same party"

replace category = 5 if economy == "good" & experience == "low" & copartisan == "Different party"
replace category = 6 if economy == "good" & experience == "high" & copartisan == "Different party"
replace category = 7 if economy == "good" & experience == "low" & copartisan == "Same party"
replace category = 8 if economy == "good" & experience == "high" & copartisan == "Same party"

* Name categories
label define category ////
		1 `" "Poor economy" "Low experience" "Different party" "' ////
		2 `" "Poor economy" "High experience" "Different party" "' ////
		3 `" "Poor economy" "Low experience" "Copartisan" "' ////
		4 `" "Poor economy" "High experience" "Copartisan" "' ////
		5 `" "Strong economy" "Low experience" "Different party" "' ////
		6 `" "Strong economy" "High experience" "Different party" "' ////
		7 `" "Strong economy" "Low experience" "Copartisan" "' ////
		8 `" "Strong economy" "High experience" "Copartisan" "' ////

label val category category

********************************************************************************
* Calculate marginal effects
********************************************************************************
* Corrupt
reg Y i.category if corrupt == 1, cl(id)
margins category if corrupt == 1
est store corrupt_plot

* Store estimates for plotting
matrix Corrupt = r(table)'
matsort Corrupt 1 "up"
matrix Corrupt = Corrupt'

* Clean
reg Y i.category if corrupt == 0, cl(id)
margins category if corrupt == 0
est store clean_plot

* Store estimates for plotting
matrix Clean = r(table)'
matsort Clean 1 "up"
matrix Clean = Clean'

********************************************************************************
* Plot marginal effects: Main text figure
********************************************************************************

* Create plot
mylabels 0(25)100, myscale(@/100) local(myla) 

coefplot (matrix(Corrupt[1,])), ci((Corrupt[5,] Corrupt[6,])) ///
	vertical scheme(s1color) ylabel(`myla') ///
	ytitle("Probability of voting for corrupt candidate (%)" " ") ///
	xtitle(" " "Candidate profile") ///
	yline(.5, lstyle(grid)) ///
	xlabel(, labsize(small)) ///
	xsize(6.6) ysize(3) ///
	color(midblue) ciopts(lcolor(gs10))

* Export plot
graph export "${export}/b_margins.pdf", replace

********************************************************************************
* Plot marginal effects: Appendix figure showing corrupt and clean
********************************************************************************

* Create plot
mylabels 0(25)100, myscale(@/100) local(myla) 

coefplot ///
	(matrix(Corrupt[1,]), offset(0) color(orange_red) ci((Corrupt[5,] Corrupt[6,]))) ///
	(matrix(Clean[1,]), color(midgreen) ci((Clean[5,] Clean[6,]))), ///
	vertical scheme(s1color) ylabel(`myla') ///
	ytitle("Probability of voting for candidate (%)" " ") ///
	xtitle(" " "Candidate profile") ///
	yline(.5, lstyle(grid)) ///
	xlabel(, labsize(small)) ///
	xsize(6.6) ysize(3) ///
	ciopts(lcolor(gs10))
	
* Export plot
graph export "${export}/b_margins_corrupt_clean.pdf", replace

********************************************************************************
* Repeat analyis with clean alternative only
******************************************************************************** 
gen alternative = "clean" if corrupt == 1 & corrupt[_n+1] == 0 & candidate == 1 & candidate[_n+1] == 2
replace alternative = "clean" if corrupt == 0 & corrupt[_n-1] == 1 & candidate == 2 & candidate[_n-1] == 1
replace alternative = "clean" if corrupt == 0 & corrupt[_n+1] == 1 & candidate == 1 & candidate[_n+1] == 2
replace alternative = "clean" if corrupt == 1 & corrupt[_n-1] == 0 & candidate == 2 & candidate[_n-1] == 1
keep if alternative == "clean"

* Calculate predicted probabilities
reg Y i.category if corrupt == 1, cl(id)
margins category if corrupt == 1

* Sort results
matrix plot = r(table)'

matsort plot 1 "up"

matrix plot = plot'

* Create plot
mylabels 0(25)100, myscale(@/100) local(myla) 

coefplot (matrix(plot[1,])), ci((plot[5,] plot[6,])) ///
	vertical scheme(s1color) ylabel(`myla') ///
	ytitle("Probability of voting for corrupt candidate (%)" " ") ///
	xtitle(" " "Candidate profile") ///
	yline(.5, lstyle(grid)) ///
	xlabel(, labsize(small)) ///
	xsize(6.6) ysize(3) ///
	color(midblue) ciopts(lcolor(gs10))

* Export plot
graph export "${export}/b_margins_clean.pdf", replace


********************************************************************************
* Define categories: Franchino and Zucchini
********************************************************************************
use "${import}/franchino_zucchini.dta", replace

* Create left vs. right categorization
gen left = 0
replace left = 1 if left_right <= 4

gen right = 0
replace right = 1 if left_right >= 6

* Drop NA values
drop if missing(Y)

* Pool corruption into one treatment
gen corrupt = 0
replace corrupt = 1 if corruption == 2 | corruption == 3

* Create categories
decode taxspend, gen(tax)
decode samesex, gen(same)

* Create categories
gen category = .
replace category = 1 if corrupt == 1 & tax == "Cut taxes" & same == "No rights"
replace category = 2 if corrupt == 1 & tax == "More social services" & same == "No rights"
replace category = 3 if corrupt == 1 & tax == "Maintain level of provision" & same == "No rights"

replace category = 4 if corrupt == 1 & tax == "Cut taxes" & same == "Some rights"
replace category = 5 if corrupt == 1 & tax == "More social services" & same == "Some rights"
replace category = 6 if corrupt == 1 & tax == "Maintain level of provision" & same == "Some rights"

replace category = 7 if corrupt == 1 & tax == "Cut taxes" & same == "Same rights"
replace category = 8 if corrupt == 1 & tax == "More social services" & same == "Same rights"
replace category = 9 if corrupt == 1 & tax == "Maintain level of provision" & same == "Same rights"

* Name categories
label define category ////
		1 `" "Cut taxes" "No rights" "' ////
		2 `" "More services" "No rights" "' ////
		3 `" "Status quo" "No rights" "' ////	
		4 `" "Cut taxes" "Some rights" "' ////
		5 `" "More services" "Some rights" "' ////
		6 `" "Status quo" "Some rights" "' ////	
		7 `" "Cut taxes" "Same rights" "' ////
		8 `" "More services" "Same rights" "' ////
		9 `" "Status quo" "Same rights" "' ////

label val category category

********************************************************************************
* Calculate marginal effects
********************************************************************************
foreach i in left right {
preserve

keep if `i' == 1

reg Y i.category, cl(IDContatto)
margins category

********************************************************************************
* Plot marginal effects
********************************************************************************
* Sort results
matrix plot = r(table)'

matsort plot 1 "up"

matrix plot = plot'

* Create plot
mylabels 0(25)100, myscale(@/100) local(myla) 

coefplot (matrix(plot[1,])), ci((plot[5,] plot[6,])) ///
	vertical scheme(s1color) ylabel(`myla') ///
	ytitle("Probability of voting for corrupt candidate (%)" " ") ///
	xtitle(" " "Policy platform") ///
	yline(.5, lstyle(grid)) ///
	xlabel(, labsize(medsmall)) ///
	xsize(6.6) ysize(3) ///
	color(midblue) ciopts(lcolor(gs10))

* Export plot
graph export "${export}/fz_margins_`i'.pdf", replace

restore
}

********************************************************************************
* Repeat analyis with clean alternative only
******************************************************************************** 
gen alternative = "clean" if corrupt == 1 & corrupt[_n+1] == 0 & n_vote == n_vote[_n+1]
replace alternative = "clean" if corrupt == 0 & corrupt[_n-1] == 1 & n_vote == n_vote[_n-1]
replace alternative = "clean" if corrupt == 0 & corrupt[_n+1] == 1 & n_vote == n_vote[_n+1]
replace alternative = "clean" if corrupt == 1 & corrupt[_n-1] == 0 & n_vote == n_vote[_n-1]
keep if alternative == "clean"

* Calculate predicted probabilies
foreach i in left right {
preserve

keep if `i' == 1

reg Y i.category, cl(IDContatto)
margins category


* Sort results
matrix plot = r(table)'

matsort plot 1 "up"

matrix plot = plot'

* Create plot
mylabels 0(25)100, myscale(@/100) local(myla) 

coefplot (matrix(plot[1,])), ci((plot[5,] plot[6,])) ///
	vertical scheme(s1color) ylabel(`myla') ///
	ytitle("Probability of voting for corrupt candidate (%)" " ") ///
	xtitle(" " "Policy platform") ///
	yline(.5, lstyle(grid)) ///
	xlabel(, labsize(medsmall)) ///
	xsize(6.6) ysize(3) ///
	color(midblue) ciopts(lcolor(gs10))

* Export plot
graph export "${export}/fz_margins_`i'_clean.pdf", replace

restore
}

********************************************************************************
* Define categories: Mares and Visconti
********************************************************************************
use "${import}/mares_visconti.dta", replace

* Pool corruption into one treatment
replace corrupt = 0
replace corrupt = 1 if atinte == 3 | atinte == 2

* Pool policy offerings into one treatment
gen policy = "None"
replace policy = "Programmatic" if atpol == 2 | atpol == 3

* Threats variable
gen threats = "No threats"
replace threats = "Threatens" if atinti == 2

* Vote buying variable
gen vb = "No vote buying"
replace vb = "Vote buying" if atmita == 2 | atmita == 3


* Create categories across all levels of illicit acitivities
decode atexp, gen(experience)

* Create categories
gen category = .
replace category = 1 if corrupt == 1 & policy == "None" & experience == "Challenger"
replace category = 2 if corrupt == 1 & policy == "None" & experience == "Incumbent"
replace category = 3 if corrupt == 1 & policy == "Programmatic" & experience == "Challenger"
replace category = 4 if corrupt == 1 & policy == "Programmatic" & experience == "Incumbent"

* Name categories
label define category ////
		1 `" "Not programmatic" "Low experience" "' ////
		2 `" "Not programmatic" "High experience" "' ////
		3 `" "Programmatic" "Low experience" "' ////
		4 `" "Programmatic" "High experience" "' ////	
		
label val category category


********************************************************************************
* Calculate marginal effects
********************************************************************************
reg outcome i.category, cl(idnum)
margins category

********************************************************************************
* Plot marginal effects 
********************************************************************************
* Sort results
matrix plot = r(table)'

matsort plot 1 "up"

matrix plot = plot'

* Create plot
mylabels 0(25)100, myscale(@/100) local(myla) 

coefplot (matrix(plot[1,])), ci((plot[5,] plot[6,])) ///
	vertical scheme(s1color) ylabel(`myla') ///
	ytitle("Probability of voting for corrupt candidate (%)" " ") ///
	xtitle(" " "Candidate profile") ///
	yline(.5, lstyle(grid)) ///
	xlabel(, labsize(small)) ///
	xsize(6.6) ysize(3) ///
	color(midblue) ciopts(lcolor(gs10))

* Export plot
graph export "${export}/mv_margins.pdf", replace

********************************************************************************
* Create categories conditional on levels of illicit acitivities
********************************************************************************

replace category = .
replace category = 1 if corrupt == 1 & policy == "None" & experience == "Challenger" & threats == "Threatens" & vb == "Vote buying"
replace category = 2 if corrupt == 1 & policy == "None" & experience == "Incumbent" & threats == "Threatens" & vb == "Vote buying"

replace category = 3 if corrupt == 1 & policy == "Programmatic" & experience == "Challenger" & threats == "Threatens" & vb == "Vote buying"
replace category = 4 if corrupt == 1 & policy == "Programmatic" & experience == "Incumbent" & threats == "Threatens" & vb == "Vote buying"

replace category = 5 if corrupt == 1 & policy == "None" & experience == "Challenger" & threats == "No threats" & vb == "No vote buying"
replace category = 6 if corrupt == 1 & policy == "None" & experience == "Incumbent" & threats == "No threats" & vb == "No vote buying"

replace category = 7 if corrupt == 1 & policy == "Programmatic" & experience == "Challenger" & threats == "No threats" & vb == "No vote buying"
replace category = 8 if corrupt == 1 & policy == "Programmatic" & experience == "Incumbent" & threats == "No threats" & vb == "No vote buying"

* Name categories
label drop category
label define category ////
		1 `" "Not programmatic" "Low experience" "Other illicit" "' ////
		2 `" "Not programmatic" "High experience" "Other illicit" "' ////
		3 `" "Programmatic" "Low experience" "Other illicit" "' ////
		4 `" "Programmatic" "High experience" "Other illicit" "' ////	
		5 `" "Not programmatic" "Low experience" "No other illicit" "' ////
		6 `" "Not programmatic" "High experience" "No other illicit" "' ////
		7 `" "Programmatic" "Low experience" "No other illicit" "' ////
		8 `" "Programmatic" "High experience" "No other illicit" "' ////	
		
label val category category


********************************************************************************
* Calculate marginal effects
********************************************************************************
reg outcome i.category, cl(idnum)
margins category

********************************************************************************
* Plot marginal effects 
********************************************************************************
* Sort results
matrix plot = r(table)'

matsort plot 1 "up"

matrix plot = plot'

* Create plot
mylabels 0(25)100, myscale(@/100) local(myla) 

coefplot (matrix(plot[1,])), ci((plot[5,] plot[6,])) ///
	vertical scheme(s1color) ylabel(`myla') ///
	ytitle("Probability of voting for corrupt candidate (%)" " ") ///
	xtitle(" " "Candidate profile") ///
	yline(.5, lstyle(grid)) ///
	xlabel(, labsize(small)) ///
	xsize(6.6) ysize(3) ///
	color(midblue) ciopts(lcolor(gs10))

* Export plot
graph export "${export}/mv_margins_illicit.pdf", replace

********************************************************************************
* Define categories: Chauchard, Klasnja, and Harish
********************************************************************************
use "${import}/chauchard_klasnja_harish.dta", replace

* Pool corruption into one treatment
gen corrupt = 0
replace corrupt = 1 if legal == 2
keep if legal == 2 | legal == 3

* Create categories
gen category = .
replace category = 1 if corrupt == 1 & record == 2 & out_party == 1 & co_ethn_dummy == 0
replace category = 2 if corrupt == 1 & record == 1 & out_party == 1 & co_ethn_dummy == 0

replace category = 3 if corrupt == 1 & record == 2 & out_party == 0 & co_ethn_dummy == 0
replace category = 4 if corrupt == 1 & record == 1 & out_party == 0 & co_ethn_dummy == 0

replace category = 5 if corrupt == 1 & record == 2 & out_party == 0 & co_ethn_dummy == 1
replace category = 6 if corrupt == 1 & record == 1 & out_party == 0 & co_ethn_dummy == 1

* Name categories
label define category ////
		1 `" "Bad performance" "Different party" "Not coethnic" "' ////
		2 `" "Good performance" "Different party" "Not coethnic" "' ////
		3 `" "Bad performance" "Copartisan" "Not coethnic" "' ////
		4 `" "Good performance" "Copartisan" "Not coethnic" "' ////	
		5 `" "Bad performance" "Copartisan" "Coethnic" "' ////
		6 `" "Good performance" "Copartisan" "Coethnic" "' ////

label val category category

********************************************************************************
* Calculate marginal effects
********************************************************************************
reg dv_vote i.category, cl(id)
margins category

********************************************************************************
* Plot marginal effects
********************************************************************************
* Sort results
matrix plot = r(table)'

matsort plot 1 "up"

matrix plot = plot'

* Create plot
mylabels 0(25)100, myscale(@/100) local(myla) 

coefplot (matrix(plot[1,])), ci((plot[5,] plot[6,])) ///
	vertical scheme(s1color) ylabel(`myla') ///
	ytitle("Probability of voting for corrupt candidate (%)" " ") ///
	xtitle(" " "Candidate profile") ///
	yline(.5, lstyle(grid)) ///
	xlabel(, labsize(small)) ///
	xsize(6.6) ysize(3) ///
	color(midblue) ciopts(lcolor(gs10))

* Export plot
graph export "${export}/ckh_margins.pdf", replace

