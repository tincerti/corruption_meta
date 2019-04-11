********************************************************************************
* Preamble, globals, and import
********************************************************************************
clear all

global import "/Users/trevorincerti/Box Sync/Projects/corruption_meta/data/"
global export "/Users/trevorincerti/Box Sync/Projects/corruption_meta/figs/"


use "${import}/franchino_zucchini.dta"

********************************************************************************
* Define categories
********************************************************************************
* Drop NA values
drop if missing(Y)

* Pool corruption into one treatment
gen corrupt = 0
replace corrupt = 1 if corruption == 2 | corruption == 3

* Create categories
decode taxspend, gen(tax)
decode samesex, gen(same)

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
coefplot (matrix(plot[1,])), ci((plot[5,] plot[6,])) vertical scheme(s1color) ylabel(0(.1)1) yline(.5, lstyle(grid)) xlabel(, labsize(vsmall)) color(midblue) xsize(7) ysize(4)

* Export plot
graph export "${export}/fz_margins.pdf", replace





