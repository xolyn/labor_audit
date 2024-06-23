

*****alternative coding of the timing of public reporting
******staggered adoption approach

gen afterannounce=1 if year>=2017 & Country=="Indonesia"
replace afterannounce=0 if year<2017 & Country=="Indonesia"

replace afterannounce=1 if year>2016 & Country=="Vietnam"
replace afterannounce=1 if year==2016 & month>=6 & Country=="Vietnam"
replace afterannounce=0 if year==2016 & month<6 & Country=="Vietnam"
replace afterannounce=0 if year<2016 & Country=="Vietnam"


replace afterannounce=1 if year>=2017 & Country=="Jordan"
replace afterannounce=0 if year<2016 & Country=="Jordan"
replace afterannounce=1 if year==2016 & month>=11 & Country=="Jordan"
replace afterannounce=0 if year==2016 & month<11 & Country=="Jordan"


replace afterannounce=1 if year>=2018 & Country=="Nicaragua"
replace afterannounce=0 if year<2018 & Country=="Nicaragua"

replace afterannounce=1 if year>=2018 & Country=="Haiti" //Haiti has always published facctory level info
replace afterannounce=0 if year<2018 & Country=="Haiti"
replace afterannounce=1 if year==2017 & month>=7 & Country=="Haiti"
replace afterannounce=0 if year==2017 & month<7 & Country=="Haiti"




******the online portal only reports factories that have at least 2 audits (cycle 2), 
gen after2auditdisc=0 if year<2017
replace after2auditdisc=1 if year>2017 & Cycle>=2
replace after2auditdisc=0 if year==2017 & month<7
replace after2auditdisc=1 if year==2017 & month>=7 & Cycle>=2
replace after2auditdisc=0 if year<2018 & Country=="Nicaragua" 

