**************************************************************************
**************June 23 calculate variables for analysis

***********Final audit item cleaning, delete reported items unique to one or two countries

***previously, we start with 1,392,365 audited items across the 7 countries
*** Lingyu ran machine learning algorithm to group almost identical audit items together
****Lingyu then manually checked each grouping and further manually grouped similar items together with Python 
*****Chunyun go through a final check of the grouping of the same items across the years and countries and deleted many items not consistently asked.
******these steps lead to 1,048,459 items in the items dataset (with 37,691 items in the 'delete'category to be deleted if needed)


drop if regexm(Q,"employer deny workers access to their personal documents")
//6209 deleted; this item was for public reporting only in Jordan while not reported in other countries
//unique to Jordan

 drop if QCleaned=="has the employer adequately prepared for emergencies in the accommodation?"
 //6125 deleted. only for public reporting in Jordan


drop if regexm(QCleaned,"Limits on the use of fixed contracts")
//6196; only for public reporting in Vietnam, uniqe to Vietnam

drop if QCleaned=="has the employer taken action to prevent and limit workers' exposure to chemicals and hazardous substances?"
 //6267 entries deleted; only for public reporting in Vietnam

replace disclosed=0 if QCleaned=="does the employer comply with requirements on hiv/aids?"
//2503 changed; miscoded as disclosed in Cambodia dataset

replace disclosed=0 if QCleaned=="does the employer include the entire period of continuous employment when determining workers' entitlements to maternity leave, attendance bonus, seniority bonus, and/or annual leave?"
//2503 changed; miscoded as disclosed in Cambodia dataset


replace disclosed=1 if QCleaned=="fire detection and alarm system"
//2503 changed. previously not coded as "reported" in Cambodia dataset

 replace disclosed=0 if regexm(QCleaned,"has legal construction permits")
 //584 changed. previously miscoded as "reported" in Bangladesh
 
 drop if QCleaned=="has the employer taken sufficient steps to ensure that migrant workers do not pay any unauthorized fees?"
 //474 deleted; only asksed in Jordan
 
 
 drop if QCleaned=="is the accommodation protected against fire?"
 //6093 deleted; only for pubilic reporting in Jordan
 
 replace disclosed=1 if regexm(QCleaned,"storage of chemicals and hazardous")
 //1802 changed. Not taged as "reported" in Vietnam
 
 
 replace disclosed=1 if QCleaned=="interfere union"
 //1775 changed. not taged as "reported in Vietnam

 replace disclosed=1 if QCleaned=="forced overtime under threat of penalty"
 //2 changed; previous not coded as for public reporting
 
 
 
 
 ***************************************************************
 ****genearge some variables
 
 ***year variable 
 gen year=substr(AssesmentDate,7,4)
  destring year,replace
  
 ***similar items that share the same CPs(compliance point) with those items for public reporting
 
 gen similarCP=1 if regexm(CPCleaned,"chemicals and hazardous substances|child labourers|coercion|collective bargaining|dialogue, discipline and disputes|emergency preparedness|forced labour and overtime|freedom to associate|gender|interference and discrimination|minimum wages/piece rate wages|osh management systems|overtime wages|paid leave|strikes|wage information, use and deduction|welfare facilities")
 
 replace similarCP=0 if similarCP==.
 
 ***three types of standards: reported, nonreported similar items, nonreported distant items
 gen threestds=1 if disclosed==1
 replace threestds=2 if similarCP==1 & disclosed==0
 replace threestds=3 if similarCP==0

** total items in each audit
drop if CPCleaned=="delete" 
//delete those items that are unique to less than three countries and could not be grouped with other similar items across the 7 countries
//these deleted 18 audits in Indonesia that used a short version of audits & 3 short audits in Nicaragua

bysort QuestID: gen totalitems=_N
bysort QuestID: egen totalvios=sum(Finding)

gen overallcompl=100*(totalitems-totalvios)/totalitems

drop if totalitems<122
//28435 entries deleted. 
//deleted 267 audits in Cambodia that used short version audits in 2021 
//this left 100 full version audits in Cambodia in 2021)
//the regular full version audits in Cambodia has 160 items while the short version has around 100 items



****compliance rates with each of the three types of audited labor standards

bysort QuestID threestds: gen threestditems=_N
bysort QuestID threestds: egen threestdvios=sum(Finding)

gen threestdcompl=100*(threestditems-threestdvios)/threestditems




****transform the long data table 
duplicates drop QuestID threestds,force

keep QuestID totalitems totalvios overallcompl threestds threestditems threestdvios threestdcompl

reshape wide totalitems totalvios overallcompl threestditems threestdvios threestdcompl, i(QuestID) j(threestds)

rename totalitems1 totalitems
rename totalvios1 totalvios
rename overallcompl1 overallcompl

label variable totalitems "total items in audit"
label variable totalvios "total violations in the audit"
label variable overallcompl "overall compliance%"

drop totalitems2 totalvios2 totalitems3 totalvios3
drop overallcompl2 overallcompl3


rename threestditems2 similarCPitems
rename threestdvios2 similarCPvios
rename threestdcompl2 similarCPcompl

label variable similarCPitems "total items in the same Compliance Point as disclosed items"

label variable similarCPvios "violations in the same Compliance Point as disclosed items"

label variable similarCPcompl "compliance% of the same Compliance Point as disclosed items"


rename threestditems3 distantCPitems
rename threestdvios3 distantCPvios
rename threestdcompl3 distantCPcompl

label variable distantCPitems "items in distant Compliance Point far from disclosed items"
label variable distantCPvios "violations of distant Compliance Point far from disclosed items"
label variable distantCPcompl "compliance % of distant Compliance Point far from disclosed items"

****save dataset

****merge with other datasets of independent variables and moderators


