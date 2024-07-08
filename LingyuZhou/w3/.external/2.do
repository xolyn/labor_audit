// use "D:\r2.dta"
// gen year = substr(AssesmentDate, -4, 4)

*********************************************************************************
******run these stata codes before runing the Python codes for this CP

**************clean OSH management systems

replace QCleaned="does the factory have a valid business license?" if QCleaned=="has the factory submitted a notice to the chief inspector before starting operations?" & Country=="Bangladesh"
//one item asked in different ways in Bangladesh

replace CPCleaned="osh management systems" if QCleaned=="mechanisms/committee for cooperation on osh"


replace QCleaned=Q if Q=="does the workplace have a functioning hiv/aids committee?"
//2503; rectify coding error in Cambodia

replace QCleaned="mechanisms/committee for cooperation on osh" if Q=="has the employer developed mechanisms to ensure cooperation between workers and management on osh matters?"
//2503 changed, Cambodia

replace disclosed=1 if QCleaned=="mechanisms/committee for cooperation on osh" 
//3087 changed; right contribute distribution


replace QCleaned="does the factory have a written osh policy?" if QCleaned=="is there an adequate osh policy that is signed by top management?" & Country=="Cambodia"

replace QCleaned="has the employer performed an assessment of general occupational safety and health issues in the factory?" if Q=="does the employer conduct risk assessment?"

replace QCleaned="has the employer performed an assessment of general occupational safety and health issues in the factory?" if Q=="does the employer conduct regular hazards and risk assessment?"

replace QCleaned="does the factory have a written osh policy?" if regexm(Q,"does the employer have written plans for osh programs?|does the employer have a written policy and plans for osh programs?")

duplicates tag QuestID if Country=="Jordan" & year=="2015" & QCleaned=="does the factory have a written osh policy?",gen(JDdup)

drop if index==470616
drop if index==470687
 //two identical questions in Jordan in 2015
 
drop JDdup

replace QCleaned="does the employer investigate work-related accidents and indicate the technical recommendations necessary to prevent them?" if regexm(Q,"does the employer investigate work-related accidents and indicate the technical recommendations necessary to prevent them?|does the employer record and investigate work-related accidents and illnesses, and indicate the technical recommendations necessary to prevent them?")

replace QCleaned="has legal construction permits/structural safety certificate" if Q=="does the employer have legally required construction/building permits?"

replace QCleaned="has legal construction permits/structural safety certificate" if Q=="does the employer ensure the building is safe and maintain legally required permits"

replace QCleaned="does the employer record work-related accidents?" if Q=="does the employer record work-related accidents and diseases and submit the record to ofatma on a monthly basis?"
//161 changes, asked differently in Haiti


replace QCleaned="has legal construction permits/structural safety certificate" if QCleaned=="does the factory have a valid business license?" & Country=="Jordan"


*********************************************************************************
*****run these  in full to clean some CPs

*****************************************************************************
*****************overtime wages CP 

replace CPCleaned="overtime wages" if CP=="premium pay"

replace QCleaned="Correct pay for work on public holidays" if regexm(QCleaned,"does the employer pay all workers correctly for work on public holidays?|does the employer pay workers 100% more than their normal wage for all overtime hours worked on national holidays?|does the employer pay workers 150% of their normal wage for overtime worked on weekly rest days and public holidays?|does the employer pay all workers correctly for work on public holidays?")

replace QCleaned="Correct pay for work on public holidays" if Q=="does the employer pay workers 50% above the normal wage when workers work regular hours on legally mandated holidays?"
//161 changes, one item in Haiti grouped under "premium pay"


replace QCleaned="Correct pay for overtime at night" if regexm(Q,"does the employer pay workers correctly for all overtime hours worked at night?|does the employer pay workers double their normal wage for all overtime hours worked at night?")

replace QCleaned="Correct pay for overtime at weekly rest days" if regexm(Q,"does the employer pay workers correctly for all overtime hours worked on weekly rest days?|does the employer pay workers double their normal wage for all hours worked on weekly rest days?|does the employer pay workers 100% more than their normal wage for all overtime hours worked on weekly rest days?")

replace QCleaned="Correct pay for overtime at weekly rest days" if Q=="does the employer pay workers 50% above the normal wage when workers work regular hours on weekly rest days?"
//161 changed. combine 1 in Haiti with others

replace QCleaned="Correct pay for overtime at weekly rest days" if regexm(Q,"does the employer pay workers correctly when they take compensatory days off after working on weekly rest days?")
//584 changed; Combine one item in Bangladesh with those in other countries



replace QCleaned="Correct pay for overtime at weekly rest days" if regexm(Q,"does the employer provide meals and drinks of at least 1,400 calories to workers working overtime for 3 hours or more?|does the employer provide meals and drinks of at least 1,400 calories to workers working overtime for 4 hours or more?")
//834 changed, combine one item in Indonesia with those in Cambodia and Jordan


replace QCleaned="Correct pay for work on public holidays" if QCleaned=="does the employer pay piece rate workers correctly for all overtime hours worked on regular work days?" 
//584 changed, combine one item in Balgdesh with those in other countries


drop if regexm(Q,"does the employer pay workers 100% above the normal wage for all overtime hours worked at night?|does the employer pay workers 100% above the normal wage for overtime hours worked on legally mandated holidays?|does the employer pay workers 100% above the normal wage for overtime hours worked on weekly rest days?|does the employer comply with national law regarding payment for regular working hours worked at night?") & Country=="Haiti"
//320 deleted; 4 items only asked in 2015-2018 in Haiti


drop if Q=="does the factory incorporate all required allowances and additional payments into the calculation of wage-based benefits (e.g. social insurance payments, overtime, paid leave, etc.)?"
//1555 deleted; 1 item only in Vietnam and not asked in 2015


drop if Q=="does the employer pay at least 5% higher than the normal applicable wage level for workers who perform hazardous and dangerous work?"
//1775 deleted; 1 unique item only asked in Vietnam


drop if Q=="does the employer pay all workers the correct meal allowance or give them a reasonable free meal when they work overtime?" & Country=="Cambodia"
//2503 deleted; one item only asked in Cambodia


drop if QCleaned=="Correct pay for overtime at night" 
//4157 deleted; one item only in Cambodia, Nicaragua, and Vietnam

drop if QCleaned=="does the employer pay workers correctly for regular working hours worked at night?"
//only asked in Cambodia and Vietnam


********************************************************************************
**********clean Prison Labour CP

replace QCleaned="prison labor consent and same treatment" if regexm(Q,"if prison labour is used, do the prisoners receive similar treatment to non-prison workers working in the factory?|if prison labour is used, have the prisoners freely consented to do the work; do they receive similar treatment to non-prison workers; and is the work carried out under the supervision and control of a public authority?|if prison labour is used, have the prisoners freely consented to do the work?")


drop if Q=="if prison labour is used, is the work carried out under the supervision and control of a public authority?" & Country=="Indonesia"
//833 deleted; similar item only in Indonesia 


drop if Q=="if prison labour is used, have the prisoners freely consented to do the work?" & Country=="Indonesia"
//833 deleted; similar item only in Indonesia 

