
sum disclosedcompl similarCPcompl distantCPcompl 

global cons mng i.union CBA femalepc regularwkpc size factoryageln Cycle 


****improvement on publicly reported standards
xtset facID yearforpanel


reg disclosedcomplnofreeunion i.after17jul i.year i.countryf $cons, cluster(facID)
est store m0

xtreg disclosedcomplnofreeunion i.after17jul i.year ,fe cluster(facID)
est store m1

xtreg disclosedcomplnofreeunion i.after17jul $cons i.year,fe cluster(facID)
est store m2


xtreg disclosedcomplnofreeunion i.after17jul $cons i.year if Country=="Vietnam" ,fe cluster(facID)
est store m3

xtreg disclosedcomplnofreeunion i.after17jul $cons i.year if Country=="Indonesia" ,fe cluster(facID)
est store m4

xtreg disclosedcomplnofreeunion i.after17jul $cons i.year if Country=="Jordan" & year<2019,fe cluster(facID)
est store m5

esttab m0 m1 m2 m3 m3 m4 m5 using results.csv, ///
        cells(b(star fmt(3)) se(par fmt(3))) stats(N r2 ar2) replace
		

		
		
xtreg disclosedcomplnofreeunion i.after17jul $cons i.year if Country=="Haiti", fe cluster(facID)

******moderators of compliance with reported standards
xtreg disclosedcomplnofreeunion i.after17jul##c.l.disclosedcompl $cons i.year, fe cluster(facID)


xtreg disclosedcomplnofreeunion i.after17jul##c.pressfreedom $cons i.year, fe cluster(facID)
est store m1

xtreg disclosedcomplnofreeunion i.after17jul##c.buyer1pressfreedom $cons i.year, fe cluster(facID)

xtreg disclosedcomplnofreeunion i.after17jul##ib3.buyer1continentnum $cons i.year,fe cluster(facID)
est store m2

xtreg disclosedcomplnofreeunion i.after17jul##c.NoBWbuyersmissingas0 $cons i.year, fe cluster(facID)
est store m3

reg disclosedcomplnofreeunion i.after17jul##c.Cycle $cons i.year,cluster(facID)
est store m4

xtreg similarCPcompl i.after17jul##c.mng $cons i.year,fe cluster(facID)
est store m5

xtreg disclosedcomplnofreeunion i.after17jul##c.size $cons i.year,fe cluster(facID)
est store m6

xtreg disclosedcomplnofreeunion i.after17jul##i.union $cons i.year,fe cluster(facID)
est store m7

esttab  m2 m3 m3 m4 m5 m6 m7 using results.csv, ///
        cells(b(star fmt(3)) se(par fmt(3))) stats(N r2 ar2) replace

		

margins, at(after17jul=(0 1) size=(6.01 7.95) )


margins, at(after17jul=(0 1) pressfreedom=(23.85 60.41))


margins, at(after17jul=(0 1) union=(0 1)) atmeans noestimcheck

margins, at(after17jul=(0 1) Cycle=(2 8)) //2.26 7.92 for 5 countries

margins, at(after17jul=(0 1) mng=(7 12)) //1sd -+:6.75 12.33 for Vietbam
marginsplot

margins, at(after17jul=(0 1) mng=(8 13)) //1sd -+:6.75 12.33 for Indonesia

margins, at(after17jul=(0 1) mng=(5 13)) atmeans noestimcheck //1sd -+:4.56 12.56 for Jordan


margins, at(after17jul=(0 1) mng=(6 12)) //1sd -+:6.42 12.54 for 5 countries 


margins, at(after17jul=(0 1) l.disclosedcompl=(84.4 98.6)) atmeans


****Spillover or tradeoff
xtreg similarCPcompl i.after17jul i.year,fe cluster(facID)
est store m1

xtreg similarCPcompl i.after17jul $cons i.year,fe cluster(facID)
est store m2

xtreg similarCPcompl i.after17jul $cons i.year if Country=="Vietnam",fe cluster(facID)
est store m3

xtreg similarCPcompl i.after17jul $cons i.year if Country=="Indonesia",fe cluster(facID)
est store m4

xtreg similarCPcompl i.after17jul $cons i.year if Country=="Jordan",fe cluster(facID)
est store m5


xtreg similarCPcompl i.after17jul $cons i.year if HaitiNic==1,fe cluster(facID) //+sig*

xtreg distantCPcompl i.after17jul $cons i.year if Country=="Haiti",fe cluster(facID) //+p.066

xtreg similarCPcompl i.after17jul $cons i.year if Country=="Nicaragua",fe cluster(facID) //+sig p.013


esttab m1 m2 m3 m3 m4 m5 using results.csv, ///
        cells(b(star fmt(3)) se(par fmt(3))) stats(N r2 ar2) replace
		
		
xtreg distantCPcompl i.after17jul i.year,fe cluster(facID)
est store m1

xtreg distantCPcompl i.after17jul $cons i.year,fe cluster(facID)
est store m2

xtreg distantCPcompl i.after17jul $cons i.year if Country=="Vietnam",fe cluster(facID)
est store m3

xtreg distantCPcompl i.after17jul $cons i.year if Country=="Indonesia",fe cluster(facID)
est store m4

xtreg distantCPcompl i.after17jul $cons i.year if Country=="Jordan",fe cluster(facID)
est store m5


xtreg similarCPcompl i.after17jul $cons i.year if HaitiNic==1,fe
xtreg distantCPcompl i.after17jul $cons i.year if HaitiNic==1,fe

xtreg similarCPcompl i.after17jul $cons i.year if Country=="Haiti",fe
xtreg distantCPcompl i.after17jul $cons i.year if Country=="Haiti",fe


xtreg similarCPcompl i.after17jul $cons i.year if Country=="Nicaragua",fe
xtreg distantCPcompl i.after17jul $cons i.year if Country=="Nicaragua",fe




****moderators of spill over effect
xtreg similarCPcompl i.after17jul##c.pressfreedom $cons i.year, fe cluster(facID)
est store m1

xtreg similar CPcompl i.after17jul##ib3.buyer1continentnum $cons i.year,fe cluster(facID)
est store m2

xtreg similarCPcompl i.after17jul##c.NoBWbuyersmissingas0 $cons i.year, fe cluster(facID)
est store m3

xtreg similarCPcompl i.after17jul##c.Cycle $cons  i.year, fe cluster(facID)
est store m4

xtreg similarCPcompl i.after17jul##c.mng $cons i.year,fe cluster(facID)
est store m5

xtreg similarCPcompl i.after17jul##c.size $cons i.year,fe cluster(facID)
est store m6

xtreg similarCPcompl i.after17jul##i.union $cons i.year,fe cluster(facID)
est store m7


esttab m2 m3 m3 m4 m5 m6 m7 using results.csv, ///
        cells(b(star fmt(3)) se(par fmt(3))) stats(N r2 ar2) replace


margins, at(after17jul=(0 1) mng=(6 12)) //1sd -+:6.42 12.54 for 5 countries
	
	
xtreg distantCPcompl i.after17jul##c.pressfreedom $cons i.year, fe cluster(facID)
est store m1

xtreg distantCPcompl i.after17jul##ib3.buyer1continentnum $cons i.year,fe cluster(facID)
est store m2

xtreg distantCPcompl i.after17jul##c.NoBWbuyersmissingas0 $cons i.year, fe cluster(facID)
est store m3

xtreg distantCPcompl i.after17jul##c.Cycle $cons i.year, fe cluster(facID)
est store m4

xtreg distantCPcompl i.after17jul##c.mng $cons i.year,fe cluster(facID)
est store m5

xtreg distantCPcompl i.after17jul##c.size $cons i.year,fe cluster(facID)
est store m6

xtreg distantCPcompl i.after17jul##i.union $cons i.year,fe cluster(facID)
est store m7



 margins, at (after17jul=(0 1) NoBWbuyersmissingas0=(0 2)) atmeans noestimcheck
 marginsplot





**** after17jul interacts with buyer 1 country pressfreedom

xtreg disclosedcomplnofreeunion i.after17jul##c.buyer1pressfreedom $cons i.year, fe cluster(facID)
est store m1

xtreg disclosedcomplnofreeunion i.after17jul##c.buyer1pressfreedom $cons i.year if Country=="Vietnam", fe cluster(facID)
est store m2

xtreg disclosedcomplnofreeunion i.after17jul##c.buyer1pressfreedom $cons i.year if Country=="Indonesia", fe cluster(facID)
est store m3

xtreg disclosedcomplnofreeunion i.after17jul##c.buyer1pressfreedom $cons i.year if Country=="Jordan", fe cluster(facID)
est store m4


esttab m1 m2 m3 m4 using results.csv, ///
        cells(b(star fmt(3)) se(par fmt(3))) stats(N r2 ar2) replace



xtreg similarCPcompl i.after17jul##c.buyer1pressfreedom $cons i.year, fe cluster(facID)
est store m1

xtreg similarCPcompl i.after17jul##c.buyer1pressfreedom $cons i.year if Country=="Vietnam", fe cluster(facID)
est store m2

xtreg similarCPcompl i.after17jul##c.buyer1pressfreedom $cons i.year if Country=="Indonesia", fe cluster(facID)
est store m3

xtreg similarCPcompl i.after17jul##c.buyer1pressfreedom $cons i.year if Country=="Jordan", fe cluster(facID)
est store m4





xtreg distantCPcompl i.after17jul##c.buyer1pressfreedom $cons i.year, fe cluster(facID)
est store m1

xtreg distantCPcompl i.after17jul##c.buyer1pressfreedom $cons i.year if Country=="Vietnam", fe cluster(facID)
est store m2

xtreg distantCPcompl i.after17jul##c.buyer1pressfreedom $cons i.year if Country=="Indonesia", fe cluster(facID)
est store m3

xtreg distantCPcompl i.after17jul##c.buyer1pressfreedom $cons i.year if Country=="Jordan", fe cluster(facID)
est store m4









*****use 2nd audit cycle after July 2017 as the dividing point
***produce the same pattern of results

xtreg disclosedcompl i.after2auditdisc i.year, fe

xtreg similarCPcompl i.after2auditdisc i.year,fe

xtreg distantCPcompl i.after2auditdisc i.year,fe


xtreg disclosedcomplnoFOA i.after2auditdisc $cons discloseditems i.year,fe

xtreg disclosedcompl  i.after2auditdisc $cons discloseditems i.year, fe

xtreg similarCPcompl i.after2auditdisc $cons similarCPitems i.year, fe

xtreg distantCPcompl i.after2auditdisc $cons distantCPitems i.year,fe


xtreg disclosedcompl i.after2auditdisc##c.pressfreedom $cons discloseditems i.year, fe

xtreg similarCPcompl i.after2auditdisc##c.pressfreedom $cons similarCPitems i.year, fe

xtreg distantCPcompl i.after2auditdisc##c.pressfreedom $cons distantCPitems  i.year,fe


margins, at(after17jul=(0 1) pressfreedom =(24.1 60.5)) atmeans   
marginsplot


xtreg disclosedcompl i.after2auditdisc##c.NoBWbuyersmissingas0 $cons discloseditems i.year, fe

xtreg similarCPcompl i.after2auditdisc##c.NoBWbuyersmissingas0 $cons similarCPitems i.year, fe

xtreg distantCPcompl i.after2auditdisc##c.NoBWbuyersmissingas0 $cons distantCPitems i.year, fe


margins, at(after17jul=(0 1) buyer1BWmissingas0 =(0 1)) atmeans noestimcheck  
marginsplot

margins, at(after17jul=(0 1) NoBWbuyersmissingas0 =(0 2)) atmeans noestimcheck  



xtreg disclosedcompl i.after2auditdisc##c.Cycle $cons discloseditems i.year, fe

xtreg similarCPcompl i.after2auditdisc##c.Cycle $cons similarCPitems i.year, fe

xtreg distantCPcompl i.after2auditdisc##c.Cycle $cons distantCPitems i.year, fe

margins, at(after17jul=(0 1) Cycle=(2 8))  //1 sd +-: 2.26 vs. 7.92
marginsplot



xtreg disclosedcompl i.after2auditdisc##c.mng $cons discloseditems i.year,fe

xtreg similarCPcompl i.after2auditdisc##c.mng $cons similarCPitems i.year,fe

xtreg distantCPcompl i.after2auditdisc##c.mng $cons distantCPitems i.year,fe

margins, at(after17jul=(0 1) mng =(6 13)) atmeans
marginsplot


xtreg disclosedcompl i.after2auditdisc##i.union $cons discloseditems i.year,fe

xtreg similarCPcompl i.after2auditdisc##i.union $cons similarCPitems i.year,fe

xtreg distantCPcompl i.after2auditdisc##i.union $cons distantCPitems i.year,fe






****** control for yearforpanel produce very similar results
xtreg disclosedcomplnoFOA i.after2auditdisci.yearforpanel, fe

xtreg disclosedcompl i.after2auditdisci.yearforpanel, fe

xtreg similarCPcompl i.after2auditdisci.yearforpanel,fe

xtreg distantCPcompl i.after2auditdisci.yearforpanel,fe


xtreg disclosedcomplnoFOA i.after2auditdisc$cons discloseditems i.yearforpanel, fe

xtreg disclosedcompl i.after2auditdisc$cons discloseditems i.yearforpanel, fe

xtreg similarCPcompl i.after2auditdisc$cons similarCPitems i.yearforpanel, fe

xtreg distantCPcompl i.after2auditdisc$cons distantCPitems i.yearforpanel,fe


xtreg disclosedcompl i.after17jul##c.pressfreedom $cons discloseditems i.yearforpanel, fe

xtreg similarCPcompl i.after17jul##c.pressfreedom $cons similarCPitems i.yearforpanel, fe

xtreg distantCPcompl i.after17jul##c.pressfreedom $cons distantCPitems  i.yearforpanel,fe


margins, at(after17jul=(0 1) pressfreedom =(24.1 60.5)) atmeans   
marginsplot



xtreg disclosedcompl i.after17jul##c.ruleoflaw $cons discloseditems i.yearforpanel, fe

xtreg similarCPcompl i.after17jul##c.ruleoflaw $cons similarCPitems i.yearforpanel, fe

xtreg distantCPcompl i.after17jul##c.ruleoflaw $cons distantCPitems  i.yearforpanel,fe


margins,at(after17jul=(0 1) ruleoflaw=(.48 .55)) atmeans
marginsplot



xtreg disclosedcompl i.after17jul##c.NoBWbuyersmissingas0 $cons discloseditems i.yearforpanel, fe

xtreg similarCPcompl i.after17jul##c.NoBWbuyersmissingas0 $cons similarCPitems i.yearforpanel, fe

xtreg distantCPcompl i.after17jul##c.NoBWbuyersmissingas0 $cons distantCPitems i.yearforpanel, fe


margins, at(after17jul=(0 1) buyer1BWmissingas0 =(0 1)) atmeans noestimcheck  
marginsplot

margins, at(after17jul=(0 1) NoBWbuyersmissingas0 =(0 2)) atmeans noestimcheck  



xtreg disclosedcompl i.after17jul##c.Cycle $cons discloseditems i.yearforpanel, fe

xtreg similarCPcompl i.after17jul##c.Cycle $cons similarCPitems i.yearforpanel, fe

xtreg distantCPcompl i.after17jul##c.Cycle $cons distantCPitems i.yearforpanel, fe

margins, at(after17jul=(0 1) Cycle=(2 8))  //1 sd +-: 2.26 vs. 7.92
marginsplot



xtreg disclosedcompl i.after17jul##c.mng $cons discloseditems i.yearforpanel,fe

xtreg similarCPcompl i.after17jul##c.mng $cons similarCPitems i.yearforpanel,fe

xtreg distantCPcompl i.after17jul##c.mng $cons distantCPitems i.yearforpanel,fe

margins, at(after17jul=(0 1) mng =(6 13)) atmeans
marginsplot


xtreg disclosedcompl i.after17jul##i.CBA $cons discloseditems i.yearforpanel,fe

xtreg similarCPcompl i.after17jul##i.CBA $cons similarCPitems i.yearforpanel,fe

xtreg distantCPcompl i.after17jul##i.CBA $cons distantCPitems i.yearforpanel,fe


margins, at(after17jul=(0 1) union=(0 1)) atmeans
marginsplot



xtreg disclosedcompl i.after17jul##ib3.buyer1continentnum $cons i.year,fe

xtreg similarCPcompl i.after17jul##ib3.buyer1continentnum $cons i.year,fe

xtreg distantCPcompl i.after17jul##ib3.buyer1continentnum $cons i.year,fe




