########################################################################
#		Apply DiD methods to labor audits data
#		Yuequan
#		240623|250729
########################################################################



# preparation
	## install R packages
	# install.packages("")

	## load R packages
	lib <- c("haven","writexl","tidyverse","plm","did","mediation","modelsummary","ggeffects")
	lapply(lib,require,character.only=TRUE)

	## clear up space
	rm(list=ls())

	## helper functions to extract mediation results
	.medResult <- function(med_result) {
		summary_data <- summary(med_result)

		df <- data.frame(
			Effect = c("ACME", "ADE", "Total Effect", "Prop. Mediated", "Sample Size", "Simulations"),
			Estimate = c(summary_data$d0, summary_data$z0, summary_data$tau.coef, summary_data$n0, summary_data$nobs, summary_data$sims),
			CI_Lower = c(summary_data$d0.ci[1], summary_data$z0.ci[1], summary_data$tau.ci[1], summary_data$n0.ci[1], NA, NA),
			CI_Upper = c(summary_data$d0.ci[2], summary_data$z0.ci[2], summary_data$tau.ci[2], summary_data$n0.ci[2], NA, NA),
			P_Value = c(summary_data$d0.p, summary_data$z0.p, summary_data$tau.p, summary_data$n0.p, NA, NA)
		)

		return(df)
	}



# read in clean data
	dt <- read_dta("2025 Aug data for analysis.dta")

	dt2 <- dt |>
		filter(dmy(AssesmentDate)<ymd(20200301)) |> # before COVID19
		drop_na(mngindex13,union) |> # ,femalepc,regularwkpc,size,factoryageln # remove rows w/ missing values
		mutate(
			fid = as.factor(FactoryAssessedID),
			AssesmentDate = dmy(AssesmentDate),
			ym = floor_date(AssesmentDate, unit="month"),
			y = year(AssesmentDate),
			T2 = case_when(
				Country=="Vietnam" 		& AssesmentDate>=ymd(20160601)	~ "1",
				Country=="Jordan" 		& AssesmentDate>=ymd(20161101)	~ "1",
				Country=="Indonesia" 	& AssesmentDate>=ymd(20170101)	~ "1",
				Country=="Haiti" 			& AssesmentDate>=ymd(20170701)	~ "1",
				Country=="Nicaragua" 	& AssesmentDate>=ymd(20180101)	~ "1",
				TRUE 																									~ "0"
				) %>% factor(levels=c("0","1"),labels=c("Before","After")), # staggered treatment
			# T2r = case_when(
			# 	Country=="Vietnam" 		& AssesmentDate>=ymd(20160601)	~ "1",
			# 	Country=="Jordan" 		& AssesmentDate>=ymd(20161101)	~ "1",
			# 	Country=="Indonesia" 	& AssesmentDate>=ymd(20170101)	~ "1",
			# 	# Country=="Haiti" 			& AssesmentDate>=ymd(20170701)	~ "1",
			# 	Country=="Nicaragua" 	& AssesmentDate>=ymd(20180101)	~ "1",
			# 	Country=="Haiti" 			& AssesmentDate>=ymd(20100101)	~ "1",
			# 	Country=="Cambodia"		& AssesmentDate>=ymd(20140301)	~ "1",
			# 	TRUE 																									~ "0"
			# 	) %>% factor(levels=c("0","1"),labels=c("Before","After")), # robust treatment
			T3 = case_when(
				Country=="Vietnam" 		& y>=2016	~ "1",
				Country=="Jordan" 		& y>=2016	~ "1",
				Country=="Indonesia" 	& y>=2017	~ "1",
				Country=="Haiti" 			& y>=2017	~ "1",
				Country=="Nicaragua" 	& y>=2018	~ "1",
				TRUE 														~ "0"
				) %>% factor(levels=c("0","1"),labels=c("Before","After")), # staggered treatment by year
			# T3r = case_when(
			# 	Country=="Vietnam" 		& y>=2016	~ "1",
			# 	Country=="Jordan" 		& y>=2016	~ "1",
			# 	Country=="Indonesia" 	& y>=2017	~ "1",
			# 	# Country=="Haiti" 			& y>=2017	~ "1",
			# 	Country=="Nicaragua" 	& y>=2018	~ "1",
			# 	Country=="Haiti" 			& y>=2010	~ "1",
			# 	Country=="Cambodia"		& y>=2014	~ "1",
			# 	TRUE 														~ "0"
			# 	) %>% factor(levels=c("0","1"),labels=c("Before","After")), # robust treatment by year
			T3r2 = case_when(
				Country %in% c("Haiti", "Cambodia") ~ NA_character_,
				Country=="Vietnam" 		& y>=2016	~ "1",
				Country=="Jordan" 		& y>=2016	~ "1",
				Country=="Indonesia" 	& y>=2017	~ "1",
				Country=="Nicaragua" 	& y>=2018	~ "1",
				TRUE 														~ "0"
				) %>% factor(levels=c("0","1"),labels=c("Before","After")), # robust treatment by year, NA for Haiti and Cambodia
			T3r3 = case_when(
				Country %in% c("Bangladesh", "Cambodia") ~ NA_character_,
				Country=="Vietnam" 		& y>=2016	~ "1",
				Country=="Jordan" 		& y>=2016	~ "1",
				Country=="Indonesia" 	& y>=2017	~ "1",
				Country=="Haiti" 			& y>=2017	~ "1",
				Country=="Nicaragua" 	& y>=2018	~ "1",
				TRUE 														~ "0"
				) %>% factor(levels=c("0","1"),labels=c("Before","After")), # robust treatment by year, NA for Bangladesh and Cambodia
			)
	# names(dt2)
	# with(dt2, table(mngindex13, exclude=NULL))
	# pdt2 <- pdata.frame(dt2, index=c("fid","ym"))
	pdt2 <- dt2 |>
		mutate(n = row_number(), .by=c(fid, y)) |>
		mutate(# treat duplicated fid in the same year as new factories
			fid2 = if_else(n>1, paste0(FactoryAssessedID, "_", n), as.character(FactoryAssessedID)) %>% as.factor
			) |>
		pdata.frame(index=c("fid2","y")) #,"Country" # add Country to the index to allow for clustering

	dt3 <- dt |>
		mutate(
			AssesmentDate = dmy(AssesmentDate),
			fid = as.integer(FactoryAssessedID),
			ym = format(AssesmentDate, "%Y%m") %>% as.integer, ym_r = dense_rank(ym),
			T2_ym = case_when(
				Country=="Vietnam" 			~ 18, # AssesmentDate>=ymd(20160601)
				Country=="Jordan" 			~ 23, # AssesmentDate>=ymd(20161101)
				Country=="Indonesia" 		~ 25, # AssesmentDate>=ymd(20170101)
				Country=="Haiti" 				~ 31, # AssesmentDate>=ymd(20170701)
				Country=="Nicaragua" 		~ 37, # AssesmentDate>=ymd(20180101)
				TRUE 										~ 0
				)
			) |>
		filter(AssesmentDate<ymd(20200301)) |>
		drop_na(mngindex13,union,femalepc,regularwkpc,size,factoryageln)
	# str(dt3$ym); dt3 |> arrange(ym) |> distinct(ym,ym_r) |> filter(ym==201801) |> print(n=Inf) # look up ym_r for the corresponding ym
	# cannot conduct robustness analysis such as in TWFE because the method excludes units that have no untreated obs (i.e., factories in Haiti and Cambodia will be excluded)



# Mediation analysis
	set.seed(250806)

	vars <- c("overallcompl","nondisclosedrt","mngindex13","mngindex8","union","T3","RRic2010")
	wdt2 <- map_dfc(vars, ~ Within(pdt2[[.x]], effect = "twoways")) |> set_names(vars)

## mediation
	med1.fit <- lm(mngindex13 ~ -1 + T3, data=wdt2)
	out1.fit <- lm(overallcompl ~ -1 + mngindex13 + T3, data=wdt2)
	med1.out <- mediate(med1.fit, out1.fit, treat = "T3", mediator = "mngindex13", boot = T)

	med2.fit <- lm(mngindex13 ~ -1 + T3, data=wdt2)
	out2.fit <- lm(nondisclosedrt ~ -1 + mngindex13 + T3, data=wdt2)
	med2.out <- mediate(med2.fit, out2.fit, treat = "T3", mediator = "mngindex13", boot = T)

	med3.fit <- lm(mngindex8 ~ -1 + T3, data=wdt2)
	out3.fit <- lm(overallcompl ~ -1 + mngindex8 + T3, data=wdt2)
	med3.out <- mediate(med3.fit, out3.fit, treat = "T3", mediator = "mngindex8", boot = T)

	med4.fit <- lm(mngindex8 ~ -1 + T3, data=wdt2)
	out4.fit <- lm(nondisclosedrt ~ -1 + mngindex8 + T3, data=wdt2)
	med4.out <- mediate(med4.fit, out4.fit, treat = "T3", mediator = "mngindex8", boot = T)

	med5.fit <- lm(union ~ -1 + T3, data=wdt2)
	out5.fit <- lm(overallcompl ~ -1 + union + T3, data=wdt2)
	med5.out <- mediate(med5.fit, out5.fit, treat = "T3", mediator = "union", boot = T)

	med6.fit <- lm(union ~ -1 + T3, data=wdt2)
	out6.fit <- lm(nondisclosedrt ~ -1 + union + T3, data=wdt2)
	med6.out <- mediate(med6.fit, out6.fit, treat = "T3", mediator = "union", boot = T)


## moderated mediation
	mmed1.fit <- lm(mngindex13 ~ -1 + T3*RRic2010, data=wdt2)
	mout1.fit <- lm(overallcompl ~ -1 + mngindex13*RRic2010 + T3*RRic2010, data=wdt2)
	mmed1.out.l <- mediate(mmed1.fit, mout1.fit, treat = "T3", mediator = "mngindex13", covariates = list(RRic2010 = 0), boot = T)
	mmed1.out.h <- mediate(mmed1.fit, mout1.fit, treat = "T3", mediator = "mngindex13", covariates = list(RRic2010 = 2), boot = T)
	mmed1.out <- mediate(mmed1.fit, mout1.fit, treat = "T3", mediator = "mngindex13", boot = T)
	test.modmed(mmed1.out, covariates.1 = list(RRic2010 = 0), covariates.2 = list(RRic2010 = 2))

	mmed2.fit <- lm(mngindex13 ~ -1 + T3*RRic2010, data=wdt2)
	mout2.fit <- lm(nondisclosedrt ~ -1 + mngindex13*RRic2010 + T3*RRic2010, data=wdt2)
	mmed2.out.l <- mediate(mmed2.fit, mout2.fit, treat = "T3", mediator = "mngindex13", covariates = list(RRic2010 = 0), boot = T)
	mmed2.out.h <- mediate(mmed2.fit, mout2.fit, treat = "T3", mediator = "mngindex13", covariates = list(RRic2010 = 2), boot = T)
	mmed2.out <- mediate(mmed2.fit, mout2.fit, treat = "T3", mediator = "mngindex13", boot = T)
	test.modmed(mmed2.out, covariates.1 = list(RRic2010 = 0), covariates.2 = list(RRic2010 = 2))

	mmed3.fit <- lm(mngindex8 ~ -1 + T3*RRic2010, data=wdt2)
	mout3.fit <- lm(overallcompl ~ -1 + mngindex8*RRic2010 + T3*RRic2010, data=wdt2)
	mmed3.out.l <- mediate(mmed3.fit, mout3.fit, treat = "T3", mediator = "mngindex8", covariates = list(RRic2010 = 0), boot = T)
	mmed3.out.h <- mediate(mmed3.fit, mout3.fit, treat = "T3", mediator = "mngindex8", covariates = list(RRic2010 = 2), boot = T)
	mmed3.out <- mediate(mmed3.fit, mout3.fit, treat = "T3", mediator = "mngindex8", boot = T)
	test.modmed(mmed3.out, covariates.1 = list(RRic2010 = 0), covariates.2 = list(RRic2010 = 2))

	mmed4.fit <- lm(mngindex8 ~ -1 + T3*RRic2010, data=wdt2)
	mout4.fit <- lm(nondisclosedrt ~ -1 + mngindex8*RRic2010 + T3*RRic2010, data=wdt2)
	mmed4.out.l <- mediate(mmed4.fit, mout4.fit, treat = "T3", mediator = "mngindex8", covariates = list(RRic2010 = 0), boot = T)
	mmed4.out.h <- mediate(mmed4.fit, mout4.fit, treat = "T3", mediator = "mngindex8", covariates = list(RRic2010 = 2), boot = T)
	mmed4.out <- mediate(mmed4.fit, mout4.fit, treat = "T3", mediator = "mngindex8", boot = T)
	test.modmed(mmed4.out, covariates.1 = list(RRic2010 = 0), covariates.2 = list(RRic2010 = 2))

	mmed5.fit <- lm(union ~ -1 + T3*RRic2010, data=wdt2)
	mout5.fit <- lm(overallcompl ~ -1 + union*RRic2010 + T3*RRic2010, data=wdt2)
	mmed5.out.l <- mediate(mmed5.fit, mout5.fit, treat = "T3", mediator = "union", covariates = list(RRic2010 = 0), boot = T)
	mmed5.out.h <- mediate(mmed5.fit, mout5.fit, treat = "T3", mediator = "union", covariates = list(RRic2010 = 2), boot = T)
	mmed5.out <- mediate(mmed5.fit, mout5.fit, treat = "T3", mediator = "union", boot = T)
	test.modmed(mmed5.out, covariates.1 = list(RRic2010 = 0), covariates.2 = list(RRic2010 = 2))

	mmed6.fit <- lm(union ~ -1 + T3*RRic2010, data=wdt2)
	mout6.fit <- lm(nondisclosedrt ~ -1 + union*RRic2010 + T3*RRic2010, data=wdt2)
	mmed6.out.l <- mediate(mmed6.fit, mout6.fit, treat = "T3", mediator = "union", covariates = list(RRic2010 = 0), boot = T)
	mmed6.out.h <- mediate(mmed6.fit, mout6.fit, treat = "T3", mediator = "union", covariates = list(RRic2010 = 2), boot = T)
	mmed6.out <- mediate(mmed6.fit, mout6.fit, treat = "T3", mediator = "union", boot = T)
	test.modmed(mmed6.out, covariates.1 = list(RRic2010 = 0), covariates.2 = list(RRic2010 = 2))

## write results
	write_xlsx(
		list(
			"overall_mng13" = .medResult(med1.out), 
			"nondisclosed_mng13" = .medResult(med2.out),
			"overall_mng8" = .medResult(med3.out), 
			"nondisclosed_mng8" = .medResult(med4.out),
			"overall_union" = .medResult(med5.out),
			"nondisclosed_union" = .medResult(med6.out),
			"overall_mng13_ml" = .medResult(mmed1.out.l), 
			"overall_mng13_mh" = .medResult(mmed1.out.h), 
			"nondisclosed_mng13_ml" = .medResult(mmed2.out.l),
			"nondisclosed_mng13_mh" = .medResult(mmed2.out.h),
			"overall_mng8_ml" = .medResult(mmed3.out.l), 
			"overall_mng8_mh" = .medResult(mmed3.out.h),
			"nondisclosed_mng8_ml" = .medResult(mmed4.out.l),
			"nondisclosed_mng8_mh" = .medResult(mmed4.out.h),
			"overall_union_ml" = .medResult(mmed5.out.l),
			"overall_union_mh" = .medResult(mmed5.out.h),
			"nondisclosed_union_ml" = .medResult(mmed6.out.l),
			"nondisclosed_union_mh" = .medResult(mmed6.out.h)
			), 
		path = "mediation_results.xlsx"
		)



# DiD by two-way fixed effects panel models
	dvs <- c("reportedcompl","similarCPcompl","distantCPcompl")
	mos <- c("buyer1FTindexband","RRic2010","mngindex13","union")


## event study
	L <- 3; F <- 3

	dt2.es <- as.data.frame(pdt2) |>
		mutate(
			cdgt = case_when(
				Country %in% c("Bangladesh", "Cambodia") ~ NA_real_,
				Country=="Vietnam" 		~ 2016,
				Country=="Jordan" 		~ 2016,
				Country=="Indonesia" 	~ 2017,
				Country=="Haiti" 			~ 2017,
				Country=="Nicaragua" 	~ 2018,
				TRUE 									~ 0 # never treated
			)
		)

	Dtl <- sapply(-L:F, function(l) {
		dtl <- 1*( (dt2.es$y == dt2.es$cdgt + l) & (dt2.es$cdgt > 0) )
		dtl
	}) |> as.data.frame()

	colnames(Dtl) <- c(paste0("Dtmin", L:1), paste0("Dt", 0:F))
	dt2.es <- cbind(dt2.es, Dtl)

	es.dv <- c("overallcompl","nondisclosedrt")
	
	es.f <- lapply(es.dv, function(x) {
		formula(paste0(x,"~ Dtmin3 + Dtmin2 + Dt0 + Dt1 + Dt2 + Dt3"))
	})
	
	es.m <- lapply(es.f, function(x) plm(x, data = dt2.es, model = "within", effect = "twoways", index=c("fid2","y")))

	idx.pre <- 1:L-1    # pre-treatment periods (excluding omitted -1)
	idx.post <- L:(L+F)  # treatment and post-treatment periods
	
	es.dt <- lapply(seq_along(es.m), function(i) {
		coefs1 <- coef(es.m[[i]])[1:(L+F)]
		ses1 <- summary(es.m[[i]], vcov=function(y) vcovHC(y, method="ar"))$coefficients[,2][1:(L+F)]
		coefs <- c(coefs1[idx.pre], 0, coefs1[idx.post])  # add 0 for omitted period
		ses <- c(ses1[idx.pre], 0, ses1[idx.post])        # add 0 for omitted period
		exposure <- -L:F
		cmat <- data.frame(coefs=coefs, ses=ses, exposure=exposure, m=es.dv[[i]])
	}) %>% do.call(rbind,.) %>% mutate(m=ifelse(m=="overallcompl","Overall Compliance","Non-disclosed Compliance"))

	rect <- data.frame(xmin=-(L+.1), xmax=-0.9, ymin=-Inf, ymax=Inf)
	ggplot(es.dt, aes(y=coefs, x=exposure)) +
		geom_line(color="#00274C") + geom_point(size=1.5) +
		geom_hline(yintercept=0, linetype="dashed") +
		geom_errorbar(aes(ymin=(coefs-1.96*ses), ymax=(coefs+1.96*ses)), width=0.2) +
		facet_grid(.~m) +
		theme_bw() +
		labs(x="Exposure", y="Coefficients") +
		scale_x_continuous(breaks=-L:F, minor_breaks=NULL) +
		geom_rect(data=rect, inherit.aes=FALSE, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="transparent", fill="#FFCB05", alpha=0.2)


## main
	T <- "T3" # set to "T2" or "T3" to switch between treatments

	fss <- sapply(dvs, function(dv) formula(paste0(dv," ~ ", T)))
	fs <- sapply(dvs, function(dv) formula(paste0(dv," ~ ", T, " + buyer1FTindexband + RRic2010 + mngindex13 + union + femalepc + regularwkpc + size + factoryageln + Cycle")))
	fms <- lapply(dvs, function(dv) {
		lapply(mos, function(mo) {
			formula(paste0(dv, " ~ ", T, "*", mo, " + ", paste(mos[-which(mos==mo)], collapse=" + "), " + femalepc + regularwkpc + size + factoryageln + Cycle"))
			})
		}) |> unlist(use.names=FALSE)

	mss <- lapply(fss, function(f) plm(f, data=pdt2, effect="twoways", model="within"))
	lapply(mss, function(m) summary(m, vcov=function(x) vcovHC(x, method="ar")))
	lapply(mss, function(m) summary(m, vcov=function(x) clubSandwich::vcovCR(x, cluster="Country", type="CR0")))
	clubSandwich::vcovCR(mss[[3]], cluster="Country", type="CR0")
	clubSandwich::vcovCR(mss[[3]], cluster="Country", type="CR1")

	ms <- lapply(fs, function(f) plm(f, data=pdt2, effect="twoways", model="within"))
	# lapply(ms, function(m) summary(m, vcov=function(x) vcovHC(x, method="ar")))
	mms <- lapply(fms, function(fm) plm(fm, data=pdt2, effect="twoways", model="within"))
	# lapply(mms[3], function(mm) summary(mm, vcov=function(x) vcovHC(x, method="ar")))


## robust (recoding Cambodia and Haiti as treatment)
	# rfs <- sapply(dvs, function(dv) formula(paste0(dv," ~ T2r + buyer1FTindexband + RRic2010 + mngindex13 + union + femalepc + regularwkpc + size + factoryageln + Cycle")))
	# rfms <- lapply(dvs, function(dv) {
	# 	lapply(mos, function(mo) {
	# 		formula(paste0(dv, " ~ T2r*", mo, " + ", paste(mos[-which(mos==mo)], collapse=" + "), " + femalepc + regularwkpc + size + factoryageln + Cycle"))
	# 		})
	# 	}) |> unlist(use.names=FALSE)

	# rms <- lapply(rfs, function(rf) plm(rf, data=pdt2, effect="twoways", model="within"))
	# # lapply(rms[1:4], function(rm) summary(rm, vcov=function(x) vcovHC(x, method="ar")))
	# rmms <- lapply(rfms, function(rfm) plm(rfm, data=pdt2, effect="twoways", model="within"))
	# # lapply(rmms[3], function(rmm) summary(rmm, vcov=function(x) vcovHC(x, method="ar")))


## regression tables
	modelsummary(c(mss,ms), output="main_new.xlsx", vcov=function(x) plm::vcovHC(x,method="ar"), stars=TRUE)
	modelsummary(mms, output="moderators.xlsx", vcov=function(x) plm::vcovHC(x,method="ar"), stars=TRUE)

	modelsummary(rms, output="rmain.xlsx", vcov=function(x) plm::vcovHC(x,method="ar"), stars=TRUE)
	modelsummary(rmms, output="rmoderators.xlsx", vcov=function(x) plm::vcovHC(x,method="ar"), stars=TRUE)


## moderation plots
	options(ggeffects_margin = "empirical")

	predict_response(mms[[1]], terms=c("T2",mos[1]), vcov_fun=plm::vcovHC, vcov_args=list(method="ar")) |> plot(show_title=FALSE, show_x_title=FALSE, connect_lines=TRUE, name="Predicted compliance for reported standards") + labs(colour="Fashion Transporancy Index Band (buyer 1)") + theme(legend.position = "bottom")
	ggsave(paste0("moderator_",mos[1],".png"))

	predict_response(mms[[2]], terms=c("T2",mos[2]), vcov_fun=plm::vcovHC, vcov_args=list(method="ar")) |> plot(show_title=FALSE, show_x_title=FALSE, connect_lines=TRUE, name="Predicted compliance for reported standards") + labs(colour="RepRisk Incidents (since 2010)") + theme(legend.position = "bottom") + scale_colour_brewer(palette = "Set1", labels = c("-1 SD", "Mean", "+1 SD"))
	ggsave(paste0("moderator_",mos[2],".png"))

	predict_response(mms[[3]], terms=c("T2",mos[3]), vcov_fun=plm::vcovHC, vcov_args=list(method="ar")) |> plot(show_title=FALSE, show_x_title=FALSE, connect_lines=TRUE, name="Predicted compliance for reported standards") + labs(colour="Management System Index") + theme(legend.position = "bottom") + scale_colour_brewer(palette = "Set1", labels = c("-1 SD", "Mean", "+1 SD"))
	ggsave(paste0("moderator_",mos[3],".png"))

	predict_response(mms[[4]], terms=c("T2",mos[4]), vcov_fun=plm::vcovHC, vcov_args=list(method="ar")) |> plot(show_title=FALSE, show_x_title=FALSE, connect_lines=TRUE, name="Predicted compliance for reported standards") + labs(colour="Union") + theme(legend.position = "bottom") + scale_colour_brewer(palette = "Set1", labels = c("No", "Yes"))
	ggsave(paste0("moderator_",mos[4],".png"))


	predict_response(rmms[[1]], terms=c("T2r",mos[1]), vcov_fun=plm::vcovHC, vcov_args=list(method="ar")) |> plot(show_title=FALSE, show_x_title=FALSE, connect_lines=TRUE, name="Predicted compliance for reported standards") + labs(colour="Fashion Transporancy Index Band (buyer 1)") + theme(legend.position = "bottom")
	ggsave(paste0("rmoderator_",mos[1],".png"))

	predict_response(rmms[[2]], terms=c("T2r",mos[2]), vcov_fun=plm::vcovHC, vcov_args=list(method="ar")) |> plot(show_title=FALSE, show_x_title=FALSE, connect_lines=TRUE, name="Predicted compliance for reported standards") + labs(colour="RepRisk Incidents (since 2010)") + theme(legend.position = "bottom") + scale_colour_brewer(palette = "Set1", labels = c("-1 SD", "Mean", "+1 SD"))
	ggsave(paste0("rmoderator_",mos[2],".png"))

	predict_response(rmms[[3]], terms=c("T2r",mos[3]), vcov_fun=plm::vcovHC, vcov_args=list(method="ar")) |> plot(show_title=FALSE, show_x_title=FALSE, connect_lines=TRUE, name="Predicted compliance for reported standards") + labs(colour="Management System Index") + theme(legend.position = "bottom") + scale_colour_brewer(palette = "Set1", labels = c("-1 SD", "Mean", "+1 SD"))
	ggsave(paste0("rmoderator_",mos[3],".png"))

	predict_response(rmms[[4]], terms=c("T2r",mos[4]), vcov_fun=plm::vcovHC, vcov_args=list(method="ar")) |> plot(show_title=FALSE, show_x_title=FALSE, connect_lines=TRUE, name="Predicted compliance for reported standards") + labs(colour="Union") + theme(legend.position = "bottom") + scale_colour_brewer(palette = "Set1", labels = c("No", "Yes"))
	ggsave(paste0("rmoderator_",mos[4],".png"))


## TESTING CODES
	# m_r_2 <- plm(reportedcompl ~ T2 + buyer1FTindexband + RRic2010 + mngindex13 + union + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym"))
	# summary(m_r_2, vcov=function(x) vcovHC(x, method="ar"))

	# m_r_2b <- plm(reportedcompl ~ T2*buyer1FTindexband + RRic2010 + mngindex13 + union + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym"))
	# summary(m_r_2b, vcov=function(x) vcovHC(x, method="ar"))
	# m_r_2n <- plm(reportedcompl ~ T2*RRic2010 + buyer1FTindexband + mngindex13 + union + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym"))
	# summary(m_r_2n, vcov=function(x) vcovHC(x, method="ar"))
	# m_r_2m <- plm(reportedcompl ~ T2*mngindex13 + buyer1FTindexband + RRic2010 + union + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym"))
	# summary(m_r_2m, vcov=function(x) vcovHC(x, method="ar"))
	# m_r_2u <- plm(reportedcompl ~ T2*union + buyer1FTindexband + RRic2010 + mngindex13 + femalepc + regularwkpc + size + factoryageln + Cycle, data=dt2, effect="twoways", model="within", index=c("fid","ym"))
	# summary(m_r_2u, vcov=function(x) vcovHC(x, method="ar"))

	# sapply(1:4, function(i) {
	# 	predict_response(mms[[i]], terms=c("T2",mos[i]), vcov_fun=plm::vcovHC, vcov_args=list(method="ar")) |> plot(connect_lines=TRUE)
	# 	ggsave(paste0("moderator_",mos[i],".png"))
	# 	})

	# sapply(1:4, function(i) {
	# 	predict_response(rmms[[i]], terms=c("T2r",mos[i]), vcov_fun=plm::vcovHC, vcov_args=list(method="ar")) |> plot(connect_lines=TRUE)
	# 	ggsave(paste0("rmoderator_",mos[i],".png"))
	# 	})



# DiD by Callaway and Sant'Anna (2021)
	dvs_labels <- c("Reported","Similar","Distant")

## group-time ATEs for each of three outcomes
	m_gts <- lapply(dvs, function(dv) {
		set.seed(240921)
		att_gt(
			yname = dv, idname = "fid", tname = "ym_r", gname = "T2_ym",
			xformla = NULL, data = dt3,
			panel = TRUE, allow_unbalanced_panel = TRUE, clustervars = NULL,
			control_group = "nevertreated", #"notyettreated"
			est_method = "dr"
			)
		})
	# use the following code to understand the warning messages (in short, due to unbalanced panel and therefore missingness in certain periods)
	# subset(dt3, subset=!is.na(Country)) |> summarize(n=n(), .by=c(ym,ym_r, Country)) |> pivot_wider(names_from=Country, values_from=n) |> print(n=Inf)
	# the parallel trend assumption appears to be hold without covariates; including "size" and "factoryageln" as pre-treatment covariates does not substantially change the results

	for (i in seq_along(dvs)) {
		### pre-test the parallel assumption
		m_e <- aggte(m_gts[[i]], type="dynamic", na.rm=TRUE)
		# summary(m_e)
		ggdid(m_e) + scale_x_continuous(expand=c(0.01,0), n.breaks=20) + labs(title=paste(dvs_labels[i],"Items"), x="Exposure Periods")
		ggsave(paste0("DiD_dynamic_",dvs_labels[i],".png"), width=8, height=5)

		### summarize effects by country
		m_g <- aggte(m_gts[[i]], type="group", na.rm=TRUE)
		# summary(m_g)
		ggdid(m_g) + scale_y_discrete(labels=c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua")) + labs(title=paste(dvs_labels[i],"Items"), y="Countries")
		ggsave(paste0("DiD_group_",dvs_labels[i],".png"), width=8, height=5)

		###	summarize the overall effect
		m_s <- aggte(m_gts[[i]], type="simple", na.rm=TRUE)
		sink(paste0("DiD_overall_",dvs_labels[i],".txt"))
		summary(m_s)
		sink()
	}


## TESTING CODES
	# m <- att_gt(
	# 	yname = "reportedcompl", idname = "fid", tname = "ym_r", gname = "T2_ym",
	# 	xformla = ~size+factoryageln, 
	# 	data = dt3,
	# 	# xformla = NULL, 
	# 	# data = subset(dt3, union==0),
	# 	panel = TRUE, allow_unbalanced_panel = TRUE, clustervars = NULL,
	# 	control_group = "nevertreated", #"notyettreated"
	# 	est_method = "ipw"
	# 	# est_method = "dr"
	# 	)
	# # warnings()

	# # summary(m)
	# ggdid(m) + theme(axis.text.y=element_text(size=10), axis.text.x=element_text(angle=90, vjust=0.5, hjust=1, size=10))

	# agg.es <- aggte(m, type="dynamic", na.rm=TRUE)
	# # summary(agg.es)
	# ggdid(agg.es) + scale_x_continuous(expand=c(0.01,0), n.breaks=10)

	# agg.cs <- aggte(m, type="calendar", na.rm=TRUE)
	# # summary(agg.cs)
	# ggdid(agg.cs) + scale_x_continuous(expand=c(0.01,0), guide=guide_axis(angle=90), n.breaks=10)

	# agg.gs <- aggte(m, type="group", na.rm=TRUE)
	# # summary(agg.gs)
	# ggdid(agg.gs) + scale_y_discrete(labels=c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua"))

	# agg.simple <- aggte(m, type="simple", na.rm=TRUE)
	# summary(agg.simple)

	### moderation
	# mods <- list(
	# 	dt3_union0 = subset(dt3, union==0), dt3_union1 = subset(dt3, union==1),
	# 	dt3_mngl = subset(dt3, mng<7), dt3_mngh = subset(dt3, mng>=7),
	# 	dt3_b1l = subset(dt3, as.integer(buyer1FTindexband)<=3), dt3_b1h = subset(dt3, as.integer(buyer1FTindexband)<=3)
	# 	)
	# # sapply(mods, dim)

	# m_gt_mod <- lapply(mods, function(mod) {
	# 	lapply(dvs, function(dv) {
	# 		att_gt(
	# 			yname = dv,
	# 			idname = "fid",
	# 			tname = "ym",
	# 			gname = "T2_ym",
	# 			xformla = NULL,
	# 			data = mod,
	# 			panel = TRUE,
	# 			allow_unbalanced_panel = TRUE,
	# 			control_group = "nevertreated", #"notyettreated"
	# 			clustervars = NULL,
	# 			est_method = "dr"
	# 			)
	# 		})
	# 	})

	# for (i in seq_along(mods)) {
	# 	for (j in seq_along(dvs)) {
	# 		m_t <- aggte(m_gt_mod[[i]][[j]], type="dynamic", na.rm=TRUE)
	# 		summary(m_t)
	# 		ggdid(m_t) + scale_x_continuous(expand=c(0.01,0), n.breaks=20) + labs(title="Reported", x="Exposure Periods")
	# 		# ggsave(paste0("DiD_",str_remove(names(mods),"^dt3_")[i],"_",dvs[j],"_dynamic.png"))

	# 		m_g <- aggte(m_gt_mod[[i]][[j]], type="group", na.rm=TRUE)
	# 		summary(m_g)
	# 		ggdid(m_g) + scale_y_discrete(labels=c("Vietnam","Jordan","Indonesia","Haiti","Nicaragua")) + labs(title="Reported", y="Countries")
	# 		# ggsave(paste0("DiD_",str_remove(names(mods),"^dt3_")[i],"_",dvs[j],"_group.png"))
	# 	}
	# }