# Accompanying R script to: "Analysis of overwintering outcomes in honey bee
# large-scale colony feeding studies using the BEEHAVE model"
# By:  Farah Abi-Akar, Amelie Schmolke, Colleen Roy, Nika Galic, Silvia Hinarejos
# This file is dated 2020-03-18, created in R version 3.6.3.

# First, download all files into one folder.  Identify that folder here:
  filefolder<- 'C:/Users/examplefolder'
# Second, ensure you have installed these packages.  Then, run these lines:
  library(plyr)
  library(ggplot2)
  library(reshape2)
  library(lmerTest)
  library(r2glmm)
  library(dplyr)


# LSCFS data: Import and preparation --------------------------------------

  #Identify the folder containing input files
    setwd(filefolder)

  #Import Colony Condition Assessments data from Colony Feeding Studies
    main<- read.csv("CCA_LSCFS_data.csv")
    main$apiary<- substr(main$colony,1,1)

  #Import overwintering outcome classifications
    cat <- read.csv("OverwinterOutcomes.csv") 
    cat$outcome<- factor(cat$outcome, levels=rev(levels(factor(cat$outcome))))
    cat$outcome2<- plyr::revalue(cat$outcome, c(Survived="Colony survived", 
                                             Lost="Colony lost")) #For graphing
    
  #Merge, format dates, make ratio of g honey per adult bee
    main2<- merge(main, cat, all=T);    rm(cat, main)
    main2$year<- as.factor(substr(main2$study, 7,10))
    main2$date <- as.Date(main2$date, format="%m/%d/%Y", tz="UTC")
    main2$ratio<- main2$honey.kg*1000/main2$adult.num 

  #Identify initial CCA of each study (varies by year)
    tab<- data.frame (year = c(seq(2013,2016)),   cca_first = c(3,3,3,2))
    main2<- merge(main2, tab, all=T);   rm(tab)
    
  #For graphing, align all years into one
    main2$dateonly<- as.Date (paste0 ("2013", substr(main2$date,5,10)), 
                              format="%Y-%m-%d", tz="America/Chicago")

  #Omit CCAs before initial (pre-placement) and after winter in the spring
    main2$ccan<- as.numeric (substr(main2$cca,4,5))
    main3<- subset(main2, ccan>=cca_first & doy>166);    rm(main2)
    

# LSCFS data: LOESS graphs --------------------------------------------------

  #1. Honey
    ggplot(main3, aes(x=dateonly, y=honey.kg, shape=outcome2)) +
        geom_point(aes(color=outcome2), size=.9, alpha=.6) + theme_bw() + 
        scale_shape_manual(values=c(16,15), guide=FALSE) +
        stat_smooth(aes(color=outcome2)) + ylab("Honey (kg)") + 
        scale_color_manual(values=c("#009E73", "#E69F00"), 
                           guide=guide_legend(title=NULL)) +
        theme(axis.title.x =element_blank(), legend.position="bottom")
      
  #2. Adult bees
     ggplot(main3, aes(x=dateonly, y=adult.num, shape=outcome2)) + 
         geom_point(aes(color=outcome2), size=.9, alpha=.6) + theme_bw() +
         scale_shape_manual(values=c(16,15), guide=FALSE) +
         stat_smooth(aes(color=outcome2)) + ylab("Adult bee number") +
         scale_color_manual(values=c("#009E73", "#E69F00"), 
                            guide=guide_legend(title=NULL)) +
         theme(axis.title.x =element_blank())

  #3. Ratio (Errors are OK, due to log of 0-pupae colonies)
     ggplot(main3, aes(x=dateonly, y=ratio, shape=outcome2)) + 
       geom_point(aes(color=outcome2), size=.9, alpha=.6) + theme_bw() +
       scale_shape_manual(values=c(16,15), guide=FALSE) +
       stat_smooth(aes(color=outcome2)) + ylab("Ratio honey/adults (g/bee)") +
       scale_color_manual(values=c("#009E73", "#E69F00"), 
                          guide=guide_legend(title=NULL)) +
       theme(axis.title.x =element_blank(), legend.position = "none", 
             panel.grid.minor.y=element_blank()) +
       scale_y_log10() + annotation_logticks(sides="l")

  #4. Pupae
     ggplot(main3, aes(x=dateonly, y=pupae.num, shape=outcome2)) + 
       geom_point(aes(color=outcome2), size=.9, alpha=.6) + theme_bw() +
       scale_shape_manual(values=c(16,15), guide=FALSE) +
       stat_smooth(aes(color=outcome2)) + ylab("Pupae number") +
       scale_color_manual(values=c("#009E73", "#E69F00"), 
                          guide=guide_legend(title=NULL)) +
       theme(axis.title.x =element_blank(), legend.position = "none")


# LSCFS data: Application of fall thresholds for overwintering ------------------------

  #Subset to fall CCAs only
    fall<- subset(main3, doy >284 )
    
  #THRESHOLDS ______________________________
   #1. Honey
      fall$thold_h<- ifelse(fall$honey.cells<4000, "Loss", "Mid")
      fall$thold_h [fall$honey.cells>60000] <- "Survival"
      fall$thold_h<- factor(fall$thold_h, levels=c("Survival", "Mid", "Loss"))
   #2. Adult bees
      fall$thold_b<- ifelse(fall$adult.num<4000, "Loss", "Mid")
      fall$thold_b [fall$adult.num>19000] <- "Survival"
      fall$thold_b<- factor(fall$thold_b, levels=c("Survival", "Mid", "Loss"))
   #1+2. Honey & adult bees
      fall$thold_hb [fall$thold_b=="Mid" & fall$thold_h=="Mid"] <- "Neither"
      fall$thold_hb [fall$thold_b=="Loss" | fall$thold_h=="Loss"] <- "Loss"
      fall$thold_hb [fall$thold_b=="Survival" | fall$thold_h=="Survival"] <- "Survival"
      fall$thold_hb <- factor(fall$thold_hb, levels=c("Survival","Loss","Neither"))
   #3. Ratio g honey/bee, where above thresholds don't apply
      fall$thold_r<- ifelse(fall$ratio>=2.2222, "Many", "Mid")
      fall$thold_r<- ifelse(fall$thold_hb=="Neither", fall$thold_r,NA )
   #4. Pupae, where above thresholds don't apply
      fall$thold_p<- ifelse(fall$pupae.num>=11000, "Many", "Mid")
      fall$thold_p<- ifelse(fall$thold_hb=="Neither" & fall$thold_r=="Mid",
                            fall$thold_p, NA )
      fall$thold_pG<- ifelse(fall$thold_p=="Mid", NA, fall$thold_p) #For graphing
   #1+2+3+4. All four
      fall$thold_hbrp <- fall$thold_hb
      fall$thold_hbrp [fall$thold_r=="Many" | fall$thold_p=="Many"] <- "Survival"
      fall$thold_hbrp<- factor(fall$thold_hbrp, levels=c("Survival","Loss","Neither"))


# LSCFS data: Thresholds graphs ---------------------------------------------
      
  #Prepare data for threshold box plot ______________________________
    #Melt
      fall2<- melt(fall, id.vars=c("study","colony","apiary","cca","date","doy",
                                       "outcome","thold_hb", "thold_r"), 
                     measure.vars=c("honey.kg", "adult.num","ratio", "pupae.num"))
    #Add thresholds, for dotted lines
      fall2$min <- ifelse(fall2$variable=="honey.kg", 2, 4000)
      fall2$min [fall2$variable %in% c("ratio","pupae.num") ]<- NA
      fall2$max <- ifelse(fall2$variable=="honey.kg", 30, 19000)
      fall2$max [fall2$variable=="ratio"]<- 2.22222
      fall2$max [fall2$variable=="pupae.num"]<- 11000
      fall2$variable<- revalue(fall2$variable, c('honey.kg'='Honey (kg)',
                                                 'adult.num'='Adult bees',
                                                 'ratio'='Ratio honey/adults (among remaining)',
                                                 'pupae.num'='Pupae (among remaining)'))
    #From the two secondary criteria, remove points that already have 
      # primary predictions.
      fall2$omit<- "."
      fall2$omit [fall2$thold_hb!="Neither" & 
                    fall2$variable=='Ratio honey/adults (among remaining)'] <- "omit"
      fall2$omit [fall2$thold_hbr!="Neither" & fall2$thold_r!="Mid" & 
                    fall2$variable=='Pupae (among remaining)'] <- "omit"
      fall3<- subset(fall2, omit==".", select=-omit);    rm(fall2)

  #Threshold box plot ______________________________
      ggplot(fall3, aes(x=outcome, y=value)) +
         geom_hline(aes(yintercept = max), color="#009E73", linetype=5, size=1) +
         geom_hline(aes(yintercept = min), color="#E69F00", linetype=5, size=1) +
         geom_jitter(width=.25, size=.9, color="gray" ) + 
         geom_boxplot(outlier.shape=NA, fill=NA) +  theme_bw() +
         facet_wrap(~variable, nrow=2, scales="free_x", strip.position = "bottom") + 
         stat_summary(fun.y="mean", geom="point", shape=5) + 
         coord_flip() + xlab("Overwintering result") + ylab(NULL)
      
  #Threshold scatter plot with shading ______________________________
      #ID three points of triangle, which identifies the ratio prediction area
      triangle<- data.frame (x=c(2.22222*4, 30, 30), y=c(4, 4, 1/2.22222*30))
      ggplot(fall, aes(x=honey.kg, y=adult.num/1000)) + theme_bw() +
        annotate("rect", ymin=0, ymax=4, xmin=0, xmax=Inf, alpha=0.2, fill="#E69F00") +
        annotate("rect", ymin=19, ymax=Inf, xmin=0, xmax=Inf, alpha=0.2, fill="#009E73") +
        annotate("rect", ymin=0, ymax=Inf, xmin=0, xmax=2, alpha=0.2, fill="#E69F00") +
        annotate("rect", ymin=0, ymax=Inf, xmin=30, xmax=Inf, alpha=0.2, fill="#009E73")  +
        geom_polygon(data=triangle, aes(x=x, y=y, group=1), fill="#009E73", alpha=.2) +
        geom_point(aes(shape=thold_pG), size=3) + 
        scale_shape_manual(values=c(15,16,24), guide=FALSE) +
        geom_point(aes(color=outcome2, shape=outcome2), alpha=.8, size=2) + 
        theme(legend.position="bottom") +
        xlab("Honey in October (kg)") + ylab("Adult bees in October (thousands)")+
        scale_color_manual(values=c("#009E73", "#E69F00"), guide=guide_legend(title=NULL))
      rm(triangle)
      

# LSCFS data: Statistical tests on thresholds -------------------------------

  #1. Honey thresholds           
     calc<- subset(fall, thold_h!="Mid") 
     calc$thold_h<- droplevels(calc$thold_h)
     table(calc$outcome, calc$thold_h, useNA="ifany")
     fisher.test(calc$outcome, calc$thold_h, conf.int=T, alternative="two.sided")
  #2. Adult bee thresholds 
     calc<- subset(fall, thold_b!="Mid") 
     calc$thold_b<- droplevels(calc$thold_b)
     table(calc$outcome, calc$thold_b, useNA="ifany")
     fisher.test(calc$outcome, calc$thold_b, conf.int=T, alternative="two.sided")
  #3. Ratio threshold
     calc<- subset(fall, thold_r=="Many") 
     table(calc$outcome, calc$thold_r, useNA="ifany")
     binom.test(14,16, alternative="two.sided")
  #4. Pupae threshold
     calc<- subset(fall, thold_p=="Many") 
     table(calc$outcome, calc$thold_p, useNA="ifany")
     binom.test(8,9, alternative="two.sided") 

  #All: honey, adults, ratio, pupae (un-predicted visible as "Neither")
     table(fall$outcome, fall$thold_hbrp, useNA="ifany")


# Targeted simulations: Import and preparation ----------------------------

  #Import simulation outcomes (means of 20 replicates, once/month), format date & apiary
    setwd(filefolder) 
    sim<- read.csv ('TargetedSimsMeanOutputs.csv')
    sim$date<- as.Date(sim$date, format="%m/%d/%Y", tz="UTC")
    sim$day<- as.numeric(format(sim$date, "%j"))
    sim$year <- as.factor (substr(sim$study, 7,10))
    sim$Apiary <- paste(sim$study, sim$site, sep="-")

  #Identify keys/IDs with which to link scenario characteristics to table 
    sim$ID_initial <- substr(sim$assessment.file, 6,7) #Initial condition file ID
    sim$ID_feeding <- substr(sim$feeding.schedule.file, 11,12) #Feeding file ID
    sim$ID_feeding <- gsub("_","",sim$ID_feeding)
    sim<- subset(sim, select=-c(assessment.file, feeding.schedule.file))
    
  #Import the defined characteristics of the BEEHAVE scenarios, merge
    forID<- read.csv("ScenarioCharacteristics.csv")
    #remove columns in which all values identical by design
    forID<- subset(forID, select=-c(init.brood, init.pollen, init.pupae, treat.date))
    intermediate<- merge(sim, forID, all=T)  
    sim2<- merge(sim, forID, all=T)  

    #Lists of unique scenarios tested, for reference    
      ref_init<- subset(forID, select=c(ID_initial, init.adults, init.honey))
      ref_init<- ref_init[!duplicated(ref_init),]
      ref_feed<- subset(forID, select=c(ID_feeding, treat.honey, suppl.date, suppl.honey))
      ref_feed<- ref_feed[!duplicated(ref_feed),]
     
    #Make a column for total supplemental honey given
      sim2$suppl.tot<- NA
      sim2$suppl.tot [sim2$suppl.date==235 & sim2$suppl.honey==1.18 |
                   sim2$suppl.date==266 & sim2$suppl.honey==2.35 ] <- 2.35*2
      sim2$suppl.tot [sim2$suppl.date==235 & sim2$suppl.honey==2.35 |
                  sim2$suppl.date==266 & sim2$suppl.honey==4.7  ] <- 4.7*2
      sim2$suppl.tot [sim2$suppl.date==235 & sim2$suppl.honey==3.53 |
                  sim2$suppl.date==266 & sim2$suppl.honey==7.05 ] <- 7.05*2
      sim2$suppl.tot [sim2$suppl.date==235 & sim2$suppl.honey==4.7 |
                   sim2$suppl.date==266 & sim2$suppl.honey==9.4 ] <- 9.4*2
    #Variable prep for Linear Mixed Effects (LME) model
      sim2$suppdateC<- sim2$suppl.date - min(sim2$suppl.date)
      sim2$init.adults0 <- sim2$init.adults/1000
      sim2$AdultBees_BH_mean0 <- sim2$AdultBees_BH_mean/1000
      sim2$init.honey2<- sim2$init.honey^2
      
  #Subsetting for LME model
    #October 21 results only, from all years
    simOct<- subset(sim2, day %in% c(294,295))
    #Remove scenarios that produced at least one rep with 0 fall bees (outliers)
    simOct$rm0<- as.factor(ifelse(simOct$ZeroAdultReps>0, 1, 0))
    simOct<- subset(simOct, rm0==0, select=-rm0) 
    rm(sim2)



# Targeted simulations: LME models, Oct. 21 BEEHAVE outputs ---------------

  #Final LME model predicting adult bees on Oct. 21 in BEEHAVE _________________
    simOct$Apiary<- substr(simOct$Apiary, 3,30)
    fit_bees<- lmerTest::lmer (AdultBees_BH_mean ~ init.adults0*init.honey + 
                       suppdateC*suppl.honey + treat.honey*init.honey +  (1|Apiary), data=simOct)
    summary(fit_bees)  
    
    #R squared beta
      r2beta(fit_bees, partial=F, method="kr", data=simOct)
        
    #Residuals
      #Scatter plot
      simOct$predsB<- predict(fit_bees)
      axismax<- max( c(max(simOct$predsB), max(simOct$AdultBees_BH_mean)))
      axismin<- min( c(min(simOct$predsB), min(simOct$AdultBees_BH_mean)))
      ggplot(simOct, aes(x=AdultBees_BH_mean, y=predsB, color=Apiary)) + 
        geom_point(size=1) +  geom_abline(intercept=0, slope = 1) +  theme_bw() +
        ylab("LME-predicted adult bees") + xlab("BEEHAVE adult bees") +
        xlim(c(axismin,axismax)) + ylim(c(axismin,axismax))
      #Histograms, separated by apiary and colored by initial honey amount
      simOct$residsB<- resid(fit_bees)
      ggplot(simOct, aes(residsB, fill=as.factor(init.honey))) +  
        geom_histogram(bins=20) + theme_bw() +
        facet_wrap(~Apiary, ncol=2) + geom_vline(xintercept = 0, linetype=2) + 
        scale_fill_discrete(guide=guide_legend(title="Initial honey\n(kg honey\nequivalent)")) +
        xlab("Residuals from targeted simulations'\nfall adult bee LME model")

      
      
  #Final LME model predicting honey on Oct. 21 in BEEHAVE ______________________
    fit_honey<- lmer (Honey_BH_mean ~ init.adults0*init.honey2 +  
                        suppdateC*suppl.honey + treat.honey*init.honey2 + 
                        (1|Apiary), data=simOct)
    summary(fit_honey) #Note: the scaling warning doesn't impact results.
    
    #R squared beta
      r2beta(fit_honey, partial=F, method="kr", data=simOct)
    
    #Residuals
      #Scatter plot
      simOct$predsH<- predict(fit_honey)
      axismax<- max( c(max(simOct$predsH), max(simOct$Honey_BH_mean)))
      axismin<- min( c(min(simOct$predsH), min(simOct$Honey_BH_mean)))
      ggplot(simOct, aes(x=Honey_BH_mean, y=predsH, color=Apiary)) + 
        geom_point(size=1) +  geom_abline(intercept=0, slope = 1) +  theme_bw() +
        ylab("LME-predicted honey") + xlab("BEEHAVE honey") +
        xlim(c(axismin,axismax)) + ylim(c(axismin,axismax))
      #Histograms, separated by apiary and colored by initial honey amount
      simOct$residsH<- resid(fit_honey)
      ggplot(simOct, aes(residsH, fill=as.factor(init.honey))) +  
        geom_histogram(bins=20) + theme_bw() +
        facet_wrap(~Apiary, ncol=2) + geom_vline(xintercept = 0, linetype=2) + 
        scale_fill_discrete(guide=guide_legend(title="Initial honey\n(kg honey\nequivalent)")) +
        xlab("Residuals from targeted simulations'\nfall honey LME model")


# Targeted simulations: Line graphs of predictions ------------------------

  # Adult bee LME prediction set _____________________________________________
  
    newAd<- expand.grid(init.adults0 = unique(simOct$init.adults0),
                        init.honey = unique(simOct$init.honey),
                        suppdateC = unique(simOct$suppdateC),
                        suppl.honey = unique(simOct$suppl.honey),
                        treat.honey = unique(simOct$treat.honey))
    newAd$predsB<- predict(fit_bees, newdata=newAd, re.form=~0)
    newAd$suppl.tot<- NA
    newAd$suppl.tot [newAd$suppdateC==0 & newAd$suppl.honey==1.18 |
                      newAd$suppdateC==31 & newAd$suppl.honey==2.35 ] <- 2.35*2
    newAd$suppl.tot [newAd$suppdateC==0 & newAd$suppl.honey==2.35 |
                      newAd$suppdateC==31 & newAd$suppl.honey==4.7  ] <- 4.7*2
    newAd$suppl.tot [newAd$suppdateC==0 & newAd$suppl.honey==3.53 |
                      newAd$suppdateC==31 & newAd$suppl.honey==7.05 ] <- 7.05*2
    newAd$suppl.tot [newAd$suppdateC==0 & newAd$suppl.honey==4.7 |
                      newAd$suppdateC==31 & newAd$suppl.honey==9.4 ] <- 9.4*2
    newAd0<- subset(newAd, !is.na(suppl.tot))
    
  # Honey LME prediction set____ _____________________________________________
  
    newHon<- expand.grid(init.adults0 = unique(simOct$init.adults0),
                         init.honey = unique(simOct$init.honey),
                         suppdateC = unique(simOct$suppdateC),
                         suppl.honey = unique(simOct$suppl.honey),
                         treat.honey = unique(simOct$treat.honey))  
    newHon$init.honey2<- newHon$init.honey^2
    newHon$predsH<- predict(fit_honey, newdata=newHon, re.form=~0)
    newHon$suppl.tot<- NA
    newHon$suppl.tot [newHon$suppdateC==0 & newHon$suppl.honey==1.18 |
                         newHon$suppdateC==31 & newHon$suppl.honey==2.35 ] <- 2.35*2
    newHon$suppl.tot [newHon$suppdateC==0 & newHon$suppl.honey==2.35 |
                         newHon$suppdateC==31 & newHon$suppl.honey==4.7  ] <- 4.7*2
    newHon$suppl.tot [newHon$suppdateC==0 & newHon$suppl.honey==3.53 |
                         newHon$suppdateC==31 & newHon$suppl.honey==7.05 ] <- 7.05*2
    newHon$suppl.tot [newHon$suppdateC==0 & newHon$suppl.honey==4.7 |
                         newHon$suppdateC==31 & newHon$suppl.honey==9.4 ] <- 9.4*2
    newHon0<- subset(newHon, !is.na(suppl.tot))
	 
    #Separate set needed to depict curve
      newHonb<- expand.grid(init.adults0 = unique(simOct$init.adults0),
                           init.honey = c(unique(simOct$init.honey), seq(4,36)),
                           suppdateC = 0,
                           suppl.honey = unique(simOct$suppl.honey)[4],
                           treat.honey = unique(simOct$treat.honey))  
      newHonb$init.honey2<- newHonb$init.honey^2
      newHonb$source<- ifelse(newHonb$init.honey %in% c(3.6,13.7,36.3), "point", "blank")
      newHonb$predsH2<- predict(fit_honey, newdata=newHonb, re.form=~0)
      newHonb$suppl.tot<-  9.4*2

         
  # Merge all newsets __________________________________________
    newsets<- join_all(list(newHonb, newHon0, newAd0), type="full")
    rm(newAd, newAd0, newHon, newHon0, newHonb)
    
    #Melt for graphing
      newsets$predsB0<- newsets$predsB/1000
    
    #For first figure
      newsets1<- melt(newsets, id=c("init.adults0", "init.honey","suppdateC","suppl.honey",
                                    "treat.honey", "suppl.tot", "init.honey2","source"),
                      measure.vars=c("predsB0","predsH"))
      newsets1$variable<- revalue(newsets1$variable, c("predsB0"="Adult bees (thousands)",
                                                       "predsH"="Honey (kg)"))
    
    #For second figure
      newsets2<- subset(newsets, suppdateC==0 & suppl.honey==unique(simOct$suppl.honey)[4] )
      newsets2$predsB0<- predict(fit_bees, newdata=newsets2, re.form=~0)/1000 
      newsets2<- melt(newsets2, id=c("init.adults0", "init.honey","suppdateC","suppl.honey",
                                    "treat.honey", "suppl.tot", "init.honey2","source"),
                      measure.vars=c("predsB0","predsH2"))
      newsets2$variable<- revalue(newsets2$variable, c("predsB0"="Adult bees (thousands)",
                                                       "predsH2"="Honey (kg)"))
    

  # 1st figure _____________________________________________

    newsets1b<- subset(newsets1, init.adults0==13.348 & init.honey==13.7)
    newsets1b$suppdateC2<- revalue(as.factor(newsets1b$suppdateC), c("0"="Aug. 23", "31"="Sep. 23"))

    ggplot(newsets1b, aes(x=suppl.honey, y=value, color=suppdateC2)) + 
        geom_point(aes(shape=suppdateC2)) + geom_line(aes(linetype=as.factor(treat.honey))) + 
        facet_wrap(~variable, ncol=1, scales="free_y", strip.position = "left") +
        scale_x_continuous(breaks=seq(1,10,by=2)) +
        scale_color_manual(guide=guide_legend(title="Supplemental\nstart date"),
                           values=c("#0072B2", "#D55E00")) +
        scale_shape_discrete(guide=guide_legend(title="Supplemental\nstart date")) +
        scale_linetype_discrete(guide=guide_legend(title="Treatment sugar\n(kg honey equiv.)")) +
        theme_bw() + xlab("Supplemental sugar (kg honey equiv.)") + 
        ylab ("Mean values on October 21") + 
        theme(legend.position = "bottom", legend.box="horizontal", 
             legend.direction="vertical", panel.grid.minor = element_blank())


  # 2nd figure _____________________________________________
  
      ggplot(newsets2, aes(x=init.honey, y=value, color=as.factor(init.adults0*1000), 
                           linetype=as.factor(treat.honey))) +  
          geom_point(aes(shape=source)) + geom_line() + 
          facet_wrap(~variable, ncol=1, scales="free_y", strip.position = "left") +
          scale_shape_manual(values=c(NA, 16), guide=F) + 
          scale_color_manual(guide=guide_legend(title="Initial adult bees"),
                               values=c("#0072B2","#009E73", "#D55E00")) +
          scale_linetype_discrete(guide=guide_legend(title="Treatment sugar\n(kg honey equiv.)")) +
          theme_bw() + xlab("Inital honey (kg)") + ylab ("Mean values on October 21") +
          theme(legend.position = "bottom", legend.box="horizontal", 
               legend.direction="vertical", panel.grid.minor = element_blank())

        
