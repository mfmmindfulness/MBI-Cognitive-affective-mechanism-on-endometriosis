#Investigate the effect of MBI on Pain Catastrophizing and Affect. 
# The mediator role of PC and Affect in the effect of MBI on Pain and QoL mental health

#data frames----
clinical.trial.mechanism=read.xlsx("clinical.trial.all.xlsx")

names(clinical.trial.mechanism)

clinical.trial.mechanism=clinical.trial.mechanism %>% 
  select(id,time,group,pelvic.pain,pain.unpleasantness,sf_36_mental.health,
         affect_positive,affect_Negative,PCS_total,PCS_rumination,
         PCS_magnification,PCS_helplessness,dysuria,dyspareunia,dyschezia, dysmenorrhea)

clinical.trial.mech=clinical.trial.mechanism %>% 
  filter(time=="t2") %>% 
  select(id,group,pelvic.pain,pain.unpleasantness,sf_36_mental.health,
         affect_positive,affect_Negative,PCS_total,PCS_rumination,
         PCS_magnification,PCS_helplessness)

med.mechanism= clinical.trial.mechanism %>% 
  select(id, group, time,PCS_total,PCS_rumination,
         PCS_magnification,PCS_helplessness,
         affect_positive,affect_Negative,pelvic.pain,pain.unpleasantness,sf_36_mental.health) %>% 
  pivot_wider(names_from = time,values_from = c( sf_36_mental.health,PCS_total,PCS_rumination,PCS_magnification,PCS_helplessness,
                                                 affect_positive,affect_Negative,pelvic.pain,pain.unpleasantness)) %>% 
  dummy_cols(select_columns ="group") %>% 
  select(-group_control,-group)



#imputation using MICE----  
install.packages("mice")
library(mice)

#data frame wide format
clinical.trial.mechanism.imp=clinical.trial.mechanism %>%
  select(id,time,group,pelvic.pain,pain.unpleasantness,sf_36_mental.health,
         affect_positive,affect_Negative,PCS_total) %>% 
  pivot_wider(names_from = time,values_from=c(pelvic.pain,pain.unpleasantness,sf_36_mental.health,
                                              affect_positive,affect_Negative,PCS_total)) %>% 
  dummy_cols(select_columns ="group") %>% 
  select(-group_control,-group)


#construct the predictor matrix setting: -2  to indicate the cluster variable, 1 imputation model with a fixed effect and a random intercept(default)

med.matrix=make.predictorMatrix(clinical.trial.mechanism.imp)

med.matrix[,"group_intervention"]=-2

imputed_med=mice(clinical.trial.mechanism.imp, m=5, predictorMatrix = med.matrix, seed=225)
summary(imputed_med)


# Check similarity between raw and imputed data to each variable

stripplot(imputed_med, affect_positive_t2 ~ .imp, pch = 20, cex = 2)





# Distribution----

#T1
clinical.trial.mechanism %>% 
  filter(time=="t1") %>% 
  select(group,affect_positive,affect_Negative,PCS_total,PCS_rumination,
         PCS_magnification,PCS_helplessness) %>%
  pivot_longer(-group) %>% 
  ggplot(aes(x = group, y = value, color = group)) + facet_wrap(~ name, scales = "free", nrow = 4) +
  coord_flip ()+
  geom_boxplot() +
  geom_jitter(shape = 10, position = position_jitter(0.1)) +
  stat_summary(fun = median, geom ="pointrange",color = "black") +
  ylab("Value") +
  theme( axis.text.y = element_blank())

#T2
clinical.trial.mechanism %>% 
  filter(time=="t2") %>% 
  select(group,affect_positive,affect_Negative,PCS_total,PCS_rumination,
         PCS_magnification,PCS_helplessness) %>%
  pivot_longer(-group) %>% 
  ggplot(aes(x = group, y = value, color = group)) + facet_wrap(~ name, scales = "free", nrow = 4) +
  coord_flip ()+
  geom_boxplot() +
  geom_jitter(shape = 10, position = position_jitter(0.1)) +
  stat_summary(fun = median, geom ="pointrange",color = "black") +
  ylab("Value") +
  theme( axis.text.y = element_blank())

#T3
clinical.trial.mechanism %>% 
  filter(time=="t3") %>% 
  select(group,affect_positive,affect_Negative,PCS_total,PCS_rumination,
         PCS_magnification,PCS_helplessness) %>%
  pivot_longer(-group) %>% 
  ggplot(aes(x = group, y = value, color = group)) + facet_wrap(~ name, scales = "free", nrow = 4) +
  coord_flip ()+
  geom_boxplot() +
  geom_jitter(shape = 10, position = position_jitter(0.1)) +
  stat_summary(fun = median, geom ="pointrange",color = "black") +
  ylab("Value") +
  theme( axis.text.y = element_blank())



# Descriptive statistic----

sample.by.time.med= clinical.trial.mechanism %>% 
  select (id, time, group) %>% 
  group_by(time, group) %>% 
  summarise(id = n()) %>% 
  rename(participants=id) %>% 
  flextable()

#N by variable without NA
var.mechanism.lm.n=clinical.trial.mechanism %>% 
  select( group, time,PCS_total,PCS_rumination,PCS_magnification,PCS_helplessness,
          affect_positive,affect_Negative,pelvic.pain,pain.unpleasantness, sf_36_mental.health) %>%  
  pivot_longer(-time & -group) %>%
  filter(!is.na(value)) %>% 
  group_by(name, time, group) %>% 
  summarise(n=n())
print.data.frame(var.mechanism.lm.n)

#change default statistic in tableby
names(clinical.trial.mechanism)




clinical.trial.mechanism.tab2=clinical.trial.mechanism %>% 
  pivot_wider(names_from = time,values_from=c(pelvic.pain,pain.unpleasantness,affect_positive,affect_Negative,PCS_total,PCS_rumination,
                                              PCS_magnification,PCS_helplessness,sf_36_mental.health))
names(clinical.trial.mechanism.tab2)

mycontrols2= tableby.control(test=TRUE, total=FALSE,
                             numeric.test="wt", cat.test="chisq",
                             numeric.stats=c("mean","sd", "median", "q1q3","Nmiss","N"),
                             cat.stats=c("countpct"),
                             stats.labels=list(mean="Mean",sd="SD", median="Median", q1q3="Q1,Q3"))



tab.mechanism2=tableby(group~pelvic.pain_t1+pelvic.pain_t2+pain.unpleasantness_t1+pain.unpleasantness_t2+
                         affect_positive_t1+affect_positive_t2+affect_Negative_t1+
                         affect_Negative_t2+PCS_total_t1+PCS_total_t2+PCS_rumination_t1+PCS_rumination_t2+
                         PCS_magnification_t1+PCS_magnification_t2+PCS_helplessness_t1+PCS_helplessness_t2+
                         sf_36_mental.health_t1+sf_36_mental.health_t2,
                       data=clinical.trial.mechanism.tab2, control=mycontrols2)



tab.mechanism2=summary(tab.mechanism2,text=TRUE)
tab.mechanism2=as.data.frame(tab.mechanism2)



tab.mechanism2 %>%
  rename(variables="") %>% 
  flextable %>% 
  autofit



# Correlation between, pain, negative affect, positive affect and pain catastrophizing----

cor_affect.PC.pain1=clinical.trial.mechanism %>% 
  filter(time=="t1") %>% 
  select(affect_positive,affect_Negative,PCS_total,pelvic.pain,pain.unpleasantness,sf_36_mental.health) %>% 
  lowerCor()

cor_affect.PC.pain2=clinical.trial.mechanism %>% 
  filter(time=="t2") %>% 
  select(affect_positive,affect_Negative,PCS_total,pelvic.pain,pain.unpleasantness,sf_36_mental.health) %>% 
  lowerCor()




# Linear regression of the dv pain and Qul_MH on id group----
#pelvic.pain

group.pelvic.pain= '
# Path e
pelvic.pain_t2~e*group_intervention+pelvic.pain_t1'

set.seed(2021)

fit_group.pelvic.pain=sem(group.pelvic.pain, med.mechanism , se="bootstrap", bootstrap= 2000)

summary(fit_group.pelvic.pain, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)

group.ppi.par=parameterEstimates(fit_group.pelvic.pain, ci=TRUE, level=0.95, boot.ci.type= "perc")

group.ppi.sum=standardizedSolution(fit_group.pelvic.pain)

#pain unpleasantness
group.unpleasantness= '
# Path e
pain.unpleasantness_t2~e*group_intervention+pain.unpleasantness_t1'

set.seed(2021)

fit_group.unpleasantness=sem(group.unpleasantness, med.mechanism , se="bootstrap", bootstrap= 2000)

summary(fit_group.unpleasantness, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)

group.pu.par=parameterEstimates(fit_group.unpleasantness, ci=TRUE, level=0.95, boot.ci.type= "perc")

group.pu.sum=standardizedSolution(fit_group.unpleasantness)

# group and Mental health----

mental.health= '
# Path h
sf_36_mental.health_t2~h*group_intervention+sf_36_mental.health_t1'

set.seed(2021)

fit_mental.health=sem(mental.health, med.mechanism , se="bootstrap", bootstrap= 2000)

summary(fit_mental.health, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)

group.mh.parm=parameterEstimates(fit_mental.health, ci=TRUE, level=0.95, boot.ci.type= "perc")

group.mh.stand=standardizedSolution(fit_mental.health)



#PC rumination, magnification, helplessness----

#PC sub component
group.pc.all.pain= '# Path a
PCS_rumination_t2~a*group_intervention+PCS_rumination_t1
# Path b
PCS_magnification_t2~b*group_intervention+PCS_magnification_t1
# Path c
PCS_helplessness_t2~c*group_intervention+PCS_helplessness_t1
# Path d
pelvic.pain_t2~d*PCS_rumination_t2+PCS_rumination_t1+pelvic.pain_t1
# Path e
pelvic.pain_t2~e*PCS_magnification_t2+PCS_magnification_t1
# Path f
pelvic.pain_t2~f*PCS_helplessness_t2+PCS_helplessness_t1

# Each PC path  
rumination.pain:=a*d
magnification.pain:=b*e
helplessness.pain:=c*f

# Total effect . 
total:=a*d+b*e+c*f'

set.seed(2021)


fit_group.pc.all=sem(group.pc.all.pain,med.mechanism , se="bootstrap", bootstrap= 2000)

summary(fit_group.pc.all, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)

group.pc.parm=parameterEstimates(fit_group.pc.all, ci=TRUE, level=0.95, boot.ci.type= "perc")

group.pc.stand=standardizedSolution(fit_group.pc.all)



# Table simple regression: group->pelvic.pain,group->pain.unpleasantness, group->rumination|magnification|helplessness----


group.ppi1= group.ppi.sum %>% 
  filter(label%in% c("e")) %>% 
  select(est.std) %>% 
  rename(Standardized.beta=est.std)

group.ppi2=group.ppi.par %>% 
  filter(label%in% c("e")) %>% 
  select(-op) %>% 
  relocate(any_of(c("rhs","ihs"))) %>% 
  rename(group.pp1=rhs,var2=lhs, Unstandardized.beta=est, SE=se)

group.pu1= group.pu.sum %>% 
  filter(label%in% c("e")) %>% 
  select(est.std) %>% 
  rename(Standardized.beta=est.std)

group.pu2=group.pu.par %>% 
  filter(label%in% c("e")) %>% 
  select(-op) %>% 
  relocate(any_of(c("rhs","ihs"))) %>% 
  rename(group.pp1=rhs,var2=lhs, Unstandardized.beta=est, SE=se)


group.pc1=group.pc.stand %>% 
  filter(label%in% c("a","b","c")) %>% 
  select(est.std) %>% 
  rename(Standardized.beta=est.std)

group.pc2=group.pc.parm %>% 
  filter(label%in% c("a","b","c")) %>% 
  select(-op) %>% 
  relocate(any_of(c("rhs","ihs"))) %>% 
  rename(group.pp1=rhs,var2=lhs, Unstandardized.beta=est, SE=se)

group.mh1= group.mh.stand %>% 
  filter(label%in% c("h")) %>% 
  select(est.std) %>% 
  rename(Standardized.beta=est.std)

group.mh2=group.mh.parm %>% 
  filter(label%in% c("h")) %>% 
  select(-op) %>% 
  relocate(any_of(c("rhs","ihs"))) %>% 
  rename(group.pp1=rhs,var2=lhs, Unstandardized.beta=est, SE=se)



group.ppi.tab=group.ppi1 %>% 
  bind_cols(group.ppi2) 

group.pc.tab=group.pc1 %>% 
  bind_cols(group.pc2) 

group.pu.tab=group.pu1 %>% 
  bind_cols(group.pu2) 

group.mh.tab=group.mh1 %>% 
  bind_cols(group.mh2)

group.pc.pain.tab=group.ppi.tab %>% 
  bind_rows(group.pu.tab,group.pc.tab,group.mh.tab) 



group.pc.pain.tab=group.pc.pain.tab[c(3,4,5,6,7,11),]

group.pain.pc.tab=group.pc.pain.tab %>% 
  relocate(any_of(c("group.pp1","var2","label","Unstandardized.beta", "Standardized.beta"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-var2, -label) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6),j=1, value = as_paragraph (c("Group-->PPI", "Group-->PU","Group-->Rumination",
                                                        "Group-->Magnification","Group-->Helplessness","Group-->MH"))) %>% 
  autofit()

# Parallel mediator (PA-PC), 2 Exogenous (PELVIC PAIN-MENATL HEALTH)----



parallel.med_b= '
# Path a
PCS_total_t2~a*group_intervention+PCS_total_t1

# Path b
pelvic.pain_t2~b*PCS_total_t2+pelvic.pain_t1+PCS_total_t1

# Path f

sf_36_mental.health_t2~f*PCS_total_t2+sf_36_mental.health_t1+PCS_total_t1

# Path e

pelvic.pain_t2~e*group_intervention

# Path c
affect_positive_t2~c*group_intervention+affect_positive_t1

# Path d
pelvic.pain_t2~d*affect_positive_t2

# Path g

sf_36_mental.health_t2~g*affect_positive_t2+affect_positive_t1

## Path h

sf_36_mental.health_t2~h*group_intervention

# Indirect effect pain catastrophizing. 
group_pc_pelvic.pain:=a*b
group_pc_mental.health:=a*f

# Indirect effect positive affect . 
group_pos.affect_pelvic.pain:=c*d
group_pos.mental.health:=c*g
'

# Fit/estimate the model
set.seed(2021)

fit_parallel.med_b=sem(parallel.med_b,med.mechanism , se="bootstrap", bootstrap= 2000)

# Summarize the results/output

sum.fit_parallel.med_b=summary(fit_parallel.med_b, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)


med_b=parameterEstimates(fit_parallel.med_b, ci=TRUE, level=0.95, boot.ci.type= "perc")



med_b.stand=standardizedSolution(fit_parallel.med_b)

#Large positive values indicate the model underpredicts the correlation; large negative values suggest overprediction of correlation

resid(fit_parallel.med_b, "cor")

# Imputed----

install.packages("semTools")
library(semTools)


set.seed(2021)

fit_parallel.med_b.imp=sem.mi(parallel.med_b,imputed_med)

sum.fit_parallel.med_b.imp=summary(fit_parallel.med_b.imp, fit.measures=TRUE, ci = TRUE, stand = TRUE, rsq = TRUE,
                                   standardized= TRUE)
sum.fit_parallel.med_b.imp=tibble(sum.fit_parallel.med_b.imp)

fitMeasures(fit_parallel.med_b.imp)

print.data.frame(sum.fit_parallel.med_b.imp)

# misfit of the bivariate associations - residuals in correlational units, subtraction of the observed - model-implied matrices-----
#Large positive values indicate the model underpredicts the correlation; large negative values suggest overprediction of correlation

resid(fit_parallel.med_b.imp, "cor")


# Parallel mediator (PA-PC), 2 Exogenous (PELVIC UNPLEASANTNESS-MENATL HEALTH)----

parallel.med_c= '
# Path a
PCS_total_t2~a*group_intervention+PCS_total_t1

# Path b
pain.unpleasantness_t2~b*PCS_total_t2+pain.unpleasantness_t1+PCS_total_t1

# Path f

sf_36_mental.health_t2~f*PCS_total_t2+sf_36_mental.health_t1+PCS_total_t1

# Path e

pain.unpleasantness_t2~e*group_intervention

# Path c
affect_positive_t2~c*group_intervention+affect_positive_t1

# Path d
pain.unpleasantness_t2~d*affect_positive_t2

# Path g

sf_36_mental.health_t2~g*affect_positive_t2+affect_positive_t1

## Path h

sf_36_mental.health_t2~h*group_intervention

# Indirect effect pain catastrophizing. 
group_pc_unpleasantness:=a*b
group_pc_mental.health:=a*f

# Indirect effect positive affect . 
group_pa_unpleasantness:=c*d
group_pa.mental.health:=c*g
'

# Fit/estimate the model
set.seed(2021)

# Resampling method: Percentile Bootstraping, does not assume normality

fit_parallel.med_c=sem(parallel.med_c,med.mechanism , se="bootstrap", bootstrap= 2000)
fit_parallel.med_c2=sem(parallel.med_c2,med.mechanism , se="bootstrap", bootstrap= 2000)

# Summarize the results/output

sum.fit_parallel.med_c=summary(fit_parallel.med_c, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)
summary(fit_parallel.med_c2, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)

med_c=parameterEstimates(fit_parallel.med_c, ci=TRUE, level=0.95, boot.ci.type= "perc")

med_c.stand=standardizedSolution(fit_parallel.med_c)

# misfit of the bivariate associations - residuals in correlational units, subtraction of the observed - model-implied matrices
#Large positive values indicate the model underpredicts the correlation; large negative values suggest overprediction of correlation

resid(fit_parallel.med_c, "cor")

# Imputed----

set.seed(2021)

fit_parallel.med_c.imp=sem.mi(parallel.med_c,imputed_med )

sum.fit_parallel.med_c.imp=summary(fit_parallel.med_c.imp, fit.measures=TRUE, ci = TRUE, stand = TRUE, rsq = TRUE,
                                   standardized= TRUE)
sum.fit_parallel.med_c.imp=tibble(sum.fit_parallel.med_c.imp)

fitMeasures(fit_parallel.med_c.imp)

# misfit of the bivariate associations - residuals in correlational units, subtraction of the observed - model-implied matrices-----
#Large positive values indicate the model underpredicts the correlation; large negative values suggest overprediction of correlation

resid(fit_parallel.med_c.imp, "cor")

# Parallel mediator (NA-PC), 2 Exogenous (PELVIC UNPLEASANTNESS-MENATL HEALTH)----

parallel.med_d= '
# Path a
PCS_total_t2~a*group_intervention+PCS_total_t1

# Path b
pain.unpleasantness_t2~b*PCS_total_t2+pain.unpleasantness_t1+PCS_total_t1

# Path f

sf_36_mental.health_t2~f*PCS_total_t2+sf_36_mental.health_t1+PCS_total_t1

# Path e

pain.unpleasantness_t2~e*group_intervention

# Path c
affect_Negative_t2~c*group_intervention+affect_Negative_t1

# Path d
pain.unpleasantness_t2~d*affect_Negative_t2

# Path g

sf_36_mental.health_t2~g*affect_Negative_t2+affect_Negative_t1

## Path h

sf_36_mental.health_t2~h*group_intervention

# Indirect effect pain catastrophizing. 
group_pc_unpleasantness:=a*b
group_pc_mental.health:=a*f

# Indirect effect Negative affect . 
group_pn_unpleasantness:=c*d
group_pn.mental.health:=c*g

# Total effect 
total:=a*b+c*d+e+h'




# Fit/estimate the model
set.seed(2021)

# Resampling method: Percentile Bootstraping, does not assume normality

fit_parallel.med_d=sem(parallel.med_d,med.mechanism , se="bootstrap", bootstrap= 2000)

fit_parallel.med_d2=sem(parallel.med_d2,med.mechanism , se="bootstrap", bootstrap= 2000)

# Summarize the results/output

sum.fit_parallel.med_d=summary(fit_parallel.med_d, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)

summary(fit_parallel.med_d2, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)

med_d=parameterEstimates(fit_parallel.med_d, ci=TRUE, level=0.95, boot.ci.type= "perc")

med_d.stand=standardizedSolution(fit_parallel.med_d)

#Large positive values indicate the model underpredicts the correlation; large negative values suggest overprediction of correlation

resid(fit_parallel.med_d, "cor")


#Imputed----

set.seed(2021)

fit_parallel.med_d.imp=sem.mi(parallel.med_d,imputed_med )

sum.fit_parallel.med_d.imp=summary(fit_parallel.med_d.imp, fit.measures=TRUE, ci = TRUE, stand = TRUE, rsq = TRUE,
                                   standardized= TRUE)
sum.fit_parallel.med_d.imp=tibble(sum.fit_parallel.med_d.imp)

fitMeasures(fit_parallel.med_d.imp)

# misfit of the bivariate associations - residuals in correlational units, subtraction of the observed - model-implied matrices-----
#Large positive values indicate the model underpredicts the correlation; large negative values suggest overprediction of correlation

resid(fit_parallel.med_d.imp, "cor")



# Parallel mediator (NA-PC), 2 Exogenous (PELVIC PAIN-MENATL HEALTH)----

parallel.med_e= '
# Path a
PCS_total_t2~a*group_intervention+PCS_total_t1

# Path b
pelvic.pain_t2~b*PCS_total_t2+pelvic.pain_t1+PCS_total_t1

# Path f

sf_36_mental.health_t2~f*PCS_total_t2+sf_36_mental.health_t1+PCS_total_t1

# Path e

pelvic.pain_t2~e*group_intervention

# Path c
affect_Negative_t2~c*group_intervention+affect_Negative_t1

# Path d
pelvic.pain_t2~d*affect_Negative_t2

# Path g

sf_36_mental.health_t2~g*affect_Negative_t2+affect_Negative_t1

## Path h

sf_36_mental.health_t2~h*group_intervention

# Indirect effect pain catastrophizing. 
group_pc_pelvic.pain:=a*b
group_pc_mental.health:=a*f

# Indirect effect Negative affect . 
group_na_pelvic.pain:=c*d
group_na.mental.health:=c*g

# Total effect . 
total:=a*b+c*d+e+h'



# Fit/estimate the model
set.seed(2021)

# Resampling method: Percentile Bootstraping, does not assume normality

fit_parallel.med_e=sem(parallel.med_e,med.mechanism , se="bootstrap", bootstrap= 2000)

fit_parallel.med_e2=sem(parallel.med_e2,med.mechanism , se="bootstrap", bootstrap= 2000)

# Summarize the results/output

sum.fit_parallel.med_e=summary(fit_parallel.med_e, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)

summary(fit_parallel.med_e2, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)

med_e=parameterEstimates(fit_parallel.med_e, ci=TRUE, level=0.95, boot.ci.type= "perc")

med_e.stand=standardizedSolution(fit_parallel.med_e)


# misfit of the bivariate associations - residuals in correlational units, subtraction of the observed - model-implied matrices-----
#Large positive values indicate the model underpredicts the correlation; large negative values suggest overprediction of correlation

resid(fit_parallel.med_e, "cor")


#imputed----
set.seed(2021)

fit_parallel.med_e.imp=sem.mi(parallel.med_e,imputed_med )

sum.fit_parallel.med_e.imp=summary(fit_parallel.med_e.imp, fit.measures=TRUE, ci = TRUE, stand = TRUE, rsq = TRUE,
                                   standardized= TRUE)
sum.fit_parallel.med_e.imp=tibble(sum.fit_parallel.med_e.imp)

fitMeasures(fit_parallel.med_e.imp)

# misfit of the bivariate associations - residuals in correlational units, subtraction of the observed - model-implied matrices-----
#Large positive values indicate the model underpredicts the correlation; large negative values suggest overprediction of correlation

resid(fit_parallel.med_e.imp, "cor")

# Table parallel models----

# Model 1 (parallel.med_b)----
med_b.stand1= med_b.stand %>% 
  filter(label%in% c("a","b","f","e","c","d","g","h","group_pc_pelvic.pain",
                     "group_pc_mental.health","group_pos.affect_pelvic.pain",
                     "group_pos.mental.health")) %>% 
  select(est.std) %>% 
  rename(Standardized.beta=est.std)

med.b1=med_b %>% 
  filter(label%in% c("a","b","f","e","c","d","g","h","group_pc_pelvic.pain",
                     "group_pc_mental.health","group_pos.affect_pelvic.pain",
                     "group_pos.mental.health")) %>% 
  select(-op) %>% 
  relocate(any_of(c("rhs","ihs"))) %>% 
  rename(parallel.med_b=rhs,var2=lhs, Unstandardized.beta=est, SE=se)

med.b1=med.b1 %>% 
  bind_cols(med_b.stand1) %>% 
  relocate(any_of(c("parallel.med_b","var2","label","Unstandardized.beta", "Standardized.beta"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-var2, -label) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8,9,10,11,12),j=1, value = as_paragraph (c("a - Group-->PC", "b - PC-->PPI","f - PC-->MH","e' - Group-->PPI",
                                                                       "c - Group-->PA","d - PA-->PPI",
                                                                       "g - PA-->MH","h' - Group-->MH","Group-->PC-->PPI",
                                                                       "Group-->PC-->MH","Group-->PA-->PPI","Group-->PA-->MH"))) %>% 
  autofit()

# fit measures

fit.b1=sum.fit_parallel.med_b$FIT %>% 
  tibble()
fit.b1=fit.b1[c(3,4,5,9,17),]
fit.b1=fit.b1 %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()

# Imputed----


med.b1.imp=sum.fit_parallel.med_b.imp %>% 
  filter(label%in% c("a","b","f","e","c","d","g","h","group_pc_pelvic.pain",
                     "group_pc_mental.health","group_pos.affect_pelvic.pain",
                     "group_pos.mental.health")) %>% 
  select(- op, - exo, - df, - std.lv, - std.nox,- label, - lhs) %>% 
  relocate(any_of(c("rhs","ihs","est","std.all"))) %>% 
  rename(parallel.med_b.imp=rhs, Unstandardized.beta=est, Standardized.beta=std.all , SE=se)

med.b1.imp=med.b1.imp %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8,9,10,11,12),j=1, value = as_paragraph (c("a - Group-->PC", "b - PC-->PPI","f - PC-->MH","e' - Group-->PPI",
                                                                       "c - Group-->PA","d - PA-->PPI",
                                                                       "g - PA-->MH","h' - Group-->MH","Group-->PC-->PPI",
                                                                       "Group-->PC-->MH","Group-->PA-->PPI","Group-->PA-->MH"))) %>% 
  autofit()

# fit measures

fit.b1.imp=fitMeasures(fit_parallel.med_b.imp)
fit.b1.imp=fit.b1.imp %>% 
  tibble() 
fit.b1.imp=fit.b1.imp[c(1,2,3,16,25),]

fit.b1.imp=fit.b1.imp %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()

# Model 2 (parallel.med_c)----

med_c.stand1= med_c.stand %>% 
  filter(label%in% c("a","b","f","e","c","d","g","h","group_pc_unpleasantness",
                     "group_pc_mental.health","group_pa_unpleasantness",
                     "group_pa.mental.health")) %>% 
  select(est.std) %>% 
  rename(Standardized.beta=est.std)

med_c1=med_c %>% 
  filter(label%in% c("a","b","f","e","c","d","g","h","group_pc_unpleasantness",
                     "group_pc_mental.health","group_pa_unpleasantness",
                     "group_pa.mental.health")) %>% 
  select(-op) %>% 
  relocate(any_of(c("rhs","ihs"))) %>% 
  rename(parallel.med_c=rhs,var2=lhs, Unstandardized.beta=est, SE=se)

med.c1=med_c1 %>% 
  bind_cols(med_c.stand1) %>% 
  relocate(any_of(c("parallel.med_c","var2","label","Unstandardized.beta", "Standardized.beta"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-var2, -label) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8,9,10,11,12),j=1, value = as_paragraph (c("a - Group-->PC", "b - PC-->PU","f - PC-->MH","e' - Group-->PU",
                                                                       "c - Group-->PA","d - PA-->PU",
                                                                       "g - PA-->MH","h' - Group-->MH","Group-->PC-->PU",
                                                                       "Group-->PC-->MH","Group-->PA-->PU","Group-->PA-->MH"))) %>% 
  autofit()

# fit measures

fit.c=sum.fit_parallel.med_c$FIT %>% 
  tibble()
fit.c=fit.c[c(3,4,5,9,17),]
fit.c=fit.c %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()


# Imputed----


med.c1.imp=sum.fit_parallel.med_c.imp %>% 
  filter(label%in% c("a","b","f","e","c","d","g","h","group_pc_unpleasantness",
                     "group_pc_mental.health","group_pa_unpleasantness",
                     "group_pa.mental.health")) %>% 
  select(- op, - exo, - df, - std.lv, - std.nox,- label, - lhs) %>% 
  relocate(any_of(c("rhs","ihs","est","std.all"))) %>% 
  rename(parallel.med_c.imp=rhs, Unstandardized.beta=est, Standardized.beta=std.all , SE=se)

med.c1.imp=med.c1.imp %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8,9,10,11,12),j=1, value = as_paragraph (c("a - Group-->PC", "b - PC-->PU","f - PC-->MH","e' - Group-->PU",
                                                                       "c - Group-->PA","d - PA-->PU",
                                                                       "g - PA-->MH","h' - Group-->MH","Group-->PC-->PU",
                                                                       "Group-->PC-->MH","Group-->PA-->PU","Group-->PA-->MH"))) %>% 
  autofit()

# fit measures

fit.c1.imp=fitMeasures(fit_parallel.med_c.imp)
fit.c1.imp=fit.c1.imp %>% 
  tibble() 
fit.c1.imp=fit.c1.imp[c(1,2,3,16,25),]

fit.c1.imp=fit.c1.imp %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()


# Model 3 (parallel.med_d)----

med_d.stand1= med_d.stand %>% 
  filter(label%in% c("a","b","f","e","c","d","g","h","group_pc_unpleasantness",
                     "group_pc_mental.health","group_pn_unpleasantness",
                     "group_pn.mental.health")) %>% 
  select(est.std) %>% 
  rename(Standardized.beta=est.std)

med_d1=med_d %>% 
  filter(label%in% c("a","b","f","e","c","d","g","h","group_pc_unpleasantness",
                     "group_pc_mental.health","group_pn_unpleasantness",
                     "group_pn.mental.health")) %>% 
  select(-op) %>% 
  relocate(any_of(c("rhs","ihs"))) %>% 
  rename(parallel.med_d=rhs,var2=lhs, Unstandardized.beta=est, SE=se)

med.d1=med_d1 %>% 
  bind_cols(med_d.stand1) %>% 
  relocate(any_of(c("parallel.med_d","var2","label","Unstandardized.beta", "Standardized.beta"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-var2, -label) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8,9,10,11,12),j=1, value = as_paragraph (c("a - Group-->PC", "b - PC-->PU","f - PC-->MH","e' - Group-->PU",
                                                                       "c - Group-->NA","d - NA-->PU",
                                                                       "g - NA-->MH","h' - Group-->MH","Group-->PC-->PU",
                                                                       "Group-->PC-->MH","Group-->NA-->PU","Group-->NA-->MH"))) %>% 
  autofit()

# fit measures

fit.d=sum.fit_parallel.med_d$FIT %>% 
  tibble()
fit.d=fit.d[c(3,4,5,9,17),]
fit.d=fit.d %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()

# Imputed----


med.d1.imp=sum.fit_parallel.med_d.imp %>% 
  filter(label%in% c("a","b","f","e","c","d","g","h","group_pc_unpleasantness",
                     "group_pc_mental.health","group_pn_unpleasantness",
                     "group_pn.mental.health")) %>% 
  select(- op, - exo, - df, - std.lv, - std.nox,- label, - lhs) %>% 
  relocate(any_of(c("rhs","ihs","est","std.all"))) %>% 
  rename(parallel.med_b.imp=rhs, Unstandardized.beta=est, Standardized.beta=std.all , SE=se)

med.d1.imp=med.d1.imp %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8,9,10,11,12),j=1, value = as_paragraph (c("a - Group-->PC", "b - PC-->PU","f - PC-->MH","e' - Group-->PU",
                                                                       "c - Group-->NA","d - NA-->PU",
                                                                       "g - NA-->MH","h' - Group-->MH","Group-->PC-->PU",
                                                                       "Group-->PC-->MH","Group-->NA-->PU","Group-->NA-->MH"))) %>% 
  autofit()

# fit measures

fit.d1.imp=fitMeasures(fit_parallel.med_d.imp)
fit.d1.imp=fit.d1.imp %>% 
  tibble() 
fit.d1.imp=fit.d1.imp[c(1,2,3,16,25),]

fit.d1.imp=fit.d1.imp %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()



# Model 4 (parallel.med_e)----

med_e.stand1= med_e.stand %>% 
  filter(label%in% c("a","b","f","e","c","d","g","h","group_pc_pelvic.pain",
                     "group_pc_mental.health","group_na_pelvic.pain",
                     "group_na.mental.health")) %>% 
  select(est.std) %>% 
  rename(Standardized.beta=est.std)

med_e1=med_e %>% 
  filter(label%in% c("a","b","f","e","c","d","g","h","group_pc_pelvic.pain",
                     "group_pc_mental.health","group_na_pelvic.pain",
                     "group_na.mental.health")) %>% 
  select(-op) %>% 
  relocate(any_of(c("rhs","ihs"))) %>% 
  rename(parallel.med_e=rhs,var2=lhs, Unstandardized.beta=est, SE=se)

med.e1=med_e1 %>% 
  bind_cols(med_e.stand1) %>% 
  relocate(any_of(c("parallel.med_e","var2","label","Unstandardized.beta", "Standardized.beta"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-var2, -label) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8,9,10,11,12),j=1, value = as_paragraph (c("a - Group-->PC", "b - PC-->PPI","f - PC-->MH","e' - Group-->PPI",
                                                                       "c - Group-->NA","d - NA-->PPI",
                                                                       "g - NA-->MH","h' - Group-->MH","Group-->PC-->PPI",
                                                                       "Group-->PC-->MH","Group-->NA-->PPI","Group-->NA-->MH"))) %>% 
  autofit()

# fit measures

fit.e=sum.fit_parallel.med_e$FIT %>% 
  tibble()
fit.e=fit.e[c(3,4,5,9,17),]
fit.e=fit.e %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()


# Imputed----


med.e1.imp=sum.fit_parallel.med_e.imp %>% 
  filter(label%in% c("a","b","f","e","c","d","g","h","group_pc_pelvic.pain",
                     "group_pc_mental.health","group_na_pelvic.pain",
                     "group_na.mental.health")) %>% 
  select(- op, - exo, - df, - std.lv, - std.nox,- label, - lhs) %>% 
  relocate(any_of(c("rhs","ihs","est","std.all"))) %>% 
  rename(parallel.med_e.imp=rhs, Unstandardized.beta=est, Standardized.beta=std.all , SE=se)

med.e1.imp=med.e1.imp %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8,9,10,11,12),j=1, value = as_paragraph (c("a - Group-->PC", "b - PC-->PPI","f - PC-->MH","e' - Group-->PPI",
                                                                       "c - Group-->NA","d - NA-->PPI",
                                                                       "g - NA-->MH","h' - Group-->MH","Group-->PC-->PPI",
                                                                       "Group-->PC-->MH","Group-->NA-->PPI","Group-->NA-->MH"))) %>% 
  autofit()

# fit measures

fit.e1.imp=fitMeasures(fit_parallel.med_e.imp)
fit.e1.imp=fit.e1.imp %>% 
  tibble() 
fit.e1.imp=fit.e1.imp[c(1,2,3,16,25),]

fit.e1.imp=fit.e1.imp %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()


# Serial model 1, PC-PA effect on MH controlled by pain - add pain-->MH----

serial.med_b.pain= '
# Path a
PCS_total_t2~a*group_intervention+PCS_total_t1

# Path b
pelvic.pain_t2~b*PCS_total_t2+pelvic.pain_t1+PCS_total_t1


# Path e

pelvic.pain_t2~e*group_intervention

# Path h

affect_positive_t2~h*PCS_total_t2+PCS_total_t1+affect_positive_t1

# Path g

sf_36_mental.health_t2~g*affect_positive_t2+affect_positive_t1

## Path i

sf_36_mental.health_t2~i*group_intervention

## Path j

sf_36_mental.health_t2~j*pelvic.pain_t2+pelvic.pain_t1

# Indirect effect 
group_pc_pelvic.pain:=a*b
group_pc_pa.mh:=a*h*g
a_b_j:= a*b*j
'


# Fit/estimate the model
set.seed(2021)

# Resampling method: Percentile Bootstraping, does not assume normality

fit_serial.med_b3=sem(serial.med_b3.pain,med.mechanism , se="bootstrap", bootstrap= 2000)


# Summarize the results/output

sum.fit_serial.med_b=summary(fit_serial.med_b3.pain, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)


# Standardized parameter estimates.

med_b.stand3=standardizedSolution(fit_serial.med_b3.pain )

med_b3=parameterEstimates(fit_serial.med_b3.pain, ci=TRUE, level=0.95, boot.ci.type= "perc")


# misfit of the bivariate associations - residuals in correlational units, subtraction of the observed - model-implied matrices-----
#Large positive values indicate the model underpredicts the correlation; large negative values suggest overprediction of correlation

resid(fit_serial.med_b.pain, "cor")

#imputed----

set.seed(2021)

fit_serial.med_b3.imp=sem.mi(serial.med_b3.pain,imputed_med)



sum.fit_serial.med_b3.imp=summary(fit_serial.med_b3.imp.pain, fit.measures=TRUE, ci = TRUE, stand = TRUE, rsq = TRUE,
                                  standardized= TRUE)



fitMeasures(fit_serial.med_b3.imp.pain)

#Large positive values indicate the model underpredicts the correlation; large negative values suggest overprediction of correlation

resid(fit_serial.med_b3.imp.pain, "cor")



# Serial mediator model 1, (NA-PC), 2 Exogenous (PELVIC PAIN-MENATL HEALTH)----



serial.med_c.pain= '
# Path a
PCS_total_t2~a*group_intervention+PCS_total_t1

# Path b
pelvic.pain_t2~b*PCS_total_t2+pelvic.pain_t1+PCS_total_t1


# Path e

pelvic.pain_t2~e*group_intervention

# Path h

affect_Negative_t2~h*PCS_total_t2+PCS_total_t1+affect_Negative_t1

# Path g

sf_36_mental.health_t2~g*affect_Negative_t2+affect_Negative_t1

## Path i

sf_36_mental.health_t2~i*group_intervention

## Path j

sf_36_mental.health_t2~j*pelvic.pain_t2+pelvic.pain_t1

# Indirect effect 
group_pc_pelvic.pain:=a*b
pc_pn.mh:=h*g
group_pc_pn.mh:=a*h*g
a_b_j:=a*b*j
'



# Fit/estimate the model
set.seed(2021)

# Resampling method: Percentile Bootstraping, does not assume normality

fit_serial.med_c=sem(serial.med_c.pain,med.mechanism , se="bootstrap", bootstrap= 2000)



# Summarize the results/output

sum.fit_serial.med_c=summary(fit_serial.med_c.pain, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)



# Standardized parameter estimates.

med_serial.c.stand=standardizedSolution(fit_serial.med_c.pain )

med_serial.c=parameterEstimates(fit_serial.med_c.pain, ci=TRUE, level=0.95, boot.ci.type= "perc")




resid(fit_serial.med_c.pain, "cor")


resid(fit_serial.med_c.pain, "cor")

#imputed----

set.seed(2021)


fit_serial.med_c.imp=sem.mi(serial.med_c,imputed_med.pain)


sum.fit_serial.med_c.imp=summary(fit_serial.med_c.imp.pain, fit.measures=TRUE, ci = TRUE, stand = TRUE, rsq = TRUE,
                                 standardized= TRUE)
sum.fit_serial.med_c.imp=tibble(sum.fit_serial.med_c.imp.pain)



fitMeasures(fit_serial.med_c.imp.pain)


#Large positive values indicate the model underpredicts the correlation; large negative values suggest overprediction of correlation

resid(fit_serial.med_c.imp.pain, "cor")

# Serial mediator model 1,  (PA-PC), 2 Exogenous (PAIN UNPLEASANTNESS-MENTAL HEALTH)----



serial.med_d.pain= '
# Path a
PCS_total_t2~a*group_intervention+PCS_total_t1

# Path b
pain.unpleasantness_t2~b*PCS_total_t2+pain.unpleasantness_t1+PCS_total_t1

# Path e

pain.unpleasantness_t2~e*group_intervention

# Path h

affect_positive_t2~h*PCS_total_t2+PCS_total_t1+affect_positive_t1

# Path g

sf_36_mental.health_t2~g*affect_positive_t2+affect_positive_t1+sf_36_mental.health_t1

## Path i

sf_36_mental.health_t2~i*group_intervention

## Path j

sf_36_mental.health_t2~j*pain.unpleasantness_t2+pain.unpleasantness_t1

# Indirect effect 
a_b:=a*b
a_h_g:=a*h*g
a_b_j:=a*b*j
'


# Fit/estimate the model
set.seed(2021)

# Resampling method: Percentile Bootstraping, does not assume normality


fit_serial.med_d.pain=sem(serial.med_d.pain,med.mechanism , se="bootstrap", bootstrap= 2000)

# Summarize the results/output

sum.fit_serial.med_d=summary(fit_serial.med_d.pain, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)


# Standardized parameter estimates.

med_serial.d.stand =standardizedSolution(fit_serial.med_d.pain )

med_serial.d=parameterEstimates(fit_serial.med_d.pain, ci=TRUE, level=0.95, boot.ci.type= "perc")


resid(fit_serial.med_d.pain, "cor")

#imputed----
install.packages("semTools")

set.seed(2021)

fit_serial.med_d.imp=sem.mi(serial.med_d,imputed_med.pain)


sum.fit_serial.med_d.imp=summary(fit_serial.med_d.imp.pain, fit.measures=TRUE, ci = TRUE, stand = TRUE, rsq = TRUE,
                                 standardized= TRUE)
sum.fit_serial.med_d.imp=tibble(sum.fit_serial.med_d.imp.pain)



fitMeasures(fit_serial.med_d.imp.pain)


resid(fit_serial.med_d.imp.pain, "cor")



#Serial mediator model 2 e----

serial.med_e.pain= '
# Path a
PCS_total_t2~a*group_intervention+PCS_total_t1

# Path b
pain.unpleasantness_t2~b*PCS_total_t2+pain.unpleasantness_t1+PCS_total_t1


# Path e

pain.unpleasantness_t2~e*group_intervention

# Path h

affect_Negative_t2~h*PCS_total_t2+PCS_total_t1+affect_Negative_t1

# Path g

sf_36_mental.health_t2~g*affect_Negative_t2+affect_Negative_t1

## Path i

sf_36_mental.health_t2~i*group_intervention

## Path j

sf_36_mental.health_t2~j*pain.unpleasantness_t2+pain.unpleasantness_t1

# Indirect effect 
a_b:=a*b
h_g:=h*g
a_h_g:=a*h*g
a_b_j:=a*b*j
'


serial.med_e.pain.r= '
# Path a
PCS_total_t2~a*group_intervention+PCS_total_t1

# Path b
pelvic.pain_t2~b*PCS_total_t2+pelvic.pain_t1+PCS_total_t1


## Path i

sf_36_mental.health_t2~i*group_intervention

## Path j

sf_36_mental.health_t2~j*pelvic.pain_t2+pelvic.pain_t1

# Indirect effect 

a_b_j:=a*b*j
'

# Fit/estimate the model
set.seed(2021)

# Resampling method: Percentile Bootstraping, does not assume normality

fit_serial.med_e=sem(serial.med_e,med.mechanism.pain , se="bootstrap", bootstrap= 2000)


#r2

fit_serial.med_e.pain.r=sem(serial.med_e.pain.r,med.mechanism , se="bootstrap", bootstrap= 2000)

# Summarize the results/output

sum.fit_serial.med_e=summary(fit_serial.med_e.pain, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)



# Standardized parameter estimates.

med_serial.e.stand=standardizedSolution(fit_serial.med_e.pain )

med_serial.e =parameterEstimates(fit_serial.med_e.pain, ci=TRUE, level=0.95, boot.ci.type= "perc")


resid(fit_serial.med_e.pain, "cor")

#imputed----

set.seed(2021)

fit_serial.med_e.imp=sem.mi(serial.med_e.pain,imputed_med)


sum.fit_serial.med_e.imp=summary(fit_serial.med_e.imp.pain, fit.measures=TRUE, ci = TRUE, stand = TRUE, rsq = TRUE,
                                 standardized= TRUE)
sum.fit_serial.med_e.imp=tibble(sum.fit_serial.med_e.imp.pain)


summary(fit_serial.med_e.imp.pain, fit.measures=TRUE, ci = TRUE, stand = TRUE, rsq = TRUE,
        standardized= TRUE)

fitMeasures(fit_serial.med_e.imp.pain)

resid(fit_serial.med_e.imp.pain, "cor")


# Table serial models 1----

# Model 1 (serial.med_b3)----
med_b3.stand= med_b.stand3 %>% 
  filter(label%in% c("a","b","e","h","g","i","j","group_pc_pelvic.pain",
                     "group_pc_pa.mh","a_b_j ")) %>% 
  select(est.std) %>% 
  rename(Standardized.beta=est.std)

med.b3=med_b3 %>% 
  filter(label%in% c("a","b","e","h","g","i","j","group_pc_pelvic.pain",
                     "group_pc_pa.mh","a_b_j ")) %>% 
  select(-op) %>% 
  relocate(any_of(c("rhs","ihs"))) %>% 
  rename(serial.med_b3=rhs,var2=lhs, Unstandardized.beta=est, SE=se)

med.b3=med.b3 %>% 
  bind_cols(med_b3.stand) %>% 
  relocate(any_of(c("serial.med_b3","var2","label","Unstandardized.beta", "Standardized.beta"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-var2, -label) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8,9,10),j=1, value = as_paragraph (c("a - Group-->PC", "b - PC-->PPI","e' - Group-->PPI",
                                                            "h - PCS-->PA","g - PA-->MH","i' - Group-->MH","j - PPI-->MH", "Group-->PC-->PPI",
                                                            "Group-->PC-->PA-->MH","Group-->PC-->PPI-->MH"))) %>% 
  autofit()

# fit measures

fit.sb=sum.fit_serial.med_b$FIT %>% 
  tibble()
fit.sb=fit.sb[c(3,4,5,9,17),]
fit.sb=fit.sb %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()


#imputed----


med.serialb3.imp=sum.fit_serial.med_b3.imp %>% 
  filter(label%in% c("a","b","e","h","g","i","j","group_pc_pelvic.pain",
                     "group_pc_pa.mh","a_b_j")) %>% 
  select(- op, - exo, - df, - std.lv, - std.nox,- label, - lhs) %>% 
  relocate(any_of(c("rhs","ihs","est","std.all"))) %>% 
  rename(serial.med_b.imp=rhs, Unstandardized.beta=est, Standardized.beta=std.all , SE=se)

med.serialb3.imp=med.serialb3.imp %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8,9,10),j=1, value = as_paragraph (c("a - Group-->PC", "b - PC-->PPI","e' - Group-->PPI",
                                                                 "h - PCS-->PA","g - PA-->MH","i' - Group-->MH","j - PPI-->MH", "Group-->PC-->PPI",
                                                                 "Group-->PC-->PA-->MH","Group-->PC-->PPI-->MH"))) %>% 
  autofit()


# fit measures

fit.serialb3.imp=fitMeasures(fit_serial.med_b3.imp)
fit.serialb3.imp=fit.serialb3.imp %>% 
  tibble() 
fit.serialb3.imp=fit.serialb3.imp[c(1,2,3,16,25),]

fit.serialb3.imp=fit.serialb3.imp %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()


# Model 2 (serial.med_c)----
med_serialc.stand= med_serial.c.stand %>% 
  filter(label%in% c("a","b","e","h","g","i","j", "group_pc_pelvic.pain",
                     "group_pc_pn.mh","a_b_j")) %>% 
  select(est.std) %>% 
  rename(Standardized.beta=est.std)

med.serial.c=med_serial.c %>% 
  filter(label%in% c("a","b","e","h","g","i", "j" ,"group_pc_pelvic.pain",
                     "group_pc_pn.mh","a_b_j")) %>% 
  select(-op) %>% 
  relocate(any_of(c("rhs","ihs"))) %>% 
  rename(serial.med_c=rhs,var2=lhs, Unstandardized.beta=est, SE=se)

med.serialc=med.serial.c %>% 
  bind_cols(med_serialc.stand) %>% 
  relocate(any_of(c("serial.med_c","var2","label","Unstandardized.beta", "Standardized.beta"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-var2, -label) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8,9,10),j=1, value = as_paragraph (c("a - Group-->PC", "b - PC-->PPI","e' - Group-->PPI",
                                                            "h - PC-->NA","g - NA-->MH","i' - Group-->MH","j - PPI-->MH","Group-->PC-->PPI",
                                                            "Group-->PC-->NA-->MH","Group-->PC-->PPI-->MH"))) %>% 
  autofit()

# fit measures

fit.sc=sum.fit_serial.med_c$FIT %>% 
  tibble()
fit.sc=fit.sc[c(3,4,5,9,17),]
fit.sc=fit.sc %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()

#imputed----

med.serialc.imp=sum.fit_serial.med_c.imp %>% 
  filter(label%in% c("a","b","e","h","g","i","j", "group_pc_pelvic.pain",
                     "group_pc_pn.mh","a_b_j")) %>% 
  select(- op, - exo, - df, - std.lv, - std.nox,- label, - lhs) %>% 
  relocate(any_of(c("rhs","ihs","est","std.all"))) %>% 
  rename(serial.med_c.imp=rhs, Unstandardized.beta=est, Standardized.beta=std.all , SE=se)

med.serialc.imp=med.serialc.imp %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8),j=1, value = as_paragraph (c("a - Group-->PC", "b - PC-->PPI","e' - Group-->PPI",
                                                            "h - PC-->NA","g - NA-->MH","i' - Group-->MH","j - PPI-->MH","Group-->PC-->PPI",
                                                            "Group-->PC-->NA-->MH","Group-->PC-->PPI-->MH"))) %>% 
  autofit()


# fit measures

fit.sc.imp=fitMeasures(fit_serial.med_c.imp) %>% 
  tibble()

fit.sc.imp=fit.sc.imp[c(1,2,3,16,25),]

fit.sc.imp=fit.sc.imp %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()



# Model 3 (serial.med_d)----
med_seriald.stand= med_serial.d.stand %>% 
  filter(label%in% c("a","b","e","h","g","i","j", "a_b",
                     "a_h_g","a_b_j")) %>% 
  select(est.std) %>% 
  rename(Standardized.beta=est.std)

med.serial.d=med_serial.d %>% 
  filter(label%in% c("a","b","e","h","g","i","j", "a_b",
                     "a_h_g","a_b_j")) %>% 
  select(-op) %>% 
  relocate(any_of(c("rhs","ihs"))) %>% 
  rename(serial.med_d=rhs,var2=lhs, Unstandardized.beta=est, SE=se)

med.seriald=med.serial.d %>% 
  bind_cols(med_seriald.stand) %>% 
  relocate(any_of(c("serial.med_d","var2","label","Unstandardized.beta", "Standardized.beta"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-var2, -label) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8,9,10),j=1, value = as_paragraph (c("a - Group-->PC", "b - PC-->PU","e' - Group-->PU",
                                                            "h - PC-->PA","g - PA-->MH","i' - Group-->MH","j - PU-->MH" ,"Group-->PC-->PU",
                                                            "Group-->PC-->PA-->MH","Group-->PC-->PU-->MH"))) %>% 
  autofit()


# fit measures

fit.sd=sum.fit_serial.med_d$FIT %>% 
  tibble()
fit.sd=fit.sd[c(3,4,5,9,17),]
fit.sd=fit.sd %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()

#imputed----

med.seriald.imp=sum.fit_serial.med_d.imp %>% 
  filter(label%in% c("a","b","e","h","g","i","j", "a_b",
                     "a_h_g","a_b_j")) %>% 
  select(- op, - exo, - df, - std.lv, - std.nox,- label, - lhs) %>% 
  relocate(any_of(c("rhs","ihs","est","std.all"))) %>% 
  rename(serial.med_d.imp=rhs, Unstandardized.beta=est, Standardized.beta=std.all , SE=se)

med.seriald.imp=med.seriald.imp %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8,9,10),j=1, value = as_paragraph (c("a - Group-->PC", "b - PC-->PU","e' - Group-->PU",
                                                                 "h - PC-->PA","g - PA-->MH","i' - Group-->MH","j - PU-->MH" ,"Group-->PC-->PU",
                                                                 "Group-->PC-->PA-->MH","Group-->PC-->PU-->MH"))) %>% 
  autofit()

# fit measures
fit.sd.imp=fitMeasures(fit_serial.med_d.imp) %>% 
  tibble()

fit.sd.imp=fit.sd.imp[c(1,2,3,16,25),]

fit.sd.imp=fit.sd.imp %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()


# Model 4 (serial.med_e)----

med_seriale.stand= med_serial.e.stand %>% 
  filter(label%in% c("a","b","e","h","g","i", "j" ,"a_b",
                     "a_h_g","a_b_j")) %>% 
  select(est.std) %>% 
  rename(Standardized.beta=est.std)

med.serial.e=med_serial.e %>% 
  filter(label%in% c("a","b","e","h","g","i","j" ,"a_b",
                     "a_h_g","a_b_j")) %>% 
  select(-op) %>% 
  relocate(any_of(c("rhs","ihs"))) %>% 
  rename(serial.med_e=rhs,var2=lhs, Unstandardized.beta=est, SE=se)

med.seriale=med.serial.e %>% 
  bind_cols(med_seriale.stand) %>% 
  relocate(any_of(c("serial.med_e","var2","label","Unstandardized.beta", "Standardized.beta"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-var2, -label) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8,9,10),j=1, value = as_paragraph (c("a - Group-->PC", "b - PC-->PU","e' - Group-->PU",
                                                            "h - PC-->NA","g - NA-->MH","i' - Group-->MH","j - PU-->MH","Group-->PC-->PU",
                                                            "Group-->PC-->NA-->MH","Group-->PC-->PU-->MH"))) %>% 
  autofit()

# fit measures

fit.se=sum.fit_serial.med_e$FIT %>% 
  tibble()
fit.se=fit.se[c(3,4,5,9,17),]
fit.se=fit.se %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()

#imputed----

med.seriale.imp=sum.fit_serial.med_e.imp %>% 
  filter(label%in% c("a","b","e","h","g","i","j" ,"a_b",
                     "a_h_g","a_b_j")) %>% 
  select(- op, - exo, - df, - std.lv, - std.nox,- label, - lhs) %>% 
  relocate(any_of(c("rhs","ihs","est","std.all"))) %>% 
  rename(serial.med_e.imp=rhs, Unstandardized.beta=est, Standardized.beta=std.all , SE=se)

med.seriale.imp=med.seriale.imp %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8,9,10),j=1, value = as_paragraph (c("a - Group-->PC", "b - PC-->PU","e' - Group-->PU",
                                                                 "h - PC-->NA","g - NA-->MH","i' - Group-->MH","j - PU-->MH","Group-->PC-->PU",
                                                                 "Group-->PC-->NA-->MH","Group-->PC-->PU-->MH"))) %>% 
  autofit()


# fit measures
fit.se.imp=fitMeasures(fit_serial.med_e.imp) %>% 
  tibble()

fit.se.imp=fit.se.imp[c(1,2,3,16,25),]

fit.se.imp=fit.se.imp %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()




#Effectsize Mediators


ppi.effectsize=lm(pelvic.pain_t2~group_intervention+pelvic.pain_t1, data = med.mechanism[-c(48,49),])
summary(ppi.effectsize)
ppi.effectsize1=cohens_f_squared(ppi.effectsize)

pu.effectsize=lm(pain.unpleasantness_t2~group_intervention+pain.unpleasantness_t1, data = med.mechanism)
summary(pu.effectsize)
pu.effectsize1=cohens_f_squared(pu.effectsize)



#Effectsize Mediators


pc.effectsize=lm(PCS_total_t2~group_intervention+PCS_total_t1, data = med.mechanism)
summary(pc.effectsize)

pc.rum.effectsize=lm(PCS_rumination_t2~group_intervention+PCS_rumination_t1, data = med.mechanism)
summary(pc.rum.effectsize)

pc.mag.effectsize=lm(PCS_magnification_t2~group_intervention+PCS_magnification_t1, data = med.mechanism)
summary(pc.mag.effectsize)

pc.help.effectsize=lm(PCS_helplessness_t2~group_intervention+PCS_helplessness_t1, data = med.mechanism)
summary(pc.help.effectsize)

pa.effectsize=lm(affect_positive_t2~group_intervention+affect_positive_t1, data = med.mechanism)
summary(pa.effectsize)

na.effectsize=lm(affect_Negative_t2~group_intervention+affect_Negative_t1, data = med.mechanism)
summary(na.effectsize)


pc.effectsize1=cohens_f_squared(pc.effectsize)
pc.rum.effectsize1=cohens_f_squared(pc.rum.effectsize)
pc.mag.effectsize1=cohens_f_squared(pc.mag.effectsize)
pc.help.effectsize1=cohens_f_squared(pc.help.effectsize)
pa.effectsize1=cohens_f_squared(pa.effectsize)
na.effectsize1=cohens_f_squared(na.effectsize)


#table effectsize

pc.effectsize1=pc.effectsize1 %>% 
  filter(Parameter%in% c("group_intervention")) %>% 
  mutate( Parameter=ifelse(Parameter=="group_intervention","PC", Parameter ) ) %>% 
  rename(Post=Parameter)

pc.rum.effectsize1=pc.rum.effectsize1 %>% 
  filter(Parameter%in% c("group_intervention")) %>% 
  mutate( Parameter=ifelse(Parameter=="group_intervention","Rumination", Parameter ) ) %>% 
  rename(Post=Parameter)

pc.mag.effectsize1=pc.mag.effectsize1 %>% 
  filter(Parameter%in% c("group_intervention")) %>% 
  mutate( Parameter=ifelse(Parameter=="group_intervention","Magnitication", Parameter ) ) %>% 
  rename(Post=Parameter)

pc.help.effectsize1=pc.help.effectsize1 %>% 
  filter(Parameter%in% c("group_intervention")) %>% 
  mutate( Parameter=ifelse(Parameter=="group_intervention","Helplessness", Parameter ) ) %>% 
  rename(Post=Parameter)


pa.effectsize1=pa.effectsize1 %>% 
  filter(Parameter%in% c("group_intervention")) %>% 
  mutate( Parameter=ifelse(Parameter=="group_intervention","PA", Parameter ) ) %>% 
  rename(Post=Parameter)

na.effectsize1=na.effectsize1 %>% 
  filter(Parameter%in% c("group_intervention")) %>% 
  mutate( Parameter=ifelse(Parameter=="group_intervention","NA", Parameter ) ) %>% 
  rename(Post=Parameter)


pc.effectsize1 %>% 
  bind_rows(pc.rum.effectsize1,pc.mag.effectsize1,pc.help.effectsize1,
            pa.effectsize1,na.effectsize1)%>%
  mutate_if(is.numeric, round, 3)


# Serial model 2 PC-->PA--->PPI----

serial.med_b3.alt= '
# Path a
PCS_total_t2~a*group_intervention+PCS_total_t1

# Path h

affect_positive_t2~h*PCS_total_t2+PCS_total_t1+affect_positive_t1

# Path b
pelvic.pain_t2~b*affect_positive_t2+pelvic.pain_t1+affect_positive_t1


# Path e

pelvic.pain_t2~e*group_intervention


# Path g

sf_36_mental.health_t2~g*affect_positive_t2+affect_positive_t1

## Path i

sf_36_mental.health_t2~i*group_intervention

# Indirect effect 
a_h_b:=a*h*b
a_h_g:=a*h*g
'


# Fit/estimate the model
set.seed(2021)

# Resampling method: Percentile Bootstraping, does not assume normality

fit_serial.med_b3.alt=sem(serial.med_b3.alt,med.mechanism , se="bootstrap", bootstrap= 2000)


# Summarize the results/output

sum.fit_serial.med_b.alt=summary(fit_serial.med_b3.alt, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)

# Standardized parameter estimates.

med_b.stand3.alt=standardizedSolution(fit_serial.med_b3.alt )

med_b3.alt=parameterEstimates(fit_serial.med_b3.alt, ci=TRUE, level=0.95, boot.ci.type= "perc")


#Residual correltion matrix
resid(fit_serial.med_b3.alt, "cor")


#imputed----

set.seed(2021)

fit_serial.med_b3.imp.alt=sem.mi(serial.med_b3.alt,imputed_med)

sum.fit_serial.med_b3.imp.alt=summary(fit_serial.med_b3.imp.alt, fit.measures=TRUE, ci = TRUE, stand = TRUE, rsq = TRUE, standardized= TRUE)

sum.fit_serial.med_b3.imp.alt=tibble(sum.fit_serial.med_b3.imp.alt)

fitMeasures(fit_serial.med_b3.imp.alt)


#Residual correlation matrix
resid(fit_serial.med_b3.imp.alt, "cor")


# Serial model 2, PC-->NA--->PPI----

serial.med_c.alt= '
# Path a
PCS_total_t2~a*group_intervention+PCS_total_t1

# Path h

affect_Negative_t2~h*PCS_total_t2+PCS_total_t1+affect_Negative_t1

# Path b
pelvic.pain_t2~b*affect_Negative_t2+pelvic.pain_t1+affect_Negative_t1


# Path e

pelvic.pain_t2~e*group_intervention


# Path g

sf_36_mental.health_t2~g*affect_Negative_t2+affect_Negative_t1

## Path i

sf_36_mental.health_t2~i*group_intervention

# Indirect effect 
a_h_b:=a*h*b
a_h_g:=a*h*g
'


# Fit/estimate the model
set.seed(2021)

# Resampling method: Percentile Bootstraping, does not assume normality

fit_serial.med_c.alt=sem(serial.med_c.alt,med.mechanism , se="bootstrap", bootstrap= 2000)


# Summarize the results/output

sum.fit_serial.med_c.alt=summary(fit_serial.med_c.alt, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)

# Standardized parameter estimates.

med_c.stand.alt=standardizedSolution(fit_serial.med_c.alt )

med_c.alt=parameterEstimates(fit_serial.med_c.alt, ci=TRUE, level=0.95, boot.ci.type= "perc")

#imputed----

set.seed(2021)

fit_serial.med_c.imp.alt=sem.mi(serial.med_c.alt,imputed_med)

sum.fit_serial.med_c.imp.alt=summary(fit_serial.med_c.imp.alt, fit.measures=TRUE, ci = TRUE, stand = TRUE, rsq = TRUE,
                                     standardized= TRUE)
sum.fit_serial.med_c.imp.alt=tibble(sum.fit_serial.med_c.imp.alt)

fitMeasures(fit_serial.med_c.imp.alt)

#Residual correlation matrix
resid(fit_serial.med_c.imp.alt, "cor")


#Serial mediator 2,  PC-->PA--->PU----


serial.med_d.alt= '
# Path a
PCS_total_t2~a*group_intervention+PCS_total_t1

# Path h

affect_positive_t2~h*PCS_total_t2+PCS_total_t1+affect_positive_t1

# Path b
pain.unpleasantness_t2~b*affect_positive_t2+pain.unpleasantness_t1+affect_positive_t1


# Path e

pain.unpleasantness_t2~e*group_intervention


# Path g

sf_36_mental.health_t2~g*affect_positive_t2+affect_positive_t1

## Path i

sf_36_mental.health_t2~i*group_intervention

# Indirect effect 
a_h_b:=a*h*b
a_h_g:=a*h*g
'


# Fit/estimate the model
set.seed(2021)

# Resampling method: Percentile Bootstraping, does not assume normality

fit_serial.med_d.alt=sem(serial.med_d.alt,med.mechanism , se="bootstrap", bootstrap= 2000)


# Summarize the results/output

sum.fit_serial.med_d.alt=summary(fit_serial.med_d.alt, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)

# Standardized parameter estimates.

med_serial.d.stand.alt =standardizedSolution(fit_serial.med_d.alt )

med_serial.d.alt=parameterEstimates(fit_serial.med_d.alt, ci=TRUE, level=0.95, boot.ci.type= "perc")

#imputed----

set.seed(2021)

fit_serial.med_d.imp.alt=sem.mi(serial.med_d.alt,imputed_med)

sum.fit_serial.med_d.imp.alt=summary(fit_serial.med_d.imp.alt, fit.measures=TRUE, ci = TRUE, stand = TRUE, rsq = TRUE,
                                     standardized= TRUE)
sum.fit_serial.med_d.imp.alt=tibble(sum.fit_serial.med_d.imp.alt)

fitMeasures(fit_serial.med_d.imp.alt)

#Residual correlation matrix
resid(fit_serial.med_d.imp.alt, "cor")

#serial.med_e----

serial.med_e.alt= '
# Path a
PCS_total_t2~a*group_intervention+PCS_total_t1

# Path h

affect_Negative_t2~h*PCS_total_t2+PCS_total_t1+affect_Negative_t1

# Path b
pain.unpleasantness_t2~b*affect_Negative_t2+pain.unpleasantness_t1+affect_Negative_t1


# Path e

pain.unpleasantness_t2~e*group_intervention


# Path g

sf_36_mental.health_t2~g*affect_Negative_t2+affect_Negative_t1

## Path i

sf_36_mental.health_t2~i*group_intervention

# Indirect effect 
a_h_b:=a*h*b
a_h_g:=a*h*g
'


# Fit/estimate the model
set.seed(2021)

# Resampling method: Percentile Bootstraping, does not assume normality

fit_serial.med_e.alt=sem(serial.med_e.alt,med.mechanism , se="bootstrap", bootstrap= 2000)


# Summarize the results/output

sum.fit_serial.med_e.alt=summary(fit_serial.med_e.alt, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)

# Standardized parameter estimates.

med_serial.e.stand.alt =standardizedSolution(fit_serial.med_e.alt )

med_serial.e.alt=parameterEstimates(fit_serial.med_e.alt, ci=TRUE, level=0.95, boot.ci.type= "perc")

#imputed----

set.seed(2021)

fit_serial.med_e.imp.alt=sem.mi(serial.med_e.alt,imputed_med)

sum.fit_serial.med_e.imp.alt=summary(fit_serial.med_e.imp.alt, fit.measures=TRUE, ci = TRUE, stand = TRUE, rsq = TRUE,
                                     standardized= TRUE)
sum.fit_serial.med_e.imp.alt=tibble(sum.fit_serial.med_e.imp.alt)

fitMeasures(fit_serial.med_e.imp.alt)


#Residual correlation matrix
resid(fit_serial.med_e.imp.alt, "cor")





# Table serial models 2----



# Model 1b (serial.med_b3.alt)----
med_b3.stand.alt= med_b.stand3.alt %>% 
  filter(label%in% c("a","h","b","e","g","i","a_h_b",
                     "a_h_g")) %>% 
  select(est.std) %>% 
  rename(Standardized.beta=est.std)

med.b3.alt=med_b3.alt %>% 
  filter(label%in% c("a","h","b","e","g","i","a_h_b",
                     "a_h_g")) %>% 
  select(-op) %>% 
  relocate(any_of(c("rhs","ihs"))) %>% 
  rename(serial.med_b3.alt=rhs,var2=lhs, Unstandardized.beta=est, SE=se)

med.b3.alt=med.b3.alt %>% 
  bind_cols(med_b3.stand.alt) %>% 
  relocate(any_of(c("serial.med_b3.alt","var2","label","Unstandardized.beta", "Standardized.beta"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-var2, -label) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8),j=1, value = as_paragraph (c("a - Group-->PC", "h - PC-->PA", "b - PA-->PPI",
                                                            "e' - Group-->PPI",
                                                            "g - PA-->MH","i' - Group-->MH","Group-->PC-->PA-->PPI",
                                                            "Group-->PC-->PA-->MH"))) %>% 
  autofit()

# fit measures

fit.sb.alt=sum.fit_serial.med_b.alt$FIT %>% 
  tibble()
fit.sb.alt=fit.sb.alt[c(3,4,5,9,17),]
fit.sb.alt=fit.sb.alt %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()
#imputed----


med.serialb3.imp.alt=sum.fit_serial.med_b3.imp.alt %>% 
  filter(label%in% c("a","h","b","e","g","i","a_h_b",
                     "a_h_g")) %>% 
  select(- op, - exo, - df, - std.lv, - std.nox,- label, - lhs) %>% 
  relocate(any_of(c("rhs","ihs","est","std.all"))) %>% 
  rename(serial.med_b.imp.alt=rhs, Unstandardized.beta=est, Standardized.beta=std.all , SE=se)

med.serialb3.imp.alt=med.serialb3.imp.alt %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8),j=1, value = as_paragraph (c("a - Group-->PC", "h - PC-->PA", "b - PA-->PPI",
                                                            "e' - Group-->PPI",
                                                            "g - PA-->MH","i' - Group-->MH","Group-->PC-->PA-->PPI",
                                                            "Group-->PC-->PA-->MH"))) %>% 
  autofit()

# fit measures

fit.serialb3.imp.alt=fitMeasures(fit_serial.med_b3.imp.alt)
fit.serialb3.imp.alt=fit.serialb3.imp.alt %>% 
  tibble() 
fit.serialb3.imp.alt=fit.serialb3.imp.alt[c(1,2,3,16,25),]

fit.serialb3.imp.alt=fit.serialb3.imp.alt %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()


# Model 2b (serial.med_c.alt)----

med_serialc.stand.alt= med_c.stand.alt %>% 
  filter(label%in% c("a","h","b","e","g","i","a_h_b",
                     "a_h_g")) %>% 
  select(est.std) %>% 
  rename(Standardized.beta=est.std)

med.serial.c.alt=med_c.alt %>% 
  filter(label%in% c("a","h","b","e","g","i","a_h_b",
                     "a_h_g")) %>% 
  select(-op) %>% 
  relocate(any_of(c("rhs","ihs"))) %>% 
  rename(serial.med_c.alt=rhs,var2=lhs, Unstandardized.beta=est, SE=se)

med.serialc.alt=med.serial.c.alt %>% 
  bind_cols(med_serialc.stand.alt) %>% 
  relocate(any_of(c("serial.med_c.alt","var2","label","Unstandardized.beta", "Standardized.beta"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-var2, -label) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8),j=1, value = as_paragraph (c("a - Group-->PC","h - PC-->NA", "b - NA-->PPI","e' - Group-->PPI",
                                                            "g - NA-->MH","i' - Group-->MH","Group-->PC-->NA-->PPI",
                                                            "Group-->PC-->NA-->MH"))) %>% 
  autofit()

# fit measures

fit.sc.alt=sum.fit_serial.med_c.alt$FIT %>% 
  tibble()
fit.sc.alt=fit.sc.alt[c(3,4,5,9,17),]
fit.sc.alt=fit.sc.alt %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()

#imputed----

med.serialc.imp.alt=sum.fit_serial.med_c.imp.alt %>% 
  filter(label%in% c("a","h","b","e","g","i","a_h_b",
                     "a_h_g")) %>% 
  select(- op, - exo, - df, - std.lv, - std.nox,- label, - lhs) %>% 
  relocate(any_of(c("rhs","ihs","est","std.all"))) %>% 
  rename(serial.med_c.imp.alt=rhs, Unstandardized.beta=est, Standardized.beta=std.all , SE=se)

med.serialc.imp.alt=med.serialc.imp.alt %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8),j=1, value = as_paragraph (c("a - Group-->PC","h - PC-->NA", "b - NA-->PPI","e' - Group-->PPI",
                                                            "g - NA-->MH","i' - Group-->MH","Group-->PC-->NA-->PPI",
                                                            "Group-->PC-->NA-->MH"))) %>% 
  autofit()


# fit measures

fit.sc.imp.alt=fitMeasures(fit_serial.med_c.imp.alt) %>% 
  tibble()

fit.sc.imp.alt=fit.sc.imp.alt[c(1,2,3,16,25),]

fit.sc.imp.alt=fit.sc.imp.alt %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()


# Model 3b (serial.med_d.alt)----
med_seriald.stand.alt= med_serial.d.stand.alt %>% 
  filter(label%in% c("a","h","b","e","g","i","a_h_b",
                     "a_h_g")) %>% 
  select(est.std) %>% 
  rename(Standardized.beta=est.std)

med.serial.d.alt=med_serial.d.alt %>% 
  filter(label%in% c("a","h","b","e","g","i","a_h_b",
                     "a_h_g")) %>% 
  select(-op) %>% 
  relocate(any_of(c("rhs","ihs"))) %>% 
  rename(serial.med_d.alt=rhs,var2=lhs, Unstandardized.beta=est, SE=se)

med.seriald.alt=med.serial.d.alt %>% 
  bind_cols(med_seriald.stand.alt) %>% 
  relocate(any_of(c("serial.med_d.alt","var2","label","Unstandardized.beta", "Standardized.beta"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-var2, -label) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8),j=1, value = as_paragraph (c("a - Group-->PC", "h - PC-->PA", "b - PA-->PU","e' - Group-->PU",
                                                            "g - PA-->MH","i' - Group-->MH","Group-->PC-->PA-->PU",
                                                            "Group-->PC-->PA-->MH"))) %>% 
  autofit()


# fit measures

fit.sd.alt=sum.fit_serial.med_d.alt$FIT %>% 
  tibble()
fit.sd.alt=fit.sd.alt[c(3,4,5,9,17),]
fit.sd.alt=fit.sd.alt %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()

#imputed----

med.seriald.imp.alt=sum.fit_serial.med_d.imp.alt %>% 
  filter(label%in% c("a","h","b","e","g","i","a_h_b",
                     "a_h_g")) %>% 
  select(- op, - exo, - df, - std.lv, - std.nox,- label, - lhs) %>% 
  relocate(any_of(c("rhs","ihs","est","std.all"))) %>% 
  rename(serial.med_d.imp.alt=rhs, Unstandardized.beta=est, Standardized.beta=std.all , SE=se)

med.seriald.imp.alt=med.seriald.imp.alt %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8),j=1, value = as_paragraph (c("a - Group-->PC", "h - PC-->PA", "b - PA-->PU","e' - Group-->PU",
                                                            "g - PA-->MH","i' - Group-->MH","Group-->PC-->PA-->PU",
                                                            "Group-->PC-->PA-->MH"))) %>% 
  autofit()

# fit measures
fit.sd.imp.alt=fitMeasures(fit_serial.med_d.imp.alt) %>% 
  tibble()

fit.sd.imp.alt=fit.sd.imp.alt[c(1,2,3,16,25),]

fit.sd.imp.alt=fit.sd.imp.alt %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()



# Model 4b (serial.med_e.alt)----
med_seriale.stand.alt= med_serial.e.stand.alt %>% 
  filter(label%in% c("a","h","b","e","h","g","i","a_h_b",
                     "a_h_g")) %>% 
  select(est.std) %>% 
  rename(Standardized.beta=est.std)

med.serial.e.alt=med_serial.e.alt %>% 
  filter(label%in% c("a","h","b","e","h","g","i","a_h_b",
                     "a_h_g")) %>% 
  select(-op) %>% 
  relocate(any_of(c("rhs","ihs"))) %>% 
  rename(serial.med_e.alt=rhs,var2=lhs, Unstandardized.beta=est, SE=se)

med.seriale.alt=med.serial.e.alt %>% 
  bind_cols(med_seriale.stand.alt) %>% 
  relocate(any_of(c("serial.med_e.alt","var2","label","Unstandardized.beta", "Standardized.beta"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-var2, -label) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8),j=1, value = as_paragraph (c("a - Group-->PC","h - PC-->NA", "b - NA-->PU","e' - Group-->PU",
                                                            "g - NA-->MH","i' - Group-->MH","Group-->PC-->NA-->PU",
                                                            "Group-->PC-->NA-->MH"))) %>% 
  autofit()


# fit measures

fit.se.alt=sum.fit_serial.med_e.alt$FIT %>% 
  tibble()
fit.se.alt=fit.se.alt[c(3,4,5,9,17),]
fit.se.alt=fit.se.alt %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()

#imputed----

med.seriale.imp.alt=sum.fit_serial.med_e.imp.alt %>% 
  filter(label%in% c("a","h","b","e","h","g","i","a_h_b",
                     "a_h_g")) %>% 
  select(- op, - exo, - df, - std.lv, - std.nox,- label, - lhs) %>% 
  relocate(any_of(c("rhs","ihs","est","std.all"))) %>% 
  rename(serial.med_e.imp.alt=rhs, Unstandardized.beta=est, Standardized.beta=std.all , SE=se)

med.seriale.imp.alt=med.seriale.imp.alt %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable() %>% 
  compose(i=c(1,2,3,4,5,6,7,8),j=1, value = as_paragraph (c("a - Group-->PC","h - PC-->NA", "b - NA-->PU","e' - Group-->PU",
                                                            "g - NA-->MH","i' - Group-->MH","Group-->PC-->NA-->PU",
                                                            "Group-->PC-->NA-->MH"))) %>% 
  autofit()

# fit measures
fit.se.imp.alt=fitMeasures(fit_serial.med_e.imp.alt) %>% 
  tibble()

fit.se.imp.alt=fit.se.imp.alt[c(1,2,3,16,25),]

fit.se.imp.alt=fit.se.imp.alt %>% 
  add_column(fit.index= c("chisq","df","pvalue","cfi", "rmsea")) %>% 
  relocate(any_of(c("fit.index"))) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  flextable()

packages.power.med= c("lavaan", "bmem")


invisible(lapply(packages.power.med, library, character.only = TRUE)) 



#power parallel model PPI-PA----

power_PC_PPI= '
pelvic.pain_t2 ~ e*group_intervention+start(-0.09)*group_intervention+b*PCS_total_t2+start(0.13)*PCS_total_t2
PCS_total_t2 ~ a*group_intervention+start(-8.46)*group_intervention
PCS_total_t2 ~~ start(80.6)*PCS_total_t2
pelvic.pain_t2 ~~ start(4.3)*pelvic.pain_t2
'


power_PC_PPI.med = 'ab:=a*b'



power_PC_PPI_PA_MH= '
pelvic.pain_t2 ~ e*group_intervention+start(-0.19)*group_intervention+b*PCS_total_t2+start(0.13)*PCS_total_t2+d*affect_positive_t2+start(-0.02)*affect_positive_t2
PCS_total_t2 ~ a*group_intervention+start(-8.99)*group_intervention
affect_positive_t2 ~ c*group_intervention+start(3.12)*group_intervention
sf_36_mental.health_t2 ~ h*group_intervention+start(1.64)*group_intervention+f*PCS_total_t2+start(-0.41)*PCS_total_t2+g*affect_positive_t2+start(2.83)*affect_positive_t2

PCS_total_t2 ~~ start(80.6)*PCS_total_t2
pelvic.pain_t2 ~~ start(4.3)*pelvic.pain_t2
affect_positive_t2 ~~ start(16.02)*affect_positive_t2
sf_36_mental.health_t2 ~~ start(195)*sf_36_mental.health_t2
'

power_PC_PPI_PA_MH.med = 'ab:=a*b
                          cd:=c*d
                          af:=a*f
                          cg:=c*g'




set.seed(2021)

power.parallel.ppi.pa=power.boot(model=power_PC_PPI_PA_MH, indirect=power_PC_PPI_PA_MH.med, nobs=46, nrep=100, nboot=100, parallel="multicore",
                                 skewness= c(0,-0.44, -0.13, 0.73, 0.08), kurtosis= c(0,2.32, 1.90, 3.20, 1.96), 
                                 ovnames=c("group_intervention","PCS_total_t2","pelvic.pain_t2", "affect_positive_t2", "sf_36_mental.health_t2"))



summary(power.parallel.ppi.pa)


#Increase N power simulation

set.seed(2021)

power.parallel.ppi.pa.n100=power.boot(model=power_PC_PPI_PA_MH, indirect=power_PC_PPI_PA_MH.med, nobs=100, nrep=100, nboot=100, parallel="multicore",
                                      skewness= c(0,-0.44, -0.13, 0.73, 0.08), kurtosis= c(0,2.32, 1.90, 3.20, 1.96), 
                                      ovnames=c("group_intervention","PCS_total_t2","pelvic.pain_t2", "affect_positive_t2", "sf_36_mental.health_t2"))



summary(power.parallel.ppi.pa.n100)

set.seed(2021)

power.parallel.ppi.pa.n150=power.boot(model=power_PC_PPI_PA_MH, indirect=power_PC_PPI_PA_MH.med, nobs=150, nrep=100, nboot=100, parallel="multicore",
                                      skewness= c(0,-0.44, -0.13, 0.73, 0.08), kurtosis= c(0,2.32, 1.90, 3.20, 1.96), 
                                      ovnames=c("group_intervention","PCS_total_t2","pelvic.pain_t2", "affect_positive_t2", "sf_36_mental.health_t2"))



summary(power.parallel.ppi.pa.n150)




#power parallel model PPI-NA----

power_PC_PPI_NA_MH= '
pelvic.pain_t2 ~ e*group_intervention+start(-0.51)*group_intervention+b*PCS_total_t2+start(0.14)*PCS_total_t2+d*affect_Negative_t2+start(-0.14)*affect_Negative_t2
PCS_total_t2 ~ a*group_intervention+start(-8.99)*group_intervention
affect_Negative_t2 ~ c*group_intervention+start(-2.23)*group_intervention
sf_36_mental.health_t2 ~ h*group_intervention+start(1.25)*group_intervention+f*PCS_total_t2+start(-0.80)*PCS_total_t2+g*affect_Negative_t2+start(-2.52)*affect_Negative_t2

PCS_total_t2 ~~ start(80.6)*PCS_total_t2
pelvic.pain_t2 ~~ start(4.3)*pelvic.pain_t2
affect_Negative_t2 ~~ start(11.50)*affect_Negative_t2
sf_36_mental.health_t2 ~~ start(232)*sf_36_mental.health_t2
'

power_PC_PPI_NA_MH.med = 'ab:=a*b
                          cd:=c*d
                          af:=a*f
                          cg:=c*g'



set.seed(2021)

power.parallel.ppi.na=power.boot(model=power_PC_PPI_PA_MH, indirect=power_PC_PPI_NA_MH.med, nobs=46, nrep=100, nboot=100, parallel="multicore",
                                 skewness= c(0,-0.44, -0.13, 0.48, 0.084), kurtosis= c(0,2.32, 1.90, 2.67, 1.96), 
                                 ovnames=c("group_intervention","PCS_total_t2","pelvic.pain_t2", "affect_Negative_t2", "sf_36_mental.health_t2"))



summary(power.parallel.ppi.na)


#Increase N power simulation


set.seed(2021)

power.parallel.ppi.na.n100=power.boot(model=power_PC_PPI_PA_MH, indirect=power_PC_PPI_NA_MH.med, nobs=100, nrep=100, nboot=100, parallel="multicore",
                                      skewness= c(0,-0.44, -0.13, 0.48, 0.084), kurtosis= c(0,2.32, 1.90, 2.67, 1.96), 
                                      ovnames=c("group_intervention","PCS_total_t2","pelvic.pain_t2", "affect_Negative_t2", "sf_36_mental.health_t2"))



summary(power.parallel.ppi.na.n100)


set.seed(2021)

power.parallel.ppi.na.n150=power.boot(model=power_PC_PPI_PA_MH, indirect=power_PC_PPI_NA_MH.med, nobs=150, nrep=100, nboot=100, parallel="multicore",
                                      skewness= c(0,-0.44, -0.13, 0.48, 0.084), kurtosis= c(0,2.32, 1.90, 2.67, 1.96), 
                                      ovnames=c("group_intervention","PCS_total_t2","pelvic.pain_t2", "affect_Negative_t2", "sf_36_mental.health_t2"))



summary(power.parallel.ppi.na.n150)


# power parallel PA-PU----


power_PC_PU_PA_MH= '
pain.unpleasantness_t2 ~ e*group_intervention+start(-0.84)*group_intervention+b*PCS_total_t2+start(0.10)*PCS_total_t2+d*affect_positive_t2+start(-0.12)*affect_positive_t2
PCS_total_t2 ~ a*group_intervention+start(-9.23)*group_intervention
affect_positive_t2 ~ c*group_intervention+start(3.04)*group_intervention
sf_36_mental.health_t2 ~ h*group_intervention+start(2.18)*group_intervention+f*PCS_total_t2+start(-0.40)*PCS_total_t2+g*affect_positive_t2+start(2.83)*affect_positive_t2

PCS_total_t2 ~~ start(80.6)*PCS_total_t2
pain.unpleasantness_t2 ~~ start(3.83)*pelvic.pain_t2
affect_positive_t2 ~~ start(15.7)*affect_positive_t2
sf_36_mental.health_t2 ~~ start(191)*sf_36_mental.health_t2
'

power_PC_PU_PA_MH.med = 'ab:=a*b
                          cd:=c*d
                          af:=a*f
                          cg:=c*g'




set.seed(2021)

power.parallel.pu.pa=power.boot(model=power_PC_PU_PA_MH, indirect=power_PC_PU_PA_MH.med, nobs=46, nrep=100, nboot=100, parallel="multicore",
                                skewness= c(0,-0.44, -0.03, 0.73, 0.08), kurtosis= c(0,2.32, 1.86, 3.20, 1.96), 
                                ovnames=c("group_intervention","PCS_total_t2","pain.unpleasantness_t2", "affect_positive_t2", "sf_36_mental.health_t2"))



summary(power.parallel.pu.pa)


#Increase N power simulation

set.seed(2021)

power.parallel.pu.pa.n100=power.boot(model=power_PC_PU_PA_MH, indirect=power_PC_PU_PA_MH.med, nobs=100, nrep=100, nboot=100, parallel="multicore",
                                     skewness= c(0,-0.44, -0.03, 0.73, 0.08), kurtosis= c(0,2.32, 1.86, 3.20, 1.96), 
                                     ovnames=c("group_intervention","PCS_total_t2","pain.unpleasantness_t2", "affect_positive_t2", "sf_36_mental.health_t2"))



summary(power.parallel.pu.pa.n100)


set.seed(2021)

power.parallel.pu.pa.n150=power.boot(model=power_PC_PU_PA_MH, indirect=power_PC_PU_PA_MH.med, nobs=150, nrep=100, nboot=100, parallel="multicore",
                                     skewness= c(0,-0.44, -0.03, 0.73, 0.08), kurtosis= c(0,2.32, 1.86, 3.20, 1.96), 
                                     ovnames=c("group_intervention","PCS_total_t2","pain.unpleasantness_t2", "affect_positive_t2", "sf_36_mental.health_t2"))



summary(power.parallel.pu.pa.n150)


# power parallel NA-PU----


power_PC_PU_NA_MH= '
pain.unpleasantness_t2 ~ e*group_intervention+start(-0.95)*group_intervention+b*PCS_total_t2+start(0.13)*PCS_total_t2+d*affect_Negative_t2+start(0.02)*affect_Negative_t2
PCS_total_t2 ~ a*group_intervention+start(-9.23)*group_intervention
affect_Negative_t2 ~ c*group_intervention+start(-2.09)*group_intervention
sf_36_mental.health_t2 ~ h*group_intervention+start(2.47)*group_intervention+f*PCS_total_t2+start(-0.83)*PCS_total_t2+g*affect_Negative_t2+start(-2.39)*affect_Negative_t2

PCS_total_t2 ~~ start(80.6)*PCS_total_t2
pain.unpleasantness_t2 ~~ start(3.83)*pelvic.pain_t2
affect_Negative_t2 ~~ start(11.4)*affect_Negative_t2
sf_36_mental.health_t2 ~~ start(234)*sf_36_mental.health_t2
'

power_PC_PU_NA_MH.med = 'ab:=a*b
                          cd:=c*d
                          af:=a*f
                          cg:=c*g'




set.seed(2021)

power.parallel.pu.na=power.boot(model=power_PC_PU_NA_MH, indirect=power_PC_PU_NA_MH.med, nobs=46, nrep=100, nboot=100, parallel="multicore",
                                skewness= c(0,-0.44, -0.03, 0.48, 0.08), kurtosis= c(0,2.32, 1.86, 2.67, 1.96), 
                                ovnames=c("group_intervention","PCS_total_t2","pain.unpleasantness_t2", "affect_Negative_t2", "sf_36_mental.health_t2"))



summary(power.parallel.pu.na)


#Increase N power simulation

set.seed(2021)

power.parallel.pu.na.n100=power.boot(model=power_PC_PU_NA_MH, indirect=power_PC_PU_NA_MH.med, nobs=100, nrep=100, nboot=100, parallel="multicore",
                                     skewness= c(0,-0.44, -0.03, 0.48, 0.08), kurtosis= c(0,2.32, 1.86, 2.67, 1.96), 
                                     ovnames=c("group_intervention","PCS_total_t2","pain.unpleasantness_t2", "affect_Negative_t2", "sf_36_mental.health_t2"))



summary(power.parallel.pu.na.n100)


set.seed(2021)

power.parallel.pu.na.n150=power.boot(model=power_PC_PU_NA_MH, indirect=power_PC_PU_NA_MH.med, nobs=150, nrep=100, nboot=100, parallel="multicore",
                                     skewness= c(0,-0.44, -0.03, 0.48, 0.08), kurtosis= c(0,2.32, 1.86, 2.67, 1.96), 
                                     ovnames=c("group_intervention","PCS_total_t2","pain.unpleasantness_t2", "affect_Negative_t2", "sf_36_mental.health_t2"))



summary(power.parallel.pu.na.n150)


#power serial model PPI-PA----



power_PC_PPI_PA_MH2= '
pelvic.pain_t2 ~ e*group_intervention+start(-0.09)*group_intervention+b*PCS_total_t2+start(0.13)*PCS_total_t2
PCS_total_t2 ~ a*group_intervention+start(-8.46)*group_intervention
affect_positive_t2 ~ h*PCS_total_t2+start(-0.22)*PCS_total_t2
sf_36_mental.health_t2 ~ i*group_intervention+start(-0.48)*group_intervention+j*pelvic.pain_t2+start(-3.36)*pelvic.pain_t2+g*affect_positive_t2+start(3.13)*affect_positive_t2

PCS_total_t2 ~~ start(80.6)*PCS_total_t2
pelvic.pain_t2 ~~ start(4.3)*pelvic.pain_t2
affect_positive_t2 ~~ start(14.19)*affect_positive_t2
sf_36_mental.health_t2 ~~ start(189)*sf_36_mental.health_t2
'

power_PC_PPI_PA_MH.med2 = 'ab:=a*b
                          ahg:=a*h*g
                          abj:=a*b*j'




set.seed(2021)

power.serial.ppi.pa=power.boot(model=power_PC_PPI_PA_MH2, indirect=power_PC_PPI_PA_MH.med2, nobs=46, nrep=100, nboot=100, parallel="multicore",
                               skewness= c(0,-0.44, -0.13, 0.73, 0.08), kurtosis= c(0,2.32, 1.90, 3.20, 1.96), 
                               ovnames=c("group_intervention","PCS_total_t2","pelvic.pain_t2", "affect_positive_t2", "sf_36_mental.health_t2"))



summary(power.serial.ppi.pa)


#power serial model PPI-NA----



power_PC_PPI_NA_MH2= '
pelvic.pain_t2 ~ e*group_intervention+start(-0.09)*group_intervention+b*PCS_total_t2+start(0.13)*PCS_total_t2
PCS_total_t2 ~ a*group_intervention+start(-8.46)*group_intervention
affect_Negative_t2 ~ h*PCS_total_t2+start(0.09)*PCS_total_t2
sf_36_mental.health_t2 ~ i*group_intervention+start(0.73)*group_intervention+j*pelvic.pain_t2+start(-4.76)*pelvic.pain_t2+g*affect_Negative_t2+start(-3.10)*affect_Negative_t2

PCS_total_t2 ~~ start(80.6)*PCS_total_t2
pelvic.pain_t2 ~~ start(4.3)*pelvic.pain_t2
affect_Negative_t2 ~~ start(10.41)*affect_positive_t2
sf_36_mental.health_t2 ~~ start(247)*sf_36_mental.health_t2
'

power_PC_PPI_NA_MH.med2 = 'ab:=a*b
                          ahg:=a*h*g
                          abj:=a*b*j'




set.seed(2021)

power.serial.ppi.na=power.boot(model=power_PC_PPI_NA_MH2, indirect=power_PC_PPI_NA_MH.med2, nobs=46, nrep=100, nboot=100, parallel="multicore",
                               skewness= c(0,-0.44, -0.13, 0.48, 0.08), kurtosis= c(0,2.32, 1.90, 2.67, 1.96), 
                               ovnames=c("group_intervention","PCS_total_t2","pelvic.pain_t2", "affect_Negative_t2", "sf_36_mental.health_t2"))



summary(power.serial.ppi.na)



#Increase N power simulation

set.seed(2021)

power.serial.ppi.na.n100=power.boot(model=power_PC_PPI_NA_MH2, indirect=power_PC_PPI_NA_MH.med2, nobs=100, nrep=100, nboot=100, parallel="multicore",
                                    skewness= c(0,-0.44, -0.13, 0.48, 0.08), kurtosis= c(0,2.32, 1.90, 2.67, 1.96), 
                                    ovnames=c("group_intervention","PCS_total_t2","pelvic.pain_t2", "affect_Negative_t2", "sf_36_mental.health_t2"))



summary(power.serial.ppi.na.n100)


set.seed(2021)

power.serial.ppi.na.n150=power.boot(model=power_PC_PPI_NA_MH2, indirect=power_PC_PPI_NA_MH.med2, nobs=150, nrep=100, nboot=100, parallel="multicore",
                                    skewness= c(0,-0.44, -0.13, 0.48, 0.08), kurtosis= c(0,2.32, 1.90, 2.67, 1.96), 
                                    ovnames=c("group_intervention","PCS_total_t2","pelvic.pain_t2", "affect_Negative_t2", "sf_36_mental.health_t2"))



summary(power.serial.ppi.na.n150)


#power serial model PU-PA----



power_PC_PU_PA_MH2= '
pain.unpleasantness_t2 ~ e*group_intervention+start(-1.01)*group_intervention+b*PCS_total_t2+start(0.13)*PCS_total_t2
PCS_total_t2 ~ a*group_intervention+start(-9.23)*group_intervention
affect_positive_t2 ~ h*PCS_total_t2+start(-0.22)*PCS_total_t2
sf_36_mental.health_t2 ~ i*group_intervention+start(-0.90)*group_intervention+j*pain.unpleasantness_t2+start(-3.95)*pain.unpleasantness_t2+g*affect_positive_t2+start(2.33)*affect_positive_t2

PCS_total_t2 ~~ start(80.6)*PCS_total_t2
pain.unpleasantness_t2 ~~ start(4)*pain.unpleasantness_t2
affect_positive_t2 ~~ start(12.81)*affect_positive_t2
sf_36_mental.health_t2 ~~ start(119)*sf_36_mental.health_t2
'

power_PC_PU_PA_MH.med2 = 'ab:=a*b
                          ahg:=a*h*g
                          abj:=a*b*j'




set.seed(2021)

power.serial.pu.pa=power.boot(model=power_PC_PU_PA_MH2, indirect=power_PC_PU_PA_MH.med2, nobs=46, nrep=100, nboot=100, parallel="multicore",
                              skewness= c(0,-0.44, -0.03, 0.73, 0.08), kurtosis= c(0,2.32, 1.86, 3.20, 1.96), 
                              ovnames=c("group_intervention","PCS_total_t2","pain.unpleasantness_t2", "affect_positive_t2", "sf_36_mental.health_t2"))



summary(power.serial.pu.pa)



#power serial model PU-NA----



power_PC_PU_NA_MH2= '
pain.unpleasantness_t2 ~ e*group_intervention+start(-1.01)*group_intervention+b*PCS_total_t2+start(0.13)*PCS_total_t2
PCS_total_t2 ~ a*group_intervention+start(-9.23)*group_intervention
affect_Negative_t2 ~ h*PCS_total_t2+start(0.09)*PCS_total_t2
sf_36_mental.health_t2 ~ i*group_intervention+start(-4.63)*group_intervention+j*pain.unpleasantness_t2+start(-5.82)*pain.unpleasantness_t2+g*affect_Negative_t2+start(-2.09)*affect_Negative_t2

PCS_total_t2 ~~ start(80.6)*PCS_total_t2
pain.unpleasantness_t2 ~~ start(4)*pain.unpleasantness_t2
affect_Negative_t2 ~~ start(10.54)*affect_Negative_t2
sf_36_mental.health_t2 ~~ start(198)*sf_36_mental.health_t2
'

power_PC_PU_NA_MH.med2 = 'ab:=a*b
                          ahg:=a*h*g
                          abj:=a*b*j'




set.seed(2021)

power.serial.pu.na=power.boot(model=power_PC_PU_NA_MH2, indirect=power_PC_PU_NA_MH.med2, nobs=46, nrep=100, nboot=100, parallel="multicore",
                              skewness= c(0,-0.44, -0.03, 0.48, 0.08), kurtosis= c(0,2.32, 1.86, 2.67, 1.96), 
                              ovnames=c("group_intervention","PCS_total_t2","pain.unpleasantness_t2", "affect_Negative_t2", "sf_36_mental.health_t2"))



summary(power.serial.pu.na)


#Increase N power simulation

set.seed(2021)

power.serial.pu.na.n100=power.boot(model=power_PC_PU_NA_MH2, indirect=power_PC_PU_NA_MH.med2, nobs=100, nrep=100, nboot=100, parallel="multicore",
                                   skewness= c(0,-0.44, -0.03, 0.48, 0.08), kurtosis= c(0,2.32, 1.86, 2.67, 1.96), 
                                   ovnames=c("group_intervention","PCS_total_t2","pain.unpleasantness_t2", "affect_Negative_t2", "sf_36_mental.health_t2"))



summary(power.serial.pu.na.n100)


#power serial model 2 PPI-PA----



power_PC_PPI_PA_MH.ser2= '
pelvic.pain_t2 ~ e*group_intervention+start(-0.09)*group_intervention+b*affect_positive_t2+start(-0.13)*affect_positive_t2
PCS_total_t2 ~ a*group_intervention+start(-8.46)*group_intervention
affect_positive_t2 ~ h*PCS_total_t2+start(-0.22)*PCS_total_t2
sf_36_mental.health_t2 ~ i*group_intervention+start(-0.02)*group_intervention+g*affect_positive_t2+start(3.67)*affect_positive_t2

PCS_total_t2 ~~ start(80.6)*PCS_total_t2
pelvic.pain_t2 ~~ start(4.3)*pelvic.pain_t2
affect_positive_t2 ~~ start(14.19)*affect_positive_t2
sf_36_mental.health_t2 ~~ start(189)*sf_36_mental.health_t2
'

power_PC_PPI_PA_MH.med.ser2 = '
                          ahg:=a*h*g
                          ahb:=a*h*b'


set.seed(2021)

power.serial.ppi.pa.ser2=power.boot(model=power_PC_PPI_PA_MH.ser2, indirect=power_PC_PPI_PA_MH.med.ser2, nobs=46, nrep=100, nboot=100, parallel="multicore",
                                    skewness= c(0,-0.44, -0.13, 0.73, 0.08), kurtosis= c(0,2.32, 1.90, 3.20, 1.96), 
                                    ovnames=c("group_intervention","PCS_total_t2","pelvic.pain_t2", "affect_positive_t2", "sf_36_mental.health_t2"))



summary(power.serial.ppi.pa.ser2)

#Increase N power simulation

set.seed(2021)


power.serial.ppi.pa.ser2.n100=power.boot(model=power_PC_PPI_PA_MH.ser2, indirect=power_PC_PPI_PA_MH.med.ser2, nobs=100, nrep=100, nboot=100, parallel="multicore",
                                         skewness= c(0,-0.44, -0.13, 0.73, 0.08), kurtosis= c(0,2.32, 1.90, 3.20, 1.96), 
                                         ovnames=c("group_intervention","PCS_total_t2","pelvic.pain_t2", "affect_positive_t2", "sf_36_mental.health_t2"))



summary(power.serial.ppi.pa.ser2.n100)


set.seed(2021)


power.serial.ppi.pa.ser2.n150=power.boot(model=power_PC_PPI_PA_MH.ser2, indirect=power_PC_PPI_PA_MH.med.ser2, nobs=150, nrep=100, nboot=100, parallel="multicore",
                                         skewness= c(0,-0.44, -0.13, 0.73, 0.08), kurtosis= c(0,2.32, 1.90, 3.20, 1.96), 
                                         ovnames=c("group_intervention","PCS_total_t2","pelvic.pain_t2", "affect_positive_t2", "sf_36_mental.health_t2"))



summary(power.serial.ppi.pa.ser2.n150)

#power serial model 2 PPI-NA----



power_PC_PPI_NA_MH.ser2= '
pelvic.pain_t2 ~ e*group_intervention+start(-1.36)*group_intervention+b*affect_Negative_t2+start(-0.03)*affect_Negative_t2
PCS_total_t2 ~ a*group_intervention+start(-8.46)*group_intervention
affect_Negative_t2 ~ h*PCS_total_t2+start(0.09)*PCS_total_t2
sf_36_mental.health_t2 ~ i*group_intervention+start(5.792)*group_intervention+g*affect_Negative_t2+start(-3.09)*affect_Negative_t2

PCS_total_t2 ~~ start(80.6)*PCS_total_t2
pelvic.pain_t2 ~~ start(5.9)*pelvic.pain_t2
affect_Negative_t2 ~~ start(10.41)*affect_positive_t2
sf_36_mental.health_t2 ~~ start(391)*sf_36_mental.health_t2
'

power_PC_PPI_NA_MH.med.ser2 = '
                          ahg:=a*h*g
                          ahb:=a*h*b'


set.seed(2021)

power.serial.ppi.Na.ser2=power.boot(model=power_PC_PPI_NA_MH.ser2, indirect=power_PC_PPI_NA_MH.med.ser2, nobs=46, nrep=100, nboot=100, parallel="multicore",
                                    skewness= c(0,-0.44, -0.13, 0.48, 0.08), kurtosis= c(0,2.32, 1.90, 2.67, 1.96), 
                                    ovnames=c("group_intervention","PCS_total_t2","pelvic.pain_t2", "affect_Negative_t2", "sf_36_mental.health_t2"))



summary(power.serial.ppi.Na.ser2)


#Increase N power simulation

set.seed(2021)


power.serial.ppi.Na.ser2.n100=power.boot(model=power_PC_PPI_NA_MH.ser2, indirect=power_PC_PPI_NA_MH.med.ser2, nobs=100, nrep=100, nboot=100, parallel="multicore",
                                         skewness= c(0,-0.44, -0.13, 0.48, 0.08), kurtosis= c(0,2.32, 1.90, 2.67, 1.96), 
                                         ovnames=c("group_intervention","PCS_total_t2","pelvic.pain_t2", "affect_Negative_t2", "sf_36_mental.health_t2"))



summary(power.serial.ppi.Na.ser2.n100)


set.seed(2021)


power.serial.ppi.Na.ser2.n150=power.boot(model=power_PC_PPI_NA_MH.ser2, indirect=power_PC_PPI_NA_MH.med.ser2, nobs=150, nrep=100, nboot=100, parallel="multicore",
                                         skewness= c(0,-0.44, -0.13, 0.48, 0.08), kurtosis= c(0,2.32, 1.90, 2.67, 1.96), 
                                         ovnames=c("group_intervention","PCS_total_t2","pelvic.pain_t2", "affect_Negative_t2", "sf_36_mental.health_t2"))



summary(power.serial.ppi.Na.ser2.n150)


#power serial model 2 PU-PA----



power_PC_PU_PA_MH.ser2= '
pain.unpleasantness_t2 ~ e*group_intervention+start(-1.42)*group_intervention+b*affect_positive_t2+start(-0.25)*affect_positive_t2
PCS_total_t2 ~ a*group_intervention+start(-9.23)*group_intervention
affect_positive_t2 ~ h*PCS_total_t2+start(-0.22)*PCS_total_t2
sf_36_mental.health_t2 ~ i*group_intervention+start(0.42)*group_intervention+g*affect_positive_t2+start(3.57)*affect_positive_t2

PCS_total_t2 ~~ start(80.6)*PCS_total_t2
pain.unpleasantness_t2 ~~ start(4.3)*pain.unpleasantness_t2
affect_positive_t2 ~~ start(12.81)*affect_positive_t2
sf_36_mental.health_t2 ~~ start(279)*sf_36_mental.health_t2
'

power_PC_PU_PA_MH.med.ser2 = '
                          ahg:=a*h*g
                          ahb:=a*h*b'

set.seed(2021)

power.serial.pu.pa.ser2=power.boot(model=power_PC_PU_PA_MH.ser2, indirect=power_PC_PU_PA_MH.med.ser2, nobs=46, nrep=100, nboot=100, parallel="multicore",
                                   skewness= c(0,-0.44, -0.03, 0.73, 0.08), kurtosis= c(0,2.32, 1.86, 3.20, 1.96), 
                                   ovnames=c("group_intervention","PCS_total_t2","pain.unpleasantness_t2", "affect_positive_t2", "sf_36_mental.health_t2"))



summary(power.serial.pu.pa.ser2)

#power serial model 2 PU-NA----



power_PC_PU_NA_MH.ser2= '
pain.unpleasantness_t2 ~ e*group_intervention+start(-1.74)*group_intervention+b*affect_Negative_t2+start(0.14)*affect_affect_Negative_t2
PCS_total_t2 ~ a*group_intervention+start(-9.23)*group_intervention
affect_affect_Negative_t2 ~ h*PCS_total_t2+start(0.09)*PCS_total_t2
sf_36_mental.health_t2 ~ i*group_intervention+start(5.11)*group_intervention+g*affect_affect_Negative_t2+start(-3.0)*affect_positive_t2

PCS_total_t2 ~~ start(80.6)*PCS_total_t2
pain.unpleasantness_t2 ~~ start(5)*pain.unpleasantness_t2
affect_affect_Negative_t2 ~~ start(10.54)*affect_affect_Negative_t2
sf_36_mental.health_t2 ~~ start(385)*sf_36_mental.health_t2
'

power_PC_PU_NA_MH.med.ser2 = '
                          ahg:=a*h*g
                          ahb:=a*h*b'

set.seed(2021)

power.serial.pu.na.ser2=power.boot(model=power_PC_PU_NA_MH.ser2, indirect=power_PC_PU_NA_MH.med.ser2, nobs=46, nrep=100, nboot=100, parallel="multicore",
                                   skewness= c(0,-0.44, -0.03, 0.48, 0.08), kurtosis= c(0,2.32, 1.86, 2.67, 1.96), 
                                   ovnames=c("group_intervention","PCS_total_t2","pain.unpleasantness_t2", "affect_Negative_t2", "sf_36_mental.health_t2"))



summary(power.serial.pu.na.ser2)

#Increasing N power simulation


set.seed(2021)

power.serial.pu.na.ser2.n100=power.boot(model=power_PC_PU_NA_MH.ser2, indirect=power_PC_PU_NA_MH.med.ser2, nobs=100, nrep=100, nboot=100, parallel="multicore",
                                        skewness= c(0,-0.44, -0.03, 0.48, 0.08), kurtosis= c(0,2.32, 1.86, 2.67, 1.96), 
                                        ovnames=c("group_intervention","PCS_total_t2","pain.unpleasantness_t2", "affect_Negative_t2", "sf_36_mental.health_t2"))



summary(power.serial.pu.na.ser2.n100)


set.seed(2021)

power.serial.pu.na.ser2.n150=power.boot(model=power_PC_PU_NA_MH.ser2, indirect=power_PC_PU_NA_MH.med.ser2, nobs=150, nrep=100, nboot=100, parallel="multicore",
                                        skewness= c(0,-0.44, -0.03, 0.48, 0.08), kurtosis= c(0,2.32, 1.86, 2.67, 1.96), 
                                        ovnames=c("group_intervention","PCS_total_t2","pain.unpleasantness_t2", "affect_Negative_t2", "sf_36_mental.health_t2"))



summary(power.serial.pu.na.ser2.n150)
