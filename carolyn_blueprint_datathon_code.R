#blueprint datathon @stanford 
##author: Carolyn Oliver
###date: April 13, 2019

##set up ----
  #libraries ----
  library(tidyverse)
  library(readxl)
  #load data----
  stanford_blueprint_datathon_2019_data <- as.tibble(read.csv("~/Downloads/stanford_blueprint_datathon_2019_data.csv"))
  census_aggregated <- as.tibble(read.delim2("~/Desktop/census_aggregated.csv.txt"))
  data_full <- as.tibble(merge(stanford_blueprint_datathon_2019_data, census_aggregated, all.x=TRUE, all.y=TRUE, by=c("age","gender","state","income","education"))) %>% 
               mutate(chlamydia_diag = chlamydia*count) %>% 
    mutate(gential_warts_diag = gential_warts*count) %>% 
    mutate(gonorrhea_diag = gonorrhea*count) %>% 
    mutate(herpes_diag = herpes*count) %>% 
    mutate(hpv_diag = hpv*count) %>% 
    mutate(other_std_diag = other_std*count) %>% 
    mutate(parasitic_diag = parasitic*count) %>% 
    mutate(syphilis_diag = syphilis*count) %>% 
    mutate(trich_diag = trich*count) %>% 
    filter(age!="0-17 years old")
    
  

##explore data ----
  #correlation of STD-screening ----
  chlamydia_std_screen <- data_full %>% select(chlamydia, std_screen) %>% filter(is.na(chlamydia)==FALSE & is.na(std_screen)==FALSE)
  cor(chlamydia_std_screen)
  
  genital_warts_std_screen <- data_full %>% select(gential_warts, std_screen) %>% filter(is.na(gential_warts)==FALSE & is.na(std_screen)==FALSE)
  cor(genital_warts_std_screen)
  
  gonorrhea_std_screen <- data_full %>% select(gonorrhea, std_screen) %>% filter(is.na(gonorrhea)==FALSE & is.na(std_screen)==FALSE)
  cor(gonorrhea_std_screen)
  
  herpes_std_screen <- data_full %>% select(herpes, std_screen) %>% filter(is.na(herpes)==FALSE & is.na(std_screen)==FALSE)
  cor(herpes_std_screen)
  
  hpv_std_screen <- data_full %>% select(hpv, std_screen) %>% filter(is.na(hpv)==FALSE & is.na(std_screen)==FALSE)
  cor(hpv_std_screen)
  
  other_std_std_screen <- data_full %>% select(other_std, std_screen) %>% filter(is.na(other_std)==FALSE & is.na(std_screen)==FALSE)
  cor(other_std_std_screen)
  
  parasitic_std_screen <- data_full %>% select(parasitic, std_screen) %>% filter(is.na(parasitic)==FALSE & is.na(std_screen)==FALSE)
  cor(parasitic_std_screen)
  
  syphilis_std_screen <- data_full %>% select(syphilis, std_screen) %>% filter(is.na(syphilis)==FALSE & is.na(std_screen)==FALSE)
  cor(syphilis_std_screen)
  
  trich_std_screen <- data_full %>% select(trich, std_screen) %>% filter(is.na(trich)==FALSE & is.na(std_screen)==FALSE)
  cor(trich_std_screen)
  
  
  #linear regression model ----
  set.seed(100)  # setting seed to reproduce results of random sampling
  trainingRowIndex <- sample(1:nrow(data_full), 0.8*nrow(data_full))  # row indices for training data
  trainingData <- data_full[trainingRowIndex, ]  # model training data
  testData  <- data_full[-trainingRowIndex, ]
  
  first_model <- lm(chlamydia~age+gender+education+income+state, data=trainingData %>% filter(is.na(chlamydia)==FALSE))
  summary(first_model)
  pred_first_model <- predict(first_model, testData)
  summary(pred_first_model)
  pred_first_model
  
  actuals_preds <- data.frame(cbind(actuals=testData$chlamydia, predicteds=pred_first_model))  # make actuals_predicteds dataframe.
  correlation_accuracy <- cor(actuals_preds)
  head(actuals_preds)
  
  
  #BRFSS data

##estimate effects of HIV policies on HIV screening ----
    #step 1: load and clean BRFSS and policy datasets ----
    LLCP2017 <- as.tibble(read_excel("Downloads/LLCP2017.XLSX"))
    brfss <- LLCP2017 %>% select(`_STATE`, `_LLCPWT`,  `_AGE80`, SEX, EDUCA, `_INCOMG`, HIVRISK5, `_AIDTST3`, `_HCVU651`, HPVADVC2, `_SMOKER3`, `_BMI5`, LSATISFY) %>% 
      mutate(state=`_STATE`)
    
    HIV_policies1 <- as.tibble(read.delim("~/Desktop/policies1.csv.txt")) %>% 
      rowid_to_column("state") %>% 
      mutate(state_name=State) %>% 
      select(state, state_name, Perinatal.HIV.testing:STD.communicable.infectious.disease.criminalization)
    
    HIV_policies2 <- as.tibble(read.delim("~/Desktop/policies2.csv.txt")) %>% 
      rowid_to_column("state") %>%
      mutate(state_name=State) %>% 
      mutate(state = state_name)
      select(state, state_name, HIV.Testing.Anonymous.Testing.Available..:State.Guidelines.on.Health.Care.Workers.with.HIV.Practice.Restrictions.Based.on.HIV.Status.Exposure.Prone.Procedures..)
    
    #step 2: merge BRFSS and policy datasets ----
    brfss_policy1 <- as.tibble(merge(brfss, HIV_policies1, by="state")) %>% 
      mutate(state=state_name) %>% 
      mutate(age=`_AGE80`) %>% 
      mutate(weight= `_LLCPWT`) %>% 
      mutate(gender=SEX) %>% 
      mutate(education = EDUCA) %>% 
      mutate(income = `_INCOMG`) %>% 
      mutate(hiv_screen = ifelse(`_AIDTST3`==2, 1,
                                 ifelse(`_AIDTST3`==1, 0, NA))) %>%
      mutate(hiv_risk = ifelse(HIVRISK5==2, 1,
                               ifelse(HIVRISK5==1, 0, NA))) %>% 
      mutate(healthcare_coverage = `_HCVU651`) %>% 
      mutate(hpv_vacc = HPVADVC2) %>% 
      mutate(smoker = `_SMOKER3`) %>% 
      mutate(bmi = `_BMI5`) %>% 
      mutate(healthcare_satisfaction = LSATISFY) %>% 
      select(state, age, weight, gender, education, income, hiv_screen, hiv_risk, healthcare_coverage, 
             hpv_vacc, smoker, bmi, healthcare_satisfaction, 
             Perinatal.HIV.testing:STD.communicable.infectious.disease.criminalization)
    
    brfss_all_policy <- as.tibble(merge(brfss_policy1, HIV_policies2, by="state"))
    
    #step 3: see if correlation exists between HIV policies and HIV screening ----
  #states x mean HIV screen rate ----
  brfss_policy %>% group_by(state) %>% 
    filter(is.na(hiv_screen)==FALSE) %>% 
    summarize(mean=sum(hiv_screen*weight), weighted_mean = mean/sum(weight)) %>% 
    ggplot(mapping = aes(reorder(state, -weighted_mean), weighted_mean)) + geom_col() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
    labs(x = "State", y="Mean HIV Screen Rate")
  
  #states x mean HIV screen rate for high risk group (people who claim to be high risk) ----
  brfss_policy %>% group_by(state) %>% 
    filter(hiv_risk==1) %>% 
    filter(is.na(hiv_screen)==FALSE) %>% 
    summarize(mean=sum(hiv_screen*weight), weighted_mean = mean/sum(weight)) %>% 
    ggplot(mapping = aes(reorder(state, -weighted_mean), weighted_mean)) + geom_col() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
    labs(x = "State", y="Mean HIV Screen Rate Among Self-Report High Risk Pop")
  
  brfss_policy %>%  group_by(state) %>% 
    summarise(screen = sum(hiv_screen*weight, na.rm = TRUE)/sum(weight), risk = sum(hiv_risk*weight, na.rm = TRUE)/sum(weight)) %>% 
    ggplot(mapping = aes(screen, risk)) + geom_point() + 
    geom_smooth() + labs(title = "HIV Screening vs. Self-reported HIV Risk")+
    geom_text(aes(label=state),hjust=0, vjust=0)
  
  #correlation ----
  cor_hiv_risk_screen <- brfss_policy %>% select(hiv_risk, hiv_screen) %>% filter(is.na(hiv_screen)==FALSE & is.na(hiv_risk)==FALSE)
  cor(cor_hiv_risk_screen)
  
  #logistic regression for pre screening counseling as predictor of HIV screening rates ----
  hiv_screen_pre_test_counsel_unadjusted <- glm(hiv_screen~HIV.Testing.Pre.Test.Counseling..
                                               , data=brfss_all_policy, family="binomial")
  
  summary(hiv_screen_pre_test_counsel_unadjusted)
  exp(coef(hiv_screen_pre_test_counsel_unadjusted)) #get Odds Ratios for each covariate
  confint.default(hiv_screen_pre_test_counsel_unadjusted)
  
  hiv_screen_pre_test_counsel_adjusted <- glm(hiv_screen~HIV.Testing.Pre.Test.Counseling..+
                                               age+gender+income+education
                                             , data=brfss_all_policy, family="binomial")
  
  summary(hiv_screen_pre_test_counsel_adjusted)
  exp(coef(hiv_screen_pre_test_counsel_adjusted)) #get Odds Ratios for each covariate
  confint.default(hiv_screen_pre_test_counsel_adjusted)
  
  #logistic regression for post test counseling as predictor of HIV screening rates ----
  hiv_screen_post_test_counsel_unadjusted <- glm(hiv_screen~HIV.Testing.Post.Test.Counseling..Only.for.HIV.Diagnosis...,
                              data=brfss_all_policy, family="binomial")
  
  summary(hiv_screen_post_test_counsel_unadjusted)
  exp(coef(hiv_screen_post_test_counsel_unadjusted)) #get Odds Ratios for each covariate
  confint.default(hiv_screen_post_test_counsel_unadjusted)
  
  hiv_screen_post_test_counsel_adjusted <- glm(hiv_screen~HIV.Testing.Post.Test.Counseling..Only.for.HIV.Diagnosis...+
                                                age+gender+income+education
                                                , data=brfss_all_policy, family="binomial")
  
  summary(hiv_screen_post_test_counsel_adjusted)
  exp(coef(hiv_screen_post_test_counsel_adjusted)) #get Odds Ratios for each covariate
  confint.default(hiv_screen_post_test_counsel_adjusted)
  
  #logistic regression for anonymous testing as predictor of HIV screening rates ----
  hiv_screen_anonymous_test_unadjusted <- glm(hiv_screen~HIV.Testing.Anonymous.Testing.Available..,
                                                data=brfss_all_policy, family="binomial")
  
  summary(hiv_screen_anonymous_test_unadjusted)
  exp(coef(hiv_screen_anonymous_test_unadjusted)) #get Odds Ratios for each covariate
  confint.default(hiv_screen_anonymous_test_unadjusted)
  
  hiv_screen_anonymous_test_adjusted <- glm(hiv_screen~HIV.Testing.Anonymous.Testing.Available..+
                                         age+gender+income+education
                                       , data=brfss_all_policy, family="binomial")
  
  summary(hiv_screen_anonymous_test_adjusted)
  exp(coef(hiv_screen_anonymous_test_adjusted)) #get Odds Ratios for each covariate
  confint.default(hiv_screen_anonymous_test_adjusted)
  
  
  #logistic regression using all three policies as predictors of HIV screening rates ----
  hiv_screen_all_policy_adjusted <- glm(hiv_screen~HIV.Testing.Post.Test.Counseling..Only.for.HIV.Diagnosis...+
                                               HIV.Testing.Anonymous.Testing.Available..+
                                               HIV.Testing.Pre.Test.Counseling..+
                                               age+gender+income+education
                                             , data=brfss_all_policy, family = "binomial")
  
  summary(hiv_screen_all_policy_adjusted)
  exp(coef(hiv_screen_all_policy_adjusted)) #get Odds Ratios for each covariate
  confint.default(hiv_screen_all_policy_adjusted)
  
  
  
