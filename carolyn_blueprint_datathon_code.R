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
    
  

##Data Descriptives ----
  #Chlamydia ----
  #chlamydia ratio X age X gender
  data_full %>% select(age, gender, date, chlamydia) %>% 
    group_by(age, gender) %>% summarize(mean = mean(chlamydia, na.rm = TRUE)) %>% 
    ggplot(mapping = aes(age, mean, fill=gender)) + geom_col() + facet_wrap(.~gender, nrow=2)+ 
    labs(x = "Age", y="Mean Ratio of Chlamydia Diagnoses")
  
  #chlamydia diags X age X gender
    data_full %>% select(age, gender, date, chlamydia_diag) %>% 
      group_by(age, gender) %>% summarize(mean = mean(chlamydia_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(age, mean, fill=gender)) + geom_col() + facet_wrap(.~gender, nrow=2)+ 
    labs(x = "Age", y="# of Chlamydia Diagnoses")
    
    #chlamydia ratio x age x gender x time
    data_full %>% select(age, gender, date, chlamydia) %>% 
      group_by(age, date, gender) %>% summarize(mean = mean(chlamydia,  na.rm = TRUE)) %>% 
      ggplot(mapping = aes(date, mean, fill=gender)) + geom_col() + facet_wrap(gender~age, nrow=2)+ 
      labs(x = "Date", y="Mean Ratio of Chlamydia Diagnoses")
    
    #chlamydia diag x age x gender x time
    data_full %>% select(age, gender, date, chlamydia_diag) %>% 
      group_by(age, date, gender) %>% summarize(mean = mean(chlamydia_diag,  na.rm = TRUE)) %>% 
      ggplot(mapping = aes(date, mean, fill=gender)) + geom_col() + facet_wrap(gender~age, nrow=2)+ 
      labs(x = "Date", y="# of Chlamydia Diagnoses")
    
    #chlamydia ratio X state (male)
    data_full %>% select(state, gender, date, chlamydia) %>% 
      filter(gender=="Male") %>% 
      group_by(state, gender) %>% summarize(mean = mean(chlamydia, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="Mean Ratio of Chlamydia Diagnoses")
    
    #chlamydia diag X state (male)
    data_full %>% select(state, gender, date, chlamydia_diag) %>% 
      filter(gender=="Male") %>% 
      group_by(state, gender) %>% summarize(mean = mean(chlamydia_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="# of Chlamydia Diagnoses")
    
    #chlamydia ratio X state (female)
    data_full %>% select(state, gender, date, chlamydia) %>% 
      filter(gender=="Female") %>% 
      group_by(state, gender) %>% summarize(mean = mean(chlamydia, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="Mean Ratio of Chlamydia Diagnoses")
    
    #chlamydia diag X state (female)
    data_full %>% select(state, gender, date, chlamydia_diag) %>% 
      filter(gender=="Female") %>% 
      group_by(state, gender) %>% summarize(mean = mean(chlamydia_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="# of Chlamydia Diagnoses")
    
    #chlamydia ratio x income x gender
    data_full %>% select(age, income, gender, date, chlamydia, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(income, gender) %>% summarize(mean = mean(chlamydia, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(income, -mean), mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Less than $14,999", "$15,000 - $19,999", "$20,000 - $24,999", "$25,000 - $29,999", 
                                  "$30,000 - $34,999", "$35,000 - $39,999", "$40,000 - $44,999", "$45,000 - $49,999", 
                                  "$50,000 - $54,999", "$55,000 - $59,999", "$60,000 - $64,999", "$65,000 - $69,999",   
                                  "$70,000 - $74,999", "$75,000 - $79,999", "$80,000 - $84,999", "$85,000 - $89,999",   
                                  "$90,000 - $94,999", "$95,000 - $99,999", "$100,000 - $104,999", "$105,000 - $109,999", 
                                  "$110,000 - $114,999", "$115,000 - $119,999", "$120,000 - $124,999", "$125,000 - $129,999", 
                                  "$130,000 - $134,999", "$135,000 - $139,999", "$140,000 - $144,999", "$145,000 - $149,999",    
                                  "$150,000 - $159,999", "$160,000 - $169,999", "$170,000 - $179,999", "$180,000 - $189,999", 
                                  "$190,000 - $199,999", "$200,000 - $224,999", "$225,000 - $249,999", "$250,000 or more"))+ 
      labs(x = "Income", y="Mean Ratio of Chlamydia Diagnoses")
    
    #chlamydia diag x income x gender
    data_full %>% select(age, income, gender, date, chlamydia_diag, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(income, gender) %>% summarize(mean = mean(chlamydia_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(income, -mean), mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Less than $14,999", "$15,000 - $19,999", "$20,000 - $24,999", "$25,000 - $29,999", 
                                  "$30,000 - $34,999", "$35,000 - $39,999", "$40,000 - $44,999", "$45,000 - $49,999", 
                                  "$50,000 - $54,999", "$55,000 - $59,999", "$60,000 - $64,999", "$65,000 - $69,999",   
                                  "$70,000 - $74,999", "$75,000 - $79,999", "$80,000 - $84,999", "$85,000 - $89,999",   
                                  "$90,000 - $94,999", "$95,000 - $99,999", "$100,000 - $104,999", "$105,000 - $109,999", 
                                  "$110,000 - $114,999", "$115,000 - $119,999", "$120,000 - $124,999", "$125,000 - $129,999", 
                                  "$130,000 - $134,999", "$135,000 - $139,999", "$140,000 - $144,999", "$145,000 - $149,999",    
                                  "$150,000 - $159,999", "$160,000 - $169,999", "$170,000 - $179,999", "$180,000 - $189,999", 
                                  "$190,000 - $199,999", "$200,000 - $224,999", "$225,000 - $249,999", "$250,000 or more"))+ 
      labs(x = "Income", y="# of Chlamydia Diagnoses")
    
    #chlamydia ratio x education x gender
    data_full %>% select(age, education, gender, date, chlamydia) %>% 
      group_by(education, gender) %>% summarize(mean = mean(chlamydia, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(education, mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Some High School or Less", "High School", "Some College", "College", "Graduate School"))+ 
      labs(x = "Education", y="Mean Ratio of chlamydia Diagnoses")
    
    #chlamydia diag x education x gender
    data_full %>% select(age, education, gender, date, chlamydia_diag, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(education, gender) %>% summarize(mean = mean(chlamydia_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(education, mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Some High School or Less", "High School", "Some College", "College", "Graduate School"))+ 
      labs(x = "Education", y="# of chlamydia Diagnoses")
    
    
  #  #chlamydia x income
  #  data_full %>% select(age, gender, date, income, education, chlamydia, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
  #    filter(gender=="Female") %>% 
  #    group_by(age, date, gender, income, education) %>% summarize(mean = mean(chlamydia, na.rm = TRUE)) %>% 
  #    ggplot(mapping = aes(date, mean)) + geom_col() + facet_wrap(.~income, nrow=2)
  
  #Genital Warts ----
    data_full %>% select(age, gender, date, gential_warts) %>% 
      group_by(age, gender) %>% summarize(mean = mean(gential_warts, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(age, mean, fill=gender)) + geom_col() + facet_wrap(.~gender, nrow=2)+ 
      labs(x = "Age", y="Mean Ratio of Genital Warts Diagnoses")
    
    #gential_warts diags X age X gender
    data_full %>% select(age, gender, date, gential_warts_diag) %>% 
      group_by(age, gender) %>% summarize(mean = mean(gential_warts_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(age, mean, fill=gender)) + geom_col() + facet_wrap(.~gender, nrow=2)+ 
      labs(x = "Age", y="# of Genital Warts Diagnoses")
    
    #gential_warts ratio x age x gender x time
    data_full %>% select(age, gender, date, gential_warts) %>% 
      group_by(age, date, gender) %>% summarize(mean = mean(gential_warts,  na.rm = TRUE)) %>% 
      ggplot(mapping = aes(date, mean, fill=gender)) + geom_col() + facet_wrap(gender~age, nrow=2)+ 
      labs(x = "Date", y="Mean Ratio of Genital Warts Diagnoses")
    
    #gential_warts diag x age x gender x time
    data_full %>% select(age, gender, date, gential_warts_diag) %>% 
      group_by(age, date, gender) %>% summarize(mean = mean(gential_warts_diag,  na.rm = TRUE)) %>% 
      ggplot(mapping = aes(date, mean, fill=gender)) + geom_col() + facet_wrap(gender~age, nrow=2)+ 
      labs(x = "Date", y="# of Genital Warts Diagnoses")
    
    #gential_warts ratio X state (male)
    data_full %>% select(state, gender, date, gential_warts) %>% 
      filter(gender=="Male") %>% 
      group_by(state, gender) %>% summarize(mean = mean(gential_warts, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="Mean Ratio of Genital Warts Diagnoses")
    
    #gential_warts diag X state (male)
    data_full %>% select(state, gender, date, gential_warts_diag) %>% 
      filter(gender=="Male") %>% 
      group_by(state, gender) %>% summarize(mean = mean(gential_warts_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="# of Genital Warts Diagnoses")
    
    #gential_warts ratio X state (female)
    data_full %>% select(state, gender, date, gential_warts) %>% 
      filter(gender=="Female") %>% 
      group_by(state, gender) %>% summarize(mean = mean(gential_warts, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="Mean Ratio of Genital Warts Diagnoses")
    
    #gential_warts diag X state (female)
    data_full %>% select(state, gender, date, gential_warts_diag) %>% 
      filter(gender=="Female") %>% 
      group_by(state, gender) %>% summarize(mean = mean(gential_warts_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="# of Genital Warts Diagnoses")
    
    #gential_warts ratio x income x gender
    data_full %>% select(age, income, gender, date, gential_warts, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(income, gender) %>% summarize(mean = mean(gential_warts, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(income, -mean), mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Less than $14,999", "$15,000 - $19,999", "$20,000 - $24,999", "$25,000 - $29,999", 
                                  "$30,000 - $34,999", "$35,000 - $39,999", "$40,000 - $44,999", "$45,000 - $49,999", 
                                  "$50,000 - $54,999", "$55,000 - $59,999", "$60,000 - $64,999", "$65,000 - $69,999",   
                                  "$70,000 - $74,999", "$75,000 - $79,999", "$80,000 - $84,999", "$85,000 - $89,999",   
                                  "$90,000 - $94,999", "$95,000 - $99,999", "$100,000 - $104,999", "$105,000 - $109,999", 
                                  "$110,000 - $114,999", "$115,000 - $119,999", "$120,000 - $124,999", "$125,000 - $129,999", 
                                  "$130,000 - $134,999", "$135,000 - $139,999", "$140,000 - $144,999", "$145,000 - $149,999",    
                                  "$150,000 - $159,999", "$160,000 - $169,999", "$170,000 - $179,999", "$180,000 - $189,999", 
                                  "$190,000 - $199,999", "$200,000 - $224,999", "$225,000 - $249,999", "$250,000 or more"))+ 
      labs(x = "Income", y="Mean Ratio of Genital Warts Diagnoses")
    
    #gential_warts diag x income x gender
    data_full %>% select(age, income, gender, date, gential_warts_diag, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(income, gender) %>% summarize(mean = mean(gential_warts_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(income, -mean), mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Less than $14,999", "$15,000 - $19,999", "$20,000 - $24,999", "$25,000 - $29,999", 
                                  "$30,000 - $34,999", "$35,000 - $39,999", "$40,000 - $44,999", "$45,000 - $49,999", 
                                  "$50,000 - $54,999", "$55,000 - $59,999", "$60,000 - $64,999", "$65,000 - $69,999",   
                                  "$70,000 - $74,999", "$75,000 - $79,999", "$80,000 - $84,999", "$85,000 - $89,999",   
                                  "$90,000 - $94,999", "$95,000 - $99,999", "$100,000 - $104,999", "$105,000 - $109,999", 
                                  "$110,000 - $114,999", "$115,000 - $119,999", "$120,000 - $124,999", "$125,000 - $129,999", 
                                  "$130,000 - $134,999", "$135,000 - $139,999", "$140,000 - $144,999", "$145,000 - $149,999",    
                                  "$150,000 - $159,999", "$160,000 - $169,999", "$170,000 - $179,999", "$180,000 - $189,999", 
                                  "$190,000 - $199,999", "$200,000 - $224,999", "$225,000 - $249,999", "$250,000 or more"))+ 
      labs(x = "Income", y="# of Genital Warts Diagnoses")
    
    #gential_warts ratio x education x gender
    data_full %>% select(age, education, gender, date, gential_warts) %>% 
      group_by(education, gender) %>% summarize(mean = mean(gential_warts, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(education, mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Some High School or Less", "High School", "Some College", "College", "Graduate School"))+ 
      labs(x = "Education", y="Mean Ratio of Genital Warts Diagnoses")
    
    #gential_warts diag x education x gender
    data_full %>% select(age, education, gender, date, gential_warts_diag, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(education, gender) %>% summarize(mean = mean(gential_warts_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(education, mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Some High School or Less", "High School", "Some College", "College", "Graduate School"))+ 
      labs(x = "Education", y="# of Genital Warts Diagnoses")
    
    
  #Gonorrhea ----
    #gonorrhea ratio X age X gender
    data_full %>% select(age, gender, date, gonorrhea) %>% 
      group_by(age, gender) %>% summarize(mean = mean(gonorrhea, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(age, mean, fill=gender)) + geom_col() + facet_wrap(.~gender, nrow=2)+ 
      labs(x = "Age", y="Mean Ratio of gonorrhea Diagnoses")
    
    #gonorrhea diags X age X gender
    data_full %>% select(age, gender, date, gonorrhea_diag) %>% 
      group_by(age, gender) %>% summarize(mean = mean(gonorrhea_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(age, mean, fill=gender)) + geom_col() + facet_wrap(.~gender, nrow=2)+ 
      labs(x = "Age", y="# of gonorrhea Diagnoses")
    
    #gonorrhea ratio x age x gender x time
    data_full %>% select(age, gender, date, gonorrhea) %>% 
      group_by(age, date, gender) %>% summarize(mean = mean(gonorrhea,  na.rm = TRUE)) %>% 
      ggplot(mapping = aes(date, mean, fill=gender)) + geom_col() + facet_wrap(gender~age, nrow=2)+ 
      labs(x = "Date", y="Mean Ratio of gonorrhea Diagnoses")
    
    #gonorrhea diag x age x gender x time
    data_full %>% select(age, gender, date, gonorrhea_diag) %>% 
      group_by(age, date, gender) %>% summarize(mean = mean(gonorrhea_diag,  na.rm = TRUE)) %>% 
      ggplot(mapping = aes(date, mean, fill=gender)) + geom_col() + facet_wrap(gender~age, nrow=2)+ 
      labs(x = "Date", y="# of gonorrhea Diagnoses")
    
    #gonorrhea ratio X state (male)
    data_full %>% select(state, gender, date, gonorrhea) %>% 
      filter(gender=="Male") %>% 
      group_by(state, gender) %>% summarize(mean = mean(gonorrhea, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="Mean Ratio of gonorrhea Diagnoses")
    
    #gonorrhea diag X state (male)
    data_full %>% select(state, gender, date, gonorrhea_diag) %>% 
      filter(gender=="Male") %>% 
      group_by(state, gender) %>% summarize(mean = mean(gonorrhea_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="# of gonorrhea Diagnoses")
    
    #gonorrhea ratio X state (female)
    data_full %>% select(state, gender, date, gonorrhea) %>% 
      filter(gender=="Female") %>% 
      group_by(state, gender) %>% summarize(mean = mean(gonorrhea, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="Mean Ratio of gonorrhea Diagnoses")
    
    #gonorrhea diag X state (female)
    data_full %>% select(state, gender, date, gonorrhea_diag) %>% 
      filter(gender=="Female") %>% 
      group_by(state, gender) %>% summarize(mean = mean(gonorrhea_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="# of gonorrhea Diagnoses")
    
    #gonorrhea ratio x income x gender
    data_full %>% select(age, income, gender, date, gonorrhea, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(income, gender) %>% summarize(mean = mean(gonorrhea, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(income, -mean), mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Less than $14,999", "$15,000 - $19,999", "$20,000 - $24,999", "$25,000 - $29,999", 
                                  "$30,000 - $34,999", "$35,000 - $39,999", "$40,000 - $44,999", "$45,000 - $49,999", 
                                  "$50,000 - $54,999", "$55,000 - $59,999", "$60,000 - $64,999", "$65,000 - $69,999",   
                                  "$70,000 - $74,999", "$75,000 - $79,999", "$80,000 - $84,999", "$85,000 - $89,999",   
                                  "$90,000 - $94,999", "$95,000 - $99,999", "$100,000 - $104,999", "$105,000 - $109,999", 
                                  "$110,000 - $114,999", "$115,000 - $119,999", "$120,000 - $124,999", "$125,000 - $129,999", 
                                  "$130,000 - $134,999", "$135,000 - $139,999", "$140,000 - $144,999", "$145,000 - $149,999",    
                                  "$150,000 - $159,999", "$160,000 - $169,999", "$170,000 - $179,999", "$180,000 - $189,999", 
                                  "$190,000 - $199,999", "$200,000 - $224,999", "$225,000 - $249,999", "$250,000 or more"))+ 
      labs(x = "Income", y="Mean Ratio of gonorrhea Diagnoses")
    
    #gonorrhea diag x income x gender
    data_full %>% select(age, income, gender, date, gonorrhea_diag, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(income, gender) %>% summarize(mean = mean(gonorrhea_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(income, -mean), mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Less than $14,999", "$15,000 - $19,999", "$20,000 - $24,999", "$25,000 - $29,999", 
                                  "$30,000 - $34,999", "$35,000 - $39,999", "$40,000 - $44,999", "$45,000 - $49,999", 
                                  "$50,000 - $54,999", "$55,000 - $59,999", "$60,000 - $64,999", "$65,000 - $69,999",   
                                  "$70,000 - $74,999", "$75,000 - $79,999", "$80,000 - $84,999", "$85,000 - $89,999",   
                                  "$90,000 - $94,999", "$95,000 - $99,999", "$100,000 - $104,999", "$105,000 - $109,999", 
                                  "$110,000 - $114,999", "$115,000 - $119,999", "$120,000 - $124,999", "$125,000 - $129,999", 
                                  "$130,000 - $134,999", "$135,000 - $139,999", "$140,000 - $144,999", "$145,000 - $149,999",    
                                  "$150,000 - $159,999", "$160,000 - $169,999", "$170,000 - $179,999", "$180,000 - $189,999", 
                                  "$190,000 - $199,999", "$200,000 - $224,999", "$225,000 - $249,999", "$250,000 or more"))+ 
      labs(x = "Income", y="# of gonorrhea Diagnoses")
    
    #gonorrhea ratio x education x gender
    data_full %>% select(age, education, gender, date, gonorrhea) %>% 
      group_by(education, gender) %>% summarize(mean = mean(gonorrhea, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(education, mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Some High School or Less", "High School", "Some College", "College", "Graduate School"))+ 
      labs(x = "Education", y="Mean Ratio of gonorrhea Diagnoses")
    
    #gonorrhea diag x education x gender
    data_full %>% select(age, education, gender, date, gonorrhea_diag, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(education, gender) %>% summarize(mean = mean(gonorrhea_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(education, mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Some High School or Less", "High School", "Some College", "College", "Graduate School"))+ 
      labs(x = "Education", y="# of gonorrhea Diagnoses")
    
    
    
  #Herpes ----
    #herpes ratio X age X gender
    data_full %>% select(age, gender, date, herpes) %>% 
      group_by(age, gender) %>% summarize(mean = mean(herpes, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(age, mean, fill=gender)) + geom_col() + facet_wrap(.~gender, nrow=2)+ 
      labs(x = "Age", y="Mean Ratio of herpes Diagnoses")
    
    #herpes diags X age X gender
    data_full %>% select(age, gender, date, herpes_diag) %>% 
      group_by(age, gender) %>% summarize(mean = mean(herpes_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(age, mean, fill=gender)) + geom_col() + facet_wrap(.~gender, nrow=2)+ 
      labs(x = "Age", y="# of herpes Diagnoses")
    
    #herpes ratio x age x gender x time
    data_full %>% select(age, gender, date, herpes) %>% 
      group_by(age, date, gender) %>% summarize(mean = mean(herpes,  na.rm = TRUE)) %>% 
      ggplot(mapping = aes(date, mean, fill=gender)) + geom_col() + facet_wrap(gender~age, nrow=2)+ 
      labs(x = "Date", y="Mean Ratio of herpes Diagnoses")
    
    #herpes diag x age x gender x time
    data_full %>% select(age, gender, date, herpes_diag) %>% 
      group_by(age, date, gender) %>% summarize(mean = mean(herpes_diag,  na.rm = TRUE)) %>% 
      ggplot(mapping = aes(date, mean, fill=gender)) + geom_col() + facet_wrap(gender~age, nrow=2)+ 
      labs(x = "Date", y="# of herpes Diagnoses")
    
    #herpes ratio X state (male)
    data_full %>% select(state, gender, date, herpes) %>% 
      filter(gender=="Male") %>% 
      group_by(state, gender) %>% summarize(mean = mean(herpes, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="Mean Ratio of herpes Diagnoses")
    
    #herpes diag X state (male)
    data_full %>% select(state, gender, date, herpes_diag) %>% 
      filter(gender=="Male") %>% 
      group_by(state, gender) %>% summarize(mean = mean(herpes_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="# of herpes Diagnoses")
    
    #herpes ratio X state (female)
    data_full %>% select(state, gender, date, herpes) %>% 
      filter(gender=="Female") %>% 
      group_by(state, gender) %>% summarize(mean = mean(herpes, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="Mean Ratio of herpes Diagnoses")
    
    #herpes diag X state (female)
    data_full %>% select(state, gender, date, herpes_diag) %>% 
      filter(gender=="Female") %>% 
      group_by(state, gender) %>% summarize(mean = mean(herpes_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="# of herpes Diagnoses")
    
    #herpes ratio x income x gender
    data_full %>% select(age, income, gender, date, herpes, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(income, gender) %>% summarize(mean = mean(herpes, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(income, -mean), mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Less than $14,999", "$15,000 - $19,999", "$20,000 - $24,999", "$25,000 - $29,999", 
                                  "$30,000 - $34,999", "$35,000 - $39,999", "$40,000 - $44,999", "$45,000 - $49,999", 
                                  "$50,000 - $54,999", "$55,000 - $59,999", "$60,000 - $64,999", "$65,000 - $69,999",   
                                  "$70,000 - $74,999", "$75,000 - $79,999", "$80,000 - $84,999", "$85,000 - $89,999",   
                                  "$90,000 - $94,999", "$95,000 - $99,999", "$100,000 - $104,999", "$105,000 - $109,999", 
                                  "$110,000 - $114,999", "$115,000 - $119,999", "$120,000 - $124,999", "$125,000 - $129,999", 
                                  "$130,000 - $134,999", "$135,000 - $139,999", "$140,000 - $144,999", "$145,000 - $149,999",    
                                  "$150,000 - $159,999", "$160,000 - $169,999", "$170,000 - $179,999", "$180,000 - $189,999", 
                                  "$190,000 - $199,999", "$200,000 - $224,999", "$225,000 - $249,999", "$250,000 or more"))+ 
      labs(x = "Income", y="Mean Ratio of herpes Diagnoses")
    
    #herpes diag x income x gender
    data_full %>% select(age, income, gender, date, herpes_diag, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(income, gender) %>% summarize(mean = mean(herpes_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(income, -mean), mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Less than $14,999", "$15,000 - $19,999", "$20,000 - $24,999", "$25,000 - $29,999", 
                                  "$30,000 - $34,999", "$35,000 - $39,999", "$40,000 - $44,999", "$45,000 - $49,999", 
                                  "$50,000 - $54,999", "$55,000 - $59,999", "$60,000 - $64,999", "$65,000 - $69,999",   
                                  "$70,000 - $74,999", "$75,000 - $79,999", "$80,000 - $84,999", "$85,000 - $89,999",   
                                  "$90,000 - $94,999", "$95,000 - $99,999", "$100,000 - $104,999", "$105,000 - $109,999", 
                                  "$110,000 - $114,999", "$115,000 - $119,999", "$120,000 - $124,999", "$125,000 - $129,999", 
                                  "$130,000 - $134,999", "$135,000 - $139,999", "$140,000 - $144,999", "$145,000 - $149,999",    
                                  "$150,000 - $159,999", "$160,000 - $169,999", "$170,000 - $179,999", "$180,000 - $189,999", 
                                  "$190,000 - $199,999", "$200,000 - $224,999", "$225,000 - $249,999", "$250,000 or more"))+ 
      labs(x = "Income", y="# of herpes Diagnoses")
    
    #herpes ratio x education x gender
    data_full %>% select(age, education, gender, date, herpes) %>% 
      group_by(education, gender) %>% summarize(mean = mean(herpes, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(education, mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Some High School or Less", "High School", "Some College", "College", "Graduate School"))+ 
      labs(x = "Education", y="Mean Ratio of herpes Diagnoses")
    
    #herpes diag x education x gender
    data_full %>% select(age, education, gender, date, herpes_diag, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(education, gender) %>% summarize(mean = mean(herpes_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(education, mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Some High School or Less", "High School", "Some College", "College", "Graduate School"))+ 
      labs(x = "Education", y="# of herpes Diagnoses")
    
    
    
  #HPV ----
    #hpv ratio X age X gender
    data_full %>% select(age, gender, date, hpv) %>% 
      group_by(age, gender) %>% summarize(mean = mean(hpv, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(age, mean, fill=gender)) + geom_col() + facet_wrap(.~gender, nrow=2)+ 
      labs(x = "Age", y="Mean Ratio of hpv Diagnoses")
    
    #hpv diags X age X gender
    data_full %>% select(age, gender, date, hpv_diag) %>% 
      group_by(age, gender) %>% summarize(mean = mean(hpv_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(age, mean, fill=gender)) + geom_col() + facet_wrap(.~gender, nrow=2)+ 
      labs(x = "Age", y="# of hpv Diagnoses")
    
    #hpv ratio x age x gender x time
    data_full %>% select(age, gender, date, hpv) %>% 
      group_by(age, date, gender) %>% summarize(mean = mean(hpv,  na.rm = TRUE)) %>% 
      ggplot(mapping = aes(date, mean, fill=gender)) + geom_col() + facet_wrap(gender~age, nrow=2)+ 
      labs(x = "Date", y="Mean Ratio of hpv Diagnoses")
    
    #hpv diag x age x gender x time
    data_full %>% select(age, gender, date, hpv_diag) %>% 
      group_by(age, date, gender) %>% summarize(mean = mean(hpv_diag,  na.rm = TRUE)) %>% 
      ggplot(mapping = aes(date, mean, fill=gender)) + geom_col() + facet_wrap(gender~age, nrow=2)+ 
      labs(x = "Date", y="# of hpv Diagnoses")
    
    #hpv ratio X state (male)
    data_full %>% select(state, gender, date, hpv) %>% 
      filter(gender=="Male") %>% 
      group_by(state, gender) %>% summarize(mean = mean(hpv, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="Mean Ratio of hpv Diagnoses")
    
    #hpv diag X state (male)
    data_full %>% select(state, gender, date, hpv_diag) %>% 
      filter(gender=="Male") %>% 
      group_by(state, gender) %>% summarize(mean = mean(hpv_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="# of hpv Diagnoses")
    
    #hpv ratio X state (female)
    data_full %>% select(state, gender, date, hpv) %>% 
      filter(gender=="Female") %>% 
      group_by(state, gender) %>% summarize(mean = mean(hpv, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="Mean Ratio of hpv Diagnoses")
    
    #hpv diag X state (female)
    data_full %>% select(state, gender, date, hpv_diag) %>% 
      filter(gender=="Female") %>% 
      group_by(state, gender) %>% summarize(mean = mean(hpv_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="# of hpv Diagnoses")
    
    #hpv ratio x income x gender
    data_full %>% select(age, income, gender, date, hpv, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(income, gender) %>% summarize(mean = mean(hpv, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(income, -mean), mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Less than $14,999", "$15,000 - $19,999", "$20,000 - $24,999", "$25,000 - $29,999", 
                                  "$30,000 - $34,999", "$35,000 - $39,999", "$40,000 - $44,999", "$45,000 - $49,999", 
                                  "$50,000 - $54,999", "$55,000 - $59,999", "$60,000 - $64,999", "$65,000 - $69,999",   
                                  "$70,000 - $74,999", "$75,000 - $79,999", "$80,000 - $84,999", "$85,000 - $89,999",   
                                  "$90,000 - $94,999", "$95,000 - $99,999", "$100,000 - $104,999", "$105,000 - $109,999", 
                                  "$110,000 - $114,999", "$115,000 - $119,999", "$120,000 - $124,999", "$125,000 - $129,999", 
                                  "$130,000 - $134,999", "$135,000 - $139,999", "$140,000 - $144,999", "$145,000 - $149,999",    
                                  "$150,000 - $159,999", "$160,000 - $169,999", "$170,000 - $179,999", "$180,000 - $189,999", 
                                  "$190,000 - $199,999", "$200,000 - $224,999", "$225,000 - $249,999", "$250,000 or more"))+ 
      labs(x = "Income", y="Mean Ratio of hpv Diagnoses")
    
    #hpv diag x income x gender
    data_full %>% select(age, income, gender, date, hpv_diag, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(income, gender) %>% summarize(mean = mean(hpv_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(income, -mean), mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Less than $14,999", "$15,000 - $19,999", "$20,000 - $24,999", "$25,000 - $29,999", 
                                  "$30,000 - $34,999", "$35,000 - $39,999", "$40,000 - $44,999", "$45,000 - $49,999", 
                                  "$50,000 - $54,999", "$55,000 - $59,999", "$60,000 - $64,999", "$65,000 - $69,999",   
                                  "$70,000 - $74,999", "$75,000 - $79,999", "$80,000 - $84,999", "$85,000 - $89,999",   
                                  "$90,000 - $94,999", "$95,000 - $99,999", "$100,000 - $104,999", "$105,000 - $109,999", 
                                  "$110,000 - $114,999", "$115,000 - $119,999", "$120,000 - $124,999", "$125,000 - $129,999", 
                                  "$130,000 - $134,999", "$135,000 - $139,999", "$140,000 - $144,999", "$145,000 - $149,999",    
                                  "$150,000 - $159,999", "$160,000 - $169,999", "$170,000 - $179,999", "$180,000 - $189,999", 
                                  "$190,000 - $199,999", "$200,000 - $224,999", "$225,000 - $249,999", "$250,000 or more"))+ 
      labs(x = "Income", y="# of hpv Diagnoses")
    
    #hpv ratio x education x gender
    data_full %>% select(age, education, gender, date, hpv) %>% 
      group_by(education, gender) %>% summarize(mean = mean(hpv, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(education, mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Some High School or Less", "High School", "Some College", "College", "Graduate School"))+ 
      labs(x = "Education", y="Mean Ratio of hpv Diagnoses")
    
    #hpv diag x education x gender
    data_full %>% select(age, education, gender, date, hpv_diag, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(education, gender) %>% summarize(mean = mean(hpv_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(education, mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Some High School or Less", "High School", "Some College", "College", "Graduate School"))+ 
      labs(x = "Education", y="# of hpv Diagnoses")
    
    
    
  #Other STDs ----
    #other_std ratio X age X gender
    data_full %>% select(age, gender, date, other_std) %>% 
      group_by(age, gender) %>% summarize(mean = mean(other_std, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(age, mean, fill=gender)) + geom_col() + facet_wrap(.~gender, nrow=2)+ 
      labs(x = "Age", y="Mean Ratio of other_std Diagnoses")
    
    #other_std diags X age X gender
    data_full %>% select(age, gender, date, other_std_diag) %>% 
      group_by(age, gender) %>% summarize(mean = mean(other_std_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(age, mean, fill=gender)) + geom_col() + facet_wrap(.~gender, nrow=2)+ 
      labs(x = "Age", y="# of other_std Diagnoses")
    
    #other_std ratio x age x gender x time
    data_full %>% select(age, gender, date, other_std) %>% 
      group_by(age, date, gender) %>% summarize(mean = mean(other_std,  na.rm = TRUE)) %>% 
      ggplot(mapping = aes(date, mean, fill=gender)) + geom_col() + facet_wrap(gender~age, nrow=2)+ 
      labs(x = "Date", y="Mean Ratio of other_std Diagnoses")
    
    #other_std diag x age x gender x time
    data_full %>% select(age, gender, date, other_std_diag) %>% 
      group_by(age, date, gender) %>% summarize(mean = mean(other_std_diag,  na.rm = TRUE)) %>% 
      ggplot(mapping = aes(date, mean, fill=gender)) + geom_col() + facet_wrap(gender~age, nrow=2)+ 
      labs(x = "Date", y="# of other_std Diagnoses")
    
    #other_std ratio X state (male)
    data_full %>% select(state, gender, date, other_std) %>% 
      filter(gender=="Male") %>% 
      group_by(state, gender) %>% summarize(mean = mean(other_std, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="Mean Ratio of other_std Diagnoses")
    
    #other_std diag X state (male)
    data_full %>% select(state, gender, date, other_std_diag) %>% 
      filter(gender=="Male") %>% 
      group_by(state, gender) %>% summarize(mean = mean(other_std_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="# of other_std Diagnoses")
    
    #other_std ratio X state (female)
    data_full %>% select(state, gender, date, other_std) %>% 
      filter(gender=="Female") %>% 
      group_by(state, gender) %>% summarize(mean = mean(other_std, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="Mean Ratio of other_std Diagnoses")
    
    #other_std diag X state (female)
    data_full %>% select(state, gender, date, other_std_diag) %>% 
      filter(gender=="Female") %>% 
      group_by(state, gender) %>% summarize(mean = mean(other_std_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="# of other_std Diagnoses")
    
    #other_std ratio x income x gender
    data_full %>% select(age, income, gender, date, other_std, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(income, gender) %>% summarize(mean = mean(other_std, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(income, -mean), mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Less than $14,999", "$15,000 - $19,999", "$20,000 - $24,999", "$25,000 - $29,999", 
                                  "$30,000 - $34,999", "$35,000 - $39,999", "$40,000 - $44,999", "$45,000 - $49,999", 
                                  "$50,000 - $54,999", "$55,000 - $59,999", "$60,000 - $64,999", "$65,000 - $69,999",   
                                  "$70,000 - $74,999", "$75,000 - $79,999", "$80,000 - $84,999", "$85,000 - $89,999",   
                                  "$90,000 - $94,999", "$95,000 - $99,999", "$100,000 - $104,999", "$105,000 - $109,999", 
                                  "$110,000 - $114,999", "$115,000 - $119,999", "$120,000 - $124,999", "$125,000 - $129,999", 
                                  "$130,000 - $134,999", "$135,000 - $139,999", "$140,000 - $144,999", "$145,000 - $149,999",    
                                  "$150,000 - $159,999", "$160,000 - $169,999", "$170,000 - $179,999", "$180,000 - $189,999", 
                                  "$190,000 - $199,999", "$200,000 - $224,999", "$225,000 - $249,999", "$250,000 or more"))+ 
      labs(x = "Income", y="Mean Ratio of other_std Diagnoses")
    
    #other_std diag x income x gender
    data_full %>% select(age, income, gender, date, other_std_diag, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(income, gender) %>% summarize(mean = mean(other_std_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(income, -mean), mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Less than $14,999", "$15,000 - $19,999", "$20,000 - $24,999", "$25,000 - $29,999", 
                                  "$30,000 - $34,999", "$35,000 - $39,999", "$40,000 - $44,999", "$45,000 - $49,999", 
                                  "$50,000 - $54,999", "$55,000 - $59,999", "$60,000 - $64,999", "$65,000 - $69,999",   
                                  "$70,000 - $74,999", "$75,000 - $79,999", "$80,000 - $84,999", "$85,000 - $89,999",   
                                  "$90,000 - $94,999", "$95,000 - $99,999", "$100,000 - $104,999", "$105,000 - $109,999", 
                                  "$110,000 - $114,999", "$115,000 - $119,999", "$120,000 - $124,999", "$125,000 - $129,999", 
                                  "$130,000 - $134,999", "$135,000 - $139,999", "$140,000 - $144,999", "$145,000 - $149,999",    
                                  "$150,000 - $159,999", "$160,000 - $169,999", "$170,000 - $179,999", "$180,000 - $189,999", 
                                  "$190,000 - $199,999", "$200,000 - $224,999", "$225,000 - $249,999", "$250,000 or more"))+ 
      labs(x = "Income", y="# of other_std Diagnoses")
    
    #other_std ratio x education x gender
    data_full %>% select(age, education, gender, date, other_std) %>% 
      group_by(education, gender) %>% summarize(mean = mean(other_std, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(education, mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Some High School or Less", "High School", "Some College", "College", "Graduate School"))+ 
      labs(x = "Education", y="Mean Ratio of other_std Diagnoses")
    
    #other_std diag x education x gender
    data_full %>% select(age, education, gender, date, other_std_diag, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(education, gender) %>% summarize(mean = mean(other_std_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(education, mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Some High School or Less", "High School", "Some College", "College", "Graduate School"))+ 
      labs(x = "Education", y="# of other_std Diagnoses")
  
    
  #Parasitic ----
    #parasitic ratio X age X gender
    data_full %>% select(age, gender, date, parasitic) %>% 
      group_by(age, gender) %>% summarize(mean = mean(parasitic, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(age, mean, fill=gender)) + geom_col() + facet_wrap(.~gender, nrow=2)+ 
      labs(x = "Age", y="Mean Ratio of parasitic Diagnoses")
    
    #parasitic diags X age X gender
    data_full %>% select(age, gender, date, parasitic_diag) %>% 
      group_by(age, gender) %>% summarize(mean = mean(parasitic_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(age, mean, fill=gender)) + geom_col() + facet_wrap(.~gender, nrow=2)+ 
      labs(x = "Age", y="# of parasitic Diagnoses")
    
    #parasitic ratio x age x gender x time
    data_full %>% select(age, gender, date, parasitic) %>% 
      group_by(age, date, gender) %>% summarize(mean = mean(parasitic,  na.rm = TRUE)) %>% 
      ggplot(mapping = aes(date, mean, fill=gender)) + geom_col() + facet_wrap(gender~age, nrow=2)+ 
      labs(x = "Date", y="Mean Ratio of parasitic Diagnoses")
    
    #parasitic diag x age x gender x time
    data_full %>% select(age, gender, date, parasitic_diag) %>% 
      group_by(age, date, gender) %>% summarize(mean = mean(parasitic_diag,  na.rm = TRUE)) %>% 
      ggplot(mapping = aes(date, mean, fill=gender)) + geom_col() + facet_wrap(gender~age, nrow=2)+ 
      labs(x = "Date", y="# of parasitic Diagnoses")
    
    #parasitic ratio X state (male)
    data_full %>% select(state, gender, date, parasitic) %>% 
      filter(gender=="Male") %>% 
      group_by(state, gender) %>% summarize(mean = mean(parasitic, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="Mean Ratio of parasitic Diagnoses")
    
    #parasitic diag X state (male)
    data_full %>% select(state, gender, date, parasitic_diag) %>% 
      filter(gender=="Male") %>% 
      group_by(state, gender) %>% summarize(mean = mean(parasitic_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="# of parasitic Diagnoses")
    
    #parasitic ratio X state (female)
    data_full %>% select(state, gender, date, parasitic) %>% 
      filter(gender=="Female") %>% 
      group_by(state, gender) %>% summarize(mean = mean(parasitic, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="Mean Ratio of parasitic Diagnoses")
    
    #parasitic diag X state (female)
    data_full %>% select(state, gender, date, parasitic_diag) %>% 
      filter(gender=="Female") %>% 
      group_by(state, gender) %>% summarize(mean = mean(parasitic_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="# of parasitic Diagnoses")
    
    #parasitic ratio x income x gender
    data_full %>% select(age, income, gender, date, parasitic, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(income, gender) %>% summarize(mean = mean(parasitic, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(income, -mean), mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Less than $14,999", "$15,000 - $19,999", "$20,000 - $24,999", "$25,000 - $29,999", 
                                  "$30,000 - $34,999", "$35,000 - $39,999", "$40,000 - $44,999", "$45,000 - $49,999", 
                                  "$50,000 - $54,999", "$55,000 - $59,999", "$60,000 - $64,999", "$65,000 - $69,999",   
                                  "$70,000 - $74,999", "$75,000 - $79,999", "$80,000 - $84,999", "$85,000 - $89,999",   
                                  "$90,000 - $94,999", "$95,000 - $99,999", "$100,000 - $104,999", "$105,000 - $109,999", 
                                  "$110,000 - $114,999", "$115,000 - $119,999", "$120,000 - $124,999", "$125,000 - $129,999", 
                                  "$130,000 - $134,999", "$135,000 - $139,999", "$140,000 - $144,999", "$145,000 - $149,999",    
                                  "$150,000 - $159,999", "$160,000 - $169,999", "$170,000 - $179,999", "$180,000 - $189,999", 
                                  "$190,000 - $199,999", "$200,000 - $224,999", "$225,000 - $249,999", "$250,000 or more"))+ 
      labs(x = "Income", y="Mean Ratio of parasitic Diagnoses")
    
    #parasitic diag x income x gender
    data_full %>% select(age, income, gender, date, parasitic_diag, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(income, gender) %>% summarize(mean = mean(parasitic_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(income, -mean), mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Less than $14,999", "$15,000 - $19,999", "$20,000 - $24,999", "$25,000 - $29,999", 
                                  "$30,000 - $34,999", "$35,000 - $39,999", "$40,000 - $44,999", "$45,000 - $49,999", 
                                  "$50,000 - $54,999", "$55,000 - $59,999", "$60,000 - $64,999", "$65,000 - $69,999",   
                                  "$70,000 - $74,999", "$75,000 - $79,999", "$80,000 - $84,999", "$85,000 - $89,999",   
                                  "$90,000 - $94,999", "$95,000 - $99,999", "$100,000 - $104,999", "$105,000 - $109,999", 
                                  "$110,000 - $114,999", "$115,000 - $119,999", "$120,000 - $124,999", "$125,000 - $129,999", 
                                  "$130,000 - $134,999", "$135,000 - $139,999", "$140,000 - $144,999", "$145,000 - $149,999",    
                                  "$150,000 - $159,999", "$160,000 - $169,999", "$170,000 - $179,999", "$180,000 - $189,999", 
                                  "$190,000 - $199,999", "$200,000 - $224,999", "$225,000 - $249,999", "$250,000 or more"))+ 
      labs(x = "Income", y="# of parasitic Diagnoses")
    
    #parasitic ratio x education x gender
    data_full %>% select(age, education, gender, date, parasitic) %>% 
      group_by(education, gender) %>% summarize(mean = mean(parasitic, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(education, mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Some High School or Less", "High School", "Some College", "College", "Graduate School"))+ 
      labs(x = "Education", y="Mean Ratio of parasitic Diagnoses")
    
    #parasitic diag x education x gender
    data_full %>% select(age, education, gender, date, parasitic_diag, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(education, gender) %>% summarize(mean = mean(parasitic_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(education, mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Some High School or Less", "High School", "Some College", "College", "Graduate School"))+ 
      labs(x = "Education", y="# of parasitic Diagnoses")
    
    
  #Syphilis ----
    #syphilis ratio X age X gender
    data_full %>% select(age, gender, date, syphilis) %>% 
      group_by(age, gender) %>% summarize(mean = mean(syphilis, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(age, mean, fill=gender)) + geom_col() + facet_wrap(.~gender, nrow=2)+ 
      labs(x = "Age", y="Mean Ratio of syphilis Diagnoses")
    
    #syphilis diags X age X gender
    data_full %>% select(age, gender, date, syphilis_diag) %>% 
      group_by(age, gender) %>% summarize(mean = mean(syphilis_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(age, mean, fill=gender)) + geom_col() + facet_wrap(.~gender, nrow=2)+ 
      labs(x = "Age", y="# of syphilis Diagnoses")
    
    #syphilis ratio x age x gender x time
    data_full %>% select(age, gender, date, syphilis) %>% 
      group_by(age, date, gender) %>% summarize(mean = mean(syphilis,  na.rm = TRUE)) %>% 
      ggplot(mapping = aes(date, mean, fill=gender)) + geom_col() + facet_wrap(gender~age, nrow=2)+ 
      labs(x = "Date", y="Mean Ratio of syphilis Diagnoses")
    
    #syphilis diag x age x gender x time
    data_full %>% select(age, gender, date, syphilis_diag) %>% 
      group_by(age, date, gender) %>% summarize(mean = mean(syphilis_diag,  na.rm = TRUE)) %>% 
      ggplot(mapping = aes(date, mean, fill=gender)) + geom_col() + facet_wrap(gender~age, nrow=2)+ 
      labs(x = "Date", y="# of syphilis Diagnoses")
    
    #syphilis ratio X state (male)
    data_full %>% select(state, gender, date, syphilis) %>% 
      filter(gender=="Male") %>% 
      group_by(state, gender) %>% summarize(mean = mean(syphilis, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="Mean Ratio of syphilis Diagnoses")
    
    #syphilis diag X state (male)
    data_full %>% select(state, gender, date, syphilis_diag) %>% 
      filter(gender=="Male") %>% 
      group_by(state, gender) %>% summarize(mean = mean(syphilis_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="# of syphilis Diagnoses")
    
    #syphilis ratio X state (female)
    data_full %>% select(state, gender, date, syphilis) %>% 
      filter(gender=="Female") %>% 
      group_by(state, gender) %>% summarize(mean = mean(syphilis, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="Mean Ratio of syphilis Diagnoses")
    
    #syphilis diag X state (female)
    data_full %>% select(state, gender, date, syphilis_diag) %>% 
      filter(gender=="Female") %>% 
      group_by(state, gender) %>% summarize(mean = mean(syphilis_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="# of syphilis Diagnoses")
    
    #syphilis ratio x income x gender
    data_full %>% select(age, income, gender, date, syphilis, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(income, gender) %>% summarize(mean = mean(syphilis, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(income, -mean), mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Less than $14,999", "$15,000 - $19,999", "$20,000 - $24,999", "$25,000 - $29,999", 
                                  "$30,000 - $34,999", "$35,000 - $39,999", "$40,000 - $44,999", "$45,000 - $49,999", 
                                  "$50,000 - $54,999", "$55,000 - $59,999", "$60,000 - $64,999", "$65,000 - $69,999",   
                                  "$70,000 - $74,999", "$75,000 - $79,999", "$80,000 - $84,999", "$85,000 - $89,999",   
                                  "$90,000 - $94,999", "$95,000 - $99,999", "$100,000 - $104,999", "$105,000 - $109,999", 
                                  "$110,000 - $114,999", "$115,000 - $119,999", "$120,000 - $124,999", "$125,000 - $129,999", 
                                  "$130,000 - $134,999", "$135,000 - $139,999", "$140,000 - $144,999", "$145,000 - $149,999",    
                                  "$150,000 - $159,999", "$160,000 - $169,999", "$170,000 - $179,999", "$180,000 - $189,999", 
                                  "$190,000 - $199,999", "$200,000 - $224,999", "$225,000 - $249,999", "$250,000 or more"))+ 
      labs(x = "Income", y="Mean Ratio of syphilis Diagnoses")
    
    #syphilis diag x income x gender
    data_full %>% select(age, income, gender, date, syphilis_diag, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(income, gender) %>% summarize(mean = mean(syphilis_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(income, -mean), mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Less than $14,999", "$15,000 - $19,999", "$20,000 - $24,999", "$25,000 - $29,999", 
                                  "$30,000 - $34,999", "$35,000 - $39,999", "$40,000 - $44,999", "$45,000 - $49,999", 
                                  "$50,000 - $54,999", "$55,000 - $59,999", "$60,000 - $64,999", "$65,000 - $69,999",   
                                  "$70,000 - $74,999", "$75,000 - $79,999", "$80,000 - $84,999", "$85,000 - $89,999",   
                                  "$90,000 - $94,999", "$95,000 - $99,999", "$100,000 - $104,999", "$105,000 - $109,999", 
                                  "$110,000 - $114,999", "$115,000 - $119,999", "$120,000 - $124,999", "$125,000 - $129,999", 
                                  "$130,000 - $134,999", "$135,000 - $139,999", "$140,000 - $144,999", "$145,000 - $149,999",    
                                  "$150,000 - $159,999", "$160,000 - $169,999", "$170,000 - $179,999", "$180,000 - $189,999", 
                                  "$190,000 - $199,999", "$200,000 - $224,999", "$225,000 - $249,999", "$250,000 or more"))+ 
      labs(x = "Income", y="# of syphilis Diagnoses")
    
    #syphilis ratio x education x gender
    data_full %>% select(age, education, gender, date, syphilis) %>% 
      group_by(education, gender) %>% summarize(mean = mean(syphilis, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(education, mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Some High School or Less", "High School", "Some College", "College", "Graduate School"))+ 
      labs(x = "Education", y="Mean Ratio of syphilis Diagnoses")
    
    #syphilis diag x education x gender
    data_full %>% select(age, education, gender, date, syphilis_diag, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(education, gender) %>% summarize(mean = mean(syphilis_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(education, mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Some High School or Less", "High School", "Some College", "College", "Graduate School"))+ 
      labs(x = "Education", y="# of syphilis Diagnoses")
    
    
  #Trich ----
    #trich ratio X age X gender
    data_full %>% select(age, gender, date, trich) %>% 
      group_by(age, gender) %>% summarize(mean = mean(trich, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(age, mean, fill=gender)) + geom_col() + facet_wrap(.~gender, nrow=2)+ 
      labs(x = "Age", y="Mean Ratio of trich Diagnoses")
    
    #trich diags X age X gender
    data_full %>% select(age, gender, date, trich_diag) %>% 
      group_by(age, gender) %>% summarize(mean = mean(trich_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(age, mean, fill=gender)) + geom_col() + facet_wrap(.~gender, nrow=2)+ 
      labs(x = "Age", y="# of trich Diagnoses")
    
    #trich ratio x age x gender x time
    data_full %>% select(age, gender, date, trich) %>% 
      group_by(age, date, gender) %>% summarize(mean = mean(trich,  na.rm = TRUE)) %>% 
      ggplot(mapping = aes(date, mean, fill=gender)) + geom_col() + facet_wrap(gender~age, nrow=2)+ 
      labs(x = "Date", y="Mean Ratio of trich Diagnoses")
    
    #trich diag x age x gender x time
    data_full %>% select(age, gender, date, trich_diag) %>% 
      group_by(age, date, gender) %>% summarize(mean = mean(trich_diag,  na.rm = TRUE)) %>% 
      ggplot(mapping = aes(date, mean, fill=gender)) + geom_col() + facet_wrap(gender~age, nrow=2)+ 
      labs(x = "Date", y="# of trich Diagnoses")
    
    #trich ratio X state (male)
    data_full %>% select(state, gender, date, trich) %>% 
      filter(gender=="Male") %>% 
      group_by(state, gender) %>% summarize(mean = mean(trich, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="Mean Ratio of trich Diagnoses")
    
    #trich diag X state (male)
    data_full %>% select(state, gender, date, trich_diag) %>% 
      filter(gender=="Male") %>% 
      group_by(state, gender) %>% summarize(mean = mean(trich_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="# of trich Diagnoses")
    
    #trich ratio X state (female)
    data_full %>% select(state, gender, date, trich) %>% 
      filter(gender=="Female") %>% 
      group_by(state, gender) %>% summarize(mean = mean(trich, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="Mean Ratio of trich Diagnoses")
    
    #trich diag X state (female)
    data_full %>% select(state, gender, date, trich_diag) %>% 
      filter(gender=="Female") %>% 
      group_by(state, gender) %>% summarize(mean = mean(trich_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(state, -mean), mean, fill=gender)) + geom_col() + 
      facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+ 
      labs(x = "State", y="# of trich Diagnoses")
    
    #trich ratio x income x gender
    data_full %>% select(age, income, gender, date, trich, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(income, gender) %>% summarize(mean = mean(trich, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(income, -mean), mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Less than $14,999", "$15,000 - $19,999", "$20,000 - $24,999", "$25,000 - $29,999", 
                                  "$30,000 - $34,999", "$35,000 - $39,999", "$40,000 - $44,999", "$45,000 - $49,999", 
                                  "$50,000 - $54,999", "$55,000 - $59,999", "$60,000 - $64,999", "$65,000 - $69,999",   
                                  "$70,000 - $74,999", "$75,000 - $79,999", "$80,000 - $84,999", "$85,000 - $89,999",   
                                  "$90,000 - $94,999", "$95,000 - $99,999", "$100,000 - $104,999", "$105,000 - $109,999", 
                                  "$110,000 - $114,999", "$115,000 - $119,999", "$120,000 - $124,999", "$125,000 - $129,999", 
                                  "$130,000 - $134,999", "$135,000 - $139,999", "$140,000 - $144,999", "$145,000 - $149,999",    
                                  "$150,000 - $159,999", "$160,000 - $169,999", "$170,000 - $179,999", "$180,000 - $189,999", 
                                  "$190,000 - $199,999", "$200,000 - $224,999", "$225,000 - $249,999", "$250,000 or more"))+ 
      labs(x = "Income", y="Mean Ratio of trich Diagnoses")
    
    #trich diag x income x gender
    data_full %>% select(age, income, gender, date, trich_diag, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(income, gender) %>% summarize(mean = mean(trich_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(reorder(income, -mean), mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Less than $14,999", "$15,000 - $19,999", "$20,000 - $24,999", "$25,000 - $29,999", 
                                  "$30,000 - $34,999", "$35,000 - $39,999", "$40,000 - $44,999", "$45,000 - $49,999", 
                                  "$50,000 - $54,999", "$55,000 - $59,999", "$60,000 - $64,999", "$65,000 - $69,999",   
                                  "$70,000 - $74,999", "$75,000 - $79,999", "$80,000 - $84,999", "$85,000 - $89,999",   
                                  "$90,000 - $94,999", "$95,000 - $99,999", "$100,000 - $104,999", "$105,000 - $109,999", 
                                  "$110,000 - $114,999", "$115,000 - $119,999", "$120,000 - $124,999", "$125,000 - $129,999", 
                                  "$130,000 - $134,999", "$135,000 - $139,999", "$140,000 - $144,999", "$145,000 - $149,999",    
                                  "$150,000 - $159,999", "$160,000 - $169,999", "$170,000 - $179,999", "$180,000 - $189,999", 
                                  "$190,000 - $199,999", "$200,000 - $224,999", "$225,000 - $249,999", "$250,000 or more"))+ 
      labs(x = "Income", y="# of trich Diagnoses")
    
    #trich ratio x education x gender
    data_full %>% select(age, education, gender, date, trich) %>% 
      group_by(education, gender) %>% summarize(mean = mean(trich, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(education, mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Some High School or Less", "High School", "Some College", "College", "Graduate School"))+ 
      labs(x = "Education", y="Mean Ratio of trich Diagnoses")
    
    #trich diag x education x gender
    data_full %>% select(age, education, gender, date, trich_diag, gential_warts, gonorrhea, herpes, hpv, other_std, parasitic, syphilis, trich) %>% 
      group_by(education, gender) %>% summarize(mean = mean(trich_diag, na.rm = TRUE)) %>% 
      ggplot(mapping = aes(education, mean, fill=gender)) + geom_col() + facet_wrap(gender~., nrow=2) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
      scale_x_discrete(limits = c("Some High School or Less", "High School", "Some College", "College", "Graduate School"))+ 
      labs(x = "Education", y="# of trich Diagnoses")
    
    
    
  
  
    
  
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
  
  
  