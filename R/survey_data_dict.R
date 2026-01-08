#'
#'
#' Usual data dictionaries
#'



data_dict_col_names = list(goal = c("partacc_goal", "partacc_goal_date"),
                           pi = paste0("partacc_pi_", c("name", "title", "inst", "email", "phone")),
                           copi = paste0("partacc_copi_", c("name", "title", "inst", "email", "phone")),
                           details = c("partacc_hypoth", "partacc_aims"),
                           theme = c("partacc_theme_yn", "partacc_theme_details"),
                           funding = c(paste0("partacc_fund_", c("source", "entity", "details", "2nd_entity", "2nd_details")),
                                       paste0("partacc_irb_", c("contact", "name"))),
                           
                           sample_size = paste0("partacc_", c("norm", "preclin", "mci", "mild_dem", "sev_dem", "total"), "_n"),
                           minorities = paste0("partacc_", c("minor", "stratify")),
                           
                           resources = paste0("partacc_res_req___", c(1:3)),
                           
                           data_var = paste0("partacc_data_", c("demog", "medhist", "socialdet", "neuroexam", "cogtest", "mri", "amyloid", "tau", "image_files", "csf", "blood", "bldbiom", "genetic")),
                           
                           human_subj = paste0("partacc_", c("procedures", "duration", "comp_amt")),
                           
                           bank_blood = paste0("partacc_bank_", c("plasma", "serum", "buffy", "dna", "rna"), "___1"),
                           bank_blood_link = c(paste0("partacc_bank_", c("plasma", "serum"), "_vol"), rep(NA, 3)),
                           
                           bank_other = paste0("partacc_bank_", c("csf", "urine"), "___1"),
                           bank_other_link = paste0("partacc_bank_", c("csf", "urine"), "_vol"),
                           
                           bank_cells = paste0("partacc_bank_", c("pbmc", "fibro"), "___1"),
                           bank_brain = c(paste0("partacc_bank_", c("froz", "fix", "paraf"), "_brain", "___1"), "partacc_bank_brain_sec"),
                           
                           stats = c("partacc_stat", "partacc_stat_name")
                                        
                           )
                          


data_dict_md_names = list(goal = c("Goal", "Grant Deadline"),
                          pi = c("Name", "Title", "Institution", "Email", "Phone"),
                          details = c("Hypothesis", "Specific Aims"),
                          theme = c("Theme", "Details"),
                          funding = c("Funding source", "Entity", "Details", "Secondary entity", "Secondary details", "IRB Contact", "IRB Protocol #"),
                          
                          sample_size = c("Normal Controls", "Preclinical AD", "MCI", "Mild Dementia", "Moderate to Severe", "Total N"),
                          minorities = c("Disparities", "Stratification"),
                          
                          resources = c("Existing Data", "Human Subjects", "Biospecimens"),
                          data_var = c("Demographics", "Medical History", "Social Determinants", "Clinical Exam", "Cognitive Testing", "MRI Values", "Amyloid PET Values", "Tau PET Values", "Raw MRI/PET Image Files",
                                       "CSF", "Blood Test", "AD Blood Biomarkers", "Genetics"),
                          human_subj = c("Study procedures", "Study duration", "Compensation"),
                          
                          bank_blood = c("Plasma", "Serum", "Buffy Coat", "DNA", "RNA"),
                          bank_other = c("CSF", "Urine"),
                          bank_cells = c("PBMC", "Fibroblasts"),
                          bank_brain = c("Fixed", "Frozen", "Parafin", "Region")
                          
                          
                          )



data_dict_factor = list(partacc_goal = c("Formal request for ADRC data", "Preliminary inquiry for further discussion", "Request for letter of support"),
                        partacc_theme_yn = c("Study related to Deep South Disparities", "This study is not related to Deep South disparities"),
                        partacc_fund_source = c("Already funded", "Not yet funded"),
                        partacc_fund_entity = c("NIH funded grant/application", "Extramural (non-NIH) entity", "Intramurally funded"),
                        partacc_fund_2nd_entity = c("NIH funded grant/application", "Extramural (non-NIH) entity", "Intramurally funded"),
                        partacc_irb_contact = c("Yes, we have IRB approval", "IRB contacted for discussion / pending approval", "Not yet discussed project with IRB"),
                        partacc_minor = c("This study tests hypothesis on B/AA disparities or other race issues", "This study does NOT test hypothesis on racial disparities"),
                        data_var = list(head="partacc_data",
                                        levels = c("Required", "If available", "Not needed")),
                        partacc_stat = c("Statistician has already been consulted", "Would like to discuss statistics with the ADRC")
                        )




data_dict_group_names <- list(bank_blood = "Blood",
                              bank_other = "Other fluid",
                              bank_cells = "Cells",
                              bank_brain = "Brain tissue")






