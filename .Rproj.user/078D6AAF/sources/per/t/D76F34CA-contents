# Global reporting indicator MM-2.1.C
# Load main .rmd data chunk first 

library(webshot)

fsc_2021 %>% 
  select(date, 
         reporting_organization = report_org_code, 
         implementing_partner = org_code, 
         state, 
         township,  
         location, 
         admin3_pcode, 
         admin4_pcode, 
         activity_red, 
         delivery_modality, 
         beneficiaries) %>% 
  rbind(
    fsc %>% 
      select(date, 
             reporting_organization, 
             implementing_partner, 
             state, 
             township, 
             location, 
             admin3_pcode, 
             admin4_pcode, 
             activity_red, 
             delivery_modality,
             beneficiaries)) %>% 
  filter(delivery_modality %in% c("In-kind", 
                                  "Service delivery/support", 
                                  "Voucher", 
                                  "Hybrid (In-kind & CBT/CVA)", 
                                  "Hybrid (In-kind & Cash)")) %>%
  filter(activity_red %in% c("food distribution", 
                             "food_cash for work_assets", 
                             "HEB and fortfied rice")) %>% 
  group_by(state, township, admin3_pcode, location) %>% 
  slice(which.max(beneficiaries)) %>% 
  group_by(date) %>% 
  summarise(beneficiaries = sum(beneficiaries)) %>% 
  arrange(date) %>% 
  mutate(beneficiaries = cumsum(beneficiaries)) %>% 
    # write_csv("./data/mm21c_2021_2022.csv") 
  flextable() %>% 
  set_caption("Number of persons reached per month MM-2.1.c option A") %>% 
  theme_booktabs() %>% 
  set_table_properties(layout = "autofit") %>% 
  save_as_image(path = "./plots/option_a.png")


# Option B
fsc_2021 %>% 
  select(date, 
         reporting_organization = report_org_code, 
         implementing_partner = org_code, 
         state, 
         township,  
         location, 
         admin3_pcode, 
         admin4_pcode, 
         activity_red, 
         delivery_modality, 
         beneficiaries, 
         unique_beneficiaries) %>%
  mutate(new_beneficiaries = ifelse(
    unique_beneficiaries == "Yes",
    beneficiaries, 
    0)) %>%  
  select(-beneficiaries, -unique_beneficiaries) %>% 
  rbind(
    fsc %>% 
      select(date, 
             reporting_organization, 
             implementing_partner, 
             state, 
             township, 
             location, 
             admin3_pcode, 
             admin4_pcode, 
             activity_red, 
             delivery_modality,
             new_beneficiaries)) %>% 
  filter(delivery_modality %in% c("In-kind", 
                                  "Service delivery/support", 
                                  "Voucher", 
                                  "Hybrid (In-kind & CBT/CVA)", 
                                  "Hybrid (In-kind & Cash)")) %>%
  filter(activity_red %in% c("food distribution", 
                             "food_cash for work_assets", 
                             "HEB and fortfied rice")) %>% 
  mutate(year = as.character(year(date))) %>% 
  group_by(year, date) %>% 
  summarise(beneficiaries = sum(new_beneficiaries), 
            .groups = "drop") %>%
  arrange(date) %>% 
  mutate(beneficiaries = cumsum(beneficiaries)) %>%
  flextable() %>% 
  set_caption("Number of persons reached per month MM-2.1.C option B") %>% 
  theme_booktabs() %>% 
  set_table_properties(layout = "autofit") %>% 
  save_as_image(path = "./plots/option_b.png")
  

# Old script, just for reference to see the "only" 2022 calculation 
fsc %>%  
  filter(delivery_modality %in% c("In-kind", 
                                  "Hybrid")) %>%  
  filter(activity_red %in% c("food distribution", 
                             "food_cash for work_assets", 
                             "HEB and fortfied rice")) %>% 
  group_by(date) %>% 
  summarise(beneficiaries = sum(new_beneficiaries)) %>% 
  arrange(date) %>% 
  mutate(beneficiaries = cumsum(beneficiaries)) %>% 
  flextable() %>% 
  set_caption("Number of persons reached per month MM-2.1.C") %>% 
  theme_zebra() %>% 
  set_table_properties(layout = "autofit") 