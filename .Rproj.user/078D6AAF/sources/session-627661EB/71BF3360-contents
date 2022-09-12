fsc %>% 
  mutate(is_idp = ifelse(beneficiary_type == "Internally Displaced", 
                         TRUE, FALSE)) %>% 
  filter(activity_red == "food distribution") %>% 
  filter(!is.na(total_value_mmk)) %>%
  group_by(is_idp, quarter) %>% 
  summarise(frequencies = sum(beneficiaries), 
            total_value_mmk = sum(total_value_mmk)) %>% 
  mutate(unit_cost = total_value_mmk / frequencies) %>%
  # This is just to match the OCHA template
  pivot_wider(names_from = quarter, values_from = unit_cost) %>% 
  arrange(desc(is_idp))

fsc %>% 
  mutate(is_idp = ifelse(beneficiary_type == "Internally Displaced", 
                         TRUE, FALSE)) %>% 
  filter(strat_obj == "so_3") %>% 
  filter(!is.na(total_value_mmk)) %>%
  group_by(is_idp, quarter) %>% 
  summarise(frequencies = sum(beneficiaries), 
            total_value_mmk = sum(total_value_mmk)) %>% 
  mutate(unit_cost = total_value_mmk / frequencies) %>%
  # This is just to match the OCHA template
  pivot_wider(names_from = quarter, values_from = unit_cost) %>% 
  arrange(desc(is_idp))

fsc %>% group_by(strat_obj, activity_red) %>% 
  summarise(beneficiaries = sum(beneficiaries))
  summarise(total_value_usd = sum(total_value_usd, na.rm = TRUE))
  
  fsc %>% 
    mutate(is_idp = ifelse(beneficiary_type == "Internally Displaced", 
                           TRUE, FALSE)) %>% 
    filter(strat_obj == "so_3") %>% 
    group_by(beneficiary_type) %>% 
    summarise(frequencies = sum(beneficiaries))
