fsc %>% select(date, state, township, admin3_pcode,
               reporting_organization, implementing_partner,
               activity = activity_red, 
               beneficiaries, new_beneficiaries) %>% 
  write_csv("./data/fsc_5w_sharing_220901.csv")



baseline_pin <- read_csv("./data/baseline_pin.csv") %>% clean_names()

baseline_pin %>%
  rename(idps = id_ps, 
         resettled = idp_returnees_resettled_locally_integrated_sep_2021, 
         stateless = non_displaced_stateless_people_in_rakhine_2021_hno, 
         vulnerable = number_of_most_vulnerable_crisis_affected_people_baseline) %>% 
  summarise_at(c("idps", 
                 "resettled", 
                 "stateless", 
                 "vulnerable"), 
               ~ sum(., na.rm = TRUE))%>% 
  mutate(other = resettled + stateless + vulnerable)


pin_2022 <- read_csv("./data/pin_2022_for_input.csv") %>%  
  clean_names()

pin_2022 <- pin_2022 %>% 
  select(township, 
         admin3 = township_pcode, 
         pin_idp = internally_displaced_people_idp_pi_n, 
         pin_returnees = id_ps_returnees_resettled_locally_integrated_pi_n, 
         pin_stateless = non_displaced_stateless_people_in_rakhine_pi_n, 
         pin_other = mostly_vulnerable_crisis_affected_people_pi_n, 
         pin_total = total_pi_n,
         pin_female_pc = female_percent_10,  
         pin_children_pc = children_age_18_percent_11, 
         pin_adults_pc = adults_age_18_60_percent_12, 
         pin_elderly_pc = elderly_age_60_percent_13, 
         pin_pwd_pc = people_with_disabilities_percent, 
         target_idp = internally_displaced_people_idp_target, 
         target_returnees = id_ps_returnees_resettled_locally_integrated_target, 
         target_stateless = non_displaced_stateless_people_in_rakhine_target, 
         target_other = mostly_vulnerable_crisis_affected_people_target, 
         target_total = total_target, 
         target_female_pc = female_percent_10,
         target_children_pc = children_age_18_percent_22, 
         target_adults_pc = adults_age_18_60_percent_23, 
         target_elderly_pc = elderly_age_60_percent_24,
         so1_target:other_vulnerable_so3) %>% 
  mutate(target_pwd_pc = pin_pwd_pc) %>% 
  mutate_at(vars(matches("_pc")), 
            ~ round(.x / 100, digits = 2)) %>% 
  rename(admin3_pcode = admin3) %>% 
  arrange(admin3_pcode) %>% 
  replace(is.na(.), 0) %>% 
  right_join(
    pin %>%
      select(state,
             admin1_pcode,
             admin3_pcode,
             conflict_score,
             mdp_adjust) %>%
      mutate_at(vars(conflict_score,
                     mdp_adjust),
                ~ scale(.x)) %>%
      mutate_at(vars(conflict_score,
                     mdp_adjust),
                ~ range_wna(.x)) %>%
      mutate(priority_score = (conflict_score + mdp_adjust) / 2) %>%
      unnest() %>% 
      arrange(admin3_pcode),
    by = "admin3_pcode"
  ) %>%
  do.call(data.frame, .) %>% 
  as_tibble()


pin_2022 %>% 
  group_by(state) %>% 
  summarise(target = sum(so3_target)) 

pin_2022 %>% 
  select(matches("so2|so3")) %>% 
  select(-contains("target")) %>% 
  summarise_all(sum) %>% 
  pivot_longer(cols = everything())

fsc %>%
  filter(!is.na(total_value_usd)) %>% 
  group_by(strat_obj) %>% 
  summarise(beneficiaries = sum(beneficiaries), 
            total_value_usd = sum(total_value_usd)) %>% 
  mutate(pc = total_value_usd / beneficiaries)

fsc %>% count(fsc_objectives, 
              hrp_indicator) %>% 
  print()
