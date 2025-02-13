
NEI <- NEI %>% mutate(across(c(2:50), ~ parse_number(.)))

NEI<-NEI  %>% mutate(
  #1
  if_you_had_perfect_vision_without_glasses_contact_lenses_or_any_other_type_of_vision_correction_how_different_would_your_life_be=recode(if_you_had_perfect_vision_without_glasses_contact_lenses_or_any_other_type_of_vision_correction_how_different_would_your_life_be, '1'='100', '2'='50','3'='0','4'='100'),
  #2
  how_much_difficulty_do_you_have_doing_work_or_hobbies_that_require_you_to_see_well_up_close_such_as_cooking_fixing_things_around_the_house_sewing_using_hand_tools_or_working_with_a_computer=recode(how_much_difficulty_do_you_have_doing_work_or_hobbies_that_require_you_to_see_well_up_close_such_as_cooking_fixing_things_around_the_house_sewing_using_hand_tools_or_working_with_a_computer, '1'='100', '2'='75','3'='50','4'='25','5'='0','6'='NA'),
  #3
  how_much_difficulty_do_you_have_seeing_because_of_changes_in_the_clarity_of_your_vision_over_the_course_of_the_day=recode(how_much_difficulty_do_you_have_seeing_because_of_changes_in_the_clarity_of_your_vision_over_the_course_of_the_day, '1'='100', '2'='100','3'='66.6','4'='33.3','5'='0'),
  #4
  how_much_difficulty_do_you_have_judging_distances_like_walking_downstairs_or_parking_a_car=recode(how_much_difficulty_do_you_have_judging_distances_like_walking_downstairs_or_parking_a_car, '1'='100', '2'='66.6','3'='33.3','4'='0'),
  #5
  how_much_difficulty_do_you_have_seeing_things_off_to_the_side_like_cars_coming_out_of_driveways_or_side_streets_or_people_coming_out_of_doorways=recode(how_much_difficulty_do_you_have_seeing_things_off_to_the_side_like_cars_coming_out_of_driveways_or_side_streets_or_people_coming_out_of_doorways, '1'='100', '2'='66.6','3'='33.3','4'='0'),
  #6
  how_much_difficulty_do_you_have_getting_used_to_the_dark_when_you_move_from_a_lighted_area_into_a_dark_place_like_walking_into_a_dark_movie_theater=recode(how_much_difficulty_do_you_have_getting_used_to_the_dark_when_you_move_from_a_lighted_area_into_a_dark_place_like_walking_into_a_dark_movie_theater, '1'='100', '2'='66.6','3'='33.3','4'='0'),
  #7
  how_much_difficulty_do_you_have_reading_ordinary_print_in_newspapers=recode(how_much_difficulty_do_you_have_reading_ordinary_print_in_newspapers, '1'='100', '2'='75','3'='50','4'='25','5'='0'),
  #8
  how_much_difficulty_do_you_have_reading_the_small_print_in_a_telephone_book_on_a_medicine_bottle_or_on_legal_forms=recode(how_much_difficulty_do_you_have_reading_the_small_print_in_a_telephone_book_on_a_medicine_bottle_or_on_legal_forms, '1'='100', '2'='75','3'='50','4'='25','5'='0'),
  #9
  how_much_difficulty_do_you_have_driving_at_night=recode(how_much_difficulty_do_you_have_driving_at_night, '1'='100', '2'='75','3'='50','4'='25','5'='0','6'='NA'),
  #10
  how_much_difficulty_do_you_have_driving_in_difficult_conditions_such_as_in_bad_weather_during_rush_hour_on_the_freeway_or_in_city_traffic=recode(how_much_difficulty_do_you_have_driving_in_difficult_conditions_such_as_in_bad_weather_during_rush_hour_on_the_freeway_or_in_city_traffic, '1'='100', '2'='75','3'='50','4'='25','5'='0','6'='NA'),
  #11
  because_of_your_eyesight_how_much_difficulty_do_you_have_with_your_daily_activities=recode(because_of_your_eyesight_how_much_difficulty_do_you_have_with_your_daily_activities, '1'='100', '2'='66.6','3'='33.3','4'='0'),
  #12
  because_of_your_eyesight_how_much_difficulty_do_you_have_taking_part_in_active_sports_or_other_outdoor_activities_that_you_enjoy_like_hiking_swimming_aerobics_team_sports_or_jogging=recode(because_of_your_eyesight_how_much_difficulty_do_you_have_taking_part_in_active_sports_or_other_outdoor_activities_that_you_enjoy_like_hiking_swimming_aerobics_team_sports_or_jogging,  '1'='100', '2'='75','3'='50','4'='25','5'='0','6'='NA'),
  #13
  do_you_need_to_wear_glasses_or_bi_focal_lenses_or_use_a_magnifier_when_you_are_reading_something_brief_like_directions_a_menu_or_a_recipe=recode(do_you_need_to_wear_glasses_or_bi_focal_lenses_or_use_a_magnifier_when_you_are_reading_something_brief_like_directions_a_menu_or_a_recipe, '1'='0', '2'='50','3'='100'),
  #14
  do_you_need_to_wear_glasses_or_bi_focal_lenses_or_use_a_magnifier_when_you_are_reading_something_long_like_a_book_a_magazine_article_or_the_newspaper=recode(do_you_need_to_wear_glasses_or_bi_focal_lenses_or_use_a_magnifier_when_you_are_reading_something_long_like_a_book_a_magazine_article_or_the_newspaper,'1'='0', '2'='50','3'='100'),
  #15
  when_driving_at_night_do_you_need_to_wear_glasses_or_contacts=recode(when_driving_at_night_do_you_need_to_wear_glasses_or_contacts, '1'='33.3', '2'='66.6','3'='100','4'='0', '5'='NA'),
  #16
  at_dusk_when_it_is_just_starting_to_get_dark_do_you_need_to_wear_glasses_or_contacts_for_driving=recode(at_dusk_when_it_is_just_starting_to_get_dark_do_you_need_to_wear_glasses_or_contacts_for_driving, '1'='33.3', '2'='66.6','3'='100','4'='0', '5'='NA'),
  #17
  how_often_when_you_are_around_bright_lights_at_night_do_you_see_starbursts_or_halos_that_bother_you_or_make_it_difficult_to_see=recode(how_often_when_you_are_around_bright_lights_at_night_do_you_see_starbursts_or_halos_that_bother_you_or_make_it_difficult_to_see, '1'='0', '2'='25','3'='50','4'='75','5'='100'),
  #18
  how_often_do_you_experience_pain_or_discomfort_in_and_around_your_eyes_for_example_burning_itching_or_aching=recode(how_often_do_you_experience_pain_or_discomfort_in_and_around_your_eyes_for_example_burning_itching_or_aching, '1'='0', '2'='25','3'='50','4'='75','5'='100'),
  #19
  how_much_does_dryness_in_your_eyes_bother_you=recode(how_much_does_dryness_in_your_eyes_bother_you, '1'='100', '2'='100','3'='75','4'='50','5'='25','6'='0'),
  #20
  how_often_are_you_bothered_by_changes_in_the_clarity_of_your_vision_over_the_course_of_the_day=recode(how_often_are_you_bothered_by_changes_in_the_clarity_of_your_vision_over_the_course_of_the_day, '1'='100', '2'='75','3'='50','4'='25','5'='0'),
  #21
  how_often_do_you_worry_about_your_eyesight_or_vision=recode(how_often_do_you_worry_about_your_eyesight_or_vision, '1'='100', '2'='75','3'='50','4'='25','5'='0'),
  #22
  how_often_do_you_notice_or_think_about_your_eyesight_or_vision=recode(how_often_do_you_notice_or_think_about_your_eyesight_or_vision, '1'='100', '2'='75','3'='50','4'='25','5'='0'),
  #23
  at_this_time_how_clear_is_your_vision_using_the_correction_you_normally_use_including_glasses_contact_lenses_a_magnifier_surgery_or_nothing_at_all=recode(at_this_time_how_clear_is_your_vision_using_the_correction_you_normally_use_including_glasses_contact_lenses_a_magnifier_surgery_or_nothing_at_all, '1'='100', '2'='66.6','3'='33.3','4'='0'),
  #24
  how_much_pain_or_discomfort_do_you_have_in_and_around_your_eyes_for_example_burning_itching_or_aching=recode(how_much_pain_or_discomfort_do_you_have_in_and_around_your_eyes_for_example_burning_itching_or_aching, '1'='100', '2'='75','3'='50','4'='25','5'='0'),
  #25
  how_often_do_you_have_headaches_that_you_think_are_related_to_your_vision_or_vision_correction=recode(how_often_do_you_have_headaches_that_you_think_are_related_to_your_vision_or_vision_correction, '1'='100', '2'='75','3'='50','4'='25','5'='0'),
  #26
  how_satisfied_are_you_with_the_glasses_contact_lenses_magnifier_or_other_type_of_correction_including_surgery_you_have=recode(how_satisfied_are_you_with_the_glasses_contact_lenses_magnifier_or_other_type_of_correction_including_surgery_you_have, '1'='100', '2'='80','3'='60','4'='40','5'='20','6'='0'),
  #27
  in_terms_of_your_appearance_how_satisfied_are_you_with_the_glasses_contact_lenses_magnifier_or_other_type_of_correction_including_surgery_you_have=recode(in_terms_of_your_appearance_how_satisfied_are_you_with_the_glasses_contact_lenses_magnifier_or_other_type_of_correction_including_surgery_you_have, '1'='100', '2'='80','3'='60','4'='40','5'='20','6'='0'),
  #28
  if_you_had_perfect_vision_without_glasses_contacts_or_any_other_type_of_vision_correction_how_much_do_you_think_your_life_would_change=recode(if_you_had_perfect_vision_without_glasses_contacts_or_any_other_type_of_vision_correction_how_much_do_you_think_your_life_would_change, '1'='100', '2'='50','3'='0','4'='100'),
  #29
  in_terms_of_your_appearance_is_the_type_of_vision_correction_you_have_now_the_best_you_have_ever_had=recode(in_terms_of_your_appearance_is_the_type_of_vision_correction_you_have_now_the_best_you_have_ever_had, '1'='100', '2'='0'),
  #30
  in_terms_of_your_appearance_is_there_a_type_of_vision_correction_that_is_better_than_what_you_have_now=recode(in_terms_of_your_appearance_is_there_a_type_of_vision_correction_that_is_better_than_what_you_have_now, '1'='0', '2'='100'),
  #31
  how_often_did_you_use_a_type_of_correction_or_treatment_that_was_uncomfortable_in_the_last_4_weeks_because_it_made_you_look_better=recode(how_often_did_you_use_a_type_of_correction_or_treatment_that_was_uncomfortable_in_the_last_4_weeks_because_it_made_you_look_better, '1'='0', '2'='25','3'='50','4'='75','5'='100'),
  #32
  how_often_did_you_use_a_type_of_correction_that_did_not_correct_your_vision_as_well_as_another_correction_would_have_in_the_last_4_weeks_because_it_made_you_look_better=recode(how_often_did_you_use_a_type_of_correction_that_did_not_correct_your_vision_as_well_as_another_correction_would_have_in_the_last_4_weeks_because_it_made_you_look_better, '1'='0', '2'='25','3'='50','4'='75','5'='100'),
  #33
  because_of_your_vision_do_you_take_part_less_than_you_would_like_in_active_sports_or_other_outdoor_activities_like_hiking_swimming_aerobics_team_sports_or_jogging=recode(because_of_your_vision_do_you_take_part_less_than_you_would_like_in_active_sports_or_other_outdoor_activities_like_hiking_swimming_aerobics_team_sports_or_jogging, '1'='0', '2'='100'),
  #34
  are_there_any_recreational_or_sports_activities_that_you_don_t_do_because_of_your_eyesight_or_the_type_of_vision_correction_you_have=recode(are_there_any_recreational_or_sports_activities_that_you_don_t_do_because_of_your_eyesight_or_the_type_of_vision_correction_you_have, '1'='0', '2'='50','3'='100'),
  #35
  are_there_daily_activities_that_you_would_like_to_do_but_don_t_do_because_of_your_vision_or_the_type_of_vision_correction_you_have=recode(are_there_daily_activities_that_you_would_like_to_do_but_don_t_do_because_of_your_vision_or_the_type_of_vision_correction_you_have, '1'='0', '2'='50','3'='100'),
  #36
  tearing=recode(tearing, '1'='NA', '2'='100'),
  #36
  if_yes_how_bothersome_has_it_been=recode(if_yes_how_bothersome_has_it_been, '1'='0', '2'='25','3'='50','4'='75'),
  #37
  distorted_vision=recode(distorted_vision, '1'='NA', '2'='100'),
  #37
  if_yes_how_bothersome_has_it_been_2=recode(if_yes_how_bothersome_has_it_been_2, '1'='0', '2'='25','3'='50','4'='75'),
  #38
  glare=recode(glare, '1'='NA', '2'='100'),
  #38
  if_yes_how_bothersome_has_it_been_3=recode(if_yes_how_bothersome_has_it_been_3, '1'='0', '2'='25','3'='50','4'='75'),
  #39
  blurry_vision_with_your_eyesight_or_the_type_of_vision_correction_you_use=recode(blurry_vision_with_your_eyesight_or_the_type_of_vision_correction_you_use, '1'='NA', '2'='100'),
  #39
  if_yes_how_bothersome_has_it_been_4=recode(if_yes_how_bothersome_has_it_been_4, '1'='0', '2'='25','3'='50','4'='75'),
  #40
  trouble_seeing=recode(trouble_seeing, '1'='NA', '2'='100'),
  #40
  if_yes_how_bothersome_has_it_been_5=recode(if_yes_how_bothersome_has_it_been_5, '1'='0', '2'='25','3'='50','4'='75'),
  #41
  itching_in_or_around_your_eyes=recode(itching_in_or_around_your_eyes, '1'='NA', '2'='100'),
  #41
  if_yes_how_bothersome_has_it_been_6=recode(if_yes_how_bothersome_has_it_been_6, '1'='0', '2'='25','3'='50','4'='75'),
  #42
  soreness_or_tiredness_in_your_eyes=recode(soreness_or_tiredness_in_your_eyes, '1'='NA', '2'='100'),
  #42
  if_yes_how_bothersome_has_it_been_7=recode(if_yes_how_bothersome_has_it_been_7, '1'='0', '2'='25','3'='50','4'='75')
  )

NEI[,2:50]<- sapply(NEI[,2:50],as.numeric)

NEI<- NEI%>%
    mutate(Clarity = select(., c(24,38,39:40,43:46)) %>% rowSums(na.rm = TRUE),
           Expectations = select(., c(2,29)) %>% rowSums(na.rm = TRUE),
           Near = select(., c(3,8,9,12)) %>% rowSums(na.rm = TRUE),
           Far = select(., c(5,6,7,10,11)) %>% rowSums(na.rm = TRUE),
           Fluctuations = select(., c(4,21)) %>% rowSums(na.rm = TRUE),
           Limitations = select(., c(13,34:36)) %>% rowSums(na.rm = TRUE),
          Glare = select(., c(18,41:42)) %>% rowSums(na.rm = TRUE),
           Symptoms = select(., c(19,20,25,26,37:38,47:50)) %>% rowSums(na.rm = TRUE),
            Dependence = select(., c(14:17)) %>% rowSums(na.rm = TRUE),
          Worry = select(., c(22,23)) %>% rowSums(na.rm = TRUE),
          Supoptimal = select(., c(32,33)) %>% rowSums(na.rm = TRUE),
          Appearance = select(., c(28,30,31)) %>% rowSums(na.rm = TRUE),
          Satisfaction = select(., c(27)) %>% rowSums(na.rm = TRUE),
          )
