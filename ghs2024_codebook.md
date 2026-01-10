# GHS 2024 Household File - Codebook

**Generated:** 2026-01-10 18:49:10.511683

## Dataset Overview

- **File:** ghs-2024-hhold-v1.dta
- **Observations:** 20,940
- **Variables:** 190
- **Overall Completeness:** 100.00%

## Variable Health Summary

|health_status | n_variables| pct_of_total|
|:-------------|-----------:|------------:|
|Complete (0%) |         190|          100|

## Complete Variable List

| # | Variable | Label | Type | Missing (%) | Unique |
|---|----------|-------|------|-------------|--------|
| 1 | `uqnr` | Unique number | Character | 0.0% | 20940 |
| 2 | `personnr` | Person number | Character | 0.0% | 1 |
| 3 | `psu` | Primary sampling unit | Numeric | 0.0% | 3218 |
| 4 | `prov` | Province | Numeric (Labelled) | 0.0% | 9 |
| 5 | `head_popgrp` | Population group of household head | Numeric (Labelled) | 0.0% | 4 |
| 6 | `head_sex` | Sex of household head | Numeric (Labelled) | 0.0% | 2 |
| 7 | `head_age` | Age of household head | Numeric (Labelled) | 0.0% | 92 |
| 8 | `hsg_maind` | Type of main dwelling | Numeric (Labelled) | 0.0% | 12 |
| 9 | `hsg_walls` | Type of material used for the wall | Numeric (Labelled) | 0.0% | 13 |
| 10 | `hsg_roof` | Type of material used for the roof | Numeric (Labelled) | 0.0% | 13 |
| 11 | `hsg_floor` | Type of material used for the floor | Numeric (Labelled) | 0.0% | 9 |
| 12 | `hsg_cond_wall` | Walls condition | Numeric (Labelled) | 0.0% | 5 |
| 13 | `hsg_cond_roof` | Roof condition | Numeric (Labelled) | 0.0% | 6 |
| 14 | `hsg_cond_floor` | Floor condition | Numeric (Labelled) | 0.0% | 6 |
| 15 | `hsg_rm_tot` | Total rooms | Numeric (Labelled) | 0.0% | 26 |
| 16 | `hsg_tenure` | Ownership of dwelling | Numeric (Labelled) | 0.0% | 9 |
| 17 | `hsg_rent` | Monthly rent or mortgage | Numeric (Labelled) | 0.0% | 11 |
| 18 | `hsg_rdp` | RDP or state subsidised dwelling | Numeric (Labelled) | 0.0% | 4 |
| 19 | `hsg_subsidy` | House subsidy received | Numeric (Labelled) | 0.0% | 5 |
| 20 | `wat_drinkwat` | Main source of water | Numeric (Labelled) | 0.0% | 16 |
| 21 | `wat_dist` | Distance of water source from the dwelling | Numeric (Labelled) | 0.0% | 7 |
| 22 | `wat_time` | Time spent to fetch water | Numeric (Labelled) | 0.0% | 8 |
| 23 | `wat_pipe` | Usage of piped or tap water | Numeric (Labelled) | 0.0% | 4 |
| 24 | `WAT_QLT_Safe` | Safe to drink | Numeric (Labelled) | 0.0% | 3 |
| 25 | `WAT_QLT_Clear` | Clear | Numeric (Labelled) | 0.0% | 3 |
| 26 | `WAT_QLT_Taste` | Taste good | Numeric (Labelled) | 0.0% | 3 |
| 27 | `WAT_QLT_smell` | Free from any smells | Numeric (Labelled) | 0.0% | 3 |
| 28 | `wat_treat` | Treatment of drinking water | Numeric (Labelled) | 0.0% | 4 |
| 29 | `wat_mun_source` | Water supply | Numeric (Labelled) | 0.0% | 4 |
| 30 | `wat_meter` | Water meter | Numeric (Labelled) | 0.0% | 3 |
| 31 | `wat_pay` | Payment for water | Numeric (Labelled) | 0.0% | 3 |
| 32 | `wat_inte_12mth` | Water supply interruption | Numeric (Labelled) | 0.0% | 3 |
| 33 | `wat_inte_freq` | Frequency of water interruptions | Numeric (Labelled) | 0.0% | 6 |
| 34 | `wat_inte_2days` | Duration of water interruption | Numeric (Labelled) | 0.0% | 4 |
| 35 | `wat_inte_altsource` | Alternative drinking water sources | Numeric (Labelled) | 0.0% | 14 |
| 36 | `wat_inte_15days` | Total duration of interruption | Numeric (Labelled) | 0.0% | 4 |
| 37 | `wat_bottle` | Frequency of drinking bottled water at home | Numeric (Labelled) | 0.0% | 5 |
| 38 | `san_toil` | Type of toilet facility | Numeric (Labelled) | 0.0% | 13 |
| 39 | `san_share` | Toilet facility shared | Numeric (Labelled) | 0.0% | 4 |
| 40 | `san_location` | Location of the toilet facility | Numeric (Labelled) | 0.0% | 5 |
| 41 | `san_distance` | Distance to nearest toilet facility | Numeric (Labelled) | 0.0% | 6 |
| 42 | `san_pit_empty` | Emptied septic tank | Numeric (Labelled) | 0.0% | 4 |
| 43 | `san_pit_dispose` | Where contents of pit latrine emptied | Numeric (Labelled) | 0.0% | 8 |
| 44 | `san_washfac` | Hand washing facilities | Numeric (Labelled) | 0.0% | 5 |
| 45 | `san_wash` | Use of soap to wash hands after using toilet | Numeric (Labelled) | 0.0% | 7 |
| 46 | `eng_access` | Access to electricity | Numeric (Labelled) | 0.0% | 3 |
| 47 | `eng_mainelect` | Main source of electricity | Numeric (Labelled) | 0.0% | 7 |
| 48 | `eng_mains` | Connection to mains electricity supply | Numeric (Labelled) | 0.0% | 4 |
| 49 | `eng_supply` | Supplier of electricity | Numeric (Labelled) | 0.0% | 5 |
| 50 | `eng_cook` | Energy for cooking | Numeric (Labelled) | 0.0% | 9 |
| 51 | `eng_light` | Energy for lighting | Numeric (Labelled) | 0.0% | 7 |
| 52 | `eng_sheat` | Energy for space heating | Numeric (Labelled) | 0.0% | 9 |
| 53 | `eng_loadshed_freq` | Electrical outages during the previous week | Numeric (Labelled) | 0.0% | 6 |
| 54 | `eng_light_alt` | Alternative source of energy for lighting | Numeric (Labelled) | 0.0% | 11 |
| 55 | `eng_cook_alt` | Alternative source of energy for cooking | Numeric (Labelled) | 0.0% | 9 |
| 56 | `eng_pay` | paying for electricity | Numeric (Labelled) | 0.0% | 4 |
| 57 | `swr_rub` | Refuse removal | Numeric (Labelled) | 0.0% | 12 |
| 58 | `swr_burnwaste` | Burnt solid waste | Numeric (Labelled) | 0.0% | 8 |
| 59 | `swr_env_rub` | Irregular or no waste removal | Numeric (Labelled) | 0.0% | 3 |
| 60 | `swr_env_lit` | Littering | Numeric (Labelled) | 0.0% | 3 |
| 61 | `swr_env_wat` | Water pollution | Numeric (Labelled) | 0.0% | 3 |
| 62 | `swr_env_air` | Outdoor/indoor air pollution | Numeric (Labelled) | 0.0% | 3 |
| 63 | `swr_env_lan` | Land degradation | Numeric (Labelled) | 0.0% | 3 |
| 64 | `swr_env_noi` | Excessive noise | Numeric (Labelled) | 0.0% | 3 |
| 65 | `com_phone` | Telephone | Numeric (Labelled) | 0.0% | 3 |
| 66 | `com_cell` | Cellular telephone | Numeric (Labelled) | 0.0% | 2 |
| 67 | `com_int_fixed` | Internet connection in the household | Numeric (Labelled) | 0.0% | 3 |
| 68 | `com_int_mobile` | Any place via any other mobile access services | Numeric (Labelled) | 0.0% | 3 |
| 69 | `com_int_pubwifi` | public wifi | Numeric (Labelled) | 0.0% | 3 |
| 70 | `com_int_stud` | Internet for students at a school/university/co... | Numeric (Labelled) | 0.0% | 3 |
| 71 | `com_int_work` | At place of work | Numeric (Labelled) | 0.0% | 3 |
| 72 | `com_int_libr` | Internet in a library or community hall/Thusong... | Numeric (Labelled) | 0.0% | 3 |
| 73 | `com_int_cafe` | Internet Caf? | Numeric (Labelled) | 0.0% | 3 |
| 74 | `com_mail_post` | Mail carrier at home (postal delivery, mailman/... | Numeric (Labelled) | 0.0% | 3 |
| 75 | `com_mail_Pbag` | PO Box/ Private bag | Numeric (Labelled) | 0.0% | 3 |
| 76 | `com_mail_Postnet` | Postnet branch | Numeric (Labelled) | 0.0% | 3 |
| 77 | `com_mail_Other` | Other (e.g. local delivery office, school, shop) | Numeric (Labelled) | 0.0% | 3 |
| 78 | `tra_taxi` | Trips using taxis | Numeric (Labelled) | 0.0% | 3 |
| 79 | `tra_bus` | Trips using Buses | Numeric (Labelled) | 0.0% | 3 |
| 80 | `tra_train` | Trips Using Trains | Numeric (Labelled) | 0.0% | 3 |
| 81 | `tra_taxi_trips` | total number of trips using taxis | Numeric (Labelled) | 0.0% | 45 |
| 82 | `tra_taxi_cost` | Total cost of using taxis | Numeric (Labelled) | 0.0% | 324 |
| 83 | `tra_taxi_dist` | distance to the nearest taxi stop | Numeric (Labelled) | 0.0% | 26 |
| 84 | `tra_bus_trips` | total number of trips using buses | Numeric (Labelled) | 0.0% | 25 |
| 85 | `tra_bus_cost` | Total cost of using buses | Numeric (Labelled) | 0.0% | 154 |
| 86 | `tra_bus_dist` | distance to the nearest bus stop | Numeric (Labelled) | 0.0% | 18 |
| 87 | `tra_train_trips` | total number of trips using trains | Numeric (Labelled) | 0.0% | 15 |
| 88 | `tra_train_cost` | Total cost of using trains | Numeric (Labelled) | 0.0% | 41 |
| 89 | `tra_train_dist` | distance to the nearest train station | Numeric (Labelled) | 0.0% | 12 |
| 90 | `hhw_hltfac` | Medical help | Numeric (Labelled) | 0.0% | 14 |
| 91 | `hhw_transp` | Means of transport to nearest health facility | Numeric (Labelled) | 0.0% | 8 |
| 92 | `hhw_time` | Travel time to health facility | Numeric (Labelled) | 0.0% | 6 |
| 93 | `fsd_hung_adult` | Insufficient food for adult | Numeric (Labelled) | 0.0% | 7 |
| 94 | `fsd_hung_child` | Insufficient food for children | Numeric (Labelled) | 0.0% | 7 |
| 95 | `fsd_worried` | Worried about food | Numeric (Labelled) | 0.0% | 3 |
| 96 | `fsd_healthy` | Lack of nutritious food | Numeric (Labelled) | 0.0% | 3 |
| 97 | `fsd_fewfoods` | lack of variety food kind | Numeric (Labelled) | 0.0% | 3 |
| 98 | `fsd_fewfoods_days` | lacked variety food kind for 5 or more days in ... | Numeric (Labelled) | 0.0% | 3 |
| 99 | `fsd_skipped` | Skipped meals | Numeric (Labelled) | 0.0% | 3 |
| 100 | `fsd_skipped_days` | skipped meals for 5 or more days in the past 30... | Numeric (Labelled) | 0.0% | 3 |
| 101 | `fsd_ateless` | ate less food | Numeric (Labelled) | 0.0% | 3 |
| 102 | `fsd_ateless_days` | ate less food for 5 or more days in the past 30... | Numeric (Labelled) | 0.0% | 3 |
| 103 | `fsd_ranout` | run out of food | Numeric (Labelled) | 0.0% | 3 |
| 104 | `fsd_ranout_days` | run out of food for 5 or more days in the past ... | Numeric (Labelled) | 0.0% | 3 |
| 105 | `fsd_hungry` | Hunger | Numeric (Labelled) | 0.0% | 3 |
| 106 | `fsd_whlday` | Not eating the whole day | Numeric (Labelled) | 0.0% | 3 |
| 107 | `agr_agri` | Agricultural activities | Numeric (Labelled) | 0.0% | 2 |
| 108 | `agr_agrino` | Members involved in agricultural activities | Numeric (Labelled) | 0.0% | 10 |
| 109 | `agr_agrdecmaker` | Main decision maker | Numeric (Labelled) | 0.0% | 8 |
| 110 | `agr_agri_type_live` | Livestock production | Numeric (Labelled) | 0.0% | 3 |
| 111 | `agr_agri_type_poul` | Poultry production | Numeric (Labelled) | 0.0% | 3 |
| 112 | `agr_agri_type_grai` | Grains and food crops | Numeric (Labelled) | 0.0% | 3 |
| 113 | `agr_agri_type_indus` | Industrial crops | Numeric (Labelled) | 0.0% | 3 |
| 114 | `agr_agri_type_fruit` | Fruit and vegetable production | Numeric (Labelled) | 0.0% | 3 |
| 115 | `agr_agri_type_fod` | Fodder grazing/pasture or grass for animals | Numeric (Labelled) | 0.0% | 3 |
| 116 | `agr_agri_type_fish` | Fish farming/aquaculture | Numeric (Labelled) | 0.0% | 3 |
| 117 | `agr_agri_type_for` | Forestry | Numeric (Labelled) | 0.0% | 3 |
| 118 | `agr_agri_type_game` | Game farming | Numeric (Labelled) | 0.0% | 3 |
| 119 | `agr_mainuse` | Use of agricultural products and stock keeping | Numeric (Labelled) | 0.0% | 6 |
| 120 | `agr_plant_farm` | Farm land | Numeric (Labelled) | 0.0% | 3 |
| 121 | `agr_plant_back` | Backyard garden | Numeric (Labelled) | 0.0% | 3 |
| 122 | `agr_plant_schl` | School garden | Numeric (Labelled) | 0.0% | 3 |
| 123 | `agr_plant_comm` | Communal garden | Numeric (Labelled) | 0.0% | 3 |
| 124 | `agr_plant_vrd` | On verges of roads and unused public/municipal ... | Numeric (Labelled) | 0.0% | 3 |
| 125 | `agr_plant_oth` | Other crop planting activities | Numeric (Labelled) | 0.0% | 3 |
| 126 | `fin_inc_sal` | Salaries/wages/commission | Numeric (Labelled) | 0.0% | 2 |
| 127 | `fin_inc_buss` | Income from a business | Numeric (Labelled) | 0.0% | 2 |
| 128 | `fin_inc_rem` | Remittances | Numeric (Labelled) | 0.0% | 2 |
| 129 | `fin_inc_pen` | Pensions | Numeric (Labelled) | 0.0% | 2 |
| 130 | `fin_inc_grant` | Grants | Numeric (Labelled) | 0.0% | 2 |
| 131 | `fin_inc_agric` | Sales of farm products and services | Numeric (Labelled) | 0.0% | 2 |
| 132 | `fin_inc_none` | No Income | Numeric (Labelled) | 0.0% | 3 |
| 133 | `fin_inc_oth` | Other income sources, e.g. rental income, interest | Numeric (Labelled) | 0.0% | 2 |
| 134 | `fin_inc_main` | Main income source | Numeric (Labelled) | 0.0% | 9 |
| 135 | `fin_rem` | Approximately how much from remittances | Numeric (Labelled) | 0.0% | 83 |
| 136 | `fin_pen` | Approximately how much from pensions | Numeric (Labelled) | 0.0% | 115 |
| 137 | `fin_reqinc` | Net household income per month in rand | Numeric (Labelled) | 0.0% | 279 |
| 138 | `fin_compinc` | Monthly income of your household higher | Numeric (Labelled) | 0.0% | 6 |
| 139 | `fin_exp` | Household expenditure | Numeric (Labelled) | 0.0% | 15 |
| 140 | `hwl_vehicle` | Ownership of  a motor vehicle | Numeric (Labelled) | 0.0% | 3 |
| 141 | `hwl_totveh` | Total number of vehicles | Numeric (Labelled) | 0.0% | 12 |
| 142 | `hwl_domw` | Domestic or household worker's services used by... | Numeric (Labelled) | 0.0% | 3 |
| 143 | `hwl_status` | Wealth/poverty status of you and your household... | Numeric (Labelled) | 0.0% | 7 |
| 144 | `hwl_happy` | Happier, the same or less happy with life | Numeric (Labelled) | 0.0% | 6 |
| 145 | `hwl_assets_tv` | TV Set | Numeric (Labelled) | 0.0% | 3 |
| 146 | `hwl_assets_pool` | Swimming pool | Numeric (Labelled) | 0.0% | 3 |
| 147 | `hwl_assets_dvd` | DVD player/Blu-Ray player | Numeric (Labelled) | 0.0% | 3 |
| 148 | `hwl_assets_paytv` | Pay TV (M-Net/DSTV/Top TV) Subscription | Numeric (Labelled) | 0.0% | 3 |
| 149 | `hwl_assets_ac` | Air conditioner (excluding fans) | Numeric (Labelled) | 0.0% | 3 |
| 150 | `hwl_assets_comp` | Computer/Desktop/Laptop | Numeric (Labelled) | 0.0% | 3 |
| 151 | `hwl_assets_vac` | Vacuum cleaner | Numeric (Labelled) | 0.0% | 3 |
| 152 | `hwl_assets_dishw` | Dishwashing machine | Numeric (Labelled) | 0.0% | 3 |
| 153 | `hwl_assets_washm` | Washing machine | Numeric (Labelled) | 0.0% | 3 |
| 154 | `HWL_Assets_tumd` | Tumble dryer | Numeric (Labelled) | 0.0% | 3 |
| 155 | `hwl_assets_freezer` | Deep freezer ? free standing | Numeric (Labelled) | 0.0% | 3 |
| 156 | `hwl_assets_fridge` | Refrigerator or Combined Fridge Freezer | Numeric (Labelled) | 0.0% | 3 |
| 157 | `hwl_assets_estove` | Electric stove | Numeric (Labelled) | 0.0% | 3 |
| 158 | `hwl_assets_microw` | Microwave oven | Numeric (Labelled) | 0.0% | 3 |
| 159 | `hwl_assets_sink` | Built-in kitchen sink | Numeric (Labelled) | 0.0% | 3 |
| 160 | `hwl_assets_secure` | Home Security Service | Numeric (Labelled) | 0.0% | 3 |
| 161 | `hwl_assets_hometh` | Home theatre system | Numeric (Labelled) | 0.0% | 3 |
| 162 | `hwl_assets_geyser` | Geyser providing hot running water | Numeric (Labelled) | 0.0% | 3 |
| 163 | `hwl_assets_solarg` | Solar hot water geyser | Numeric (Labelled) | 0.0% | 3 |
| 164 | `hwl_assets_solarp` | Solar electrical panel(s) | Numeric (Labelled) | 0.0% | 3 |
| 165 | `hwl_assets_rainwtnk` | Rain water tank | Numeric (Labelled) | 0.0% | 3 |
| 166 | `hwl_assets_borehole` | Borehole | Numeric (Labelled) | 0.0% | 3 |
| 167 | `hwl_assets_radio` | Radio | Numeric (Labelled) | 0.0% | 3 |
| 168 | `hwl_assets_Gstove` | Gas stove | Numeric (Labelled) | 0.0% | 3 |
| 169 | `hwl_res_name` | Person who answered most of the questions in th... | Numeric (Labelled) | 0.0% | 14 |
| 170 | `hholdsz` | Household size | Numeric (Labelled) | 0.0% | 21 |
| 171 | `mobphon_hh` | Own a mobile telephone | Numeric (Labelled) | 0.0% | 15 |
| 172 | `smartphone_hh` | Own a smart phone | Numeric (Labelled) | 0.0% | 13 |
| 173 | `onemed_hh` | Medical aid scheme | Numeric (Labelled) | 0.0% | 11 |
| 174 | `chld5yr_hh` | Children 5 and younger | Numeric (Labelled) | 0.0% | 8 |
| 175 | `chld17yr_hh` | Children 17 and younger | Numeric (Labelled) | 0.0% | 12 |
| 176 | `ad60plusyr_hh` | Adults | Numeric (Labelled) | 0.0% | 6 |
| 177 | `lab_salary_hh` | Household Monthly salary | Numeric (Labelled) | 0.0% | 1330 |
| 178 | `soc_grant_hh` | Social grant | Numeric (Labelled) | 0.0% | 18 |
| 179 | `totalgrnt_hh` | Total social grant | Numeric (Labelled) | 0.0% | 289 |
| 180 | `undisab_hh` | UN Disability | Numeric (Labelled) | 0.0% | 6 |
| 181 | `disab_hh` | Disability | Numeric (Labelled) | 0.0% | 6 |
| 182 | `sevdisab_hh` | Severe disability | Numeric (Labelled) | 0.0% | 6 |
| 183 | `econact_hh` | Economically active | Numeric (Labelled) | 0.0% | 11 |
| 184 | `totmhinc` | Household income | Numeric (Labelled) | 0.0% | 3567 |
| 185 | `stratum` | Stratum | Character | 0.0% | 248 |
| 186 | `geotype` | Geographical type | Numeric (Labelled) | 0.0% | 3 |
| 187 | `metro` | Metro | Numeric (Labelled) | 0.0% | 2 |
| 188 | `metro_code` | Metro Code | Numeric (Labelled) | 0.0% | 17 |
| 189 | `rotation` | Rotation | Numeric (Labelled) | 0.0% | 4 |
| 190 | `house_wgt` | House Weight | Numeric | 0.0% | 4292 |
