---
title: "Data Wrangling"
author: "Daniel Fuller"
date: "26/11/2021"
output:
      html_document:
        keep_md: true
---



# Data Wrangling for Sleep, PA, and Cancer study


```r
sleep_data_temp <- NULL

sleep_data_temp <- dir(path = "Participant Data/", full.names = TRUE, pattern = "*.csv", recursive = TRUE) %>%
   purrr::map(function(i){
     dfhx <- read_csv(i, col_types = cols(.default = col_character()))
     dfhx$p_id <- stringr::str_sub(i, 19, 21)
     dfhx$time <- stringr::str_sub(i, 23, 24)
     sleep_data_temp <- bind_rows(sleep_data_temp, dfhx)
   })
```

```
## New names:
## New names:
## New names:
## New names:
## New names:
## • `A_Sleep` -> `A_Sleep...6`
## • `A_Sleep` -> `A_Sleep...9`
```

```r
merged_data <- bind_rows(sleep_data_temp)

glimpse(merged_data)
```

```
## Rows: 2,979,315
## Columns: 21
## $ Date        <chr> "1/23/2017", "1/23/2017", "1/23/2017", "1/23/2017", "1/23/…
## $ Time        <chr> "15:07:00", "15:08:00", "15:09:00", "15:10:00", "15:11:00"…
## $ ZCM         <chr> "136", "66", "110", "246", "274", "256", "282", "278", "18…
## $ A_Sleep     <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"…
## $ TIB         <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"…
## $ SLP         <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"…
## $ M_Sleep     <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"…
## $ p_id        <chr> "001", "001", "001", "001", "001", "001", "001", "001", "0…
## $ time        <chr> "t1", "t1", "t1", "t1", "t1", "t1", "t1", "t1", "t1", "t1"…
## $ LZCM        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ PIM         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ Event       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ A_Sleep...6 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ A_Sleep...9 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ A_Sleep...5 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ A_Sleep...8 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ A_SLeep...5 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ A_SLeep...8 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ HPIM        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ Light       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ B_Sleep     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
```

```r
table(merged_data$p_id)
```

```
## 
##   001   002   003   004   005   006   007   008   009   010   012   014   015 
## 39445 29965 40365 40523 30297 40597 29042 38672 40422 19738 40290 42019 40804 
##   016   017   018   019   021   022   023   024   025   026   027   028   030 
## 20270 40183  9787 39752 40897 39648 20539 39864 39264 39148 38087 10684 40419 
##   031   033   034   035   036   037   038   039   040   041   042   043   044 
## 38733 38880 10155 19754 39961 38885 20476  9751 30379 31854 19518 10459 41535 
##   045   046   047   049   050   051   052   053   054   055   056   057   058 
## 30217 36344 30174 40690 41733 39771 30655 19881 10539 29470 40580 39850 30605 
##   059   060   061   063   064   065   066   067   068   069   070   071   072 
## 30914 29365 30540 31149 41565  9499 28627 10773 30315 39381  9796 29778 39975 
##   073   074   075   076   077   079   080   081   082   083   084   085   088 
## 42191 40328 40691 40792 29475 31441 41949 37585 39967 39764  9324  9815 28241 
##   089   091   092   093   095   096   097   098   099   100   101   102   104 
## 40137 30532 40807 10127 40109 29425 21263 36976 40758 30742 29189 19650 41084 
##   105   106   107   108 
## 10192 19902 40570 43042
```

```r
table(merged_data$time)
```

```
## 
##     t1     t2     t3     t4 
## 864539 766537 725228 623011
```

```r
rm(sleep_data_temp)
```

### Removing participants with bad data

Removed during data cleaning 
- 11, 13, 20, 29, 32, 78, 86, 87, 90, 94, 103

Removed because only provided baseline data
- 28, 34, 39, 43, 54, 65, 67, 70, 84, 87, 93


```r
merged_data <- filter(merged_data, p_id != "028" & 
                                    p_id != "034" & 
                                    p_id != "039" & 
                                    p_id != "043" & 
                                    p_id != "054" & 
                                    p_id != "065" & 
                                    p_id != "067" & 
                                    p_id != "070" & 
                                    p_id != "084" & 
                                    p_id != "093")

table(merged_data$p_id)
```

```
## 
##   001   002   003   004   005   006   007   008   009   010   012   014   015 
## 39445 29965 40365 40523 30297 40597 29042 38672 40422 19738 40290 42019 40804 
##   016   017   018   019   021   022   023   024   025   026   027   030   031 
## 20270 40183  9787 39752 40897 39648 20539 39864 39264 39148 38087 40419 38733 
##   033   035   036   037   038   040   041   042   044   045   046   047   049 
## 38880 19754 39961 38885 20476 30379 31854 19518 41535 30217 36344 30174 40690 
##   050   051   052   053   055   056   057   058   059   060   061   063   064 
## 41733 39771 30655 19881 29470 40580 39850 30605 30914 29365 30540 31149 41565 
##   066   068   069   071   072   073   074   075   076   077   079   080   081 
## 28627 30315 39381 29778 39975 42191 40328 40691 40792 29475 31441 41949 37585 
##   082   083   085   088   089   091   092   095   096   097   098   099   100 
## 39967 39764  9815 28241 40137 30532 40807 40109 29425 21263 36976 40758 30742 
##   101   102   104   105   106   107   108 
## 29189 19650 41084 10192 19902 40570 43042
```


### Combining the time variables and creating a week variable


```r
merged_data$date_time <- with(merged_data, mdy(Date) + hms(Time))

merged_data$week_year <- week(merged_data$date_time)
merged_data$day <- day(merged_data$date_time)
```

### Creating physical activity variables 

I'm using a generalization of method from this paper  

Moran DS, Heled Y, Gonzalez RR. Metabolic rate monitoring and energy expenditure prediction using a novel actigraphy method. Med Sci Monit. 2004 Nov;10(11):MT117-20. Epub 2004 Oct 26. PMID: 15507861. [https://pubmed.ncbi.nlm.nih.gov/15507861/](https://pubmed.ncbi.nlm.nih.gov/15507861/)

- ZCM of 40 or greater is moderate to vigorous activity
- ZCM of sedentary to light activity is ZCM greater than 0 less than 40 


```r
### Fix activity type for M_Sleep instead of ZCM sleep

merged_data <- merged_data %>% 
                mutate(activity_type_sleep = case_when(
                  M_Sleep == 1 ~ "sleep",
                  ZCM >= 45 ~ "mvpa",
                  ZCM < 45 ~ "sed_light"
                ))

table(merged_data$activity_type_sleep)
```

```
## 
##      mvpa sed_light     sleep 
##    252944   1825623    799641
```

```r
merged_data$M_Sleep <- as.numeric(merged_data$M_Sleep)
```

```
## Warning: NAs introduced by coercion
```

```r
merged_data$TIB <- as.numeric(merged_data$TIB)
merged_data$SLP <- as.numeric(merged_data$SLP)
merged_data$ZCM <- as.numeric(merged_data$ZCM)

merged_data$time <- str_replace_all(merged_data$time, "t", toupper)
```

## Creating total time of week for summary activities


```r
merged_data <- merged_data %>%
	mutate(sed_light_mvpa_minutes = case_when(
		activity_type_sleep == "sed_light" ~ 1,
		activity_type_sleep == "sleep" ~ 0,
		activity_type_sleep == "mvpa" ~ 1
	))

merged_data <- merged_data %>%
	mutate(mvpa_minutes = case_when(
		activity_type_sleep == "sed_light" ~ 0,
		activity_type_sleep == "sleep" ~ 0,
		activity_type_sleep == "mvpa" ~ 1
	))

merged_data <- merged_data %>%
	mutate(sed_light_minutes = case_when(
		activity_type_sleep == "sed_light" ~ 1,
		activity_type_sleep == "sleep" ~ 0,
		activity_type_sleep == "mvpa" ~ 0
	))

merged_data <- merged_data %>%
	mutate(sleep = case_when(
		activity_type_sleep == "sed_light" ~ 0,
		activity_type_sleep == "sleep" ~ 1,
		activity_type_sleep == "mvpa" ~ 0
	))
```

### Summary Statistics - Sleep


```r
merged_data$total_sleep_mvpa_light <- merged_data$sleep + merged_data$mvpa_minutes + merged_data$sed_light_minutes

table(merged_data$total_sleep_mvpa_light)
```

```
## 
##       1 
## 2878208
```

```r
summary(merged_data$total_sleep_mvpa_light)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       1       1       1       1       1       1
```

```r
merged_data$day <- as.factor(merged_data$day)

day <- merged_data %>%
  group_by(p_id, day, time) %>%
    summarise(sleep_minutes = sum(sleep),
              mvpa_minutes = sum(mvpa_minutes),
              sed_light_minutes = sum(sed_light_minutes),
              sed_light_mvpa_minutes = sum(sed_light_mvpa_minutes),
              total_sleep_mvpa_light = sum(total_sleep_mvpa_light),
              tib = sum(TIB),
              slp = sum(SLP),
              zcm = mean(ZCM),
              total_minutes = n()
              )
```

```
## `summarise()` has grouped output by 'p_id', 'day'. You can override using the
## `.groups` argument.
```

```r
day$zcm <- as.integer(day$zcm)
summary(day$total_minutes)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       1    1440    1440    1255    1440    1440
```

## Plot of ZCM sleep versus SLEEP1 variable


```r
summary(day$sleep_minutes)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0   297.2   385.5   348.6   448.0  1403.0
```

```r
sleep_plot <- ggplot(day, aes(x = sleep_minutes)) +
              geom_histogram() +
              theme_classic()
plot(sleep_plot)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](data_wrangling_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

### Summary stats for all variables 


```r
day %>%
  group_by(time) %>%
    get_summary_stats(sleep_minutes)
```

```
## # A tibble: 4 × 14
##   time  variable        n   min   max median    q1    q3   iqr   mad  mean    sd
##   <chr> <chr>       <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 T1    sleep_minu…   609     0  1403    393  297   453    156 110.   354.  179.
## 2 T2    sleep_minu…   613     0  1402    387  301   448    147  99.3  358.  177.
## 3 T3    sleep_minu…   577     0   618    380  277   447    170 114.   336.  156.
## 4 T4    sleep_minu…   495     0   607    385  300.  438.   139  96.4  344.  150.
## # … with 2 more variables: se <dbl>, ci <dbl>
```

```r
day %>%
  group_by(time) %>%
    get_summary_stats(mvpa_minutes)
```

```
## # A tibble: 4 × 14
##   time  variable        n   min   max median    q1    q3   iqr   mad  mean    sd
##   <chr> <chr>       <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 T1    mvpa_minut…   609     0   353    105    63   153    90  66.7  111.  64.4
## 2 T2    mvpa_minut…   613     0   340    104    61   153    92  65.2  111.  65.8
## 3 T3    mvpa_minut…   577     0   416    107    62   154    92  66.7  112.  65.8
## 4 T4    mvpa_minut…   495     0   305    100    64   141    77  57.8  106.  57.7
## # … with 2 more variables: se <dbl>, ci <dbl>
```

```r
day %>%
  group_by(time) %>%
    get_summary_stats(sed_light_minutes)
```

```
## # A tibble: 4 × 14
##   time  variable        n   min   max median    q1    q3   iqr   mad  mean    sd
##   <chr> <chr>       <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 T1    sed_light_…   609     0  1231    877  746    949  203   133.  789.  267.
## 2 T2    sed_light_…   613     1  1273    876  755    948  193   122.  781.  279.
## 3 T3    sed_light_…   577     1  1408    872  781    962  181   135.  808.  276.
## 4 T4    sed_light_…   495     2  1307    893  798.   950  152.  108.  808.  261.
## # … with 2 more variables: se <dbl>, ci <dbl>
```

### Checking daily MVPA and sleep overtime

### Plot for Sleep 

```r
sleep_plot <- ggplot(day, aes(x = day, y = sleep_minutes, group = time, colour = factor(time))) +
              geom_boxplot() +
              theme_classic()
plot(sleep_plot)
```

![](data_wrangling_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

### Plot for MVPA

```r
mvpa_plot <- ggplot(day, aes(x = day, y = mvpa_minutes, group = time, colour = factor(time))) +
              geom_boxplot() +
              theme_classic()
plot(mvpa_plot)
```

![](data_wrangling_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

### Plot for Sed, Light

```r
all_activity_plot <- ggplot(day, aes(x = day, y = sed_light_mvpa_minutes, group = time, colour = factor(time))) +
              geom_boxplot() +
              theme_classic()
plot(all_activity_plot)
```

![](data_wrangling_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

# Aggregating to the day level 


```r
day_summary <- day %>%
  group_by(p_id, time) %>%
    summarise(sleep_minutes = median(sleep_minutes, na.rm = TRUE),
              mvpa_minutes = mean(mvpa_minutes, na.rm = TRUE),
              sed_light_minutes = mean(sed_light_minutes, na.rm = TRUE),
              sed_light_mvpa_minutes = mean(sed_light_mvpa_minutes, na.rm = TRUE),
              total_sleep_mvpa_light = mean(total_sleep_mvpa_light, na.rm = TRUE),
              tib = sum(tib, na.rm = TRUE),
              slp = sum(slp, na.rm = TRUE),
              )
```

```
## `summarise()` has grouped output by 'p_id'. You can override using the
## `.groups` argument.
```

# Writing CSV files


```r
write_csv(day, "participant_days.csv")
write_csv(day_summary, "day_summary.csv")
```

### Reshaping SPSS data


```r
spss_data <- read_sav("Ryan Thesis Dataset.sav")

spss_data$T1_HVLTR_totalrecall <- spss_data$HVLTR_totalrecall
spss_data$T1_LNS <- spss_data$LNS

spss_data1 <- select(spss_data, ParticipantNumber, T1_FACTCog_TOTAL_Score, T2_FACTCog_TOTAL_Score, T3_FACTCog_TOTAL_Score, T4_FACTCog_TOTAL_Score, T1_HVLTR_totalrecall, T2_HVLTR_totalrecall, T3_HVLTR_totalrecall, T4_HVLTR_totalrecall, T1_MFSI_total, T2_MFSI_total, T3_MFSI_total, T4_MFSI_total, T1_LNS, T2_LNS, T3_LNS, T4_LNS, T1_HADS_depsum, T2_HADS_depsum, T3_HADS_depsum, T4_HADS_depsum, T1_HADS_anxsum, T2_HADS_anxsum, T3_HADS_anxsum, T4_HADS_anxsum)

spss_data_small <- spss_data1 %>% 
  pivot_longer(-ParticipantNumber) %>% 
  separate(name, into = c("time", "name"), sep = 3) %>%
  pivot_wider(values_fn=sum)

spss_data_small$time <- str_replace(spss_data_small$time, "_", "")
spss_data_small$p_id <- spss_data_small$ParticipantNumber

spss_data_small$p_id <- str_pad(spss_data_small$p_id, width=3, side="left", pad="0")
```

### Pulling demographic data


```r
spss_data_demo <- spss_data %>% select(1:85)

spss_data_demo$p_id <- spss_data_demo$ParticipantNumber
spss_data_demo$p_id <- str_pad(spss_data_demo$p_id, width=3, side="left", pad="0")
```

### Joining sleep and PA data to SPSS data


```r
spss_data_small$p_id <- as.character(spss_data_small$p_id)

data_joined <- day_summary %>% left_join(spss_data_small, by = c("p_id", "time"))

data <- data_joined %>% full_join(spss_data_demo, by = c("p_id"))

glimpse(data)
```

```
## Rows: 302
## Columns: 101
## Groups: p_id [100]
## $ p_id                             <chr> "001", "001", "001", "001", "002", "0…
## $ time                             <chr> "T1", "T2", "T3", "T4", "T2", "T3", "…
## $ sleep_minutes                    <dbl> 425.5, 319.0, 396.5, 327.5, 464.5, 47…
## $ mvpa_minutes                     <dbl> 114.12500, 155.12500, 138.87500, 132.…
## $ sed_light_minutes                <dbl> 722.2500, 785.8750, 762.8750, 773.375…
## $ sed_light_mvpa_minutes           <dbl> 836.375, 941.000, 901.750, 906.125, 8…
## $ total_sleep_mvpa_light           <dbl> 1213.000, 1239.875, 1261.125, 1216.62…
## $ tib                              <dbl> 4161, 3899, 4240, 4065, 4361, 4240, 4…
## $ slp                              <dbl> 3880, 3144, 3565, 3211, 3980, 3770, 3…
## $ ParticipantNumber.x              <dbl> 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4…
## $ FACTCog_TOTAL_Score              <dbl> 60, 40, 54, 40, 75, 88, 85, 71, 73, 9…
## $ HVLTR_totalrecall                <dbl> 33, 31, 27, 26, 27, 24, 26, 24, 25, 2…
## $ MFSI_total                       <dbl> 41, 47, 45, 28, 25, -4, -4, 46, 44, 1…
## $ LNS                              <dbl> 21, 18, 21, 23, 26, 29, 28, 24, 21, 2…
## $ HADS_depsum                      <dbl> 9, 10, 8, 8, 7, 1, 1, 9, 10, 9, 7, 5,…
## $ HADS_anxsum                      <dbl> 12, 11, 8, 8, 11, 8, 7, 15, 10, 12, 7…
## $ ParticipantNumber.y              <dbl> 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4…
## $ MMSEScore                        <dbl+lbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
## $ DOB                              <date> 1967-01-22, 1967-01-22, 1967-01-22, …
## $ Age_T1                           <dbl> 50, 50, 50, 50, 38, 38, 38, 56, 56, 5…
## $ age_binned                       <dbl+lbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ DOA1                             <date> 2017-01-23, 2017-01-23, 2017-01-23, …
## $ PostalCode                       <chr> "A1B 2G1", "A1B 2G1", "A1B 2G1", "A1B…
## $ Height                           <dbl> 69, 69, 69, 69, 63, 63, 63, 58, 58, 5…
## $ Weight                           <dbl> 170, 170, 170, 170, 152, 152, 152, 20…
## $ BMI                              <dbl> 25.17328, 25.17328, 25.17328, 25.1732…
## $ RelationshipStatus               <dbl+lbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
## $ relationshipstatus_binned        <dbl+lbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
## $ NumberChildren                   <dbl+lbl> 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, …
## $ children_binned                  <dbl+lbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
## $ Race                             <dbl+lbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ YearsEducation                   <dbl> 22, 22, 22, 22, 19, 19, 19, 16, 16, 1…
## $ education_binned                 <dbl+lbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
## $ EmploymentStatus                 <dbl+lbl> 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, …
## $ WorkHours                        <dbl> 45.0, 45.0, 45.0, 45.0, 50.0, 50.0, 5…
## $ DateCancerDiagnosis              <chr> "11/17/2016", "11/17/2016", "11/17/20…
## $ PastTreatments                   <chr> "surgery", "surgery", "surgery", "sur…
## $ PastTreatmentDate                <chr> "12/06/2016", "12/06/2016", "12/06/20…
## $ ComorbidIllness                  <chr> "MS, chronic gastrointestinal,depress…
## $ PrescriptionMed                  <chr> "pristiq 50mg, ranitidime 150mg", "pr…
## $ OTCMedications                   <chr> "tylenol, multivitamins", "tylenol, m…
## $ HMedication                      <chr> "none", "none", "none", "none", "none…
## $ SmokeStatus                      <dbl+lbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ NumberCigPerDay                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ Alcohol                          <dbl> 15, 15, 15, 15, 4, 4, 4, 0, 0, 0, 0, …
## $ PhysicalActivity                 <dbl> 9.50, 9.50, 9.50, 9.50, 2.75, 2.75, 2…
## $ T1_Caffeine                      <dbl> 0.0, 0.0, 0.0, 0.0, 4.5, 4.5, 4.5, 0.…
## $ DateCancerDiagnosis_CHARTS       <date> 2016-11-10, 2016-11-10, 2016-11-10, …
## $ AgeDiagnosis_CHARTS              <dbl> 49, 49, 49, 49, 38, 38, 38, 56, 56, 5…
## $ U65Diagnosis_CHARTS              <dbl+lbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
## $ Caucasian                        <dbl+lbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
## $ Good_BadSleep                    <dbl+lbl> 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, …
## $ Premenopausal_CHARTS             <dbl+lbl> 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, …
## $ CancerLeftRight_CHARTS           <dbl+lbl> 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, …
## $ PrognosticStage7_CHARTS          <chr> "IIB", "IIB", "IIB", "IIB", "IIA", "I…
## $ PrognosticStage8_CHARTS          <chr> "IB", "IB", "IB", "IB", "IA", "IA", "…
## $ RecurrenceScore_CHARTS           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
## $ TStage_CHARTS                    <chr> "pT2", "pT2", "pT2", "pT2", "pT1b", "…
## $ NStage_CHARTS                    <chr> "pN1a", "pN1a", "pN1a", "pN1a", "pN1a…
## $ Grade_CHARTS                     <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3…
## $ ERpos_CHARTS                     <dbl+lbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
## $ PRpos_CHARTS                     <dbl+lbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
## $ HER2pos_CHARTS                   <dbl+lbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ DateFirstSurgery_CHARTS          <date> 2016-12-06, 2016-12-06, 2016-12-06, …
## $ Lumpectomy_CHARTS                <dbl+lbl> 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, …
## $ SimpleMastectomy_CHARTS          <dbl+lbl> 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, …
## $ ModifiedRadicalMastectomy_CHARTS <dbl+lbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ SentinelLymphNodeBiopsy_CHARTS   <dbl+lbl> 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, …
## $ AuxillaryDissection_CHARTS       <dbl+lbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ SecondSurgeryReq_CHARTS          <dbl+lbl> 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, …
## $ Chemo_CHARTS                     <dbl+lbl> 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, …
## $ DateChemoStart_CHARTS            <date> 2017-01-27, 2017-01-27, 2017-01-27, …
## $ DateChemoEnd_CHARTS              <date> 2017-05-12, 2017-05-12, 2017-05-12, …
## $ TypeChemo_CHARTS                 <chr> "FEC-D", "FEC-D", "FEC-D", "FEC-D", "…
## $ DoseChemo_CHARTS                 <chr> "Full", "Full", "Full", "Full", "20% …
## $ Trastuzumab_CHARTS               <dbl+lbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ DateTrastuzumabStart_CHARTS      <date> NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ DateTrastuzumabEnd_CHARTS        <date> NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ Radiation_CHARTS                 <dbl+lbl> 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, …
## $ DateRadStart_CHARTS              <date> 2017-06-13, 2017-06-13, 2017-06-13, …
## $ DateRadEnd_CHARTS                <date> 2017-07-25, 2017-07-25, 2017-07-25, …
## $ TotalDoseRad_CHARTS              <dbl> 6000, 6000, 6000, 6000, 5000, 5000, 5…
## $ HormoneThearpy_CHARTS            <dbl+lbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
## $ TypeHormoneThearpy_CHARTS        <chr> "Tamoxifen", "Tamoxifen", "Tamoxifen"…
## $ DateHormoneStart_CHARTS          <date> 2017-06-15, 2017-06-15, 2017-06-15, …
## $ DateHormoneEnd_CHARTS            <date> NA, NA, NA, NA, 2018-08-15, 2018-08-…
## $ SecHormoneThearpy_CHARTS         <dbl+lbl> 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, …
## $ TypeSecHormoneThearpy_CHARTS     <chr> "", "", "", "", "Letrozole", "Letrozo…
## $ DateSecHormoneStart_CHARTS       <date> NA, NA, NA, NA, 2017-08-16, 2017-08-…
## $ DateSecHormoneEnd_CHARTS         <date> NA, NA, NA, NA, NA, NA, NA, 2017-07-…
## $ OtherHormoneTreatment_CHARTS     <dbl+lbl> 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, …
## $ DateOtherHormone_CHARTS          <date> NA, NA, NA, NA, 2017-07-06, 2017-07-…
## $ TypeOtherHormone_CHARTS          <chr> "", "", "", "", "Bilateral salpigo-oo…
## $ OtherTreatment_CHARTS            <dbl+lbl> 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, …
## $ DateOtherTreatment_CHARTS        <date> NA, NA, NA, NA, 2018-01-24, 2018-01-…
## $ TypeOtherTreatment_CHARTS        <chr> "", "", "", "", "Prophylactic right s…
## $ Notes                            <chr> "5000 cGy in 25# to left breast and a…
## $ HistologicType_CHARTS            <chr> "Ductal", "Ductal", "Ductal", "Ductal…
## $ Reconstruction_CHARTS            <dbl+lbl> 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, …
## $ TypePrimarySurg_CHARTS           <chr> "Left lumpectomy and sentinal lymph n…
## $ TypeOtherSurg_CHARTS             <chr> "", "", "", "", "Left Axillary Lymph …
```

## Write the analysis dataset


```r
write.csv(data, "analysis_data.csv")
```

