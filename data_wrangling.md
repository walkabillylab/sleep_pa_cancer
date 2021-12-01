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
excel_path <- list.files("/Users/dfuller/Documents/Ryan Collins Thesis/sleep_pa_cancer/Participant Data/", pattern = '*.xlsx', full.names = TRUE) %>%
    set_names()

### Time 1 
combine_excel_1 <- map_dfr(excel_path, ~read_excel(., sheet = "T1"), .id = "filename") 
combine_excel_1$wave <- 1
combine_excel_1 <- combine_excel_1 %>% 
                mutate(p_id = str_sub(filename, 80, 83),
                       time = str_sub(Time, 12, 20)
                )
combine_excel_1 <- combine_excel_1 %>% select(p_id, wave, Date, time, ZCM, SLEEP1)
glimpse(combine_excel_1)
```

```
## Rows: 3,144,011
## Columns: 6
## $ p_id   <chr> "P001", "P001", "P001", "P001", "P001", "P001", "P001", "P001",…
## $ wave   <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
## $ Date   <dttm> 2017-01-16, 2017-01-16, 2017-01-16, 2017-01-16, 2017-01-16, 20…
## $ time   <chr> "13:21:00", "13:22:00", "13:23:00", "13:24:00", "13:25:00", "13…
## $ ZCM    <dbl> 0, 268, 218, 326, 198, 280, 302, 342, 300, 288, 216, 120, 254, …
## $ SLEEP1 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
```

```r
participants_w1 <- table(combine_excel_1$p_id)
participants_w1
```

```
## 
##   P001   P002   P003   P004   P005   P006   P007   P008   P009   P010   P011 
##  44700  74848  33170  31809  30151  30113  57330  25949  47556  47530  16076 
##   P012   P013   P014   P015   P016   P017   P018   P019   P021   P022   P023 
##  30070  98232  31625  21600  35994  28735  24457  27446  30277  25901  21543 
##   P024   P025   P026   P027   P028   P029   P030   P031   P033   P034   P035 
##  25948  25926  39293  27520 194424  98232  18700  20316  24726  24729  26420 
##   P036   P037   P038   P039   P040   P041   P042   P043   P044   P045   P046 
##  23280  18903  39124  12933  25962  20247  30143  18591  49113  25552  34431 
##   P047   P048   P049   P050   P051   P052   P053   P054   P055   P056   P057 
##  20191  28710  53321  18855  20140  39293  20624  18801  27355  22617  29976 
##   P058   P059   P060   P061   P062   P063   P064   P065   P066   P067   P068 
##  31780  15837  31355  39293  35842  38906  20090  26148  49116  21359  15545 
##   P069   P070   P071   P072   P073   P074   P075   P076   P077   P079   P080 
##  17060  24448  49116  23078  43017  33008  18272  31601  21673  20123  24263 
##   P081   P082   P083   P084   P085   P086   P088   P089   P091   P092   P093 
##  18918  20207  36068  21586  32982  19875  38576  29020  26009  21735  20159 
##   P095   P096   P097   P098   P099   P100   P101   P102   P104   P105   P106 
##  31138  33226  23368  29014  18586  25746  18722  13489  21256   3110  17490 
##   P107   P108 
##  29057  24236
```

```r
colSums(is.na(combine_excel_1))
```

```
##    p_id    wave    Date    time     ZCM  SLEEP1 
##       0       0       0       0       0 2769877
```

```r
### Wave 2
combine_excel_2 <- map_dfr(excel_path, ~read_excel(., sheet = "T2"), .id = "filename") 
combine_excel_2$wave <- 2
combine_excel_2 <- combine_excel_2 %>% 
                mutate(p_id = str_sub(filename, 80, 83),
                       time = str_sub(Time, 12, 20)
                )
combine_excel_2 <- combine_excel_2 %>% select(p_id, wave, Date, time, ZCM, SLEEP1)
glimpse(combine_excel_2)
```

```
## Rows: 2,600,635
## Columns: 6
## $ p_id   <chr> "P001", "P001", "P001", "P001", "P001", "P001", "P001", "P001",…
## $ wave   <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, …
## $ Date   <dttm> 2017-05-29, 2017-05-29, 2017-05-29, 2017-05-29, 2017-05-29, 20…
## $ time   <chr> "09:31:00", "09:32:00", "09:33:00", "09:34:00", "09:35:00", "09…
## $ ZCM    <dbl> 0, 0, 22, 94, 36, 0, 0, 224, 288, 164, 266, 238, 278, 276, 32, …
## $ SLEEP1 <dbl> 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
```

```r
participants_w2 <- table(combine_excel_2$p_id)
participants_w2
```

```
## 
##  P001  P002  P003  P004  P005  P006  P008  P009  P010  P011  P012  P014  P015 
## 26109 33614 20316 28851 30365 34513 21519 39293 39293 28636 27277 44772 34705 
##  P017  P019  P021  P022  P023  P024  P025  P026  P027  P029  P030  P031  P033 
## 29037 28833 21562 44873 21680 44860 49220 38958 50552 98232 17335 21544 38896 
##  P035  P036  P037  P038  P040  P041  P042  P044  P045  P046  P047  P048  P049 
## 30285 23180 27313 39293 18658 21905 30104 28698 37248 27327 44658 18964 73359 
##  P050  P051  P052  P053  P055  P056  P057  P058  P059  P060  P061  P062  P064 
## 17622 16983 36075 24408 32764 15356 27314 40370 31644 31548 17386 38710 18839 
##  P066  P068  P069  P071  P072  P073  P074  P075  P076  P077  P079  P080  P081 
## 27556 33566 24508 31723 27383 39293 39293 20044 20033 28988 18893 16075 17387 
##  P082  P083  P085  P088  P089  P091  P092  P095  P096  P097  P098  P099  P100 
## 26065 39293 37353 24236 24270 26066 16990 22943 23140 30223 33206 20336 30210 
##  P101  P102  P104  P105  P106  P107  P108 
## 34310 39293 21831 39293 16114 34593 31272
```

```r
## Wave 3 
combine_excel_3 <- map_dfr(excel_path, ~read_excel(., sheet = "T3"), .id = "filename") 
combine_excel_3$wave <- 3
combine_excel_3 <- combine_excel_3 %>% 
                mutate(p_id = str_sub(filename, 80, 83),
                       time = str_sub(Time, 12, 20)
                )
combine_excel_3 <- combine_excel_3 %>% select(p_id, wave, Date, time, ZCM, SLEEP1)
glimpse(combine_excel_3)
```

```
## Rows: 2,362,878
## Columns: 6
## $ p_id   <chr> "P001", "P001", "P001", "P001", "P001", "P001", "P001", "P001",…
## $ wave   <dbl> 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, …
## $ Date   <dttm> 2017-09-20, 2017-09-20, 2017-09-20, 2017-09-20, 2017-09-20, 20…
## $ time   <chr> "08:52:00", "08:53:00", "08:54:00", "08:55:00", "08:56:00", "08…
## $ ZCM    <dbl> 0, 370, 226, 266, 154, 246, 174, 206, 264, 308, 280, 194, 214, …
## $ SLEEP1 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
```

```r
participants_w3 <- table(combine_excel_3$p_id)
participants_w3
```

```
## 
##  P001  P002  P003  P004  P005  P006  P007  P008  P009  P010  P011  P012  P014 
## 42039 33151 30405 26040 40329 19075 59146 20198 37234 20162 17484 33228 27372 
##  P015  P017  P019  P021  P022  P024  P025  P026  P027  P029  P030  P031  P033 
## 32094 38783 34542 23430 37449 28666 31573 30430 20107 49116 18947 19099 20124 
##  P035  P036  P037  P040  P041  P044  P045  P046  P047  P049  P050  P051  P052 
## 36086 46117 38758 59421 29739 30261 29998 29721 21175 39036 29057 19881 37603 
##  P053  P055  P056  P057  P058  P059  P060  P061  P062  P063  P064  P066  P068 
## 15959 39293 20142 23078 27035 30512 33078 16931 25698 31779 18755 22983 22094 
##  P069  P071  P072  P073  P074  P075  P076  P077  P079  P080  P081  P082  P083 
## 16116 27069 28953 39293 36209 28829 24691 39293 27425 27518 18489 20240 39293 
##  P085  P089  P092  P095  P096  P097  P098  P099  P100  P101  P102  P104  P105 
## 28808 21724 28785 28868 35411 29448 39293 31719 38783 18642  7942 12949 39293 
##  P107  P108 
## 33206 30176
```

```r
## Wave 4
combine_excel_4 <- map_dfr(excel_path, ~read_excel(., sheet = "T4"), .id = "filename") 
combine_excel_4$wave <- 4
combine_excel_4 <- combine_excel_4 %>% 
                mutate(p_id = str_sub(filename, 80, 83),
                       time = str_sub(Time, 12, 20)
                )
combine_excel_4 <- combine_excel_4 %>% select(p_id, wave, Date, time, ZCM, SLEEP1)
glimpse(combine_excel_4)
```

```
## Rows: 2,118,011
## Columns: 6
## $ p_id   <chr> "P001", "P001", "P001", "P001", "P001", "P001", "P001", "P001",…
## $ wave   <dbl> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, …
## $ Date   <dttm> 2018-01-09, 2018-01-09, 2018-01-09, 2018-01-09, 2018-01-09, 20…
## $ time   <chr> "15:36:00", "15:37:00", "15:38:00", "15:39:00", "15:40:00", "15…
## $ ZCM    <dbl> 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ SLEEP1 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
```

```r
participants_w4 <- table(combine_excel_4$p_id)
participants_w4
```

```
## 
##  P001  P002  P003  P004  P005  P006  P007  P008  P009  P010  P011  P012  P014 
## 38671 24271 31999 32032 25742 19075 53101 19697 47446 23579 12968 37280 45984 
##  P015  P016  P017  P019  P021  P022  P024  P025  P026  P027  P030  P031  P033 
## 31771 48565 24549 18725 21759 41575 23136 49116 23020 29799 18764 20427 26025 
##  P035  P036  P037  P040  P041  P044  P045  P046  P049  P050  P051  P052  P053 
## 27528 21408 14522 21151 30119 34589 30389 23175 30248 20188 20154  5030 20091 
##  P055  P056  P057  P058  P060  P061  P062  P063  P064  P066  P068  P069  P072 
## 39293 24663 20046 24373 34624 13032 39293 18495 18775 30348 15873 18663 30158 
##  P073  P074  P075  P076  P079  P080  P081  P082  P083  P085  P088  P089  P091 
## 17003 28833 39293 21419 31658 26048 31732 30084 34671 36088 39293 21690 35662 
##  P092  P095  P096  P097  P098  P099  P100  P101  P102  P104  P105  P107  P108 
## 20260 20230  4212 39293 39293 30082  2276 20184 32086 21680 39293 24270 16074
```

```r
merged_data <- bind_rows(combine_excel_1, combine_excel_2, combine_excel_3, combine_excel_4)

rm(combine_excel_1, combine_excel_2, combine_excel_3, combine_excel_4)

write_csv(merged_data, "/Users/dfuller/Documents/Ryan Collins Thesis/sleep_pa_cancer/merged_data.csv")
```

There are many many NAs in the sleep data. Not sure how this is calculated in the software or how we can extract this if we need it from the data we have. For example for Wave 1 we have 2769877 NAs for Sleep out of 3144011 data points. Many participants have no SLEEP1 data for entire wave. 

### Combining the time variables and creating a week variable


```r
merged_data$date_time <- with(merged_data, ymd(Date) + hms(time))
merged_data$date_time <- as.POSIXct(merged_data$date_time)

merged_data$week_year <- week(merged_data$date_time)
merged_data$day <- day(merged_data$date_time)
```

### Creating physical activity variables 

I'm using a generalization of method from this paper  

Moran DS, Heled Y, Gonzalez RR. Metabolic rate monitoring and energy expenditure prediction using a novel actigraphy method. Med Sci Monit. 2004 Nov;10(11):MT117-20. Epub 2004 Oct 26. PMID: 15507861. [https://pubmed.ncbi.nlm.nih.gov/15507861/](https://pubmed.ncbi.nlm.nih.gov/15507861/)

- ZCM of 40 or greater is moderate to vigorous activity
- ZCM of 0 is sleep (**This is probably WRONG**)
- ZCM of sedentary to light activity is ZCM greater than 0 less than 40 


```r
merged_data <- merged_data %>% 
                mutate(activity_type = case_when(
                  ZCM >= 40 ~ "MVPA",
                  ZCM == 0 ~ "Sleep",
                  ZCM > 0 & ZCM < 40 ~ "Sed_Light"
                ))

table(merged_data$activity_type)
```

```
## 
##      MVPA Sed_Light     Sleep 
##   2276515    742146   7206035
```

## Creating total time of week for summary activities


```r
merged_data <- merged_data %>%
	mutate(sed_light_mvpa_minutes = case_when(
		activity_type == "Sed_Light" ~ 1,
		activity_type == "Sleep" ~ 0,
		activity_type == "MVPA" ~ 1
	))

merged_data <- merged_data %>%
	mutate(mvpa_minutes = case_when(
		activity_type == "Sed_Light" ~ 0,
		activity_type == "Sleep" ~ 0,
		activity_type == "MVPA" ~ 1
	))

merged_data <- merged_data %>%
	mutate(sleep_minutes = case_when(
		activity_type == "Sed_Light" ~ 0,
		activity_type == "Sleep" ~ 1,
		activity_type == "MVPA" ~ 0
	))
```

### Summary Statistics - Sleep


```r
day <- merged_data %>%
  group_by(p_id, day, wave) %>%
    summarise(sleep_minutes = sum(sleep_minutes),
              mvpa_minutes = sum(mvpa_minutes),
              sed_light_mvpa_minutes = sum(sed_light_mvpa_minutes),
              zcm = mean(ZCM),
              total_minutes = n()
              )
```

```
## `summarise()` has grouped output by 'p_id', 'day'. You can override using the `.groups` argument.
```

```r
day$zcm <- as.integer(day$zcm)

summary(day$total_minutes)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      61    1440    1440    1447    1440    7200
```

```r
day <- filter(day, total_minutes <= 1440)


day %>%
  group_by(wave) %>%
    get_summary_stats(sleep_minutes)
```

```
## # A tibble: 4 x 14
##    wave variable        n   min   max median    q1    q3   iqr   mad  mean    sd
##   <dbl> <chr>       <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1     1 sleep_minu…  1885     0  1440   1108   391  1413  1022  492.  925.  491.
## 2     2 sleep_minu…  1749    25  1440   1210   400  1429  1029  341.  965.  493.
## 3     3 sleep_minu…  1657     2  1440   1219   405  1430  1025  328.  964.  495.
## 4     4 sleep_minu…  1505     0  1440   1106   382  1419  1037  495.  924.  502.
## # … with 2 more variables: se <dbl>, ci <dbl>
```

```r
day %>%
  group_by(wave) %>%
    get_summary_stats(mvpa_minutes)
```

```
## # A tibble: 4 x 14
##    wave variable        n   min   max median    q1    q3   iqr   mad  mean    sd
##   <dbl> <chr>       <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1     1 mvpa_minut…  1885     0  1440    107     3   741   738 159.   334.  389.
## 2     2 mvpa_minut…  1749     0  1125     68     1   748   747 101.   311.  386.
## 3     3 mvpa_minut…  1657     0  1430     60     0   757   757  89.0  313.  392.
## 4     4 mvpa_minut…  1505     0  1440     95     3   808   805 141.   345.  413.
## # … with 2 more variables: se <dbl>, ci <dbl>
```

```r
day %>%
  group_by(wave) %>%
    get_summary_stats(sed_light_mvpa_minutes)
```

```
## # A tibble: 4 x 14
##    wave variable        n   min   max median    q1    q3   iqr   mad  mean    sd
##   <dbl> <chr>       <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1     1 sed_light_…  1885     0  1440    175    15  1015  1000  259.  447.  490.
## 2     2 sed_light_…  1749     0  1405    116     9  1005   996  172.  414.  490.
## 3     3 sed_light_…  1657     0  1433    110     8   997   989  163.  413.  488.
## 4     4 sed_light_…  1505     0  1440    162    14  1038  1024  240.  448.  500.
## # … with 2 more variables: se <dbl>, ci <dbl>
```

There are around 270 cases where the total time is greater than 1440 minutes, which is impossible. This is likely caused by accelerometer error or excel data storage being silly at the end of the file. I will remove those days. Average sleep minutes are around 950 per day, meaning they are sleeping around 16 hours per day. Maybe they are sleeping this much. There are many days were pepole are in sleep the entire time. The MVPA per day is around 300 minutes and is very consistent across the waves. This is too much MVPA but the threshold we are using might be low as well. Sed, light and MVPA is around 400 minutes per day (6.6 hours). That is not unreasonable. 


### Checking daily MVPA and sleep overtime

### Plot for Sleep 

```r
sleep_plot <- ggplot(day, aes(x = day, y = sleep_minutes, group = wave, colour = factor(wave))) +
              geom_boxplot() +
              theme_classic()
plot(sleep_plot)
```

```
## Warning: Removed 2 rows containing non-finite values (stat_boxplot).
```

![](data_wrangling_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

### Plot for MVPA

```r
mvpa_plot <- ggplot(day, aes(x = day, y = mvpa_minutes, group = wave, colour = factor(wave))) +
              geom_boxplot() +
              theme_classic()
plot(mvpa_plot)
```

```
## Warning: Removed 2 rows containing non-finite values (stat_boxplot).
```

![](data_wrangling_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


### Plot for Sed, Light, MVPA

```r
all_activity_plot <- ggplot(day, aes(x = day, y = sed_light_mvpa_minutes, group = wave, colour = factor(wave))) +
              geom_boxplot() +
              theme_classic()
plot(all_activity_plot)
```

```
## Warning: Removed 2 rows containing non-finite values (stat_boxplot).
```

![](data_wrangling_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
