Set working directory and load packages

    # set working directory
    setwd("C:/Users/askes/OneDrive/Skrivebord/SocKul - Exam/data/")

    # packages 
    library(pacman)
    pacman::p_load(tidyverse, patchwork)

    # import data
    Sac <- read_csv("SaccadesDV.csv")

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   Blink = col_logical(),
    ##   Direction = col_character(),
    ##   Eye = col_character(),
    ##   CURRENT_SAC_MSG_TEXT_1 = col_character(),
    ##   Message = col_character(),
    ##   Event = col_character(),
    ##   PreviousRating = col_logical(),
    ##   PreviousGroupRating = col_logical()
    ## )

    ## See spec(...) for full column specifications.

    ## Warning: 295851 parsing failures.
    ##    row                 col           expected actual             file
    ## 247203 PreviousRating      1/0/T/F/TRUE/FALSE      3 'SaccadesDV.csv'
    ## 247203 PreviousGroupRating 1/0/T/F/TRUE/FALSE      3 'SaccadesDV.csv'
    ## 247204 PreviousRating      1/0/T/F/TRUE/FALSE      3 'SaccadesDV.csv'
    ## 247204 PreviousGroupRating 1/0/T/F/TRUE/FALSE      3 'SaccadesDV.csv'
    ## 247205 PreviousRating      1/0/T/F/TRUE/FALSE      3 'SaccadesDV.csv'
    ## ...... ................... .................. ...... ................
    ## See problems(...) for more details.

    Fix <- read_csv("FixationsDV.csv")

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   Eye = col_character(),
    ##   CURRENT_FIX_MSG_TEXT_1 = col_character(),
    ##   Event = col_character(),
    ##   AOI = col_character(),
    ##   PreviousRating = col_logical(),
    ##   PreviousGroupRating = col_logical(),
    ##   Message = col_character()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: 312764 parsing failures.
    ##    row                 col           expected actual              file
    ## 255913 PreviousRating      1/0/T/F/TRUE/FALSE      3 'FixationsDV.csv'
    ## 255913 PreviousGroupRating 1/0/T/F/TRUE/FALSE      3 'FixationsDV.csv'
    ## 255914 PreviousRating      1/0/T/F/TRUE/FALSE      3 'FixationsDV.csv'
    ## 255914 PreviousGroupRating 1/0/T/F/TRUE/FALSE      3 'FixationsDV.csv'
    ## 255915 PreviousRating      1/0/T/F/TRUE/FALSE      3 'FixationsDV.csv'
    ## ...... ................... .................. ...... .................
    ## See problems(...) for more details.

    Calibration <- read.csv("Calibration.csv", sep = " ") # ?
    AOI <- read_csv("AoI_Coordinates.csv")

    ## Parsed with column specification:
    ## cols(
    ##   Trial = col_character(),
    ##   Left = col_double(),
    ##   Top = col_double(),
    ##   Right = col_double(),
    ##   Bottom = col_double(),
    ##   Event = col_character()
    ## )

    # RAW + create placeholder for raw data
    #Raw_placeholder <- read_csv("Samples.csv")
    #Raw <- Raw_placeholder

Explore
=======

    # colnames in raw data
    #colnames(Raw)
    unique(Fix$AOI)

    ##  [1] "5"             "Face"          NA              "FixationCross"
    ##  [5] "6"             "4"             "7"             "8"            
    ##  [9] "2"             "1"             "3"             "Scale"        
    ## [13] "Text"

    # explore some columns
    summary(Fix$PreviousRating)

    ##    Mode    TRUE    NA's 
    ## logical    5424  415077

    summary(Fix$PreviousGroupRating)

    ##    Mode    TRUE    NA's 
    ## logical   10990  409511

    summary(Sac$CurrentRating)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    1.00    4.00    5.00    4.89    6.00    8.00      17

    unique(Sac$CurrentRating)

    ## [1]  1  5  6  4  2  3  7  8 NA

    # There is a little difference between the sum of durations and the total reaction time - could be due to saccades.
    Fix %>% filter(ID == 101 & Session == 1 & Trial == 1) %>% 
      summarise(
        sum_RT = sum(Duration),
        RT = max(RT)
    )

    ## # A tibble: 1 x 2
    ##   sum_RT    RT
    ##    <dbl> <dbl>
    ## 1  17894 18573

    # Explore the data

    Fix %>% filter(ID == 101) %>% 
      group_by(Session, Trial) %>% 
      summarise(
        fixations = max(FixationN),
        max_pupil = max(PupilSize),
        min_pupil = min(PupilSize),
        mean_pupil = mean(PupilSize),
        mean_dur_ms = mean(Duration),
        max_dur_ms = max(Duration),
        group = Group[1]
        
    )

    ## # A tibble: 306 x 9
    ## # Groups:   Session [2]
    ##    Session Trial fixations max_pupil min_pupil mean_pupil mean_dur_ms
    ##      <dbl> <dbl>     <dbl>     <dbl>     <dbl>      <dbl>       <dbl>
    ##  1       1     1        59     47100     30500     37764.        303.
    ##  2       1     2        29     45600     34200     39366.        389.
    ##  3       1     3        20     38700     33800     36850         438.
    ##  4       1     4        13     42900     36600     39646.        571.
    ##  5       1     5        12     44000     38200     40758.        484.
    ##  6       1     6        28     39400     28700     36479.        321.
    ##  7       1     7        23     43600     34100     39474.        323.
    ##  8       1     8        14     39000     31900     35329.        597.
    ##  9       1     9        25     42500     32700     37700         423.
    ## 10       1    10        20     47600     35600     42065         471.
    ## # ... with 296 more rows, and 2 more variables: max_dur_ms <dbl>,
    ## #   group <dbl>

    # mean number of fixations for each group
    Fix %>% 
      group_by(Group,ID) %>% 
      summarise(
        max_fix_ID = max(FixationN)) %>% 
      
      group_by(Group) %>% 
      summarise(
        Max_mean_nr_fix = mean(max_fix_ID))

    ## # A tibble: 2 x 2
    ##   Group Max_mean_nr_fix
    ##   <dbl>           <dbl>
    ## 1     0            74.4
    ## 2     1            76.9

    # AOI - face pupil size
    Fix$Face <- ifelse(Fix$AOI == "Face", 1, 0)

    Fix %>% 
      
      group_by(Group, Session, Trial, Face) %>% 
      summarise(
        mean_pupil = mean(PupilSize)) %>% 
      
      group_by(Group, Face) %>% 
      summarise(
        mean = mean(mean_pupil)
      )

    ## # A tibble: 6 x 3
    ## # Groups:   Group [2]
    ##   Group  Face   mean
    ##   <dbl> <dbl>  <dbl>
    ## 1     0     0 36433.
    ## 2     0     1 36802.
    ## 3     0    NA 38559.
    ## 4     1     0 36207.
    ## 5     1     1 34565.
    ## 6     1    NA 35735.

    ### TRY EXTRACT SCALE DURATIONS



    # Select one trial 
    ID_105 <- Fix %>% dplyr::filter(ID == 105 & Session == 1 & Picture == 0)


    # Total duration sum of fixations
    dur_sum <- sum(ID_105$Duration)

    # Total duration sum of fixations on the whole Scale
    scale_sum <- ID_105 %>% dplyr::filter(AOI %in% c(1,2,3,4,5,6,7,8))
    scale_sum <- sum(scale_sum$Duration)

    # Total duration on target rate
    ID_105 <-  ID_105 %>% dplyr::filter(AOI %in% c(1,2,3,4,5,6,7,8))
    ID_105$CurrentGroupRating <- as.numeric(ID_105$CurrentGroupRating)
    ID_105$AOI <- as.numeric(ID_105$AOI)
    ID_105 <- ID_105 %>% dplyr::filter(AOI == CurrentGroupRating)

    target_sum <- sum(ID_105$Duration)


    ID <- ID_105$ID[1]
    Trial <- ID_105$Trial[1]
    Picture <- ID_105$Picture[1]
    Diagnosis <- ID_105$Group[1]
    Session <- ID_105$Session[1]
    RT <- ID_105$RT[1]
        
        
    d <- tibble(dur_sum, scale_sum, target_sum, ID, Trial, Picture, Diagnosis, Session, RT)
    d

    ## # A tibble: 1 x 9
    ##   dur_sum scale_sum target_sum    ID Trial Picture Diagnosis Session    RT
    ##     <dbl>     <dbl>      <dbl> <dbl> <dbl>   <dbl>     <dbl>   <dbl> <dbl>
    ## 1   12578      4650        738   105    79       0         1       1 11934

    ## Wrap up in a big loop

    # start with a subset
    session_1 <- Fix %>% filter(Session == 1 & ID %in% c(101,103,105))


    # Make empty dataframe
    df <- tibble(dur_sum = numeric(), 
                 scale_sum = numeric(), 
                 target_sum = numeric(), 
                 ID = numeric(), 
                 Trial = numeric(),
                 Picture = numeric(), 
                 Diagnosis = numeric(), 
                 Session = numeric(),
                 RT = numeric())

    # create list of ID's
    id_list <- unique(session_1$ID)

    # 
    # for (crt_id in 1:length(id_list)){
    #   
    #   # take only one participant at a time
    #   ID <- dplyr::filter(session_1, ID == id_list[crt_id])
    #   
    #   for (crt_trial in 1:length(unique(ID$Trial))){
    #     
    #     # take one trial at a time from 1 to 153
    #     trial <- dplyr::filter(ID, Trial == crt_trial)
    #     
    #     # Total duration sum of fixations
    #     dur_sum <- sum(trial$Duration)
    #     
    #     # Total duration sum of fixations on the whole Scale
    #     scale_sum <- trial %>% dplyr::filter(AOI %in% c(1,2,3,4,5,6,7,8))
    #     scale_sum <- sum(scale_sum$Duration)
    #     
    #     # Total duration on target rate
    #     trial <- trial %>% dplyr::filter(AOI %in% c(1,2,3,4,5,6,7,8))
    #     
    #     trial$CurrentGroupRating <- as.numeric(trial$CurrentGroupRating)
    #     trial$AOI <- as.numeric(trial$AOI)
    #     
    #     target_sum <- trial %>% dplyr::filter(AOI == CurrentGroupRating)
    #     target_sum <- sum(target_sum$Duration)
    #     
    #     
    #     # Extract info to add to row
    #     ID <- trial$ID[1]
    #     Trial <- trial$Trial[1]
    #     Picture <- trial$Picture[1]
    #     Diagnosis <- trial$Group[1]
    #     Session <- trial$Session[1]
    #     RT <- trial$RT[1]
    #     
    #     # Create row
    #     d <- tibble(dur_sum, scale_sum, target_sum, ID, Trial, Picture, Diagnosis, Session, RT) 
    #     
    #     # Combine with premade empty dataframe
    #     if (nrow(df) == 0) {
    #       df <- d}
    #       else {
    #         df <- rbind(df, d)
    #         
    #         }
    #     }
    # }
    # 
    #     
    # 
    # 
    # 
    # 
    # 
    # for (crt_ses in 1:length(unique(Fix$Session))){
    #   
    #   # make subset with only one session
    #   round <- dplyr::filter(Fix, Session == crt_ses)
    #   
    #   if (round$Session[1] == 1){
    #     # Run premade loop for session 1
    #     
    #   } else {
    #     # make loop that can 'find' the groupratings for session 2.
    #     
    #   }
    #   
    # }

to do
=====

i need to figure out problem with 'filter\_'
