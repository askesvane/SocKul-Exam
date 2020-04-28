Set working directory and load packages

    # set working directory
    setwd("C:/Users/askes/OneDrive/Skrivebord/SocKul - Exam/data/")

    # packages 
    library(pacman)
    pacman::p_load(tidyverse, patchwork, readr)

    # import data
    Sac <- read_csv("SaccadesDV.csv", 
        col_types = cols(
          CurrentGroupRating = col_number(), 
          CurrentRating = col_number(), 
          PreviousGroupRating = col_number(), 
          PreviousRating = col_number()))


    Fix <- read_csv("FixationsDV.csv", 
        col_types = cols(
          CurrentGroupRating = col_number(), 
          PreviousGroupRating = col_number(), 
          PreviousRating = col_number()))


    #Calibration <- read.csv("Calibration.csv", sep = " ")
    #AOI <- read_csv("AoI_Coordinates.csv")

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

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    1.00    4.00    5.00    4.89    6.00    8.00  255912

    summary(Fix$PreviousGroupRating)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    1.00    3.00    5.00    4.83    7.00    8.00  255912

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

27/04
=====

Move CurrentRating column from Saccade to Fixation data

    # Make new column
    Fix <- Fix %>% mutate(CurrentRating = NA)
    Fix$CurrentRating <- as.numeric(Fix$CurrentRating)


    # make empty dataframe
    FixNew <- Fix[0,]


    # # The loop I tried to change from trial to picture so it matches.
    # for (crt_ses in 1:length(unique(Fix$Session))){
    # 
    #   # make subset with only one session
    #   session <- dplyr::filter(Fix, Session == crt_ses)
    #   
    #   # create list of ID's
    #   id_list <- unique(session$ID)
    #   
    #   for (crt_id in 1:length(id_list)){
    #     
    #     # take only one participant at a time
    #     ID <- dplyr::filter(session, ID == id_list[crt_id]) # write crt_id
    #     
    #     pic_list <- unique(ID$Picture)
    # 
    #     
    #     for (crt_pic in 1:length(pic_list)){
    #       
    #       # take one trial at a time from 1 to 153
    #       trial <- dplyr::filter(ID, Picture == pic_list[crt_pic]) # should be crt_trial
    #       
    #       # Extract the givemn rating
    #       Rating <- filter(Sac, Session == crt_ses & ID == id_list[crt_id] & Picture == pic_list[crt_pic])
    #       Rating <- Rating$CurrentRating[1]
    #       Rating <- as.numeric(Rating)
    #       
    #       
    #       # Add to dataframe if it is not an NA.
    #       if (!is.na(Rating) == TRUE){
    #         trial$CurrentRating <- Rating
    #       }
    #       
    #       
    #       # Combine with premade empty dataframe
    #       if (nrow(FixNew) == 0) {
    #         FixNew <- trial}
    #       else {
    #         FixNew <- rbind(FixNew, trial)}
    #       
    #     }
    #     
    #   }
    # 
    # }


    # Just check
    check <- FixNew %>% filter(ID == 111 & Picture == 3)
    check <- FixNew %>% filter(ID == 218 & Picture == 5)
    check <- FixNew %>% filter(ID == 130 & Picture == 56)
    check <- FixNew %>% filter(ID == 210 & Picture == 88)
    rm(check)



    # General comments:
    # ID 205 only went through 116 trials in session 2
    # ID138T0 has been removed as it is not a real trial (does not exist in Fix)
    # There is only one row in ID136T146 - maybe should be removed

    # Write to csv
    write_csv(FixNew,"C:/Users/askes/OneDrive/Skrivebord/SocKul - Exam/data/FixNew.csv")


    # Clean up Environment
    rm(ID, session, trial)

    ## Warning in rm(ID, session, trial): object 'ID' not found

    ## Warning in rm(ID, session, trial): object 'session' not found

    ## Warning in rm(ID, session, trial): object 'trial' not found

27/04
=====

Try to extract relevant values from one trial

    # # Select one trial 
    # id <- 101
    # ses <- 1
    # pic <- 0
    # 
    # trial <- FixNew %>% dplyr::filter(ID == id & Session == ses & Picture == pic)
    # trial_sac <- Sac %>% dplyr::filter(ID == id & Session == ses & Picture == pic)
    # 
    # 
    # ### Info we extract directly
    # ID <- trial$ID[1]
    # Trial <- trial$Trial[1]
    # Picture <- trial$Picture[1]
    # Diagnosis <- trial$Group[1]
    # Session <- trial$Session[1]
    # RT <- trial$RT[1]
    # 
    # 
    # ### info to be calculated
    # 
    # # Total duration sum of fixations and saccades
    # FixTime <- sum(trial$Duration)
    # SacTime <- sum(trial_sac$Duration)
    # 
    # 
    # # Total duration sum of fixations on face
    # FaceTime <- trial %>% 
    #   mutate(AOI = as.factor(AOI)) %>% 
    #   dplyr::filter(AOI == "Face")
    # FaceTime <- sum(FaceTime$Duration)
    # 
    # 
    # # Total duration sum of fixations on the whole Scale
    # scale <- trial %>% dplyr::filter(AOI %in% c(1,2,3,4,5,6,7,8))
    # ScaleTime <- sum(scale$Duration)
    # 
    # 
    # # Total duration on target rate
    # GR_Time <- scale %>% mutate(AOI = as.numeric(AOI)) %>% dplyr::filter(AOI == CurrentGroupRating | AOI == PreviousGroupRating)
    # GR_Time <- sum(GR_Time$Duration)
    # 
    # 
    # # Extract ratings - allow for NA's
    # CurrentRating <- trial$CurrentRating[1]
    # CurrentGroupRating <- trial$CurrentGroupRating[1]
    # 
    # if (is.na(Rating) == TRUE){
    #   PreviousRating <- NA
    # } else {
    #     PreviousRating <- trial$PreviousRating[1]
    #   }
    # 
    # if (is.na(Rating) == TRUE){
    #   PreviousGroupRating <- NA
    # } else {
    #     PreviousGroupRating <- trial$PreviousGroupRating[1]
    #   }
    # 
    # # Collect all info in a row
    # d <- tibble(ID, Trial, Picture, Diagnosis, Session, 
    #             RT, FixTime, SacTime, FaceTime, ScaleTime, GR_Time, 
    #             CurrentRating, CurrentGroupRating, PreviousRating, PreviousGroupRating)
    # 
    # d$PreviousRating <- as.numeric(d$PreviousRating)
    # d$PreviousGroupRating <- as.numeric(d$PreviousGroupRating)

Extract relevant values for all trials in a loop

    # # Read data with the new column
    # FixNew <- read_csv("FixNew.csv", col_types = cols(
    #       CurrentGroupRating = col_number(), 
    #       PreviousGroupRating = col_number(), 
    #       PreviousRating = col_number()))
    # 
    # 
    # ## Wrap up in a big loop
    # 
    # 
    # # Make container DF
    # data <- tibble(ID = as.numeric(), 
    #              Trial = as.numeric(), 
    #              Picture = as.numeric(), 
    #              Diagnosis = as.numeric(), 
    #              Session = as.numeric(), 
    #              RT = as.numeric(), 
    #              FixTime = as.numeric(), 
    #              SacTime = as.numeric(), 
    #              FaceTime = as.numeric(), 
    #              ScaleTime = as.numeric(), 
    #              GR_Time = as.numeric(), 
    #              CurrentRating = as.numeric(), 
    #              CurrentGroupRating = as.numeric(), 
    #              PreviousRating = as.numeric(), 
    #              PreviousGroupRating = as.numeric())
    # 

    # # The loop
    # for (crt_ses in 1:length(unique(FixNew$Session))){
    # 
    #   # make subset with only one session
    #   session <- subset(FixNew, Session == crt_ses)
    #   # create list of ID's
    #   id_list <- unique(session$ID)
    #   
    #   
    #   for (crt_id in 1:length(id_list)){
    #     
    #     # take only one participant at a time
    #     id <- subset(session, ID == id_list[crt_id])
    #     # create list of pic's
    #     pic_list <- unique(id$Picture)
    #     
    # 
    #     for (crt_pic in 1:length(pic_list)){
    #       
    #       # take one trial at a time from 1 to 153
    #       trial <- subset(id, Picture == pic_list[crt_pic])
    #       trial_sac <- subset(Sac, 
    #                           ID == id_list[crt_id] & 
    #                           Session == crt_ses & 
    #                           Picture == pic_list[crt_pic])
    #       
    #       
    #       ### Info we extract directly
    #       ID <- trial$ID[1]
    #       Trial <- trial$Trial[1]
    #       Picture <- trial$Picture[1]
    #       Diagnosis <- trial$Group[1]
    #       Session <- trial$Session[1]
    #       RT <- trial$RT[1]
    #       
    #       
    #       ### info to be calculated
    #       # Total duration sum of fixations and saccades
    #       FixTime <- sum(trial$Duration)
    #       SacTime <- sum(trial_sac$Duration)
    #       
    #       
    #       # Total duration sum of fixations on face
    #       FaceTime <- trial %>% 
    #         mutate(AOI = as.factor(AOI)) %>% 
    #         subset(AOI == "Face")
    #       
    #       FaceTime <- sum(FaceTime$Duration)
    #       
    #       
    #       # Total duration sum of fixations on the whole Scale
    #       scale <- trial %>% subset(AOI %in% c(1,2,3,4,5,6,7,8))
    #       ScaleTime <- sum(scale$Duration)
    #       
    #       
    #       # Total duration on target rate
    #       GR_Time <- scale %>% 
    #         mutate(AOI = as.numeric(AOI)) %>% 
    #         subset(AOI == CurrentGroupRating | AOI == PreviousGroupRating)
    #       
    #       GR_Time <- sum(GR_Time$Duration)
    #       
    #       
    #       # Extract ratings - allow for NA's
    #       CurrentRating <- trial$CurrentRating[1]
    #       CurrentGroupRating <- trial$CurrentGroupRating[1]
    #       
    #       if (is.na(Rating) == TRUE){
    #         PreviousRating <- NA
    #         } else {
    #           PreviousRating <- trial$PreviousRating[1]
    #           }
    #       
    #       if (is.na(Rating) == TRUE){
    #         PreviousGroupRating <- NA
    #         } else {
    #           PreviousGroupRating <- trial$PreviousGroupRating[1]
    #           }
    #       
    #       # Collect all info in a row
    #       row <- tibble(ID, Trial, Picture, Diagnosis, Session, 
    #             RT, FixTime, SacTime, FaceTime, ScaleTime, GR_Time, 
    #             CurrentRating, CurrentGroupRating, PreviousRating, PreviousGroupRating)
    #       
    #       row$PreviousRating <- as.numeric(row$PreviousRating)
    #       row$PreviousGroupRating <- as.numeric(row$PreviousGroupRating)
    #       
    #       
    #       # Combine with premade empty dataframe
    #       if (nrow(data) == 0) {
    #         data <- row
    #         } else {
    #         data <- rbind(data, row)}
    #       
    #     }
    #     
    #   }
    # 
    # }
    # 



    # # Write to csv
    # write_csv(data,"C:/Users/askes/OneDrive/Skrivebord/SocKul - Exam/data/data_preprocessed.csv")
    # 
    # # Check it out
    # summary(data)
    # 
    # # Those should be removed from data.
    # check <- FixNew %>% filter(ID == 136 & Trial == 146 & Picture == 63) # weird one row
    # check <- FixNew %>% filter(ID == 211 & Picture == 99) # missing 'CurrentRating' values for session 2
    # 
    # 
    # 
    # 
    # # Clean up Environment
    # rm(id,row,scale,session,trial,trial_sac, check)

28/04 Explore hypotheses
========================

    # set working directory
    setwd("C:/Users/askes/OneDrive/Skrivebord/SocKul - Exam/data/")

    # packages 
    library(pacman)
    pacman::p_load(tidyverse, patchwork, readr, brms, ggthemes)

    # Read preprocessed data
    d <- read_csv("data_preprocessed.csv", col_types = cols(
          CurrentGroupRating = col_number(), 
          PreviousGroupRating = col_number(), 
          PreviousRating = col_number()))


    d <- d %>% mutate(
      Session = as.factor(Session),
      Diagnosis = as.factor(Diagnosis)
    )

Plot
----

    # Illustration of the FaceTime data by diagnosis and session
    ggplot(data = d) + 
      geom_boxplot(aes(x = Session, y = FaceTime)) +
      
      labs(
        title = "Fixation Time on AOI Face", 
        subtitle = "Divided by TD(0) and SCZ(1)",
        caption = "Data source: Simonsen et al.") + 
      
      theme_bw() + theme(legend.background = element_rect(fill="lightblue")) +
      scale_colour_pander() + scale_fill_pander() +
      ylab("Time spent fixating on Face in ms") +
      theme_pander() + 
      theme(text = element_text(family = "Times"),
            legend.position = "none") +
      facet_wrap(.~ Diagnosis)

    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x,
    ## x$y, : font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x,
    ## x$y, : font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

![](sockul-look-through-data_files/figure-markdown_strict/unnamed-chunk-7-1.png)

    ggplot(data = d) + 
      geom_boxplot(aes(x = Diagnosis, y = FaceTime)) +
      
      labs(
        title = "Fixation Time on AOI Face", 
        subtitle = "Divided by TD(0) and SCZ(1)",
        caption = "Data source: Simonsen et al.") + 
      ylab("Time spent fixating on Face in ms") +
      
      theme_bw() + theme(legend.background = element_rect(fill="lightblue"))

![](sockul-look-through-data_files/figure-markdown_strict/unnamed-chunk-7-2.png)

    # Time spent fixating by group rating - We might have to standardize because the reaction times could be shorter
    ggplot(data = d) + 
      geom_boxplot(aes(x = Session, y = GR_Time, fill = Diagnosis)) +
      
      labs(
        title = "Fixation Time on Group Rating", 
        subtitle = "Divided by session",
        caption = "Data source: Simonsen et al.") + 
      ylab("Time spent fixating on group rating in ms") +
      
      theme_bw() + theme(legend.background = element_rect(fill="lightblue"))

![](sockul-look-through-data_files/figure-markdown_strict/unnamed-chunk-7-3.png)

    # Checking out reaction times 
    ggplot(data = d) + 
      geom_boxplot(aes(x = Session, y = RT, fill = Diagnosis)) +
      
      labs(
        title = "Reaction Time by Trial", 
        subtitle = "Divided by session and diagnosis",
        caption = "Data source: Simonsen et al.") + 
      ylab("RT") +
      
      theme_bw() + theme(legend.background = element_rect(fill="lightblue"))

![](sockul-look-through-data_files/figure-markdown_strict/unnamed-chunk-7-4.png)

Face model
----------

    # # formula
    # formula <- bf(FaceTime ~ 0 + Diagnosis + (1|ID) + (1|Picture))
    # 
    # # Set priors
    # get_prior(formula, d, family = gaussian())
    # 
    # prior <- c(
    #   prior(normal(3000, 100), class = b),
    #   prior(normal(10, 150), class = sd),
    #   prior(normal(100, 1500), class = sigma) 
    # )
    # 
    # 
    # 
    # # Run model based on priors alone
    # model0 <- brm(
    #   formula,
    #   d,
    #   family = gaussian(),
    #   prior = prior,
    #   sample_prior = "only",
    #   chains = 2,
    #   cores = 2
    # )
    # 
    # pp_check(model1, nsamples=100)
    # 
    # model1 <- brm(
    #   formula,
    #   d,
    #   family = gaussian(),
    #   prior = prior0,
    #   sample_prior = T,
    #   chains = 2,
    #   cores = 2
    # )
    # 
    # pp_check(model1, nsamples=100)
    # 
    # 
    # # Hypothesis testing
    # plot(hypothesis(NS_m0, "DiagnosisASD > 0")) # we expect that ASD beta (from TD to ASD) is bigger than 0.
    # hypothesis(NS_m0, "DiagnosisASD > 0") 
    # # evid.Ratio: 6 times as much evidence that the difference is there, though effect = not strong
    # 
    # summary(model1.1)
    # # Pitch variability for intercept (diagnosis TD) is 0.17
    # # slope from TD to ASD is 0.08
    # 
    # # chain plot
    # plot(NS_m0)
