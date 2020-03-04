library(plyr)
library(tidyr)
library(dplyr)
library(reshape2)
library(ggplot2)

# CONSTANTS ----------------------------------------------------------------------------

CS_list <- list('A' = c('green', 'blue'),
                'B' = c('blue', 'green'),
                'C' = c('red', 'blue'),
                'D' = c('blue', 'red'),
                'E' = c('green', 'red'),
                'F' = c('red', 'green'),
                'G' = c('green', 'blue'),
                'H' = c('blue', 'green'),
                'I' = c('red', 'blue'),
                'J' = c('blue', 'red'),
                'K' = c('green', 'red'),
                'L' = c('red', 'green'))
CS_list <- lapply(CS_list, function(x) {names(x) <- c('left', 'right'); return(x)})
CS_list_digits <- lapply(CS_list, function(x) {names(x) <- c('37', '39'); return(x)})
colors_CS <- c('red', 'blue', 'green')
CS_deval_list <- list('A' = 'MM',
                      'B' = 'BBQ',
                      'C' = 'TT',
                      'D' = 'BBQ',
                      'E' = 'MM',
                      'F' = 'TT',
                      'G' = 'BBQ',
                      'H' = 'MM',
                      'I' = 'BBQ',
                      'J' = 'TT',
                      'K' = 'TT',
                      'L' = 'MM')

list_snacks_versions <- list('A' = c('MM', 'BBQ', 'MM'),
                             'B' = c('BBQ', 'MM', 'BBQ'),
                             'C' = c('TT', 'BBQ', 'TT'),
                             'D' = c('BBQ', 'TT', 'BBQ'),
                             'E' = c('MM', 'TT', 'MM'),
                             'F' = c('TT', 'MM', 'TT'),
                             'G' = c('MM', 'BBQ', 'BBQ'),
                             'H' = c('BBQ', 'MM', 'MM'),
                             'I' = c('TT', 'BBQ', 'BBQ'),
                             'J' = c('BBQ', 'TT', 'TT'),
                             'K' = c('MM', 'TT', 'TT'),
                             'L' = c('TT', 'MM', 'MM'))
list_snacks_versions_instr <- list('A' = c('M&M', 'BBQ', 'M&M'),
                             'B' = c('BBQ', 'M&M', 'BBQ'),
                             'C' = c('TT', 'BBQ', 'TT'),
                             'D' = c('BBQ', 'TT', 'BBQ'),
                             'E' = c('M&M', 'TT', 'M&M'),
                             'F' = c('TT', 'M&M', 'TT'),
                             'G' = c('M&M', 'BBQ', 'BBQ'),
                             'H' = c('BBQ', 'M&M', 'M&M'),
                             'I' = c('TT', 'BBQ', 'BBQ'),
                             'J' = c('BBQ', 'TT', 'TT'),
                             'K' = c('M&M', 'TT', 'TT'),
                             'L' = c('TT', 'M&M', 'M&M'))
list_snacks_versions <- lapply(list_snacks_versions, function(x) {names(x) <- c('left', 'right', 'deval'); return(x)})
list_snacks_versions_instr <- lapply(list_snacks_versions_instr, function(x) {names(x) <- c('left', 'right', 'deval'); return(x)})                                                                                
pairs_color_snack <- c('red' =  'TT', 'blue' = 'BBQ', 'green' = 'MM', 'yellow' = 'Empty')
# -----------------------------------------------------------------------------------------------------------






# General information about participants: ID, version, stage of interest -------------------------------------------
completers <- function(dat){
  dattab <- dat %>% 
    group_by(PIN) %>% 
    group_map(~ qc_complete_participants(.x))
  dattab1 <- data.frame(PIN = unique(dat$PIN), test = unlist(dattab))
  return(dattab1)
}


qc_complete_participants <- function(dat){
  if (any(dat$stage == 'Close HIT & Questions')){
    return('Ok')
  } else { return('failed')}
}



get_stage_id <- function(string_name_stage, id){
  # return a choosen stage of experiment for specific participant
  # arguments: name of stage('Key-testing', 'Pav condition') and ID of participant (1,2 ..)
  # output: dataframe, subset of original dataset
  dat %>%
    filter(PIN == id & Stage == string_name_stage)
}

get_stage <- function(string_name_stage,dat){
  dat %>%
    filter(stage == string_name_stage)
}

duration_stage <- function(stage_string, dat){
  # return a choosen stage of experiment for all participants in original dataset
  # arguments: name of stage('Key-testing', 'Pav condition'), dataset
  # output: a tibble, subset of original dataset
  dat %>%
    select(PIN, stage, timestamp) %>%
    filter(stage == stage_string) %>%
    group_by(PIN) %>%
    filter(row_number()==1 | row_number()==n()) %>%
    mutate(difference = timestamp - lag(timestamp)) %>%
    filter(!is.na(difference)) %>%
    select(-c(stage, timestamp))
}

duration_stage_pav <- function(dat){
  # return a choosen stage of experiment for all participants in original dataset
  # arguments: name of stage('Key-testing', 'Pav condition'), dataset
  # output: a tibble, subset of original dataset
  dat %>%
    select(PIN, stage, timestamp, version) %>%
    filter(stage %in% c('Pav Condition', 'Pav Condition Response')) %>%
    group_by(PIN) %>%
    filter(row_number()==1 | row_number()==n()) %>%
    mutate(difference = timestamp - lag(timestamp)) %>%
    filter(!is.na(difference)) %>%
    select(-c(stage, timestamp, version))
}

general_info_all_participants <- function(dat){
  tab1 <- dat %>%
    select(PIN, version, timestamp, location) %>%
    group_by(PIN) %>%
    filter(row_number()==1 | row_number()==n()) %>%
    mutate(time_spent = timestamp - lag(timestamp)) %>%
    filter(!is.na(time_spent)) %>%
    select(-timestamp)
  tab2 <- dat %>% 
    select(PIN, event.type) %>% 
    filter(event.type == 'key press') %>% 
    group_by(PIN) %>% 
    count()
  fin <- data.frame(merge(tab1, tab2, by='PIN'))
  names(fin) <- c("PIN", "version", "location","total_time", "num_acts")
  return(fin)
}


# -----------------------------------------------------------------------------------------------------------






# Instrumental conditioning , the number of correct / incorrect answers --------------------------------

get_num_quest_ic <- function(Stage_encoded, event_encoded, dat){
  # returns a table which contains the number of questions asked at the stage
  # arguments: the name of the stage( 'Instr Condition Real'), the way the question 
  # is presented ('image/text appears'), dataset
  dat %>%
    select(PIN, event.type, event.raw.details, stage) %>%
    filter(stage == Stage_encoded) %>%
    filter(event.type == event_encoded) %>%
    group_by(PIN) %>%
    summarise(n = n())
}

get_num_correct_answs_ic <- function(Stage_encoded, correct_encoded, dat){
  # returns a table which contains the number of correct answers for the questions asked at the stage
  # arguments: the name of the stage( 'Instr Condition Real'), the way the correct answer 
  # is described ('Correct!'), dataset
  num_correct <- dat %>%
    select(PIN, event.type, event.raw.details, stage) %>%
    filter(stage == Stage_encoded) %>%
    filter(event.raw.details == correct_encoded) %>%
    group_by(PIN) %>%
    summarise(n = n())
}

final_table <- function(num_questions, num_correct, duration_stage, num_act){
  # returns a final table concluding the stage 
  # arguments: table with the number of questions, table with the number of correct answers,
  # table with the duration of the game andversion of the game for each ID (returned by function dur_proced_all_participants )
  num_questions[order(num_questions$PIN),]
  num_correct[order(num_correct$PIN),]
  duration <- duration[,c(1,2)]
  duration[order(duration$PIN),]
  duration_stage[order(duration_stage$PIN),]
  final <- merge(duration, merge(duration_stage, 
                                 merge(num_questions, 
                                       merge(num_correct, num_act, by = 'PIN')), by='PIN'), by = 'PIN')
  names(final) <- c('PIN','version', 'time_spent_stage', 'num_questions', 'correct_answ',"num_act")
  final$ratio <- (final$correct_answ / final$num_questions) * 100
  return(final)
}


instr_stage_qc <- function(dat){
  num_correct_qc = 0
  num_incorrect_qc = 0
  for (i in 1:nrow(dat)){
    if (dat$event.type[i] == 'image/text appears'){
      if ( unlist(strsplit(as.character(dat$event.converted.details[i+1]), " "))[1] == 
           ( names(which(list_snacks_versions[[dat$version[1]]][1:2] == 
                         unlist(strsplit(as.character(dat$event.converted.details[i]), " "))[1])) ) ){
        if (dat$event.raw.details[i+2] == 'Correct'){
          num_correct_qc = num_correct_qc + 1
        } else {
          print('Something is wrong!')
        }
      } else if (dat$event.raw.details[i+2] == 'Incorrect'){
        num_incorrect_qc = num_incorrect_qc + 1
      } else {
        print('Something is wrong!')
      }
    }
  }
  return(c('correct' = num_correct_qc, 'incorrect' = num_incorrect_qc, 'total' = (num_incorrect_qc + num_correct_qc)))
}

# Alternative version. Works when the answer(Correct/Incorrect) is further then 2 cells down

instr_stage_qc_alt <- function(dat){
  num_correct_qc = 0
  num_incorrect_qc = 0
  greatest_num_consecutive_right_answ = 0
  greatest_num_consecutive_wrong_answ = 0
  right_answ_streak = 0
  wrong_answ_streak = 0
  for (i in 1:nrow(dat)){
    if (dat$event.type[i] == 'image/text appears'){
      k = which(dat[(i+1):nrow(dat),]$event.type == 'text appears')[1]
      cat('i= ', i, 'k= ', k, '\n')
      l = which(dat[(i+1):nrow(dat),]$event.converted.details == 'left arrow button pressed' | dat[(i+1):nrow(dat),]$event.converted.details == 'right arrow button pressed' )[1]
      if ( unlist(strsplit(as.character(dat$event.converted.details[i+l]), " "))[1] == 
           ( names(which(list_snacks_versions[[dat$version[1]]][1:2] == 
                         unlist(strsplit(as.character(dat$event.converted.details[i]), " "))[1])) ) ) {
        if (dat$event.raw.details[i+k] == 'Correct'){
          num_correct_qc = num_correct_qc + 1
          right_answ_streak = right_answ_streak + 1
          wrong_answ_streak = 0
          if (right_answ_streak > greatest_num_consecutive_right_answ){
            greatest_num_consecutive_right_answ <- right_answ_streak
          }
        } else {
          print('Something is wrong!correct sect')
        }
      } else if (dat$event.raw.details[i+k] == 'Incorrect'){
        num_incorrect_qc = num_incorrect_qc + 1
        right_answ_streak = 0
        wrong_answ_streak = wrong_answ_streak + 1
        if (wrong_answ_streak > greatest_num_consecutive_wrong_answ){
          greatest_num_consecutive_wrong_answ <- wrong_answ_streak
        }
      } else {
        print('Something is wrong!incorrect sect')
      }
    }
  }
  return(c('correct' = num_correct_qc, 'incorrect' = num_incorrect_qc, 'total' = (num_incorrect_qc + num_correct_qc), 
           'consec_right' = greatest_num_consecutive_right_answ, 'consec_wrong' = greatest_num_consecutive_wrong_answ))
}

num_act <- function(dataset){
  dataset %>% 
    select(PIN, event.type) %>% 
    filter(event.type == 'key press') %>% 
    group_by(PIN) %>% 
    count()
}
# -----------------------------------------------------------------------------------------------------------






# Pavlovian conditioning stage

# -----------------------------------------------------------------------------------------------------------
get_num_quest_pavc <- function(dat){
  # returns a table which contains the number of questions asked at the stage
  # arguments: dataset
  dat$stage <- as.character(dat$stage)
  dat %>%
    select(PIN, event.type, event.raw.details, stage) %>%
    filter(stage == 'Pav Condition' | stage == 'Pav Condition Response') %>%
    filter(event.type == 'text appears' & stage == 'Pav Condition Response') %>%
    group_by(PIN) %>%
    summarise(n = n())
}

get_num_correct_answs_pavc <- function(dat){
  # returns a table which contains the number of correct answers for the questions asked at the stage
  # arguments: dataset
  num_correct <- dat %>%
    select(PIN, event.type, event.raw.details, stage) %>%
    filter(stage == 'Pav Condition' | stage == 'Pav Condition Response') %>%
    filter(stage == 'Pav Condition Response' & event.raw.details == 'Correct') %>%
    group_by(PIN) %>%
    summarise(n = n())
}


# Quality control of Pavlovian conditioning stage

pavlov_stage_qc <- function(dat){
  num_correct_qc = 0
  num_incorrect_qc = 0
  greatest_num_consecutive_right_answ = 0
  greatest_num_consecutive_wrong_answ = 0
  right_answ_streak = 0
  wrong_answ_streak = 0
  for (i in 1:nrow(dat)){
    if (dat$event.type[i] == 'image/text appears'){
      if (as.character(dat$event.raw.details[i+1]) == pairs_color_snack[unlist(strsplit(as.character(dat$event.converted.details[i]), " "))[1]]){
        if (dat$event.type[i+2] == 'text appears' & dat$event.raw.details[i+2] == 'Correct'){
          num_correct_qc = num_correct_qc + 1
          right_answ_streak = right_answ_streak + 1
          wrong_answ_streak = 0
          if (right_answ_streak > greatest_num_consecutive_right_answ){
            greatest_num_consecutive_right_answ <- right_answ_streak
          }
        } else {
          print('Something is wrong!')
        }
      } else if (dat$event.type[i+2] == 'text appears' & dat$event.raw.details[i+2] == 'Incorrect'){
        num_incorrect_qc = num_incorrect_qc + 1
        right_answ_streak = 0
        wrong_answ_streak = wrong_answ_streak + 1
        if (wrong_answ_streak > greatest_num_consecutive_wrong_answ){
          greatest_num_consecutive_wrong_answ <- wrong_answ_streak
        }
      } else {
        print('Something is wrong!')
      }
    }
  }
  return(c('correct' = num_correct_qc, 'incorrect' = num_incorrect_qc, 'total' = (num_incorrect_qc + num_correct_qc),
           'consec_right' = greatest_num_consecutive_right_answ,'consec_wrong' = greatest_num_consecutive_wrong_answ))
}

# Combining quality checks with the results
combine_results <- function(tabulated_results, qc_tab){
  tabulated_results$consec_correct_answers <- unname(unlist(lapply(qc_tab, '[', 4)))
  tabulated_results$consec_wrong_answers <- unname(unlist(lapply(qc_tab,'[', 5)))
  tabulated_results$qc_num_questions <- unname(unlist(lapply(qc_tab, '[', 3)))
  tabulated_results$qc_correct_answers <- unname(unlist(lapply(qc_tab, '[', 1)))
  tabulated_results$status <- ifelse((tabulated_results$correct_answ == tabulated_results$correct_answ) &
                                       (tabulated_results$qc_num_questions == tabulated_results$num_questions), 'passed', 'failed' )
  return(tabulated_results)
}

pavlov_stage_deliberation <- function(subset_pav){
  beg = NULL
  stop = NULL
  delta <- c()
  for (i in 1:nrow(subset_pav)){
    if (subset_pav$event.type[i] == "image/text appears"){
      beg <- subset_pav$timestamp[i]
      stop <- NULL
    } else if (subset_pav$event.type[i] == "The answer was selected") {
      stop <- subset_pav$timestamp[i]
      delta <- c(delta, stop-beg)
      beg <- NULL
      stop <- NULL
    } else{
      next
    }
  }
  return(mean(delta))
}

# Transfer ----------------------------------------------------------------------------------------------------
get_num_colors_cs <- function(dataset){
  num_red = 0
  flag_red = 0
  num_blue = 0
  flag_blue = 0
  num_yellow = 0
  flag_yellow = 0
  num_green = 0
  flag_green = 0
  num_white = 0
  flag_white = 0
  for (i in 1:nrow(dataset)){
    if(dataset[i, 'event.type'] == 'image appears'){
      if (grepl('red',dataset[i, 'event.raw.details'])){
        flag_red = 1
        flag_blue = 0
        flag_green = 0
        flag_yellow = 0
        flag_white = 0
      } else if (grepl('blue',dataset[i, 'event.raw.details'])){
        flag_red = 0
        flag_blue = 1
        flag_green = 0
        flag_yellow = 0
        flag_white = 0
      }else if (grepl('yellow',dataset[i, 'event.raw.details'])){
        flag_red = 0
        flag_blue = 0
        flag_green = 0
        flag_yellow = 1
        flag_white = 0
      }else if (grepl('white',dataset[i, 'event.raw.details'])){
        flag_red = 0
        flag_blue = 0
        flag_green = 0
        flag_yellow = 0
        flag_white = 1
      }else if (grepl('green',dataset[i, 'event.raw.details'])){
        flag_red = 0
        flag_blue = 0
        flag_green = 1
        flag_yellow = 0
        flag_white = 0
      }
    } else if ( dataset[i, 'event.type'] =='key press' & (dataset[i, "event.raw.details"] %in% c("39", "37"))) {
      if (flag_red == 1){
        num_red = num_red + 1
      } else if (flag_blue == 1){
        num_blue = num_blue + 1
      } else if (flag_green == 1) {
        num_green = num_green + 1
      } else if (flag_white == 1){
        num_white = num_white + 1
      } else if (flag_yellow == 1){
        num_yellow = num_yellow + 1
      }
    } else next
  }
  return(c('blue' = num_blue, 'white' = num_white, 'green' = num_green, 'yellow' = num_yellow, 'red' = num_red))
}


get_cs_plus <- function(ver_string){
  return(c('CS+' = c(unname(CS_list[[ver_string]][1]), unname(CS_list[[ver_string]][2])), 'CS-' = 'yellow', 'base' = 'white', 'CS' = colors_CS[which(!(colors_CS %in% c(unname(CS_list[[ver_string]][1]), unname(CS_list[[ver_string]][2]))))] ))
}

get_CS_numbers <- function(vec_col_string, vec_cs_string){
  answ <- vec_col_string
  for (i in 1:length(vec_cs_string)){
    names(answ)[i] = names(vec_cs_string)[vec_cs_string == names(answ)[i]]
  }
  return(answ)
}


get_cs_same_different <- function(dataset, colors_cs){
  same = 0
  different = 0
  flag_1 = 0
  flag_2 = 0
  for (i in 1:nrow(dataset)){
    if(dataset[i, 'event.type'] == 'image appears'){
      if (grepl(colors_cs[1],dataset[i, 'event.raw.details'])){
        flag_1 = 1
        flag_2 = 0
      } else if (grepl(colors_cs[2],dataset[i, 'event.raw.details'])){
        flag_1 = 0
        flag_2 = 1
      } else {
        flag_1 = 0 
        flag_2 = 0
        next
      }
      cat(i,flag_1,flag_2)
      cat("\n")
    } else if ( dataset[i, 'event.type'] =='key press' & (dataset[i, "event.raw.details"] %in% c("39", "37"))) {
      if (flag_1 == 1){
        if (dataset[i, "event.raw.details"] == names(CS_list_digits[['H']][1])) {
          same = same + 1
          cat('same_flag1_+', i)
          cat('\n')
        } else different = different + 1
      } else if (flag_2 == 1) {
        if (dataset[i, "event.raw.details"] == names(CS_list_digits[['H']][2])) {
          same = same + 1
          cat('same_flag2_+', i)
          cat('\n')
        } else different = different + 1
      } 
    } else next
  }
  return(c('same' = same, 'different' = different))
}

get_cs_same_different_by_CS <- function(dataset, colors_cs){
  same_1 = 0
  same_2 = 0
  different_1 = 0
  different_2 = 0
  flag_1 = 0
  flag_2 = 0
  for (i in 1:nrow(dataset)){
    if(dataset[i, 'event.type'] == 'image appears'){
      if (grepl(colors_cs[1],dataset[i, 'event.raw.details'])){
        flag_1 = 1
        flag_2 = 0
      } else if (grepl(colors_cs[2],dataset[i, 'event.raw.details'])){
        flag_1 = 0
        flag_2 = 1
      } else {
        flag_1 = 0 
        flag_2 = 0
      }
      cat(i,flag_1,flag_2)
      cat("\n")
    } else if ( dataset[i, 'event.type'] =='key press' & (dataset[i, "event.raw.details"] %in% c("39", "37"))) {
      if (flag_1 == 1){
        if (dataset[i, "event.raw.details"] == names(CS_list_digits[['H']][1])){
          same_1 = same_1 + 1
          cat('same_flag1_+', i)
          cat('\n')
        } else different_1 = different_1 + 1
      } else if (flag_2 == 1) {
        if (dataset[i, "event.raw.details"] == names(CS_list_digits[['H']][2])) {
          same_2 = same_2 + 1
          cat('same_flag2_+', i)
          cat('\n')
        } else different_2 = different_2 + 1
      } 
    } else next
  }
  return(list('CS1' = c('same' = same_1, 'different' = different_1), 
              'CS2' = c('same' = same_2, 'different' = different_2)))
}

# -----------------------------------------------------------------------------------------------------------






# Devaluation  -----------------------------------------------------------------------------------------
# Functions for 1 stage
# rebuild_food_rate <- function(dev){
#   # returns reshaped table without NAs and with recoded names of snacks
#   # arguments: dataframe extracted from FoodRatings.csv
#   dev$FoodItem[which(dev$FoodItem == 'teddy.png')] <- 'TT'
#   dev$FoodItem[which(dev$FoodItem == 'bbq.png')] <- 'BBQ'
#   dev$FoodItem[which(dev$FoodItem == 'candies.png')] <- 'MM'
#   dev1 <- na.omit(dev[,-8])
#   dev2 <- na.omit(deval_data[-c(1:)])
#   dev <- cbind(dev1, dev2)
#   rownames(dev) <- NULL
#   dev$Pre.rating <- as.numeric(dev$Pre.rating)
#   dev$Post.rating <- as.numeric(dev$Post.rating)
#   return(dev[,-c(2,3)])
# }

rebuild_food_rate_hunger <- function(dev){
  # returns reshaped table without NAs and with recoded names of snacks
  # arguments: dataframe extracted from FoodRatings.csv
  dev1 <- na.omit(dev[-6])
  dev2 <- na.omit(dev[-c(1:5)])
  dev3 <- cbind(dev1, dev2)
  rownames(dev3) <- NULL
  dev3$Pre.rating.hunger <-  unlist(lapply(dev3$Hunger.pre.rating, function(x) as.numeric(unlist(strsplit(x, ''))[4])))
  dev3$Post.rating.hunger <- unlist(lapply(dev3$Hunger.post.rating, function(x) as.numeric(unlist(strsplit(x, ''))[4])))
  return(dev3[,-c(2:6)])
}

rebuild_food_rate_temp <- function(dev){
  # returns reshaped table without NAs and with recoded names of snacks
  # arguments: dataframe extracted from FoodRatings.csv
  dev$FoodItem[which(dev$FoodItem == 'teddy.png')] <- 'TT'
  dev$FoodItem[which(dev$FoodItem == 'bbq.png')] <- 'BBQ'
  dev$FoodItem[which(dev$FoodItem == 'candies.png')] <- 'MM'
  dev1 <- na.omit(dev[-c(5,8)])
  dev2 <- na.omit(dev[-c(1:7)])
  dev3 <- cbind(dev1, dev2)
  rownames(dev3) <- NULL
  dev3$Pre.rating <-  unlist(lapply(dev3$Pre.rating, function(x) as.numeric(unlist(strsplit(x, ''))[4])))
  dev3$Post.rating <- unlist(lapply(dev3$Post.rating, function(x) as.numeric(unlist(strsplit(x, ''))[4])))
  return(dev3[,-c(2,3)])
}


deval_column <- function(tab_food_rate, duration){
  # returns a dataframe with three new columns: version of game, duration of game and allocated types of the snacks
  # argument: reshaped dataframe 
  tab_food_rate$version <-  as.character(sapply(tab_food_rate$PIN, function(x) {duration$version[duration$PIN == x]}))
  tab_food_rate$duration <- sapply(tab_food_rate$PIN, function(x) {duration$total_time[duration$PIN == x]})
  for (i in 1:nrow(tab_food_rate)){
    if (tab_food_rate$FoodItem[i] %in% list_snacks_versions[[tab_food_rate$version[i]]]){
      if (length(which(list_snacks_versions[[tab_food_rate$version[i]]] == tab_food_rate$FoodItem[i])) == 2){
        tab_food_rate$deval[i] <- 'deval'
      } else {tab_food_rate$deval[i] <- 'nondeval'}
    }else {tab_food_rate$deval[i] <- 'CS_snack'}
  }
  return(tab_food_rate)
}


ver_help <- function(x){
  return (duration$version[duration$PIN == x])
}


dur_help <- function(x){
  return (duration$time_spent[duration$PIN == x])
}


final_tab_deval_stage <- function(dev){
  # Changing the order of the columns
  col_order <- c("PIN", "version", "duration",
                 "FoodItem", "Pre.rating", "Post.rating",
                 "difference", "deval")
  dev <- dev[,col_order]
}


final_tab_deval_stage <- function(dev){
  # Changing the order of the columns
  col_order <- c("PIN", "version", "duration",
                 "FoodItem", "Pre.rating", "Post.rating",
                 "difference", "deval")
  dev <- dev[,col_order]
}

diff_pre_post_column <- function(dev){
  # returning the datatable with a new column filled with differences between pre-pref and post-pref
  # arguments: reshaped dataframe
  dev %>% 
    mutate(difference = Pre.rating - Post.rating)
}

diff_pre_post_column_hunger <- function(dev){
  # returning the datatable with a new column filled with differences between pre-pref and post-pref
  # arguments: reshaped dataframe
  dev %>% 
    mutate(difference.hunger = Pre.rating.hunger - Post.rating.hunger)
}


# Function for stage 2
deval_stage2_count <- function(dataset){
  dataset %>%
    select(PIN, version, event.type, event.converted.details) %>%
    filter(event.type == 'key press') %>%
    mutate(act = if_else((as.character(version) %in% c('A', 'B', 'C', 'D', 'E', 'F') &
                            as.character(event.converted.details) == 'left arrow button pressed') |
                           (as.character(version) %in% c('G', 'H', 'I', 'J', 'K', 'L') &
                              as.character(event.converted.details) == 'right arrow button pressed'), 'deval','nondeval')) %>%
    group_by(PIN, version) %>%
    count(act)
}



deval_column_stage2 <- function(dataset){
  # returns a dataframe with three new columns: version of game, duration of game and allocated types of the snacks
  # argument: reshaped dataframe
    dataset$duration <- sapply(dataset$PIN, dur_help_deval2)
  return(dataset)
}


dur_help_deval2 <- function(x){
  return (dur$difference[dur$PIN == x])
}

final_tab_deval_stage2 <- function(dev){
  # Changing the order of the columns
  col_order <- c("PIN", "version", "duration",
                 "act", "n")
  dev <- dev[,col_order]
  dev %>%
    mutate(rate = n / (as.numeric(duration)/1000) )
}

roundUpNice <- function(x, nice=c(1,2,4,5,6,7,8,9,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
} 

missing_vals_s2_deval <- function(tab, participants_versions, dur_deval){
  opt = c('deval', 'nondeval')
  for(i in 1:length(participants_versions$PIN)) {
    if (any(tab$PIN == participants_versions$PIN[i])) {
      if ( length(which(c('deval', 'nondeval') %in% tab$act[tab$PIN == participants_versions$PIN[i] ])) < length(opt) ) {
        act_missing <- opt[which(!c('deval', 'nondeval') %in% tab$act[tab$PIN == participants_versions$PIN[i] ])]
        a <- data.frame(PIN = participants_versions$PIN[i], version = participants_versions$version[participants_versions$PIN == participants_versions$PIN[i] ],
                        duration = dur_deval$difference[dur_deval$PIN == participants_versions$PIN[i] ], act = act_missing, n = 0, rate = 0 )
        tab <- rbind(as.data.frame(tab), a)
      } else next
    } else {
      a <- data.frame(PIN = participants_versions$PIN[i], version = participants_versions$version[participants_versions$PIN == participants_versions$PIN[i] ],
                      duration = dur_deval$difference[dur_deval$PIN == participants_versions$PIN[i] ], act = 'deval', n = 0, rate = 0 )
      b <- data.frame(PIN = participants_versions$PIN[i], version = participants_versions$version[participants_versions$PIN == participants_versions$PIN[i] ],
                      duration = dur_deval$difference[dur_deval$PIN == participants_versions$PIN[i] ], act = 'nondeval', n = 0, rate = 0 )
      tab <- rbind(as.data.frame(tab), a, b)
    }
  }
  return(tab)
}

helper <- function(vec){
  new_vec = c()
  new_vec[which(vec == 'nondeval')] <- 'deval'
  new_vec[which(vec == 'deval')] <- 'nondeval'
  return(new_vec)
}

missing_vals_s3_deval_alter <- function(tab, participants_versions, num_sec){
  bins_for_plot <- seq(1:num_sec)
  opt = c('deval', 'nondeval')
  for(i in participants_versions){
    cat('participant ', i, '\n')
    a <- tab[tab$PIN == i, c('Bins','act')]
    if (a$Bins[length(a$Bins)] != num_sec) {
      delt = seq(a$Bins[length(a$Bins)] + 1,num_sec)
      for(k in delt){
        dat_val <- data.frame(PIN = i, Bins = k, 
                              act = 'deval', n = 0)
        dat_deval <- data.frame(PIN = i, Bins = k, 
                                act = 'nondeval', n = 0)
        tab <- rbind(as.data.frame(tab), dat_val, dat_deval)
      }
    }
    a_ind <- a$Bins[!a$Bins %in% a$Bins[duplicated(a$Bins)]] # missing timebins
    if (length(a_ind) == 0){
      next
    } else {
      a_act <- a$act[a$Bins %in% a_ind]  # actions present in bins
      acts_needed <- unname(sapply(a_act, helper))
      cat('timebins with mis values ', a_act,'\n')
      cat('acts needed ', acts_needed, '\n')
      cat(a_ind, '\n')
      for (j in 1:length(a_ind)){
        cat(i, j, '\n')
        b <- data.frame(PIN = i, Bins = a_ind[j], 
                        act = acts_needed[j], n = 0)
        tab <- rbind(as.data.frame(tab), b)
      }
      ind_absent <- bins_for_plot[which(!bins_for_plot %in% a$Bins)]
      for (k in ind_absent){
        d <- data.frame(PIN = i, Bins = k, 
                        act = 'nondeval', n = 0)
        e <- data.frame(PIN = i, Bins = k, 
                        act = 'deval', n = 0)
        tab <- rbind(as.data.frame(tab), d, e)
      }
    }
  }
  return(tab)
}

missing_vals_s3_deval <- function(tab, participants_versions, num_sec){
  bins_for_plot <- seq(1:num_sec)
  opt = c('deval', 'nondeval')
  for(i in participants_versions){
    cat('participant ', i, '\n')
    a <- tab[tab$PIN == i, c('Bins','act')]
    a_ind <- a$Bins[!a$Bins %in% a$Bins[duplicated(a$Bins)]] # missing timebins
    a_act <- a$act[a$Bins %in% a_ind]  # actions present in bins
    acts_needed <- unname(sapply(a_act, helper))
    cat('timebins with mis values ', a_act,'\n')
    cat('acts needed ', acts_needed, '\n')
    cat(a_ind, '\n')
    for (j in 1:length(a_ind)){
      cat(i, j, '\n')
      b <- data.frame(PIN = i, Bins = a_ind[j], 
                      act = acts_needed[j], n = 0)
      tab <- rbind(as.data.frame(tab), b)
    }
    ind_absent <- bins_for_plot[which(!bins_for_plot %in% a$Bins)]
    for (k in ind_absent){
      d <- data.frame(PIN = i, Bins = k, 
                      act = 'nondeval', n = 0)
      e <- data.frame(PIN = i, Bins = k, 
                      act = 'deval', n = 0)
      tab <- rbind(as.data.frame(tab), d, e)
    }
  }
  return(tab)
}

# General transfer ------------------------------------------------------------------------------------
ITI_CS <- function(dataset){
  time_ITI <- dataset$timestamp[which(grepl('green|blue|yellow|red', dataset$event.raw.details))][1] -
    dataset$timestamp[which(grepl('white', dataset$event.raw.details))][1]
  time_CS <- dataset$timestamp[which(grepl('white', dataset$event.raw.details))][2] - 
    dataset$timestamp[which(grepl('green|blue|yellow|red', dataset$event.raw.details))][1]
  ITI <- unlist(strsplit(as.character(time_ITI), ""))
  ITI <- as.numeric(paste(ITI[-length(ITI)], collapse = ''))*10
  CS <- unlist(strsplit(as.character(time_CS), ""))
  CS <- as.numeric(paste(CS[-length(CS)], collapse = ''))*10
  return(c('ITI' = ITI, 'CS_dur' = CS))
}


general_transfer <- function(dataset_subset){
  #intervals_transfer <- ITI_CS(dataset_subset)
  intervals_transfer <- c('ITI' = 6000, 'CS_dur' = 6000)
  version_PIN <- as.character(dataset_subset$version[1])
  color_CS_plus <- colors_CS[which(!colors_CS %in% CS_list[[version_PIN]])]
  color_CS_minus = 'yellow'
  CS_empty_flag = 0
  CS_reward_flag = 0
  count = 0
  pre_CS_count = c('pre_CS_minus_1' = 0,
                   'pre_CS_minus_2' = 0,
                   'pre_CS_minus_3' = 0,
                   #'pre_CS_minus_4' = 0,
                   'pre_CS_plus_1' = 0,
                   'pre_CS_plus_2' = 0,
                   'pre_CS_plus_3' = 0)
                   #'pre_CS_plus_4' = 0)
  
  while_CS_count = c('while_CS_minus_1' = 0,
                     'while_CS_minus_2' = 0,
                     'while_CS_minus_3' = 0,
                     'while_CS_minus_4' = 0,
                     'while_CS_minus_5' = 0,
                     'while_CS_minus_6' = 0,
                     'while_CS_plus_1' = 0,
                     'while_CS_plus_2' = 0,
                     'while_CS_plus_3' = 0,
                     'while_CS_plus_4' = 0,
                     'while_CS_plus_5' = 0,
                     'while_CS_plus_6' = 0)
  
  after_CS_count = c('after_CS_minus_1' = 0,
                     'after_CS_minus_2' = 0,
                     'after_CS_minus_3' = 0,
                     #'after_CS_minus_4' = 0,
                     'after_CS_plus_1' = 0,
                     'after_CS_plus_2' = 0,
                     'after_CS_plus_3' = 0)
                     #'after_CS_plus_4' = 0)
  for (i in 1:nrow(dataset_subset)){
    if(dataset_subset[i, 'event.type'] == 'image appears'){
      if (grepl(color_CS_minus, dataset_subset[i, 'event.raw.details'])){
        CS_empty_flag = 1
        CS_reward_flag = 0
        count = count + 1
        pre_CS_count = pre_CS_general(dataset_subset, i, CS_empty_flag, CS_reward_flag, pre_CS_count, intervals_transfer)
        while_CS_count = while_CS_general(dataset_subset, i, CS_empty_flag, CS_reward_flag, while_CS_count,intervals_transfer)
        after_CS_count = after_CS_general(dataset_subset, i, CS_empty_flag, CS_reward_flag, after_CS_count,intervals_transfer)
      } else if (grepl(color_CS_plus, dataset_subset[i, 'event.raw.details'])) {
        CS_empty_flag = 0
        CS_reward_flag = 1
        count = count + 1
        pre_CS_count = pre_CS_general(dataset_subset, i, CS_empty_flag, CS_reward_flag, pre_CS_count, intervals_transfer)
        while_CS_count = while_CS_general(dataset_subset, i, CS_empty_flag, CS_reward_flag, while_CS_count,intervals_transfer)
        after_CS_count = after_CS_general(dataset_subset, i, CS_empty_flag, CS_reward_flag, after_CS_count,intervals_transfer)
      } else next
    } else next
  }
  pre_CS_count <-  pre_CS_count / count
  while_CS_count <- while_CS_count / count
  after_CS_count <- after_CS_count / count
  return(list(pre_CS_count, while_CS_count, after_CS_count))
}

general_transfer_count <- function(dataset_subset){
  #intervals_transfer <- ITI_CS(dataset_subset)
  intervals_transfer <- c('ITI' = 6000, 'CS_dur' = 6000)
  version_PIN <- as.character(dataset_subset$version[1])
  color_CS_plus <- colors_CS[which(!colors_CS %in% CS_list[[version_PIN]])]
  color_CS_minus = 'yellow'
  CS_empty_flag = 0
  CS_reward_flag = 0
  count = 0
  pre_CS_count = c('pre_CS_minus_1' = 0,
                   'pre_CS_minus_2' = 0,
                   'pre_CS_minus_3' = 0,
                   #'pre_CS_minus_4' = 0,
                   'pre_CS_plus_1' = 0,
                   'pre_CS_plus_2' = 0,
                   'pre_CS_plus_3' = 0)
  #'pre_CS_plus_4' = 0)
  
  while_CS_count = c('while_CS_minus_1' = 0,
                     'while_CS_minus_2' = 0,
                     'while_CS_minus_3' = 0,
                     'while_CS_minus_4' = 0,
                     'while_CS_minus_5' = 0,
                     'while_CS_minus_6' = 0,
                     'while_CS_plus_1' = 0,
                     'while_CS_plus_2' = 0,
                     'while_CS_plus_3' = 0,
                     'while_CS_plus_4' = 0,
                     'while_CS_plus_5' = 0,
                     'while_CS_plus_6' = 0)
  
  after_CS_count = c('after_CS_minus_1' = 0,
                     'after_CS_minus_2' = 0,
                     'after_CS_minus_3' = 0,
                     #'after_CS_minus_4' = 0,
                     'after_CS_plus_1' = 0,
                     'after_CS_plus_2' = 0,
                     'after_CS_plus_3' = 0)
  #'after_CS_plus_4' = 0)
  for (i in 1:nrow(dataset_subset)){
    if(dataset_subset[i, 'event.type'] == 'image appears'){
      if (grepl(color_CS_minus, dataset_subset[i, 'event.raw.details'])){
        CS_empty_flag = 1
        CS_reward_flag = 0
        count = count + 1
        pre_CS_count = pre_CS_general(dataset_subset, i, CS_empty_flag, CS_reward_flag, pre_CS_count, intervals_transfer)
        while_CS_count = while_CS_general(dataset_subset, i, CS_empty_flag, CS_reward_flag, while_CS_count,intervals_transfer)
        after_CS_count = after_CS_general(dataset_subset, i, CS_empty_flag, CS_reward_flag, after_CS_count,intervals_transfer)
      } else if (grepl(color_CS_plus, dataset_subset[i, 'event.raw.details'])) {
        CS_empty_flag = 0
        CS_reward_flag = 1
        count = count + 1
        pre_CS_count = pre_CS_general(dataset_subset, i, CS_empty_flag, CS_reward_flag, pre_CS_count, intervals_transfer)
        while_CS_count = while_CS_general(dataset_subset, i, CS_empty_flag, CS_reward_flag, while_CS_count,intervals_transfer)
        after_CS_count = after_CS_general(dataset_subset, i, CS_empty_flag, CS_reward_flag, after_CS_count,intervals_transfer)
      } else next
    } else next
  }
  return(list(pre_CS_count, while_CS_count, after_CS_count))
}


pre_CS_general <- function(dataset_subset, entry, CS_empty_flag, CS_reward_flag, pre_CS_count, intervals_transfer){
  cat('start of pre_CS func')
  timest = dataset_subset$timestamp[entry]
  time_dif = timest - intervals_transfer[which(names(intervals_transfer) == 'ITI')] / 2 # the amount of time for pre_CS
  j = entry - 1
  cat(sprintf("\"%f\" \"%f\"\n",timest, time_dif))
  while((dataset_subset$timestamp[j] >= time_dif) && j > 1){
    cat(sprintf("\"%f\" \">=\" \"%f\"\n", dataset_subset$timestamp[j], time_dif))
    cat(sprintf("\"%f\" \"%f\"\n", dataset_subset$timestamp[j], j))
    if ( dataset_subset[j, 'event.type'] =='key press' & (dataset_subset[j, "event.raw.details"] %in% c("39", "37")) & CS_empty_flag == 1){
      print('HERE_empty')
      if(dataset_subset$timestamp[j] >= time_dif){
        if (dataset_subset$timestamp[j] >= (timest - 1000)){
          pre_CS_count[which(names(pre_CS_count) == 'pre_CS_minus_1')] = 
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_minus_1')] + 1
          j = j - 1
          print('CS_minus_1')
        } else if (dataset_subset$timestamp[j] < (timest - 1000) & dataset_subset$timestamp[j] >= (timest - 2000)  ) {
          pre_CS_count[which(names(pre_CS_count) == 'pre_CS_minus_2')] = 
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_minus_2')] + 1
          j = j - 1
          print('CS_minus_2')
        } else if (dataset_subset$timestamp[j] < (timest - 2000) & dataset_subset$timestamp[j] >= (timest - 3000)) {
          pre_CS_count[which(names(pre_CS_count) == 'pre_CS_minus_3')] = 
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_minus_3')] + 1
          j = j - 1
          print('CS_minus_3')
        } else if (dataset_subset$timestamp[j] < (timest - 3000) & dataset_subset$timestamp[j] >= (timest - 4000)){
          pre_CS_count[which(names(pre_CS_count) == 'pre_CS_minus_4')] = 
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_minus_4')] + 1
          j = j - 1
          print('CS_minus_4')
        }
      } else break
    } else if ( dataset_subset[j, 'event.type'] =='key press' & (dataset_subset[j, "event.raw.details"] %in% c("39", "37")) & CS_reward_flag == 1) {
      print('HERE_reward')
      if(dataset_subset$timestamp[j] >= time_dif){
        if (dataset_subset$timestamp[j] >= (timest - 1000)){
          pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_1')] = 
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_1')] + 1
          j = j - 1
          print('CS_plus_1')
        } else if (dataset_subset$timestamp[j] < (timest - 1000) & dataset_subset$timestamp[j] >= (timest - 2000)  ) {
          pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_2')] = 
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_2')] + 1
          j = j - 1
          print('CS_plus_2')
        } else if (dataset_subset$timestamp[j] < (timest - 2000) & dataset_subset$timestamp[j] >= (timest - 3000)) {
          pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_3')] = 
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_3')] + 1
          j = j - 1
          print('CS_plus_3')
        } else if (dataset_subset$timestamp[j] < (timest - 3000) & dataset_subset$timestamp[j] >= (timest - 4000)){
          pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_4')] = 
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_4')] + 1
          j = j - 1
          print('CS_plus_4')
        }
      } else break
    } else j = j - 1; next
  }
  return(pre_CS_count)
}


while_CS_general <- function(dataset_subset, entry, CS_empty_flag, CS_reward_flag, while_CS_count,intervals_transfer){
  cat('while CS working')
  timest = dataset_subset$timestamp[entry]
  cat('The interval is ', intervals_transfer[which(names(intervals_transfer) == 'CS_dur')])
  #time_dif = timest + intervals_transfer[which(names(intervals_transfer) == 'CS_dur')] + 5 # the amount of time for pre_CS
  time_dif = timest + 6005
  j = entry+1
  cat(sprintf("\"%f\" \"%f\"\n",timest, time_dif))
  while(dataset_subset$timestamp[j] <= time_dif & j < nrow(dataset_subset)){
    cat(sprintf("\"%f\" \"%f\"\n", dataset_subset$timestamp[j], j))
    if ( dataset_subset[j, 'event.type'] =='key press' & (dataset_subset[j, "event.raw.details"] %in% c("39", "37")) & CS_empty_flag == 1){
      print('HERE')
      if(dataset_subset$timestamp[j] <= time_dif){
        if (dataset_subset$timestamp[j] <= (timest + 1000)){
          while_CS_count[which(names(while_CS_count) == 'while_CS_minus_1')] = 
            while_CS_count[which(names(while_CS_count) == 'while_CS_minus_1')] + 1
          j = j + 1
        } else if (dataset_subset$timestamp[j] > (timest + 1000) & dataset_subset$timestamp[j] <= (timest + 2000)  ) {
          while_CS_count[which(names(while_CS_count) == 'while_CS_minus_2')] = 
            while_CS_count[which(names(while_CS_count) == 'while_CS_minus_2')] + 1
          j = j + 1
        } else if (dataset_subset$timestamp[j] > (timest + 2000) & dataset_subset$timestamp[j] <= (timest + 3000)) {
          while_CS_count[which(names(while_CS_count) == 'while_CS_minus_3')] = 
            while_CS_count[which(names(while_CS_count) == 'while_CS_minus_3')] + 1
          j = j + 1
        } else if (dataset_subset$timestamp[j] > (timest + 3000) & dataset_subset$timestamp[j] <= (timest + 4000)){
          while_CS_count[which(names(while_CS_count) == 'while_CS_minus_4')] = 
            while_CS_count[which(names(while_CS_count) == 'while_CS_minus_4')] + 1
          j = j + 1
        } else if (dataset_subset$timestamp[j] > (timest + 4000) & dataset_subset$timestamp[j] <= (timest + 5000)){
          while_CS_count[which(names(while_CS_count) == 'while_CS_minus_5')] = 
            while_CS_count[which(names(while_CS_count) == 'while_CS_minus_5')] + 1
          j = j + 1
        } else if (dataset_subset$timestamp[j] > (timest + 5000) & dataset_subset$timestamp[j] <= (timest + 6000)){
          while_CS_count[which(names(while_CS_count) == 'while_CS_minus_6')] = 
            while_CS_count[which(names(while_CS_count) == 'while_CS_minus_6')] + 1
          j = j + 1
        } else break
      } else break
    } else if (dataset_subset[j, 'event.type'] =='key press' & (dataset_subset[j, "event.raw.details"] %in% c("39", "37")) & CS_reward_flag == 1){
      print('HERE')
      if(dataset_subset$timestamp[j] <= time_dif){
        if (dataset_subset$timestamp[j] <= (timest + 1000)){
          while_CS_count[which(names(while_CS_count) == 'while_CS_plus_1')] = 
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_1')] + 1
          j = j + 1
        } else if (dataset_subset$timestamp[j] > (timest + 1000) & dataset_subset$timestamp[j] <= (timest + 2000)  ) {
          while_CS_count[which(names(while_CS_count) == 'while_CS_plus_2')] = 
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_2')] + 1
          j = j + 1
        } else if (dataset_subset$timestamp[j] > (timest + 2000) & dataset_subset$timestamp[j] <= (timest + 3000)) {
          while_CS_count[which(names(while_CS_count) == 'while_CS_plus_3')] = 
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_3')] + 1
          j = j + 1
        } else if (dataset_subset$timestamp[j] > (timest + 3000) & dataset_subset$timestamp[j] <= (timest + 4000)){
          while_CS_count[which(names(while_CS_count) == 'while_CS_plus_4')] = 
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_4')] + 1
          j = j + 1
        } else if (dataset_subset$timestamp[j] > (timest + 4000) & dataset_subset$timestamp[j] <= (timest + 5000)){
          while_CS_count[which(names(while_CS_count) == 'while_CS_plus_5')] = 
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_5')] + 1
          j = j + 1
        } else if (dataset_subset$timestamp[j] > (timest + 5000) & dataset_subset$timestamp[j] <= (timest + 6000)){
          while_CS_count[which(names(while_CS_count) == 'while_CS_plus_6')] = 
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_6')] + 1
          j = j + 1
        } else break
      } else break
    } else j = j + 1; next
  }
  return(while_CS_count)
}


after_CS_general <- function(dataset_subset, entry, CS_empty_flag, CS_reward_flag, after_CS_count,intervals_transfer){
  cat('after_CS piece running\n')
  num = entry + 1
  for (k in num:nrow(dataset_subset)){
    cat('first loop', k, '\n')
    if (grepl('white', dataset_subset[k, 'event.raw.details'])) {
      cat('found white!', k, '\n')
      timest = dataset_subset$timestamp[k]
      time_dif = timest + intervals_transfer[which(names(intervals_transfer) == 'ITI')] / 2 # the amount of time for after_CS
      l = k + 1
      while(dataset_subset$timestamp[l] <= time_dif & l < nrow(dataset_subset)){
        cat('second while', l, '\n')
        if ( dataset_subset[l, 'event.type'] =='key press' & (dataset_subset[l, "event.raw.details"] %in% c("39", "37")) & CS_empty_flag == 1) {
          print('HERE')
          if(dataset_subset$timestamp[l] <= time_dif){
            if (dataset_subset$timestamp[l] <= (timest + 1000)){
              after_CS_count[which(names(after_CS_count) == 'after_CS_minus_1')] = 
                after_CS_count[which(names(after_CS_count) == 'after_CS_minus_1')] + 1
              l = l + 1
            } else if (dataset_subset$timestamp[l] > (timest + 1000) & dataset_subset$timestamp[l] <= (timest + 2000)  ) {
              after_CS_count[which(names(after_CS_count) == 'after_CS_minus_2')] = 
                after_CS_count[which(names(after_CS_count) == 'after_CS_minus_2')] + 1
              l = l + 1
            } else if (dataset_subset$timestamp[l] > (timest + 2000) & dataset_subset$timestamp[l] <= (timest + 3000)  ) {
              after_CS_count[which(names(after_CS_count) == 'after_CS_minus_3')] = 
                after_CS_count[which(names(after_CS_count) == 'after_CS_minus_3')] + 1
              l = l + 1
            }
          } else break
        } else if ( dataset_subset[l, 'event.type'] =='key press' & (dataset_subset[l, "event.raw.details"] %in% c("39", "37")) & CS_reward_flag == 1){
          print('HERE')
          if(dataset_subset$timestamp[l] <= time_dif){
            if (dataset_subset$timestamp[l] <= (timest + 1000)){
              after_CS_count[which(names(after_CS_count) == 'after_CS_plus_1')] = 
                after_CS_count[which(names(after_CS_count) == 'after_CS_plus_1')] + 1
              l = l + 1
            } else if (dataset_subset$timestamp[l] > (timest + 1000) & dataset_subset$timestamp[l] <= (timest + 2000)  ) {
              after_CS_count[which(names(after_CS_count) == 'after_CS_plus_2')] = 
                after_CS_count[which(names(after_CS_count) == 'after_CS_plus_2')] + 1
              l = l + 1
            } else if (dataset_subset$timestamp[l] > (timest + 2000) & dataset_subset$timestamp[l] <= (timest + 3000)  ) {
              after_CS_count[which(names(after_CS_count) == 'after_CS_plus_3')] = 
                after_CS_count[which(names(after_CS_count) == 'after_CS_plus_3')] + 1
              l = l + 1
            }
          }else break
        } else l = l + 1; next
      }
      break
    } else next
  }
  return(after_CS_count)
}


# Specific transfer -----------------------------------------------------------------------------------

specific_transfer <- function(dataset_subset){
  #intervals_transfer <- ITI_CS(dataset_subset)
  intervals_transfer <- c('ITI' = 6000, 'CS_dur' = 6000)
  version_PIN <- as.character(dataset_subset$version[1])
  layout_version <- get_cs_plus(version_PIN)
  flag_CS1 = 0
  flag_CS2 = 0
  count = 0
  pre_CS_count = c('pre_CS_plus_same_1' = 0,
                   'pre_CS_plus_same_2' = 0,
                   'pre_CS_plus_same_3' = 0,
                   #'pre_CS_plus_same_4' = 0,
                   'pre_CS_plus_dif_1' = 0,
                   'pre_CS_plus_dif_2' = 0,
                   'pre_CS_plus_dif_3' = 0)
                   #'pre_CS_plus_dif_4' = 0)
  
  while_CS_count = c('while_CS_plus_same_1' = 0,
                     'while_CS_plus_same_2' = 0,
                     'while_CS_plus_same_3' = 0,
                     'while_CS_plus_same_4' = 0,
                     'while_CS_plus_same_5' = 0,
                     'while_CS_plus_same_6' = 0,
                     'while_CS_plus_dif_1' = 0,
                     'while_CS_plus_dif_2' = 0,
                     'while_CS_plus_dif_3' = 0,
                     'while_CS_plus_dif_4' = 0,
                     'while_CS_plus_dif_5' = 0,
                     'while_CS_plus_dif_6' = 0)
  
  after_CS_count = c('after_CS_plus_same_1' = 0,
                     'after_CS_plus_same_2' = 0,
                     'after_CS_plus_same_3' = 0,
                     #'after_CS_plus_same_4' = 0,
                     'after_CS_plus_dif_1' = 0,
                     'after_CS_plus_dif_2' = 0,
                     'after_CS_plus_dif_3' = 0)
                     #'after_CS_plus_dif_4' = 0)
  for (i in 1:nrow(dataset_subset)){
    print(i)
    if (dataset_subset[i, 'event.type'] == 'image appears') {
      if (grepl(layout_version[1],dataset_subset[i, 'event.raw.details'])){
        flag_CS1 = 1
        flag_CS2 = 0
        count = count + 1
        cat('Main ', i, '\n')
        pre_CS_count = pre_CS(dataset_subset, dataset_subset$timestamp[i], i, flag_CS1, flag_CS2, pre_CS_count, intervals_transfer)
        while_CS_count = while_CS(dataset_subset, dataset_subset$timestamp[i], i, flag_CS1, flag_CS2, while_CS_count,intervals_transfer)
        after_CS_count = after_CS(dataset_subset, i, flag_CS1, flag_CS2, after_CS_count, intervals_transfer)
      } else if (grepl(layout_version[2],dataset_subset[i, 'event.raw.details'])) {
        flag_CS1 = 0
        flag_CS2 = 1
        count = count + 1
        cat('Main ', i, '\n')
        pre_CS_count = pre_CS(dataset_subset, dataset_subset$timestamp[i], i, flag_CS1, flag_CS2, pre_CS_count,intervals_transfer)
        while_CS_count = while_CS(dataset_subset,dataset_subset$timestamp[i], i, flag_CS1, flag_CS2, while_CS_count,intervals_transfer)
        after_CS_count = after_CS(dataset_subset, i, flag_CS1, flag_CS2, after_CS_count,intervals_transfer)
      } else next
    } else next
  }
  pre_CS_count <-  pre_CS_count / count
  while_CS_count <- while_CS_count / count
  after_CS_count <- after_CS_count / count
  return(list(pre_CS_count, while_CS_count, after_CS_count, count))
}


specific_transfer_counts <- function(dataset_subset){
  #intervals_transfer <- ITI_CS(dataset_subset)
  intervals_transfer <- c('ITI' = 6000, 'CS_dur' = 6000)
  version_PIN <- as.character(dataset_subset$version[1])
  layout_version <- get_cs_plus(version_PIN)
  flag_CS1 = 0
  flag_CS2 = 0
  count = 0
  pre_CS_count = c('pre_CS_plus_same_1' = 0,
                   'pre_CS_plus_same_2' = 0,
                   'pre_CS_plus_same_3' = 0,
                   #'pre_CS_plus_same_4' = 0,
                   'pre_CS_plus_dif_1' = 0,
                   'pre_CS_plus_dif_2' = 0,
                   'pre_CS_plus_dif_3' = 0)
  #'pre_CS_plus_dif_4' = 0)
  
  while_CS_count = c('while_CS_plus_same_1' = 0,
                     'while_CS_plus_same_2' = 0,
                     'while_CS_plus_same_3' = 0,
                     'while_CS_plus_same_4' = 0,
                     'while_CS_plus_same_5' = 0,
                     'while_CS_plus_same_6' = 0,
                     'while_CS_plus_dif_1' = 0,
                     'while_CS_plus_dif_2' = 0,
                     'while_CS_plus_dif_3' = 0,
                     'while_CS_plus_dif_4' = 0,
                     'while_CS_plus_dif_5' = 0,
                     'while_CS_plus_dif_6' = 0)
  
  after_CS_count = c('after_CS_plus_same_1' = 0,
                     'after_CS_plus_same_2' = 0,
                     'after_CS_plus_same_3' = 0,
                     #'after_CS_plus_same_4' = 0,
                     'after_CS_plus_dif_1' = 0,
                     'after_CS_plus_dif_2' = 0,
                     'after_CS_plus_dif_3' = 0)
  #'after_CS_plus_dif_4' = 0)
  for (i in 1:nrow(dataset_subset)){
    print(i)
    if (dataset_subset[i, 'event.type'] == 'image appears') {
      if (grepl(layout_version[1],dataset_subset[i, 'event.raw.details'])){
        flag_CS1 = 1
        flag_CS2 = 0
        count = count + 1
        cat('Main ', i, '\n')
        pre_CS_count = pre_CS(dataset_subset, dataset_subset$timestamp[i], i, flag_CS1, flag_CS2, pre_CS_count, intervals_transfer)
        while_CS_count = while_CS(dataset_subset, dataset_subset$timestamp[i], i, flag_CS1, flag_CS2, while_CS_count,intervals_transfer)
        after_CS_count = after_CS(dataset_subset, i, flag_CS1, flag_CS2, after_CS_count, intervals_transfer)
      } else if (grepl(layout_version[2],dataset_subset[i, 'event.raw.details'])) {
        flag_CS1 = 0
        flag_CS2 = 1
        count = count + 1
        cat('Main ', i, '\n')
        pre_CS_count = pre_CS(dataset_subset, dataset_subset$timestamp[i], i, flag_CS1, flag_CS2, pre_CS_count,intervals_transfer)
        while_CS_count = while_CS(dataset_subset,dataset_subset$timestamp[i], i, flag_CS1, flag_CS2, while_CS_count,intervals_transfer)
        after_CS_count = after_CS(dataset_subset, i, flag_CS1, flag_CS2, after_CS_count,intervals_transfer)
      } else next
    } else next
  }
  return(list(pre_CS_count, while_CS_count, after_CS_count, count))
}

pre_CS <- function(dataset_subset, time_of_color, entry, flag1, flag2, pre_CS_count, intervals_transfer){
  cat('Pre_CS piece running\n')
  timest = time_of_color
  time_dif = timest - intervals_transfer[which(names(intervals_transfer) == 'ITI')] / 2
  j = entry - 1
  version <- as.character(dataset_subset$version[1])
  cat(timest, intervals_transfer[which(names(intervals_transfer) == 'ITI')] / 2, time_dif, j, '\n')
  while((dataset_subset$timestamp[j] >= time_dif) && j > 1) {
    cat('while', j, '\n')
    cat(flag1, '\n')
    if ( dataset_subset[j, 'event.type'] =='key press' & (dataset_subset[j, "event.raw.details"] %in% c("39", "37")) & flag1 == 1 ) {
      if ( as.character(dataset_subset[j, "event.raw.details"]) == names(CS_list_digits[[version]][1])){
        if(dataset_subset$timestamp[j] >= time_dif){
          if (dataset_subset$timestamp[j] >= (timest - 1000)){
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_same_1')] = 
              pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_same_1')] + 1
            j = j - 1
            cat('time = ',dataset_subset$timestamp[j],' timest + 1000 = ',  timest + 1000, 'j = ', j,  ' ')
            print('Pre_CS_plus_same_1')
          } else if (dataset_subset$timestamp[j] < (timest - 1000) & dataset_subset$timestamp[j] >= (timest - 2000)  ) {
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_same_2')] = 
              pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_same_2')] + 1
            j = j - 1
            print('Pre_CS_plus_same_2')
          } else if (dataset_subset$timestamp[j] < (timest - 2000) & dataset_subset$timestamp[j] >= (timest - 3000)) {
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_same_3')] = 
              pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_same_3')] + 1
            j = j - 1
            print('Pre_CS_plus_same_3')
          } else if (dataset_subset$timestamp[j] < (timest - 3000) & dataset_subset$timestamp[j] >= (timest - 4000)){
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_same_4')] = 
              pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_same_4')] + 1
            j = j - 1
            print('Pre_CS_plus_same_4')
          }
        } else break
      } else {
        if(dataset_subset$timestamp[j] >= time_dif){
          if (dataset_subset$timestamp[j] >= (timest - 1000)){
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_dif_1')] = 
              pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_dif_1')] + 1
            j = j - 1
            print('Pre_CS_plus_dif_1')
          } else if (dataset_subset$timestamp[j] < (timest - 1000) & dataset_subset$timestamp[j] >= (timest - 2000)  ) {
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_dif_2')] = 
              pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_dif_2')] + 1
            j = j - 1
            print('Pre_CS_plus_dif_2')
          } else if (dataset_subset$timestamp[j] < (timest - 2000) & dataset_subset$timestamp[j] >= (timest - 3000)) {
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_dif_3')] = 
              pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_dif_3')] + 1
            j = j - 1
            print('Pre_CS_plus_dif_3')
          } else if (dataset_subset$timestamp[j] < (timest - 3000) & dataset_subset$timestamp[j] >= (timest - 4000)){
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_dif_4')] = 
              pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_dif_4')] + 1
            j = j - 1
            print('Pre_CS_plus_dif_4')
          }
        } else break
      }
    } else if ( dataset_subset[j, 'event.type'] =='key press' & (dataset_subset[j, "event.raw.details"] %in% c("39", "37")) & flag2 == 1 ){
      if (as.character(dataset_subset[j, "event.raw.details"]) == names(CS_list_digits[[version]][2])){
        if(dataset_subset$timestamp[j] >= time_dif){
          if (dataset_subset$timestamp[j] >= (timest - 1000)){
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_same_1')] = 
              pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_same_1')] + 1
            j = j - 1
            cat('time = ',dataset_subset$timestamp[j],' timest + 1000 = ',  timest + 1000, 'j = ', j,  ' ')
            print('Pre_CS_plus_same_1')
          } else if (dataset_subset$timestamp[j] < (timest - 1000) & dataset_subset$timestamp[j] >= (timest - 2000)  ) {
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_same_2')] = 
              pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_same_2')] + 1
            j = j - 1
            print('Pre_CS_plus_same_2')
          } else if (dataset_subset$timestamp[j] < (timest - 2000) & dataset_subset$timestamp[j] >= (timest - 3000)) {
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_same_3')] = 
              pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_same_3')] + 1
            j = j - 1
            print('Pre_CS_plus_same_3')
          } else if (dataset_subset$timestamp[j] < (timest - 3000) & dataset_subset$timestamp[j] >= (timest - 4000)){
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_same_4')] = 
              pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_same_4')] + 1
            j = j - 1
            print('Pre_CS_plus_same_4')
          }
        } else break
      } else {
        if(dataset_subset$timestamp[j] >= time_dif){
          if (dataset_subset$timestamp[j] >= (timest - 1000)){
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_dif_1')] = 
              pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_dif_1')] + 1
            j = j - 1
            print('Pre_CS_plus_dif_1')
          } else if (dataset_subset$timestamp[j] < (timest - 1000) & dataset_subset$timestamp[j] >= (timest - 2000)  ) {
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_dif_2')] = 
              pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_dif_2')] + 1
            j = j - 1
            print('Pre_CS_plus_dif_2')
          } else if (dataset_subset$timestamp[j] < (timest - 2000) & dataset_subset$timestamp[j] >= (timest - 3000)) {
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_dif_3')] = 
              pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_dif_3')] + 1
            j = j - 1
            print('Pre_CS_plus_dif_3')
          } else if (dataset_subset$timestamp[j] < (timest - 3000) & dataset_subset$timestamp[j] >= (timest - 4000)){
            pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_dif_4')] = 
              pre_CS_count[which(names(pre_CS_count) == 'pre_CS_plus_dif_4')] + 1
            j = j - 1
            print('Pre_CS_plus_dif_4')
          }
        } else break
      }
    } else j = j - 1; next
  }
  return(pre_CS_count)
}


while_CS <- function(dataset_subset, time_of_color, entry, flag1, flag2, while_CS_count, intervals_transfer){
  cat('while_CS piece running\n')
  timest = time_of_color
  time_dif = timest + intervals_transfer[which(names(intervals_transfer) == 'CS_dur')]
  j = entry + 1
  version <- as.character(dataset_subset$version[1])
  print(version)
  cat(timest, intervals_transfer[which(names(intervals_transfer) == 'CS_dur')]+5, time_dif, j, '\n')
  while(dataset_subset$timestamp[j] <= time_dif & j < nrow(dataset_subset)) {
    #cat('while', j, '\n')
    #cat(flag1, '\n')
    cat(j,' ')
    if ( dataset_subset[j, 'event.type'] =='key press' & (dataset_subset[j, "event.raw.details"] %in% c("39", "37")) & flag1 == 1 ) {
      if ( as.character(dataset_subset[j, "event.raw.details"]) == names(CS_list_digits[[version]][1])){
        if(dataset_subset$timestamp[j] <= time_dif){
          if (dataset_subset$timestamp[j] <= (timest + 1000)){
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_1')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_1')] + 1
            j = j + 1
            #print('CS_plus_same_1')
          } else if (dataset_subset$timestamp[j] > (timest + 1000) & dataset_subset$timestamp[j] <= (timest + 2000)  ) {
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_2')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_2')] + 1
            j = j + 1
            #print('CS_plus_same_2')
          } else if (dataset_subset$timestamp[j] > (timest + 2000) & dataset_subset$timestamp[j] <= (timest + 3000)) {
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_3')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_3')] + 1
            j = j + 1
            #print('CS_plus_same_3')
          } else if (dataset_subset$timestamp[j] > (timest + 3000) & dataset_subset$timestamp[j] <= (timest + 4000)){
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_4')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_4')] + 1
            j = j + 1
            #print('CS_plus_same_4')
          } else if (dataset_subset$timestamp[j] > (timest + 4000) & dataset_subset$timestamp[j] <= (timest + 5000)){
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_5')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_5')] + 1
            j = j + 1
            #print('CS_plus_same_5')
          } else if (dataset_subset$timestamp[j] > (timest + 5000) & dataset_subset$timestamp[j] <= (timest + 6000)){
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_6')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_6')] + 1
            j = j + 1
            #print('CS_plus_same_6')
          } else break
        } else break
      } else {
        if(dataset_subset$timestamp[j] <= time_dif){
          if (dataset_subset$timestamp[j] <= (timest + 1000)){
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_1')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_1')] + 1
            j = j + 1
            #print('CS_plus_dif_1')
          } else if (dataset_subset$timestamp[j] > (timest + 1000) & dataset_subset$timestamp[j] <= (timest + 2000)  ) {
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_2')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_2')] + 1
            j = j + 1
            #print('CS_plus_dif_2')
          } else if (dataset_subset$timestamp[j] > (timest + 2000) & dataset_subset$timestamp[j] <= (timest + 3000)) {
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_3')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_3')] + 1
            j = j + 1
            #print('CS_plus_dif_3')
          } else if (dataset_subset$timestamp[j] > (timest + 3000) & dataset_subset$timestamp[j] <= (timest + 4000)){
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_4')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_4')] + 1
            j = j + 1
            #print('CS_plus_dif_4')
          } else if (dataset_subset$timestamp[j] > (timest + 4000) & dataset_subset$timestamp[j] <= (timest + 5000)){
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_5')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_5')] + 1
            j = j + 1
            #print('CS_plus_dif_5')
          } else if (dataset_subset$timestamp[j] > (timest + 5000) & dataset_subset$timestamp[j] <= (timest + 6000)){
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_6')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_6')] + 1
            j = j + 1
            #print('CS_plus_dif_6')
          } else break
        } else break
      }
    } else if ( dataset_subset[j, 'event.type'] =='key press' & (dataset_subset[j, "event.raw.details"] %in% c("39", "37")) & flag2 == 1 ){
      if (as.character(dataset_subset[j, "event.raw.details"]) == names(CS_list_digits[[version]][2])){
        if(dataset_subset$timestamp[j] <= time_dif){
          #cat('here!!!!\n')
          #cat(dataset$timestamp[j], ' diff = ', time_dif)
          
          if (dataset_subset$timestamp[j] <= (timest + 1000)){
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_1')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_1')] + 1
            j = j + 1
            cat('time = ',dataset_subset$timestamp[j],' timest + 1000 = ',  timest + 1000, 'j = ', j,  ' ')
            #print('CS_plus_same_1')
          } else if (dataset_subset$timestamp[j] > (timest + 1000) & dataset_subset$timestamp[j] <= (timest + 2000)  ) {
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_2')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_2')] + 1
            j = j + 1
            #print('CS_plus_same_2')
          } else if (dataset_subset$timestamp[j] > (timest + 2000) & dataset_subset$timestamp[j] <= (timest + 3000)) {
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_3')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_3')] + 1
            j = j + 1
            #print('CS_plus_same_3')
          } else if (dataset_subset$timestamp[j] > (timest + 3000) & dataset_subset$timestamp[j] <= (timest + 4000)){
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_4')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_4')] + 1
            j = j + 1
            #print('CS_plus_same_4')
          } else if (dataset_subset$timestamp[j] > (timest + 4000) & dataset_subset$timestamp[j] <= (timest + 5000)){
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_5')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_5')] + 1
            j = j + 1
            #print('CS_plus_same_5')
          } else if (dataset_subset$timestamp[j] > (timest + 5000) & dataset_subset$timestamp[j] <= (timest + 6000)){
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_6')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_same_6')] + 1
            j = j + 1
            #print('CS_plus_same_6')
          } else break
        } else break
      } else {
        if(dataset_subset$timestamp[j] <= time_dif){
          if (dataset_subset$timestamp[j] <= (timest + 1000)){
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_1')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_1')] + 1
            j = j + 1
            #print('CS_plus_dif_1')
          } else if (dataset_subset$timestamp[j] > (timest + 1000) & dataset_subset$timestamp[j] <= (timest + 2000)  ) {
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_2')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_2')] + 1
            j = j + 1
            #print('CS_plus_dif_2')
          } else if (dataset_subset$timestamp[j] > (timest + 2000) & dataset_subset$timestamp[j] <= (timest + 3000)) {
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_3')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_3')] + 1
            j = j + 1
            #print('CS_plus_dif_3')
          } else if (dataset_subset$timestamp[j] > (timest + 3000) & dataset_subset$timestamp[j] <= (timest + 4000)){
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_4')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_4')] + 1
            j = j + 1
            #print('CS_plus_dif_4')
          } else if (dataset_subset$timestamp[j] > (timest + 4000) & dataset_subset$timestamp[j] <= (timest + 5000)){
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_5')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_5')] + 1
            j = j + 1
            #print('CS_plus_dif_5')
          } else if (dataset_subset$timestamp[j] > (timest + 5000) & dataset_subset$timestamp[j] <= (timest + 6000)){
            while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_6')] = 
              while_CS_count[which(names(while_CS_count) == 'while_CS_plus_dif_6')] + 1
            j = j + 1
            #print('CS_plus_dif_6')
          } else break
        } else break
      }
    } else j = j + 1; next
  }
  return(while_CS_count)
}


after_CS <- function(dataset_subset, entry, flag1, flag2, after_CS_count,intervals_transfer){
  cat('after_CS piece running\n')
  num = entry + 1
  version <- as.character(dataset_subset$version[1])
  for (k in num:nrow(dataset_subset)){
    cat('first loop', k, '\n')
    if (grepl('white', dataset_subset[k, 'event.raw.details'])){
      cat('found white!', k, '\n')
      timest = dataset_subset$timestamp[k]
      time_dif = timest + intervals_transfer[which(names(intervals_transfer) == 'ITI')] / 2 # the amount of time for after_CS
      l = k + 1
      while(dataset_subset$timestamp[l] <= time_dif & l < nrow(dataset_subset)){
        cat('second while', l, '\n')
        if ( dataset_subset[l, 'event.type'] =='key press' & (dataset_subset[l, "event.raw.details"] %in% c("39", "37")) & flag1 == 1){
          if ( as.character(dataset_subset[l, "event.raw.details"]) == names(CS_list_digits[[version]][1])){
            if(dataset_subset$timestamp[l] <= time_dif){
              if (dataset_subset$timestamp[l] <= (timest + 1000)){
                after_CS_count[which(names(after_CS_count) == 'after_CS_plus_same_1')] = 
                  after_CS_count[which(names(after_CS_count) == 'after_CS_plus_same_1')] + 1
                l = l + 1
                print('after_CS_same_1')
              } else if (dataset_subset$timestamp[l] > (timest + 1000) & dataset_subset$timestamp[l] <= (timest + 2000)  ) {
                after_CS_count[which(names(after_CS_count) == 'after_CS_plus_same_2')] = 
                  after_CS_count[which(names(after_CS_count) == 'after_CS_plus_same_2')] + 1
                l = l + 1
                print('after_CS_same_2')
              } else if (dataset_subset$timestamp[l] > (timest + 2000) & dataset_subset$timestamp[l] <= (timest + 3000)  ) {
                after_CS_count[which(names(after_CS_count) == 'after_CS_plus_same_3')] = 
                  after_CS_count[which(names(after_CS_count) == 'after_CS_plus_same_3')] + 1
                l = l + 1
                print('after_CS_same_3')
              }
            } else break 
          } else {
            if(dataset_subset$timestamp[l] <= time_dif){
              if (dataset_subset$timestamp[l] <= (timest + 1000)){
                after_CS_count[which(names(after_CS_count) == 'after_CS_plus_dif_1')] = 
                  after_CS_count[which(names(after_CS_count) == 'after_CS_plus_dif_1')] + 1
                l = l + 1
                print('after_CS_dif_1')
              } else if (dataset_subset$timestamp[l] > (timest + 1000) & dataset_subset$timestamp[l] <= (timest + 2000)  ) {
                after_CS_count[which(names(after_CS_count) == 'after_CS_plus_dif_2')] = 
                  after_CS_count[which(names(after_CS_count) == 'after_CS_plus_dif_2')] + 1
                l = l + 1
                print('after_CS_dif_2')
              } else if (dataset_subset$timestamp[l] > (timest + 2000) & dataset_subset$timestamp[l] <= (timest + 3000)  ) {
                after_CS_count[which(names(after_CS_count) == 'after_CS_plus_dif_3')] = 
                  after_CS_count[which(names(after_CS_count) == 'after_CS_plus_dif_3')] + 1
                l = l + 1
                print('after_CS_dif_3')
              }
            } else break 
          }
        } else if ( dataset_subset[l, 'event.type'] =='key press' & (dataset_subset[l, "event.raw.details"] %in% c("39", "37")) & flag2 == 1 ) {
          if ( as.character(dataset_subset[l, "event.raw.details"]) == names(CS_list_digits[[version]][2])){
            if(dataset_subset$timestamp[l] <= time_dif){
              if (dataset_subset$timestamp[l] <= (timest + 1000)){
                after_CS_count[which(names(after_CS_count) == 'after_CS_plus_same_1')] = 
                  after_CS_count[which(names(after_CS_count) == 'after_CS_plus_same_1')] + 1
                l = l + 1
              } else if (dataset_subset$timestamp[l] > (timest + 1000) & dataset_subset$timestamp[l] <= (timest + 2000)  ) {
                after_CS_count[which(names(after_CS_count) == 'after_CS_plus_same_2')] = 
                  after_CS_count[which(names(after_CS_count) == 'after_CS_plus_same_2')] + 1
                l = l + 1
              } else if (dataset_subset$timestamp[l] > (timest + 2000) & dataset_subset$timestamp[l] <= (timest + 3000)  ) {
                after_CS_count[which(names(after_CS_count) == 'after_CS_plus_same_3')] = 
                  after_CS_count[which(names(after_CS_count) == 'after_CS_plus_same_3')] + 1
                l = l + 1
              }
            } else break 
          } else {
            if(dataset_subset$timestamp[l] <= time_dif){
              if (dataset_subset$timestamp[l] <= (timest + 1000)){
                after_CS_count[which(names(after_CS_count) == 'after_CS_plus_dif_1')] = 
                  after_CS_count[which(names(after_CS_count) == 'after_CS_plus_dif_1')] + 1
                l = l + 1
              } else if (dataset_subset$timestamp[l] > (timest + 1000) & dataset_subset$timestamp[l] <= (timest + 2000)  ) {
                after_CS_count[which(names(after_CS_count) == 'after_CS_plus_dif_2')] = 
                  after_CS_count[which(names(after_CS_count) == 'after_CS_plus_dif_2')] + 1
                l = l + 1
              } else if (dataset_subset$timestamp[l] > (timest + 2000) & dataset_subset$timestamp[l] <= (timest + 3000)  ) {
                after_CS_count[which(names(after_CS_count) == 'after_CS_plus_dif_3')] = 
                  after_CS_count[which(names(after_CS_count) == 'after_CS_plus_dif_3')] + 1
                l = l + 1
              }
            } else break 
          }
        }else l = l + 1; next
      }
      break
    } else next
  }
  return(after_CS_count)
}



for_plot_transfer <- function(groups, group_allocation, transfer_results, pre_time, after_time,while_time ){
  names(transfer_results) <- groups
  ctr <- transfer_results[names(transfer_results) %in% group_allocation$Control]
  cond <- transfer_results[!names(transfer_results) %in% group_allocation$Control]
  # Calculating stats
  ctr_means_while <- colMeans(do.call(rbind, lapply(ctr, function(x) unlist(x[2]) ) )) 
  cond_means_while <- colMeans(do.call(rbind, lapply(cond, function(x) unlist(x[2]) ) ))
  
  ctr_sd_while <- apply(do.call(rbind, lapply(ctr, function(x) unlist(x[2]) )), 2 , sd)
  cond_sd_while <- apply(do.call(rbind, lapply(cond, function(x) unlist(x[2]) )), 2 , sd)
  
  ctr_means_pre <- colMeans(do.call(rbind, lapply(ctr, function(x) unlist(x[1]) )))
  cond_means_pre <- colMeans(do.call(rbind, lapply(cond, function(x) unlist(x[1]) )))
  
  ctr_sd_pre <- apply(do.call(rbind, lapply(ctr, function(x) unlist(x[1]) )), 2 , sd)
  cond_sd_pre <- apply(do.call(rbind, lapply(cond, function(x) unlist(x[1]) )), 2 , sd)
  
  
  ctr_means_after <- colMeans(do.call(rbind, lapply(ctr, function(x) unlist(x[3]) )))
  cond_means_after <- colMeans(do.call(rbind, lapply(cond, function(x) unlist(x[3]) )))
  
  ctr_sd_after <- apply(do.call(rbind, lapply(ctr, function(x) unlist(x[3]) )), 2, sd)
  cond_sd_after <- apply(do.call(rbind, lapply(cond, function(x) unlist(x[3]) )), 2, sd)
  
  
  
  spec_transfer_for_plot_same <- data.frame(time = c(rep(c((-pre_time+1):0, 1:while_time, (while_time + 1):((while_time + 1) + (after_time-1))), 2)), group = c(rep('control', (after_time + pre_time + while_time)), rep('condition',(after_time + pre_time + while_time))),
                                            rate = c( unname(ctr_means_pre)[1:pre_time], unname(ctr_means_while)[1:while_time],unname(ctr_means_after)[1:after_time],
                                                      unname(cond_means_pre)[1:pre_time], unname(cond_means_while)[1:while_time],unname(cond_means_after)[1:after_time]),
                                            error = c( unname(ctr_sd_pre)[1:pre_time], unname(ctr_sd_while)[1:while_time],unname(ctr_sd_after)[1:after_time],
                                                       unname(cond_sd_pre)[1:pre_time], unname(cond_sd_while)[1:while_time],unname(cond_sd_after)[1:after_time]),
                                            act = rep('same', (after_time + pre_time + while_time)*2))
  spec_transfer_for_plot_different <- data.frame(time = c(rep(c((-pre_time+1):0, 1:while_time, (while_time + 1):((while_time + 1) + (after_time-1))), 2)), group = c(rep('control', (after_time + pre_time + while_time)), rep('condition',(after_time + pre_time + while_time))),
                                                 rate = c( unname(ctr_means_pre)[5:(5+(pre_time-1) )], unname(ctr_means_while)[7:(7+(while_time-1))],unname(ctr_means_after)[5:(5+(after_time-1) )],
                                                           unname(cond_means_pre)[5:(5+(pre_time-1))], unname(cond_means_while)[7:(7+(while_time-1))],unname(cond_means_after)[5:(5+(after_time-1))]),
                                                 error = c( unname(ctr_sd_pre)[5:(5+(pre_time-1))], unname(ctr_sd_while)[7:(7+(while_time-1))],unname(ctr_sd_after)[5:(5+(after_time-1))],
                                                            unname(cond_sd_pre)[5:(5+(pre_time-1))], unname(cond_sd_while)[7:(7+(while_time-1))],unname(cond_sd_after)[5:(5+(after_time-1))]),
                                                 act = rep('different', (after_time + pre_time + while_time)*2))
  
  
  return(rbind(spec_transfer_for_plot_same, spec_transfer_for_plot_different))
}



for_plot_transfer_gen <- function(groups, group_allocation, transfer_results, pre_time, after_time,while_time ){
  names(transfer_results) <- groups
  ctr <- transfer_results[names(transfer_results) %in% group_allocation$Control]
  cond <- transfer_results[!names(transfer_results) %in% group_allocation$Control]
  # Extracting the sublists 
  ctr_while <- lapply(ctr, function(x) unlist(x[2]) )
  con_while <- lapply(cond, function(x) unlist(x[2]) )
  
  ctr_pre <- lapply(ctr, function(x) unlist(x[1]) )
  con_pre <- lapply(cond, function(x) unlist(x[1]) )
  
  ctr_after <- lapply(ctr, function(x) unlist(x[3]) )
  con_after <- lapply(cond, function(x) unlist(x[3]) )
  
  
  # Calculating stats
  ctr_means_while <- colMeans(do.call(rbind, ctr_while))  # try lapply ?? too many repetitions 
  cond_means_while <- colMeans(do.call(rbind, con_while))
  
  ctr_sd_while <- apply(do.call(rbind, ctr_while), 2 , sd)
  cond_sd_while <- apply(do.call(rbind, con_while), 2 , sd)
  
  ctr_means_pre <- colMeans(do.call(rbind, ctr_pre))
  cond_means_pre <- colMeans(do.call(rbind, con_pre))
  
  ctr_sd_pre <- apply(do.call(rbind, ctr_pre), 2 , sd)
  cond_sd_pre <- apply(do.call(rbind, con_pre), 2 , sd)
  
  
  ctr_means_after <- colMeans(do.call(rbind, ctr_after))
  cond_means_after <- colMeans(do.call(rbind, con_after))
  
  ctr_sd_after <- apply(do.call(rbind, ctr_after), 2, sd)
  cond_sd_after <- apply(do.call(rbind, con_after), 2, sd)
  
  
  
  gen_transfer_for_plot_CS_min <- data.frame(time = c(rep(c((-pre_time+1):0, 1:while_time, (while_time + 1):((while_time + 1) + (after_time-1))), 2)), group = c(rep('control', (after_time + pre_time + while_time)), rep('condition',(after_time + pre_time + while_time))),
                                            rate = c( unname(ctr_means_pre)[1:pre_time], unname(ctr_means_while)[1:while_time],unname(ctr_means_after)[1:after_time],
                                                      unname(cond_means_pre)[1:pre_time], unname(cond_means_while)[1:while_time],unname(cond_means_after)[1:after_time]),
                                            error = c( unname(ctr_sd_pre)[1:pre_time], unname(ctr_sd_while)[1:while_time],unname(ctr_sd_after)[1:after_time],
                                                       unname(cond_sd_pre)[1:pre_time], unname(cond_sd_while)[1:while_time],unname(cond_sd_after)[1:after_time]),
                                            act = rep('CS-', (after_time + pre_time + while_time)*2))
  gen_transfer_for_plot_CS_plus <- data.frame(time = c(rep(c((-pre_time+1):0, 1:while_time, (while_time + 1):((while_time + 1) + (after_time-1))), 2)), group = c(rep('control', (after_time + pre_time + while_time)), rep('condition',(after_time + pre_time + while_time))),
                                                 rate = c( unname(ctr_means_pre)[5:(5+(pre_time-1) )], unname(ctr_means_while)[7:(7+(while_time-1))],unname(ctr_means_after)[5:(5+(after_time-1) )],
                                                           unname(cond_means_pre)[5:(5+(pre_time-1))], unname(cond_means_while)[7:(7+(while_time-1))],unname(cond_means_after)[5:(5+(after_time-1))]),
                                                 error = c( unname(ctr_sd_pre)[5:(5+(pre_time-1))], unname(ctr_sd_while)[7:(7+(while_time-1))],unname(ctr_sd_after)[5:(5+(after_time-1))],
                                                            unname(cond_sd_pre)[5:(5+(pre_time-1))], unname(cond_sd_while)[7:(7+(while_time-1))],unname(cond_sd_after)[5:(5+(after_time-1))]),
                                                 act = rep('CS+', (after_time + pre_time + while_time)*2))
  
  
  return(rbind(gen_transfer_for_plot_CS_min, gen_transfer_for_plot_CS_plus))
}

for_plot_transfer_oci <- function(groups, group_allocation, transfer_results, pre_time, after_time,while_time ){
  names(transfer_results) <- groups
  ctr <- transfer_results[names(transfer_results) %in% group_allocation$oci_0]
  cond <- transfer_results[!names(transfer_results) %in% group_allocation$oci_0]
  # Calculating stats
  ctr_means_while <- colMeans(do.call(rbind, lapply(ctr, function(x) unlist(x[2]) ) )) 
  cond_means_while <- colMeans(do.call(rbind, lapply(cond, function(x) unlist(x[2]) ) ))
  
  ctr_sd_while <- apply(do.call(rbind, lapply(ctr, function(x) unlist(x[2]) )), 2 , sd)
  cond_sd_while <- apply(do.call(rbind, lapply(cond, function(x) unlist(x[2]) )), 2 , sd)
  
  ctr_sem_while <- mapply(FUN = `/`, ctr_sd_while, sqrt(apply(do.call(rbind, lapply(ctr, function(x) unlist(x[2]) )), 2 , length)))
  cond_sem_while <- mapply(FUN = `/`, cond_sd_while, sqrt(apply(do.call(rbind, lapply(cond, function(x) unlist(x[2]) )), 2 , length)))
  
  ctr_means_pre <- colMeans(do.call(rbind, lapply(ctr, function(x) unlist(x[1]) )))
  cond_means_pre <- colMeans(do.call(rbind, lapply(cond, function(x) unlist(x[1]) )))
  
  ctr_sd_pre <- apply(do.call(rbind, lapply(ctr, function(x) unlist(x[1]) )), 2 , sd)
  cond_sd_pre <- apply(do.call(rbind, lapply(cond, function(x) unlist(x[1]) )), 2 , sd)
  
  ctr_sem_pre <- mapply(FUN = `/`, ctr_sd_pre, sqrt(apply(do.call(rbind, lapply(ctr, function(x) unlist(x[1]) )), 2 , length)))
  cond_sem_pre <- mapply(FUN = `/`, cond_sd_pre, sqrt(apply(do.call(rbind, lapply(cond, function(x) unlist(x[1]) )), 2 , length)))
  
  ctr_means_after <- colMeans(do.call(rbind, lapply(ctr, function(x) unlist(x[3]) )))
  cond_means_after <- colMeans(do.call(rbind, lapply(cond, function(x) unlist(x[3]) )))
  
  ctr_sd_after <- apply(do.call(rbind, lapply(ctr, function(x) unlist(x[3]) )), 2, sd)
  cond_sd_after <- apply(do.call(rbind, lapply(cond, function(x) unlist(x[3]) )), 2, sd)
  
  ctr_sem_after <- mapply(FUN = `/`, ctr_sd_after, sqrt(apply(do.call(rbind, lapply(ctr, function(x) unlist(x[1]) )), 2 , length)))
  cond_sem_after <- mapply(FUN = `/`, cond_sd_after, sqrt(apply(do.call(rbind, lapply(cond, function(x) unlist(x[1]) )), 2 , length)))
  
  
  spec_transfer_for_plot_same <- data.frame(time = c(rep(c((-pre_time+1):0, 1:while_time, (while_time + 1):((while_time + 1) + (after_time-1))))), 
                                            group = c(rep('oci_0', (after_time + pre_time + while_time)), rep('oci_1',(after_time + pre_time + while_time))),
                                            rate = c( unname(ctr_means_pre)[1:pre_time], unname(ctr_means_while)[1:while_time],unname(ctr_means_after)[1:after_time],
                                                      unname(cond_means_pre)[1:pre_time], unname(cond_means_while)[1:while_time],unname(cond_means_after)[1:after_time]),
                                            sd = c( unname(ctr_sd_pre)[1:pre_time], unname(ctr_sd_while)[1:while_time],unname(ctr_sd_after)[1:after_time],
                                                       unname(cond_sd_pre)[1:pre_time], unname(cond_sd_while)[1:while_time],unname(cond_sd_after)[1:after_time]),
                                            sem = c( unname(ctr_sem_pre)[1:pre_time], unname(ctr_sem_while)[1:while_time],unname(ctr_sem_after)[1:after_time]),
                                            act = rep('same', (after_time + pre_time + while_time)*2))
  spec_transfer_for_plot_different <- data.frame(time = c(rep(c((-pre_time+1):0, 1:while_time, (while_time + 1):((while_time + 1) + (after_time-1))))), 
                                                 group = c(rep('oci_0', (after_time + pre_time + while_time)), rep('oci_1',(after_time + pre_time + while_time))),
                                                 rate = c( unname(ctr_means_pre)[4:(5+(pre_time-2) )], unname(ctr_means_while)[7:(7+(while_time-1))],unname(ctr_means_after)[4:(5+(after_time-2) )],
                                                           unname(cond_means_pre)[4:(5+(pre_time-2))], unname(cond_means_while)[7:(7+(while_time-1))],unname(cond_means_after)[4:(5+(after_time-2))]),
                                                 sd = c( unname(ctr_sd_pre)[4:(5+(pre_time-2))], unname(ctr_sd_while)[7:(7+(while_time-1))],unname(ctr_sd_after)[4:(5+(after_time-2))],
                                                            unname(cond_sd_pre)[4:(5+(pre_time-2))], unname(cond_sd_while)[7:(7+(while_time-1))],unname(cond_sd_after)[4:(5+(after_time-2))]),
                                                 sem = c(unname(cond_sem_pre)[4:(5+(pre_time-2))], unname(cond_sem_while)[7:(7+(while_time-1))],unname(cond_sem_after)[4:(5+(after_time-2))]),
                                                 act = rep('different', (after_time + pre_time + while_time)*2))
  
  
  return(rbind(spec_transfer_for_plot_same, spec_transfer_for_plot_different))
}



for_plot_transfer_gen <- function(groups, group_allocation, transfer_results, pre_time, after_time,while_time ){
  names(transfer_results) <- groups
  ctr <- transfer_results[names(transfer_results) %in% group_allocation$Control]
  cond <- transfer_results[!names(transfer_results) %in% group_allocation$Control]
  # Extracting the sublists 
  ctr_while <- lapply(ctr, function(x) unlist(x[2]) )
  con_while <- lapply(cond, function(x) unlist(x[2]) )
  
  ctr_pre <- lapply(ctr, function(x) unlist(x[1]) )
  con_pre <- lapply(cond, function(x) unlist(x[1]) )
  
  ctr_after <- lapply(ctr, function(x) unlist(x[3]) )
  con_after <- lapply(cond, function(x) unlist(x[3]) )
  
  
  # Calculating stats
  ctr_means_while <- colMeans(do.call(rbind, ctr_while))  # try lapply ?? too many repetitions 
  cond_means_while <- colMeans(do.call(rbind, con_while))
  
  ctr_sd_while <- apply(do.call(rbind, ctr_while), 2 , sd)
  cond_sd_while <- apply(do.call(rbind, con_while), 2 , sd)
  
  ctr_means_pre <- colMeans(do.call(rbind, ctr_pre))
  cond_means_pre <- colMeans(do.call(rbind, con_pre))
  
  ctr_sd_pre <- apply(do.call(rbind, ctr_pre), 2 , sd)
  cond_sd_pre <- apply(do.call(rbind, con_pre), 2 , sd)
  
  
  ctr_means_after <- colMeans(do.call(rbind, ctr_after))
  cond_means_after <- colMeans(do.call(rbind, con_after))
  
  ctr_sd_after <- apply(do.call(rbind, ctr_after), 2, sd)
  cond_sd_after <- apply(do.call(rbind, con_after), 2, sd)
  
  
  
  gen_transfer_for_plot_CS_min <- data.frame(time = c(rep(c((-pre_time+1):0, 1:while_time, (while_time + 1):((while_time + 1) + (after_time-1))), 2)), group = c(rep('control', (after_time + pre_time + while_time)), rep('condition',(after_time + pre_time + while_time))),
                                             rate = c( unname(ctr_means_pre)[1:pre_time], unname(ctr_means_while)[1:while_time],unname(ctr_means_after)[1:after_time],
                                                       unname(cond_means_pre)[1:pre_time], unname(cond_means_while)[1:while_time],unname(cond_means_after)[1:after_time]),
                                             error = c( unname(ctr_sd_pre)[1:pre_time], unname(ctr_sd_while)[1:while_time],unname(ctr_sd_after)[1:after_time],
                                                        unname(cond_sd_pre)[1:pre_time], unname(cond_sd_while)[1:while_time],unname(cond_sd_after)[1:after_time]),
                                             act = rep('CS-', (after_time + pre_time + while_time)*2))
  gen_transfer_for_plot_CS_plus <- data.frame(time = c(rep(c((-pre_time+1):0, 1:while_time, (while_time + 1):((while_time + 1) + (after_time-1))), 2)), group = c(rep('control', (after_time + pre_time + while_time)), rep('condition',(after_time + pre_time + while_time))),
                                              rate = c( unname(ctr_means_pre)[5:(5+(pre_time-1) )], unname(ctr_means_while)[7:(7+(while_time-1))],unname(ctr_means_after)[5:(5+(after_time-1) )],
                                                        unname(cond_means_pre)[5:(5+(pre_time-1))], unname(cond_means_while)[7:(7+(while_time-1))],unname(cond_means_after)[5:(5+(after_time-1))]),
                                              error = c( unname(ctr_sd_pre)[5:(5+(pre_time-1))], unname(ctr_sd_while)[7:(7+(while_time-1))],unname(ctr_sd_after)[5:(5+(after_time-1))],
                                                         unname(cond_sd_pre)[5:(5+(pre_time-1))], unname(cond_sd_while)[7:(7+(while_time-1))],unname(cond_sd_after)[5:(5+(after_time-1))]),
                                              act = rep('CS+', (after_time + pre_time + while_time)*2))
  
  
  return(rbind(gen_transfer_for_plot_CS_min, gen_transfer_for_plot_CS_plus))
}

for_plot_transfer_gen_oci <- function(groups, group_allocation, transfer_results, pre_time, after_time,while_time ){
  names(transfer_results) <- groups
  ctr <- transfer_results[names(transfer_results) %in% group_allocation$oci_0]
  cond <- transfer_results[!names(transfer_results) %in% group_allocation$oci_0]
  # Extracting the sublists 
  ctr_while <- lapply(ctr, function(x) unlist(x[2]) )
  con_while <- lapply(cond, function(x) unlist(x[2]) )
  
  ctr_pre <- lapply(ctr, function(x) unlist(x[1]) )
  con_pre <- lapply(cond, function(x) unlist(x[1]) )
  
  ctr_after <- lapply(ctr, function(x) unlist(x[3]) )
  con_after <- lapply(cond, function(x) unlist(x[3]) )
  
  
  # Calculating stats
  ctr_means_while <- colMeans(do.call(rbind, ctr_while))  # try lapply ?? too many repetitions 
  cond_means_while <- colMeans(do.call(rbind, con_while))
  
  ctr_sd_while <- apply(do.call(rbind, ctr_while), 2 , sd)
  cond_sd_while <- apply(do.call(rbind, con_while), 2 , sd)
  
  ctr_sem_while <- mapply(FUN = `/`, ctr_sd_while, sqrt(apply(do.call(rbind, lapply(ctr, function(x) unlist(x[2]) )), 2 , length)))
  cond_sem_while <- mapply(FUN = `/`, cond_sd_while, sqrt(apply(do.call(rbind, lapply(cond, function(x) unlist(x[2]) )), 2 , length)))
  
  ctr_means_pre <- colMeans(do.call(rbind, ctr_pre))
  cond_means_pre <- colMeans(do.call(rbind, con_pre))
  
  ctr_sd_pre <- apply(do.call(rbind, ctr_pre), 2 , sd)
  cond_sd_pre <- apply(do.call(rbind, con_pre), 2 , sd)
  
  ctr_sem_pre <- mapply(FUN = `/`, ctr_sd_pre, sqrt(apply(do.call(rbind, lapply(ctr, function(x) unlist(x[1]) )), 2 , length)))
  cond_sem_pre <- mapply(FUN = `/`, cond_sd_pre, sqrt(apply(do.call(rbind, lapply(cond, function(x) unlist(x[1]) )), 2 , length)))
  
  ctr_means_after <- colMeans(do.call(rbind, ctr_after))
  cond_means_after <- colMeans(do.call(rbind, con_after))
  
  ctr_sd_after <- apply(do.call(rbind, ctr_after), 2, sd)
  cond_sd_after <- apply(do.call(rbind, con_after), 2, sd)
  
  ctr_sem_after <- mapply(FUN = `/`, ctr_sd_after, sqrt(apply(do.call(rbind, lapply(ctr, function(x) unlist(x[1]) )), 2 , length)))
  cond_sem_after <- mapply(FUN = `/`, cond_sd_after, sqrt(apply(do.call(rbind, lapply(cond, function(x) unlist(x[1]) )), 2 , length)))
  
  gen_transfer_for_plot_CS_min <- data.frame(time = c(rep(c((-pre_time+1):0, 1:while_time, (while_time + 1):((while_time + 1) + (after_time-1))), 2)), group = c(rep('oci_0', (after_time + pre_time + while_time)), rep('oci_1',(after_time + pre_time + while_time))),
                                             rate = c( unname(ctr_means_pre)[1:pre_time], unname(ctr_means_while)[1:while_time],unname(ctr_means_after)[1:after_time],
                                                       unname(cond_means_pre)[1:pre_time], unname(cond_means_while)[1:while_time],unname(cond_means_after)[1:after_time]),
                                             sd = c( unname(ctr_sd_pre)[1:pre_time], unname(ctr_sd_while)[1:while_time],unname(ctr_sd_after)[1:after_time],
                                                        unname(cond_sd_pre)[1:pre_time], unname(cond_sd_while)[1:while_time],unname(cond_sd_after)[1:after_time]),
                                             sem = c( unname(ctr_sem_pre)[1:pre_time], unname(ctr_sem_while)[1:while_time],unname(ctr_sem_after)[1:after_time]),
                                             act = rep('CS-', (after_time + pre_time + while_time)*2))
  gen_transfer_for_plot_CS_plus <- data.frame(time = c(rep(c((-pre_time+1):0, 1:while_time, (while_time + 1):((while_time + 1) + (after_time-1))), 2)), group = c(rep('oci_0', (after_time + pre_time + while_time)), rep('oci_1',(after_time + pre_time + while_time))),
                                              rate = c( unname(ctr_means_pre)[4:(5+(pre_time-2) )], unname(ctr_means_while)[7:(7+(while_time-1))],unname(ctr_means_after)[4:(5+(after_time-2) )],
                                                        unname(cond_means_pre)[4:(5+(pre_time-2))], unname(cond_means_while)[7:(7+(while_time-1))],unname(cond_means_after)[4:(5+(after_time-2))]),
                                              sd = c( unname(ctr_sd_pre)[4:(5+(pre_time-2))], unname(ctr_sd_while)[7:(7+(while_time-1))],unname(ctr_sd_after)[4:(5+(after_time-2))],
                                                         unname(cond_sd_pre)[4:(5+(pre_time-2))], unname(cond_sd_while)[7:(7+(while_time-1))],unname(cond_sd_after)[4:(5+(after_time-2))]),
                                              sem = c(unname(cond_sem_pre)[4:(5+(pre_time-2))], unname(cond_sem_while)[7:(7+(while_time-1))],unname(cond_sem_after)[4:(5+(after_time-2))]),
                                              act = rep('CS+', (after_time + pre_time + while_time)*2))
  
  return(rbind(gen_transfer_for_plot_CS_plus, gen_transfer_for_plot_CS_min))
}



qc_complete_participants <- function(dat){
  if (any(dat$stage == 'Close HIT & Questions')){
    print('Ok')
  } else { print('failed')}
}



getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
  