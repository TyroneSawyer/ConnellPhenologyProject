library('dplyr')
library('tidyr')
library('readxl')
library('tibble')
library('purrr')
library('mgcv')
library('itsadug')
library('gratia')
infoMessages('off')
set.seed(1)

#Data Extraction and Cleaning
#Generally just extracting, formatting, pruning data and putting into a tidy data frame with all necessary info.


#process of extracting and sorting data from the connell plot. Notably it's provided in a number of conflicting formats. 
#The data was previously cleaned in 2016 (Connell_plot_data_subsets folder), so the data from 1998 through 2016 are all standardised
#But from 2016 through 2022 is a lot messier. This is unfortunately reflected in this section of code!

dat <- read.csv('data/Connell_plot_data_subsets/cp_1998.csv') %>%tibble()%>%mutate('year' = 1998, .after=Namecode)
dat[dat == ''] = NA #standardising when there was missing data

#formatting data types. Coercion assumptions in the csv import can create issues with binding data
dat[c(2,3,5:16)] <- mutate_all(dat[c(2,3,5:16)], function(x) as.character(x))
dat[c(1,4)] <- mutate_all(dat[c(1,4)], function(x) as.integer(x))


for (i in 1999:2015){
  tmp <- read.csv(paste0('data/Connell_plot_data_subsets/cp_',i,'.csv')) %>%tibble()%>%mutate('year' = i, .after=Namecode)
  if ('Tree' %in% names(tmp)){    #some structures include the Tree variable, which counts the number of members of each species.
    tmp <- select(tmp,-Tree)      #Can be derived from other info, so being dropped. 
  }
  
  #standardising NAs, types, names, for binding
  tmp[tmp == ''] = NA
  tmp[c(2,3,5:16)] <- mutate_all(tmp[c(2,3,5:16)], function(x) as.character(x))
  tmp[c(1,4)] <- mutate_all(tmp[c(1,4)], function(x) as.integer(x))
  names(tmp)[5:16] <- names(dat)[5:16]
  dat <- bind_rows(dat, tmp)
}

files <- c('data/Connell_plot_data_original/Connell plot 2016-2020.xlsx', 'data/Connell_plot_data_original/Connell plot 2021-2022.xlsx')
for (file in files){    
  sheets <- excel_sheets(file)
  for (sheet in sheets){#note that sheet names are the year of collection here!
    tmp <- read_excel(file, sheet = sheet, col_names = TRUE)
    if ('Tree' %in% names(tmp)){    
      tmp <- select(tmp,-Tree)
    }
    #these files include months from the previous year. E.g. the 2016 file is 12-15,1-16,2-16,...,12-16
    #this filtering unfortunately makes strong assumptions about how the data are structured, as it's much faster and easier this way.
    #we take months up until the last column whose title doesn't have a ( in it, as this is present in all the 'Notes (XYZ)' columns
    jan_loc <- which(grepl('Jan', names(tmp)))[1]
    dec_loc <- which(!grepl('(', names(tmp),fixed = TRUE)) %>% tail(1)#gets /last/ month location. some years do /not/ have all months
    tmp <- tmp[c(1:3,jan_loc:dec_loc)] 
    #there are notes below the data, but they're separated by a row of NAs
    tmp <- which(apply(tmp, 1, function(x) all(is.na(x))))[1] %>% `-`(1)%>% slice_head(tmp, n = .)   
    tmp <- mutate(tmp,'year' = sheet, .after = Namecode)
    
    #for data that doesn't have enough months (e.g. 2020), we're just slotting in columns of NAs
    while(ncol(tmp)<16){
      tmp <- add_column(tmp, NA, .name_repair = 'unique')
    }
    tmp[tmp == ''] = NA
    tmp[c(2,3,5:16)] <- mutate_all(tmp[c(2,3,5:16)], function(x) as.character(x))
    tmp[c(1,4)] <- mutate_all(tmp[c(1,4)], function(x) as.integer(x))
    names(tmp)[5:16] <- names(dat)[5:16]
    dat <- bind_rows(dat, tmp)
  }
}
rm(tmp)
#the labels are extraordinarily heterogenous. Whilst some regex process could filter this down a bit, 
#this process is ultimately just going to be fragile and messy. Hence, I use the easy brute solution:
#here is a dictionary with 560 keys.

#if you want to update this, you'll need to amend this manually. Just get a list of the unique readings, 
#set difference out the old ones (the names of this list?) and add in the rest.
#Values are a vector, c('buds','flowers','unripe fruits','fruits')

#look, I'm just putting this in a trivial if statement so RStudio collapses it for me.
if (TRUE){
  labels <- list(
    #
    '0' = c(0,0,0,0),
    '1rf' = c(0,0,0,1),
    '2rf' = c(0,0,0,2),
    '4rf' = c(0,0,0,4),
    '5rf' = c(0,0,0,5),
    #
    '2imft' = c(0,0,2,0),
    '3rf' = c(0,0,0,3),
    '3imft' = c(0,0,3,0),
    '2fl' = c(0,2,0,0),
    '1fl' = c(0,1,0,0),
    '3fl' = c(0,3,0,0),
    #
    '2fr' = c(0,0,0,2),
    '3fr' = c(0,0,0,3),
    '1fr' = c(0,0,0,1),
    '5fr' = c(0,0,0,5),
    '4fr' = c(0,0,0,4),
    '2fbd,2fr' = c(2,0,0,2),
    #
    '1fb' = c(1,0,0,0),
    '2ft' = c(0,0,0,2),
    '1imft' = c(0,0,1,0),
    '5ft' = c(0,0,0,5),
    '3gft' = c(0,0,3,0),
    '1ft' = c(0,0,1,0),
    #
    '3fb' = c(3,0,0,0),
    '2fb' = c(2,0,0,0),
    '4ft' = c(0,0,0,4),
    '3ft' = c(0,0,0,3),
    '1imfb' = c(1,0,0,0),
    '1gfr' = c(0,0,1,0),
    #
    '4gfr' = c(0,0,4,0),
    '2gfr' = c(0,0,2,0),
    '5imft' = c(0,0,5,0),
    '3gfr' = c(0,0,3,0),
    '5imfb' = c(5,0,0,0),
    '5fl' = c(0,5,0,0),
    #
    '2imfb' = c(2,0,0,0),
    '1ft?' = c(0,0,0,1),
    '2fb,1ft' = c(2,0,0,1),
    '0?' = c(0,0,0,0),
    '1bds, 1ft' = c(1,0,0,1),
    '1fl, 1ft' = c(0,1,0,1),
    #
    '1fb, 1ft' = c(1,0,0,1),
    '2imfb, 1ft' = c(2,0,0,1),
    '3fl, 1ft' = c(0,3,0,1),
    '3fbds' = c(3,0,0,0),
    '1fbd' = c(1,0,0,0),
    '1fl,1fb' = c(1,1,0,0),
    #
    '4fb' = c(4,0,0,0),
    '3fb,1fl' = c(3,1,0,0),
    '1' = c(NA,NA,NA,NA),
    '2fbd' = c(2,0,0,0),
    '3fbd' = c(3,0,0,0),
    '?1fls' = c(0,1,0,0),
    #
    '3imfb' = c(3,0,0,0),
    '2fb, 1ft' = c(2,0,0,1),
    '2fb,fl' = c(2,2,0,0),
    'ft??' = c(0,0,0,1),
    '2ft??' = c(0,0,0,2),
    '3/4imfb' = c(3,0,0,0),
    #
    '1ft??' = c(0,0,0,1),
    '?' = c(NA,NA,NA,NA),
    'imfb' = c(NA,NA,NA,NA),
    '4imft?' = c(0,0,4,0),
    '2imft?' = c(0,0,2,0),
    '4fl' = c(0,4,0,0),
    #
    '3imft?' = c(0,0,3,0),
    '2fb,2fl' = c(2,2,0,0),
    '1fb,1imft' = c(1,0,1,0),
    '5fl?' = c(0,5,0,0),
    '4fb,2fl' = c(4,2,0,0),
    '2fb,1fl' = c(2,1,0,0),
    #
    '??' = c(NA,NA,NA,NA),
    '2imfb?' = c(2,0,0,0),
    '5fb' = c(5,0,0,0),
    '1fb,1fl' = c(1,1,0,0),
    '5ft+' = c(0,0,0,5),
    '3fl?' = c(0,0,3,0),
    #
    '4fb/fl' = c(4,4,0,0),
    '2fb?' = c(2,0,0,0),
    '4fl,fb' = c(4,4,0,0),
    '1fb,1ft' = c(1,0,0,1),
    '2fl,fb' = c(2,2,0,0),
    '4fb,1fl' = c(4,1,0,0),
    #
    '2fl,2imft' = c(0,2,2,0),
    '2fl,1ft' = c(0,2,0,1),
    '5fb,fl' = c(5,5,0,0),
    '1fl,2imft' = c(0,1,2,0),
    '3fb,2fl' = c(3,2,0,0),
    '3fb,fl' = c(3,3,0,0),
    #
    '1ft,4imft' = c(0,0,4,1),
    '0??' = c(0,0,0,0),
    '2fb/fl' = c(2,2,0,0),
    'dead?' = c(NA,NA,NA,NA),
    '5fl,fb' = c(5,5,0,0),
    '5fb/fl' = c(5,5,0,0),
    #
    "3fl,imft" = c(0,3,3,0),
    "2bare infl" = c(0,2,0,0),
    "2ft,bare infl" = c(0,2,0,2),
    "5imfb?" = c(5,0,0,0),
    "4fb,fl" = c(4,4,0,0),
    "2ft?" = c(0,0,0,2),
    #
    "4fl/imft" = c(0,4,4,0),
    "2iminfl" = c(2,0,0,0),
    "3iminfl" = c(3,0,0,0),
    "4imft" = c(0,0,4,0),
    "#" = c(NA,NA,NA,NA),
    "DEAD" = c(NA,NA,NA,NA),
    #
    "3fb/fl" = c(3,3,0,0),
    "3imft??" = c(0,0,3,0),
    "2imft,1imfb" = c(1,0,2,0),
    "3fl/ft" = c(0,3,0,3),
    "?3ft" = c(0,0,0,3),
    "5fl/ft?" = c(0,5,0,5),
    #
    "1imft?" = c(0,0,1,0),
    "3ft?" = c(0,0,0,3),
    "2fl?" = c(0,2,0,0),
    "??3imft" = c(0,0,3,0),
    "1fb/fl" = c(1,1,0,0),
    "3fl/fb" = c(3,3,0,0),             
    #
    "5fl/fb" = c(5,5,0,0),
    "1fb?" = c(1,0,0,0),
    "3/4imft" = c(0,0,3,0),
    "4imfb" = c(4,0,0,0),
    "5vimfb" = c(5,0,0,0),
    "3fb,fl?" = c(3,3,0,0),            
    #
    "2fb,3imft" = c(2,0,3,0),
    "???" = c(NA,NA,NA,NA),                 
    "4fl,imft" = c(0,4,4,0),
    "1b" = c(1,0,0,0),
    "3b" = c(3,0,0,0),
    "2b" = c(2,0,0,0),
    #
    "5fbd" = c(5,0,0,0),
    "4fbd" = c(4,0,0,0),
    "1fl,1ft" = c(0,1,0,1),
    "5gfr" = c(0,0,5,0),
    "3imfr" = c(0,0,3,0),
    "2gft" = c(0,0,2,0),               
    #
    "(4ft)" = c(0,0,0,4),
    "(1ft)" = c(0,0,0,1),
    "(2ft)" = c(0,0,0,2),
    "v.imm.ft?" = c(NA,NA,NA,NA),
    "3fls, 1ft" = c(0,3,0,1),
    "2fbds" = c(2,0,0,0),              
    #
    "1imfb?" = c(1,0,0,0),
    "1fbd,1fl" = c(1,1,0,0),
    "4fls" = c(0,4,0,0),
    "2fb??" = c(2,0,0,0),
    "2fl,1fb" = c(1,2,0,0),
    "ft?" = c(0,0,0,1),
    #
    "4ft,2fb" = c(2,0,0,4),
    "1ft,4fb" = c(4,0,0,1),
    "5fb.fl" = c(5,5,0,0),
    "5fbs" = c(5,0,0,0),
    "3fb,2ft" = c(3,0,0,2),
    "4fb,fls" = c(4,4,0,0),
    #
    "1fb,2fl" = c(1,2,0,0),
    "3imfb,1ft" = c(3,0,0,1),
    "2imfb,1ft" = c(2,0,0,1),
    "1fb,fl" = c(1,1,0,0),
    "4fl,2fb" = c(2,4,0,0),
    "4fl,1fb" = c(1,4,0,0),
    #
    "2fb(fl?)" = c(2,1,0,0),
    "3fls" = c(0,3,0,0),
    "3/4fl" = c(0,3,0,0),
    "2fl/imft?" = c(0,2,2,0),
    "3fl.fb" = c(3,3,0,0),
    "3fl,fb" = c(3,3,0,0),
    #
    "4ft (imm?)"  = c(0,0,4,0),
    "3ft (imm?)" = c(0,0,3,0),
    "1fb??" = c(1,0,0,0),
    "5 imft" = c(0,0,5,0),
    "4imfb?" = c(4,0,0,0),
    "4fl/fb" = c(4,4,0,0),             
    #
    "3fb/fl 2ft" = c(3,3,0,2),
    "5fb,1fl" = c(5,1,0,0),
    "1imft, 1fb" = c(1,0,1,0),
    "2fb.fl" = c(2,2,0,0),
    "2infl" = c(0,2,0,0),               
    "2fl,imft" = c(0,2,2,0),           
    #
    "1fb,3imft" = c(1,0,3,0),
    "5fl,imft" = c(0,5,5,0),
    "3imfb?" = c(3,0,0,0),
    "4b" = c(4,0,0,0),
    "3fr,2fb" = c(2,0,3,0),
    "2fl,1imft" = c(0,2,1,0),
    #
    "2fb,2ft" = c(2,0,0,2),
    "deleted" = c(NA,NA,NA,NA),
    "3fbfl" = c(3,3,0,0),
    "4fbfl" = c(4,4,0,0),
    "4fb1fl" = c(4,1,0,0),
    "2fl1fb" = c(1,2,0,0),
    #
    "2fb1fl" = c(2,1,0,0),
    "1fb3fl" = c(1,3,0,0),
    "1fl?" = c(0,1,0,0),  
    "4fb?" = c(4,0,0,0),
    "4imfb,1ft" = c(4,0,0,1),
    "3fb, fl" = c(3,3,0,0),
    #
    "3ft,4imfb" = c(4,0,0,3),
    "2infl?" = c(0,2,0,0),
    "2ft,4imfb" = c(4,0,0,2),
    "4ft??" = c(0,0,0,4),
    "3ft??"  = c(0,0,0,3),
    "3ft,2imfb" = c(2,0,0,3),
    #
    "1ft,4imfb" = c(4,0,0,1),
    "1fl/imft" = c(0,1,1,0),
    "5fls" = c(0,5,0,0),
    "5fl,1ft" = c(0,5,0,1),
    "3fl,1imft" = c(0,3,1,0),
    "3fl,2ft" = c(0,3,0,2),
    #
    "4fl,1ft" = c(0,4,0,1),
    "[0?]" = c(0,0,0,0),
    "1fl,1imft?" = c(0,1,1,0),
    "5fl,1imft" = c(0,5,1,0),
    "1fl (spent)" = c(0,1,0,0),
    "4imft/ft" = c(0,0,4,4),
    #
    "2ft,1fb" = c(1,0,0,2),
    "2fl,2+imft" = c(0,2,2,0),
    "3ft (4?)" = c(0,0,0,3),
    "1fl,1imft" = c(0,1,1,0),
    "2fl, 1imft" = c(0,2,1,0),
    "3fl/imft?" = c(0,3,3,0),
    #
    "4fl/imft?" = c(0,4,4,0),
    "4ft?" = c(0,0,0,4),
    "??3ft" = c(0,0,0,3),
    "3fl,4imft" = c(0,3,4,0),
    "4imft/fl" = c(0,4,4,0),
    "2bds" = c(2,0,0,0),
    #
    "v im infls" = c(NA,NA,NA,NA),
    "imfb?" = c(1,0,0,0),
    "1ft,4infl" = c(0,4,0,1),
    "4infl" = c(0,4,0,0),
    "4 imft" = c(0,0,4,0),
    "3fb, ?imft" = c(3,0,1,0),
    #
    "4/5fl" = c(0,4,0,0),
    "3fb.fl" = c(3,3,0,0),
    "4fb (fl?)" = c(4,1,0,0),
    "3fls,fbs" = c(3,3,0,0),
    "5b" = c(5,0,0,0),
    "2fr,2fb" = c(2,0,0,2),            
    #
    "1fb,2fl,2ft" = c(1,2,0,2),
    "1fb,1fl,1imft" = c(1,1,1,0),
    "1imfr" = c(0,0,1,0),
    "3flfb" = c(3,3,0,0),
    "4fb2fl" = c(4,2,0,0),
    "1fl3fb" = c(3,1,0,0),             
    #
    "4fl1fb" = c(1,4,0,0),
    "1fb1fl" = c(1,1,0,0),
    "3fb1fl" = c(3,1,0,0),
    "2bs" = c(2,0,0,0),
    "3fbifl" = c(3,0,0,0),
    "imfbs?" = c(1,0,0,0),             
    #
    "1imfb,2ft" = c(1,0,0,2),
    "2?" = c(NA,NA,NA,NA),
    "3?" = c(NA,NA,NA,NA),
    "3fl/imft" = c(0,3,3,0),
    "3imfb,1fl" = c(3,1,0,0),
    "4fb,2ft" = c(4,0,0,2),
    #
    "2fb,3fl" = c(2,3,0,0),
    "3fl,1ft" = c(0,3,0,1),
    "4fl,1imft" = c(0,4,1,0),
    "1imfb,2fl" = c(1,2,0,0),
    "2imfb,1fl" = c(2,1,0,0),
    "2fl,2ft" = c(0,2,0,2),            
    #
    "[0]" = c(0,0,0,0),
    "1+imft" = c(0,0,1,0),
    "3fl,2imft" = c(0,3,2,0), 
    "2fb,3+ft" = c(2,0,0,3),
    "1ft,2imfb" = c(2,0,0,1),
    "3(fbd?)" = c(3,0,0,0),            
    #
    "2f/fl" = c(0,2,0,2),
    "2/3 old ft" = c(0,0,0,2),
    "4fl,2imft" = c(0,4,2,0),
    "4fl,3imft" = c(0,4,3,0),
    "1f/fl" = c(0,1,0,1),
    "fb?" = c(1,0,0,0),                
    #
    "3fl, fb" = c(3,3,0,0),
    "3fbs" = c(3,0,0,0),
    "2fls" = c(0,2,0,0),
    "4fl/ft" = c(0,4,0,4),
    "4-5ft" = c(0,0,0,4),
    "2fl/ft" = c(0,2,0,2),             
    #
    "2+ ft" = c(0,0,0,2),
    "4flb" = c(4,0,0,0),
    "Dead" = c(NA,NA,NA,NA),
    "1fr,3fb" = c(3,0,0,1),
    "2fr,1fl" = c(0,1,0,2),
    "2fbd,1fr" = c(2,0,0,1),
    #
    "2imfb,2fb" = c(2,0,0,0),
    "(1fb?)" = c(1,0,0,0),
    "1imfb,1fb" = c(1,0,0,0),
    "(0?)" = c(0,0,0,0),
    "3imfb1fl" = c(3,1,0,0),
    "?2imfb" = c(2,0,0,0),             
    #
    "d" = c(NA,NA,NA,NA),
    "1fb,4fl" = c(1,4,0,0),
    "3fbs?" = c(3,0,0,0),
    "4fbs" = c(4,0,0,0),
    "5fb,3fl" = c(5,3,0,0),
    "2ft,3imfb" = c(3,0,0,2),
    #
    "2fb,1fl,2ft" = c(2,1,0,2),
    "3fb?" = c(3,0,0,0),
    "5fb,5fl" = c(5,5,0,0),
    "[5ft]" = c(0,0,0,5),
    "3fb, 2fl" = c(3,2,0,0),
    "1ft,3imfb" = c(3,0,0,1),          
    #
    "3fl,1fb" = c(1,3,0,0),
    "5fb,2fl" = c(5,2,0,0),
    "5fls,fb" = c(5,5,0,0),
    "3fbd(?)" = c(3,0,0,0),
    "2ft,?fb" = c(1,0,0,2),
    "?1imft" = c(0,0,1,0),
    #
    "2fl,2fb" = c(2,2,0,0),
    "4vimft" = c(0,0,4,0),
    "5vimft" = c(0,0,5,0),
    "1fl,1fr" = c(0,1,0,1),
    "2imfr" = c(0,0,2,0),
    "3fbd,1fl" = c(3,1,0,0),           
    #
    "4fbd,3fl" = c(4,3,0,0),
    "5imfr" = c(0,0,5,0),
    "(?)" = c(NA,NA,NA,NA),
    "3imfb,1fb" = c(3,0,0,0),
    "1imft,1ft" = c(1,0,0,1),
    "1mft" = c(0,0,1,0),
    #
    "1imfbd" = c(1,0,0,0),
    "2imfbd" = c(2,0,0,0),
    "3bds" = c(3,0,0,0),
    "3fb,3fl" = c(3,3,0,0),
    "1bds" = c(1,0,0,0),
    "1ft,5imfb" = c(5,0,0,1),
    #
    "2fls,fb" = c(2,2,0,0),
    "3fls,fb" = c(3,3,0,0),
    "1fb/2fl" = c(1,2,0,0),
    "2fl/1imft" = c(0,2,1,0),
    "?imfb" = c(1,0,0,0),
    "1fl,2ft" = c(0,1,0,2),
    #
    "[2fb,1fl]" = c(2,01,0,0),
    "2fb,4fl" = c(02,04,0,0),
    "4fl, 2fb" = c(02,04,0,0),
    "3fl,2fb" = c(02,03,0,0),
    "2 fl" = c(0,02,0,0),
    "1 fb?" = c(01,0,0,0),              
    #
    "3 fb" = c(03,0,0,0),
    "1 fb" = c(01,0,0,0),
    "2 fb" = c(02,0,0,0),
    "1 fl" = c(0,01,0,0),
    "4 fl" = c(0,04,0,0),
    "4 fb, 1ft" = c(04,0,0,01),
    #
    "3 ft"  = c(0,0,0,03),
    "5 ft"= c(0,0,0,05),
    "4/5 fb"= c(04,0,0,0),
    "2fl & fb" = c(02,02,0,0),
    "3fl & fb"= c(03,03,0,0),
    "4/5fl & fb" = c(04,04,0,0),
    #
    "2fl/fb 2ft" = c(02,02,0,02),
    "2 ft"       = c(0,0,0,02),
    "2 fl & fb"= c(02,0,0,02),
    "0 ft & fl"= c(0,0,0,0),
    "3/4fl,fb.2ft"= c(03,03,0,02),
    "3/4 fb"= c(03,0,0,0),
    #
    "2fl,3fb"= c(03,02,0,0),
    "4/5fl,fb"= c(04,04,0,0),
    "1 ft?"= c(0,0,0,01),
    "2 fb?"= c(02,0,0,0),
    "1ft,2fb"= c(02,0,0,01),
    "2ft,1fl"= c(0,01,0,02),
    #
    "4fb,1ft"= c(04,0,0,01),
    "3fl,3fb" = c(03,03,0,0),
    "?2imft" = c(0,0,02,0),
    "5fls, 2fb" = c(02,05,0,0),
    "2fb,fls"= c(02,02,0,0),
    "3ft,2fb"  = c(02,0,0,03),
    #
    "4fbd,1fl"= c(04,01,0,0),
    "2fb(?)" = c(02,0,0,0),
    "ft"= c(NA,NA,NA,NA),
    "? 3fb"= c(03,0,0,0),
    "? 2fb"= c(02,0,0,0),
    "2bd,1fl"= c(02,01,0,0),
    #
    "?2bd"= c(02,0,0,0),
    "?1bd"= c(01,0,0,0),
    "2imfbds"= c(2,0,0,0),
    "2fb, 2fl"= c(02,02,0,0),
    "1fb,3fl"= c(01,03,0,0),
    "1fb?,1fl"= c(01,01,0,0),
    #
    "5fl+" = c(0,05,0,0),
    "4?"= c(NA,NA,NA,NA),
    "2fb2ft"= c(02,0,0,02),
    "4fl+"= c(0,04,0,0),
    "?1fl"= c(0,01,0,0),
    "1fl,fb"= c(01,01,0,0),
    #
    "1ft/1fb"= c(01,0,0,01),
    "2fl/1fb" = c(01,02,0,0),
    "4fb, 1fl"= c(04,01,0,0),
    "2fb+"= c(02,0,0,0),
    "3fl+" = c(0,03,0,0),
    "4fl?"= c(0,04,0,0),
    #
    "1fl,3imft?" = c(0,01,03,0),
    "3immft" = c(0,0,03,0),
    "2fb/fl?" = c(02,02,0,0),
    "?2fb"= c(02,0,0,0),
    "imfb??"= c(01,0,0,0),
    "1(old)ft?"= c(0,0,0,01),
    #
    "4/5fb,1ft"= c(04,0,0,01),
    "4/5fb,fls"= c(04,04,0,0),
    "2fb,fls?" = c(02,01,0,0),
    "2/3bds"= c(02,0,0,0),
    "4fb,4fl" = c(04,04,0,0),
    "2ft,3fb" = c(03,0,0,02),
    #
    "1fl,2fr"= c(01,0,0,02),
    "5fl "= c(0,05,0,0),
    "3fl " = c(0,03,0,0),
    "4fb "= c(04,0,0,0),
    "1fl " = c(0,01,0,0),
    "1ft " = c(0,0,0,01),
    #
    "3fb2fl"= c(03,02,0,0),
    "2fl1imft"= c(0,02,01,0),
    "2fb1ft"= c(02,0,0,01),
    "?bds"= c(01,0,0,0),
    "3imfbds"= c(03,0,0,0),
    "5imfbds"= c(05,0,0,0),
    #
    "1imfbds"= c(01,0,0,0),
    "4flbds" = c(04,0,0,0),
    "1imft??" = c(0,0,01,0),
    "5fl??" = c(0,05,0,0),
    "2fb,2fl,2ft"= c(02,02,0,02),
    "fbs?"= c(01,0,0,0),
    #
    "2im bds"= c(02,0,0,0),
    "2fl/imft"= c(02,02,0,0),
    "1fr,1fl"= c(0,01,0,01),
    "1ft,1imft"= c(0,0,01,01),
    "1f, 2fb" = c(02,NA,NA,NA),
    "2f,1fb" = c(01,NA,NA,NA),
    #
    "?4fb"= c(04,0,0,0),
    "dead" = c(NA,NA,NA,NA),
    "3imfbs"= c(03,0,0,0),
    "4fbds" = c(04,0,0,0),
    "5fbds,2fls" = c(05,02,0,0),
    "2fbds,5fls"= c(02,05,0,0),
    #
    "3fbds,1fl" = c(03,01,0,0),
    "1fbds,1fl"= c(01,01,0,0),
    "3?fbds" = c(03,0,0,0),
    "2fbs"    = c(02,0,0,0),
    "?1fbs"   = c(01,0,0,0),
    "4fl,2ft"= c(0,04,0,02),
    #
    "5fb,1ft" = c(05,0,0,01),
    "4fbs,fls"= c(04,04,0,0),
    "2fbs,fls"= c(02,02,0,0),
    "5fb,fls" = c(05,05,0,0),
    "5fb?"    = c(05,0,0,0),
    "1fb/ft?" = c(01,0,0,01),
    #
    "4fls,fb" = c(04,04,0,0),
    "1 imft (?)" = c(0,0,01,0),
    "1fb/fl?"   = c(01,01,0,0),
    "2fb,2fl,2imft"= c(02,02,02,0),
    "imft?"  = c(0,0,01,0),
    "fb??" = c(01,0,0,0),
    #
    "4l"= c(0,04,0,0),
    "5fb?fl"= c(05,05,0,0),
    "3+imft"= c(0,0,03,0),
    "5fl +"= c(0,05,0,0),
    "3fb/fl?"= c(03,03,0,0),
    "1fl,3imft"  = c(0,01,03,0),
    #
    "3grft" = c(0,0,03,0),
    "3fb,1ft" = c(03,0,0,1),
    "4fls,2fbd"= c(02,04,0,0),
    "4fls " = c(0,04,0,0),
    "2imft??"= c(0,0,02,0),
    "1fbs?"  = c(01,0,0,0),
    #
    "1fls"   = c(0,01,0,0),
    "4fls, 1imft" = c(0,04,01,0),
    "1fb,4fl,1ft"= c(01,04,0,01),
    "3fb,fls" = c(03,03,0,0),
    "1fb2fl"  = c(01,02,0,0),
    "5fb2fl"  = c(05,02,0,0),
    #
    "2fb2fl"   = c(02,02,0,0),
    "2ft,2fb/fl"= c(02,02,0,02),
    "3/4fb"    = c(3,0,0,0),
    "2fb,fl??"  = c(02,01,0,0),
    "1ft (fallen)" = c(0,0,0,01),
    "2ft, 3fb/fl"= c(03,03,0,02),
    #
    "3/4ft"    = c(0,0,0,03),
    "2ft,3fb/fl"= c(03,03,0,02),
    "1ft, 4fb/fl"= c(04,04,0,01),
    "5fl/imft"  = c(0,05,05,0),
    "4fl,?imft" = c(0,04,01,0),
    "2fl,?imft" = c(0,02,01,0),
    #
    "2fl,imft?" = c(0,02,01,0),
    "5fl,1imft +"= c(0,05,01,0),
    "3infl?"     = c(0,03,0,0),
    "4 fb,fl"    = c(04,04,0,0),
    "5imft?"     = c(0,0,05,0),
    "2fl+"       = c(0,02,0,0),
    #
    "2fb,2imft"  = c(02,0,02,0),
    "1infl?"     = c(0,01,0,0),
    "5fl/imft?"  = c(0,05,05,0),
    "2/3ft"      = c(0,0,0,02),
    "4fl.imft"   = c(0,04,NA,NA),
    "1rf.3fl"    = c(0,03,0,01),
    #
    "fl3"       = c(0,03,0,0),
    "4imfr"      = c(0,0,04,0),
    "tfr"        = c(0,0,NA,NA),
    "3flb"       = c(03,0,0,0),
    "tfb"       = c(NA,0,0,0),
    "3flbd"     = c(03,0,0,0),
    #
    "2imft/2ft"  = c(0,0,02,02),
    "4fls,bds"   = c(04,04,0,0),
    "imft??"     = c(0,0,01,0),
    "2fl,3imft"  = c(0,02,03,0),
    "2ft,1imft"  = c(0,0,01,02),
    "1ft,2fl"    = c(0,02,0,01),
    #
    "4fl/immft"  = c(0,04,04,0),
    "2ft,2fl"    = c(0,02,0,02),
    "2imfb,2ft"  = c(02,0,0,02),
    "2fb,fl?"    = c(02,01,0,0),
    "5 ?imft"    = c(0,0,05,0),
    "1imft,1fb"  = c(01,0,01,0),
    #
    "1fl/imft?"  = c(0,01,01,0),
    "1ft,1imfb"  = c(01,0,0,01),
    "4imft,fl"   = c(0,04,04,0),
    "1fl - single branch"= c(0,01,0,0),
    "3imft(?fb)" = c(01,0,03,0),
    "1ft, 5imft" = c(0,0,05,01),
    #
    "2imft/ft"   = c(0,0,02,02),
    "3imft/ft"    = c(0,0,03,03),
    "1ft, 5fb/fl?"= c(05,NA,0,01),
    "fls?"        = c(0,01,0,0),
    "3fl(top)"    = c(0,03,0,0),
    "3bd/fl?"     = c(03,03,0,0),
    #
    "1ft,5fl/fb"  = c(05,05,0,01),
    "2fb/imft"    = c(02,0,02,0),
    "vimfb?"      = c(01,0,0,0),
    "2fr "        = c(0,0,0,02),
    "3fl,1fr"     = c(0,03,0,01),
    "2fr1fb"      = c(01,0,0,02),
    #
    "imft"        = c(0,0,NA,NA),
    "2immft"      = c(0,0,02,0),
    "fallen"      = c(NA,NA,NA,NA),
    "5?"          = c(NA,NA,NA,NA),
    "2fb/1fl"     = c(02,01,0,0),
    "2imfb,1imft" = c(02,0,01,0),
    #
    "2fb,3fls"    = c(02,03,0,0),
    "4imft,1fl"   = c(0,01,04,0),
    "1fl,4imft"   = c(0,01,04,0),
    "5ft +"       = c(0,0,0,05),
    "5ft "        = c(0,0,0,05),
    "3 +"         = c(NA,NA,NA,NA),
    #
    "2/3imft"     = c(0,0,02,0),
    "3fl (4?)"    = c(0,03,0,0),
    "2fl (top of tree)" = c(0,02,0,0),
    "2ft,4imft"    = c(0,0,04,02),
    "5ft (im?)"   = c(0,0,0,05),
    "2imft (3?)"  = c(0,0,02,0),
    #
    "5imft,1ft"   = c(0,0,05,01),
    "2imft,3imfb" = c(03,0,02,0),
    "5fb/fl?"     = c(05,05,0,0),
    "5fl/v imm ft"= c(0,05,05,0),
    "1fb/2ft"     = c(01,0,0,02),
    "4imfb/fl"    = c(04,04,0,0),
    #
    "1 imft"      = c(0,0,01,0),
    "3fl,imft?"= c(0,03,01,0)
  )
}

#some manual data cleaning borne from inspection. The following data is just 3 years of NAs.
dat <- slice(dat,-which(dat$Namecode == 'SYZYCREB' & dat$Tag == 1020))

# codes KARRBENT and KARABENT both appear, changing in 2018. using KARABENT for consistency
dat$Namecode[dat$Namecode == 'KARRBENT'] <- 'KARABENT'
#Tag 2160 has a superfluous question mark. Removing allows setting tags to doubles
dat$Tag[dat$Tag == '2160?'] <- 2160
dat$Tag <- round(as.double(dat$Tag),2)

# using      dat%>%group_by(Tag,Namecode,decimal_date) %>%summarise(n())%>%View()
# we see that there are a very small set of trees that are not precisely pinned down by Tag+Namecode
# examining No. and sample periods there are some obvious choices for how these should be grouped. 
# I'm assigning simplistic tags to these. negative values to be totally outside of the in use Tagspace
# duplicate tags are ATRABENT,NA, and CUPAFLAG,NA
# in ATRABENT: 8,9 -> -1, 18,20,NA,125 -> -2, 22 -> -3
# in CUPAFLAG: 6 -> -4, 65,68 -> -5 ,NA,113,120,127 -> -6

dat$Tag[is.na(dat$Tag) & dat$Namecode == 'ATRABENT' & dat$No. %in% c(8,9)] <- -1
dat$Tag[is.na(dat$Tag) & dat$Namecode == 'ATRABENT' & dat$No. %in% c(18,20,125)] <- -2
dat <- filter(dat, !(is.na(Tag) & Namecode == 'ATRABENT')) #duplicate data point
dat$Tag[is.na(dat$Tag) & dat$Namecode == 'ATRABENT' & dat$No. == 22] <- -3

dat$Tag[is.na(dat$Tag) & dat$Namecode == 'CUPAFLAG' & dat$No. == 6] <- -4
dat$Tag[is.na(dat$Tag) & dat$Namecode == 'CUPAFLAG' & dat$No. %in% c(65,68)] <- -5
dat$Tag[is.na(dat$Tag) & dat$Namecode == 'CUPAFLAG' & (is.na(dat$No.) | dat$No. %in% c(113,120,127))] <- -6

#and to clean out NA's
dat$Tag[is.na(dat$Tag) & dat$Namecode == 'ACROOBLO'] <- -7
dat$Tag[is.na(dat$Tag) & dat$Namecode == 'DYSORUFU'] <- -8
dat$Tag[is.na(dat$Tag) & dat$Namecode == 'SARCSTIP'] <- -9
dat$Tag[is.na(dat$Tag) & dat$Namecode == 'AKANBIDW'] <- -10
dat$Tag[is.na(dat$Tag) & dat$Namecode == 'HELIGLAB'] <- -11
dat$Tag[is.na(dat$Tag) & dat$Namecode == 'GUILMONO'] <- -12
dat$Tag[is.na(dat$Tag) & dat$Namecode == 'MELIMICR'] <- -13
dat$Tag[is.na(dat$Tag) & dat$Namecode == 'KARABENT'] <- -14
dat$Tag[is.na(dat$Tag) & dat$Namecode == 'EHREACUM'] <- -15
dat$Tag[is.na(dat$Tag) & dat$Namecode == 'ACROPUBE'] <- -16








#some forensic work using start and end dates for recordings reveals a few tag/namecode combos that 
#last for very short periods before other combos start afterwards. Especially as these have NA Tags, I'm
#treating this as if Tag was simply added a little later.
dat$Tag[is.na(dat$Tag) & dat$Namecode == 'VITELIGN'] <- 852

#No. has officially served it's purpose, the set of trees is in injected into Tags
#Proof: group_by(dat, Tag)%>% summarise(length(unique(Namecode)))%>%View()

#preparing to create a pseudo 'tidy dataset', need to pivot longer. Also using 'labels' to get usable reading values
julian_month_list <- list('Jan' = 1, 'Feb' = 2, 'Mar'= 3, 'Apr' = 4, 'May' = 5, 'Jun' = 6,
                          'Jul' = 7, 'Aug' = 8, 'Sep' = 9,'Oct' = 10,'Nov' = 11,'Dec' = 12)

dat <- select(dat,-No.) %>% pivot_longer(4:15,names_to ='julian_month', values_to = 'reading')%>%
  mutate(julian_month = sapply(julian_month, function(x) julian_month_list[[x]]),
         reading = sapply(reading, function(x) labels[[x]]))

dat$reading <- unname(dat$reading)  #left with names as artifact of the named list 
dat <- mutate(dat,decimal_date = year + (julian_month-1)/12)  #easy total ordering

#separating the lists of readings into distinct variables. We also clean up the null values which are tricksy in
#how they interact with various functions. unlist in particular is very dangerous.
dat <-dat%>%
  mutate(buds = map(reading,~.[1]),
         flowers = map(reading,~.[2]),
         green_fruits = map(reading,~.[3]),
         ripe_fruits = map(reading,~.[4]),
         .keep = 'unused')
dat[6:9][apply(dat[6:9],c(1,2),function(x) is.null(unlist(x)))] <- list(NA)
#conveniently this step contains a check that each reading has been associated with a single value. 
#if this sorta region breaks, the most likely cause is perverse values are being fed into the dictionary!
dat <- dat %>%
  mutate(across(6:9, ~unlist(.x)))

#remove months with no data
dat <- filter(dat, !is.na(buds)|!is.na(flowers)|
                !is.na(green_fruits)|!is.na(ripe_fruits))

# # This is just a little diagnostic. checks for the case where a tree tagged one way suddenly changes to another tag
# # with a (degree of) continuity of readings. Much of what it picks up is just when trees replace the dead, but still
# 
# max_dates <- dat%>%summarise(last_reading <- max(decimal_date),.groups = 'keep')
# min_dates <- dat%>%summarise(last_reading <- min(decimal_date),.groups = 'keep')
# differences <- cbind(min_dates, max_dates[,3]-min_dates[,3], max_dates[,3])
# differences[,5] <- differences[,5]+1/12 #off by one error. 
# differences[,3:5] <- round(differences[,3:5],2)
# 
# tolerance <- 0 (maximum delay between when tree stops being recorded and new tree starts)
# receipt <- data.frame()
# for (i in 1:nrow(differences)){
#   targets <- filter(differences, Namecode == differences[[i,2]])
#   j <- which(targets[,3]<differences[[i,5]]+tolerance & targets[,3]>=differences[[i,5]])
#   if (length(j)>0){
#     receipt <- rbind(receipt,differences[i,])
#     receipt <- rbind(receipt,targets[j,])
#   }
# }
# receipt

#climate data! Sourced using ANUClimate 2.0. The method I used to get this is unfortunately not automated.
#to extend you'll need to extract the data from the source. I just used their online OPENDAP tool, copied from 
#an ascii file and used basic string cleaning methods to get a data frame that I've saved as a csv. sorry..
#anyways data is presented as (number of months from January 1998,temp (C°),vpd(hPa)). At time of coding, ANU data stops at 2021

climate_data <- read.csv('data/(-28.225,153.125) climate data_1998_2022.csv')%>%
  tibble
names(climate_data)[1] <- 'month_count'

climate_data <- mutate(climate_data, year = 1998+floor((month_count-1)/12),
                       julian_month = (month_count-1)%%12+1, .keep = 'unused')

dat <-inner_join(dat, climate_data, by=c('year', 'julian_month'))
dat$Tag <- as.factor(dat$Tag)

#so, fruits is just a little bit more stable. Including both, but preferencing fruits
dat$fruits <- apply(cbind(dat$green_fruits, dat$ripe_fruits),1,function(x) max(x,na.rm = T))%>%suppressWarnings()
dat$fruits[dat$fruits==-Inf] <- NA

saveRDS(dat, file = 'outputs/cleaned_data.RDS')
