# load packages
library(gdata)
library(stringr)
library(dplyr)
library(readxl)
library(googlesheets4)

# load data
cap <- 
  read_sheet("https://docs.google.com/spreadsheets/d/1Tp26Z23HSXXZSoGXD4dbP3xukhrY1kWhzQrbtRHt4EY/edit#gid=1382609695", 
             sheet = "Captures", col_types = "c") %>%
  filter(site == "KT") %>% 
  dplyr::select(ring, code, sex, observer) %>% 
  mutate(flag = NA) %>% 
  dplyr::rename(bander = observer) %>% 
  mutate(bander = ifelse(bander == "LEP", "LEH", bander))

filter(cap, code == "MX.OO|XX.OO") %>% pull(ring, sex)
# function to determine which characters have been used in the color combos
find.characters <- function(v1)
{
  x1 <- unique(unlist(strsplit(v1, '')))
  indx <- grepl('[A-Z]', x1)
  c(sort(x1[indx]), sort(x1[!indx]))
}

# Run the function on the data
find.characters(as.character(cap$code))

# replace all incorrect symbols in the data
cap$code <- str_replace(cap$code, pattern = "[/]", replacement = "|")

# Used adult combos (i.e., MX.OO|XX.OO)
used_adult_codes <- cap[which(grepl(
  "[[MX]{1}X\\.[[GWRBYO]{1}[[GWRBYO]{1}\\|[[MX]{1}X.[[GWRBYO]{1}[[GWRBYO]{1}", cap$code)),]
used_adult_codes$code

# Used chick re-ringed combos (e.g., MX.RW|LX.XX)
used_chick_codes <- cap[which(grepl(
  "[[MX]{1}X\\.[[GWRBYOX]{1}[[GWRBYOX]{1}\\|[[MX]{1}X.[[GWRBYOX]{1}[[GWRBYOX]{1}",
  cap$code)),]
used_chick_codes$code

# subset data to include all combos with O on the left tarsus
# cap2 <- 
#   cap[which(grepl(
#     "[[MX]{1}X\\.[[O]{1}[[GWRBYO]{1}\\|[[MX]{1}X.[[GWRBYO]{1}[[GWRBYO]{1}",
#   cap$code)),]

# subset data to include all combos with O on the left tarsus
cap2 <- 
  cap[which(grepl(
    "[[MX]{1}X\\.[[O]{1}[[GWRBYO]{1}\\|[[MX]{1}X.[[GWRBYO]{1}[[GWRBYO]{1}",
    cap$code)),]

# Create an empty data frame for all potential Ceuta color combos
# Ceuta color combos have a red ring above the metal on the left tarsus. 
# Adults are thus assigned three colors for each leg segment in addition to 
# the RM (e.g., BX.RM|WX.OX). For chicks, a metal is given instead to the left
# tibia in addition to a color ring on the right tibia (e.g., MX.XX|LX.XX). 
# Once fledged, these individuals can be reringed with the full combination 
# including two colors on the left tarsus (with the top ring being red) 
# (e.g., MX.XX|LX.XX could be upgraded as MX.RW|LX.OX).
rings <- c("M", "B", "G", "R", "O", "W", "Y")
xx <- as.data.frame(array(, c(7,7)))
names(xx) <- c("col", "B", "G", "R", "O", "W", "Y")
xx$col <- c("", "B", "G", "R", "O", "W", "Y")
xx[1,] <- c("", "B", "G", "R", "O", "W", "Y")

# yy <- data.frame(code=c(
#   'M.OB|X.X', 'M.OG|X.X', 'M.OR|X.X', 'M.OO|X.X', 'M.OW|X.X', 'M.OL|X.X', 'M.OY|X.X'), id <- 1:7)
yy <- data.frame(code=c(
  'M.OB|XX', 'M.OG|XX', 'M.OR|XX', 'M.OO|XX', 'M.OW|XX', 'M.OY|XX'), id <- 1:6)

zz <- merge(yy, xx, all.x=T, all.y=T); zz <- zz[order(zz$id),]
zz[c(1:25),]

# Check if multiple color combinations were associated with individual metal rings
ring <- unique(cap2$ring)
for(i in ring){
  ss <- length(unique(cap2$sex[cap2$ring%in%i]))
  if(ss>1){cat(i, as.character(unique(cap2$sex[cap2$ring%in%i])), '\n')}}

#check the number of birds banded with the same combo
cr <- table(cap2$ring, cap2$code)
for (i in 1:ncol(cr)){
  x <- sum(cr[,i]>0)
  if(x>2){cat(dimnames(cr)[[2]][i], ' - ', x, '\n')}}

# List of individual color combinations in the data frame
code <- as.character(sort(unique(cap2$code)))
length(code) #26 unique color combos

# These lines will only check if number of full combinations is equal to the 
# number of total color combinations in the capture files. This is only used 
# for a check in the end, to see if all color combos were included in the color 
# combo sheets
fc0 <- 0; FC <- c() # number of full combos; list of full combos

i="MX.OO|XX.OO"

for(i in code){
  # split combo by position
  r1 <- str_sub(unlist(strsplit(i, "[.]"))[2], 1, 1) # W
  r2 <- str_sub(unlist(strsplit(i, "[.]"))[2], 2, 2) # W
  r3 <- str_sub(unlist(strsplit(i, "[.]"))[3], 1, 1) # R
  r4 <- str_sub(unlist(strsplit(i, "[.]"))[3], 2, 2) # O
  r5 <- substr(i, 1, 1) # M
  
  # r5 <- str_sub(unlist(strsplit(i, "[.]"))[3], 2, 2) # O
  # r6 <- str_sub(unlist(strsplit(i, "[.]"))[3], 2, 2) # O
  
  # if colors in all 4 positions are included in the "rings" (see line 14)
  if( sum(c(r1,r2,r3,r4)%in%rings) == 4){  
    if("M" %in% c(r1,r2,r3,r4,r5)){fc0 <- fc0+1; FC <- c(FC, i)}}}

fc0 == length(code) # number of combos = number of full combos
# IT NEEDS CHECKS IF ITS FALSE - it means that some combinations might not be 
# included in the color combo sheet

fc <- 0; FC <- c()

code <- as.character(sort(unique(cap2$code)))
for(i in code){
  # extract the combo on each leg segment
  # E.g. "BX.RM|BX.BX"
  r1 <- substr(unlist(strsplit(unlist(strsplit(i, "[.]"))[2], "[|]"))[1], 1, 1) # W
  r2 <- substr(unlist(strsplit(unlist(strsplit(i, "[.]"))[2], "[|]"))[1], 2, 2) # W
  r3 <- substr(unlist(strsplit(i, "[.]"))[3], 1, 1) # R
  r4 <- substr(unlist(strsplit(i, "[.]"))[3], 2, 2) # O
  r5 <- substr(i, 1, 1) # M
  
  
  
  # r1 <- substr(unlist(strsplit(unlist(strsplit(i, "[.]"))[1], "[X]")) # B
  # r2 <- unlist(strsplit(unlist(strsplit(unlist(strsplit(i, "[.]"))[2], "[|]"))[1], "[W]"))[2] # M
  # r3 <- unlist(strsplit(unlist(strsplit(unlist(strsplit(i, "[.]"))[2], "[|]"))[2], "[X]"))[1] # B
  # r4 <- unlist(strsplit(unlist(strsplit(i, "[.]"))[3], "[X]")) # B
  if( sum(c(r1, r2, r3, r4, r5)%in%rings) == 5){
    if("M" %in% c(r1, r2, r3, r4, r5)){fc <- fc + 1; FC <- c(FC, i)}}
  
  # Insert the metal ring and sex into each color combo cell of the table
  # For adults (i.e., CRMXX) ---> C = color
  # if(r2 %in% "M"){
  #   codei <- paste('M', '.W', r1, '|', 'X', '.', 'X', sep='')
  #   if(length(unique(cap2$ring[cap2$code%in% i]))==1){
  #     R1 <- as.character(unique(cap2$ring[cap2$code%in% i]))
  #     zz[zz$code%in%codei & zz$col%in%r3, names(zz)%in%r4] <-
  #       paste(R1, unique(cap2$sex[cap2$ring%in%R1]),sep='_')}
  #   
  #   if(length(unique(cap2$ring[cap2$code%in% i]))==2){
  #     R1 <- as.character(unique(cap2$ring[cap2$code%in% i])[1])
  #     R2 <- as.character(unique(cap2$ring[cap2$code%in% i])[2])
  #     zz[zz$code%in%codei & zz$col%in%r3, names(zz)%in%r4] <- 
  #       paste(paste(R1,unique(cap2$sex[cap2$ring%in%R1]),sep='_'), 
  #             paste(R2,unique(cap2$sex[cap2$ring%in%R2]),sep='_'), sep=', ')}
  #   
  #   if(length(unique(cap2$ring[cap2$code%in% i]))==3){	
  #     R1 <- as.character(unique(cap2$ring[cap2$code%in% i])[1])
  #     R2 <- as.character(unique(cap2$ring[cap2$code%in% i])[2])
  #     R3 <- as.character(unique(cap2$ring[cap2$code%in% i])[3])
  #     zz[zz$code%in%codei & zz$col%in%r3, names(zz)%in%r4] <- 
  #       paste(  paste(R1,unique(cap2$sex[cap2$ring%in%R1]),sep='_'), 
  #               paste(R2,unique(cap2$sex[cap2$ring%in%R2]),sep='_'),
  #               paste(R3,unique(cap2$sex[cap2$ring%in%R3]),sep='_'), sep=', ') }}
  
  # For chicks (i.e., MRCXX) ---> C = color
  # if(r1 %in% "M"){
  codei <- paste('M', '.O', r2, '|', 'XX', sep='')
  if(length(unique(cap2$ring[cap2$code%in% i]))==1){
    R1 <- as.character(unique(cap2$ring[cap2$code%in% i]))
    zz[zz$code%in%codei & zz$col%in%r3, names(zz)%in%r4] <-
      paste(R1, unique(cap2$sex[cap2$ring%in%R1]),sep='_')}
  
  if(length(unique(cap2$ring[cap2$code%in% i]))==2){
    R1 <- as.character(unique(cap2$ring[cap2$code%in% i])[1])
    R2 <- as.character(unique(cap2$ring[cap2$code%in% i])[2])
    zz[zz$code%in%codei & zz$col%in%r3, names(zz)%in%r4] <- 
      paste(paste(R1,unique(cap2$sex[cap2$ring%in%R1]),sep='_'), 
            paste(R2,unique(cap2$sex[cap2$ring%in%R2]),sep='_'), sep=', ') } 
  
  if(length(unique(cap2$ring[cap2$code%in% i]))==3){
    R1 <- as.character(unique(cap2$ring[cap2$code%in% i])[1])
    R2 <- as.character(unique(cap2$ring[cap2$code%in% i])[2])
    R3 <- as.character(unique(cap2$ring[cap2$code%in% i])[3])
    zz[zz$code%in%codei & zz$col%in%r3, names(zz)%in%r4] <- 
      paste(  paste(R1,unique(cap2$sex[cap2$ring%in%R1]),sep='_'), 
              paste(R2,unique(cap2$sex[cap2$ring%in%R2]),sep='_'),
              paste(R3,unique(cap2$sex[cap2$ring%in%R3]),sep='_'), sep=', ') }}#}

# Check the head of the spreadsheet
head(zz)

# remove the first row and first column
zz1 <- zz[-1,-2]; head(zz1)

# check one of the tables (in this case recruits with RY)
filter(zz1, code == "M.OO|XX")

# check that all combos are included in the sheet
zz2 <- zz1[!zz1$col %in%"" ,]
zzFC<- c()
for (i in 1:nrow(zz2)){
  for(j in 3:ncol(zz2)){
    c <- zz2[i,j]
    if(!is.na(c) & !is.na(zz2$col[i])) {
      cd  <- sub("X", names(zz2)[j], sub("X", zz2$col[i], zz2$code[i]))
      cd1 <- paste(strsplit(cd, NULL)[[1]][1], "X.O", strsplit(cd, NULL)[[1]][4],
                   "|XX.", strsplit(cd, NULL)[[1]][6], strsplit(cd, NULL)[[1]][7], sep='')
      zzFC <- c(zzFC, cd1)}}}

# IF TRUE THE NUMBER OF FULL COMBOS IN CAPTURE FILE IS EQUAL TO THE NUMBER OF 
# FULL COMBOS IN THE CREATED DATA FILE --> ALL GOOD!!!
length(zzFC) == fc 

# IF THE PREVIOUS STATEMENT IS FALSE -> CHECKS ARE NEEDED
# These two should both be empty if all is good:

# combinations that are included in the combo sheet but are not found in the capture file
zzFC[ !zzFC%in%FC]

# combinations that are in the capture file but were not included in the combo sheet
FC[ !FC %in% zzFC]

# number of full combinations in the capture file are equal to the number of combos in the combo sheet
length(zzFC) == fc0 

# write the final sheet to disk
# write.table(zz1, "fieldwork/Ceuta_color_combos_170420_SNPL.txt", na='', row.names=F, sep = "\t") # Write the combo sheet to file
write.table(zz1, "sheets/2024/2024_Kaitorete_Banded_Dotterel_Band_Combinations.txt", na='', row.names=F, sep = "\t") # Write the combo sheet to file

