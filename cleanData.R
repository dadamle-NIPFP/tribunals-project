library(tidyverse)
library(stringr)

# Current status: We have many dates that we fear are wrong. So we are trying to
# figure out nextDates, so that we can check date with nextDate and storyDate.
# To do this, we need nextdates. For that, in some cases, when the case is
# closed, there is no nextDate, and we need to record that as nextDate=NA. All
# nextDates we haven't gotten around to figuring out so far we set to 1-1-1970.

unexaminedDate <- as.Date("1970-1-1")
caseClosedDate <- as.Date("1970-1-2")
ignoreDate <- as.Date("1970-1-3") #This date will never appear in hearings$nextDate.
unknownDate <- as.Date("1970-1-4") 

IsNextDateSet <- function(hearings) {
  return(hearings$nextDate != unexaminedDate)
}

SetReserved <- function(hearings, pattern, ignore_case=TRUE) {
  dateFormat <- "(\\d{2}\\.\\d{2}\\.\\d{4})"
  pattern <- paste0(pattern, dateFormat)
  print(pattern)
  matchedDates <- str_match(hearings$story, regex(pattern, ignore_case=ignore_case))[,2]
  matchedDates <- as.Date(matchedDates, "%d.%m.%Y")
  hearings <- SetNextDate(hearings, matchedDates)
  return(SetStatus(hearings, !is.na(matchedDates), "reserved"))
}

SetAdjournedUnknown <- function(hearings, pattern, ignore_case=TRUE) {
  print(pattern)
  matches <- str_detect(hearings$story, regex(pattern, ignore_case=ignore_case))
  dates <- hearings$nextDate
  dates <- as.Date(NA)
  dates[matches] <- unknownDate
  hearings <- SetNextDate(hearings, dates)
  return(SetStatus(hearings, !is.na(dates), "adjourned"))
}

SetAdjourned <- function(hearings, pattern, ignore_case=TRUE) {
  dateFormat <- "(\\d{2}\\.\\d{2}\\.\\d{4})"
  pattern <- paste0(pattern, dateFormat)
  print(pattern)
  matchedDates <- str_match(hearings$story, regex(pattern, ignore_case=ignore_case))[,2]
  matchedDates <- as.Date(matchedDates, "%d.%m.%Y")
  hearings <- SetNextDate(hearings, matchedDates)
  return(SetStatus(hearings, !is.na(matchedDates), "adjourned"))
}

# We next hearings$nextDate to inputDates wherever inputDates is valid and there
# is no conflict with any previously set nextDate.
SetNextDate <- function(hearings, inputDates) {
  validDates <- !is.na(inputDates)
  inputDates[!validDates] <- ignoreDate # We set it to ignoreDate here so that the next line (inputDates!=hearings$nextDate) goes through cleanly.
  problems <- validDates & IsNextDateSet(hearings) & (inputDates != hearings$nextDate)
  noproblems <- validDates & !problems
  cat("Setting", sum(noproblems), "nextDates.\n")
  if (any(problems)) cat("NextDate Problems:", which(problems), ".\n")
  hearings$nextDate[noproblems] <- inputDates[noproblems]
  return(hearings)
}

SetFinalStatus <- function(hearings, pattern, status) {
  matches <- str_detect(hearings$story, fixed(pattern, ignore_case=TRUE))
  print(pattern)
  cat("Setting", sum(matches), "final status statuses\n")
  hearings <- SetStatus(hearings, matches, status)
#  dates <- hearings$nextDate
#  dates <- as.Date(NA)
  dates <- rep(as.Date(NA), times=length(hearings$nextDate))
  dates[matches] <- caseClosedDate
  cat("Setting", sum(!is.na(dates)), "final status dates\n")
  hearings <- SetNextDate(hearings, dates)
  return(hearings)
}

SetNextDateAndStatus <- function(hearings, pattern, date, status) {
  matches <- str_detect(hearings$story, regex(pattern, ignore_case=TRUE))
  print(pattern)
  cat("Setting", sum(matches), "status statuses\n")
  hearings <- SetStatus(hearings, matches, status)
  dates <- hearings$nextDate
  dates <- as.Date(NA)
  dates[matches] <- date
  cat("Setting", sum(!is.na(dates)), "dates\n")
  hearings <- SetNextDate(hearings, dates)
  return(hearings)
}

SetStatus <- function(hearings, matches, status){
  problems <- matches & hearings$status != "unset" & hearings$status != status
  noproblems <- matches & !problems
  cat("Setting", sum(noproblems), "statuses\n")
  if (any(problems)) cat("Status Problems: ", which(problems), ".\n")
  hearings$status[noproblems] <- status
  return(hearings)
}

SetStdCaseNumber <- function(hearings) {
  hearings <- hearings %>% separate(caseNo, into=c("type", "caseNumber", "year"), remove=FALSE)
  stopifnot(all(nchar(hearings$type)==2))
  hearings$caseNumber <- strtoi(hearings$caseNumber, base=10L)
  if (anyNA(hearings$caseNumber)) stop("Bad caseNumber")
  hearings$year <- strtoi(hearings$year, base=10)
  hearings$stdCaseName<- paste(hearings$type, sprintf("%03d", hearings$caseNumber), hearings$year, sep = "")
}

GetBefore <- function(shortstory) {
  for (word in rev(shortstory)) {
    word <- tolower(word)
    if (word == "recovery") return("RO") 
    if (word == "presiding") return("PO") 
    if (word == "registrar" || word == "registar" || word == "registra" || word == "registrarr") return("RG") 
  }
  return(NA)
}

source("readJSON.R")
rawdata <- getRawData()

hearings <- as_tibble(rawdata)
hearings <- hearings %>% mutate(date=as.Date(date, "%d/%m/%Y"))

appeals <- hearings %>% filter(appealNo != "")
nonappeals <- hearings %>% filter(appealNo == "")

appeals <- appeals %>%
              separate(appealNo, into=c("type", "caseNumber", "year"), remove=FALSE) %>%
              separate(caseNo, into=c("oldType", "oldCaseNumber", "oldYear"), remove=FALSE) %>%
              mutate(caseNumber = strtoi(caseNumber, base=10)) %>% 
              mutate(oldCaseNumber = strtoi(oldCaseNumber, base=10)) %>%
              mutate(year = strtoi(year, base=10)) %>%
              mutate(oldYear = strtoi(oldYear, base=10)) %>%
              mutate(oldName = paste(oldType, sprintf("%03d", caseNumber), year, sep = ""))

nonappeals <- nonappeals %>%
              separate(caseNo, into=c("type", "caseNumber", "year"), remove=FALSE) %>%
              mutate(caseNumber = strtoi(caseNumber, base=10)) %>% 
              mutate(year = strtoi(year, base=10))  %>%
              mutate(oldType = NA, oldCaseNumber = NA, oldYear = NA, oldName = NA)

hearings <- rbind(appeals, nonappeals)
if (anyNA(hearings$type) || anyNA(appeals$oldType)) stop("Bad type.")
if (anyNA(hearings$caseNumber) || anyNA(appeals$oldCaseNumber)) stop("Bad casenumber")
if (anyNA(hearings$year) || any(hearings$year < 1990) || any(hearings$year > 2016)) stop("Bad year")
if (anyNA(appeals$oldYear) || any(appeals$oldYear < 1990) || any(appeals$oldYear > 2016)) stop("Bad year")

hearings <- hearings %>%
              filter(type %in% c("OA", "SA", "RC")) %>%
              mutate(stdName = paste(type, sprintf("%03d", caseNumber), year, sep = "")) %>%
              select(-caseNo, -appealNo, -caseNumber, -oldCaseNumber) %>%
              mutate(before = NA, status = "unset")

hearings$before[grep("^RECOVERY OFFICER", hearings$story)] <- "RO"
nobefore <- is.na(hearings$before)
lastwords <- str_sub(hearings$story[nobefore], start = -180)
lastwords <- str_split(lastwords, boundary("word"))
hearings$before[nobefore] <- sapply(lastwords, GetBefore)
# There are only about 6 hearings left:
#> which(is.na(hearings$before))
#[1]  456 3400 7411 8334 8342 8466
# We do manually.
hearings$before[8334] <- "RG"
hearings$before[8342] <- "RO"
hearings$before[8466] <- "RG"

#------------------failure------------------

hearings$failure <- NA
hearings$failure[grep("same purpose for which it was listed today", hearings$story)] <- TRUE


mylist <- grep("requested .* time to suggest further course of action. Request allowed", hearings$story)
regdirected <- grep("registry is directed", hearings$story)
regdirectedfresh <- grep("registry is directed to issue fresh", hearings$story)
issuefresh <- grep("issue fresh", hearings$story)

debug()

adjRequestFormat <- "request.*for adjournment.*|requested for some more time"
adjRequestMade <- grepl(adjRequestFormat, hearings$story)
requestAllowed <- grepl("Request allowed", hearings$story)
adjRequestAllowed <- adjRequestMade & requestAllowed
nonAdjReqAllowed <- requestAllowed & !adjRequestAllowed

lastFinalOpportunity <- "last and final opportunity"
failingWhichCost <- "failing which cost"

lastChance <-
#-------------------------------------------
storyDates <- str_extract(hearings$story, "\\d{2}.\\d{2}.\\d{4}") # Extracts the first date from the body. This is likely to be more accurate than the date field.
hearings$storyDate <- as.Date(storyDates, "%d.%m.%Y")

#TODO: check out > which(hearings$caseNumber > 999)
#[1] 1410

# Some hearings have no 'story', and that is ok.
# But one hearing, with date 2016-09-01 and computerID "40000", has the stories of a bunch of other hearings in it.
# I am editing it here.
selector=which(hearings$date=="2016-9-1" & hearings$computerID=="40000")
hearings$story[selector] <- trimws(strsplit(hearings$story[selector], "DEBTS RECOVERY TRIBUNAL-III, DELHI")[[1]][2])

# Another hearing, with date 2016-11-3 and computerId 58334, has the same problem.
# I am editing it here.
selector=which(hearings$date=="2016-11-3" & hearings$computerID=="58334")
hearings$story[selector] <- trimws(strsplit(hearings$story[13560], "DRT-III, Delhi\\.")[[1]][1])

hearings$nextDate <- unexaminedDate
# Fixing some dates manually:
selector <- which(hearings$date=="2016-3-21" & hearings$stdName=="RC0462015")
hearings$nextDate[selector] <- as.Date("04.04.2016", "%d.%m.%Y")
# > hearings[grep("\\d+\\.\\d+\\.\\d+\\.\\d+", hearings$story),]$story reveals a date of "21.0.3.2016". Fixing it here:
selector <- grep("\\d+\\.\\d+\\.\\d+\\.\\d+", hearings$story)
hearings$nextDate[selector] <- as.Date("21.03.2016", "%d.%m.%Y")


# Fix a few things first. There are some cases where the date format is "dd.mm .yyyy":
hearings$story <- gsub("(\\d{2}\\.\\d{2}) (\\.\\d{4})", "\\1\\2", hearings$story)
# Another cases where the date format is "dd.mm. yyyy":
hearings$story <- gsub("(\\d{2}\\.\\d{2}\\.) (\\d{4})", "\\1\\2", hearings$story)
# Case where the date format is dd.m.yyyy: We need to put a 0
hearings$story <- gsub("(\\d{2}\\.)(\\d\\.20\\d{2})", "\\10\\2", hearings$story)
# Case where the date format is dd. mm.yyyy
hearings$story <- gsub("(\\d{2}\\.) (\\d{2}\\.20\\d{2})", "\\1\\2", hearings$story)
# There is one case where a "List the case on" string needs fixing"
hearings$story <- gsub("(List the case on \\d{2}\\.\\d{2}\\.)(\\d{3}\\.)", "\\12\\2", hearings$story)
#List the case on 21.04.016.

hearings$story <- gsub("(\\d{2})/(\\d{2})/(\\d{4})", "\\1.\\2.\\3", hearings$story)

hearings$story[2846]<-gsub("21.0.3.2016", "21.03.2016", hearings$story[2846])
hearings$story[7044]<-gsub("for0", "for 0", hearings$story[7044])
hearings$story[7062]<-gsub("on 228", "on 22", hearings$story[7062])
hearings$story[7129]<-gsub("072016", "07.2016", hearings$story[7129])
hearings$story[9505]<-gsub("13.010.2016", "13.10.2016", hearings$story[9505])
hearings$story[9506]<-gsub("13.010.2016", "13.10.2016", hearings$story[9506])
hearings$story[9559]<-gsub("09.216", "09.2016", hearings$story[9559])
hearings$story[9580]<-gsub("10.16", "10.2016", hearings$story[9580])
hearings$story[9582]<-gsub("10.16", "10.2016", hearings$story[9582])
hearings$story[10530]<-gsub("08.216", "08.2016", hearings$story[10530])
hearings$story[14947]<-gsub("05.17", "05.2017", hearings$story[14947])

adjournedToUnknownDate <- c(4393, 7411, 7412)
#TODO: 7411 and 7412 are duplicates, one needs to be dropped
hearings$status[adjournedToUnknownDate] <- "adjourned"
hearings$nextDate[adjournedToUnknownDate] <- unknownDate

#Some cases are sent by PO to registrar on that date itself.
adjToReg <- grep("Registrar today itself", hearings$story)
hearings$status[adjToReg] <- "adjourned"
hearings$nextDate[adjToReg] <- hearings$date[adjToReg]

#adjournmentPatterns = (
hearings <- SetAdjourned(hearings, "List the case +")
hearings <- SetAdjourned(hearings, "List +the case o[nf] *")
hearings <- SetAdjourned(hearings, "the matter be listed for exhibition +on ")
hearings <- SetAdjourned(hearings, "Be listed before Hon.ble P\\.*O\\.* on ")
hearings <- SetAdjourned(hearings, "Let the matter be listed before Hon['’]ble P\\.*O\\.* *on ")
hearings <- SetAdjourned(hearings, "Let the matter be listed +for the purpose of exhibition on ")
hearings <- SetAdjourned(hearings, "atter be listed +before Hon'ble Presiding Officer +on +")
hearings <- SetAdjourned(hearings, "mat+e[r]* be listed on *") # Matter is misspelled 'Matte' at one place.
hearings <- SetAdjourned(hearings, "matters*\\s+be\\s+listed +on[\\. ]* ", ignore_case=TRUE)# There is a place where matter is mis-spelled mater.
hearings <- SetAdjourned(hearings, "Let the matter be re[-]*notified for ")
hearings <- SetAdjourned(hearings, "Let the matter be listed +for the purpose +of exhibition o[fn] documents on *")
hearings <- SetAdjourned(hearings, "Matter be listed before Registrar on ")
hearings <- SetAdjourned(hearings, "Matter stands adjourned for ")
hearings <- SetAdjourned(hearings, "Put up before the Registrar on ", ignore_case = FALSE)
hearings <- SetAdjourned(hearings, "matter is adjourned to +")
hearings <- SetAdjourned(hearings, "Be listed before the Registrar on ", ignore_case = FALSE)
hearings <- SetAdjourned(hearings, "Let the matter be listed renotified for ")
hearings <- SetAdjourned(hearings, "Relist[s]* this matte[r]* on ")
hearings <- SetAdjourned(hearings, "List the case on the date already fixed i.e. ")
hearings <- SetAdjourned(hearings, "List the case on date already fixed i.e. ")
hearings <- SetAdjourned(hearings, "Let the matter be listed for ")
hearings <- SetAdjourned(hearings, "Be listed on ", ignore_case=FALSE)
hearings <- SetAdjourned(hearings, "Matter be listed ", ignore_case=FALSE)
hearings <- SetAdjourned(hearings, "The matter be on ", ignore_case=FALSE)
hearings <- SetAdjourned(hearings, "Matter now be listed on ", ignore_case=FALSE)
hearings <- SetAdjourned(hearings, "Matter be ", ignore_case=FALSE)
hearings <- SetAdjourned(hearings, "Matter be listed before the Registrar on ", ignore_case=FALSE)
hearings <- SetAdjourned(hearings, "List this matter before Registrar on ", ignore_case=FALSE)
hearings <- SetAdjourned(hearings, "be listed before on ")
hearings <- SetAdjourned(hearings, "the matter is adjourned for ", ignore_case=FALSE)
hearings <- SetAdjourned(hearings, "Let the matter be adjourned for ", ignore_case=FALSE)
hearings <- SetAdjourned(hearings, "List the case for the date already fixed i.e. ", ignore_case=FALSE)
hearings <- SetAdjourned(hearings, "List the case  before ", ignore_case=FALSE)
hearings <- SetAdjourned(hearings, "to be listed on ", ignore_case=FALSE)
hearings <- SetAdjourned(hearings, "List the case before ", ignore_case=FALSE)
hearings <- SetAdjourned(hearings, "Matter is refortified on ", ignore_case=FALSE)
hearings <- SetAdjourned(hearings, "listed before Recovery Officer on ", ignore_case=FALSE)
hearings <- SetAdjourned(hearings, "Let the matter be listed for the purpose for exhibition of documents on ")
hearings <- SetAdjourned(hearings, "matter is adjourned on ")
hearings <- SetAdjourned(hearings, "and therefore on ")
hearings <- SetAdjourned(hearings, "thereafter be listed on ")
hearings <- SetAdjourned(hearings, "Matter is renotify for ", ignore_case=FALSE)
hearings <- SetAdjourned(hearings, "Let the matter be renotified on ")
hearings <- SetAdjourned(hearings, "Let the matter be relisted on ")
hearings <- SetAdjourned(hearings, "The matter is already listed on ")
hearings <- SetAdjourned(hearings, "listed for Hon’ble P.O. for ")
hearings <- SetAdjourned(hearings, "Let the matter be list on ")
hearings <- SetAdjourned(hearings, "the matter be listed for exhibition of documents on +")
hearings <- SetAdjourned(hearings, "exhibition of remaining documents on ")
hearings <- SetAdjourned(hearings, "Let the matter be listed exhibition for ")
hearings <- SetAdjourned(hearings, "Matter be listed Hon'ble Presiding Officer on +")
hearings <- SetAdjourned(hearings, "Let the matter be listed today i.e. ")
hearings <- SetAdjourned(hearings, "is renotified for ")
hearings <- SetAdjourned(hearings, "Let the matter listed for ")
hearings <- SetAdjourned(hearings, "Let the matter is listed for ")
hearings <- SetAdjourned(hearings, "The matter is already listed with Hon’ble P.O. on ")
hearings <- SetAdjourned(hearings, "Renotified on ")
hearings <- SetAdjourned(hearings, "List the matter on ")
hearings <- SetAdjourned(hearings, "Let the matter be  listed for the purpose of exhibition of documents on ")
hearings <- SetAdjourned(hearings, "The matter be listed before Registrar ")
hearings <- SetAdjourned(hearings, "Matter be listed Registrar on ")
hearings <- SetAdjourned(hearings, "Let the matter be listed with Hon’ble P.O. on ")
hearings <- SetAdjourned(hearings, "exhibition of the remaining documents on ")
hearings <- SetAdjourned(hearings, "Renotified the matter on ")
hearings <- SetAdjourned(hearings, "Let the matter be listed for hearing in I.A. No. 578 of 2016 on ")

# ---------------------------------Status---------------------------

hearings[hearings$story=="",]$status <- "dontknow"
hearings[hearings$story=="",]$nextDate <- unknownDate
hearings <- SetFinalStatus(hearings, "File be consigned", "closed")
hearings <- SetFinalStatus(hearings, "Files be consigned", "closed")
hearings <- SetFinalStatus(hearings, "Files be cosigned", "closed")
hearings <- SetFinalStatus(hearings, "filed be consigned", "closed")
hearings <- SetFinalStatus(hearings, "file has consigned to record", "closed")
hearings <- SetFinalStatus(hearings, "Let the filed to consigned to record room", "closed")
hearings <- SetFinalStatus(hearings, "liberty to revive", "closed")
hearings <- SetFinalStatus(hearings, "Final order is passed", "closed")
hearings <- SetFinalStatus(hearings, "no further order", "closed")
hearings <- SetReserved(hearings, "Matter is reserved for pronouncement of final order on ")
hearings <- SetReserved(hearings, "Order reserve the same shall be pronounced on ")
hearings <- SetReserved(hearings, "Order reserved the same shall be pronounced on ")
hearings <- SetReserved(hearings, "Matter is reserved for orders to be pronounced on ")
hearings <- SetReserved(hearings, "same shall be pronounced on ")
hearings <- SetAdjournedUnknown(hearings, "List the case on\\s+\\(")
hearings <- SetAdjournedUnknown(hearings, "Let the matter be listed on ----")
hearings <- SetAdjournedUnknown(hearings, "discharged from personal examination")
hearings <- SetAdjournedUnknown(hearings, "Let the matter be listed for the purpose of exhibition of documents on \\.")
hearings <- SetAdjournedUnknown(hearings, "Be listed before Hon’ble PO on ,")
hearings <- SetNextDateAndStatus(hearings, "deserve to be dismissed", caseClosedDate, "dismissed")
hearings <- SetNextDateAndStatus(hearings, "Hence, the present application is hereby dismissed at the admission stage without issuing the notice to the other side", caseClosedDate, "dismissed")
hearings <- SetNextDateAndStatus(hearings, "Matter is reserved for pronouncement of final order\\.", caseClosedDate, "reserved")
hearings <- SetNextDateAndStatus(hearings, "Pronounced in Open Court", unknownDate, "iaOrder")
hearings <- SetFinalStatus(hearings, "it should be transferred to", "transferred")
hearings <- SetFinalStatus(hearings, "is transferred to", "transferred")
hearings <- SetFinalStatus(hearings, "transfer the matter in DRT-I", "transferred")
hearings <- SetFinalStatus(hearings, "Order dictated", "closed")




hearings <- setStatus(hearings, "Let default notice be sent to the respondent bank", "adjourned")



print(sum(is.na(hearings$status)))
tmp <- hearings %>% filter(is.na(status)) %>% select(story)
print(tmp$story[1])

which(grepl("List the case on", hearings$story) & is.na(hearings$status))

# Check case with OA/1035/1995

# Check that we have all days except sat and sun

#dateissues <- hearings %>% filter(date!=storyDate) #There are ~ 200 of these. Need to go through these one by one.

# Checked that story is empty for hearing 125 on date "2016-03-21", computerid 2570
hearings %>% group_by(date) %>% tally %>% arrange(n) # TODO: Reveals a few days with 1 order or 2 orders. These seem to be saturdays etc. Lets find out whether the date field is wrong.
# TODO: check these: #> which(is.na(storyDates))
#[1] 1064 1422



# TODO: there is an almost repetition of S.A. no. 117 of 2013 02.06.2016. Delete
# the smaller.
