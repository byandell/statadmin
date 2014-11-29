## Here are some Utility Functions for Lectures.
source("routines.R")

## First we do some data setup.
query <- read.csv("../932 query.csv", header=TRUE)
query$enroll <- apply(query[,8:9], 1, max)
query <- query[,-c(3,8:10)]
query <- query[query$SECTION.NUMBER > 0,]
tmp <- strsplit(as.character(query$TERM.DESCR), " ", fixed = TRUE)
query$term <- sapply(tmp, function(x) x[1])
query$year <- sapply(tmp, function(x) x[2])
math431 <- query[query$CATALOG.NUMBER == "431",]
nonstat <- c(431,475,525,546,632,726,733,734,831,832,833)
query <- query[!(query$CATALOG.NUMBER %in% nonstat),]

## Fix Instructor Roles.
lec.names <- c("Eickhoff,Jens C","Gillett,John Robert","Koscik,Rebecca Langhough",
               "Laughlin,Nellie K.","Payesteh,Sayeed")
tmp <- query$INSTRUCTOR.ROLE.DESCR %in% c("Primary Instructor","Not Selected","Other Academic Staff") &
  query$SECTION.NUMBER < 300
query$INSTRUCTOR.ROLE.DESCR[tmp] <- "Faculty"
query$INSTRUCTOR.ROLE.DESCR[tmp & query$INSTRUCTOR.NAME %in% lec.names] <- "Lecturer/Faculty Associate"
tmp <- as.character(query$INSTRUCTOR.ROLE.DESCR)
tmp[tmp == "Emerita/Emeritus"] <- "Emeritus"
tmp[tmp == "Graduate Assistant"] <- "Grad_Stud"
tmp[tmp == "Lecturer/Faculty Associate"] <- "Lecturer"
tmp[!(tmp %in% c("Emeritus","Faculty","Grad_Stud","Lecturer"))] <- "Other"
tmp[tmp == "Lecturer" & query$INSTRUCTOR.NAME %in% 
      c("Wardrop,Robert L.","Cook,Thomas David")] <- "Emeritus"
query$instructor <- tmp
math431$instructor <- math431$INSTRUCTOR.ROLE.DESCR

## Fix Stat 327.
tmp <- query$INSTRUCTOR.NAME %in% c("Gillett,John Robert","Vieira Nunes Ludwig,Guilherme") & query$CATALOG.NUMBER == 692
query$CATALOG.NUMBER[tmp] <- 327

## Organize courses by group using course.group.
course.group <- group.courses(query)