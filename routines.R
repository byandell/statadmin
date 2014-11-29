group.courses <- function(query) {
  ## Set up courses into groups of interest.
  gateways <- c(201,224,301,302,324,371)
  gategrads <- c(541,542,543,571,572)
  mathstats <- c(309,310,311,312,431)
  gradcore <- c(609,610,709,710,849,850,998)
  directed <- c(681,682,698,699,990)
  
  ## Organize courses by group.
  course.group <- 3 - 2 * (query$CATALOG.NUMBER %in% gateways) -
    (query$CATALOG.NUMBER %in% mathstats) +
    (query$CATALOG.NUMBER %in% gategrads) +
    2 * (query$CATALOG.NUMBER > 600) +
    (query$CATALOG.NUMBER %in% gradcore) +
    2 * (query$CATALOG.NUMBER %in% directed)
  tmp <- c("intro","mathstat","major","gategrads","grad","gradcore","directed")
  ordered(tmp[course.group], tmp)
}
pull.instructor <- function(faculty, instructor, drop.directed = TRUE) {
  ## Pull instructor records for partial match to instructor name.
  if(drop.directed)
    faculty <- faculty[faculty$groups != "directed",]
  faculty[grep(instructor, faculty$instructor), -1]
}


lecsum <- function(query, subset = NULL, group = NULL,
                   by.term = TRUE, year1 = 1999, summer = FALSE, ...) {
  ## Get subset of data to work with.
  if(!is.null(subset)) {
    query <- query[subset, ]
    if(!is.null(group))
      group <- group[subset]    
  }
  lec <- query[query$SECTION.NUMBER < 300,]
  if(!is.null(group))
    group <- group[query$SECTION.NUMBER < 300]
  if(!summer) {
    if(!is.null(group))
      group <- group[lec$term != "Summer"]
    lec <- lec[lec$term != "Summer",]
  }
  
  tmpfn <- function(x, last = TRUE) sapply(strsplit(x, "-", fixed = TRUE),
                                           function(x, last) ifelse(last, x[length(x)], x[1]),
                                           last)
  if(by.term) {
    ## Summarize by group and term.
    if(!is.null(group))
      tmp <- paste(group, lec$TERM, sep = "-")
    else
      tmp <- lec$TERM
    
    lecterm <- tapply(lec$enroll, tmp, sum)
    codes <- as.numeric(tmpfn(names(lecterm)))
    term <- c("Fall","Spring","Summer")[(codes %% 10) / 2]
    year <- 1900 + floor(codes/10)
    if(!is.null(group)) {
      tmp <- tmpfn(names(lecterm), FALSE)
      if(is.ordered(group))
        tmp <- ordered(ordered(tmp, levels(group)))
      out <- data.frame(count = lecterm, year = year, term = term, group = tmp)
    }
    else
      out <- data.frame(count = lecterm, year = year, term = term)
  }
  else {
    if(!is.null(group))
      tmp <- paste(group, lec$year, sep = "-")
    else
      tmp <- lec$year
    lecterm <- tapply(lec$enroll, tmp, sum)
    year <- tmpfn(names(lecterm))
    if(!is.null(group)) {
      tmp <- tmpfn(names(lecterm), FALSE)
      if(is.ordered(group))
        tmp <- ordered(ordered(tmp, levels(group)))
      out <- data.frame(count = lecterm, year = year, group = tmp)
    }
    else
      out <- data.frame(count = lecterm, year = year)
  }
  out
}
lecplot <- function(out, main = "", smooth = TRUE, log = FALSE,
                    auto.key = list(space = "right"), title = "", ...) {
  require(lattice)
  types <- c("p","l","smooth")
  if(!smooth)
    types <- c("p","l")
  if(log) {
    out$count <- log10(out$count)
    ylab <- "log10(count)"    
  }
  else
    ylab <- "count"
    
  if("term" %in% names(out)) {
    if("group" %in% names(out))
      xyplot(count ~ year | term, out, group = group, type = types,
             auto.key = auto.key, ylab = ylab, main = title)
    else
      xyplot(count ~ year | term, out, type = types,
             auto.key = auto.key, ylab = ylab, main = title)
  }
  else {
    if("group" %in% names(out))
      xyplot(count ~ year, out, group = group, type = types,
             auto.key = auto.key, ylab = ylab, main = title)
    else
      xyplot(count ~ year, out, type = types,
             auto.key = auto.key, ylab = ylab, main = title)
  }
}
lecploty <- function(query, subset = NULL, group = NULL, 
                     title = "", ...) {
  if(is.null(subset))
    subset <- TRUE
  yearsum <- lecsum(query, subset & (query$year != "2014-2015"),
                    group, by.term = FALSE, ...)
  print(lecplot(yearsum, title = title, ...))
  invisible(yearsum)
}
lecplott <- function(query, subset = NULL, group = NULL, title = "", ...) {
  termsum <- lecsum(query, subset, group, ...)
  print(lecplot(termsum, title = title, ...))
  invisible(termsum)
}
lecplots <- function(query, subset = NULL, group = NULL, title = "", ...) {
  print(lecploty(query, subset, group, title, ...))
  print(lecplott(query, subset, group, title, ...))
  invisible(list(year = yearsum, term = termsum))
}
