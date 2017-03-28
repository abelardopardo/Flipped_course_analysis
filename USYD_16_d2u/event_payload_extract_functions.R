## the f. extracts activity id, as well as x and y coordinates from
## the payload field of an xy-click event
xyclick.data.extract <- function(x) { 
  id <- gsub("\\{\"id\":\"([A-z0-9-]+)\",.+\\}", "\\1", x)
  xy <- gsub("\\{.+,\"x\":([-]?\\d{1,4}),\"y\":([-]?\\d{1,4})\\}", "\\1 \\2", x)
  c.x <- strsplit(xy, " ")[[1]][1]
  c.y <- strsplit(xy, " ")[[1]][2]
  c(id, c.x, c.y)
}

## the f. extracts the values array from the payload field of an studykit-view event
studykit.data.extract <- function(x) {
  values <- gsub("\\{\"values\": \\[(.+)\\],.+\\}", "\\1", x)
  values
}

## f. for extracting the value (-1,0,1) of the 'answer' attribute from the 
## payload variable of "embedded-question" actions 
mcq.answer.extract <- function(x) { 
  temp <- gsub(".+\"answer\":[\"]?([-]?\\d)[\"]?\\}", "\\1", x)
  temp
}

## f. for extracting the value of the 'question_id' attribute from the 
## payload variable of "embedded-question" actions 
mcq.id.extract <- function(x) { 
  temp <- gsub("\\{\"question_id\":\"([A-z0-9_-]+)\",.+\\}", "\\1", x)
  temp
}

## f. for extracting the value of the 'outcome' attribute from the payload 
## of the "exco-answer" action 
exco.outcome.extract <- function(x) { 
  result <- gsub("\\{\"outcome\":\\s\"([a-z]+)\", .+\\}", "\\1", x)
  result
}

## f. for extracting the type of assessment from the payload 
## of the "exco-answer" action; it can be "summative" or "formative"
exco.assessment.type <- function(x) { 
  result <- gsub("\\{.+\"assessment\":\\s\"([a-z]+)\", .+\\}", "\\1", x)
  result
}

## f. for extracting data from the paylod of the "dboard-view" action 
## it returns a list of 2 elements: week and an array of dashboard values
dboard.extract <- function(x) {
  week <- gsub("\\{\"week\":\\s(\\d{1,2}),.+\\}", "\\1", x)
  vals <- gsub("\\{\"week\":\\s\\d{1,2},\\s\"values\":\\s\\[(.+)\\]\\}", "\\1", x)
  list(week, unlist(strsplit(vals, split = ", ", fixed = T)))
}

## f. for extracting the value of the 'url' attribute from the 
## payload variable of "resource-view" actions 
resview.id.extract <- function(x) { 
  temp <- gsub("\\{\"url\":\"(.+)\"\\}", "\\1", x)
  temp
}



