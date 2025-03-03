# written by james rosindell Imperial college london released open source under an MIT license

# Assign random speciation rates to class
CMEE_class <- c(
  "sb4524",
  "ac524",
  "sed24",
  "yh4724",
  "kl2621",
  "yl2524",
  "yl13424",
  "lam124",
  "bs2324",
  "hjt24",
  "yw4524",
  "yy6024",
  "tz124",
  "zz8024",
  "kz1724")

choose_student <- function(class) {
  print(sample(class,1))
}

choose_student_2 <- function(class,seedin = 1) {
  set.seed(seedin)
  print(sample(class,1))
}

choose_student_3 <- function(class,seedin=-1) {
  if (seedin <= 0){
    set.seed(floor(proc.time()[3]*1000))
  }
  else {
    set.seed(seedin)
  }
  print(sample(class,1))
}

assign_student_number <- function(class=CMEE_class,seedin=2024,min=0.002,max=0.007,sigfig=4,unique=TRUE) {
  if (seedin <= 0){
    set.seed(floor(proc.time()[3]*1000))
  }
  else {
    set.seed(seedin)
  }
  speciation_values <- signif(runif(length(class))*(max-min)+min,sigfig)
  if (unique){
    while(length(unique(speciation_values)) < length(class)){
      speciation_values <- signif(runif(length(class))*(max-min)+min,sigfig)
    }
  }
  return(cbind(speciation_values,class))
}

print(assign_student_number())
