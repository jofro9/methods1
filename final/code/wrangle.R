library(data.table)

blood = fread("https://ucdenver.instructure.com/courses/446290/files/12283514/download?wrap=1")

colnames(blood) = c(
  "rbcagegroup", "medianrbcage", "age", "aa", "famhx", "pvol", "tvol", "tstage", "bgs", "bn+",
  "organconfined", "preoppsa", "preoptherapy", "units", "sgs", "anyadjtherapy", "adjradtherapy",
  "recurrence", "censor", "timetorecurrence"
)

saveRDS(blood, file = "C:/Users/froelijo/dev/methods1/final/data/blood.rds")
