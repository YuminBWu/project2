library(rmarkdown)

wday <- c("weekday_is_monday", "weekday_is_tuesday", "weekday_is_wednesday", 
          "weekday_is_thursday","weekday_is_friday", "weekday_is_saturday","weekday_is_sunday")

for (i in 1:length(wday)) {
  render(input="C:/Users/yumin/Desktop/ST558/project2/project2_st558.Rmd", params = list(days = wday[i]), 
         output_file = paste0("day_", wday[i], ".md"))
}


