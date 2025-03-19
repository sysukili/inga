## code to prepare `nhefs1` dataset goes here


nhefs1 <- nhefs |>
    mutate(time = ifelse(death == 1, (yrdth - 83) * 12 + modth, 120)) |>
    select(qsmk, death, time, smokeyrs, sex, age, race, education)
nhefs1$death <- as.factor(nhefs1$death)
nhefs1$qsmk <- as.factor(nhefs1$qsmk)



usethis::use_data(nhefs1, overwrite = TRUE)
