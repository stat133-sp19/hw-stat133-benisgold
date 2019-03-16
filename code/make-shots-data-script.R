# Title: Data Preparation
# Description: Adds new columns: name (player name), minute (minutes elapsed at shot time).
#              Changes the values of shot_made_flag column from "y" to "shot_yes" and "n" to "shot_no".
#              Combines all the player data tables into one and exports it to ../data/shots-data.csv.
#              Creates text files summarizing each input as well as shots_data.csv in the ../output directory.
# Inputs: stephen-curry.csv, klay-thompson.csv, kevin-durant.csv, draymond-green.csv, andre-iguodala.csv
# Outputs: shots-data.csv, shots-data-summary.txt, stephen-curry-summary.txt, klay-thompson-summary.txt,
#          kevin-durant-summary.txt, draymond-green-summary.txt, andre-iguodala-summary.txt

curry = read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)
thompson = read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)
durant = read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
green = read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
iguodala = read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)

curry$name = "Stephen Curry"
thompson$name = "Klay Thompson"
durant$name = "Kevin Durant"
green$name = "Draymond Green"
iguodala$name = "Andre Iguodala"

curry$shot_made_flag[curry$shot_made_flag == "y"] = "shot_yes"
curry$shot_made_flag[curry$shot_made_flag == "n"] = "shot_no"
thompson$shot_made_flag[thompson$shot_made_flag == "y"] = "shot_yes"
thompson$shot_made_flag[thompson$shot_made_flag == "n"] = "shot_no"
durant$shot_made_flag[durant$shot_made_flag == "y"] = "shot_yes"
durant$shot_made_flag[durant$shot_made_flag == "n"] = "shot_no"
green$shot_made_flag[green$shot_made_flag == "y"] = "shot_yes"
green$shot_made_flag[green$shot_made_flag == "n"] = "shot_no"
iguodala$shot_made_flag[iguodala$shot_made_flag == "y"] = "shot_yes"
iguodala$shot_made_flag[iguodala$shot_made_flag == "n"] = "shot_no"

curry$minute = 15 - curry$minutes_remaining + 15 * (curry$period - 1)
thompson$minute = 15 - thompson$minutes_remaining + 15 * (thompson$period - 1)
durant$minute = 15 - durant$minutes_remaining + 15 * (durant$period - 1)
green$minute = 15 - green$minutes_remaining + 15 * (green$period - 1)
iguodala$minute = 15 - iguodala$minutes_remaining + 15 * (iguodala$period - 1)

sink('../output/stephen-curry-summary.txt')
summary(curry)
sink()
sink('../output/klay-thompson-summary.txt')
summary(thompson)
sink()
sink('../output/kevin-durant-summary.txt')
summary(durant)
sink()
sink('../output/draymond-green-summary.txt')
summary(green)
sink()
sink('../output/andre-iguodala-summary.txt')
summary(iguodala)
sink()

shots_data = rbind(curry, thompson, durant, green, iguodala)

write.csv(shots_data, '../data/shots-data.csv')

sink('../output/shots-data-summary.txt')
summary(shots_data)
sink()

