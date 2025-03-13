#' NHEFS Dataset
#'
#' A subset of data from the National Health and Nutrition Examination Follow-up Study (NHEFS).
#'
#' @format ## `nhefs`
#' A data frame with 1,629 rows and 64 columns:
#' \describe{
#'   \item{seqn}{Sequence number (unique identifier for each participant)}
#'   \item{qsmk}{Smoking cessation status (0 = continued smoking, 1 = quit smoking)}
#'   \item{death}{Death status (0 = alive, 1 = deceased)}
#'   \item{yrdth}{Year of death}
#'   \item{modth}{Month of death}
#'   \item{dadth}{Day of death}
#'   \item{sbp}{Systolic blood pressure (mmHg)}
#'   \item{dbp}{Diastolic blood pressure (mmHg)}
#'   \item{sex}{Gender (0 = male, 1 = female)}
#'   \item{age}{Age in years at baseline}
#'   \item{race}{Race (1 = white, 2 = black, 3 = other)}
#'   \item{income}{Annual income in US dollars}
#'   \item{education}{Years of education}
#'   \item{wt71}{Weight in 1971 (lbs)}
#'   \item{wt82}{Weight in 1982 (lbs)}
#'   \item{wt82_71}{Weight change from 1971 to 1982 (lbs)}
#'   \item{exercise}{Frequency of exercise (1 = none, 2 = moderate, 3 = high)}
#'   \item{active}{Physical activity status}
#'   \item{alcohol}{Alcohol consumption status (0 = no, 1 = yes)}
#'   \item{chol}{Total cholesterol level (mg/dL)}
#'   \item{smokeintensity}{Number of cigarettes smoked per day}
#'   \item{smokeyrs}{Number of years smoked}
#'   \item{ht}{Height in inches}
#'   \item{diabetes}{Diabetes status (0 = no, 1 = yes)}
#'   \item{marital}{Marital status (1 = married, 2 = widowed, 3 = divorced/separated, 4 = never married)}
#'   \item{mental}{Mental health status score}
#'   \item{physact}{Physical activity level score}
#'   \item{pef}{Peak expiratory flow (L/min)}
#'   \item{heart_disease}{History of heart disease (0 = no, 1 = yes)}
#'   \item{stroke}{History of stroke (0 = no, 1 = yes)}
#'   \item{cancer}{History of cancer (0 = no, 1 = yes)}
#'   \item{asthma}{Asthma diagnosis (0 = no, 1 = yes)}
#'   \item{bronchitis}{Bronchitis diagnosis (0 = no, 1 = yes)}
#'   \item{emphysema}{Emphysema diagnosis (0 = no, 1 = yes)}
#'   \item{hosp}{Number of hospital visits}
#'   \item{physvis}{Number of physician visits}
#'   \item{depress}{Depression score}
#'   \item{alcohol_freq}{Frequency of alcohol consumption}
#'   \item{diet_score}{Diet quality score}
#'   \item{vitamin_use}{Vitamin supplement use (0 = no, 1 = yes)}
#'   \item{medication_use}{Medication usage status (0 = no, 1 = yes)}
#'   \item{insurance}{Health insurance coverage status (0 = no, 1 = yes)}
#' }
#' @source <https://wwwn.cdc.gov/nchs/nhanes/nhefs/Default.aspx>
"nhefs"
