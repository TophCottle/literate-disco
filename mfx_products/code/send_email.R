# Load libraries and API --------------------------------------------------
rm(list = ls())
library(tidyverse)
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(blastula)

# Upload and clean data ---------------------------------------------------


# Write email -------------------------------------------------------------
# Define email content
#email_body <- paste0(
#"Hello,

#Here is the list of large or unusual movements of exchange rates:",

                     
 # for (var in varlist) {
#  if(highlight_changes$one_day_change > 1) {
 #  paste("One day movements:", "\n\n", colnames(var), ":", highlight_change$var[length(highlight_change$var)], "changed by", length(highlight_change$one_day_change), "%")
#   }
 #  },
                     
#"Best regards,
#Toph")
# Define email
#email_body <- "hi"
#email <- compose_email(
#body = email_body)


# Send email using Gmail SMTP server
#send_email_out(email_body, "tophgcottle@gmail.com", "tophgcottle@gmail.com", subject = "test", cc = NULL, bcc = NULL,
 #              host = "smtp.gmail.com", port = 465, password = "Iamonlyone!79",
  ##            verbose = FALSE, debug = FALSE)
#


