library(googlesheets4)
library(googledrive)

#gs4_auth_configure(api_key = '~/ccs-wes-284218-7a4138dd57c9.json')
#gs4_auth_configure(api_key = 'AIzaSyA3h1pJ8zGTGq6M-eGUmDUT5wjoiXxqBG0')

#drive_auth( path = '~/ccs-wes-284218-7a4138dd57c9.json')

options(gargle_oauth_cache = ".secrets")

read_sheet("docs.google.com/spreadsheets/d/1KKu8BDt6lzYXcXOydoZFjFC3CJoM8PGbOWfIv6y1LZI")
sheet_write(as.data.frame("test5"), 'docs.google.com/spreadsheets/d/1KKu8BDt6lzYXcXOydoZFjFC3CJoM8PGbOWfIv6y1LZI', "Sheet 1")
sheet_write(as.data.frame("test6"), 'docs.google.com/spreadsheets/d/1KKu8BDt6lzYXcXOydoZFjFC3CJoM8PGbOWfIv6y1LZI', "VAF Review")
