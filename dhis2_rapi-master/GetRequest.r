library(httr)

ua <- user_agent("https://github.com/INyabuto")



dhis2_rapi <- function(path){
  # Get HTTP request
  #url <- modify_url("https://play.dhis2.org", path = path)
  # pass the response
  resp <- GET(path, ua, authenticate("admin","district"))
  # Confirm the passed response, It should be a json format
  #if (http_type(resp)!="application/json"){
  #  stop("API did not return json", call. = FALSE)
  #}
  # Pass the out put into an r object
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  # Turn apis errors into r errorsa
  if (http_error(resp)){
    stop(
      sprintf(
        "DHIS2 API request failed [%s]\n%s\n<%s>", 
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }
  # Return an helpful object i.e not only a list by creating an s3 object
  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "dhis2_rapi"
  )
}
# Print method to return the s3 object
print.dhis2_rapi<-function(x, ...){
  cat("<dhis2 ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}
