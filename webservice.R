install.packages(c("curl","httr","rjson"))
library("curl")
library("httr")
library("rjson")

requestFailed = function(response) {
  return (response$status_code >= 400)
}

printHttpResult = function(response, result) {
  if (requestFailed(response)) {
    print(paste("The request failed with status code:", response$status_code, sep=" "))
    
    # Print the headers - they include the requert ID and the timestamp, which are useful for debugging the failure
    print(response$headers)
  }
  
  print("Result:") 
  print(fromJSON(result))  
}

req =  list(
  Inputs = list(
    "input1"= list(
      list(
        'race_ethnicity' = "group_B",
        'parental_level_of_education' = "bachelor's_degree",
        'lunch' = "standard",
        'test_preparation_course' = "none",
        'math_score' = "72",
        'reading_score' = "72",
        'writing_score' = "74",
        'Have_car' = "0"
      )
    )
  ),
  GlobalParameters = setNames(fromJSON('{}'), character(0))
)

body = enc2utf8(toJSON(req))
api_key = "7EfZpL4S0mi0ifFpLOBn2ypdR26JPQvscrlNQ7o1BMWl/Hm3ax7A3FUPlIr+xM1Ohxx7vhKRfWLGyj0v6lX99g==" # Replace this with the API key for the web service
authz_hdr = paste('Bearer', api_key, sep=' ')

response=POST(url = "https://ussouthcentral.services.azureml.net/workspaces/19fa0face9694074a2dfbf665cc69942/services/9637731ffdc6459eb4d1c5b510b11736/execute?api-version=2.0&format=swagger",
              add_headers('Content-Type' = "application/json", 'Authorization' = authz_hdr),
              body=body)

result = content(response, type="text", encoding="UTF-8")

printHttpResult(response, result)
