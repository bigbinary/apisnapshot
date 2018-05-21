json.url @api_response.url
json.createdAt @api_response.created_at
json.httpMethod @api_response.method
json.requestParams @api_response.request_params
json.requestHeaders @api_response.request_headers
json.requestBody @api_response.request_body
json.username @api_response.username
json.password @api_response.password
json.response do
  json.response_headers @api_response.response_headers.sort.to_h
  json.response_body @api_response.response['body']
  json.response_code @api_response.status_code
end
