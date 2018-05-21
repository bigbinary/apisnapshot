json.extract! @api_response, :url, :method,
              :username, :password, :created_at,
              :request_params, :request_headers, :request_body
json.response do
  json.response_headers @api_response.response_headers.sort.to_h
  json.response_body @api_response.response['body']
  json.response_code @api_response.status_code
end
