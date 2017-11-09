class ApiResponsesController < ApplicationController

  before_action :get_api_response, only: [:show]
  skip_before_action :verify_authenticity_token

  def show
    render json: api_response
  end

  def create
    request_service = RequestService.new(params[:url], params[:method], options)
    request_service.process
    if request_service.errors.present?
      render json: request_service, status: 422
    else
      render json: request_service.api_response, status: 200
    end
  end

  private

  def get_api_response
    unless @api_response = ApiResponse.find_by({token: params[:id]})
      render json: {error: "Invalid Page"}, status: 404
    end
  end

  def api_response
    {
      url: @api_response.url,
      httpMethod: @api_response.method,
      requestParams: @api_response.request_params,
      requestHeaders: @api_response.request_headers,
      requestBody: @api_response.request_body,
      username: @api_response.username,
      password: @api_response.password,
      assertions: @api_response.assertions,
      response: {
        response_headers: @api_response.response_headers.sort.to_h,
        response_body: @api_response.response['body'],
        response_code: @api_response.status_code
      }
    }
  end

  def options
    api_request_parser_service = ApiRequestParserService.new(params)
    request_headers = api_request_parser_service.process_headers
    request_parameters = api_request_parser_service.process_parameters
    params.merge(request_params: request_parameters).merge(request_headers: request_headers).permit!.to_h
  end
end
