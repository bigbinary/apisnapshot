class ApiResponsesController < ApplicationController

  before_action :load_api_response, only: [:show]
  skip_before_action :verify_authenticity_token

  def show
    render json: api_response
  end

  def create
    request_service = RequestService.new(url: api_request_params[:url],
                                         method: api_request_params[:method],
                                         options: options_for_request_service)
    request_service.process

    if request_service.errors.present?
      render json: request_service, status: 422
    else
      render json: request_service.api_response, status: 200
    end
  end

  private

  def load_api_response
    unless @api_response = ApiResponse.find_by({token: params[:id]})
      render json: {error: "Invalid Page"}, status: 404
    end
  end

  def api_response
    {
      url: @api_response.url,
      createdAt: @api_response.created_at,
      httpMethod: @api_response.method,
      requestParams: @api_response.request_params,
      requestHeaders: @api_response.request_headers,
      requestBody: @api_response.request_body,
      username: @api_response.username,
      password: @api_response.password,
      response: {
        response_headers: @api_response.response_headers.sort.to_h,
        response_body: @api_response.response['body'],
        response_code: @api_response.status_code
      }
    }
  end

  def options_for_request_service
    parsed_request_params = ApiRequestParserService.new(api_request_params).process
    api_request_params.merge(parsed_request_params).to_h
  end

  def api_request_params
    params.permit(:url, :method, :request_body, request_headers: [:key, :value], request_parameters: [:key, :value])
  end
end
