class RequestService
  include ActiveModel::Validations

  attr_reader :url, :username, :password, :method, :response, :request_parameters, :request_headers, :request_body
  attr_accessor :api_response

  validates :url, :method, presence: true

  def initialize(url:, method: nil, options: {})
    @url = url
    @method = method || :get
    @username = options[:username]
    @password = options[:password]
    @request_parameters = options[:request_parameters]
    @request_body = options[:request_body]
    @request_headers = options[:request_headers]
  end

  def process
    if valid?
      begin
        @response = RestClient::Request.execute(options)
        self.api_response = save_api_response
      rescue RestClient::ExceptionWithResponse => e
        @response = e.response
        self.api_response = save_api_response
      rescue URI::InvalidURIError
        errors.add(:url, 'Invalid URL')
      rescue SocketError => e
        errors.add(:url, 'Invalid URL or Domain')
      end
    end
  end

  private

  def save_api_response
    ApiResponse.create!({ url: url,
                        method: method.upcase,
                        response: response_body,
                        response_headers: response.headers,
                        status_code: response.code,
                        request_headers: request_headers,
                        request_params: request_parameters_to_save,
                        username: username,
                        password: password,
                        request_body: request_body }
                       )
  end

  def request_parameters_to_save
    request_parameters.is_a?(String) ? JSON.parse(request_parameters) : sanitized_request_parameters
  end

  def sanitized_request_parameters
    {}.tap do |return_value|
      request_parameters.each do |key, value|
        return_value[key] = value.is_a?(ActionDispatch::Http::UploadedFile) ? '' : value
      end
    end
  end

  def authorization_options
    if username && password
      {user: username, password: password}
    else
      {}
    end
  end

  def response_body
    {
      body: response.body.encode('UTF-8', 'binary', invalid: :replace, undef: :replace, replace: '')
    }
  end

  def options
    {url: url,
     method: method,
     verify_ssl: false,
     payload: request_parameters,
     headers: request_headers}
        .merge(authorization_options)
  end

end