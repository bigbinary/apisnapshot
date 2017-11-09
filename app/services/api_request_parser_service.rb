class ApiRequestParserService

  attr_reader :request_parameters, :request_body, :request_headers

  def initialize(parameters)
    @request_parameters = parameters[:request_parameters]
    @request_body = parameters[:request_body]
    @request_headers = parameters[:request_headers]
  end

  def process_parameters
    if request_parameters.present?
      parse(request_parameters)
    elsif request_body.present?
      request_body
    else
      {}
    end
  end

  def process_headers
    if request_headers.present?
      parse(request_headers)
    else
      {}
    end
  end

  def parse(request_array)
    parsed_hash = {}
    request_array.values.each do |request|
      parsed_hash[request[:key]] = request[:value] if request[:key] && request[:value]
    end
    parsed_hash
  end
end
