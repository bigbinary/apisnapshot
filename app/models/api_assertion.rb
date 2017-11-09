class ApiAssertion < ApplicationRecord
  belongs_to :api_response

  before_create :set_results

  private

  def set_results
    case kind
    when 'Response JSON'
      assert_from_response
    when 'Status Code'
      assert_from_status
    end
    self.success = api_value ? assert_values : assert_for_nil
  rescue JSON::ParserError => e
    self.success = false
  rescue TypeError
    self.comments = "Invalid key"
  end

  def assert_from_response
    response = JSON.parse(api_response.response_body)
    self.api_value = fetch_api_value(response)
  end

  def assert_from_status
    self.key = ''
    self.api_value = api_response.status_code
  end

  def assert_values
    case comparison
    when 'equals'
      api_value.to_s == value.to_s
    when 'does not equal'
      api_value.to_s != value.to_s
    when 'contains'
      api_value.to_s.include?(value.to_s)
    when 'lesser than'
      api_value.to_i < value.to_i
    when 'greater than'
      api_value.to_i > value.to_i
    when 'is empty'
      check_for_empty
    when 'is not empty'
      api_value.present?
    else
      false
    end
  end

  def assert_for_nil
    if nil_comparison?
      api_value.nil?
    else
      self.api_value = "NULL"
      false
    end
  end

  def nil_comparison?
    comparison == 'is NULL'
  end

  def fetch_api_value(response)
    keys = key.scan(/(\d+)|(\w+)/).map do |number, string|
      number&.to_i || string
    end
    keys.empty? ? 'Null' : response.dig(*keys)
  end

  def check_for_empty
    JSON.parse(api_value).empty?
  rescue JSON::ParserError
    api_value.empty?
  end
end
