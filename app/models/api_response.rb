class ApiResponse < ApplicationRecord
  has_secure_token
  validates :url, :method, presence: true
  has_many :assertions, class_name: 'ApiAssertion'

  accepts_nested_attributes_for :assertions, reject_if: proc {|attributes| attributes['key'].blank? && attributes['value'].blank? }

  def response_body
    response['body']
  end
end
