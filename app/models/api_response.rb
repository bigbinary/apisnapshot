class ApiResponse < ApplicationRecord
  has_secure_token
  validates :url, :method, presence: true

  def response_body
    response['body']
  end
end
