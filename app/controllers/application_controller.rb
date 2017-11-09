class ApplicationController < ActionController::Base
  protect_from_forgery with: :exception

  force_ssl if: :ssl_configured?

  def ssl_configured?
    !(Rails.env.development? || Rails.env.test?)
  end
end
