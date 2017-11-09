class CreateApiResponses < ActiveRecord::Migration[5.1]
  def change
    enable_extension :hstore unless extension_enabled?('hstore')

    create_table :api_responses do |t|
      t.string :url
      t.string :method
      t.hstore :response, default: {}
      t.hstore :response_headers, default: {}
      t.hstore :request_params, default: {}
      t.hstore :request_headers, default: {}
      t.string :status_code
      t.string :username
      t.string :password
      t.string :token
      t.text :request_body
    end

    add_index :api_responses, :token, unique: true
  end
end
