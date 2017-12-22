class AddTimestampsToApiResponses < ActiveRecord::Migration[5.1]
  def change
    add_timestamps :api_responses, null: false, default: DateTime.current

    change_column_default :api_responses, :created_at, nil
    change_column_default :api_responses, :updated_at, nil
  end
end
