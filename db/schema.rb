# This file is auto-generated from the current state of the database. Instead
# of editing this file, please use the migrations feature of Active Record to
# incrementally modify your database, and then regenerate this schema definition.
#
# Note that this schema.rb definition is the authoritative source for your
# database schema. If you need to create the application database on another
# system, you should be using db:schema:load, not running all the migrations
# from scratch. The latter is a flawed and unsustainable approach (the more migrations
# you'll amass, the slower it'll run and the greater likelihood for issues).
#
# It's strongly recommended that you check this file into your version control system.

ActiveRecord::Schema.define(version: 20171222085456) do

  # These are extensions that must be enabled in order to support this database
  enable_extension "plpgsql"
  enable_extension "hstore"

  create_table "api_assertions", force: :cascade do |t|
    t.string "key"
    t.string "value"
    t.string "comparison"
    t.string "kind"
    t.string "api_value"
    t.string "comments"
    t.boolean "success", default: false
    t.bigint "api_response_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["api_response_id"], name: "index_api_assertions_on_api_response_id"
  end

  create_table "api_responses", force: :cascade do |t|
    t.string "url"
    t.string "method"
    t.hstore "response", default: {}
    t.hstore "response_headers", default: {}
    t.hstore "request_params", default: {}
    t.hstore "request_headers", default: {}
    t.string "status_code"
    t.string "username"
    t.string "password"
    t.string "token"
    t.text "request_body"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["token"], name: "index_api_responses_on_token", unique: true
  end

end
