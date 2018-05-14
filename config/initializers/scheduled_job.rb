# Schedule jobs when Rails starts. Added code to not run jobs for rake and delayed job worker tasks.
if Delayed::Worker.delay_jobs && !($PROGRAM_NAME =~ /(rake|delayed_job)(.rb)?$/) && ApplicationRecord.connection.table_exists?("delayed_jobs")
  PurgeRecordJob.schedule_job
end

ScheduledJob.configure do |config|
  config.logger = Rails.logger
end