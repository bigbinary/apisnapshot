class PurgeRecordJob

    RETAIN_RECORDS_FOR_IN_DAYS = 7

    include ::ScheduledJob

    run_every 1.hour

    def perform
        Rails.logger.info "Going to purge old recordscod"
        ApiResponse.where("created_at < ? ", RETAIN_RECORDS_FOR_IN_DAYS.days.ago).delete_all
    end

    # run it every one hour
    def self.time_to_recur(last_run_at)
        last_run_at + 1.hour
    end
end
