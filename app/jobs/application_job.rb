class PurgeRecordJob

    RETAIN_RECORDS_FOR = 7

    include Delayed::RecurringJob
    run_every 1.hour

    def perform
        ApiResponse.where("created_at < ? ", RETAIN_RECORDS_FOR.days.ago).delete_all
    end

end
