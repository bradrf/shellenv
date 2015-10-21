class Progress
  def self.report_each(monitor, reporter=$stdout)
    self.new(monitor, reporter).each{ yield }
  end

  def initialize(monitor, reporter=$stdout)
    @monitor  = monitor
    @reporter = reporter
  end

  def each
    pos = @monitor.respond_to?(:pos) ? nil : 0
    @monitor.each do |item|
      pos and pos += 1
      report!(pos)
      yield item
    end
  end

  def report!(pos=nil)
    pos ||= @monitor.pos
    @reporter.printf "\r%4.0f%%", (pos.to_f / @monitor.size * 100)
    @reporter.flush
    self
  end

  def clear!
    @report_io.printf "\r          \r\n"
    self
  end
end
