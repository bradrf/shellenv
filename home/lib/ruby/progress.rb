# Helper that can issue progress reports on any montitored object that quacks with "size"
# (i.e. works with Array, Hash, IO, and others).
class Progress
  def initialize(monitor, reporter=$stdout)
    monitor.respond_to?(:size) or raise "#{monitor.class} must respond to :size"
    @monitor  = monitor
    @reporter = reporter
  end

  attr_reader :count

  def each
    @count = 0
    start!
    result = @monitor.each do |item|
      @count += 1
      report!(@monitor.respond_to?(:pos) ? nil : @count)
      yield item
    end
    stop!
    return result
  end

  def start!
    @stopped_at = nil
    @started_at = Time.now.utc
    self
  end

  def report!(pos=nil)
    pos ||= @monitor.pos
    @reporter.printf "\r%4.0f%%", (pos.to_f / @monitor.size * 100)
    @reporter.flush
    self
  end

  def stop!
    @stopped_at = Time.now.utc
    @reporter.printf "\r          \r"
  end

  def elapsed
    seconds = elapsed_seconds
    @elapsed = '%.1f %s' % (seconds > 119 ? [seconds / 60, :minutes] : [seconds, :seconds])
  end

  def elapsed_seconds
    (@stopped_at || Time.now.utc) - @started_at
  end
end
