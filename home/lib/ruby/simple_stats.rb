require 'ostruct'

# TODO: add way to snapshot these computed values so they don't recompute when asking for something
#       like the error_margin (i.e. it'll result in the mean computed twice)

module SimpleStats
  def self.Array(*args)
    array = Array.new(*args)
    array.extend(self)
    return array
  end

  def snaptry(sym)
    @snapshot ? @snapshot[sym] ||= yield : yield
  end

  def withsnap
    @snapshot ||= OpenStruct.new
    yield
  ensure
    @snapshot = nil
  end

  def sum
    snaptry(:sum) { self.reduce(:+) }
  end

  def mean
    snaptry(:mean) { self.length == 0 ? nil : self.sum / self.length }
  end

  def median
    snaptry(:median) do
      sorted = self.sort
      len = sorted.length
      (sorted[(len - 1) / 2] + sorted[len / 2]) / 2.0
    end
  end

  def percentile(percentile)
    snaptry(:percentile) do
      self.length > 1 or return self.first
      percentile /= 100.0
      sorted = self.sort
      len = sorted.length - 1
      k = (percentile * len + 1).floor - 1
      f = (percentile * len + 1).modulo(1)
      sorted[k] + (f * (sorted[k+1] - sorted[k]))
    end
  end

  def sample_variance
    snaptry(:sample_variance) do
      m = self.mean
      sum = self.inject(0){|accum, i| accum + (i - m) ** 2}
      sum / (self.length - 1).to_f
    end
  end

  def standard_deviation
    snaptry(:standard_deviation) do
      Math.sqrt(self.sample_variance)
    end
  end

  # See for the following definitions: http://stattrek.com/estimation/confidence-interval.aspx

  CRITICAL_VALUE = 1.96 # 95% confidence in a normal distribution

  def standard_error
    snaptry(:standard_error) do
      self.length > 0 or return 0
      self.standard_deviation / Math.sqrt(self.length)
    end
  end

  def error_margin
    snaptry(:error_margin) { CRITICAL_VALUE * self.standard_error }
  end

  def confidence_range
    snaptry(:confidence_range) do
      m = self.mean
      e = m * self.error_margin
      (m-e..m+e)
    end
  end

  def to_stats(percentile=95)
    withsnap do
      OpenStruct.new(
        sum: self.sum,
        mean: self.mean,
        median: self.median,
        percentile: self.percentile(percentile),
        standard_deviation: self.standard_deviation,
        confidence_range: self.confidence_range,
        min: self.min,
        max: self.max,
        samples: self.length
      )
    end
  end
end
