module SimpleStats
  def self.Array(*args)
    array = Array.new(*args)
    array.extend(self)
    return array
  end

  def sum
    self.reduce(:+)
  end

  def mean
    self.length == 0 ? nil : self.sum / self.length
  end

  def median
    sorted = self.sort
    len = sorted.length
    return (sorted[(len - 1) / 2] + sorted[len / 2]) / 2.0
  end

  def percentile(percentile)
    self.length > 1 or return self.first
    percentile /= 100.0
    sorted = self.sort
    len = sorted.length - 1
    k = (percentile * len + 1).floor - 1
    f = (percentile * len + 1).modulo(1)
    return sorted[k] + (f * (sorted[k+1] - sorted[k]))
  end

  def sample_variance
    m = self.mean
    sum = self.inject(0){|accum, i| accum + (i - m ) ** 2 }
    return sum / (self.length - 1).to_f
  end

  def standard_deviation
    Math.sqrt(self.sample_variance)
  end
end
