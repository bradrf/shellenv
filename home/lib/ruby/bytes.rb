class Bytes
  BINARY  = [%w{B KiB MiB GiB TiB PiB EiB ZiB YiB}, 1024]
  DECIMAL = [%w{B KB MB GB TB PB EB ZB YB}, 1000]

  def self.humanize(val, precision: 0.1, units: :binary)
    abbrev, mult = units == :binary ? BINARY : DECIMAL

    t = [1,'B']
    abbrev.each_with_index do |a,i|
      m = mult ** i
      val < m and break
      t = [m,a]
    end

    return "%#{precision}f %s" % [val.to_f / t[0], t[1]]
  end

  def self.dehumanize(str)
    m = str.match(%r{(\d*(\.\d+)?)\s*(\w{0,3})}) or raise "Unable to dehumanize: #{str}"

    v = m[2] ? m[1].to_f : m[1].to_i

    abbrev, mult = m[3][1] == ?i ? BINARY : DECIMAL

    unless exp = abbrev.index(m[3])
      if m[3] == nil || m[3].empty?
        exp = 0
      else
        raise "Unknown unit: #{m[3]} (extracted from #{str})"
      end
    end

    return v * (mult ** exp)
  end
end
