require 'shellwords'

TYPES = {
  '-s' => File::LOCK_SH,
  '-x' => File::LOCK_EX,
}

type = TYPES[ARGV[0]]

if ARGV.size < 3 || type.nil?
  $stderr.puts <<EOF

usage: #{File.basename $0, '.rb'} {-s|-x} <lock_file> <command> [<command_args>...]

EOF
  exit 1
end

file = File.open(ARGV[1], File::RDWR)
file.flock(type)
system(ARGV[2..-1].shelljoin)
exit $?.exitstatus
