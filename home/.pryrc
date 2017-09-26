# -*- mode: ruby; -*-

if defined?(PryByebug)
  def use_debug_aliases!
    [
      Pry.commands.alias_command('c', 'continue'),
      Pry.commands.alias_command('s', 'step'),
      Pry.commands.alias_command('n', 'next'),
      Pry.commands.alias_command('f', 'finish'),
      Pry.commands.alias_command('b', 'break')
    ]
  end

  # Hit Enter to repeat last command
  Pry::Commands.command(/^$/, "repeat last command") do
    _pry_.run_command Pry.history.to_a.last
  end
end

defined? Rails and
  Pry.config.prompt_name = Rails.env.to_s

load "#{ENV['HOME']}/.irbrc"
