import atexit
import os
import readline
import rlcompleter

readline.set_history_length(10000)
readline.parse_and_bind('tab: complete')

history_path = os.path.expanduser('~/.pyhist')

def save_history(history_path=history_path):
    import readline
    readline.write_history_file(history_path)

if os.path.exists(history_path):
    if os.path.isfile(history_path): readline.read_history_file(history_path)

atexit.register(save_history)
del os, atexit, readline, rlcompleter, save_history, history_path

print('(loaded {0})'.format(__file__))
