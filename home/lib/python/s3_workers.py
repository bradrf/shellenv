import sys
import logging
import threading
import queue

_logger = logging.getLogger(__name__)

class Job(object):
    def __init__(self):
        self._state = 'ready'
        self._stop_requested = threading.Event()

    def run(self, *args, **kwargs):
        try:
            if not self._stop_requested.isSet():
                self._state = 'running'
                self._runner(*args, **kwargs)
        finally:
            self._state = 'stopped'

    def stop(self):
        self._stop_requested.set()

    def __str__(self, details = ''):
        return '%s(%s) is %s' % (self.__class__.__name__, details, self._state)

    def _runner(self):
        pass

class S3ListJob(Job):
    def __init__(self, bucket, prefix, selector, key_handler, progress):
        super(self.__class__, self).__init__()
        self._bucket = bucket
        self._prefix = prefix
        self._selector = selector
        self._key_handler = key_handler
        self._progress = progress

    def __str__(self):
        return super(self.__class__, self).__str__(self._bucket.name + '/' + self._prefix)

    def _runner(self):
        for key in self._bucket.list(prefix=self._prefix):
            if self._stop_requested.isSet():
                break
            self._progress()
            if not key.md5:
                key.md5 = key.etag[1:-1] # GROSS. HACK. Likely break if multipart-uploaded...
            if self._is_selected(key):
                self._key_handler(key)

    def _is_selected(self, key):
        if not self._selector:
            return True
        size = key.size
        name = key.name
        md5 = key.md5
        return eval(self._selector)

class Worker(threading.Thread):
    _all_jobs_submitted = threading.Event()

    @classmethod
    def all_jobs_submitted(self):
        self._all_jobs_submitted.set()

    def __init__(self, work):
        super(self.__class__, self).__init__()
        self._work = work
        self._current_lock = threading.Lock()
        self._current_job = None
        self._stop_requested = threading.Event()

    def __str__(self):
        statestr = 'alive' if self.is_alive() else 'dead'
        jobstr = ' current=' + str(self._current_job) if self._current_job else ''
        return '%s(%s%s)' % (self.__class__.__name__, statestr, jobstr)

    def run(self):
        while True:
            if self._stop_requested.isSet():
                break
            if Worker._all_jobs_submitted.isSet() and self._work.empty():
                break
            try:
                with self._current_lock:
                    self._current_job = self._work.get(timeout=0.1)
            except queue.Empty:
                continue
            try:
                _logger.debug('starting: %s', self._current_job)
                self._current_job.run()
            finally:
                self._work.task_done()
                with self._current_lock:
                    self._current_job = None

    def stop(self):
        self._stop_requested.set()
        with self._current_lock:
            if self._current_job:
                self._current_job.stop()

class SimpleProgress(object):
    def __init__(self):
        self._lock = threading.RLock()

    def report(self, msg, *args):
        with self._lock:
            sys.stdout.write("\r" + (msg % args))
            sys.stdout.flush()

    def write(self, msg, *args):
        with self._lock:
            sys.stdout.write("\r" + (msg % args) + "\n")
            sys.stdout.flush()

    def finish(self):
        sys.stdout.write("\n")
        sys.stdout.flush()

class S3KeyProgress(SimpleProgress):
    def __init__(self):
        super(self.__class__, self).__init__()
        self._counter = 0
        self._selected = 0

    def report(self):
        with self._lock:
            self._counter += 1
            super(self.__class__, self).report('Selected %d of %d keys',
                                               self._selected, self._counter)
    def write(self, msg, *args):
        with self._lock:
            self._selected += 1
            super(self.__class__, self).write(msg, *args)
