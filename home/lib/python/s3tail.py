import boto

class S3Tail(object):
    BUFFER_SIZE = 1 * (1024*1024) # MiB

    # TODO: cache files locally for some amount of time (24 hours?)

    # TODO: support "globbing" to iterate mutliple sources fields
    # e.g. s3://unitycloud-collab-logs/production/s3/collab-production-s3-access-2016-07-23-*
    #      maybe only numerical globs assumeing times? somethign esle?
    def __init__(self, bucket_name, prefix, line_handler,
                 key_handler=None, bookmark=None, region=None):
        if region:
            self._conn = boto.s3.connect_to_region(region)
        else:
            self._conn = boto.connect_s3()
        self._bucket = self._conn.get_bucket(bucket_name)
        self._prefix = prefix
        self._line_handler = line_handler
        self._key_handler = key_handler
        if bookmark:
            self._bookmark_key, self._bookmark_line_num = bookmark.split(':')
            if len(self._bookmark_key) == 0:
                self._bookmark_key = None
        else:
            self._bookmark_key = None
            self._bookmark_line_num = 0

    def get_bookmark(self):
        if self._marker:
            return self._marker + ':' + str(self._line_num)
        if self._line_num:
            return ':' + str(self._line_num)

    def watch(self):
        # TODO: see about ordering response in reverse? should try to get most recent log in bucket?
        for key in self._bucket.list(prefix=self._prefix, marker=self._bookmark_key):
            self._bookmark_key = None
            if self._key_handler:
                result = self._key_handler(key.name)
                if not result:
                    continue
            result = self._read(key)
            if result is not None:
                return result
            self._marker = key.name

    ######################################################################
    # private

    def _read(self, key):
        self._buffer = ''
        self._line_num = 0
        key.open()
        while not key.closed:
            line = self._next_line(key)
            self._line_num += 1
            if self._line_num < self._bookmark_line_num:
                continue
            self._bookmark_line_num = 0
            result = self._line_handler(self._line_num, line)
            if result is not None:
                return result
        self._bookmark_line_num = 0 # safety in case bookmark count was larger than actual lines

    def _next_line(self, key):
        i = None
        for n in range(0,3): # try reading up to three times the buffer size
            i = self._buffer.find("\n")
            if i > -1:
                break
            more_data = key.read(S3Tail.BUFFER_SIZE)
            if len(more_data) > 0:
                self._buffer += more_data
            else:
                key.close()
                i = len(self._buffer) + 1 # use remaining info in buffer
                break
        line = self._buffer[0:i]
        self._buffer = self._buffer[i+1:]
        return line
