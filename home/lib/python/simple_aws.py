from time import sleep
from threading import Thread
import re
import boto.ec2
import boto.route53

# TODO:
#   * add full status info (see ec2_wait_for)

########################################
class Connections:
    connections = {}

    @staticmethod
    def get_connection(region=None):
        # stripping the "placement" designator on a region string
        if region: region = region.rstrip('abcd')
        connection = Connections.connections.get(region)
        if not connection:
            if region:
                connection = boto.ec2.connect_to_region(region)
            else:
                connection = boto.connect_ec2()
            Connections.connections[region] = connection
        return connection

########################################
class DnsRecord(object):
    def __init__(self, metadata):
        self.boto = metadata
        self.name = metadata.name.rstrip('.')
        self.rtype = 'ALIAS' if len(metadata.resource_records) == 0 else metadata.type
        self.ttl = metadata.ttl
        self.values = []
        if metadata.alias_dns_name != None:
            str(self.values.append(metadata.alias_dns_name.rstrip('.')))
        self.values += [str(rr).rstrip('.') for rr in metadata.resource_records]

    def __str__(self):
        if len(self.values) > 1:
            valstr = ' values=[%s]' % ','.join(self.values)
        elif len(self.values) == 1:
            valstr = ' value=' + self.values[0]
        else:
            valstr = ''
        return '<DnsRecord: name=%s type=%s ttl=%s%s>' % (
            self.name, self.rtype, self.ttl, valstr)

    def to_list(self):
        return [self.name, self.rtype, self.ttl] + self.values

########################################
class Volume(object):
    volumes = {}

    @staticmethod
    def set_volumes(volumes):
        for v in volumes:
            Volume.volumes[v.id] = v

    @staticmethod
    def get_volume(id, region=None):
        volume = Volume.volumes.get(id)
        if volume:
            volume.update()
        else:
            volume = Volume.volumes[id] = Connections.get_connection(region).get_all_volumes(volume_ids=[id])[0]
        return volume

    def __init__(self, device_name, block_device):
        self.block_device = block_device
        self.id = block_device.volume_id
        self.device_name = device_name
        self.delete_on_termination = block_device.delete_on_termination
        self.status = block_device.status
        # lazy-load real volume info only when an update is requested...
        self.boto = None

    def wait_for(self, status, interval=3):
        self.update()
        while self.status != status:
            print self.id+':', self.status, '!=', status
            sleep(interval)
            self.update()

    def update(self):
        if not self.boto:
            self.boto = Volume.get_volume(self.id)
        self.boto.update()
        self.status = self.boto.status

    def __str__(self):
        return '<Volume: id=%s device=%s status=%s>' % (self.id, self.device_name, self.status)

########################################
class Instance(object):
    def __init__(self, metadata):
        self.boto = metadata
        self.id = metadata.id
        self.__set_state(metadata.state)
        self.is_linux = metadata.platform == None
        self.platform = metadata.platform
        self.region = metadata.placement
        self.public = metadata.public_dns_name
        self.private = metadata.private_dns_name
        self.public_ip = metadata.ip_address
        self.private_ip = metadata.private_ip_address
        self.tags = metadata.tags
        self.__set_safe_name()
        self.__update_tags_str()
        self.volumes = [Volume(n, d) for n,d in metadata.block_device_mapping.iteritems()]

    def wait_for(self, state, interval=3):
        self.update()
        while self.state != state:
            print self.id+':', self.state, '!=', state
            sleep(interval)
            self.update()

    def update(self):
        self.boto.update()
        self.__set_state(self.boto.state)
        # might need to update ips/names too?

    def tags_set(self, key, val):
        if val:
            self.boto.add_tag(key, val)
            self.tags[key] = val
        else:
            if self.tags.has_key(key):
                self.boto.remove_tag(key)
                del(self.tags[key])
        self.__update_tags_str()

    def __set_safe_name(self):
        name = self.tags.get('Name')
        if name:
            del(self.tags['Name'])
            name = re.sub(r'[^0-9a-zA-Z_.-]', '_', name)
        if not name or len(name) < 1:
            name = self.public if self.public else self.private
        self.name = name

    def __set_state(self, state):
        self.state = state
        self.is_running = state == 'running'

    def __update_tags_str(self):
        self.tags_str = ','.join('%s=%s'%(k,re.sub(r'\s+','_',v)) for k,v in self.tags.iteritems())

    def __str__(self):
        pstr = ' public=' + self.public if self.public else ''
        vstr = ' volumes=' + ','.join(str(v) for v in self.volumes) if len(self.volumes) > 0 else ''
        return '<Instance: id=%s name=%s region=%s is_running=%s is_linux=%s%s private=%s tags=[%s]%s>' % (
            self.id, self.name, self.region, self.is_running, self.is_linux, pstr, self.private, self.tags_str, vstr)

    def to_list(self):
        return [self.name, self.id, self.region, self.state, self.public or '.', self.private, self.tags_str]

######################################################################

def get_connection(region=None):
    return Connections.get_connection(region)

def get_access_key_id():
    return get_connection().aws_access_key_id

ZONE_TYPES = ['A','CNAME']
def get_zone_dns_records(zone, records):
    global ZONE_TYPES
    if not isinstance(zone, boto.route53.zone.Zone):
        zone = boto.route53.connection.Route53Connection().get_zone(zone)
    records += [DnsRecord(r) for r in boto.route53.connection.Route53Connection().get_all_rrsets(zone.id) if r.type in ZONE_TYPES]

def get_dns_records():
    zones = boto.route53.connection.Route53Connection().get_zones()
    threads = []
    zone_recs = {}
    for zone in zones:
        zone_recs[zone] = []
        th = Thread(target=get_zone_dns_records, args=(zone, zone_recs[zone], ))
        th.start()
        threads.append(th)
    for th in threads:
        th.join()
    records = []
    for recs in zone_recs.values():
        records += recs
    return records

def get_dns_names(value, records):
    return [r.name for r in records if value in r.values]

def get_key_for(ip):
    if re.match(r'(^127\.)|(^10\.)|(^172\.1[6-9]\.)|(^172\.2[0-9]\.)|(^172\.3[0-1]\.)|(^192\.168\.)', ip):
        return 'private-ip-address'
    return 'public-ip-address'

# See http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html
# Keys (i.e. types) are AND'd and restrict the set of results! However, values are OR'd.
# Thus, the caching will have to make multiple calls for each "type" of filter.
NAME_MATCHERS = '__name_matchers'
def selector_for(*args):
    if len(args) > 0 and isinstance(args[0], list):
        args = args[0]
    selector = { NAME_MATCHERS: [] }
    for arg in args:
        if re.match(r'^i-[0-9a-z]{8}$', arg):
            key = 'instance-id'
        elif re.match(r'^[0-9]{0,3}\.[0-9]{0,3}\.[0-9]{0,3}\.[0-9]{0,3}$', arg):
            key = get_key_for(arg)
        else:
            m = re.match(r'^/(.*)/$', arg)
            if m:
                # this will return all instances that have the Name tag (regardless of value)
                key = 'tag-key'
                arg = 'Name'
                selector[NAME_MATCHERS].append(re.compile(m.group(1)))
            else:
                key = 'tag:Name'
        selector.setdefault(key,[]).append(arg)
    return selector if len(selector) > 1 or len(selector[NAME_MATCHERS]) > 0 else None

# Must pass in the list to avoid thread-result issues when called from get_instances()
def get_region_instances(region, instances, selector=None):
    conn = get_connection(region)
    if not selector:
        name_matchers = None
        ins = conn.get_only_instances()
    else:
        name_matchers = selector[NAME_MATCHERS]
        ins = []
        for key, values in selector.iteritems():
            if len(values) < 1 or key == NAME_MATCHERS: continue
            filters = {}
            filters[key] = values
            ins += conn.get_only_instances(filters=filters)
    if name_matchers:
        # post-process name matching
        for i in ins:
            i = Instance(i)
            for r in name_matchers:
                if r.search(i.name):
                    instances.append(i)
                    break
    else:
        instances += [Instance(i) for i in ins]

def get_instances(selector=None):
    regions = [r.name for r in boto.ec2.regions() if r.name not in ['us-gov-west-1','cn-north-1']]
    threads = []
    region_ins = {}
    for region in regions:
        region_ins[region] = []
        th = Thread(target=get_region_instances, args=(region, region_ins[region], selector, ))
        th.start()
        threads.append(th)
    for th in threads:
        th.join()
    instances = []
    for ins in region_ins.values():
        instances += ins
    return instances
