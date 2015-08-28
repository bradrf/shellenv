from time import sleep
import re
import multiprocessing
import boto.ec2
import boto.route53

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
        self.boto = Volume.get_volume(self.id)
        self.status = self.boto.status

    def wait_for(self, status, interval=3):
        self.update()
        while self.status != status:
            print self.id+':', self.status, '!=', status
            sleep(interval)
            self.update()

    def update(self):
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
        self.private = metadata.private_ip_address
        self.public_ip = metadata.ip_address
        self.private_ip = metadata.private_ip_address
        self.tags = metadata.tags
        try:
            self.name = self.tags['Name'].lower()
            del(self.tags['Name'])
        except:
            self.name = self.public if self.public is not None else self.private
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
            self.boto.remove_tag(key)
            del(self.tags[key])
        self.__update_tags_str()

    def __set_state(self, state):
        self.state = state
        self.is_running = state == 'running'

    def __update_tags_str(self):
        self.tags_str = ','.join('%s=%s'%(k,re.sub(r'\s+','_',v)) for k,v in self.tags.iteritems())

    def __str__(self):
        pstr = ' public=' + self.public if self.public else ''
        vstr = ' volumes=' + ','.join(str(v) for v in self.volumes) if len(self.volumes) > 0 else ''
        return '<Instance: id=%s region=%s is_running=%s is_linux=%s%s private=%s tags=[%s]%s>' % (
            self.id, self.region, self.is_running, self.is_linux, pstr, self.private, self.tags_str, vstr)

    def to_list(self):
        return [self.name, self.id, self.region, self.state, self.public or '.', self.private, self.tags_str]

######################################################################

def get_connection(region=None):
    return Connections.get_connection(region)

def get_access_key_id():
    return get_connection().aws_access_key_id

ZONE_TYPES = ['A','CNAME']
def get_zone_dns_records(zone):
    global ZONE_TYPES
    if not isinstance(zone, boto.route53.zone.Zone):
        zone = boto.route53.connection.Route53Connection().get_zone(zone)
    return [DnsRecord(r) for r in boto.route53.connection.Route53Connection().get_all_rrsets(zone.id) if r.type in ZONE_TYPES]

def get_dns_records():
    zones = boto.route53.connection.Route53Connection().get_zones()
    pool = multiprocessing.Pool(len(zones))
    records = pool.map(get_zone_dns_records, zones)
    return [val for subl in records for val in subl] # flatten

def get_dns_name(value, records):
    names = []
    for record in records:
        if value in record.values:
            names.append(record.name)
    return names

def get_region_instances(region):
    try:
        return [Instance(i) for i in get_connection(region).get_only_instances()]
    except boto.exception.EC2ResponseError:
        print 'Unable to get instances in region:', region
        return []

def get_region_volumes(region):
    return get_connection(region).get_all_volumes()

def get_instances():
    regions = [r.name for r in boto.ec2.regions() if r.name not in ['us-gov-west-1','cn-north-1']];
    pool = multiprocessing.Pool(len(regions))
    volumes = pool.map(get_region_volumes, regions)
    Volume.set_volumes([val for subl in volumes for val in subl])
    pool = multiprocessing.Pool(len(regions))
    instances = pool.map(get_region_instances, regions)
    return [val for subl in instances for val in subl] # flatten
