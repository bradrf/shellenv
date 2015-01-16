import multiprocessing
import boto.ec2
import boto.route53

########################################
class DnsRecord(object):
    def __init__(self, metadata):
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
class Instance(object):
    def __init__(self, metadata):
        self.id = metadata.id
        self.is_running = metadata.state == 'running'
        self.state = metadata.state
        self.is_linux = metadata.platform == None
        self.platform = metadata.platform
        self.region = metadata.placement
        self.public = metadata.public_dns_name
        self.private = metadata.private_ip_address
        self.tags = metadata.tags
        try:
            self.name = self.tags['Name'].lower()
            del(self.tags['Name'])
        except:
            self.name = self.public if self.public is not None else self.private

    def __str__(self):
        pstr = ' public=' + self.public if self.public else ''
        return '<Instance: id=%s region=%s is_running=%s is_linux=%s%s private=%s tags=[%s]>' % (
            self.id, self.region, self.is_running, self.is_linux, pstr, self.private,
            ','.join('%s="%s"'%(k,v) for k,v in self.tags.iteritems()))

    def to_list(self):
        tstr = ','.join('%s="%s"'%(k,v) for k,v in self.tags.iteritems())
        return [self.name, self.id, self.region, self.state, self.public or '.', self.private, tstr]

######################################################################

def get_zone_dns_records(zone):
    types = ['A','CNAME'] # todo: make this cfg'able
    if not isinstance(zone, boto.route53.zone.Zone):
        zone = boto.route53.connection.Route53Connection().get_zone(zone)
    return [DnsRecord(r) for r in boto.route53.connection.Route53Connection().get_all_rrsets(zone.id) if r.type in types]

def get_dns_records():
    zones = boto.route53.connection.Route53Connection().get_zones()
    pool = multiprocessing.Pool(len(zones))
    records = pool.map(get_zone_dns_records, zones)
    return [val for subl in records for val in subl] # flatten

def get_dns_name(value, records):
    names = []
    for record in records:
        if value in record.resource_records or value == record.alias_dns_name:
            names.append(record.name.rstrip('.'))
    return names

def get_region_instances(region):
    try:
        return [Instance(i) for i in boto.ec2.connect_to_region(region).get_only_instances()]
    except boto.exception.EC2ResponseError:
        print 'Unable to get instances in region:', region
        return []

def get_instances():
    regions = [r.name for r in boto.ec2.regions() if r.name not in ['us-gov-west-1','cn-north-1']];
    pool = multiprocessing.Pool(len(regions))
    instances = pool.map(get_region_instances, regions)
    return [val for subl in instances for val in subl] # flatten
