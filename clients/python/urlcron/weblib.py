import urllib2

class WebError(Exception):
    '''A WebError has occured'''

class ExtendedRequest(urllib2.Request):
    def __init__(self, url, data=None, headers={}, origin_req_host=None, unverifiable=False):
        urllib2.Request.__init__(self, url, data, headers, origin_req_host, unverifiable)

    def get_method(self):
        if self.has_data():
            return "PUT"
        else:
            return "DELETE"

def catchurlerror(func):
    '''Decorator to run webmethods and catch
    urllib2.URLError exceptions to raise custome
    weblib.WebError exceptions.
    '''
    def infunc(url, *args):
        try:
            return func(url, *args)
        except urllib2.URLError: 
            e = '%s is not responding. Is remote server up?' % (url)
            raise WebError(str(e))

    infunc.__name__ = func.__name__
    infunc.__dict__ = func.__dict__

    return infunc


def xurlopen(url, data=None):
    req = ExtendedRequest(url, data)

    o = urllib2.build_opener()
    res = o.open(req)

    return res

def get(url):
    return urllib2.urlopen(url).read()

@catchurlerror
def post(url, data):
    return urllib2.urlopen(url, data).read()

def delete(url):
    return xurlopen(url).read()

def put(url, data):
    return xurlopen(url, data).read()
