import sys
import weblib
import util
import simplejson

class Schedule(object):
    def __init__(self, data):
        self.name = util.get_string(data, "name")
        self.pid = util.get_string(data, "pid")

        self.start_time = util.get_date(data, "start_time")
        self.time_created = util.get_date(data, "time_created")
        self.time_started = util.get_date(data, "time_started")
        self.time_completed = util.get_date(data, "time_completed")

        self.url = util.get_string(data, "url")
        self.url_status = util.get_string(data, "url_status")
        self.url_headers = util.get_string(data, "url_headers")
        self.url_content = util.get_string(data, "url_content")
        self.status = util.get_string(data, "status")

        util.ensure(self.name, "name")
        util.ensure(self.start_time, "start_time")
        util.ensure(self.url, "url")
        util.ensure(self.status, "status")


class Response(object):
    def __init__(self, json):
        response_dict = simplejson.loads(json)
        if response_dict["status"]:
            self.status = True
        else:
            self.status = False

        self.data = Response.parse(response_dict["data"])

    @staticmethod
    def parse(param):
        if isinstance(param, dict):
            return Schedule(param)
        return param

    @staticmethod
    def to_response(func):

        def inner(*args, **kw):
            try:
                json = func(*args, **kw)
                return Response(json)
            except:
                (type, value, traceback) = sys.exc_info()
                json = '{"status":0, "data":"%s"}' % (value)
                return Response(json)

        inner.__name__ = func.__name__
        inner.__dict__ = func.__dict__

        return inner

class UrlCron(object):
    def __init__(self, host, port):
        self.host = host
        self.port = port
        self.base_url = "http://%s:%s" % (host, port)

    @Response.to_response
    def create(self, url, name=None, start_time=None):
        appurl = self.base_url + "/schedule/"

        if not start_time:
            start_time = datetime.datetime.now() + datetime.timedelta(seconds=60)

        time_tuple = util.to_tuplelist(start_time)

        if name:
            data = util.urlencode(time_tuple, name=name, url=url)
        else:
            data = util.urlencode(time_tuple, url=url)

        return weblib.post(appurl, data)

    @Response.to_response
    def cancel(self, name):
        url = self.base_url + "/schedule/" + name
        return weblib.delete(url)

    @Response.to_response
    def get(self, name):
        url = self.base_url + "/schedule/" + name
        return weblib.get(url)


    @Response.to_response
    def set_url(self, name, url):
        pass

    @Response.to_response
    def enable(self, name):
        url = self.base_url + "/schedule/enable/" + name
        return weblib.get(url)

    @Response.to_response
    def disable(self, name):
        url = self.base_url + "/schedule/disable/" + name
        return weblib.get(url)
