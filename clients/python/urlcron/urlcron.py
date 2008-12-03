import weblib
import util
import simplejson

class UrlCron(object):
    def __init__(self, host, port):
        self.host = host
        self.port = port
        self.base_url = "http://%s:%s" % (host, port)

    def create(self, name, start_time, url):
        pass

    def create(self, start_time, url):
        pass

    def cancel(self, name):
        url = self.base_url + "/" + name
        json = weblib.delete(url)
        return Response(json)

    def get(self, name):
        url = self.base_url + "/schedule/" + name
        json = weblib.get(url)
        return Response(json)

    def set_url(self, name, url):
        pass

    def enable(self, name):
        url = self.base_url + "/schedule/enable/" + name
        json = weblib.get(url)
        return Response(json)

    def disable(self, name):
        url = self.base_url + "/schedule/disable/" + name
        json = weblib.get(url)
        return Response(json)


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
