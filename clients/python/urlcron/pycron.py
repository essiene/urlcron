from weblib import *
import simplejson
from datetime import datetime

class Pycron(object):

    def get(self, url):
        ret_val = simplejson.loads(get(url))
        return self.parse(ret_val)

    def post(self, url, data):
        str_data = self.build_url(data)
        ret_val = simplejson.loads(post(url, str_data))
        return self.parse(ret_val)

    def put(self, url, data):
        str_data = self.build_url(data)        
        ret_val = simplejson.loads(put(url, str_data))
        return self.parse(ret_val)

    def delete(self, url):
        ret_val = simplejson.loads(delete(url))
        return self.parse(ret_val)

    def build_url(self, data):
        year = data['year']
        month = data['month']
        day = data['day']
        hour = data['hour']
        minute = data['minute']
        seconds = data['seconds']
        if data.has_key('name'):
            name = data['name']
        else:
            name=""
        url = data['url']
        return 'name=' + name + \
                '&url=' + url + \
                '&year=' + year + \
                '&month=' + month + \
                '&day=' + day + \
                '&hour=' + hour + \
                '&minute=' + minute + \
                '&seconds=' + seconds

    def parse(self, json):
        status = json['status']
        data = json['data']
        return Schedule(status, data)


class Schedule(object):
    def __init__(self, status, data):
        self.status = status
        try:
            if data.has_key('name'):
                self.parse_schedule(data)
            else:
                self.data = data                
        except Exception:
            self.data = data                


    def parse_schedule(self, data):            
        name = data['name']
        pid = data['pid']
        start_time = self.get_date(data, 'start_time')
        time_created = self.get_date(data, 'time_created')
        time_completed = self.get_date(data, 'time_completed')
        url = data['url']
        url_status = data['url_status']
        url_headers = data['url_headers']
        url_content = data['url_content']
        status = data['status']

        self.build_schedule(name, pid, start_time, time_created, time_completed, url, url_status, url_headers, url_content, status)


    def build_schedule(self, name, pid, start_time, time_created, time_completed, url, url_status, url_headers, url_content, status):
        self.name=name
        self.pid = pid
        self.start_time = start_time
        self.time_created = time_created
        self.time_completed = time_completed
        self.url = url
        self.url_status = url_status
        self.url_headers = url_headers
        self.url_content = url_content
        self.status = status

        if pid == "undefined":
            self.pid = None 
        
        if url == "undefined":
            self.url = None

        if url_status == "undefined":
            self.url_status = None

        if url_headers == "undefined":
            self.url_headers = None

        if url_content == "undefined":
            self.url_content = None


    def get_date(self, json, date_dict):
        if json[date_dict] != "undefined":
            year = json[date_dict]['year']
            month = json[date_dict]['month']
            day = json[date_dict]['day']
            hour = json[date_dict]['hour']
            min = json[date_dict]['minute']
            secs = json[date_dict]['seconds']
            return datetime(year, month, day, hour, min, secs)
        else:
            return None
