import nose.tools
import datetime
import urlcron

class TestUrlCron(object):
    def setUp(self):
        self.urlcron = urlcron.UrlCron("localhost", 8118)

    def test_create_with_name(self):
        onehour = datetime.datetime.now() + datetime.timedelta(seconds=60*60)
        r = self.urlcron.create("http://localhost:8118/echo/foo", name="pyschedule1", start_time=onehour)

        assert r.status == True
        assert r.data == "pyschedule1"

    def test_create_anonymous(self):
        onehour = datetime.datetime.now() + datetime.timedelta(seconds=60*60)
        r = self.urlcron.create("http://localhost:8118/echo/foo", start_time=onehour)

        assert r.status == True

    def test_get(self):
        r = self.urlcron.get("pyschedule1")

        assert r.status == True
        assert r.data.name == "pyschedule1"
        assert r.data.status == "enabled"

    def test_cancel(self):
        start_time = datetime.datetime.now() + datetime.timedelta(seconds=60*5)
        r = self.urlcron.create("http://localhost:8118/echo/foo_bar", start_time=start_time)
        r2 = self.urlcron.cancel(r.data)
        r3 = self.urlcron.get(r.data)

        assert r.status == True

        assert r2.status == True
        assert r2.data == "Successful"

        assert r3.status == False
        assert r3.data == "not_found"

    def test_set_url(self):
        start_time = datetime.datetime.now() + datetime.timedelta(seconds=60*5)
        r = self.urlcron.create("http://localhost:8118/echo/foo_bar", start_time=start_time)

        assert r.status == True

        r2 = self.urlcron.get(r.data)
        assert r2.data.url == "http://localhost:8118/echo/foo_bar"

        r3 = self.urlcron.set_url(r.data, "http://localhost:9119/add?msisdn=1234&callid=1")
        assert r3.status == True

        r4 = self.urlcron.get(r.data)
        assert r4.data.url == "http://localhost:9119/add?msisdn=1234&callid=1"


    def test_enable_disable(self):
        start_time = datetime.datetime.now() + datetime.timedelta(seconds=60*5)
        r = self.urlcron.create("http://localhost:8118/echo/foo_bar", start_time=start_time)

        assert r.status == True

        r2 = self.urlcron.get(r.data)
        assert r2.data.status == "enabled"

        r3 = self.urlcron.disable(r.data)
        assert r3.status == True

        r4 = self.urlcron.get(r.data)
        assert r4.data.status == "disabled"

        r5 = self.urlcron.enable(r.data)
        assert r5.status == True

        r6 = self.urlcron.get(r.data)
        assert r6.data.status == "enabled"

