from unittest import TestCase
from pycron import Pycron
import simplejson

class TestPyCron(object):
    def setUp(self):
        self.pyco = Pycron()            
        pass

    def tearDown(self):
        pass

    def test_get(self):
        data = {"year":"2008", "month":"12", "day":"29", "hour":"16", "minute":"20", "seconds":"11", "name":"xmas", "url":"google"}
        result = self.pyco.post("http://localhost:8118/schedule", data)
        assert result.data == "xmas"

        result = self.pyco.get("http://localhost:8118/schedule/xmas")
        assert result.name == "xmas"

    def test_post(self):
        data = {"year":"2008", "month":"12", "day":"29", "hour":"16", "minute":"20", "seconds":"11", "name":"xmas", "url":"google"}
        result = self.pyco.post("http://localhost:8118/schedule", data)
        assert result.data == "xmas"

    def test_put(self):
        data = {"year":"2008", "month":"12", "day":"29", "hour":"16", "minute":"20", "seconds":"11", "name":"xmas", "url":"google"}
        result = self.pyco.post("http://localhost:8118/schedule", data)
        assert result.data == "xmas"

        data = {"year":"2008", "month":"12", "day":"29", "hour":"16", "minute":"20", "seconds":"11", "url":"google"}
        result = self.pyco.put("http://localhost:8118/schedule/xmas", data)
        assert result.data == "Successful"

    def test_delete(self):
        data = {"year":"2008", "month":"12", "day":"29", "hour":"16", "minute":"20", "seconds":"11", "name":"xmas", "url":"google"}
        result = self.pyco.post("http://localhost:8118/schedule", data)
        assert result.data == "xmas"

        result = self.pyco.delete("http://localhost:8118/schedule/xmas")
        assert result.data == "Successful"

    def test_get_enable(self):
        data = {"year":"2008", "month":"12", "day":"29", "hour":"16", "minute":"20", "seconds":"11", "name":"xmas", "url":"google"}
        result = self.pyco.post("http://localhost:8118/schedule", data)
        assert result.data == "xmas"

        result= self.pyco.get("http://localhost:8118/schedule/enable/xmas")
        assert result.data == "Successful"

    def test_get_disable(self):
        data = {"year":"2008", "month":"12", "day":"29", "hour":"16", "minute":"20", "seconds":"11", "name":"xmas", "url":"google"}
        result = self.pyco.post("http://localhost:8118/schedule", data)
        assert result.data == "xmas"

        result= self.pyco.get("http://localhost:8118/schedule/disable/xmas")
        assert result.data == "Successful"
