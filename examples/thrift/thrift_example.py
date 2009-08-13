import sys,os
gen_py_path = os.path.join(os.path.dirname(os.path.realpath(__file__)), '..', '..', 'lib', 'proto', 'gen-py')
sys.path.append(gen_py_path)

from cloudthrift import CommandInterface
from cloudthrift.constants import *

from thrift import Thrift
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol

transport = TSocket.TSocket('localhost', 11223)
transport = TTransport.TBufferedTransport(transport)
protocol = TBinaryProtocol.TBinaryProtocol(transport)

ci = CommandInterface.Client(protocol)

transport.open()

cloudquery = CloudQuery()
cloudquery.name = "app"

resp = ci.run_command(cloudquery, "name", "")
print resp.response

resp = ci.run_command(cloudquery, "maximum_instances", "")
print resp.response
