import socket

HOST = 'localhost'    # The remote host
PORT = 9234              # The same port as used by the server
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect((HOST, PORT))
s.send('{"login": "user1", "password": "pwd"}')
#data = s.recv(1024)
s.close()
#print 'Received', repr(data)
