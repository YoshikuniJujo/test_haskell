openssl req -x509 -sha256 -nodes -days 365 -newkey rsa:2048 -subj /CN=localhost -keyout server.key -out server.crt
