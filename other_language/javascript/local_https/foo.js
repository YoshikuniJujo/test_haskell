import { readFileSync } from 'fs';
import https from 'https';
import { IncomingMessage } from 'http';

const PORT = 9090;
const HOST = '127.0.0.1';

const options = {
	key: readFileSync('./server.key'),
	cert: readFileSync('./server.crt')
};

function startHttpServer(portNumber, hostName) {
	return new Promise((resolve, reject) => {
		https.createServer(options, (request, response) => {
			response.writeHead(200, {
				'Content-Type': 'text/plain'
			});
			response.end(`welcome to local HTTPS page!`);
			console.log(`${JSON.stringify(request.headers)}`);
			resolve(request);
		}).listen(portNumber, hostName);
	});
}

(async (portNumber, hostName) => {
	const req = await startHttpServer(portNumber, hostName);
	console.log(`${JSON.stringify(req.headers)}`);
})(PORT, HOST);
