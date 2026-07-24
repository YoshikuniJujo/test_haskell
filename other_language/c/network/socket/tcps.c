#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>

#define QUEUELIMIT 5
#define MSGSIZE 1024
#define BUFSIZE (MSGSIZE + 1)

int
main(int argc, char *argv[])
{
	int servSock;
	int clitSock;
	struct sockaddr_in servSockAddr;
	struct sockaddr_in clitSockAddr;
	unsigned short servPort;
	unsigned int clitLen;
	char recvBuffer[BUFSIZE];
	int recvMsgSize, sendMsgSize;

	if (argc != 2) {
		fprintf(stderr, "argument count mismatch error.\n");
		exit(EXIT_FAILURE);
	}

	if ((servPort = (unsigned short) atoi(argv[1])) == 0) {
		fprintf(stderr, "invalid port number.\n");
		exit(EXIT_FAILURE);
	}

	printf("%d\n", servPort);

	if ((servSock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) {
		perror("socket() failed.");
		exit(EXIT_FAILURE);
	}

	memset(&servSockAddr, 0, sizeof(servSockAddr));
	servSockAddr.sin_family = AF_INET;
	servSockAddr.sin_addr.s_addr = htonl(INADDR_ANY);
	servSockAddr.sin_port = htons(servPort);

	if (bind(servSock, (struct sockaddr *) &servSockAddr, sizeof(servSockAddr))  < 0) {
		perror("bind() failed.");
		exit(EXIT_FAILURE);
	}

	if (listen(servSock, QUEUELIMIT) < 0) {
		perror("listen() failed.");
		exit(EXIT_FAILURE);
	}

	printf("here\n");

	while(1) {
		printf("there\n");
		clitLen = sizeof(clitSockAddr);
		printf("here 3\n");
		if ((clitSock = accept(servSock,
			(struct sockaddr *) &clitSockAddr, &clitLen)) < 0) {
			perror("accept() failed.\n");
			exit (EXIT_FAILURE);
		}
		printf("connected from %s.\n", inet_ntoa(clitSockAddr.sin_addr));

		while(1) {
			if ((recvMsgSize = recv(clitSock, recvBuffer, BUFSIZE, 0)) < 0) {
			} else if (recvMsgSize == 0) {
				fprintf(stderr, "connection closed by foreign host.\n");
				break;
			}

			recvBuffer[recvMsgSize] = '\0';
			printf("%s\n", recvBuffer);

			if ((sendMsgSize = send(clitSock, recvBuffer, recvMsgSize, 0)) < 0) {
				perror("send() failed.");
				exit(EXIT_FAILURE);
			} else if (sendMsgSize == 0) {
				fprintf(stderr, "connection closed by foreign host.\n");
				break;
			}
			printf("herehere\n");
		}
		close(clitSock);
	}

	close(servSock);

	return EXIT_SUCCESS;
}
