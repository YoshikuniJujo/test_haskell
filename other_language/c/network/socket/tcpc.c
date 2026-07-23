#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <string.h>
#include <unistd.h>

#define MSGSIZE 1024
#define BUFSIZE (MSGSIZE + 1)

int
main(int argc, char *argv[])
{
	printf("here 6\n");
	int sock;
	struct sockaddr_in servSockAddr;
	unsigned short servPort;
	char recvBuffer[BUFSIZE];
	char sendBuffer[BUFSIZE];
	printf("here 5\n");

	if (argc != 3) {
		fprintf(stderr, "argument count mismatch error.\n");
		exit(EXIT_FAILURE);
	}
	printf("here 4\n");

	memset(&servSockAddr, 0, sizeof(servSockAddr));
	servSockAddr.sin_family = AF_INET;
	printf("here 3\n");

	if (inet_aton(argv[1], &servSockAddr.sin_addr) == 0) {
		fprintf(stderr, "Invalid port number.\n");
		exit(EXIT_FAILURE);
	}

	if ((servPort = (unsigned short) atoi(argv[2])) == 0) {
		fprintf(stderr, "invalid port number.\n");
		exit(EXIT_FAILURE);
	}

	servSockAddr.sin_port = htons(servPort);
	printf("there\n");

	if ((sock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) {
		perror("socket() failed.");
		exit(EXIT_FAILURE);
	}

	printf("here\n");

	if (connect(sock, (struct sockaddr*) &servSockAddr, sizeof(servSockAddr)) < 0) {
		perror("connect() failed.");
		exit(EXIT_FAILURE);
	}

	printf("connect to %s\n", inet_ntoa(servSockAddr.sin_addr));

	while (1) {
		printf("please enter the characters:");
		if (fgets(sendBuffer, BUFSIZE, stdin) == NULL) {
			fprintf(stderr, "invalid input string.\n");
			exit(EXIT_FAILURE);
		}
		if (send(sock, sendBuffer, strlen(sendBuffer), 0) <= 0) {
			perror("send() failed.");
			exit(EXIT_FAILURE);
		}

		int byteRcvd = 0;
		int byteIndex = 0;

		while (byteIndex < MSGSIZE) {
			byteRcvd = recv(sock, &recvBuffer[byteIndex], 1, 0);
			if (byteRcvd > 0) {
				if (recvBuffer[byteIndex] == '\n') {
					recvBuffer[byteIndex] = '\0';
					if (strcmp(recvBuffer, "quit") == 0) {
						close(sock);
						return EXIT_SUCCESS;
					} else {
						break;
					}
				}
				byteIndex += byteRcvd;
			} else if (byteRcvd == 0) {
				perror("ERR_EMPTY_RESPONSE");
				exit(EXIT_FAILURE);
			} else {
				perror("recv() failed.");
				exit(EXIT_FAILURE);
			}
			printf("server return: %s\n", recvBuffer);
		}

	}

	return EXIT_SUCCESS;
}
