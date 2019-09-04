#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <sys/un.h>
#include <poll.h>

#include "hello.h"

int
main(int argc, char *argv[])
{

	int clifd, lsnfd;
	struct sockaddr_un cliaddr, srvaddr;
	struct pollfd fds[1] = {0,};

	lsnfd = socket(AF_UNIX, SOCK_STREAM, 0);
	if (lsnfd < 0) {
		fprintf(stderr, "socket error errno[%d]\n", errno);
		exit(-1); }

	unlink(UNIXDOMAIN_PATH);
	memset(&srvaddr, 0, sizeof(struct sockaddr_un));
	srvaddr.sun_family = AF_UNIX;
	strcpy(srvaddr.sun_path, UNIXDOMAIN_PATH);
	if (bind(lsnfd, (struct sockaddr *)&srvaddr, sizeof(struct sockaddr_un)) < 0) {
		fprintf(stderr, "bind error errno[%d]\n", errno);
		exit(-1); }

	if (listen(lsnfd, 5) < 0) {
		fprintf(stderr, "listen error errno[%d]\n", errno);
		exit(-1); }

	fds[0].fd = lsnfd;
	fds[0].events = POLLIN;

	poll(fds, 1, -1);

	if (fds[0].revents & POLLIN) {
		char recvbuf[2048] = "";
		memset(&cliaddr, 0, sizeof(struct sockaddr_un));
		socklen_t addrlen = sizeof(struct sockaddr_un);
		if ((clifd = accept(lsnfd, (struct sockaddr *)&cliaddr, &addrlen)) < 0) {
			fprintf(stderr, "accept error errno[%d]\n", errno);
			exit(-1); }
		int len = read (clifd, recvbuf, sizeof(recvbuf));
		recvbuf[len] = 0;
		fprintf(stdout, "%s\n", recvbuf);
	} else {
		fprintf(stderr, "error errno[%d]\n", errno);
		exit(- 1); }

	close(clifd);
	close(lsnfd);

	return 0;
}
