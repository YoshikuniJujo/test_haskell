#define _GNU_SOURCE

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

	int optval;

	int data;
	struct iovec iov;
	struct msghdr msgh;
	ssize_t nr;

	union {
		char buf[CMSG_SPACE(sizeof(struct ucred))];
		struct cmsghdr align;
	} controlMsg;

	struct cmsghdr *cmsgp;
	struct ucred *ucredp, ucred;

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
		optval = 1;
		if (setsockopt(clifd, SOL_SOCKET, SO_PASSCRED, &optval, sizeof(optval)) == -1) {
			fprintf(stderr, "setsockopt error errno[%d]\n", errno);
			exit(-1); }
		msgh.msg_iov = &iov;
		msgh.msg_iovlen = 1;
		iov.iov_base = &data;
		iov.iov_len = sizeof(int);
		msgh.msg_control = controlMsg.buf;
		msgh.msg_controllen = sizeof(controlMsg.buf);
		nr = recvmsg(clifd, &msgh, 0);
		if (nr == -1) { fprintf(stderr, "error occur"); exit(-1); }
		int len = msgh.msg_iov->iov_len;
//		int len = read (clifd, recvbuf, sizeof(recvbuf));
//		recvbuf[len] = 0;
		fprintf(stdout, "%s\n", (char *)msgh.msg_iov->iov_base);

		cmsgp = CMSG_FIRSTHDR(&msgh);

		if (cmsgp == NULL || cmsgp->cmsg_len != CMSG_LEN(sizeof(struct ucred)))
			{ fprintf(stderr, "bad"); exit(-1); }
		if (cmsgp->cmsg_level != SOL_SOCKET)
			{ fprintf(stderr, "bad"); exit(-1); }
		if (cmsgp->cmsg_type != SCM_CREDENTIALS)
			{ fprintf(stderr, "bad"); exit(-1); }

		ucredp = (struct ucred *) CMSG_DATA(cmsgp);

		printf("pid = %ld, uid = %ld, gid = %ld\n",
			(long) ucredp -> pid, (long) ucredp -> uid, (long) ucredp -> gid );

	} else {
		fprintf(stderr, "error errno[%d]\n", errno);
		exit(- 1); }

	close(clifd);
	close(lsnfd);

	return 0;
}
