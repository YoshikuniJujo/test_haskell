#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <errno.h>

#include "hello.h"

int
main(int argc, char *argv[])
{
	struct sockaddr_un addr;
	int srvfd;

	int data;
	struct msghdr msgh;
	struct iovec iov;

	union {
		char buf[CMSG_SPACE(sizeof(struct ucred))];
		struct cmsghdr align; } controlMsg;
	struct cmsghdr *cmsgp;

	struct ucred *ucredp;

	pid_t pid;
	uid_t uid;
	gid_t gid;

	if (argc > 1) { gid = atoi(argv[1]); } else { gid = getgid(); }
	if (argc > 2) { uid = atoi(argv[2]); } else { uid = getuid(); }
	if (argc > 3) { pid = atoi(argv[3]); } else { pid = getpid(); }

	msgh.msg_name = NULL;
	msgh.msg_namelen = 0;
	msgh.msg_iov = &iov;
	msgh.msg_iovlen = 1;
	iov.iov_base = &data;
	iov.iov_len = sizeof(int);

	data = 12345;
	fprintf(stderr, "Sending data = %d\n", data);

	msgh.msg_control = controlMsg.buf;
	msgh.msg_controllen = sizeof(controlMsg.buf);

	memset(controlMsg.buf, 0, sizeof(controlMsg.buf));
	cmsgp = CMSG_FIRSTHDR(&msgh);
	cmsgp->cmsg_len = CMSG_LEN(sizeof(struct ucred));
	cmsgp->cmsg_level = SOL_SOCKET;
	cmsgp->cmsg_type = SCM_CREDENTIALS;

	ucredp = (struct ucred *) CMSG_DATA(cmsgp);
	ucredp->pid = pid;
	ucredp->uid = uid;
	ucredp->gid = gid;

	srvfd = socket(AF_UNIX, SOCK_STREAM, 0);
	if (srvfd < 0) {
		fprintf(stderr, "socket error errno[%d]\n", errno);
		exit(-1); }

	memset(&addr, 0, sizeof(struct sockaddr_un));
	addr.sun_family = AF_UNIX;
	strcpy(addr.sun_path, UNIXDOMAIN_PATH);

	if (connect(srvfd, (struct sockaddr *)&addr, sizeof(struct sockaddr_un)) < 0) {
		fprintf(stderr, "connect error errno[%d]\n", errno);
		exit(-1); }

/*
	if (write(srvfd, "Hello, world!", strlen("Hello, world!")) < 0) {
		fprintf(stderr, "write error errno[%d]\n", errno);
		exit(-1); }
		*/

	if (sendmsg(srvfd, &msgh, 0) < 0) {
		fprintf(stderr, "sendmsg error errno[%d]\n", errno);
		exit(-1); }

	close(srvfd);

	return 0;
}
