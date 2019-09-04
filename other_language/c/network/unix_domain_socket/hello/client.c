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

	msgh.msg_name = NULL;
	msgh.msg_namelen = 0;
	msgh.msg_iov = &iov;
	msgh.msg_iovlen = 1;
	iov.iov_base = &data;
	iov.iov_len = sizeof(int);

	data = 12345;
	fprintf(stderr, "Sending data = %d\n", data);

	printf("Not explicitly sending a credentials structure\n");
	msgh.msg_control = NULL;
	msgh.msg_controllen = 0;

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
