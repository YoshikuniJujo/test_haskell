class Daemon
end

def Daemon.start
	exit!(0) if fork
	Process::setsid
	exit!(0) if fork
	Dir::chdir("/")
	File::umask(0)
	STDIN.reopen("/dev/null")
#	STDOUT.reopen("/dev/null", "w")
	STDERR.reopen("/dev/null", "w")
	yield if block_given?
end

Daemon.start do
	puts "foo"
	File.open("/home/tatsuya/tmp/sample1.txt", "w") do |f|
		f.puts("Hello, world!")
	end
	puts "bar"
end
