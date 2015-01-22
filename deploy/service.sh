#! /bin/sh
# /etc/init.d/$name: start $name.

### BEGIN INIT INFO
# Provides:             $name
# Required-Start:       ?
# Required-Stop:        ?
# Default-Start:        2 3 4 5
# Default-Stop:         S 0 1 6
# Short-Description:    $name
### END INIT INFO

PATH=/bin:/usr/bin:/sbin:/usr/sbin

name=dportalen
rundir=/usr/local/var/$name/
binpath=/usr/local/bin/$name # bin need not be named $name
binargs="Production"
user=dportalen

test -f $binpath || {
    echo "Binary not found at $binpath, please fix."
    exit 0
}

. /lib/lsb/init-functions

case "$1" in
  start)
    log_begin_msg "Starting $name daemon..."

    test -d  $rundir || {
      mkdir $rundir
      chown $user $rundir
    } || {
      echo "Unable to create working directory, please fix."
      exit 1
    }


    # start app as non-root with reading from kmsgpipe
    start-stop-daemon --start --background --quiet --chuid $user --chdir $rundir --exec $binpath -- $binargs
    log_end_msg $?
    ;;
  stop)
    log_begin_msg "Stopping $name daemon..."
    # stop app
    start-stop-daemon --stop --quiet --retry 3 --oknodo --exec $binpath
    log_end_msg $?
    ;;
  restart|force-reload)
    $0 stop
    sleep 1
    $0 start
    ;;
  *)
    log_success_msg "Usage: /etc/init.d/$name {start|stop|restart|force-reload}"
    exit 1
esac

exit 0
