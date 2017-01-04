Rate-monotonic is a free (GPL) Common Lisp library for scheduling periodic threads.  It provides an API inspired by the [RTEMS](https://www.rtems.org) (also GPL) [Rate Monotonic Manager](https://docs.rtems.org/doc-current/share/rtems/html/c_user/Rate-Monotonic-Manager.html#Rate-Monotonic-Manager).

# Example

The first is a simple example of a period scheduled to elapse every 500 milliseconds.  The statistics are measuring time in internal-real-time units, capturing minimum, maximum, and average time it takes to exit a period call to the subsequent period call.  In this case, it takes less than 1 millisecond to loop.

```lisp
CL-USER> (rm:with-timer-period () 
           (let ((p (rm:make-timer-period)))
             (dotimes (i 11)
               (rm:period p :ms 500)
               (format t "i: ~A~%" i)
               (force-output))
             
             (rm:period-statistics p)))
i: 0
i: 1
i: 2
i: 3
i: 4
i: 5
i: 6
i: 7
i: 8
i: 9
i: 10
#<STAT :COUNT 10 :MISSED 0 :MIN 0 :AVG 0 :MAX 0>
```

The next example adds a sleep call, and a call to STATUS which returns the period state and time since exiting PERIOD.  The first sleep call starts at 10 milliseconds, increasing by 10 milliseconds each loop.  Since the period is constant at 100 milliseconds, the STATUS call eventually returns :EXPIRED, and the statistics show 5 missed periods in this case.

```lisp
CL-USER> (rm:with-timer-period (25) 
           (let ((p (rm:make-timer-period)))
             (dotimes (i 14)
               (rm:period p :seconds 0.1)
               (sleep (* 0.01 (1+ i)))
               (multiple-value-bind
                     (state time) (rm:status p)
                 (format t "i: ~D :state ~A :time ~A~%" 
                         i state time))
               (force-output))
             (rm:period-statistics p)))
i: 0 :state RUNNING :time 10
i: 1 :state RUNNING :time 21
i: 2 :state RUNNING :time 31
i: 3 :state RUNNING :time 41
i: 4 :state RUNNING :time 50
i: 5 :state RUNNING :time 60
i: 6 :state RUNNING :time 70
i: 7 :state RUNNING :time 81
i: 8 :state RUNNING :time 90
i: 9 :state EXPIRED :time 101
i: 10 :state EXPIRED :time 112
i: 11 :state EXPIRED :time 121
i: 12 :state EXPIRED :time 131
i: 13 :state EXPIRED :time 141
#<STAT :COUNT 13 :MISSED 5 :MIN 10 :AVG 72 :MAX 131>
```
