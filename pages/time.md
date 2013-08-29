# Time

* [HyperSpec: Time](http://www.lispworks.com/documentation/HyperSpec/Body/25_ad.htm)
* [CLtL2: Time Functions](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node232.html)

### [get-universal-time] => universal-time

Returns the current universal time, which is the number of
seconds that have elapsed since 00:00 of January 1, 1900 in
the GMT time zone.

~~~
(get-universal-time) ;=> 3574524644
~~~

### [decode-universal-time] universal-time \[zone\] => sec, min, hr, day, m, yr, day-of-week, daylight-p, zone

Returns the decoded time represented by the given *universal time*.

~~~
(defconstant +months+ '(1 "Jan" 2 "Feb" 3 "Mar" 4 "Apr" 5 "May" 6 "Jun"
                        7 "Jul" 8 "Aug" 9 "Sep" 10 "Oct" 11 "Nov" 12 "Dec"))
(defconstant +days+ '(0 "Mon" 1 "Tue" 3 "Wed" 4 "Thu" 5 "Fri" 6 "Sat" 7 "Sun"))

(defun universal-time-to-rfc822 (ut)
  (multiple-value-bind (s m h d month y weekday dst-p tz) (decode-universal-time ut)
    (declare (ignore dst-p))
    (let ((day (getf +days+ weekday))
          (mon (getf +months+ month))
          (tzsign (if (minusp tz) "-" "+")))
          (format nil "~a, ~2,'0d ~a ~d ~2,'0d:~2,'0d:~2,'0d ~a~2,'0d00" day d mon y h m s tzsign tz))))

(universal-time-to-rfc822 (get-universal-time)) ;=> "Tue, 09 Apr 2013 14:04:52 +0800"
~~~

### [get-decoded-time] => sec, min, hour, day, month, year, day-of-week, daylight-p, zone

Equivalent to `(decode-universal-time (get-universal-time))`.

~~~
(multiple-value-bind (sec min hr day m yr day-of-week dst-p tz) (get-decoded-time)
  (list sec min hr date m yr day-of-week dst-p tz)) ;=> (51 31 12 9 4 2013 1 T 8)
~~~

### [encode-universal-time] sec min hr day m yr \[zone\] => universal-time

Converts from decoded time format to a universal time.

### [get-internal-run-time] => internal-time

Returns as an [integer] the current run time in internal
time units. Its precise meaning is implementation-dependant,
but the intent is that the difference between two calls to
this function return the amount of time expended on behalf
of the executing program.

### [get-internal-real-time] => internal-time

Returns as an [integer] the current time in internal time
units, relative to an arbitrary time base. The difference
between the values of two calls to this function is the
amount of elapsed real time (i.e., clock time) between the
two calls.

### [sleep] seconds => nil

Causes execution to cease and become dormant for
approximately the *seconds* of real time indicated by
seconds, whereupon execution is resumed.

~~~
(let ((then (get-universal-time)))
  (sleep 10)
  (- (get-universal-time) then))   ;=> 5 [secs elapsed]
~~~

### [internal-time-units-per-second]

Constant, a positive integer. The number of internal time
units in one second.
