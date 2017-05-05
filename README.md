# Calendar

I keep a text file calendar at the moment.  I wanted to learn how to use Parsec, so I built this little calendar manager.  

## Instructions

The program expects to find a file 'config' with a single line containing a path such as `/home/user/` and files `calendar.text` and `past.text` in this path.  The file `calendar.text` should contain a list of entries of the form

`startDate [startTime] [endDate] [endTime] body [(note)] [^d|^w|^y]`

The user can use indentation to define sub-entries.  Dates are in the form `YYYY-MM-DD`, times have the form `HH:MM` (24 hour), and repeats are daily `^d`, weekly `^w`, or yearly `^y`.

## Functionality

Currently, the program reads in `calendar.text` and sorts the entries into a list of past appointments and active appointments.  In addition, any repeating appointments are updated.  (A copy of each past repeating appointment is retained in the list of past appointments.)  Then the active appointments are sorted and written to `calendar.text` while the past appointments are appended to `past.text`.
