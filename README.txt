READ
HERE:

Right now, cleandata.R is working. This working code is saved in cleandata.R.working.
In cleandata.R, we are making a few changes to figure out how to combine all the adjournments into one command.

drtdownloader.py:
----------------
This python program downloads cases from DRT's horrible site. There is no easy way to download the bulk data, so this program automates the process. For every date in the range you specify, it launches a browser, goes to DRT's website, queries it to get a list of cases, and then clicks on each of them to get the data we need, and stores the data. If it errors out, it retries that date onwards. The program creates a separate output file for every date in the range you have given.

Prerequisites:
1. You need selenium bindings for python. Your distribution might have it, but it is probably old. Better to get the latest. Do this:
	sudo pip install selenium
2. You need a driver that allows selenium to interface with the browser. For firefox, it is called geckodriver. I don't think it is packaged for your distribution, so search and download the 64-bit linux version. Put it in your $PATH.
3. You need the python package 'lxml'. Again, use:
	sudo pip install lxml

Executing:
	python <programname> startdate enddate > logfile
Please redirecting stdout to a logfile, it helps debugging. If you screw up (wrong arguments etc), it gives you a helpful error message.

cleandata.R:
-----------
This R script reads in a tarball of all the data we downloaded above, and reads it into a data.frame. It also figures out next Dates for most of the hearings.

What we need to do is: for each hearing, we need:
1. Date of hearing
2. Next date scheduled
3. Whether failure or not? Not possible
4. Who caused failure? Not possible
5. Why failure? Not possible
6. What type of case?
7. Before whom?
8. Plaintiff
9. Defendant
10.If lender is private or public
