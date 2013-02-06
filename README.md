WISE-R
======

WISE-R is a set of utility codes that allow for integrating R functionality into the WISE platform. rApache includes files to enable calling R functions from VLE in javascript. rLocal includes files to enable access to WISE-related data (e.g., student work) from R console for post hoc analysis.

This document provides you with the following tips:

1. rAapache installation

2. rApache configuration on server

3. VLE configuration for rApache

4. Adding and calling R functions

1. rApache installation
-----------------------

Follow the steps introduced on the author's webpage: <http://rapache.net/manual.html>

2. rApache configuration on server
----------------------------------

2-1. Copy all the files in rApache folder to a directory (e.g., usr/local/wise_r)

2-2. Add a directive in httpd.conf. Below is an example to receive R requests through http://localhost/rhandler

		<Location /rhandler>
		    SetHandler r-handler
		    RFileHandler /usr/local/wise_r/handler.R
		</Location>

2-3. Restrict the access only from localhost in httpd.conf. Below is an example.

		<Directory /usr/local/wise_r>
		    Order deny,allow
			Deny from all
		    Allow from 127.0.0.1
		</Directory>

3. VLE configuration for rApache
----------------------------------------------

Set rApache_url with the handler in vle.properties

		rApache_url=http://localhost/rhandler

4. Add and call R functions
---------------------------

To add a R function, define FNAME (data) {...} in FNAME.r in "methods" folder. Below is an example.

		# function: sample (in sample.r)
		sample <- function(data) {
			paste(data, sep = ", ")
		}
		
To call this function from a VLE step, call "executeRMethod(fname, data, callback)" in javascript

		node.view.utils.executeRMethod("sample", "test", function(data) {
			console.log("output is %o", data);
		}
