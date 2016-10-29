### Running .mal scripts on PHP hosting ###

Create a symlink to `mal.php` with the same name as your `.mal` script and your script will be executed as if it was PHP.

Here's an example using local dev:

	cd php
	make mal.php
	echo '(prn "Hello world!")' > myscript.mal
	ln -s mal.php myscript.php
	php -S 0.0.0.0:8000

Then browse to http://localhost:8000/myscript.php and you should see "Hello world!" in your browser as `myscript.mal` is run.

You can do the same thing on live PHP web hosting by copying `mal.php` up and creating a symlink for each `.mal` file you want to be web-executable.
