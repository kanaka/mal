### Running .mal scripts on PHP hosting ###

Create a symlink to `mal-web.php` with the same name as your `.mal` script and your script will be executed as if it was PHP.

Here's an example using local dev.

First build `mal-web.php`:

	cd mal/php
	make mal-web.php

Now you can create a web runnable mal script:

	echo '(println "Hello world!")' > myscript.mal
	ln -s mal-web.php myscript.php

Start a development server with `php -S 0.0.0.0:8000` and then browse to http://localhost:8000/myscript.php and you should see "Hello world!" in your browser as `myscript.mal` is run.

You can do the same thing on live PHP web hosting by copying `mal.php` up and creating a symlink for each `.mal` file you want to be web-executable.

### PHP interop ###

In [stepA_mal.mal](./tests/stepA_mal.mal) you can find some examples of PHP interop.

Eval PHP code:

	(php* "return 7;")
	7
	
	(php* "return array(7,8,9);")
	(7 8 9)

Native function call:

	(php/date "Y-m-d" 0)
	"1970-01-01"

Accessing PHP "superglobal" variables:

	(get php/_SERVER "PHP_SELF")
	"./mal"

