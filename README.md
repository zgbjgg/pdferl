pdferl
======

create pdf or xls from erlang

dependencies
======

ruby (latest version)
rubygems and gem erlectricity
java (latest version)
erlang RB1403 or later

clone and compile
======

Clone:

			$ git clone https://github.com/jorgegarrido/pdferl.git
			
Into pdferl

			$ cd pdferl
			
Compile:

			$ make compile

examples
======

Let's compile the examples placed under directory example

			$ make example
			
The previous command will open a erlang shell, inside a erlang shell type the next instructions:

			> pdferl_cfg:config("dev.conf").
			ok
			
Previous command loads the ruby_src path in the system (you must edit this file : dev.conf)
Now let's create a xls or pdf using the examples:

			> sample_xls:make().
			<<"ok">>
	
If the response was ok, then the file has been placed in reports directory, sweet!!
