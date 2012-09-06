pdferl
======

An erlang library for fast creation of pdf, xls or rtf files

dependencies
======

You will need:

ruby (latest version)

rubygems and gem erlectricity

java (latest version)

erlang R14B03 or later

before start
======

A template must be created in iReport. 

Template must has extension .jasper.

You must build template using xml as datasource.

You must know the xpath used to build the template.

clone and compile
======

Clone:

			$ git clone https://github.com/jorgegarrido/pdferl.git
			
Into pdferl

			$ cd pdferl
			
Compile:

			$ make compile

starting & configuring pdferl
======

To start pdferl inside your application or directly from an erlang shell use:

			application:start(pdferl)
			
It will start pdferl but you need configure the path to ruby_src directory in the dev.conf
file that is placed in pdferl directory.
The path to ruby_src directory must be an absolute path, example:

			%%
			%% Path to ruby scripts used by the system.
			%%
			{path_to_ruby_src, "/path/to/pdferl/ruby_src/"}.

When dev.conf has been configured with the correct path, now you can load the config file for
pdferl application, inside your code or erlang shell type:

			pdferl_cfg:config("dev.conf")

You must provide the path where dev.conf file is actually placed.

using pdferl inside my application
======

After pdferl has been started, you just call a single function:
	
			> ok = pdferl:create(Args).

The Args passed to function are in a single tuple, and it must look like this:

			> Args = {Xml, Xpath, JasperFile, NameFile, TypeFile}.
			
Xml : the xml in erlang string format.

Xpath : the xpath used by jasper when you build the template report.

JasperFile : the jasper file (template report).

NameFile : the name file that pdf, xls or rtf will be set.

TypeFile : pdf, xls or rtf.

Now a file has been created, but pdferl can notify you if it is true, the in your code,
you must implement a simple receive for process:

			do_recv() ->
			    receive
        			{response, <<"ok">>} -> ok
    			end.
    			
The tuple {response, <<"ok">>} is sent by pdferl to you, when you receive this, you ensure that file
(pdf, xls or rtf) has been created correctly.

xml creation
======

You can use xml module to create xml format, the function encode on this takes as input a proplist,
also the function set_header sets the  correct header for a xml format, example:

			> TagPerson = [{name, "name here!"}, {last_name, "last name here!"}, {age, "00"}].
			...
			> xml:set_header(xml:encode([{document, [{people, [{person, TagPerson}]}]}])).
			"<?xml version=\"1.0\" encoding=\"UTF-8\" ?><document><people><person><name>name here!....

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
			  ok_created_succesfully
	
If the response was ok, then the file has been placed in reports directory, sweet!!

known bugs
======

Sometimes when a jasper template is created, the fonts in the host where pdferl is running are not available.
To solve the problem install the correct fonts to your host.

LICENSE
======

THIS SOFTWARE IS LICENSED UNDER BSD LICENSE. see LICENSE.txt for more info
