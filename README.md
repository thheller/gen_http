# Erlang WebApp Testing Area

These are some experiments at coming up with a Web Framework for Erlang.

Its still in drafting stage and no real "framework" has emerged yet,
I'm building this as a hobby and to learn me some Erlang. I have no real
experience in Erlang (except for a gazillion experiments), so this might
include some crappy Erlang code. Anyways, if you want to run this, do the
following:

Requirements:

* erlang (only tested R14B03)
* yaws (http://yaws.hyber.org, only tested 1.91)
* rebar (https://github.com/basho/rebar) 
* git

If you got all that, run this:

<pre>
    $ git clone git://github.com/thheller/gen_http.git
    $ cd gen_http
    $ rebar get-deps
    $ rebar compile

    % optional but cant hurt
    $ rebar eunit
</pre>

If all those worked, you need to setup yaws and configure a "Server".

# Yaws Server Config

We need a yaws server entry and some additional stuff, all pretty basic.
I have no experience with yaws so there might be smoother ways to set this up.

## Example:

    # required load paths
    ebin_dir = /Users/zilence/code/gen_http/ebin
    ebin_dir = /Users/zilence/code/gen_http/deps/bcrypt/ebin

    # very useful in development, thanks to mochiweb
    # runmod = reloader
    runmod = bcrypt

    <server localhost>
      port = 8000
      listen = 0.0.0.0
      docroot = /Users/zilence/code/gen_http/public
      statistics = true
      appmods = </, test_web exclude_paths static js css>

      <opaque>
        cookie_secret = "TU1t7hSwoP6VgEpFwTD2AU7NaaMXzDN3uGddUS9ujzUOW11XOzZBNrLOGBJkfZSy3ZhnP8RSKpH8qEtq6QtOskh6CzK98nHbHxuQwOmAWSvppmDxuI12NINEfcALvi1K"
      </opaque>
    </server>


## "cookie_secret"

"cookie_secret" must be a long, random String. *DO NOT USE MY EXAMPLE VALUE*.
It is used to sign&verify cookies, meaning we sign cookies so we can "trust"
them, since the User shouldn't be able to change them. They can still look at
them though, so dont put secrets in there. 


## Initializing the Sample

I have never used mnesia before, so this might all be completely wrong.

I start yaws with

<pre>
    yaws -i --mnesia-dir var/mnesia
     % then init the sample
    
    1> test_web:init_sample().
</pre>


There is a test running at: [http://zilence.net:8000](http://zilence.net:8000), not
sure how up-to-date I'll keep that. There is login/logout and register working. A Dummy
Test Account: test/testtest is created by default.

## Credits

I borrowed some code, since I didn't wanna write all that stuff myself. :P

So thanks goes to:

* yaws
[http://yaws.hyber.org](http://yaws.hyber.org)

* reloader.erl from mochiweb
[http://github.com/mochi/mochiweb](http://github.com/mochi/mochiweb)

* riak_core_util.erl from riak_core
[http://github.com/basho/riak_core](http://github.com/basho/riak_core)

* erlang-bcrypt
[http://github.com/smarkets/erlang-bcrypt](http://github.com/smarkets/erlang-bcrypt)

* twitter css "bootstrap"
[http://twitter.github.com/bootstrap/](http://twitter.github.com/bootstrap/)

