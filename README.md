linn
=====

process pool manager  

Build
-----

    $ rebar3 compile


Start
----
    
    linn:add_pool(your_pool_id , your_module , your_func , process_count)
    
**`your_module:your_func/0 will be execute and need {ok ,pid()}`**
    
Fetch Item
----
    
    linn:get_process(your_pool_id)
        