-module(cream_benchmark).

-export([bench/1]).


bench(ConfigMap) ->
    #{
        max_capacity := MaxSize,
        cache_count := CacheCount,
        worker_count_per_cache := WorkerCountPerCache,
        value_fun_type := ValueFunType,
        iterations := Iterations
    } = ConfigMap,

    ValueFun = value_fun_(ValueFunType),

    % create caches
    Caches = setup_caches(CacheCount, MaxSize),

    % create workers
    Workers = setup_workers(Caches, WorkerCountPerCache, ValueFun, Iterations),

    MonitorRefsSet = sets:from_list(monitor_workers(Workers), [{version, 2}]),
    T1 = os:timestamp(),
    start_workers(Workers),
    wait_for_workers(MonitorRefsSet),
    T2 = os:timestamp(),
    TimeMicros = timer:now_diff(T2, T1),

    cleanup_caches(Caches),

    #{
        config_map => ConfigMap,
        time_micros => TimeMicros
    }.


setup_caches(Count, MaxSize) ->
    [
        begin
            {ok, Cache} = cream:new(MaxSize, []),
            Cache
        end || _ <- lists:seq(1, Count)].


cleanup_caches(_Caches) ->
    [].


setup_workers(Caches, CountPerCache, ValueFun, IterationCount) ->
    [
        spawn_worker(Cache, ValueFun, IterationCount) ||
            Cache <- Caches, _ <- lists:seq(1, CountPerCache)
    ].


monitor_workers(Workers) ->
    [monitor(process, Worker) || Worker <- Workers].


start_workers(Workers) ->
    [send_go(Worker) || Worker <- Workers].


wait_for_workers(MonitorRefsSet) ->
wait_for_workers(sets:is_empty(MonitorRefsSet), MonitorRefsSet).


wait_for_workers(true, _) ->
    ok;
wait_for_workers(false, MonitorRefsSet) ->
    MonitorRef = wait_for_worker(),
    MonitorRefsSetNew = sets:del_element(MonitorRef, MonitorRefsSet),
    wait_for_workers(sets:is_empty(MonitorRefsSetNew), MonitorRefsSetNew).


wait_for_worker() ->
    receive
        {'DOWN', MonitorRef, process, _, _} -> MonitorRef;
        _ -> exit(unknown_message)
    end.


spawn_worker(Cache, ValueFun, IterationCount) ->
    spawn_link(cache_runner_(Cache, ValueFun, IterationCount)).


cache_runner_(Cache, ValueFun, IterationCount) ->
    fun() ->
        wait_for_go(),
        cache_runner(Cache, ValueFun, IterationCount)
    end.


send_go(Worker) ->
    Worker ! go.


wait_for_go() ->
    receive
        go -> ok;
        _ -> exit(unknown_message)
    end.


cache_runner(_, _, 0) ->
    ok;
cache_runner(Cache, ValueFun, IterationCount) ->
    cream:cache(Cache, key(), ValueFun),
    cache_runner(Cache, ValueFun, IterationCount -1).


value_fun_(fast) ->
    Value = lists:duplicate(100, [a,b,c]),
    fun() -> Value end;

value_fun_(slow) ->
    fun() -> slow_func(key()) end;

value_fun_(slow2) ->
    fun() -> slow_func(50) end.


key() ->
    trunc(rand:normal(50.0, 50.0)).


factorial(1) -> 1;
factorial(N) when is_integer(N) and (N > 1) -> N * factorial(N-1).

slow_func(K) ->
        [math:sqrt(factorial(K)) || _ <- lists:seq(1,200)].

