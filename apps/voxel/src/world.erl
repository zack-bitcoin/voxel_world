-module(world).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         add/4,delete/3,new/0,compress/0,read/0]).
-define(SIZE, 20).
-record(db, {raw, %the world in editable tuple format.
             compressed}).%the world in javascript compatible base64 format.

init(ok) -> {ok, #db{}}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(compress, X) -> 
    {noreply, X#db{compressed = compress_inner(X#db.raw, 0, ?SIZE)}};
handle_cast(new, _) -> 
    {noreply, #db{raw = new_empty_world(?SIZE)}};
handle_cast({delete, A, B, C}, X) ->
    X2 = delete_from_volume(A, B, C, X#db.raw),
    {noreply, X#db{raw = X2}};
handle_cast({add, A, B, C, V}, X) ->
    X2 = add_to_volume(A, B, C, V, X#db.raw),
    {noreply, X#db{raw = X2}};
handle_cast(_, X) -> {noreply, X}.
handle_call(read, _From, X) -> 
    {reply, X#db.compressed, X};
handle_call(_, _From, X) -> {reply, X, X}.

new_empty_world(Size) ->
    new2(0, [], Size).
new2(S, W, S) -> list_to_tuple(W);
new2(F, W, S) -> 
    NewFace = new_face(0, F, [], S),
    new2(F+1, [NewFace|W], S).
new_face(S, FN, W, S) -> list_to_tuple(W);
new_face(L, FN, W, S) -> 
    NewLine = new_line(0, L, FN, [], S),
    new_face(L+1, FN, [NewLine|W], S).
new_line(S, LN, FN, W, S) -> list_to_tuple(W);
new_line(E, LN, FN, W, S) -> 
    NE = new_element(E, LN, FN, S),
    new_line(E+1, LN, FN, [NE|W], S).
new_element(X, Y, Z, S) ->
    if
        ((Y>0) and (Y<(S-5))) ->
        %((Y<95) and (Y>50)) ->
        %    1 + 
        %        (((X div 6) +
        %              (Y div 6) +
        %              (Z div 6)) rem 9);

        1+((X+Z+Y) rem 8);
        true -> 0
    end.

delete_from_volume(X, Y, Z, D3) ->
    D2 = element(X+1, D3),
    D22 = delete_from_page(Y, Z, D2),
    setelement(X+1, D3, D22).
delete_from_page(Y, Z, D2) ->
    D1 = element(Y+1, D2),
    D12 = delete_from_line(Z, D1),
    setelement(Y+1, D2, D12).
delete_from_line(Z, D1) ->
    setelement(Z+1, D1, 0).

add_to_volume(X, Y, Z, V, D3) ->
    D2 = element(X+1, D3),
    D22 = add_to_page(Y, Z, V, D2),
    setelement(X+1, D3, D22).
add_to_page(Y, Z, V, D2) ->
    D1 = element(Y+1, D2),
    D12 = add_to_line(Z, V, D1),
    setelement(Y+1, D2, D12).
add_to_line(Z, V, D1) ->
    if
        (element(Z+1, D1) == 0) ->
            setelement(Z+1, D1, V);
        true -> D1
    end.


compress_inner(T, S, S) -> <<>>;
compress_inner(T, I, S) -> 
    E = element(I+1, T),
    F = compress_face(E, 0, S),
    R = compress_inner(T, I+1, S),
    <<F/binary, R/binary>>.
compress_face(F, S, S) -> <<>>;
compress_face(F, I, S) -> 
    E = element(I+1, F),
    L = compress_line(E, 0, S),
    R = compress_face(F, I+1, S),
    <<L/binary, R/binary>>.
compress_line(L, S, S) -> <<>>;
compress_line(L, I, S) -> 
    E = element(I+1, L),
    R = compress_line(L, I+1, S),
    <<E:8, R/binary>>.
new() ->
    gen_server:cast(?MODULE, new),
    compress().
compress() ->
    gen_server:cast(?MODULE, compress).
read() ->
    gen_server:call(?MODULE, read).
delete(X, Y, Z) ->
    true = int_range_check(X, ?SIZE),
    true = int_range_check(Y, ?SIZE),
    true = int_range_check(Z, ?SIZE),
    gen_server:cast(?MODULE, {delete, X, Y, Z}).
add(X, Y, Z, V) ->
    true = int_range_check(X, ?SIZE),
    true = int_range_check(Y, ?SIZE),
    true = int_range_check(Z, ?SIZE),
    true = int_range_check(V, 256),
    gen_server:cast(?MODULE, {add, X, Y, Z, V}).
   
int_range_check(X, R) -> 
    is_integer(X)
        and (X > -1)
        and (X < R).
