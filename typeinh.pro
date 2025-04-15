% 1. x,y,z
% 2. app(M, N)
% 3. abs(x, M)

i(abs(x,x)).
k(abs(x,abs(y,x))).
kstar(abs(x,abs(y,y))).
s(abs(x,abs(y,abs(z,app(app(x,z),app(y,z)))))).
c0(T) :- kstar(T).
c1(abs(f,abs(x,app(f,x)))).
c4(abs(f,abs(x,app(f,app(f,app(f,app(f,x))))))).
w(abs(x,app(x,x))).
omega(app(W,W)) :- w(W).

% 1. alpha, beta, gamma, ....
%% 2. from_to(S, T)

%% ti(from_to(alpha, alpha)).
%% tk(from_to(alpha, from_to(beta, alpha))).
%% ts(from_to(from_to(alpha,from_to(beta,gamma)),from_to(from_to(alpha,beta),from_to(alpha,gamma)))).

:- op(140, xfy, -->).

% 1. alpha, beta, gamma, ....
% 2. S --> T

ti(alpha --> alpha).
tk(alpha --> beta --> alpha).
tkstar(alpha --> beta --> beta).
tc((alpha --> alpha) --> alpha --> alpha).
ts((alpha --> beta --> gamma) --> (alpha --> beta) --> alpha --> gamma).
ty((alpha --> alpha) --> alpha).
tx(alpha --> (alpha --> beta) --> (beta --> alpha) --> beta).

:- op(150, xfx, :).

% abs(x,x) : alpha --> alpha

:- set_prolog_flag(occurs_check, true).

% types(+Visited, +Gamma, -Ms, +ArgTypes)
types(_, _, [], []).
types(V, Gamma, [M | Ms], [T | Ts]) :- types(V, Gamma, M : T),
                                       types(V, Gamma, Ms, Ts).

% deconstruct_type(+T, ?ArgTypes, ?ResultType).
deconstruct_type(Alpha, [], Alpha). % :- atom(Alpha).
deconstruct_type(R --> S, [R | ArgTypes], ResultType) :- deconstruct_type(S, ArgTypes, ResultType).

% construct_app(?Fun, +Args, ?Term).
construct_app(Fun, [], Fun).
construct_app(Fun, [Arg | Args], Term) :- construct_app(app(Fun, Arg), Args, Term).

% types(+Visited, +Gamma, +M : ?T).

types(V, Gamma, abs(X, N) : R --> S)  :- types(V, [X : R | Gamma], N : S).
types(V, Gamma, M : S)                :- not(member(S, V)),
                                         member(X : T, Gamma),
                                         deconstruct_type(T, ArgTypes, S),
                                         types([S | V], Gamma, Ms, ArgTypes),
                                         construct_app(X, Ms, M).
types(M : T) :- types([], [], M : T).
