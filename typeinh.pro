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

:- op(150, xfx, :).

% abs(x,x) : alpha --> alpha

:- set_prolog_flag(occurs_check, true).

% types(+Gamma, -Ms, +ArgTypes)
types(_, [], []).
types(Gamma, [M | Ms], [T | Ts]) :- types(Gamma, M : T),
                                    types(Gamma, Ms, Ts).

% deconstruct_type(+T, ?ArgTypes, ?ResultType).
deconstruct_type(Alpha, [], Alpha). % :- atom(Alpha).
deconstruct_type(R --> S, [R | ArgTypes], ResultType) :- deconstruct_type(S, ArgTypes, ResultType).

% construct_app(?Fun, +Args, ?Term).
construct_app(Fun, [], Fun).
construct_app(Fun, [Arg | Args], Term) :- construct_app(app(Fun, Arg), Args, Term).

% types(Gamma, M : T).

% types(+Gamma, +M : ?T).

types(Gamma, abs(X, N) : R --> S)  :- types([X : R | Gamma], N : S).
types(Gamma, M : S)                :- member(X : T, Gamma),
                                      deconstruct_type(T, ArgTypes, S),
                                      types(Gamma, Ms, ArgTypes),
                                      construct_app(X, Ms, M).
types(M : T) :- types([], M : T).
