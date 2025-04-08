% 1. x,y,z
% 2. app(M, N)
% 3. abs(x, M)

i(abs(x,x)).
k(abs(x,abs(y,x))).
kstar(abs(x,abs(y,y))).
s(abs(x,abs(y,abs(z,app(app(x,z),app(y,z)))))).

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
ts((alpha --> beta --> gamma) --> (alpha --> beta) --> alpha --> gamma).

:- op(150, xfx, :).

% abs(x,x) : alpha --> alpha

% types(Gamma, M : T).

types(Gamma, X : T)                :- member(X : T, Gamma).
types(Gamma, app(M1, M2) : S)      :- types(Gamma, M1 : R --> S),
                                      types(Gamma, M2 : R).
types(Gamma, abs(X, N) : R --> S)  :- types([X : R | Gamma], N : S).
types(M : T) :- types([], M : T).
