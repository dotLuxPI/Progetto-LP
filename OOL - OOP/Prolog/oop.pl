%%%% Magliani Andrea 894395
%%%% Nicolas Picco 894588
%%%% Luca Perego 894448

def_class(Classname, Parents) :-
    def_class(Classname, Parents, []).

def_class(Classname, Parents, Parts) :-
    catch(classname_check(Classname), class_already_defined, fail),
    parents_check(Parents),
    parts_check(Parts),
    assertz(class(Classname, Parents, Parts)).

classname_check(Classname) :-
    atom(Classname),
    clause(class(Classname, _, _), _),
    write('The class already exists!'),
    throw(class_already_defined).
classname_check(Classname) :-
    atom(Classname).

parents_check([]) :- !.
parents_check([H | T]) :-
    atom(H),
    clause(class(H, _, _), _),
    !,
    parents_check(T).

parts_check([]) :- !.
parts_check([H | T]) :-
    is_field(H),
    !,
    parts_check(T).
parts_check([H | T]) :-
    is_method(H),
    !,
    parts_check(T).

field(FieldName, Value) :-
    field(FieldName, Value, '').
field(FieldName, _, Type) :-
    atom(FieldName),
    atom(Type).

is_field(Field) :-
    Field = field(_, _, _).
is_field(Field) :-
    Field = field(_, _).

method(MethodName, Args, Form) :-
    atom(MethodName),
    is_list(Args),
    callable(Form).

is_method(Method) :-
    Method = method(_, _, _).
