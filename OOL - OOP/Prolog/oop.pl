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

make(InstanceName,ClassName) :-
    make(InstanceName,ClassName,[]).

make(InstanceName,ClassName,ParameterList) :-
    catch(is_valid_instancename(InstanceName),instance_found,fail),
    clause(class(ClassName,_,_),_),
    is_list(ParameterList),
    is_parameter(ParameterList),
    assertz(instanceof(InstanceName,ClassName,ParameterList)).

make(InstanceName,ClassName,ParameterList) :-
    clause(class(ClassName,_,_),_),
    is_list(ParameterList),
    is_parameter(ParameterList),
    (var(InstanceName), InstanceName = instanceof(_,_,_)).

is_valid_instancename(InstanceName) :-
    atom(InstanceName),
    clause(instanceof(InstanceName,_,_),_),
    write('the instance already exists!'),
    throw('instance_found').
is_valid_instancename(InstanceName) :-
    atom(InstanceName).


is_parameter([]) :- !.
is_parameter([H | T]) :-
    is_parameter_check(H),
    is_parameter(T).

is_parameter_check(Field = Value) :-
    atom(Field),
    atomic(Value).

