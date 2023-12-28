%%%% Magliani Andrea 894395
%%%% Picco Nicolas 894588
%%%% Perego Luca 894448


def_class(Classname, Parents) :-
    def_class(Classname, Parents, []).
def_class(Classname, Parents, Parts) :-
    catch(classname_check(Classname),
          class_already_defined, fail),
    parents_check(Parents),
    parts_check(Parts),
    concat_parts(Parts, Parents, [], Result),
    assertz(class(Classname, Parents, Result)).

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
    is_field_param(H),
    !,
    parts_check(T).
parts_check([H | T]) :-
    is_method(H),
    !,
    parts_check(T).

is_field_param(Field) :-
    Field = field(FieldName, _Value, Type),
    atom(FieldName),
    atom(Type).
is_field_param(Field) :-
    Field = field(FieldName, _Value),
    atom(FieldName).

is_method(Method) :-
    Method = method(MethodName, Args, Form),
    atom(MethodName),
    is_list(Args),
    callable(Form).

concat_parts(Parts, Parents, _, Result) :-
    append(Parts, [], Result),
    check_parents_parts(Parents, Result, Result).

check_parents_parts([], _, _Result) :- !.
check_parents_parts([H | T], NewParts, Result) :-
    findall(Parts, class(H, _, Parts), Parts),
    flatten(Parts, FlatParts),
    append_parts(NewParts, FlatParts, Result),
    write("check_parents_parts: "),
    writeln(Result),
    check_parents_parts(T, Result, Result).



append_parts(NewParts, [], Result) :-
    Result is NewParts,
    write("append_parts: "),
    writeln(Result),
    !.
append_parts(NewParts, [field(Key, _Value)| Tail], Result) :-
    memberchk(field(Key, _), NewParts),
    append_parts(NewParts, Tail, Result).
append_parts(NewParts, [method(Key, _, _) | Tail], Result) :-
    memberchk(method(Key, _, _), NewParts),
    append_parts(NewParts, Tail, Result).
append_parts(NewParts, [field(Key, Value) | Tail], Result) :-
    \+ memberchk(field(Key, _), NewParts),
    \+ memberchk(method(Key, _, _), NewParts),
    append(NewParts, [field(Key, Value)], UpdatedNewParts),
    append(Result, [field(Key, Value)], UpdatedNewResult),
    append_parts(UpdatedNewParts, Tail, UpdatedNewResult).
append_parts(NewParts, [method(Key, Args, Form) | Tail], Result) :-
    \+ memberchk(field(Key, _), NewParts),
    \+ memberchk(method(Key, _, _), NewParts),
    append(NewParts, [method(Key, Args, Form)], UpdatedNewParts),
    append_parts(UpdatedNewParts, Tail, Result).



make(InstanceName, ClassName) :-
    make(InstanceName, ClassName, []).
make(InstanceName, ClassName, ParameterList) :-
    catch(is_valid_instancename(InstanceName),
          instance_found, fail),
    clause(class(ClassName, _, _), _),
    is_list(ParameterList),
    is_parameter(ParameterList),
    assertz(instanceof(InstanceName, ClassName, ParameterList)).
make(InstanceName, ClassName, ParameterList) :-
    clause(class(ClassName, _, _), _),
    is_list(ParameterList),
    is_parameter(ParameterList),
    catch(is_var(InstanceName), var, write("variabile non istanziata")).

is_var(X) :-
    var(X),
    throw(var).
is_var(X) :-
    var(X).

is_valid_instancename(InstanceName) :-
    atom(InstanceName),
    clause(instanceof(InstanceName, _, _), _),
    write('The instance already exists!'),
    throw(instance_found).
is_valid_instancename(InstanceName) :-
    atom(InstanceName).

is_parameter([]) :- !.
is_parameter([H | T]) :-
    is_parameter_check(H),
    is_parameter(T).

is_parameter_check(Field = Value) :-
    atom(Field),
    atomic(Value).


is_class(Classname) :-
    atom(Classname),
    clause(class(Classname, _, _), _).


is_instance(Value) :-
    is_instance(Value, _).
is_instance(Value) :-
    clause(Value, _).
is_instance(Value, Classname) :-
    clause(instanceof(Value, Classname, _), _).


inst(InstanceName, Instance) :-
    bagof(instanceof(InstanceName, Classname, ParameterList),
          instanceof(InstanceName, Classname, ParameterList),
          Instance),
    !.
inst(_InstanceName, _Instance) :-
    write("Instance not found!"),
    fail.


field(Instance, FieldName, Result) :-
    atom(Instance),
    field(inst(Instance), FieldName, Result).
field(_Instance, FieldName, _Result) :-
    atom(FieldName).


