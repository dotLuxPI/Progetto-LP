%%%% Magliani Andrea 894395
%%%% Picco Nicolas 894588
%%%% Perego Luca 894448



%%%% DYNAMIC PREDICATE DEFINITION
:- dynamic class/3.
:- dynamic instance/3.



%%%% DEF_CLASS PREDICATE
def_class(Classname, Parents) :-
    def_class(Classname, Parents, []).
def_class(Classname, Parents, Parts) :-
    catch(classname_check(Classname),
          class_already_defined, fail),
    parents_check(Parents),
    parts_check(Parts, Classname),
    concat_parts(Parts, Parents, Result),
    assertz(class(Classname, Parents, Result)).

%%%% DEF_CLASS UTILS
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

parts_check(Parts, Classname) :-
    parts_check(Parts, Classname, [], []).
parts_check([], _, _, _) :- !.
parts_check([H | _T], _Classname, SeenFields, _SeenMethod) :-
    H = field(Key, _Value, _),
    memberchk(Key, SeenFields),
    write('Cannot define two field '),
    writeln('with the same name in the same class!'),
    !,
    fail.
parts_check([H | _T], _Classname, _SeenFields, SeenMethod) :-
    is_method(H, Key),
    memberchk(Key, SeenMethod),
    write('Cannot define two method '),
    writeln('with the same name in the same class!'),
    !,
    fail.
parts_check([H | T], Classname, SeenFields, SeenMethod) :-
    is_field_param(H, Key),
    !,
    parts_check(T, Classname, [Key | SeenFields], SeenMethod).
parts_check([H | T], Classname, SeenFields, SeenMethod) :-
    is_method(H, Key),
    create_method_predicate(H, Classname),
    !,
    parts_check(T, Classname, SeenFields, [Key | SeenMethod]).

create_method_predicate(method(MethodName, Args, Form),
                        Classname) :-
    Pred =.. [MethodName, Instance | Args],
    Clause =.. [':-', Pred,
                (pred_check(Instance, MethodName, Classname),
                 replace_in_predicate(Instance, Form, NewForm),
                 !,
                 NewForm)],
    assertz(Clause).

pred_check(Instance, MethodName, Classname) :-
    atom(Instance),
    is_instance(Instance),
    inst(Instance, instance(_InstanceName, Class, _ParameterList)),
    bagof(class(Class, Parents, Fields),
          class(Class, Parents, Fields),
          [class(C, _P, F) | _]),
    C = Classname,
    memberchk(method(MethodName, _, _), F).
pred_check(instance(InstanceName, Classname, ParameterList),
           MethodName) :-
    is_instance(instance(InstanceName, Classname, ParameterList)),
    bagof(class(Classname, Parents, Fields),
          class(Classname, Parents, Fields),
          [class(_C, _P, F) | _]),
    memberchk(method(MethodName, _, _), F).

replace_in_predicate(Instance,
                     (Predicate, Rest),
                     (NewPredicate, NewRest)) :-
    Predicate =.. [Functor | Args],
    replace_this_in_args(Instance, Args, NewArgs),
    NewPredicate =.. [Functor | NewArgs],
    !,
    replace_in_predicate(Instance, Rest, NewRest).
replace_in_predicate(Instance, Predicate, NewPredicate) :-
    Predicate =.. [Functor | Args],
    replace_this_in_args(Instance, Args, NewArgs),
    NewPredicate =.. [Functor | NewArgs],
    !.

replace_this_in_args(_, [], []).
replace_this_in_args(Instance, [H | T], [NewH | NewT]) :-
    compound(H),
    replace_in_predicate(Instance, H, NewH),
    replace_this_in_args(Instance, T, NewT).
replace_this_in_args(Instance, [H | T], [H | NewT]) :-
    var(H),
    replace_this_in_args(Instance, T, NewT).
replace_this_in_args(Instance, ['this' | T], [Instance | NewT]) :-
    replace_this_in_args(Instance, T, NewT).
replace_this_in_args(Instance, [H | T], [H | NewT]) :-
    replace_this_in_args(Instance, T, NewT).


is_field_param(Field, Key) :-
    Field = field(FieldName, Value, Type),
    atom(FieldName),
    atom(Type),
    check_type(Value, Type),
    Key = FieldName.
is_field_param(Field, Key) :-
    Field = field(FieldName, _Value),
    atom(FieldName),
    Key = FieldName.

check_type(Value, Type) :-
    atom(Value),
    is_instance(Value, Type),
    !.
check_type(instance(InstanceName, _Classname, _ParameterList),
           Type) :-
    is_instance(InstanceName, Type),
    !.
check_type(Value, Type) :-
    is_of_type(Type, Value),
    !.
check_type(_Value, _Type) :-
    writeln('Invalid Type or Type and Value not matching'),
    !,
    fail.

is_method(Method, Key) :-
    Method = method(MethodName, Args, Form),
    atom(MethodName),
    is_list(Args),
    callable(Form),
    Key = MethodName.

concat_parts(Parts, Parents, Result) :-
    append(Parts, [], TempResult),
    check_parents_parts(Parents, TempResult, Result).

check_parents_parts([], NewParts, NewParts) :- !.
check_parents_parts([H | T], NewParts, Result) :-
    findall(Parts, class(H, _, Parts), PartsList),
    flatten(PartsList, FlatParts),
    append_parts(NewParts, FlatParts, UpdatedNewParts),
    check_parents_parts(T, UpdatedNewParts, Result).

append_parts(NewParts, [], NewParts) :-
    !.
append_parts(NewParts,
             [field(Key, _Value) | Tail],
             Result) :-
    memberchk(field(Key, _), NewParts),
    append_parts(NewParts, Tail, Result),
    !.
append_parts(NewParts,
             [field(Key, _Value) | Tail],
             Result) :-
    memberchk(field(Key, _, _), NewParts),
    append_parts(NewParts, Tail, Result),
    !.
append_parts(NewParts,
             [field(Key, _Value, _Type) | Tail],
             Result) :-
    memberchk(field(Key, _, _), NewParts),
    append_parts(NewParts, Tail, Result),
    !.
append_parts(NewParts,
             [field(Key, _Value, _Type) | Tail],
             Result) :-
    memberchk(field(Key, _), NewParts),
    append_parts(NewParts, Tail, Result),
    !.
append_parts(NewParts,
             [method(Key, _, _) | Tail],
             Result) :-
    memberchk(method(Key, _, _), NewParts),
    append_parts(NewParts, Tail, Result),
    !.
append_parts(NewParts,
             [field(Key, Value) | Tail],
             Result) :-
    append(NewParts, [field(Key, Value)],
           UpdatedNewParts),
    append_parts(UpdatedNewParts, Tail, Result),
    !.
append_parts(NewParts,
             [field(Key, Value, Type) | Tail],
             Result) :-
    append(NewParts, [field(Key, Value, Type)],
           UpdatedNewParts),
    append_parts(UpdatedNewParts, Tail, Result),
    !.
append_parts(NewParts,
             [method(Key, Args, Form) | Tail],
             Result) :-
    append(NewParts, [method(Key, Args, Form)],
           UpdatedNewParts),
    append_parts(UpdatedNewParts, Tail, Result),
    !.



%%%% MAKE PREDICATE
make(InstanceName, Classname) :-
    make(InstanceName, Classname, []).
make(InstanceName, Classname, ParameterList) :-
    atom(Classname),
    clause(class(Classname, _, _), _),
    is_list(ParameterList),
    catch(is_valid_instancename(InstanceName),
          instance_found, fail),
    is_parameter(ParameterList),
    is_valid_parameter_list(ParameterList, Classname),
    concat_parameter(ParameterList, Classname, FinalParameter),
    assertz(instance(InstanceName, Classname, FinalParameter)),
    !.
make(InstanceName, Classname, ParameterList) :-
    var(InstanceName),
    atom(Classname),
    clause(class(Classname, _, _), _),
    is_list(ParameterList),
    is_parameter(ParameterList),
    is_valid_parameter_list(ParameterList, Classname),
    concat_parameter(ParameterList, Classname, FinalParameter),
    InstanceName = instance('anonymous', Classname, FinalParameter),
    !.
make(InstanceName, Classname, ParameterList) :-
    bagof(instance(InstanceName, Classname, ParameterList),
          instance(InstanceName, Classname, ParameterList),
          Result),
    member(instance(InstanceName, Classname, ParameterList), Result).

%%%% MAKE UTILS
is_valid_instancename(InstanceName) :-
    atom(InstanceName),
    clause(instance(InstanceName, _, _), _),
    write('The instance already exists!'),
    throw(instance_found).
is_valid_instancename(InstanceName) :-
    atom(InstanceName).

is_parameter([]) :- !.
is_parameter([H | T]) :-
    is_parameter_check(H),
    is_parameter(T).

is_parameter_check(Field = _Value) :-
    atom(Field).

is_valid_parameter_list([], _) :-
    !.
is_valid_parameter_list([Field = Value | T], Classname) :-
    bagof(Parts, class(Classname, _, Parts), Parts),
    flatten(Parts, FlatParts),
    member(field(Field, _, Type), FlatParts),
    check_type(Value, Type),
    !,
    is_valid_parameter_list(T, Classname).
is_valid_parameter_list([Field = _Value | T], Classname) :-
    bagof(Parts, class(Classname, _, Parts), Parts),
    flatten(Parts, FlatParts),
    member(field(Field, _), FlatParts),
    !,
    is_valid_parameter_list(T, Classname).


concat_parameter(ParameterList, Classname, FinalParameter) :-
    findall(class(Classname, Parents, Parts),
            class(Classname, Parents, Parts),
            [class(Classname, Parents, Parts) | _T]),
    append_parameter(ParameterList, Parts, FinalParameter).

append_parameter(ParameterList, [], ParameterList) :-
    !.
append_parameter(ParameterList,
                 [field(Key, _Value) | T],
                 FinalParameter) :-
    memberchk(Key = _, ParameterList),
    append_parameter(ParameterList, T, FinalParameter),
    !.
append_parameter(ParameterList,
                 [field(Key, _Value, _Type) | T],
                 FinalParameter) :-
    memberchk(Key = _, ParameterList),
    append_parameter(ParameterList, T, FinalParameter),
    !.
append_parameter(ParameterList,
                 [method(_Key, _Args, _Form) | T],
                 FinalParameter) :-
    append_parameter(ParameterList, T, FinalParameter),
    !.
append_parameter(ParameterList,
                 [field(Key, Value) | T],
                 FinalParameter) :-
    append(ParameterList,
           [Key = Value],
           IntermediateParameter),
    append_parameter(IntermediateParameter,
                     T,
                     FinalParameter),
    !.
append_parameter(ParameterList,
                 [field(Key, Value, _Type) | T],
                 FinalParameter) :-
    append(ParameterList,
           [Key = Value],
           IntermediateParameter),
    append_parameter(IntermediateParameter,
                     T,
                     FinalParameter),
    !.



%%%% IS_CLASS PREDICATE
is_class(Classname) :-
    atom(Classname),
    clause(class(Classname, _, _), _).



%%%% IS_INSTANCE PREDICATE
is_instance(Value) :-
    atom(Value),
    is_instance(Value, _).
is_instance(Value) :-
    Value = instance(_, _, _),
    clause(Value, _),
    !.
is_instance(Value, Classname) :-
    clause(instance(Value, Classname, _), _),
    !.
is_instance(Value, Classname) :-
    findall(Class,
            instance(Value, Class, _),
            [H | _]),
    has_parents(H, Classname),
    !.

%%%% IS_INSTANCE UTILS
has_parents(Class, SuperClass) :-
    findall(Parents, class(Class, Parents, _), P),
    flatten(P, FlatParents),
    get_parents(FlatParents, AllParents),
    memberchk(SuperClass, AllParents).

get_parents([], []).
get_parents([H | T], AllParents) :-
    findall(Parents, class(H, Parents, _), P),
    flatten(P, FlatP),
    append([H], FlatP, TempParents),
    get_parents(T, RestParents),
    append(TempParents, RestParents, AllParents).



%%%% INST PREDICATE
inst(InstanceName, H) :-
    bagof(instance(InstanceName, Classname, ParameterList),
          instance(InstanceName, Classname, ParameterList),
          [H | _T]),
    !.
inst(_InstanceName, _Instance) :-
    write("Instance not found!"),
    fail.



%%%% FIELD PREDICATE
field(Instance, FieldName, Result) :-
    atom(Instance),
    inst(Instance, Inst),
    field(Inst, FieldName, Result).
field(instance(_InstanceName, _Classname, ParameterList),
      FieldName,
      Result) :-
    atom(FieldName),
    memberchk(FieldName = Value, ParameterList),
    Result = Value.



%%%% FIELDX PREDICATE
fieldx(Instance, FieldNames, Result) :-
    atom(Instance),
    is_list(FieldNames),
    inst(Instance, Inst),
    fieldx(Inst, FieldNames, Result),
    !.
fieldx(_Instance, [], _Result) :-
    write('The fieldnames list is empty!'),
    !.
fieldx(Instance, [H], Result) :-
    field(Instance, H, Result),
    !.
fieldx(Instance, [H | T], Result) :-
    field(Instance, H, Res),
    is_instance(Res),
    fieldx(Res, T, Result),
    !.
fieldx(Instance, [H | T], Result) :-
    field(Instance, H, _),
    fieldx(Instance, T, Result),
    !.


