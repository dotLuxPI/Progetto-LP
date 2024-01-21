Magliani Andrea	894395
Perego Luca	894448
Picco Nicolas	894588

OOP - PRIMITIVE

1. def_class(Classname, Parents, Parts)

	def_class si occupa della creazione della classe, prende in input 
	Classname, Parents, Parts.
	Richiama i predicati classname_check, parents_check, parts_check,
	concat_parts e rewrite_instance.
	Se tutti i check restituiscono true, la classe viene asserita.



2. make(InstanceName, Classname, ParameterList)

	make ha tre diverse funzionalità:
	1. Controlla la correttezza di InstanceName, Classname e 
	ParameterList, se tutti i check restituiscono true crea un'istanza 
	o ne modifica una esistente.
	2. Controlla che InstanceName, Classname e ParameterList siano 
	definiti correttamente, se tutti i check restituiscono true viene 
	creata una variabile anonima con le caratteristiche dell'istanza.
	3. Se viene chiamato il make con qualche parametro come variabile, 
	unifica e restituisce i possibili valori dei paramatri omessi.


 
3. is_class(Classname)

	is_class restituisce true nel caso Classname sia il nome di una 
	classe esistente.



4. is_instance(Value, Classname)

	is_instance restituisce true nel caso Value sia un istanza valida
	per la classe Classname o per una sua superclasse.



5. inst(InstanceName, Var)

	inst prende in input InstanceName e restituisce in Var l'istanza
	corrispondente.



6. field(Instance, FieldName, Result)

	field dati in input un'istanza e un FieldName validi, restituisce
	in Result il valore corrispondente al Fieldname in Instance.



7. fieldx(Instance, FieldNames, Result)

	fieldx chiama ricorsivamente field sui vari FieldNames, 
	percorrendo anche le istanze nel caso siano presenti, restituisce
	in Result il valore dell'ultimo fieldName della lista.



OOP - FUNZIONI HELPER

get_type: Dato in input un Value restituisce il suo tipo.

is_subtype: Restituisce true se il primo parametro è un sottotipo del 
	secondo.

rewrite_instance: Scorre i field e riscrive le istanze complete
	(instance(InstanceName, Classname, ParameterList)) mettendo solo
	l'InstanceName che viene usato internamente come reference.

classname_check: Controlla che Classname sia valido e che non esista una
	classe con lo stesso nome, nel caso di classe già esistente lancia
	errore.

parents_check: Controlla che i parents siano tutte classi valide ed 
	esistenti.

parts_check: Controlla che le parts della classe abbiano una struttura
	corretta e non ci siano campi duplicati.

create_method_predicate: Costruisce dinamicamente un metodo e
	lo asserisce come predicate.

pred_check: Definisce i controlli standard che deve avere qualsiasi 
	metodo definito dinamicamente a runtime.

replace_in_predicate: Destruttura i predicati per sostituire 
	nei metodi il this con l'istanza, per farlo 
	usa replace_this_in_args.

replace_this_in_args: Sostituisce il this con l'istanza.

is_field_param: Controlla che il field sia costruito correttamente.

check_type: Richiama get_type e is_subtype per controllare 
	che i tipi siano corretti.

is_method: Controlla che il metodo sia strutturato correttamente.

concat_parts: Si occupa di ereditare e concatenare le parts dai parents.

check_parents_type: Controlla che i parts rispettino i tipi definiti dai
	parents.

check_all_parts: Viene richiamato in concat_parts, ed è utilizzato per
	controllare quali parts ereditare o modificare.

check_parents_parts: Gestisce l'ereditarietà delle parts dei parents.

append_parts: Viene richiamato in concat_parts e si occupa di concatenare
	effettivamente le parts dopo aver effettuato i vari check.

rewrite_par_instance: Scorre l'istanza e riesce le istanze presenti come 
	field con solo l'InstanceName, per usarlo come reference.

is_parameter: Controlla che la lista ParameterList sia strutturato 
	correttamente.

is_valid_parameter_list: Controlla che gli elementi definiti in 
	ParameterList siano definiti nella classe dell'istanza, o in una 
	superclasse e che rispettino il typing definito.

concat_parameter: Gestisce l'ereditarietà dei parametri da classe a 
	istanza.

append_parameter: Viene richiamato in concat_parameter e si occupa di 
	concatenare i parametri ereditati, dopo aver effettuato tutti
	i check.

has_parents: Viene utilizzato in is_instance e restituisce true se la 
	classe passata come primo parametro è una sottoclasse della 
	classe passata come secondo parametro.

get_parents: Scorre tutti i parents di una determinata classe, 
	arrivando fino al livello più alto e li restituisce in una lista.
