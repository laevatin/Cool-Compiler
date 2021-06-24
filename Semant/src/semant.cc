

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

/////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
/////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

static void if_err_exit(ClassTableP classtable)
{
    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) , classes(classes) {
    install_basic_classes();
    build_inheritance_graph();
    check_base_class();
    if_err_exit(this);
    analyse_inheritance_graph();
    if_err_exit(this);
    setup_featuretable();
    check_feature_inheritance();
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);
    
    add_class_namespace(Object_class);
    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  
    add_class_namespace(IO_class);
    add_inheritance_relation(IO, Object);
    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);
    add_class_namespace(Int_class);
    add_inheritance_relation(Int, Object);
    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);
    add_class_namespace(Bool_class);
    add_inheritance_relation(Bool, Object);
    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);
    add_class_namespace(Str_class);
    add_inheritance_relation(Str, Object);
    // Add the basic classes into the program
    // append_Classes(
    //     classes, 
    //     append_Classes(
    //         single_Classes(Object_class), 
    //         append_Classes(
    //             single_Classes(IO_class),
    //             append_Classes(
    //                 single_Classes(Int_class),
    //                 append_Classes(
    //                     single_Classes(Bool_class),
    //                     single_Classes(Str_class))
    //                 ))));
}

ClassTable::~ClassTable()
{
    // delete methodtable;
    // delete attrtable;
}

void ClassTable::add_class_namespace(Class_ cls)
{
    class_namespace[cls->get_name()] = cls;
}

void ClassTable::add_inheritance_relation(Symbol cur, Symbol par)
{
    // add to the adjacent list
    if (children_list.find(par) == children_list.end())
    {
        children_list[par] = SymbolList();
    }
    children_list[par].push_back(cur);
}

void ClassTable::build_inheritance_graph()
{
    for (int i = classes->first(); classes->more(i); i = classes->next(i))
    {
        Class_ class_ = classes->nth(i);
        Symbol cur_sym = class_->get_name();
        Symbol par_sym = class_->get_parent();
        // Check redefinition of basic class
        if (cur_sym == SELF_TYPE || cur_sym == Object || cur_sym == IO 
            || cur_sym == Int || cur_sym == Str)
        {
            semant_error(class_) << "Redefinition of basic class " << cur_sym->get_string() << ".\n";
            continue;
        }
        // Check whether parent class can be derived
        if (par_sym == Int || par_sym == Bool || par_sym == Str || par_sym == SELF_TYPE)
        {
            semant_error(class_) << "Class " << cur_sym->get_string() << " cannot inherit class " 
                                 << par_sym->get_string() << ".\n"; 
        }
        // Check repeated class definition
        if (class_namespace.find(cur_sym) != class_namespace.end())
        {
            semant_error(class_) << "Class " << cur_sym->get_string() << " was previously defined.\n"; 
            continue;
        }

        add_class_namespace(class_);
        // Build the adjacent list representation of inheritance graph
        add_inheritance_relation(cur_sym, par_sym);
    }
}

void ClassTable::check_base_class()
{
    for (auto p : children_list)
    {
        if (class_namespace.find(p.first) == class_namespace.end() && p.first != SELF_TYPE)
        {
            // If we cannot find it in our class namespace, report the error for all the child
            for (auto child : p.second)
            {
                semant_error(class_namespace[child]) << "Class " << child->get_string()
                    << " inherits from an undefined class " << p.first->get_string() << ".\n";
            }
        }
    }
}

void ClassTable::analyse_inheritance_graph() 
{
    // Use bfs to traverse through every node starting from Object
    // From previous checking, we can infer that the connected graph
    // rooted in Object is a tree, so we need to check if there are
    // some unconnected cycles.  
    SymbolQueue symqueue;
    SymbolSet visitset;
    visitset.insert(Object);
    symqueue.push(Object);

    // BFS
    while (!symqueue.empty()) 
    {
        Symbol node = symqueue.front();
        symqueue.pop();
        auto children = children_list.find(node);
        
        if (children == children_list.end()) 
            continue;
        
        for (auto n : children->second)
        {
            if (visitset.find(n) == visitset.end())
            {
                visitset.insert(n);
                symqueue.push(n); 
            }
        }
    }

    for (auto n : class_namespace)
    {
        if (!visitset.count(n.first))
        {
            semant_error(class_namespace[n.first]) << "Class " << n.first->get_string() 
                << ", or an ancestor of " << n.first->get_string()
                << ", is involved in an inheritance cycle.\n";
        }
    }
}

void ClassTable::setup_featuretable() 
{
    methodtable = new MethodTable(this);
    attrtable = new AttrTable(this);

    for (auto c : class_namespace) 
    {
        Class_ class_ = c.second;
        Features features = class_->get_features();
        for (int i = features->first(); features->more(i); i = features->next(i)) 
        {
            Feature feature = features->nth(i);
            if (feature->is_method())
            {
                methodtable->set_feature(class_, feature);
                // std::cout << class_->get_name() << " " << feature->get_name() << "added method" << std::endl;
            }
            else
            {
                if (feature->get_name() == self)
                {
                    semant_error(class_->get_filename(), feature) 
                        << "'self' cannot be the name of an attribute.\n";
                    continue;
                }
                attrtable->set_feature(class_, feature);
                // std::cout << class_->get_name() << " " << feature->get_name() << "added attr" << std::endl;
            }
        }
    }
}

void ClassTable::check_feature_inheritance()
{
    SymbolQueue symqueue;
    symqueue.push(Object);

    // BFS
    while (!symqueue.empty()) 
    {
        Symbol node = symqueue.front();
        symqueue.pop();
        auto children = children_list.find(node);
        if (children == children_list.end()) 
            continue;
        
        for (auto cls_sym : children->second)
        {
            Class_ child = class_namespace[cls_sym];
            Features fs = child->get_features();
            symqueue.push(cls_sym);
            for (int i = fs->first(); fs->more(i); i = fs->next(i))
            {
                Feature child_fea = fs->nth(i);
                if (child_fea->is_method())
                {
                    // Check redefined method
                    Feature pre_defined_meth = methodtable->get_feature(
                        child->get_parent(), child_fea->get_name());
                    if (pre_defined_meth) 
                    {
                        // std::cout << child->get_name() << " " << pre_defined_meth->get_name() << std::endl;
                        if (pre_defined_meth->check_redefinition(child_fea, child, this))
                            methodtable->remove_feature(child, child_fea->get_name());
                    }
                }
                else
                {
                    // Check redefined attribute
                    // std::cout << child->get_name() << " "  << std::endl;
                    Feature pre_defined_attr = attrtable->get_feature(
                        child->get_parent(), child_fea->get_name());
                    if (pre_defined_attr)
                    {
                        pre_defined_attr->check_redefinition(child_fea, child, this);
                        attrtable->remove_feature(child, child_fea->get_name());
                    }
                }
            }
        }
    }
}

bool method_class::check_redefinition(Feature other, Class_ othercls, ClassTableP err_recorder) 
{
    if (!other->is_method())
        return false;

    method_class *o = dynamic_cast<method_class *>(other);

    if (return_type != o->return_type) 
    {
        err_recorder->semant_error(othercls->get_filename(), other) << "In redefined method " 
            << other->get_name()->get_string() << ", return type " << o->return_type->get_string() 
            << " is different from original return type " << return_type->get_string() << ".\n";
        return true;
    }

    if (formals->len() != o->formals->len())
    {
        err_recorder->semant_error(othercls->get_filename(), other) 
            << "Incompatible number of formal parameters in redefined method "
            << other->get_name() << ".\n";
        return true;
    }

    for (int i = formals->first(); formals->more(i); i = formals->next(i))
    {
        Formal f1 = formals->nth(i);
        Formal f2 = o->formals->nth(i);
        if (!f1->equal_type(f2))
        {
            err_recorder->semant_error(othercls->get_filename(), other) << "In redefined method "
                << other->get_name()->get_string() << ", parameter type " << f2->get_type_decl()->get_string()
                << " is different from original type " << f1->get_type_decl()->get_string() << "\n";
            return true;
        }
    }

    return false;
}

bool attr_class::check_redefinition(Feature other, Class_ othercls, ClassTableP err_recorder)
{
    if (other->is_method())
        return false;
    
    attr_class *o = dynamic_cast<attr_class *>(other);

    if (name == o->name)
    {
        err_recorder->semant_error(othercls->get_filename(), other) << "Attribute " << name->get_string()
            << " is an attribute of an inherited class.\n";
        return true;
    }

    return false;
}

void ClassTable::check_main() 
{
    auto main_cls = class_namespace.find(Main);

    if (main_cls == class_namespace.end()) 
    {
        semant_error() << "Class Main is not defined.\n";
        return;
    }
    auto meth = methodtable->get_feature(Main, main_meth);
    if (!meth)
    {
        semant_error(main_cls->second) << "No 'main' method in class Main.\n";
        return;
    }
    // Check formals
    auto meth_type = dynamic_cast<method_class *>(meth);
    if (meth_type->get_formals()->len() != 0)
    {
        semant_error(main_cls->second) << "'main' method in class Main should have no arguments.\n";
    }
}

void ClassTable::check_type()
{
    TypeTable symtbl;
    symtbl.enterscope();
    for (int i = classes->first(); classes->more(i); i = classes->next(i))
    {
        Class_ cls = classes->nth(i);
        Symbol name = cls->get_name();
        if (name == Object || name == Int || name == Str || name == Bool || name == IO)
            continue;
        Features fts = cls->get_features();
        for (int i = fts->first(); fts->more(i); i = fts->next(i))
        {
            fts->nth(i)->check_type(&symtbl, cls, methodtable, attrtable, this);
        }
    }
    symtbl.exitscope();
}

void method_class::check_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable,
					    AttrTableP attrtable, ClassTableP classtable)
{
    symtbl->enterscope();
    // Check the formal parameters
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) 
    {
        Formal formal = formals->nth(i);
        int err = 0;
        if (formal->get_type_decl() == SELF_TYPE) 
        {
            classtable->semant_error(cls->get_filename(), formal)
                << "Formal parameter " << formal->get_name() 
                << " cannot have type SELF_TYPE.\n";
            symtbl->addid(formal->get_name(), Object);
            continue;
        }
        if (formal->get_name() == self) 
        {
            err = 1;
            classtable->semant_error(cls->get_filename(), formal) 
                << "'self' cannot be the name of a formal parameter.\n";
        }
        if (!classtable->get_class(formal->get_type_decl()))
        {
            err = 1;
            classtable->semant_error(cls->get_filename(), formal) << "Class " 
                << formal->get_type_decl() << " of formal parameter " 
                << formal->get_name() << " is undefined.\n";
        }
        if (symtbl->probe(formal->get_name())) 
        {
            err = 1;
            classtable->semant_error(cls->get_filename(), formal)
                << "Formal parameter " << formal->get_name() 
                << " is multiply defined.\n";
        }

        if (!err) 
            symtbl->addid(formal->get_name(), formal->get_type_decl());
    }
    // Check the return type
    if (!classtable->get_class(return_type) && return_type != SELF_TYPE)
    {
        classtable->semant_error(cls->get_filename(), this)
            << "Undefined return type " << return_type << " in method " << name << ".\n";
    }
    Symbol inferred = expr->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    if (!classtable->is_child_type(inferred, return_type, cls) && inferred != SELF_TYPE)
    {
        classtable->semant_error(cls->get_filename(), this)
            << "Inferred return type " << inferred << " of method " << name
            << " does not conform to declared return type " << return_type << ".\n";
    }

    symtbl->exitscope();
}

void attr_class::check_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable,
					    AttrTableP attrtable, ClassTableP classtable)
{
    if (!classtable->get_class(type_decl) && type_decl != SELF_TYPE)
        classtable->semant_error(cls->get_filename(), this) << "Class " 
            << type_decl << " of attribute " << name << " is undefined.\n";
    else
    {
        Symbol inferred = init->eval_type(symtbl, cls, methodtable, attrtable, classtable);
        if (inferred == No_type)
            return;
        if (inferred == SELF_TYPE)
            inferred = cls->get_name();
        if (!classtable->is_child_type(inferred, type_decl, cls))
            classtable->semant_error(cls->get_filename(), this)
                << "Inferred type " << inferred << " of initialization of attribute " 
                << name << " does not conform to declared type " << type_decl << ".\n";
    }
}

Symbol branch_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    symtbl->enterscope();
    symtbl->addid(name, type_decl);
    Symbol type = expr->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    // std::cout << "branch " << type << std::endl;
    symtbl->exitscope();
    return type;
}

Symbol assign_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{   
    if (name == self) {
        classtable->semant_error(cls->get_filename(), this) << "Cannot assign to 'self'.\n";
        return set_type(expr->eval_type(symtbl, cls, methodtable, attrtable, classtable))->type;
    }
    
    if (!symtbl->lookup(name) && !attrtable->get_feature(cls, name)) {
        classtable->semant_error(cls->get_filename(), this) << "Assignment to undeclared variable " << name << ".\n";
        return set_type(expr->eval_type(symtbl, cls, methodtable, attrtable, classtable))->type;
    }

    set_type(expr->eval_type(symtbl, cls, methodtable, attrtable, classtable));

    Symbol type_decl = symtbl->lookup(name);
    if (!type_decl) 
        type_decl = (dynamic_cast<attr_class *>(attrtable->get_feature(cls, name)))->get_type_decl();

    if (!classtable->is_child_type(type, type_decl, cls)) {
        classtable->semant_error(cls->get_filename(), this)
            << "Type " << type << " of assigned expression does not conform to declared type " << type_decl
            << " of identifier " << name << ".\n";
        return set_type(type_decl)->type;
    }

    return type;
}

Symbol static_dispatch_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    Symbol t0 = expr->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    Symbol t0_origin = t0;
    if (t0 == SELF_TYPE) t0 = cls->get_name();

    Symbol *ti = new Symbol[actual->len()];

    for (int i = actual->first(); actual->more(i); i = actual->next(i))
    {
        ti[i] = actual->nth(i)->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    }

    if (type_name == SELF_TYPE) 
    {
        classtable->semant_error(cls->get_filename(), this) 
            << "Static dispatch to SELF_TYPE.\n";
        delete [] ti;
        return set_type(Object)->type;
    } 

    if (!classtable->get_class(type_name) && type_name != SELF_TYPE)
    {
        classtable->semant_error(cls->get_filename(), this) 
            << "Static dispatch to undefined class " << type_name << ".\n";
        delete [] ti;
        return set_type(Object)->type;
    }

    if (!classtable->get_class(t0))
    {
        delete [] ti;
        return set_type(Object)->type;
    }
        

    if (!classtable->is_child_type(t0, type_name, cls))
    {
        classtable->semant_error(cls->get_filename(), this)
            << "Expression type " << t0_origin << " does not conform to declared static dispatch type " << type_name << ".\n";
        delete [] ti;
        return set_type(Object)->type;
    }

    auto method = dynamic_cast<method_class *>(methodtable->get_feature(type_name, name));
    if (!method)
    {
        classtable->semant_error(cls->get_filename(), this)
            << "Static dispatch to undefined method " << name->get_string() << ".\n";
        delete [] ti;
        return set_type(Object)->type;
    }

    Formals formals = method->get_formals();
    if (actual->len() != formals->len())
    {
        classtable->semant_error(cls->get_filename(), this)
            << "Method " << name->get_string() << " invoked with wrong number of arguments.\n";
        delete [] ti;
        return set_type(Object)->type;
    }

    for (int i = formals->first(); formals->more(i); i = formals->next(i))
    {
        Formal formal = formals->nth(i);
        Symbol tmp = ti[i];
        if (ti[i] == SELF_TYPE)
            tmp = cls->get_name();
        if (!classtable->is_child_type(tmp, formal->get_type_decl(), cls))
            classtable->semant_error(cls->get_filename(), this)
                << "In call of method " << method->get_name() << ", type " << ti[i] 
                << " of parameter " << formal->get_name() << " does not conform to declared type " 
                << formal->get_type_decl() << ".\n";
    }
    delete [] ti;
    Symbol return_type = method->get_return_type();
    if (!classtable->get_class(return_type) && return_type != SELF_TYPE)
        return set_type(Object)->type;

    if (return_type == SELF_TYPE)
    {
        if (t0 != cls->get_name())
            return set_type(t0)->type;
        else 
            return set_type(SELF_TYPE)->type;
    }

    return set_type(return_type)->type;
}

Symbol dispatch_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    Symbol t0 = expr->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    if (t0 == SELF_TYPE) t0 = cls->get_name();    

    Symbol *ti = new Symbol[actual->len()];
    for (int i = actual->first(); actual->more(i); i = actual->next(i))
    {
        ti[i] = actual->nth(i)->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    }

    if (!classtable->get_class(t0))
    {
        classtable->semant_error(cls->get_filename(), this) 
            << "Dispatch on undefined class " << t0 << ".\n";
        delete [] ti;
        return set_type(Object)->type;
    }

    auto method = dynamic_cast<method_class *>(methodtable->get_feature(t0, name));
    if (!method)
    {
        classtable->semant_error(cls->get_filename(), this)
            << "Dispatch to undefined method " << name->get_string() << ".\n";
        delete [] ti;
        return set_type(Object)->type;
    }

    Formals formals = method->get_formals();
    if (actual->len() != formals->len())
    {
        classtable->semant_error(cls->get_filename(), this)
            << "Method " << name->get_string() << " called with wrong number of arguments.\n";
        delete [] ti;
        return set_type(Object)->type;
    }

    for (int i = formals->first(); formals->more(i); i = formals->next(i))
    {
        Formal formal = formals->nth(i);
        Symbol tmp = ti[i];
        if (ti[i] == SELF_TYPE)
            tmp = cls->get_name();
        if (!classtable->is_child_type(tmp, formal->get_type_decl(),cls))
            classtable->semant_error(cls->get_filename(), this)
                << "In call of method " << method->get_name()->get_string() << ", type "
                << ti[i]->get_string() << " of parameter " << formal->get_name()
                << " does not conform to declared type " << formal->get_type_decl() << ".\n";
    }
    delete [] ti;

    Symbol return_type = method->get_return_type();
    if (!classtable->get_class(return_type) && return_type != SELF_TYPE)
        return set_type(Object)->type;

    if (return_type == SELF_TYPE)
    {
        if (t0 != cls->get_name())
            return set_type(t0)->type;
        else 
            return set_type(SELF_TYPE)->type;
    }
        
    return set_type(return_type)->type;
}

Symbol cond_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    if (pred->eval_type(symtbl, cls, methodtable, attrtable, classtable) != Bool)
    {
        classtable->semant_error(cls->get_filename(), this)
            << "Predicate of 'if' does not have type Bool.\n";
    }
    Symbol e2 = then_exp->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    Symbol e3 = else_exp->eval_type(symtbl, cls, methodtable, attrtable, classtable);

    return set_type(classtable->lub_type(e2, e3, cls))->type;
}

Symbol loop_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    if (pred->eval_type(symtbl, cls, methodtable, attrtable, classtable) != Bool)
    {
        classtable->semant_error(cls->get_filename(), this)
            << "Loop condition does not have type Bool.\n";
    }
    body->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    return set_type(Object)->type;
}

Symbol typcase_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    SymbolSet types_decled;
    Symbol min_type;

    expr->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    for (int i = cases->first(); cases->more(i); i = cases->next(i))
    {
        branch_class *branch = dynamic_cast<branch_class *>(cases->nth(i));
        Symbol name_decl = branch->get_name();
        Symbol type_decl = branch->get_type_decl();

        if (types_decled.count(type_decl)) 
        {
            classtable->semant_error(cls->get_filename(), branch)
                << "Duplicate branch " << branch->get_type_decl() << " in case statement.\n";
        }
        if (!classtable->get_class(type_decl) && type_decl != SELF_TYPE)
        {
            classtable->semant_error(cls->get_filename(), this)
                << "Class " << type_decl->get_string() << " of case branch is undefined.\n";
        }
        if (name_decl == self)
        {
            classtable->semant_error(cls->get_filename(), this) 
                << "'self' bound in 'case'.\n";
        }
        if (type_decl == SELF_TYPE)
        {
            classtable->semant_error(cls->get_filename(), this)
                << "Identifier " << name_decl->get_string() << " declared with type SELF_TYPE in case branch.\n";
        }
        types_decled.insert(branch->get_type_decl());
        if (i == 0)
            min_type = branch->eval_type(symtbl, cls, methodtable, attrtable, classtable);
        else
            min_type = classtable->lub_type(min_type, 
                branch->eval_type(symtbl, cls, methodtable, attrtable, classtable), cls);
    }

    return set_type(min_type)->type;
}

Symbol block_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    for (int i = body->first(); body->more(i); i = body->next(i))
    {
        type = body->nth(i)->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    }
    return type;
}

Symbol let_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    symtbl->enterscope();
    if (identifier == self)
        classtable->semant_error(cls->get_filename(), this) 
            << "'self' cannot be bound in a 'let' expression.\n";
    else
        symtbl->addid(identifier, type_decl);

    Symbol init_type = init->eval_type(symtbl, cls, methodtable, attrtable, classtable);

    if (!classtable->get_class(type_decl) && type_decl != SELF_TYPE)
        classtable->semant_error(cls->get_filename(), this)
            << "Class " << type_decl << " of let-bound identifier " << identifier << " is undefined.\n";
    else if (init_type != No_type && !classtable->is_child_type(init_type, type_decl, cls))
        classtable->semant_error(cls->get_filename(), this)
            << "Inferred type " << init_type << " of initialization of " << identifier
            << " does not conform to identifier's declared type " << type_decl << ".\n";
    
    set_type(body->eval_type(symtbl, cls, methodtable, attrtable, classtable));
    symtbl->exitscope();
    return type;
}

Symbol plus_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    Symbol e1_type = e1->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    Symbol e2_type = e2->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    
    if (e1_type != Int || e2_type != Int)
        classtable->semant_error(cls->get_filename(), this)
            << "non-Int arguments: " << e1_type << " + " << e2_type << "\n";
    return set_type(Int)->type;
}

Symbol sub_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    Symbol e1_type = e1->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    Symbol e2_type = e2->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    
    if (e1_type != Int || e2_type != Int)
        classtable->semant_error(cls->get_filename(), this)
            << "non-Int arguments: " << e1_type << " - " << e2_type << "\n";
    return set_type(Int)->type;
}

Symbol mul_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    Symbol e1_type = e1->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    Symbol e2_type = e2->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    
    if (e1_type != Int || e2_type != Int)
        classtable->semant_error(cls->get_filename(), this)
            << "non-Int arguments: " << e1_type << " * " << e2_type << "\n";
    return set_type(Int)->type;
}

Symbol divide_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    Symbol e1_type = e1->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    Symbol e2_type = e2->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    
    if (e1_type != Int || e2_type != Int)
        classtable->semant_error(cls->get_filename(), this)
            << "non-Int arguments: " << e1_type << " / " << e2_type << "\n";
    return set_type(Int)->type;
}

Symbol neg_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    Symbol e1_type = e1->eval_type(symtbl, cls, methodtable, attrtable, classtable);

    if (e1_type != Int)
        classtable->semant_error(cls->get_filename(), this)
            << "Argument of '~' has type " << e1_type << " instead of Int.\n";
    return set_type(Int)->type;
}

Symbol lt_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    Symbol e1_type = e1->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    Symbol e2_type = e2->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    
    if (e1_type != Int || e2_type != Int)
        classtable->semant_error(cls->get_filename(), this)
            << "non-Int arguments: " << e1_type << " < " << e2_type << "\n";
    return set_type(Bool)->type;
}

Symbol eq_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    Symbol e1_type = e1->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    Symbol e2_type = e2->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    
    if (e1_type == Int || e2_type == Int || e1_type == Str || 
        e2_type == Str || e1_type == Bool || e2_type == Bool)
        if (e1_type != e2_type)
            classtable->semant_error(cls->get_filename(), this)
                << "Illegal comparison with a basic type.\n";
    return set_type(Bool)->type;
}

Symbol leq_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    Symbol e1_type = e1->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    Symbol e2_type = e2->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    
    if (e1_type != Int || e2_type != Int)
        classtable->semant_error(cls->get_filename(), this)
            << "non-Int arguments: " << e1_type << " <= " << e2_type << "\n";
    return set_type(Bool)->type;
}

Symbol comp_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    Symbol e1_type = e1->eval_type(symtbl, cls, methodtable, attrtable, classtable);

    if (e1_type != Bool)
        classtable->semant_error(cls->get_filename(), this) 
            << "Argument of 'not' has type " << e1_type << " instead of Bool.\n";
    return set_type(Bool)->type;
}
// Const types
Symbol int_const_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    return set_type(Int)->type;
}

Symbol bool_const_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    return set_type(Bool)->type;
}

Symbol string_const_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    return set_type(Str)->type;
}

Symbol new__class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    if (classtable->get_class(type_name) || type_name == SELF_TYPE)
    {
        return set_type(type_name)->type;
    }
    classtable->semant_error(cls->get_filename(), this) 
        << "'new' used with undefined class " << type_name << ".\n";
    return set_type(Object)->type;
}

Symbol isvoid_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    e1->eval_type(symtbl, cls, methodtable, attrtable, classtable);
    return set_type(Bool)->type;
}

Symbol no_expr_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    return set_type(No_type)->type;
}

Symbol object_class::eval_type(TypeTableP symtbl, Class_ cls, MethodTableP methodtable, 
					        AttrTableP attrtable, ClassTableP classtable)
{
    if (name == self)
        return set_type(SELF_TYPE)->type;
    Symbol t = symtbl->lookup(name); 
    if (t) 
        return set_type(t)->type;
    attr_class *attr = dynamic_cast<attr_class *>(attrtable->get_feature(cls, name));
    if (attr)
        return set_type(attr->get_type_decl())->type;
    classtable->semant_error(cls->get_filename(), this) 
        << "Undeclared identifier " << name << ".\n";
    return set_type(Object)->type;
}

Class_ ClassTable::get_class(Symbol sym) 
{
    if (!class_namespace.count(sym))
    {
        // std::cout << "get_class: " << sym->get_string() << " does not exist." << std::endl;
        return nullptr;
    }

    return class_namespace[sym];
}

bool ClassTable::is_child_type(Symbol T1, Symbol T2, Class_ cls)
{
    if (T1 == No_type || T2 == No_type) 
        return false;

    if (T1 == T2) 
        return true;

    if (T2 == SELF_TYPE)
        return false;
    
    if (T1 == SELF_TYPE)
        T1 = cls->get_name();
    
    if (T1 == T2) 
        return true;

    Symbol child = T2;
    SymbolQueue symqueue;
    SymbolSet visitset;

    visitset.insert(T2);
    symqueue.push(T2);

    // BFS
    while (!symqueue.empty()) 
    {
        Symbol node = symqueue.front();
        symqueue.pop();
        auto children = children_list.find(node);
        
        if (children == children_list.end()) 
            continue;
        
        for (auto n : children->second)
        {
            if (visitset.find(n) == visitset.end())
            {
                if (T1 == n) return true;
                visitset.insert(n);
                symqueue.push(n); 
            }
        }
    }

    return false;
}

Symbol ClassTable::lub_type(Symbol T1, Symbol T2, Class_ cls)
{
    // std::cout << "Input " << T1 << " " << T2 << " end ";
    if (T1 == No_type || T2 == No_type)
        return Object;
    if (T1 == T2)
        return T1;
    if (T1 == SELF_TYPE)
        T1 = cls->get_name();
    if (T2 == SELF_TYPE)
        T2 = cls->get_name();
    if (T1 == T2)
        return T1;
    
    SymbolSet t1_par_set;
    t1_par_set.insert(T1);

    while (T1 != Object)
    {
        T1 = class_namespace[T1]->get_parent();
        t1_par_set.insert(T1);
    }

    while (T2 != Object)
    {
        if (t1_par_set.count(T2)){
            // std::cout << T2 << std::endl;
            return T2;}
        T2 = class_namespace[T2]->get_parent();
    }
    
    return Object;
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 

FeatureTable::FeatureTable(ClassTableP classtable) : classtable(classtable)
{
    for (auto cls : classtable->class_namespace)
    {
        FeatureMap map;
        class_feature_map[cls.second->get_name()] = map;
    }
};

void FeatureTable::set_feature(Symbol class_name, Feature feature) {
    if (!class_feature_map.count(class_name)) 
    {
        FeatureMap map;
        class_feature_map[class_name] = map;
        std::cout << "(should not happen) added to feature map: " << class_name << std::endl;
    }

    if (class_feature_map[class_name].count(feature->get_name())) 
    {
        multiple_def_err(classtable->get_class(class_name), class_feature_map[class_name][feature->get_name()]);
        return;
    }

    class_feature_map[class_name][feature->get_name()] = feature;
}

void FeatureTable::set_feature(Class_ class_, Feature feature) 
{
    set_feature(class_->get_name(), feature);
}

Feature FeatureTable::get_feature(Symbol class_name, Symbol feature_name) 
{
    Class_ class_;
    if (!class_feature_map.count(class_name)) 
    {
        std::cout << "(should not happen) get_feature: " << class_name->get_string() << " does not exist." << std::endl;
        return nullptr;
    }

    while (class_name != No_class) 
    {
        auto map = class_feature_map[class_name];
        auto meth = map.find(feature_name);
        // std::cout << "get_feature: " << class_name << " " << feature_name << std::endl; 
        if (meth != map.end()) 
        {
            return meth->second;
        }
        // std::cout << class_name->get_string() << Object->get_string() << std::endl;
        class_ = classtable->get_class(class_name);
        class_name = class_->get_parent();
    }
    return nullptr;
}

Feature FeatureTable::get_feature(Class_ class_, Symbol feature_name) 
{
    return get_feature(class_->get_name(), feature_name);
}

void FeatureTable::remove_feature(Symbol class_name, Symbol feature_name) 
{
    if (!class_feature_map.count(class_name)) 
    {
        // std::cout << "remove_feature: " << class_name->get_string() << " does not exist." << std::endl;
        return;
    }
    if (!class_feature_map[class_name].count(feature_name)) 
    {
        // std::cout << "remove_feature: " << feature_name->get_string() << " does not exist." << std::endl;
        return;
    }
    class_feature_map[class_name].erase(feature_name);
}

void FeatureTable::remove_feature(Class_ class_, Symbol feature_name) 
{
    remove_feature(class_->get_name(), feature_name);
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTableP classtable = new ClassTable(classes);
    classtable->check_main();
    classtable->check_type();
    /* some semantic analysis code may go here */

    if_err_exit(classtable);
}

