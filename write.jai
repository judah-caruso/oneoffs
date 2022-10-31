/*
    Write is a small writing/markup tool that
    allows you to change how text is
    transformed. It was made to replace
    HTMl/markdown in my day-to-day.

    Write's syntax is inspired by Lisp. However,
    unlike Lisp, text is the main primitive and
    can be placed anywhere. Only special
    sequences are considered markup.

    For example, to embolden a piece of text:
        (bold Hello, this is a test.)
    transforms into:
        <strong>Hello, this is a test.</strong>

    The character '(' signals the start of an
    expression. To write a literal '(', escape
    it or use the '/' definition.
        This \(could be\) a (/ decent) test.
    transforms into:
        This (could be) a (decent) test.

    Note: any special character can be output
    literally if a '\' is placed before it.
        \( -> (
        \# -> #
        and so on...

    Because Write attempts to be as agnostic
    as possible, the above definitions are not
    defined by default. A 'use' directive must
    be placed at the top of the file:
        #use html
        #use markdown
        #use my_defs

    These definitions are builtin and cannot be
    edited (unless you modify the source and
    recompile). However, they can be overridden
    like so:
        #def bold $value
            <b>$value</b> #! Now using 'b' instead of 'strong'
        #end

    The above syntax isn't just for overriding
    builtin definitions, but defining new ones
    as well. For example, we may want to define
    a way to write urls:
        #define site $url $text
            <a href="http://$url">$text</a>
        #end

    It can now be used like so:
        (site judahcaruso.com | This is my website!)

    Aside from definitions, variables can also
    be defined. Variables are values that can
    be changed over time (definitions can also
    be overridden, however, variables are
    intended to be).

    The following file:
        #use markdown

        #let a-variable Hello, I am a (bold variable).
        #a-variable
        #set a-variable Now my value is 10
        #a-variable
    will produce:
        Hello, I am a **variable**.
        Now my value is 10

    Write always prints the final text to
    standard out.
*/

// @Todo:
// - Handle recursive defines (needs to error when found)
// - Handle recursive variables (shouldn't break/error)
// - Add builtin (+ - * /) so we can do math with variables
// - A very simple way to #ifdef out #use directives

PROGRAM_NAME    :: "j-write";
PROGRAM_VERSION :: "1a";
PROGRAM_USAGE   :: "@bold{.. usage} % @bold{<input file>}\n";

#run ct_compile_to_binary(PROGRAM_NAME);

main :: () {
    // Disable logging since 'read_entire_file' will log on error.
    context.logger = (_1: string, _2: *void, _3: Log_Info) {};

    args := get_command_line_arguments();
    consume_array(*args);

    if !args.count {
        color_print(PROGRAM_USAGE, PROGRAM_NAME, stderr = true);
        exit(1);
    }

    input_file := args[0];
    contents, read_ok := read_entire_file(input_file);
    if !read_ok {
        color_print("@red,bold{.. error} unable to load %\n", input_file, stderr = true);
        exit(1);
    }

    current_directory := path_strip_filename(input_file);
    output, o_ok := process_source(input_file, contents, current_directory);
    if !o_ok exit(1);

    output = trim(output, " \n\t\r");
    write_strings(output, "\n");
}

Token_Kind :: enum {
    Invalid;
    Let;
    Set;
    Use;
    Def;
    End;
    Variable;
    Identifier;
    Argument;
    Text;
    Number;
    Hash;
    LParen;
    RParen;
    FSlash;
    Pipe;
    Escape;
    Comment;
}

valid_rune :: (r: rune) -> bool {
    special :: (r) => r == #char "("  ||
                      r == #char ")"  ||
                      r == #char "#"  ||
                      r == #char "|"  ||
                      r == #char "\\" ||
                      r == #char "\n" ||
                      r == #char "\r" ||
                      r == #char "\t";

    return !special(r) && ((r >= 0x0020 && r <= 0x007F) || (r >= 44032 && r <= 55171));
}

starts_number :: (r: rune) -> bool {
    return r >= #char "0" && r <= #char "9";
}

is_identifier :: (r: rune) -> bool {
    return (r >= #char "a" && r <= #char "z") ||
           (r >= #char "A" && r <= #char "Z") ||
           (r == #char "-" || r == #char "_") ||
           (r == #char "!" || r == #char "'") ||
           (r == #char "/" || r == #char ".") ||
           starts_number(r);
}

Parser :: struct(T: Type, TK: Type) {
    index:  int;
    tokens: [..]T;
    errors: [..]Error;
};

peek :: (using p: *Parser) -> *p.T, bool {
    if index >= tokens.count return null, false;
    token := *tokens[index];
    return token, true;
}

next :: (using p: *Parser) -> *p.T, bool {
    if index >= tokens.count return null, false;

    defer index += 1;
    token := *tokens[index];
    return token, true;
}

need :: (using p: *Parser, kinds: ..p.TK) -> *p.T, bool {
    token, ok := next(p);
    if !ok return null, false;

    for kinds if it == token.kind {
        return token, true;
    }

    return token, false;
}

error :: (using p: *Parser, format_string: string, args: ..Any) {
    err := array_add(*p.errors);
    err.message = sprint(format_string, ..args);
}

check_need :: (p: *Parser, token: *$T, ok: bool, name: string) #expand {
    if !token {
        error(p, "expected % but found EOF", name);
        `return .{}, false;
    }

    if !ok {
        error(p, "expected % but found '%'", name, token.kind);
        `return .{}, false;
    }
}

parse_use :: (using p: *Parser) -> Use, bool {
    use: Use = ---;

    token, ok := need(p, .Use);
    check_need(p, token, ok, "#use");

    text, t_ok := need(p, .Identifier);
    check_need(p, text, t_ok, "module name");

    use.module = trim(text._string);
    if use.module == {
        case "text"; #through;
        case "html"; #through;
        case "markdown";
            use.internal = true;
        case;
            use.internal = false;
    }

    return use, true;
}

parse_def :: (using p: *Parser) -> Def, bool {
    def: Def = ---;

    token, ok := need(p, .Def);
    check_need(p, token, ok, "#def");

    name, n_ok := need(p, .Identifier);
    check_need(p, name, n_ok, "identifier");

    arg_names: [..]string;

    // Scan arguments: argument
    while ok {
        token, ok = peek(p);
        if !ok break;

        // @Todo: replace this
        if token.kind == .Text    ||
           token.kind == .Escape  ||
           token.kind == .Comment ||
           token.kind == .End break;

        if token.kind != .Argument {
            error(p, "expected an argument but found '%'", token.kind);
            return .{}, false;
        }

        array_add(*arg_names, copy_string(token._string));
        next(p);
    }

    // Scan def body: until #end
    body: [..]Value;
    while ok {
        token, ok = peek(p);
        if !ok || token.kind == .End break;

        value, p_ok := parse_value(p);
        if !p_ok break;

        array_add(*body, value);
    }

    token, ok = need(p, .End);
    check_need(p, token, ok, "#end");

    def.name = copy_string(name._string);
    def.args = arg_names;
    def.body = body;

    return def, true;
}

parse_let :: (using p: *Parser, is_set := false) -> Variable, bool {
    let: Variable = ---;

    token, l_ok := need(p, .Let, .Set);
    check_need(p, token, l_ok, ifx is_set then "#set" else "#let");

    ident, i_ok := need(p, .Identifier);
    check_need(p, ident, i_ok, "identifier");

    value, ok := parse_value(p, true);
    if !ok {
        error(p, "#% requires a value", ifx is_set then "set" else "let");
        return let, false;
    }

    let.name   = trim(ident._string);
    let.value  = value;
    let.is_set = is_set;

    return let, true;
}

parse_call :: (using p: *Parser) -> Call, bool {
    call: Call = ---;

    _, ok := need(p, .LParen); // Skip LParen
    if !ok return call, false;

    token, i_ok := need(p, .Identifier);
    check_need(p, token, i_ok, "identifier");

    arg_idx := 0;
    args: [..]Arg_List;
    list: *Arg_List;

    while true {
        token, ok := peek(p);
        if !ok return call, false;
        if token.kind == .RParen break;

        if token.kind == .Pipe {
            arg_idx += 1;
            list = array_add(*args);
            list.index = arg_idx;
            next(p);
            continue;
        }

        value, p_ok := parse_value(p);
        if !p_ok return call, false;

        if !list {
            list = array_add(*args);
            list.index = arg_idx;
        }

        array_add(*list.values, value);
    }

    call.name = token._string;
    call.args = args;

    token, ok = need(p, .RParen); // Skip RParen
    check_need(p, token, ok, "')'");

    return call, true;
}

parse_value :: (using p: *Parser, in_let := false) -> *Value, bool {
    token, ok := peek(p);
    if !ok return null, false;

    if token.kind == {
        case .Let;
            let, ok := parse_let(p);
            if !ok return null, false;

            value := New(Value);
            value.type     = Variable;
            value.variable = let;
            return value, true;

        case .Set;
            set, ok := parse_let(p, true);
            if !ok return null, false;

            value := New(Value);
            value.type     = Variable;
            value.variable = set;
            return value, true;

        case .Variable;
            value := New(Value);
            value.type = Variable;
            value.variable.name   = token._string;
            value.variable.is_set = false;

            next(p);

            return value, true;

        case .Use;
            use, ok := parse_use(p);
            if !ok return null, false;

            value := New(Value);
            value.type = Use;
            value.use  = use;

            return value, true;

        case .Def;
            def, ok := parse_def(p);
            if !ok return null, false;

            value := New(Value);
            value.type = Def;
            value.def  = def;

            return value, true;

        case .Text;
            next(p);

            value := New(Value);
            value.type = Text;
            value.text = copy_string(token._string);

            if in_let {
                value.text = trim(value.text);
            }

            return value, true;

        case .Number;
            next(p);

            value := New(Value);
            value.type   = Number;
            value.number = token._float;

            return value, true;

        case .Escape;
            next(p);

            value := New(Value);
            value.type = Text;
            value.text = copy_string(to_string(token._rune));

            return value, true;

        case .LParen;
            call, ok := parse_call(p);
            if !ok return null, false;

            value := New(Value);
            value.type = Call;
            value.call = call;

            return value, true;

        case;
            error(p, "unexpected token: %", token.kind);
    }

    return null, false;
}

setup_builtin_macros :: (doc: *Document) {
    register_def(doc, ".", "$value");

    raw_date := to_calendar(doc.publish_date);

    {
        date_string := sprint("%,%,%", raw_date.year, raw_date.month_starting_at_0 + 1, raw_date.day_of_month_starting_at_0 + 1);
        register_def(
            doc,
            "/date",
            date_string,
        );
    }

    {
        time_string := sprint("%:%:%", raw_date.hour, raw_date.minute, raw_date.second);
        register_def(
            doc,
            "/time",
            time_string,
        );
    }
}

lex_and_parse :: (source: string) -> []*Value, bool {
    lexer: Lexer(TOKEN_KIND = Token_Kind, IGNORE_WHITESPACE = false);
    lexer.source = source;

    register_handler(*lexer, (r) => r == #char "\\", (lex: $L) -> bool {
        next_rune(lex); // Skip slash

        r, ok := next_rune(lex);
        if !ok {
            error(lex, "expected a character after '\\' but found nothing");
            return false;
        }

        if r == {
            case #char "("; #through;
            case #char ")"; #through;
            case #char "#"; #through;
            case #char "$"; #through;
            case #char "n"; #through;
            case #char "s"; #through;
            case #char "t"; #through;
            case #char "\\";
                token := array_add(*lex.tokens);
                token.kind  = .Escape;

                if r == {
                    case #char "n";
                        token._rune = #char "\n";
                    case #char "s";
                        token._rune = #char " ";
                    case #char "t";
                        token._rune = #char "\t";
                    case;
                        token._rune = r;
                }

            case;
                error(lex, sprint("unknown escape sequence: \\%", to_string(r)));
                return false;
        }

        return true;
    });

    // Def argument: $foo
    register_handler(*lexer, (r) => r == #char "$", (lex: $L) -> bool {
        next_rune(lex); // skip $

        name := capture_while(lex, is_identifier);
        var  := array_add(*lex.tokens);
        var.kind    = .Argument;
        var._string = name;

        skip_while(lex, is_whitespace);

        return true;
    });

    // Call start
    register_handler(*lexer, (r) => r == #char "(",  (lex: $L) -> bool {
        paren := next_rune(lex);

        p := array_add(*lex.tokens);
        p.kind  = .LParen;
        p._rune = paren;

        ident := capture_while(lex, is_identifier);
        if !ident.count {
            error(lex, "expected identifier after '('");
            return false;
        }

        i := array_add(*lex.tokens);
        i.kind    = .Identifier;
        i._string = ident;

        return true;
    });

    // Call end
    register_handler(*lexer, (r) => r == #char ")",  (lex: $L) -> bool {
        paren := next_rune(lex);
        p := array_add(*lex.tokens);
        p.kind  = .RParen;
        p._rune = paren;
        return true;
    });

    register_handler(*lexer, (r) => r == #char "#",  (lex: $L) -> bool {
        next_rune(lex); // Skip hash

        // Line comment #!
        r, ok := peek_rune(lex);
        if ok && r == #char "!" {
            comment := capture_while(lex, (r) => r != #char "\n");
            skip_while(lex, is_whitespace);

            token := array_add(*lex.tokens);
            token.kind    = .Comment;
            token._string = trim(comment);
            return true;
        }

        i := capture_while(lex, is_identifier);
        if !i.count {
            error(lex, "expected identifier after '#'");
            return false;
        }

        ident := array_add(*lex.tokens);
        ident._string = i;

        needs_argument := true;
        if i == {
            case "let";
                ident.kind = .Let;
            case "set";
                ident.kind = .Set;
            case "use";
                ident.kind = .Use;
            case "def";
                ident.kind = .Def;
            case "end";
                ident.kind = .End;
                needs_argument = false;
            case;
                ident.kind = .Variable;
                needs_argument = false;
        }

        skip_while(lex, is_whitespace);

        if needs_argument {
            name := capture_while(lex, is_identifier);
            var  := array_add(*lex.tokens);
            var.kind    = .Identifier;
            var._string = name;
            skip_while(lex, is_whitespace);
        }

        return true;
    });

    register_handler(*lexer, (r) => r == #char "|",  (lex: $L) -> bool {
        pipe := next_rune(lex);
        p := array_add(*lex.tokens);
        p.kind  = .Pipe;
        p._rune = pipe;
        return true;
    });

    register_handler(*lexer, (r) => r == #char "/",  (lex: $L) -> bool {
        slash := next_rune(lex);
        p := array_add(*lex.tokens);
        p.kind  = .FSlash;
        p._rune = slash;
        return true;
    });

    register_handler(*lexer, starts_number, (lex: $L) -> bool {
        continues_number :: (r: rune) -> bool {
            return starts_number(r) ||
                   r == #char "."   ||
                   r == #char "_";
        }

        num := capture_while(lex, continues_number);
        number, ok := parse_float(*num);
        if !ok {
            error(lex, "invalid number: '%'", num);
            return false;
        }

        n := array_add(*lex.tokens);
        n.kind   = .Number;
        n._float = number;

        return true;
    });

    register_handler(*lexer, valid_rune, (lex: $L) -> bool {
        text := capture_while(lex, valid_rune);
        if !text.count {
            error(lex, "expected text");
            return false;
        }

        if trim(text, " \t\n\r").count {
            t := array_add(*lex.tokens);
            t.kind    = .Text;
            t._string = text;
        }

        return true;
    });

    register_handler(*lexer, is_whitespace, (lex: $L) -> bool {
        added_spaces: String_Builder;

        while true {
            value, ok := peek_rune(lex);
            if !ok || !is_whitespace(value) break;
            next_rune(lex);
            append(*added_spaces, to_string(value));
        }

        token := array_add(*lex.tokens);
        token.kind    = .Text;
        token._string = to_string(*added_spaces);

        return true;
    });

    ok := lex(*lexer);
    if !ok {
        for error: lexer.errors {
            color_print("@red,bold{.. error} %\n", error.message, stderr = true);
            for error.hints color_print("   @italic{%}\n", it, stderr = true);
        }

        return .[], false;
    }

    parser: Parser(lexer.Token, lexer.TOKEN_KIND);
    parser.tokens = lexer.tokens;

    values: [..]*Value;
    while true {
        token, t_ok := peek(*parser);
        if !t_ok break;

        // Ignore comments for now
        if token.kind == .Comment {
            next(*parser);
            continue;
        }

        value, ok := parse_value(*parser);
        if !ok break;

        array_add(*values, value);
    }

    if parser.errors.count {
        for parser.errors color_print("@red,bold{.. error} %\n", it.message, stderr = true);
        return .[], false;
    }

    return values, true;
}

process_source :: (filename: string, source: string, input_dir: string) -> (output: string, ok: bool) {
    values, ok := lex_and_parse(source);
    if !ok return "", false;

    doc: Document;
    doc.publish_date   = current_time_consensus();
    doc.input_path     = input_dir;
    doc.input_filename = filename;

    setup_builtin_macros(*doc);

    builder: String_Builder;
    for values {
        str, ok := expand_value(*doc, it);
        if !ok break;

        if str.count append(*builder, str);
    }

    if doc.errors.count {
        for doc.errors {
            color_print("@red,bold{.. error} %\n", it.message, stderr = true);
            if it.hints.count {
                for it.hints color_print("@yellow,bold{.. hint} @italic{%}\n", it, stderr = true);
                write_string("\n");
            }
        }

        return "", false;
    }

    return to_string(*builder), true;
}

Document :: struct {
    publish_date:    Apollo_Time;
    input_path:      string;
    input_filename:  string;
    output_filename: string;
    output_buffer:   String_Builder;
    values:          [..]Value;
    defines:         Table(type_of(Def.name), Def);
    variables:       Table(type_of(Variable.name), Variable);
    errors:          [..]Error;
}

Error :: struct {
    message: string;
    hints:   [..]string;
}

register_def :: (using doc: *Document, name: string, body: ..string) -> Def {
    is_valid :: (chr: u8) -> bool {
        return (chr >= #char "a" && chr <= #char "z") ||
               (chr >= #char "A" && chr <= #char "Z") ||
               (chr == #char "_" || chr == #char "-");
    }

    args:      [..]Value;
    arg_names: [..]string;

    for body {
        str := it;
        parts := split(str, "$");
        for parts if it.count {
            arg_name := it;
            for 0..arg_name.count - 1 {
                if !is_valid(arg_name.data[it]) {
                    arg_name.count = it;
                    break;
                }
            }

            if arg_name.count array_add_if_unique(*arg_names, arg_name);
        }

        value := array_add(*args);
        value.type = Text;
        value.text = it;
    }

    def: Def;
    def.name = copy_string(name);
    def.args = arg_names;
    def.body = args;

    table_set(*doc.defines, def.name, def);
    return def;
}

create_call :: (def: *Def, args: ..$T) -> Call {
    call_args: [..]Value;
    for args {
        arg := array_add(*call_args);
        set(arg, it);
    }

    call: Call;
    call.name = def.name;
    call.args = call_args;

    return call;
}

error :: (using doc: *Document, format_string: string, args: ..Any) -> *Error {
    err := array_add(*doc.errors);
    err.message = sprint(format_string, ..args);
    return err;
}

add_hints :: (err: *Error, hints: ..string) {
    for hints array_add(*err.hints, it);
}

expand_call :: (using doc: *Document, call: *Call) -> string, bool {
    macro := table_find_pointer(*defines, call.name);
    if !macro {
        variable := table_find_pointer(*variables, call.name);
        if variable {
            if call.args.count {
                error(doc, "variable '%' cannot be given arguments!", call.name);
                return "", false;
            }

            expanded, ok := expand_value(doc, variable.value);
            return expanded, ok;
        }

        error(doc, "no macro or variable '%' was defined", call.name);
        return "", false;
    }

    if call.args.count != macro.args.count {
        error(doc,
            "macro '%' expects % argument% but was given %\n",
            macro.name,
            macro.args.count,
            ifx macro.args.count == 1 then "" else "s",
            call.args.count);

        return "", false;
    }

    builder: String_Builder;
    for * node: macro.body {
        expanded, ok := expand_value(doc, node);
        if !ok return "", false;
        append(*builder, expanded);
    }

    pre_expansion  := to_string(*builder);
    post_expansion := pre_expansion;

    for args: call.args {
        expanded_args: String_Builder;

        for * arg: args.values {
            expanded, ok := expand_value(doc, arg);
            if !ok return "", false;
            append(*expanded_args, expanded);
        }

        placeholder := tprint("$%", macro.args[it_index]);
        post_expansion = replace(
            post_expansion,
            placeholder,
            trim(to_string(*expanded_args), " \n\t\r")
        );
    }

    return post_expansion, true;
}

expand_value :: (using doc: *Document, value: *Value) -> string, bool {
    if value.type == {
        case Text;
            return copy_string(value.text), true;

        case Number;
            return sprint("%", value.number), true;

        case Call;
            expanded, ok := expand_call(doc, *value.call);
            return trim(expanded, " \n\t\r"), ok;

        case Use;
            return "", import_module(doc, *value.use);

        case Def;
            return "", table_set(*doc.defines, value.def.name, value.def) != null;

        case Variable;
            var := value.variable;
            if var.value {
                if var.is_set {
                    ptr := table_find_pointer(*doc.variables, var.name);
                    if !ptr {
                        error(doc, "variable '%' was never given a value", var.name);
                        return "", false;
                    }

                    ptr.value = var.value;
                }
                else {
                    table_set(*doc.variables, var.name, var);
                }

                return "", true;
            }
            else { // If there's no value, it should be a call
                call: Call;
                call.name = var.name;

                expanded, ok := expand_call(doc, *call);
                return expanded, ok;
            }
    }

    error(doc, "unknown value of type '%'", value.type);
    return "", false;
}

import_module :: (doc: *Document, use: Use) -> bool {
    source: string;

    if use.internal {
        if use.module == {
            case "html";
                source = DEFAULT_HTML_DEFINES;
            case "markdown";
                source = DEFAULT_MARKDOWN_DEFINES;
            case;
                error(doc, "unknown internal module in @bold{#use %}", use.module);
                return false;
        }
    }
    else {
        module_file := tprint("%1%2%3.write", doc.input_path, PLATFORM_SEPARATOR, use.module);

        contents, r_ok := read_entire_file(module_file);
        if !r_ok {
            err := error(doc, "unable to @bold{#use %}", use.module);
            add_hints(err, sprint("does '%.write' exist in the current directory?", use.module));
            return false;
        }

        source = contents;
    }

    values, l_ok := lex_and_parse(source);
    if !l_ok return false;

    // Only import variables and definitions from the module
    for values if it.type == Variable || it.type == Def {
        _, ok := expand_value(doc, it);
        if !ok return false;
    }

    return true;
}

Def :: struct {
    name: string;
    args: []string;
    body: []Value;
}

Variable :: struct {
    name:   string;
    value:  *Value;
    is_set: bool;
}

Use :: struct {
    internal: bool;
    module:   string;
}

Value :: struct {
    type: Type;
    using _: union {
        text:     Text;
        number:   Number;
        call:     Call;
        use:      Use;
        def:      Def;
        variable: Variable;
    };
}

Text   :: #type string;
Number :: #type f64;

Call :: struct {
    name: string;
    args: [..]Arg_List;
}

Arg_List :: struct {
    index:  int;
    values: [..]Value;
}

#run {
    color_print("@yellow{.. verifying builtin macros compile...}\n");

    h, html_ok := lex_and_parse(DEFAULT_HTML_DEFINES);
    assert(html_ok, "'DEFAULT_HTML_DEFINES' failed to parse!");

    m, markdown_ok := lex_and_parse(DEFAULT_MARKDOWN_DEFINES);
    assert(markdown_ok, "'DEFAULT_MARKDOWN_DEFINES' failed to parse!");

    color_print("@green{.. they do!}\n");
}

DEFAULT_HTML_DEFINES :: #string END
#! These defines are imported with: '#use html'

#def bold      $value <strong>$value</strong> #end
#def italic    $value <em>$value</em>         #end
#def highlight $value <mark>$value</mark>     #end
#def strike    $value <del>$value</del>       #end
#def line      $value <u>$value</u>           #end
#def sub       $value <sub>$value<sub>        #end
#def sup       $value <sup>$value<sup>        #end
#def var       $value <var>$value</var>       #end

#! (' hello, world) -> 'hello, world'
#def ' $value &lsquo;$value&rsquo; #end

#! (" hello, world) -> "Hello, World"
#def " $value &ldquo;$value&rdquo; #end

#! (/ Hello, World) -> (Hello, World)
#def / $value &lparen;$value&rparen; #end

#! (- this | could be | a test) -> this-could be-a test
#def - $before $middle $after
   $before&mdash;$middle&mdash;$after
#end

#! (list
#!    (item hello)
#!    (item world))
#def list $items <ol>$items</ol> #end
#def item $value <li>$value</li> #end

#def break   <br/> #end
#def section <hr/> #end
END;

DEFAULT_MARKDOWN_DEFINES :: #string END
#! These defines are imported with: '#use markdown'

#def bold      $value **$value**       #end
#def italic    $value *$value*         #end
#def highlight $value `$value`         #end
#def strike    $value ~~$value~~       #end
#def line      $value <u>$value</u>    #end
#def sub       $value <sub>$value<sub> #end
#def sup       $value <sup>$value<sup> #end
#def var       $value `$value`         #end

#! #! (' hello, world) -> 'hello, world'
#def ' $value '$value' #end

#! #! (" hello, world) -> "Hello, World"
#def " $value "$value" #end

#! #! (/ Hello, World) -> (Hello, World)
#def / $value &lparen;$value&rparen; #end

#! #! (- this | could be | a test) -> this-could be-a test
#def - $before $middle $after
    $before&mdash;$middle&mdash;$after
#end

#! (list
#!    (item hello)
#!    (item world))
#def list $items $items\n   #end
#def item $value \t* $value #end

#def break   \n #end
#def section \n---\n #end
END;


#import,file "./base/module.jai";

// Jai imports
#import "File";
#import "Basic";
#import "String";
#import "String";
#import "Hash_Table";
