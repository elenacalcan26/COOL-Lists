(*
    Wrapper class is used to wrap the basic classes: String, Int, Bool and IO.
    It has a field to store the string value of String, Int, Bool objects read from stdin.
*)

class Wrapper {
    value : String;

    init(n : String) : SELF_TYPE {
        {
            value <- n;
            self;
        }
    };

    getvalue() : String { value };

    toString() : String { value };
};

class StringWrapper inherits Wrapper {
    toString() : String {
        "String("
        .concat(value)
        .concat(")")
    };
};

class IntWrapper inherits Wrapper {
    toString() : String {
        "Int("
        .concat(value)
        .concat(")")
    };
};

class BoolWrapper inherits Wrapper {
    toString() : String {
        "Bool("
        .concat(value)
        .concat(")")
    };
};

class IOWrapper inherits Wrapper {
    toString() : String { "IO()" };
};
