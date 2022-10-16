class List {
    head() : Object { { abort(); self; } };

    tail() : List { { abort(); self; } };

    isEmpty() : Bool { true };

    cons(h : Object) : Cons {
        new Cons.init(h, self)
    };

    add(o : Object) : List { new Cons.init(o, self) };

    append(l : List) : List { l };

    toString(): String {
        "[ ]\n"
    };

    toStringInner() : String { "" };

    merge(other : List) : SELF_TYPE {
        self (* TODO *)
    };

    filterBy() : SELF_TYPE {
        self (* TODO *)
    };

    sortBy() : SELF_TYPE {
        self (* TODO *)
    };

    getNthElem(n : Int) : String { "" };

    print() : IO { new IO.out_string("\n") };
};

class Cons inherits List {
    -- stores data
    hd : Object;
    tl : List;

    head() : Object { hd };

    tail() : List { tl };

    isEmpty() : Bool { false };

    init(h : Object, t : List) : Cons {
        {
            hd <- h;
            tl <- t;
            self;
        }
    };

    -- add a new object in the list
    add(o : Object) : List {
        (new Cons).init(hd, tl.add(o))
    };

    -- append 2 lists
    append(l : List) : List {
       (new Cons).init(hd, tl.append(l))
    };

    toString() : String {
        let str : String <- "[ ",
            copy : List <- self in
        {
            while not copy.isEmpty() loop
            {
                str <- str.concat(copy.toStringInner());
                copy <- copy.tail();

            } pool;
            str;
        }
    };

    toStringInner() : String {
        if isEmpty() then
            " ]\n"
        else if tl.isEmpty() then
            castHeadToString()
            .concat(" ]\n")
        else
           castHeadToString().concat(", ").concat(tl.toStringInner())
        fi fi
    };

    merge(other : List) : SELF_TYPE {
        self (* TODO *)
    };

    filterBy() : SELF_TYPE {
        self (* TODO *)
    };

    sortBy() : SELF_TYPE {
        self (* TODO *)
    };

    getNthElem(n : Int) : String {
        if n = 0 then
            castHeadToString()
        else
            {
                n <- n - 1;
                tl.getNthElem(n);
            }
        fi
    };

    (*
        Downcast the head element of the list
    *)
    castHeadToString() : String  {
        let str : String <-
            case self.head() of
                product : Product => product.toString();
                rank : Rank => rank.toString();
                string : String => string;
                oHead: Object => "Object()";
            esac
        in
        str
    };

    -- used for debug
    print() : IO {
        {
            new IO.out_string(castHeadToString());
            new IO.out_string(" ");
            tl.print();
        }
    };
};