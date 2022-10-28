class List {
    head() : Object { { abort(); self; } };

    tail() : List { { abort(); self; } };

    isEmpty() : Bool { true };

    cons(h : Object) : Cons {
        new Cons.init(h, self)
    };

    add(o : Object) : List { new Cons.init(o, self) };

    append(l : List) : List { l };

    toString(): String { "" };

    -- returns elements of a inner list as string
    toStringInner() : String { "" };

    merge(other : List) : List { self };

    filterBy(f : Filter) : List { self };

    (*
        -- comparator -> the used comparator for sorting
        -- order -> the manner in which the list is sorted; true - ascending, false - descending
    *)
    sortBy(comparator : Comparator, order : Bool) : List { self };

    getInnerNthElem(n : Int) : String { "" };

    -- returns the nth list form the list of lists
    getNthList(n : Int) : List { self };

    -- used to remove the nth element from the given index
    removeFromIndex(n : Int) : List { self };

    -- used to replace the nth list from the list of lists with a given one as a parameter
    replaceListAtIndex(n : Int, l : List) : List { self };

    --  returns the minimum or maximum element of the list based on the sorting order
    getMinMaxInnerElem(o : Object, cmp : Comparator, order : Bool) : Object { head() };

    -- deletes the given element from list
    removeGivenElem(o : Object) : List { self };

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
        let str : String <- "",
            copy : List <- self,
            idx : Int <- 1,
            atoiHelper : A2I <- new A2I in
        {
            str <- atoiHelper.i2a(idx).concat(": [ ").concat(str);
            while not copy.isEmpty() loop
            {
                case copy.head() of
                    l : List => str <- str.concat(l.toStringInner()).concat(" ]\n");
                    o : Object => str <- str.concat(castHeadToString());
                esac;

                copy <- copy.tail();
                idx <- idx + 1;

                if not copy.isEmpty() then
                    str <- str.concat(atoiHelper.i2a(idx).concat(": [ "))
                else
                    str <- str.concat("")
                fi;

            } pool;
            str;
        }
    };

    toStringInner() : String {
        if isEmpty() then
            ""
        else if tl.isEmpty() then
            castHeadToString()
        else
            castHeadToString().concat(", ").concat(tl.toStringInner())
        fi fi
    };

    getNthList(idx : Int) : List {
        let iter : Int <- 1,
            copy : List <- self in
        {
            -- loop the list of lists until we find the nth one
            while iter < idx loop
            {
                iter <- iter + 1;
                copy <- copy.tail();
            } pool;

            -- cast to List type
            case copy.head() of
                l : List => l;
            esac;
        }
    };

    removeFromIndex(n : Int) : List {
        let copy : List <- self,
            iter : Int <- 1,
            aux : List <- new List in -- accumulator role, retains the resulting list
        {
            -- iterate the first n - 1 elements
            while iter < n loop
            {
                iter <- iter + 1;
                aux <- aux.add(copy.head()); -- add element
                copy <- copy.tail();
            } pool;

            -- append whith the nth element tail
            aux <- aux.append(copy.tail());
            aux;
        }
    };

    replaceListAtIndex(n : Int, l : List) : List {
        let copy : List <- self,
            iter : Int <- 1,
            aux : List <- new List in -- accumulator role, retains the resulting list
        {
            while not copy.isEmpty() loop
            {
                -- add elements (this time lists); on the nth index, will append the new list
                aux <- if iter = n then aux.cons(l) else aux.cons(copy.head()) fi;
                iter <- iter + 1;
                copy <- copy.tail();
            } pool;
            aux;
        }
    };

    merge(other : List) : List {
        let copy : List <- self,
            aux : List <- other in
        {
            -- add elements from the given list to the current instance, one by one
            while not aux.isEmpty() loop
            {
                copy <- copy.add(aux.head());
                aux <- aux.tail();

            } pool;
            copy;
        }
    };

    filterBy(f : Filter) : List {
        let aux : List <- self,
            filteredList : List <- new List in
        {
            while not aux.isEmpty() loop
            {
                -- add an element in the list if it passes the filter condition
                filteredList <- if f.filter(aux.head()) then filteredList.add(aux.head()) else filteredList fi;
                aux <- aux.tail();
            } pool;
            filteredList;
        }
    };

    getMinMaxInnerElem(elem : Object, cmp : Comparator, order : Bool) : Object {
        let copy : List <- self in
        {
            if copy.isEmpty()
            then
                elem
            else {
                while not copy.isEmpty() loop
                {
                    -- check if elements respects the order
                    if cmp.compareTo(elem, copy.head()) = order
                    then
                        copy <- copy.tail()
                    else
                    {
                        elem <- copy.head();
                        copy <- copy.tail();
                    }
                    fi;
                } pool;
               elem;
            } fi;
            elem;
        }
    };

    removeGivenElem(elem : Object) : List {
        let copy : List <- self,
            acc : List <- new List in -- stores the elements that are different than the removed one
        {
            -- loop list
            while not copy.isEmpty() loop
            {
                if not copy.head() = elem
                then
                    acc <- acc.add(copy.head())
                else
                    0 -- dummy
                fi;
                copy <- copy.tail();
            } pool;
            acc;
        }
    };

    -- basically, it is created a new list by adding elements one by one according to the sorting order
    sortBy(comparator : Comparator, order : Bool) : List {
        let copy : List <- self,
            acc : List <- new List, -- accumulator role, stores the sorted list
            elem : Object in
        {
            while not copy.isEmpty() loop
            {
                -- get the minimum/maximum element
                elem <- copy.getMinMaxInnerElem(copy.head(), comparator, order);
                -- add it to the new list
                acc <- acc.add(elem);
                -- remove the found element from the current instance
                copy <- copy.removeGivenElem(elem);
            } pool;
            acc;
        }
    };

    getInnerNthElem(n : Int) : String {
        if n = 0 then
            -- found element
            castHeadToString()
        else
            {
                -- move to tail element and will check if its head is the wanted element
                n <- n - 1;
                tl.getInnerNthElem(n);
            }
        fi
    };


    -- Downcast the head element of the list and returns its string format
    castHeadToString() : String  {
        let str : String <-
            case self.head() of
                product : Product => product.toString();
                rank : Rank => rank.toString();
                string : String => string;
                sw : StringWrapper => sw.toString();
                iw : IntWrapper => iw.toString();
                bw : BoolWrapper => bw.toString();
                iow : IOWrapper => iow.toString();
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