(* Think of these as abstract classes *)
class Comparator {
    compareTo(o1 : Object, o2 : Object): Bool { false };
};

class PriceComparator inherits Comparator {
    compareTo(o1 : Object, o2 : Object) : Bool {
        let p1 : Int <- getPrice(o1),
            p2 : Int <- getPrice(o2) in
        {
            if p1 < p2 then true else false fi;
        }
    };

    getPrice(obj : Object) : Int {
        case obj of
            p : Product => p.getprice();
            o : Object => { abort(); 0; };
        esac
    };
};

class RankComparator inherits Comparator {
    compareTo(o1 : Object, o2 : Object) : Bool {
        let r1 : Int <- getRank(o1),
            r2 : Int <- getRank(o2) in
        {
            if r1 < r2 then true else false fi;
        }
    };

    getRank(obj : Object) : Int {
        case obj of
            p : Private => 1;
            c : Corporal => 2;
            s : Sergent => 3;
            o : Officer => 4;
            object : Object => { abort(); 0; };
        esac
    };
};

class AlphabeticComparator inherits Comparator{
    compareTo(o1 : Object, o2 : Object) : Bool {
        let s1 : String <- getString(o1),
            s2 : String <- getString(o2) in
        {
            if s1 < s2 then true else false fi;
        }
    };

    getString(obj : Object) : String {
        case obj of
            sw : StringWrapper => sw.getvalue();
            o : Object => { abort(); ""; };
        esac
    };
};

class Filter {
    filter(o : Object):Bool {true};
};

class ProductFiler inherits Filter {
    filter(o : Object) : Bool {
        case o of
            product : Product => true;
            object : Object => false;
        esac
    };
};

class RankFilter inherits Filter {
    filter(o : Object) : Bool {
        case o of
            rank : Rank => true;
            object : Object => false;
        esac
    };
};

class SamePriceFilter inherits Filter {
    filter(o : Object) : Bool {
        case o of
            product : Product => product.getprice() = product@Product.getprice();
            object : Object => false;
        esac
    };
};