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

    getPrice(o : Object) : Int {
        case o of
            p : Product => p.getprice();
            obj : Object => { abort(); 0; };
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