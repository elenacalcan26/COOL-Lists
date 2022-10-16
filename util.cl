(* Think of these as abstract classes *)
class Comparator {
    compareTo(o1 : Object, o2 : Object):Int {0};
};

class Filter {
    filter(o : Object):Bool {true};
};

(* TODO: implement specified comparators and filters*)

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