(*
    Helper class used to collect the data from stdin and to create
    the correspondent object.
*)

class ObjectLoader {
    tokens : StringTokenizer;
    objType : String;

    init(st : StringTokenizer, obj : String) : SELF_TYPE {
        {
            tokens <- st;
            objType <- obj;
            self;
        }
    };

    createObject() : Object {
        if objType = "Soda" then
           new Soda.init(tokens.nextToken(), tokens.nextToken(), new A2I.a2i(tokens.nextToken()))
        else if objType = "Coffee" then
            new Coffee.init(tokens.nextToken(), tokens.nextToken(), new A2I.a2i(tokens.nextToken()))
        else if objType = "Laptop" then
            new Laptop.init(tokens.nextToken(), tokens.nextToken(), new A2I.a2i(tokens.nextToken()))
        else if objType = "Router" then
            new Router.init(tokens.nextToken(), tokens.nextToken(), new A2I.a2i(tokens.nextToken()))
        else abort()
        fi fi fi fi
    };

};