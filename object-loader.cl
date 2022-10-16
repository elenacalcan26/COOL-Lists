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
        else if objType = "Private" then
            new Private.init(tokens.nextToken())
        else if objType = "Corporal" then
            new Corporal.init(tokens.nextToken())
        else if objType = "Sergent" then
            new Sergent.init(tokens.nextToken())
        else if objType = "Officer" then
            new Officer.init(tokens.nextToken())
        else if objType = "String" then
            new StringWrapper.init(tokens.nextToken())
        else if objType = "Int" then
            new IntWrapper.init(tokens.nextToken())
        else if objType = "Bool" then
            new BoolWrapper.init(tokens.nextToken())
        else if objType = "IO" then
            new IOWrapper.init("")
        else abort()
        fi fi fi fi fi fi fi fi fi fi fi fi
    };
};