class Main inherits IO {
    lists : List <- new List;
    looping : Bool <- true;
    somestr : String;
    tokenizer : StringTokenizer;

    main() : Object {

        let cmd : String <- "load" in
            {
                while looping loop {

                    -- check command
                    if cmd = "load" then {
                        let inputStr : String,
                            loadedObjects : List <- new List,
                            tokens : StringTokenizer,
                            objType : String,
                            loader : ObjectLoader <- new ObjectLoader in
                        {
                            inputStr <- in_string();
                            tokens <- new StringTokenizer.init(inputStr, inputStr.length());
                            objType <- tokens.nextToken();

                            while (not objType = "END") loop {

                                loader <- loader.init(tokens, objType);
                                loadedObjects <- loadedObjects.add(loader.createObject());

                                -- parse next string
                                inputStr <- in_string();
                                tokens <- new StringTokenizer.init(inputStr, inputStr.length());
                                objType <- tokens.nextToken();

                            } pool;

                            lists <- lists.add(loadedObjects);

                        };
                    } else if cmd = "print" then
                        out_string(lists.toString())
                    else abort()
                    fi fi;
                somestr <- in_string();
                tokenizer <- new StringTokenizer.init(somestr, somestr.length());
                cmd <- tokenizer.nextToken();
            } pool;

        }
    };
};
