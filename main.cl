class Main inherits IO {
    lists : List <- new List;
    looping : Bool <- true;
    somestr : String;
    tokenizer : StringTokenizer;
    atoiHelper : A2I <- new A2I;

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
                        if 1 = tokenizer.getTokensLen() then
                            -- print all lists
                            out_string(lists.toString())
                        else
                            {
                                let wantedList : List <- lists.getNthList(atoiHelper.a2i(tokenizer.nextToken())) in
                                {
                                    out_string("[ ".concat(wantedList.toStringInner()));
                                };
                            }
                        fi

                    else if cmd = "merge" then
                        {
                            let idx1 : Int <- atoiHelper.a2i(tokenizer.nextToken()),
                                idx2 : Int <- atoiHelper.a2i(tokenizer.nextToken()),
                                list1 : List <- lists.getNthList(idx1),
                                list2 : List <- lists.getNthList(idx2),
                                merged : List in
                                {
                                    merged <- list1.merge(list2);

                                    lists <- lists.removeFromIndex(idx1);
                                    idx2 <- idx2 - 1;
                                    lists <- lists.removeFromIndex(idx2);

                                    lists <- lists.add(merged);
                                };
                        }
                    else abort()
                    fi fi fi;
                somestr <- in_string();
                tokenizer <- new StringTokenizer.init(somestr, somestr.length());
                cmd <- tokenizer.nextToken();
            } pool;

        }
    };
};
