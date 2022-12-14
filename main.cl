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
                    if cmd = "load" then
                        loadCmd()
                    else if cmd = "print" then
                        printCmd()
                    else if cmd = "merge" then
                        mergeCmd()
                    else if cmd = "filterBy" then
                        filterCmd()
                    else if cmd = "sortBy" then
                        sortByCmd()
                    else abort()
                    fi fi fi fi fi;
                somestr <- in_string();
                tokenizer <- new StringTokenizer.init(somestr, somestr.length());
                cmd <- tokenizer.nextToken();
            } pool;

        }
    };

    loadCmd() : Object {
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
        }
    };

    printCmd() : Object {
        if 1 = tokenizer.getTokensLen() then
            -- print all lists
            out_string(lists.toString())
        else
            -- get nth list & print it
            let wantedList : List <- lists.getNthList(atoiHelper.a2i(tokenizer.nextToken())) in
            {
                out_string("[ ".concat(wantedList.toStringInner()).concat(" ]\n"));
            }
        fi
    };

    mergeCmd() : Object {
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
        }
    };

    filterCmd() : Object {
        let idx : Int <- atoiHelper.a2i(tokenizer.nextToken()),
            filter : String <- tokenizer.nextToken(),
            filteredList : List <- lists.getNthList(idx) in
        {
            if filter = "ProductFilter" then
                filteredList <- filteredList.filterBy(new ProductFiler)
            else if filter = "RankFilter" then
                filteredList <- filteredList.filterBy(new RankFilter)
            else if filter = "SamePriceFilter" then
                filteredList <- filteredList.filterBy(new SamePriceFilter)
            else
                lists
            fi fi fi;

            lists <- lists.replaceListAtIndex(idx, filteredList);
        }
    };

    sortByCmd() : Object {
        let idx : Int <- atoiHelper.a2i(tokenizer.nextToken()),
            comparator : String <- tokenizer.nextToken(),
            mode : String <- tokenizer.nextToken(),
            order : Bool <- if mode = "ascendent" then true else false fi,
            listToBeSorted : List <- lists.getNthList(idx) in
        {
            if comparator = "PriceComparator" then
                listToBeSorted <- listToBeSorted.sortBy(new PriceComparator, order)
            else if comparator = "RankComparator" then
                listToBeSorted <- listToBeSorted.sortBy(new RankComparator, order)
            else if comparator = "AlphabeticComparator" then
                listToBeSorted <- listToBeSorted.sortBy(new AlphabeticComparator, order)
            else
                listToBeSorted
            fi fi fi;

            lists <- lists.replaceListAtIndex(idx, listToBeSorted);
        }
    };
};
