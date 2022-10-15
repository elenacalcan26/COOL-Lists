(*
    This class is used to break string into tokens. It mimics the Java StringTokanizer class.
*)

class StringTokenizer {
    tokens : List;
    delim : String <- " ";
    tokenPos : Int <- 0; -- current index of the token to be processed
    tokensLen : Int <- 0; -- total number of tokens

    (*
        Initialize the StringTokenizer class

        str -> string to be tokenized
        len -> str length

    *)
    init(str : String, len : Int) : SELF_TYPE {
        let tokenizedString : String <- str,
            tokenizedStringLen : Int <- len,
            tmpTokensList : List <- new List, -- temporary list, has accumulator role
            curr : Int <- 0,
            substrStartPos : Int <- 0,
            extractedSubStr: String <- "" in
            {
                -- loop character by character
                while curr < tokenizedStringLen loop
                    {
                        if tokenizedString.substr(curr, 1) = delim then
                            -- found a delimiter
                            {
                                -- subtract the obtained substring and add it to the temporary token list
                                extractedSubStr <- tokenizedString.substr(substrStartPos, curr - substrStartPos);
                                tmpTokensList <- tmpTokensList.add(extractedSubStr);
                                substrStartPos <- curr + 1; -- start position of the next substring
                                tokensLen <- tokensLen + 1;
                            }
                        else 0 -- dummy
                        fi;
                        curr <- curr + 1;
                    } pool;
                    -- last substring and creats the final list
                    extractedSubStr <- tokenizedString.substr(substrStartPos, curr - substrStartPos);
                    tokens <- tmpTokensList.add(extractedSubStr);
                    tokensLen <- tokensLen + 1;

            self;
        }
    };

    getTokens() : List { tokens };

    getTokensLen() : Int { tokensLen };

    getTokenPos() : Int { tokenPos };

    (*
        Returns the next token to be processed
    *)
    nextToken() : String {
        if tokenPos = tokensLen then
        {
            -- no more tokens => abort
            new IO.out_string("No more tokens available! Abort!");
            abort();
            ""; -- dummy
        }
        else {
            let str : String in
            {
                -- get the token from the token list based on tokenPos
                str <- tokens.getNthElem(tokenPos);
                tokenPos <- tokenPos + 1;
                str;
            };
        }
        fi
    };

};