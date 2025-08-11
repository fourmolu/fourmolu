{-# LANGUAGE TemplateHaskell #-}

expr_with_short_branches =
        if d
        then e
        else f

expr_with_tall_branches =
        if
        d
        then
        e
        e
        else
        f
        f

expr_with_do_blocks =
        if cond
        then do then1
                then2
        else do else1
                else2

expr_with_comments =
        if d
        then
        -- comment before true branch
        e -- comment after true expression
        else
        -- comment before false branch
        f -- comment after false expression

expr_with_tall_condition =
        if      a
                && b    then e  else f

expr_chain =
        if a
        then b
        else if c
        then d
        else if e
        then f
        else g

tuple = (g,
        if
        d
        then
        e
        else
        f
        )

statement = do
        h

        if i then j else k

        if i then j
        else k

        if i
        then j
        else k

        if
        i
        then
                j
        else
                k

if top_level then m else n

if top_level
        then m
        else n

if top_level
        then
                m
        else
                n

$(if top_level then m else n)

$(if top_level
        then m
        else n)

$(if top_level
        then
                m
        else
                n)
