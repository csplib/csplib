(define (domain plotting)
    
    (:requirements :typing :equality :universal-preconditions :conditional-effects)

    (:types number colour)

    (:constants null wildcard - colour)

    (:predicates
        (hand ?c - colour)
        (coloured ?r ?c - number ?c - colour)

        (succ ?n1 ?n2 - number)
        (gt ?n1 ?n2 - number)
        (pred ?n1 ?n2 - number)
        (lt ?n1 ?n2 - number)

        (distance ?n1 ?n2 ?n3 - number)

        (isfirstcolumn ?n - number) ;; is the first column?
        (islastcolumn ?n - number) ;; is the last column?
        (istoprow ?n - number) ;; is the top row?
        (isbottomrow ?n - number) ;; is the bottom column?

        ;; are we allowed to fire on those?
        (blockedcol ?n - number)
        (blockedrow ?n - number)
    )

    ;; removing a partial row:
    ;;    - we are removing part of a row and there is a next block that has a different
    ;;      colour than the first one.
    (:action shoot-partial-row
        ;; ?r - what row we are shooting
        ;; ?f ?t - from and to, the range that we are trying to remove from the board
        ;; ?c - the colour of the range
        :parameters (?r - number ?t - number ?c - colour)
        :precondition
            (and
                ;; there exists a number that is the successor of the to
                ;; and it has a different colour than c
                (exists (?col - number)
                    (and
                        (succ ?col ?t)
                        (not (coloured ?r ?col ?c))
                        (not (coloured ?r ?col null))))
                      
                ;; exist some column before ?t or ?t that has colour ?c      
                (exists (?col - number)
                    (and
                        (or (lt ?col ?t) (=  ?col ?t))
                        (coloured ?r ?col ?c)))

                ;; stop possible weird stuff
                (not (= ?c null))
                (not (= ?c wildcard))
                ;; colour block and hand is the same (we avoid null movements)
                (or (hand ?c) (hand wildcard))

                ;; from the start, all the blocks up to ?t have either the colour ?c or are null
                (forall (?col - number) 
                    ;; For all columns, either:
                    (or
                        ;; we are out of bounds or
                        (gt ?col ?t)
                 
                        ;; the middle colors and color of ?t are null or the correct one
                        (coloured ?r ?col ?c)
                        (coloured ?r ?col null))))
        :effect
            (and
                ;; Change hands colour and
                ;; The next cell that we cannot remove gets the colour from the hand
                (forall (?nextcolumn - number ?nextcolour - colour)
                    (when 
                        (and
                            ;; there is a new row down this one
                            (succ ?nextcolumn ?t) 
                            ;; and the cell is coloured ?nextcolour
                            (coloured ?r ?nextcolumn ?nextcolour))
                        ;; The hand gets a new colour
                        (and
                            ;; change next cell colour
                            (not (coloured ?r ?nextcolumn ?nextcolour))
                            (coloured ?r ?nextcolumn ?c)
                            ;; change hand colours
                            (hand ?nextcolour)
                            (not (hand ?c))
                            (not (hand wildcard)))))

                ;; finally move everything downwards. we have 2 cases:
                ;;  - we are on the top row
                ;;  - we are on another row
                (forall (?currentrow ?nextrow ?currentcol - number)
                    (and
                        ;; We are on the top row: we must restore the "null" colour
                        (forall (?currentcolor - colour)
                            (when
                                (and 
                                    ;; we are on the top row 
                                    (istoprow ?currentrow)
                                    ;; The column is in the correct range
                                    (or (lt ?currentcol ?t) (= ?currentcol ?t))
                                    ;; We identify the colour of the cell
                                    (coloured ?currentrow ?currentcol ?currentcolor)
                                    ;; avoid contradiction
                                    (not (coloured ?currentrow ?currentcol null)))
                                (and
                                    (not (coloured ?currentrow ?currentcol ?currentcolor))
                                    (coloured ?currentrow ?currentcol null))))

                        ;; We are on any other row: disable the current colour and change the next colour
                        (forall (?currentcolor ?nextcolor - colour)
                            (when
                                ;; when the row is on top of the one we deleted, we "decrease"
                                ;; one position downwards 
                                (and
                                    (lt ?currentrow ?r)            ;; R R R R <- current row 
                                    (succ ?nextrow ?currentrow)    ;; B B B R <- nextrow
                                    ;; The current column is in the correct range (less than the ?t)
                                    (or (lt ?currentcol ?t) (= ?currentcol ?t))

                                    ;; here we ensure that the cells have the pertaining colours
                                    (coloured ?currentrow ?currentcol ?currentcolor)
                                    (coloured ?nextrow ?currentcol ?nextcolor)
                                    ;; avoid the contradiction in the effect if both colours are equal
                                    (not (= ?currentcolor ?nextcolor)))
                                (and ;; and as an effect we change the lower row
                                    (not (coloured ?nextrow ?currentcol ?nextcolor))
                                    (coloured ?nextrow ?currentcol ?currentcolor))))))))

    (:action shoot-column
        ;; ?column - what col we are shooting
        ;; ?t - to, the cell where we stop eating
        ;; ?c - the colour we will act on
        :parameters (?column - number ?t - number ?c - colour)
        :precondition 
            (and
                ;; the successor of the to is of a different colour
                ;; or we are eating the whole column.
                ;; note we don't have to check null colour because gravity
                (or
                    (exists (?row - number)
                        (and
                            (succ ?row ?t)
                            (not (coloured ?row ?column ?c))))
                    (isbottomrow ?t))

                ;; stop possible weird stuff
                (not (= ?c null))
                (not (= ?c wildcard))
                ;; colour block and hand is the same (we avoid null movements)
                (or (hand ?c) (hand wildcard))

                ;; all the middle blocks also have the same colour
                (forall (?row - number) 
                    (or
                        ;; either we are out of bounds or
                        (gt ?row ?t)
                        ;; we are exactly in ?t and it has the correct colour
                        (and (= ?row ?t) (coloured ?row ?column ?c))
                        ;; we are on top of ?t and therefore it can be either ?c or null
                        (and
                            (lt ?row ?t)
                            (or
                                (coloured ?row ?column ?c)
                                (coloured ?row ?column null))))))
        :effect
            (and
                (forall (?runningrow - number)
                    (and
                        ;; we remove the colour of the cells before the "to" (?t)
                        (when 
                            (and
                                (coloured ?runningrow ?column ?c)
                                (or (lt ?runningrow ?t)
                                    (= ?runningrow ?t))
                                (not (coloured ?runningrow ?column null)))
                            (and
                                (coloured ?runningrow ?column null)
                                (not (coloured ?runningrow ?column ?c))))

                        ;; we set the next block colour to the removed cells colour
                        ;; and we set the hands colour to the correct one
                        (forall (?nextcolour - colour)
                            (when
                                (and
                                    (succ ?runningrow ?t)
                                    (coloured ?runningrow ?column ?nextcolour))
                                (and
                                    ;; next cell colour
                                    (not (coloured ?runningrow ?column ?nextcolour))
                                    (coloured ?runningrow ?column ?c)
                                    ;; hand colour
                                    (not (hand wildcard))
                                    (not (hand ?c))
                                    (hand ?nextcolour))))))

                ;; When we are eating the whole column, we set the correct colour onto the hand
                (when
                    (isbottomrow ?t)
                    (and
                        (not (hand wildcard))
                        (hand ?c)))))

    ;; Shoot complete row and any flavour of column
    (:action shoot-row-and-column
            ;; ?r - what complete row we are shooting.
            ;; ?t - At which row we are stopping on the column that we are "eating"
            ;; ?c - the colour of the range
            :parameters (?r - number ?t - number ?c - colour)
            :precondition
                (and
                    ;; rows of the shot are coherent
                    (gt ?t ?r)

                    ;; stop possible weird stuff
                    (not (= ?c null))
                    (not (= ?c wildcard))
                    ;; colour block and hand is the same (we avoid null movements)
                    (or (hand ?c) (hand wildcard))

                    ;; all the blocks in the row and in the corresponding part of the
                    ;; column are either the same colour or are empty
                    (forall (?row ?col - number) 
                        (and
                            (imply
                                (= ?r ?row)
                                (or
                                    (coloured ?r ?col ?c)
                                    (coloured ?r ?col null)))
                            (imply
                                (and
                                    (gt ?row ?r)
                                    (not (gt ?row ?t))
                                    (islastcolumn ?col))
                                (or
                                    (coloured ?row ?col ?c)
                                    (coloured ?row ?col null)))))

                    ;; we have to eat at least one of those!
                    (exists (?row ?col - number) 
                        (or 
                            (and
                                (= ?r ?row)
                                (coloured ?r ?col ?c))
                        
                            (and
                                (gt ?row ?r)
                                (not (gt ?row ?t))
                                (islastcolumn ?col)
                                (coloured ?row ?col ?c))))
                            
                    ;; either ?t is the last row, or in its successor there is a different colour
                    (or
                        (isbottomrow ?t)
                        (exists (?nextrow ?lastcol - number)
                            (and
                                (succ ?nextrow ?t)
                                (islastcolumn ?lastcol)
                                (not (coloured ?nextrow ?lastcol ?c))
                                (not (coloured ?nextrow ?lastcol null))))))
            :effect
                (and
                    ;; move everything downwards except the last column.
                    ;; 2 cases: we are on the top row or  we are on a middle row
                    (forall (?currentrow ?currentcol ?nextrow - number)
                        (and
                            ;; case 1 - We are on the top row: we must restore the "null" colour
                            (when
                                (istoprow ?currentrow)
                                (and
                                    (not (coloured ?currentrow ?currentcol ?c))
                                    (coloured ?currentrow ?currentcol null)))

                            ;; case 2 - We are on the middle row: disable the current colour and change the next colour
                            (forall (?currentcolor ?nextcolor - colour)
                                (when
                                    (and
                                        (not (istoprow ?nextrow))        ;; is not top row
                                        (not (islastcolumn ?currentcol)) ;; is not last column
                                        ;; position the "pointers"
                                        (lt ?currentrow ?r)              
                                        (succ ?nextrow ?currentrow)
                                        ;; ensure that the cells have the pertaining colours
                                        (coloured ?currentrow ?currentcol ?currentcolor)
                                        (coloured ?nextrow ?currentcol ?nextcolor)
                                        ;;avoid effect contradiction (if both colours are equal)
                                        (not (= ?currentcolor ?nextcolor))
                                    )
                                    (and ;; and as an effect we change the lower row
                                        (not (coloured ?nextrow ?currentcol ?nextcolor))
                                        (coloured ?nextrow ?currentcol ?currentcolor))))))

                    ;; The waterfall effect
                    (forall (?currentrow ?nextrow ?lastcolumn ?d ?dplus1 ?d2 - number ?nextcolour ?currentcolour - colour)
                        (and

                            ;; unconditionally, if we reach the end of any row at all, the cell
                            ;; on the top right of the grid will get a null.
                            (when
                                (and
                                    (istoprow ?currentrow)
                                    (islastcolumn ?lastcolumn)
                                    (coloured ?currentrow ?lastcolumn ?nextcolour)
                                    (not (coloured ?currentrow ?lastcolumn null)))
                                (and
                                    (not (coloured ?currentrow ?lastcolumn ?nextcolour))
                                    (coloured ?currentrow ?lastcolumn null)))

                   
                            ;; we act only on the set of cells between ?r and ?t
                            ;; base case: we are on top of the row
                            (when 
                                (and
                                    ;; we consider the case of the last column
                                    (islastcolumn ?lastcolumn)
                                    ;; distance between the row we shoot at and where we stop is ?d
                                    (distance ?r ?t ?d)
                                    ;; and we calculate the distance we have to move cells downwards
                                    (succ ?dplus1 ?d)
                                    ;; we need to fix this here to be able to compute the distance with the
                                    ;; nextrow pointer, which is the one we use to change 
                                    (istoprow ?currentrow)
                                    ;; we are on top of the ?t
                                    (or (lt ?nextrow ?t) (= ?nextrow ?t))
                                    ;; ?d2 is the distance between nextrow currentrow
                                    ;; and this distance is less than what we should copy
                                    (distance ?currentrow ?nextrow ?d2)
                                    (lt ?d2 ?dplus1)

                                    ;; and the colours are correct and not null
                                    (coloured ?nextrow ?lastcolumn ?nextcolour)
                                    (not (= ?nextcolour null)))
                                (and ;; we switch colours
                                    (not (coloured ?nextrow ?lastcolumn ?nextcolour))
                                    (coloured ?nextrow ?lastcolumn null)))
                            ;; other cases
                            (when 
                                (and
                                     ;; we consider the case of the last column
                                    (islastcolumn ?lastcolumn)
                                    ;; distance between the row we shoot at and where we stop is ?d
                                    (distance ?r ?t ?d)
                                    ;; and we calculate the distance we have to move cells downwards
                                    (succ ?dplus1 ?d)
                                    ;; we are on top of the ?t
                                    (or (lt ?nextrow ?t) (= ?nextrow ?t))
                                    ;; and the distance is ?dplus1
                                    (distance ?nextrow ?currentrow ?dplus1)
                                    ;; and the colours are correct and different
                                    (coloured ?currentrow ?lastcolumn ?currentcolour)
                                    (coloured ?nextrow ?lastcolumn ?nextcolour)
                                    (not (= ?currentcolour ?nextcolour)))
                                (and ;; we switch colours
                                    (not (coloured ?nextrow ?lastcolumn ?nextcolour))
                                    (coloured ?nextrow ?lastcolumn ?currentcolour)))
                            ;; finally, we change the colour of the cell on the bottom of the ?t.
                            (when 
                                (and
                                     ;; we consider the case of the last column
                                    (islastcolumn ?lastcolumn)
                                    ;; distance between the row we shoot at and where we stop is ?d
                                    (distance ?r ?t ?d)
                                    ;; and we calculate the distance we have to move cells downwards
                                    (succ ?dplus1 ?d)
                                    (succ ?nextrow ?t)
                                    ;; and the colour is correct and different from null
                                    (coloured ?nextrow ?lastcolumn ?nextcolour))
                                (and ;; we switch colours
                                    (not (coloured ?nextrow ?lastcolumn ?nextcolour))
                                    (coloured ?nextrow ?lastcolumn ?c)))))
                   
                   ;; Change hands colour:
                   ;; base case when we eat all the last column or is all null
                   (when 
                        (isbottomrow ?t)
                        (and 
                            (hand ?c)
                            (not (hand wildcard))))

                    ;; - we are not the last row before ground, therefore
                    ;; the colour of the hand becomes the one under the last cell of the row
                    (forall (?nextrow ?lastcol - number ?nextcolour - colour)
                        (and 
                            (when 
                                (and
                                    ;; there is another row down this one
                                    (succ ?nextrow ?t)
                                    ;; and we are on the last column
                                    (islastcolumn ?lastcol)
                                    ;; and the next (down) cell is coloured ?nextcolour
                                    (coloured ?nextrow ?lastcol ?nextcolour))
                                (and
                                    ;; we change the colour of the next (down) cell
                                    (not (coloured ?nextrow ?lastcol ?nextcolour))
                                    (coloured ?nextrow ?lastcol ?c)
                                    ;; we set the hand to the next cell colour
                                    (hand ?nextcolour)
                                    (not (hand ?c))
                                    ;; we ensure we lose the wildcard if we had it
                                    (not (hand wildcard))))))))


;; Shoot complete row and 0 rows down
    (:action shoot-only-full-row
            ;; ?r - what complete row we are shooting.
            ;; ?c - the colour of the range
            :parameters (?r - number ?c - colour)
            :precondition
                (and
                    ;; stop possible weird stuff
                    (not (= ?c null))
                    (not (= ?c wildcard))
                    ;; colour block and hand is the same (we avoid null movements)
                    (or (hand ?c) (hand wildcard))

                    ;; all the blocks in the row and in the corresponding part of the
                    ;; column are either the same colour or are empty
                    (forall (?col - number) 
                        (or
                            (coloured ?r ?col ?c)
                            (coloured ?r ?col null)))

                    ;; we have to eat at least one of those!
                    (exists (?col - number) 
                        (coloured ?r ?col ?c))
                            
                    ;; either ?r is the last row, or in the cell below the last one 
                    ;; we have a different colour than ?c
                    (or
                        (isbottomrow ?r)
                        (exists (?nextrow ?lastcol - number)
                            (and
                                (succ ?nextrow ?r)
                                (islastcolumn ?lastcol)
                                (not (coloured ?nextrow ?lastcol ?c))
                                (not (coloured ?nextrow ?lastcol null))))))
            :effect
                (and
                    ;; move everything downwards
                    ;; 2 cases: we are on the top row or  we are on a middle row
                    (forall (?currentrow ?currentcol ?nextrow - number)
                        (and
                            ;; case 1 - We are on the top row: we must restore the "null" colour
                            (when
                                (istoprow ?currentrow)
                                (and
                                    (not (coloured ?currentrow ?currentcol ?c))
                                    (coloured ?currentrow ?currentcol null)))

                            ;; case 2 - We are on the middle row: disable the current colour and change the next colour
                            (forall (?currentcolor ?nextcolor - colour)
                                (when
                                    (and
                                        (not (istoprow ?nextrow))        ;; is not top row
                                        ;; position the "pointers"
                                        (lt ?currentrow ?r)              
                                        (succ ?nextrow ?currentrow)
                                        ;; ensure that the cells have the pertaining colours
                                        (coloured ?currentrow ?currentcol ?currentcolor)
                                        (coloured ?nextrow ?currentcol ?nextcolor)
                                        ;;avoid effect contradiction (if both colours are equal)
                                        (not (= ?currentcolor ?nextcolor))
                                    )
                                    (and ;; and as an effect we change the lower row
                                        (not (coloured ?nextrow ?currentcol ?nextcolor))
                                        (coloured ?nextrow ?currentcol ?currentcolor))))))

                   ;; Change hands colour:
                   ;; base case when we eat all the last column or is all null
                   (when 
                        (isbottomrow ?r)
                        (and 
                            (hand ?c)
                            (not (hand wildcard))))

                    ;; - we are not the last row before ground, therefore
                    ;; the colour of the hand becomes the one under the last cell of the row
                    (forall (?nextrow ?lastcol - number ?nextcolour - colour)
                        (and 
                            (when 
                                (and
                                    ;; there is another row down this one
                                    (succ ?nextrow ?r)
                                    ;; and we are on the last column
                                    (islastcolumn ?lastcol)
                                    ;; and the next (down) cell is coloured ?nextcolour
                                    (coloured ?nextrow ?lastcol ?nextcolour))
                                (and
                                    ;; we change the colour of the next (down) cell
                                    (not (coloured ?nextrow ?lastcol ?nextcolour))
                                    (coloured ?nextrow ?lastcol ?c)
                                    ;; we set the hand to the next cell colour
                                    (hand ?nextcolour)
                                    (not (hand ?c))
                                    ;; we ensure we lose the wildcard if we had it
                                    (not (hand wildcard))))))))
)

