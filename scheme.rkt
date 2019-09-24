#lang racket

( define ( interleave L1 L2 )
  ( cond
     ( ( and ( null? L1 ) ( null? L2 ) ) '() )
     ( ( null? L1 )  ( cons ( car L2 ) ( cdr L2 ) ) )
     ( ( null? L2 )  ( cons ( car L1 ) ( cdr L1 ) ) )
     ( else ( cons ( car L1 )
                   ( cons ( car L2 )
                          ( interleave ( cdr L1 ) ( cdr L2 ) )  ) ) )
  )
)

( define ( key-store L1 KEY )
  ( cond
     ( ( null? L1 ) '() )
     ( ( equal? ( car ( car L1 ) ) KEY ) ( car ( cdr ( car L1 ) ) ) )
     (  else ( key-store ( cdr L1 ) KEY ) )
  )
)

( define ( list-replace ALIST SYM VAL ) 
  ( cond
     ( ( null? ALIST ) '() ) 
     ( ( equal? SYM ( car ALIST ) ) ( cons VAL
                                           ( list-replace ( cdr ALIST ) SYM VAL ) ) )
     ( ( symbol? ( car ALIST ) ) ( cons ( car ALIST )
                                        ( list-replace ( cdr ALIST ) SYM VAL ) ) )
     (  else ( cons ( list-replace ( car ALIST ) SYM VAL )
                    ( list-replace ( cdr ALIST ) SYM VAL ) ) )
  )
)

( define ( first-n L1 N )
  ( cond
    ( ( or ( < N 1 ) ( null? L1 ) ) '() ) 
    (  else ( cons ( car L1 )( first-n ( cdr L1 ) ( - N 1 ) ) ) )
  )
)

( define ( forget-n L1 N )
  ( cond
    ( ( <= N 0 ) L1 ) ;
    ( ( < N 1 )  ( cdr L1 ) ) 
    ( ( null? L1 ) '() )
    ( else ( forget-n ( cdr L1 ) ( - N 1 ) ) )
  )
)

( define (running-sum L)
  ( cond
     ( ( null? L ) '() )
     ( ( null? ( cdr L ) )  ( cons
                             ( car L )
                             ( running-sum ( cdr L ) ) ) )
     ( else ( cons
              ( car L )
              ( running-sum ( cons
                              ( + ( car L ) ( car ( cdr L ) ) )
                              ( cdr ( cdr L ) ) ) ) ) ) 
  )
)

;;;counts the occurences of a given symbol SYM in a list L
(define (count-sym L SYM)
  (cond
    ( ( null? L ) 0 )
    ( ( equal? ( car L ) SYM ) ( + 1 ( count-sym ( cdr L ) SYM ) ) )
    ( else ( count-sym ( cdr L ) SYM ) )
  )
)

( define ( counts XS )
  ( cond
    ( ( null? XS ) '() )
    ( else ( cons
             ( list ( car XS ) ( count-sym XS ( car XS ) ) )
             ( counts ( filter ( lambda ( x ) ( not ( equal? ( car XS ) x) ) )
                               XS ) ) ) )
  ) 
)

( define ( indices L1 X )
  ( map ( lambda ( x ) ( + 1 x ) )
        ( cond
          ( ( null? L1 ) '() )
          ( ( equal? X ( car L1 ) ) ( cons -1
                                           ( indices ( cdr L1 ) X ) ) )
          ( else ( indices ( cdr L1 ) X ) )
         )
  )
)

( define ( join-together L1 L2 )
  ( cond
     ( ( and ( null? L1 ) ( null? L2 ) ) '() )
     ( ( null? L1 )  ( cons ( car L2 ) ( cdr L2 ) ) )
     ( ( null? L2 )  ( cons ( car L1 ) ( cdr L1 ) ) )
     ( ( < ( car L1 ) ( car L2 ) ) ( cons ( car L1 )
                                          ( join-together ( cdr L1 ) L2 ) )
     )

     ( ( > ( car L1 ) ( car L2 ) ) ( cons ( car L2 )
                                          ( join-together L1 ( cdr L2 ) ) )
     )
  )
)

;;;splits the list L1 using a procedure ( f L1 N )
( define ( split f L1)
 ( f L1 ( round  ( / ( length L1 ) 2 ) ) )
)

( define ( merge-sorter L1 )
  (cond
    ( ( null? L1 ) '() )
    ( ( < ( length L1 ) 2 ) L1 )
    ( else ( join-together ( merge-sorter ( split first-n L1 ) )
                           ( merge-sorter ( split forget-n L1 ) ) ) )
  )
)

;;;creates a list of all nodes from an el-graph g
( define (decompose g)
   ( cond
      ( ( null?  g ) '() )
      ( else ( cons ( caar g ) ( cons ( cadar g ) ( decompose ( cdr g ) ) ) ) )
   )
)

;;;creates x-graph using a list of nodes L1 and a graph g
( define ( srcs-targets L1 g )
   ( cond
      ( ( null? L1 ) '() )
      ( else ( cons ( list ( car L1 ) ( edges ( car L1 ) g ) )
                     (srcs-targets ( filter
                                    ( lambda (x) ( not ( equal? ( car L1 ) x) ) )
                                     L1 ) g) ) )
   )
)

;;;finds all edges for a given symbol VERT in el-graph g 
( define ( edges VERT g )
   ( cond
      ( ( null? g ) '() )
      ( ( equal? ( caar g ) VERT ) ( cons ( cadar g ) ( edges VERT ( cdr g ) ) ) )
      ( else ( edges VERT ( cdr g ) ) )
   )
)

(define ( el-graph->x-graph g )
  ( srcs-targets ( decompose g ) g )
)

( define ( x-graph->el-graph g )
  ( cond
     ( ( null? g ) '() )
     ( else ( append ( map ( lambda ( target ) ( list ( caar g ) target ) )
                           ( cadar g ) ) ( x-graph->el-graph ( cdr g ) ) ) )
   )
)

(provide interleave key-store list-replace first-n forget-n running-sum counts indices join-together merge-sorter el-graph->x-graph x-graph->el-graph)