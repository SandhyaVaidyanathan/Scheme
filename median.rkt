;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname mediansample) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;Sandhya Vaidyanathan
;cs4250 hw5
;04/26/2016


;Calculate the number of elements in a list
(define (count list)
  (if (null? list)
      0
      (+ 1 (count (cdr list)))
  )
) 

;Calculate the position for the median in an odd list
(define(odd_count list)
    (+(/(- (count list) 1) 2)1)
    )

;Calculate the sum of all the elements in the list
(define (total list)
  (if(null? list)
     0
     (+ (car list) (total (cdr list)))
     )
  )

;Calculate the average for the list
(define (avg list)
  (/ (total list) (count list))
  )

;Calculate the position for the 1st middle element in an even list
(define (even_count1 list)
  (/(count list)2)
  )

;Calculate the position for the 2nd middle element in an even list
(define (even_count2 list)
  ( +(/(count list)2) 1)
  )

;Select the median for the even list
(define (compare list x y)
(cond
  ((<(abs(-(avg list)x)) (abs(-(avg list)y))) x)
  (else y )))

;Display the element at a specific position 'a' from list
(define show (lambda (list a) 
      (if (eq? a 1) 
          (car list) 
          (show (cdr list) (- a 1)))))

;Returns the value of 1st middle element (even list)
(define (even1 list) 
  (show list (even_count1 list))
  )

;Returns the value of 2nd middle element (even list)
(define (even2 list) 
  (show list (even_count2 list))
  ) 

;Returns median for an even list
(define (even_median list)
  (compare list (even1 list) (even2 list)))

;Returns median for an odd list
(define (odd_median list)
   (show list (odd_count list))
  )

;Find the smallest from the list
(define (smallest list atm)     
  (cond
    ( (null? list) atm)
    ( (< (car list) atm) (smallest (cdr list)(car list)))
     (else
      (smallest (cdr list) atm ))
  ))

;New list after removing the smallest element 'atm'
(define (Newls list atm)                
  (cond
    ( (null? list) '() )           
    ( (= (car list) atm) (cdr list))    
    (else
     (cons (car list)(Newls (cdr list) atm)))  
  ))

;Returns the sorted list is ascending order 
(define (sortfn  list) 
   (cond
     ( (null? list) '() )
     ( else
       (cons (smallest list (car list))     
                      (sortfn (Newls list (smallest list (car list)))))                          
      )
  ))

; Find the median for odd or even lists from the sorted list
(define (median list)
  (cond
    ((even? (count list)) (even_median (sortfn list)) )
      ((odd? (count list)) (odd_median (sortfn list)))
      ( (=(count list) 0) `())
      )  
  )