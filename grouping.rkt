#lang racket

;reading students data from a file
(include "student-sample.rkt")
(display student-data)

;constructor for a single group
(define  (create-group group-id st-ids)
  (cons group-id st-ids  ))

;test
(create-group 1 '("20204988" "20206444" "20201934" "20207648" "20202890" "20202610" ))




;selector function that returns the group-id
(define (get-group-id group )
  (car group ))

;test
(display "group id or group number:\n ")
(get-group-id '(1 "20204988" "20204882"))




;selector function that returns the list of students from the group

(define (get-student-ids group)
  (cdr group))

(display "list of students from the group: \n")
;test
(get-student-ids '(1 "20204988" "20204882")) 



; group predicate function to validate below conditions:
; There should be minimum 5 students and maximum 6 students for each group.
; there should be a group-id for each group
; group-id has to be an integer
;group-id should be positive number that is greater than 0.

(define (is-group2? group)
  (if (pair? group)
      ; pair true
      (if (and (>= (length (get-student-ids group ))  5) (<=(length(get-student-ids group)) 6))
          ; group length true
          #t
          ; group length false
          #f)
      ; pair false
      #f)
)
(define (is-group? group)
  (cond
    [(not (pair? group)) #f]
    [(not (and (>= (length (get-student-ids group)) 5) (<= (length (get-student-ids group)) 6))) #f]
    [(not (number? (get-group-id group))) #f]
    [(not (positive? (get-group-id group)))#f]
    [else #t]
    )
  )
; test
(is-group? 1) ; #f
(is-group? '("702" "123" "234")) ; #f
(is-group? '(1)) ; #f
(is-group? '(1 "123" "234" "456" "567" "678")) ; #t
(is-group? '(-1 "123" "234" "444" "555" "666")) ; #f
(is-group? '(100 "123" "234" "345" "456" "567" "678")) ; #t


;student properties.

(define (create-student id name sex ethnicity age);constructor function for students details
  (list id name sex ethnicity age))

(define (get-student-id student-details)
  (car student-details))

(define (get-student-name student-details)
  (car (cdr student-details)))

(define (get-student-gender student-details)
  (car (cdr (cdr student-details))))

(define (get-student-ethnicity student-details)
  (car (cdr (cdr (cdr student-details)))))

(define (get-student-age student-details)
   (car (cdr (cdr (cdr (cdr student-details))))))



;Type predicate for single student
(define (student? student)
  (cond
    [(not (list? student)) #f]
    [(not (= (length student) 5)) #f]
    [(not (string? (get-student-id student))) #f]
    [(not (string? (get-student-name student))) #f]
    [(not (string? (get-student-gender student))) #f]
    [(not (string? (get-student-ethnicity student))) #f]
    [(not (number? (get-student-age student))) #f]
    [else #t]))

; test tpe predicate
(display "student type predicate #t #f #f\n")
(student? (create-student "1" "Kereena" "female" "Indian" 23))
(student? (list 2 "Kereena" "female" "Indian" 23))
(student? (list "Kereena"))



; Pretty-print functions for student and group


;pretty print function for single student details.
(define (student-print student)
  
      (begin
        (printf "---------------------------------------------------------~n")
        (printf "       id: ~s~n" (get-student-id student))
        (printf "     name: ~s~n" (get-student-name student))
        (printf "      sex: ~s~n" (get-student-gender student))
        (printf "ethnicity: ~s~n" (get-student-ethnicity student))
        (printf "      age: ~s~n" (get-student-age student))
        (printf "---------------------------------------------------------~n"))
      (printf "    student: ~s~n" student))


;test to display student details
(for-each student-print(take student-data 3))

;test
(let ([my-student (create-student "1" "Raaji" "female" "Indian" 25)])
  (student-print my-student))




;Selecting a group from Grouping
(define (get-group grouping group-id)
  
  (create-group group-id 
              (map cdr (filter (lambda (stud-group-assoc)
                                (equal? (car stud-group-assoc)
                                   group-id)) grouping))))                  
  
(display "get-group\n ") 
;test
(get-group '((1 . "20206444") (1 . "20202610") (2 . "20201934") (1 . "20204988") (2 . "20202890")) 2)



; Function that returns the number of groups from a grouping associations.

(define (groups-from-grouping grouping) ; get the groups from grouping associations.
  (map car grouping))

(define (unique-groups-from-grouping grouping unique-groups)

  (if (empty? grouping)
      unique-groups
       
      (let([current-group(car grouping)] 
           [remaining-groups(cdr grouping)])

        (if 
         (member current-group remaining-groups);
         (unique-groups-from-grouping remaining-groups unique-groups);dont-append 
         (unique-groups-from-grouping remaining-groups (append unique-groups (list current-group))))))); unique group growing here :)  
           

;function to return number of groups from association of groups
(define (nof-groups grouping )
  (length (unique-groups-from-grouping (groups-from-grouping grouping) '())))


;test
(nof-groups '((1 . "20206444") (1 . "20202610") (2 . "20201934") (1 . "20204988") (2 . "20202890")))

;function to return maximum and minimum group size from the grouping associations

(define (group-size grouping predicate)
  (let ([group-ids (unique-groups-from-grouping (groups-from-grouping grouping) '())])
    (apply predicate (map (lambda (group-id) (length (get-student-ids (get-group grouping group-id)))) group-ids))))

(display "The maximum group size of grouping is \n")
;test 
(group-size '((1 . "20206444") (1 . "20202610") (2 . "20201934") (1 . "20204988") (2 . "20202890")) max)
(display "The manimum group size of grouping is \n")
;test 
(group-size '((1 . "20206444") (1 . "20202610") (2 . "20201934") (1 . "20204988") (2 . "20202890")) min)


;function that asserts group sizes gsl have positive integer.
(define( group-sizes-positive? gsl )

  (if (empty? gsl)
      #t
      (if (not (positive? (car gsl)))
          #f
          (group-sizes-positive? (cdr gsl)))))
  
(display "group-size assertion of positive integers\n" )
;test
(group-sizes-positive? '(5 6 4 5))
(group-sizes-positive? '(5 6 -1 5))


;Function for the sum of group-sizes is equal to the number of students.
(define (sum-of-gsl-equals-to-sl gsl sl)
  (equal? (apply + gsl) (length sl))
)

(display "group sizes and number of students of groups sizes are equal\n ")
;test
(sum-of-gsl-equals-to-sl '(1 2) '("student a" "student b" "student c"))
(sum-of-gsl-equals-to-sl '(1 2) '("student a" "student b"))

; function for Random grouping
(define (random-group-sl  gsl sl current-group-id)
  (if (empty? gsl)
      '() ; no more groups to form
      (let
          ([shuffled-sl (shuffle sl)] ; use shuffle to randomize student list
           [group-size (car gsl)])
        (let
            ([student-group (take shuffled-sl group-size)]  ; take group-size number of students from shuffled student list
             [remaining-students (drop shuffled-sl group-size)]) ; remaining students
         (append (list (create-group current-group-id student-group))   ; append new group
                 (random-group-sl (cdr gsl) remaining-students (+ 1 current-group-id))) ; .. to recursive solution for remaining students 
          ))))  

;test
(display "random grouping\n")
(define my-gsl '(5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5)) ; define sizes of groups, 40x5 = 200 (size of student-data)
(random-group-sl my-gsl student-data 1)


; Grouping from list of groups.

(define (grouping-from-grplist group-list)
  (if (empty? group-list)
      '()
      (let ([current-group (car group-list)])
        (let ([group-id (car current-group)]
              [stud-list-from-group (cdr current-group)])
          (append (map (lambda (student-id) (cons group-id student-id)) stud-list-from-group)
                  (grouping-from-grplist (cdr group-list)))))))

;test
(display "grouping from list of groups\n " )
(grouping-from-grplist '((1 "a" "b")(2 "c") (3 "d" "f")))



; find a student detail by id from student list sl, recursive solution
(define (find-student-by-id sl id)
  (if (empty? sl)
      #f
      (let ([current-student (car sl)])
        (if (equal? id (get-student-id current-student))
            current-student
            (find-student-by-id (cdr sl) id)))))

; test find student by id in student data
(display "find student 20209775 by id in student data\n")
(find-student-by-id student-data "20209775")


;grouping to group list
(define (grouping-to-grouplist grouping sl)
  (let ([group-ids (unique-groups-from-grouping (groups-from-grouping grouping) '())]) ; find groups in grouping
    (let ([grouping-with-students (map
                                   (lambda (grp-stud-assoc) (cons (car grp-stud-assoc)  ; creates '(grp-id . ("stud-id" "name" "gender" "eth" age)) 
                                                                  (find-student-by-id sl (cdr grp-stud-assoc))))
                                   grouping)]) ; adds student details to grouping element
      (map (lambda (grp-id)
             (get-group grouping-with-students grp-id)) group-ids)) ; for each group-id create group 
  ))


; program for function to create grouping by counting
(define (grouping-by-counting student-ids  current-group-id  k-groups);here k-groups mean number of groups
  (if (empty? student-ids)
      '()  ; no more students to group
      (let
          ([current-student-id (car student-ids)])
        (append (list (cons current-group-id current-student-id))
                (grouping-by-counting (cdr student-ids); grouping-by-counting procedure finds the remaining group.
                                      (if (= k-groups current-group-id )
                                          1 ; resets the current-group-id to 1 if it after it reaching maximum number of groups
                                          (+ 1 current-group-id)) k-groups))))) 
                      
;test for grouping by counting
(display "grouping by counting (form 15 groups)\n")
(grouping-to-grouplist (grouping-by-counting (map get-student-id student-data) 1 15) student-data)
 
    


; sorting key generator, produces a key which is combined from ethnicity,gender for each student (used for balanced counting)
(define (sort-key-ethnic-gender student)
  (let ([stud-eth (get-student-ethnicity student)] ; extract gender and eth from student
        [stud-gender (get-student-gender student)])
    (string-append stud-eth "," stud-gender))) ; combine stud eth and stud gender

; Balanced grouping by counting
; (the "pragmatic" solution is to sort the students based on ethnicity and sex, then do normal counting)
(define (balanced-grouping-by-counting students k)
  (grouping-by-counting (map get-student-id (sort students string<? #:key sort-key-ethnic-gender)) 1 k)) ; sort and use "normal" grouping by counting

;test
(display "grouping by balanced counting (form 30 groups)\n ")
(grouping-to-grouplist (balanced-grouping-by-counting student-data 30) student-data)



; Random grouping with group predicate
; makes use of shuffle function for randomness
(define (random-grouping-predicate  gsl sl current-group-id group-predicate? rejected-cand-groups)
  (if (or (empty? gsl)(= rejected-cand-groups 1000))  ; if 1000 groups are rejected, we return no more groups are formed
      '()
      (let
          ([shuffled-sl (shuffle sl)]  ; randomize student list
           [group-size (car gsl)])
        (let
            ([candidate-students (take shuffled-sl group-size)]   ; take first "group-size" students from shuffled student list
             [remaining-students (drop shuffled-sl group-size)])  ; remaining students from shuffled student list
          (if (group-predicate? candidate-students)  ; is group predicate satisfied for candidate students?
              (append (list (create-group current-group-id candidate-students))  ; yes, then append to solution
                 (random-grouping-predicate (cdr gsl) remaining-students (+ 1 current-group-id) group-predicate? 1))
              (random-grouping-predicate gsl sl current-group-id group-predicate? (+ 1 rejected-cand-groups))  ; no, then recursively try grouping again
              ; ... but mention we have one more rejected candidate group
              
          ) ) )))

;Predicate check if all students are females in the group

(define (all-female? candidate-students)
  (let ([all-genders (map get-student-gender candidate-students)])
    (let([female-genders (filter (lambda (gender) (equal? "female" gender)) all-genders)])
      (equal? (length all-genders ) (length female-genders)
      ))))
      
  ;test1
(display "Test of all-female? should be #f #t #f\n")
(all-female? (take student-data 10))

(all-female? (list
              (create-student "1" "Raaji" "female" "Indian" 25)
              (create-student "2" "Tanja" "female" "Danish" 25)))

;test2
(all-female? (list
              (create-student "1" "Raja" "male" "Indian" 25)
              (create-student "2" "Inger" "female" "Danish" 25)))

;test for random grouping predicate with all female-predicate
(display "created groups with fimale only\n" )
(random-grouping-predicate my-gsl student-data 1 all-female? 1)


; predicate for at least n students of minimum age a
(define (nof-stud-minage? n a)
  (lambda (candidate-students) ; creates another procedure that takes candidate students
    (let ([candidate-students-ages (map get-student-age candidate-students)])
      (let ([candidate-students-with-minage-a (filter (lambda (cand-stud-age) (>= cand-stud-age a)) candidate-students-ages)])
        (>=
         (length candidate-students-with-minage-a)  ; number of students with minage a
         n)))))


;test2
((nof-stud-minage? 2 25) (list
                          (create-student "1" "Raja" "male" "Indian" 25)
                          (create-student "2" "Inger" "female" "Danish" 20)))

;test3
(display "created groups 2 students 25 or older\n" )
(random-grouping-predicate my-gsl student-data 1 (nof-stud-minage? 2 25) 1)



; Remove dulicates from list.

(define (unique-list lst )
  (if (empty? lst)
      '()
      (let([current-element (car lst)]
           [remaining-list (cdr lst)])
        (if 
         (member current-element remaining-list);
         (unique-list remaining-list );dont-append 
         (append (list current-element) (unique-list remaining-list))))))

; test
(display "Uniques list that removes duplicates\n ")
(unique-list '(1 2 3 3 4 5 5 ))

;Predicate that accepts students with different ages

(define (age-diversity? candidate-students)
  (let ([candidates-all-ages (map get-student-age candidate-students)]) 
    (let ([unique-candidate-ages (unique-list candidates-all-ages)])
      (= (length candidates-all-ages  ) (length unique-candidate-ages)))))

; test1
(age-diversity? (list
                          (create-student "1" "Raja" "male" "Indian" 25)
                          (create-student "2" "Susie" "female" "Danish" 20)))

;test2
(display "created groups with age diversity\n" )
(random-grouping-predicate my-gsl student-data 1 age-diversity? 1)

;


; type predicate for an element in a grouping
(define (grouping-element? grouping-element)
  (cond [(not (pair? grouping-element)) #f]
        [(not (number? (car grouping-element))) #f]
        [(not (positive? (car grouping-element))) #f]
        [(not (string? (cdr grouping-element))) #f]
        [else #t]))

; recursive function to check that all elements in lsg are grouping-elements?
(define (all-grouping-elements? lst)
  (if (empty? lst)
      #t
      (if (not (grouping-element? (car lst)))
          #f
          (all-grouping-elements? (cdr lst)))))

; type predicate for grouping
(define (grouping? grouping)
  (cond [(not (list? grouping)) #f]
        [(< (length grouping) 1) #f]
        [(not (all-grouping-elements? grouping)) #f]
        [else #t]))

; test grouping predicate
(display "grouping? predicate, shoudl be #t #f #f #f #f\n")
(grouping? (list (cons 1 "a") (cons 1 "b") (cons 1 "c")))
(grouping? "hello")  ; false
(grouping? 1)  ; false
(grouping? '())  ; false
(grouping? (list (cons -1 "a")))
(grouping? (list (cons "a" "b")))





