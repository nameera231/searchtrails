#lang racket
(require csc151)

;format of original file:
;  '(trail_id "name" "area_name" "city_name" "state_name" "country_name" "{'lat': num, 'lng': num}" popularity length elevation_gain difficulty_rating "route_type" visitor_usage avg_rating num_reviews "['features']" "['activities']" "units\r")
(define alltrailslst (read-csv-file "/home/cooperma/Documents/nationalparktrails.csv"))

;;;Procedure
;;;   editlst
;;;Parameters
;;;   lst, a list containing one line of trail information from alltrailslst
;;;Purpose
;;;   to remove the 6th, 8th and 18th element of lst.
;;;   Used to clean dataset (alltrailslst)
;;;Produces
;;;   shortlst, a list containing all the same values as lst except with the 6th, 8th and 18th elements removed
(define editlst
  (lambda (traillst)
    (let* ([lst1 (append (take traillst 5) (drop traillst 6))]
           [lst2 (append (take lst1 6) (drop lst1 7))])
      (take lst2 15))))

;;;Procedure
;;;   clean-line
;;;Parameters
;;;   lst, a list containing one line of trail information from alltrailslst
;;;Purpose
;;;   transforms the string containing latitude and longitude into a list of containing the numerical value for each
;;;   transforms the string containing trail features into a list of individual strings, one for each feature
;;;   transforms the string containing trail activities into a list of individual strings, one for each activity
;;;   Used to clean dataset (alltrailslst)
;;;Produces
;;;   cleanlst, a list with all the same values as lst except the latitude & longitude, features, and activity information are all in list format
(define clean-line
  (lambda (lst)
    (let ([listvec (list->vector lst)])
      (vector-set! listvec 6                    ;turns latitude and longitude values into a list of two numbers
                   (map string->number
                        (string-split
                         (list->string
                          (remove* (list #\{ #\} #\' #\: #\l #\a #\t #\n #\g #\,)
                                   (string->list (list-ref lst 6)))))))
      (vector-set! listvec 15                   ;turns features into a list of strings
                   (string-split
                    (list->string
                     (remove* (list #\[ #\] #\, #\')
                              (string->list (list-ref lst 15))))))
      (vector-set! listvec 16                   ;turns activities into a list of strings
                   (string-split
                    (list->string
                     (remove* (list #\[ #\] #\, #\')
                              (string->list (list-ref lst 16))))))
      (vector->list listvec))))

;format of new, cleaned file:
;  '(trail_id "name" "area_name" "city_name" "state_name" '( latitude longitude) length elevation_gain difficulty_rating "route_type" visitor_usage avg_rating num_reviews '("features") '("activities") )
(define cleantrails (filter (negate null?) (drop (map editlst (map clean-line alltrailslst)) 1)))
(define sample (take cleantrails 5))
(define zipcodes (read-csv-file "/home/cooperma/Documents/us-zip-codes.csv"))


;anna
(define input-vec (make-vector 9 "x"))
;input-vec format: zip, max-radius, length, diff, traf, rating, type, features, activities
;;;Procedure
;;;   trail-input
;;;Parameters
;;;   none
;;;Purpose
;;;   to allow the user to input their desired specifications for their trail
;;;Produces
;;;   absolutely nothing; result is a side-effect on input-vec
(define trail-input
  (lambda ()
    (display "Please enter your Zip Code: ")
    (display #\newline)
    (define correctzip
      (lambda ()
        (let ([zip (read)])
          (cond [(and (integer? zip)
                      (real? zip)
                      (assoc (number->string zip) zipcodes))
                 (vector-set! input-vec 0 (number->string zip))]
                [else
                 (display "input is in an incorrect format. Please re-enter your input in the indicated format: ")
                 (display #\newline)
                 (correctzip)]))))
    (correctzip)
    (display "Please enter your maximum desired travel distance: (in miles)")
    (display #\newline)
    (define correct-max-radius
      (lambda ()
        (let ([max-radius (read)])
          (cond [(and (number? max-radius)
                      (real? max-radius))
                 (vector-set! input-vec 1 max-radius)]
                [else
                 (display "input is in an incorrect format. Please re-enter your input in the indicated format: ")
                 (display #\newline)
                 (correct-max-radius)]))))
    (correct-max-radius)
    (display "How long would you like your trail to be?: (in miles)")
    (display #\newline)
    (define correct-length
      (lambda ()
        (let ([length (read)])
          (cond [(and (number? length)
                      (real? length))
                 (vector-set! input-vec 2 (* length 1609.34))]
                [else
                 (display "input is in an incorrect format. Please re-enter your input in the indicated format: ")
                 (display #\newline)
                 (correct-length)]))))
    (correct-length)
    (display "if you have no preference for one of the following variables, please enter NA in the form of a string")
    (display #\newline)
    (display "How difficult would you like your trail to be?: (please enter a number 1-5, 5 being the most difficult)")
    (display #\newline)
    (define correct-diff
      (lambda ()
        (let ([diff (read)])
          (cond [(or (and (string? diff)
                          (string-ci=? diff "na"))  
                     (and (number? diff)
                          (real? diff)
                          (>= diff 1)
                          (<= diff 5)))
                 (vector-set! input-vec 3 diff)]
                [else
                 (display "input is in an incorrect format. Please re-enter your input in the indicated format: ")
                 (display #\newline)
                 (correct-diff)]))))
    (correct-diff)
    (display "How heavily trafficked would you like your trail to be?: High, Medium, or Low (please enter one in the form of a string)")
    (display #\newline)
    (define correct-traf
      (lambda ()
        (let ([traf (read)])
          (cond [(or (and (string? traf)
                          (string-ci=? traf "na"))
                     (and (string? traf)
                          (or (string-ci=? traf "high")    
                              (string-ci=? traf "medium")
                              (string-ci=? traf "low"))))
                 (vector-set! input-vec 4 (cond [(string-ci=? traf "high")
                                                 3]
                                                [(string-ci=? traf "medium")
                                                 2]
                                                [else
                                                 1]))]
                [else
                 (display "input is in an incorrect format. Please re-enter your input in the indicated format: ")
                 (display #\newline)
                 (correct-traf)]))))
    (correct-traf)
    (display "How highly rated would you like your trail to be?: (please enter a number 1-5)")
    (display #\newline)
    (define correct-rating
      (lambda ()
        (let ([rating (read)])
          (cond [(or (and (string? rating)
                          (string-ci=? rating "na"))
                     (and (number? rating)
                          (real? rating)
                          (and (>= rating 1)
                               (<= rating 5))))
                 (vector-set! input-vec 5 rating)]
                [else
                 (display "input is in an incorrect format. Please re-enter your input in the indicated format: ")
                 (display #\newline)
                 (correct-rating)]))))
    (correct-rating)
    (display "What type of trail would you like?: Loop, Out and Back, or Point to Point (please enter one in the form of a string)")
    (display #\newline)
    (define correct-type
      (lambda ()
        (let ([type (read)])
          (cond [(or (and (string? type)
                          (string-ci=? type "na"))
                     (and (string? type)
                          (or (string-ci=? type "loop")
                              (string-ci=? type "out and back")
                              (string-ci=? type "point to point"))))
                 (vector-set! input-vec 6 type)]
                [else
                 (display "input is in an incorrect format. Please re-enter your input in the indicated format: ")
                 (display #\newline)
                 (correct-type)]))))
    (correct-type)
    (display "What features would you like your trail to have?: (please enter one of the following features in the form of a single string) ")
    (display (list "views" "dogs-no" "wildlife" "wild-flowers" "forest" "kids" "river" "lake" "waterfall" "dogs-leash" "beach" "ada" "partially-paved" "historic-site" "strollers" "hot-springs" "paved" "cave" "dogs" "rails-trails" "city-walk"))
    (display #\newline)
    (define correct-features
      (lambda ()
        (let ([features (read)])
          (cond [(or (and (string? features)
                          (string-ci=? features "na"))
                     (string? features))
                 (vector-set! input-vec 7 (string-split features))]
                [else
                 (display "input is in an incorrect format. Please re-enter your input in the indicated format: ")
                 (display #\newline)
                 (correct-features)]))))
    (correct-features)
    (display "What activities would you like to be available on your trail?: (please enter activities in the form of a single string) ")
    (display (list "hiking" "nature-trips" "birding" "walking" "trail-running" "backpacking" "camping" "horseback-riding" "fishing" "snowshoeing" "rock-climbing" "mountain-biking" "fly-fishing" "scenic-driving" "off-road-driving" "cross-country-skiing" "canoeing" "road-biking" "paddle-sports" "whitewater-kayaking" "skiing" "snowboarding" "bike-touring" "sea-kayaking" "ice-climbing" "rails-trails" "surfing"))
    (display #\newline)
    (define correct-acts
      (lambda ()
        (let ([acts (read)])
          (cond [(or (and (string? acts)
                          (string-ci=? acts "na"))
                     (string? acts))
                 (vector-set! input-vec 8 (string-split acts))]
                [else
                 (display "input is in an incorrect format. Please re-enter your input in the indicated format: ")
                 (display #\newline)
                 (correct-acts)]))))
    (correct-acts)))


(define sample2 (drop (take cleantrails 2953) 2941)) ;these are some trails in the seattle area (zipcode 98105), about 120 miles away. I used them for testing and it seemed to work fine

;;;Procedure:
;;;   limit-distance
;;;Parameters:
;;;   zip, a zip code in the form of a string. Provided by user.
;;;   max-radius, the maximum desired radius in miles in the form of a real number. Provided by user.
;;;Purpose:
;;;   filters cleantrails (a table of trail information) based whether or not a given trail fall within max-radius of zip
;;;Produces:
;;;   trails-within-radius, a table of only the trails that fall within max-radius, based on max-radius and latitude and longitude data acquired from zip
;;;Preconditions:
;;;   zip must be contained within zipcodes
;;;   cleantrails must be defined as a table of trail information
;;;Postconditions:
;;;   if t is a list describing a single trail and the distance from zip to t is less than max-radius, t is contained within trails-within-radius
;;;   length of trail-within-radius <= length of cleantrails
(define limit-distance
  (lambda (zip max-radius)
    (let* ([entry (assoc zip zipcodes)]  ;zipcodes is (read-csv-file "/Users/(name)/us-zip-codes.csv"))
           [lat1 (cadr entry)]
           [long1 (caddr entry)])
      (letrec ([kernel
                (lambda (filtered-list old-list lat2 long2)
                  (let ([dmileslat (* 69 (abs (- lat1 lat2)))]
                        [dmileslong (* 54.6 (abs (- long1 long2)))])
                    (cond [(null? (cdr old-list))
                           (if (< (+ (square dmileslat) (square dmileslong)) (square max-radius))
                               (cons (car old-list) filtered-list)
                               filtered-list)]
                          [(< (+ (square dmileslat) (square dmileslong)) (square max-radius))
                           (kernel (cons (car old-list) filtered-list) (cdr old-list) (car (list-ref (cadr old-list) 5)) (cadr (list-ref (cadr old-list) 5)))]
                          [else
                           (kernel filtered-list (cdr old-list) (car (list-ref (cadr old-list) 5))  (cadr (list-ref (cadr old-list) 5)))])))])
        (kernel null cleantrails  (car (list-ref (car cleantrails) 5)) (cadr (list-ref (car cleantrails) 5)))      ;cleantrails is the name of a static table containing the cleaned trail data                       
        ))))  

;;;Procedure
;;;   assignlengthscore
;;;Parameters
;;;   traillst, the cleaned table of trail information
;;;   length, the user-input for desired length of trail (can be a number in miles or the string "NA")
;;;Purpose
;;;   Changes score of each trail based on how well close it is to the user's desired length of trail
;;;Produces
;;;   nothing, called for side effect
;;;Preconditions
;;;   only real number inputs for length will produce changes to trail scores
;;;Postconditions
;;; It will construct an element infront of traillst that contains score based on how close the length of each trail is to length (the parameter).
(define assignlengthscore
  (lambda (traillst length)
    (let kernel ([newtraillst (list)]
                 [trailremaining traillst])
      (if (null? trailremaining)
          newtraillst
          (let* ([ thislen (list-ref ( car trailremaining) 6)]
                 [ percentlen (/ thislen length)]
                 [ percentdiff ( if (<= percentlen 1) (- 1 percentlen) ( - percentlen 1))]
                 [ scaleddiff ( - 1000 (* 100 percentdiff))]
                 )
            (kernel (cons  (cons scaleddiff (car trailremaining)) newtraillst) (cdr trailremaining))))
      )
    ))


;;;Procedure
;;;   assigntrafficscore
;;;Parameters
;;;   traillst, the cleaned table of trail information
;;;   traf, the user-input for desired traffic of trail in the form of a string
;;;Purpose
;;;   Changes score of each trail based on how well it fits to the user's desired traffic
;;;Produces
;;;   nothing, called for side effect
;;;Preconditions
;;;   only "high", medium", "low", or "na" can be possible values for traf.
;;;Postconditions
;;;   any trail with that has traffic equal to input "traf" will have 100 added to its score
;;;   if traf is "na" it will have no effect on resulting score
(define assigntrafficscore
  (lambda (traillst traf)
    (let ([trailvec (map list->vector traillst)])
      (letrec ([kernel
                (lambda (newtrails trailvec traf)
                  (let* ([trailtraf (if (number? (vector-ref (car trailvec) 11))
                                        (vector-ref (car trailvec) 11)
                                        0)])
                    (cond [(null? (cdr trailvec))
                           (cond [(= traf trailtraf)
                                  (vector-set! (car trailvec) 0 (+ (vector-ref (car trailvec) 0) 100))
                                  (map vector->list (cons (car trailvec) newtrails))]
                                 [else
                                  (map vector->list (cons (car trailvec) newtrails))])]
                          [(= traf trailtraf)
                           (vector-set! (car trailvec) 0 (+ (vector-ref (car trailvec) 0) 100))
                           (kernel (cons (car trailvec) newtrails) (cdr trailvec) traf)]
                          [else
                           (kernel (cons (car trailvec) newtrails) (cdr trailvec) traf)])))])
        (kernel null trailvec traf)))))

;;;Procedure:
;;;   makestringlist
;;;Parameters:
;;;   trailtble, a list of lists containing trail information
;;;   index, a positive integer number 
;;;Purpose:
;;;   to create a list of all the possible strings in each list of trailtable at index
;;;   written to determine all the possible activities and features in cleantrails
;;;Produces:
;;;   lst, a list of strings
;;;Preconditions:
;;;   each of the lists within traillst will contain a list of strings at index
;;;   index < length of the lists within trailtble
;;;Postconditions:
;;;   if "a" is a string in the list of strings at index in one of the trails within trailtble, then "a" is contained within lst
;;;   no two strings within lst will be the same, ignoring case (that is, string-ci=? will be false for every pair of strings in lst)
;;;   the strings within lst will be sorted from most common to least common based on how often they occur at within trailtble (at index)
(define makestringlist
  (lambda (trailtble index)
    (letrec ([allstringslst (lambda (trails-remaining stringlst)
                              (if (null? (cdr trails-remaining))
                                  (append (list-ref (car trails-remaining) index) stringlst)
                                  (allstringslst (cdr trails-remaining) (append (list-ref (car trails-remaining) index) stringlst))))]
             [tally->string (lambda (tally)
                              (list-ref tally 0))]
             [sorttallies>= (lambda (tally1 tally2)
                              (>= (list-ref tally1 1) (list-ref tally2 1)))]
             )
      (append (map tally->string (sort (tally-all (allstringslst trailtble null)) sorttallies>=)))
      )
    ))

;Fun fact: featureslst and activitylst are ordered from most common to least common
(define featureslst (makestringlist cleantrails 13))
;> featureslst
;'("views" "dogs-no" "wildlife" "wild-flowers" "forest" "kids" "river" "lake" "waterfall" "dogs-leash" "beach" "ada" "partially-paved" "historic-site" "strollers" "hot-springs" "paved" "cave" "dogs" "rails-trails" "city-walk")
(define activitylst (makestringlist cleantrails 14))
; activitylst
;'("hiking" "nature-trips" "birding" "walking" "trail-running" "backpacking" "camping" "horseback-riding" "fishing" "snowshoeing" "rock-climbing" "mountain-biking" "fly-fishing" "scenic-driving" "off-road-driving" "cross-country-skiing" "canoeing" "road-biking" "paddle-sports" "whitewater-kayaking" "skiing" "snowboarding" "bike-touring" "sea-kayaking" "ice-climbing" "rails-trails" "surfing")

;;;Procedure:
;;;   assignfeaturescore
;;;Parameters:
;;;   traillst, a list of lists containing trail information
;;;   features, a list of strings provided by the user where each string is a desired feature
;;;Purpose:
;;;   to modify the score of each trail within traillst based on how many of the desired features that trail has
;;;      (containing one or more of the desired features will cause the score of a trail to be increased)
;;;Produces:
;;;   newtraillst, a list of lists containing all the same trail information as traillst but with the score modified
;;;Preconditions:
;;;   each of the lists within traillst will contain:
;;;      a integer number as the first element, representing the score for that trail
;;;      a list of strings describing the features of that trail as the 15th element.
;;;   neither traillst or features can be null
;;;Postconditions:
;;;   if "a" is a desired feature contained within features and also within the feature list at the 15th element of a list within traillst,
;;;      the score of that trail will be increased by 100
;;;   if n is the initial score of a trail in traillst, and m is the score of that same trail after assignfeaturescore is called, n<=m
;;;   length of traillst = length of newtraillst
;;;   the length of the lists within traillst will remain unchanged
(define assignfeaturescore
  (lambda (traillst features)
    (let ([trailvec (map list->vector traillst)])
      (letrec ([altor (lambda (bool1 bool2)                                   ;created to use in combination with reduce, so that boolean lists can be reduced
                        (or bool1 bool2))]
               [featurematch? (lambda (features-remaining)                    ;first test to see if any of the features given by user are actually possible features. Meant to save time
                                (cond [(null? features-remaining)
                                       #f]
                                      [(reduce altor (map (section string-ci=? (car features-remaining) <>) featureslst))
                                       #t]
                                      [else 
                                       (featurematch? (cdr features-remaining))]
                                      ))]
               [find-num-matches (lambda (matches-so-far features-remaining)  ;takes a single list of features from trailvec and finds the total number of matches to features
                                   (cond [(null? features-remaining)
                                          matches-so-far]
                                         [(reduce altor (map (section string-ci=? (car features-remaining) <>) features))
                                          (find-num-matches (+ 1 matches-so-far) (cdr features-remaining))]
                                         [else
                                          (find-num-matches matches-so-far (cdr features-remaining))]
                                         ))]
               [kernel (lambda (newtrails trailvec-remaining)                  ;recursive procedure that uses find-num-matches to add 100 to the existing score of each trail in trailvec each time a feature of that trail matches a string in features 
                         (let ([thesefeatures (vector-ref (car trailvec-remaining) 14)])
                           (cond [(null? (cdr trailvec-remaining))
                                  (vector-set! (car trailvec-remaining) 0 (+ (vector-ref (car trailvec-remaining) 0) (* 100 (find-num-matches 0 thesefeatures))))
                                  (map vector->list (cons (car trailvec-remaining) newtrails))]
                                 [else
                                  (vector-set! (car trailvec-remaining) 0 (+ (vector-ref (car trailvec-remaining) 0) (* 100 (find-num-matches 0 thesefeatures))))
                                  (kernel (cons (car trailvec-remaining) newtrails) (cdr trailvec-remaining))]
                                 )))]
               )
        (if (featurematch? features)
            (kernel null trailvec)
            traillst)
        ))))

;;;Procedure:
;;;   assignactivityscore
;;;Parameters:
;;;   traillst, a list of lists containing trail information
;;;   activities, a list of strings provided by the user where each string is a desired activity
;;;Purpose:
;;;   to modify the score of each trail within traillst based on how many of the desired activities that trail has
;;;      (containing one or more of the desired activites will cause the score of a trail to be increased)
;;;Produces:
;;;   newtraillst, a list of lists containing all the same trail information as traillst but with the score modified
;;;Preconditions:
;;;   each of the lists within traillst will contain:
;;;      a integer number as the first element, representing the score for that trail
;;;      a list of strings describing the activities of that trail as the 16th element.
;;;   neither traillst or features can be null
;;;Postconditions:
;;;   if "a" is a desired activity contained within features and also within the acitivity list at the 15th element of a list within traillst,
;;;      the score of that trail will be increased by 100
;;;   if n is the initial score of a trail in traillst, and m is the score of that same trail after assignfeaturescore is called, n<=m
;;;   length of traillst = length of newtraillst
;;;   the length of the lists within traillst will remain unchanged                             
(define assignactivityscore
  (lambda (traillst activities)
    (let ([trailvec (map list->vector traillst)])
      (letrec ([altor (lambda (bool1 bool2)                                      ;created to use in combination with reduce, so that boolean lists can be reduced
                        (or bool1 bool2))]
               [activitymatch? (lambda (activities-remaining)                    ;first test to see if any of the activities given by user are actually possible activities. Meant to save time
                                 (cond [(null? activities-remaining)
                                        #f]
                                       [(reduce altor (map (section string-ci=? (car activities-remaining) <>) activitylst))
                                        #t]
                                       [else
                                        (activitymatch? (cdr activities-remaining))]
                                       ))]
               [find-num-matches (lambda (matches-so-far activities-remaining)  ;takes a single list of activities from trailvec and finds the total number of matches to activities
                                   (cond [(null? activities-remaining)
                                          matches-so-far]
                                         [(reduce altor (map (section string-ci=? (car activities-remaining) <>) activities))
                                          (find-num-matches (+ 1 matches-so-far) (cdr activities-remaining))]
                                         [else
                                          (find-num-matches matches-so-far (cdr activities-remaining))]
                                         ))]
               [kernel (lambda (newtrails trailvec-remaining)                    ;recursive procedure that uses find-num-matches to add 100 to the existing score of each trail in trailvec each time an activity of that trail matches a string in activities
                         (let ([theseactivities (vector-ref (car trailvec-remaining) 15)])
                           (cond [(null? (cdr trailvec-remaining))
                                  (vector-set! (car trailvec-remaining) 0 (+ (vector-ref (car trailvec-remaining) 0) (* 100 (find-num-matches 0 theseactivities))))
                                  (map vector->list (cons (car trailvec-remaining) newtrails))]
                                 [else
                                  (vector-set! (car trailvec-remaining) 0 (+ (vector-ref (car trailvec-remaining) 0) (* 100 (find-num-matches 0 theseactivities))))
                                  (kernel (cons (car trailvec-remaining) newtrails) (cdr trailvec-remaining))]
                                 )))]
               )
        (if (activitymatch? activities)
            (kernel null trailvec)
            traillst)
        ))))


;;;Procedure
;;;   assigndifficultyscore
;;;Parameters
;;;   traillst, the cleaned table of trail information
;;;   diff, the user-input for desired difficulty of trail (can be a number 1-5 or the string "NA")
;;;Purpose
;;;   Changes score of each trail based on how well it fits to the user's desired difficulty
;;;Produces
;;;   nothing, called for side effect
;;;Preconditions
;;;   only numerical inputs for diff will produce changes to trail scores
;;;   diff must be between or equal to 1 and 5 [unverified]
;;;Postconditions
;;;   any trail with a difficulty level equal to diff will have 100 added to its score
;;;   any trail with a difficulty level differing from diff by 1 will have 50 added to its score
;;;   any trail with a difficulty level differing from diff by 2 will have 25 added to its score
(define assigndifficultyscore
  (lambda (traillst diff)
    (let ([trailvec (map list->vector traillst)])
      (letrec ([kernel
                (lambda (newtrails trailvec diff)
                  (let* ([traildiff (vector-ref (car trailvec) 9)])
                    (when (not (= 0 traildiff))
                      (cond
                        [(null? (cdr trailvec))
                         (cond [(= diff traildiff)
                                (vector-set! (car trailvec) 0 (+ (vector-ref (car trailvec) 0) 100))
                                (map vector->list (cons (car trailvec) newtrails))]
                               [else
                                (map vector->list (cons (car trailvec) newtrails))])]
                        [(= diff traildiff)
                         (vector-set! (car trailvec) 0 (+ (vector-ref (car trailvec) 0) 100))
                         (kernel (cons (car trailvec) newtrails) (cdr trailvec) diff)]
                        [(or (= (+ diff 1) traildiff)
                             (= (- diff 1) traildiff))
                         (vector-set! (car trailvec) 0 (+ (vector-ref (car trailvec) 0) 50))
                         (kernel (cons (car trailvec) newtrails) (cdr trailvec) diff)]
                        [(or (= (+ diff 2) traildiff)
                             (= (- diff 2) traildiff))
                         (vector-set! (car trailvec) 0 (+ (vector-ref (car trailvec) 0) 25))
                         (kernel (cons (car trailvec) newtrails) (cdr trailvec) diff)]
                        [else
                         (kernel (cons (car trailvec) newtrails) (cdr trailvec) diff)]))))])
        (kernel null trailvec diff)))))

;;;Procedure
;;;   assigntypescore
;;;Parameters
;;;   traillst, a list of lists
;;;   type, a string
;;;Purpose
;;;   to assign a score of 100 if the input type is the same as the trail type
;;;Produces
;;;   newtrails, a list of lists
(define assigntypescore
  (lambda (traillst type)
    (let ([trailvec (map list->vector traillst)])
      (letrec ([kernel
                (lambda (newtrails trailvec type)
                  (let* ([trailtype (if (string? (vector-ref (car trailvec) 10))
                                        (vector-ref (car trailvec) 10)
                                        "")])
                    (cond [(null? (cdr trailvec))
                           (cond [(string-ci=? type trailtype)
                                  (vector-set! (car trailvec) 0 (+ (vector-ref (car trailvec) 0) 100))
                                  (map vector->list (cons (car trailvec) newtrails))]
                                 [else
                                  (map vector->list (cons (car trailvec) newtrails))])]
                          [(string-ci=? type trailtype)
                           (vector-set! (car trailvec) 0 (+ (vector-ref (car trailvec) 0) 100))
                           (kernel (cons (car trailvec) newtrails) (cdr trailvec) type)]
                          [else
                           (kernel (cons (car trailvec) newtrails) (cdr trailvec) type)])))])
        (kernel null trailvec type)))))

;;;Procedure
;;;   assignratingscore
;;;Parameters
;;;   traillst, the cleaned table of trail information
;;;   rating, the user-input for desired rating of trail in the form of a integer from 1 to 5
;;;Purpose
;;;   Changes score of each trail based on how close it is to rating
;;;Produces
;;;   newtraillst, the same as traillst except with the scores of each trail edited
;;;Preconditions
;;   each of the lists within traillst will contain:
;;;      a integer number as the first element, representing the score for that trail
;;;      a number as the 13th element representing rating.
;;;Postconditions:
;;;   if n is the initial score of a trail in traillst, and m is the score of that same trail after assignfeaturescore is called, n<=m
;;;   length of traillst = length of newtraillst
;;;   the length of the lists within traillst will remain unchanged
;;;   if a rating is equal to the rating of a given trail, 100 will be added to that trail's score
;;;   if a rating is 0.5 different from the rating of a given trail, 80 will be added to that trail's score
;;;   if a rating is 1.5 different from the rating of a given trail, 60 will be added to that trail's score
;;;   if a rating is 2 different from the rating of a given trail, 40 will be added to that trail's score
;;;   if a rating is 2.5 different from the rating of a given trail, 20 will be added to that trail's score
  (lambda (traillst rating)
    (let ([trailvec (map list->vector traillst)])
      (letrec ([kernel
                (lambda (newtrails trailvec rating)
                  (let* ([trailrating (vector-ref (car trailvec) 12)])
                    (cond
                      [(null? (cdr trailvec))
                       (cond [(= rating trailrating)
                              (vector-set! (car trailvec) 0 (+ (vector-ref (car trailvec) 0) 100))
                              (map vector->list (cons (car trailvec) newtrails))]
                             [else
                              (map vector->list (cons (car trailvec) newtrails))])]
                      [(= rating trailrating)
                       (vector-set! (car trailvec) 0 (+ (vector-ref (car trailvec) 0) 100))
                       (kernel (cons (car trailvec) newtrails) (cdr trailvec) rating)]
                      [(or (= (+ rating .5) trailrating)
                           (= (- rating .5) trailrating))
                       (vector-set! (car trailvec) 0 (+ (vector-ref (car trailvec) 0) 80))
                       (kernel (cons (car trailvec) newtrails) (cdr trailvec) rating)]
                      [(or (= (+ rating 1) trailrating)
                           (= (- rating 1) trailrating))
                       (vector-set! (car trailvec) 0 (+ (vector-ref (car trailvec) 0) 60))
                       (kernel (cons (car trailvec) newtrails) (cdr trailvec) rating)]
                      [(or (= (+ rating 1.5) trailrating)
                           (= (- rating 1.5) trailrating))
                       (vector-set! (car trailvec) 0 (+ (vector-ref (car trailvec) 0) 40))
                       (kernel (cons (car trailvec) newtrails) (cdr trailvec) rating)]
                      [(or (= (+ rating 2) trailrating)
                           (= (- rating 2) trailrating))
                       (vector-set! (car trailvec) 0 (+ (vector-ref (car trailvec) 0) 20))
                       (kernel (cons (car trailvec) newtrails) (cdr trailvec) rating)]
                      [(or (= (+ rating 2.5) trailrating)
                           (= (- rating 2.5) trailrating))
                       (vector-set! (car trailvec) 0 (+ (vector-ref (car trailvec) 0)))
                       (kernel (cons (car trailvec) newtrails) (cdr trailvec) rating)]
                      [else
                       (kernel (cons (car trailvec) newtrails) (cdr trailvec) rating)])))])
        (kernel null trailvec rating)))))


;input-vec format: zip, max-radius, length, diff, traf, rating, type, features, activities

;;;Procedure
;;;  trailmaster-3000
;;;Parameters
;;;   none
;;;Purpose
;;;   to call the necessary procedures to take input from user and based on that input return the top five trails from cleantrails that best match the user's preferences
;;;Produces
;;;   besttrails, a list of 5 or fewer strings, where each string is the name of a trail that best matches the user's preferences
;;;Preconditions
;;;   none
;;;Postconditions:
;;;   all strings in besttrails are names of trails in cleantrails (ie equal to the second element in one of the lists of cleantrails)
;;;   if there are no trails within the desired max distance of the user's location, an error will be returned
(define trailmaster-3000
  (lambda ()
    (trail-input)
    (let* ([shorttraillst (if (null? (limit-distance (vector-ref input-vec 0) (vector-ref input-vec 1)))
                              (error "there are no national park trails within the given travel distance. Please search again with a larger travel distance.")
                              (limit-distance (vector-ref input-vec 0) (vector-ref input-vec 1)))]
           [length-score  (assignlengthscore shorttraillst (vector-ref input-vec 2))]
           [diff-score (if (and (string? (vector-ref input-vec 3))
                                (string-ci=? "NA" (vector-ref input-vec 3)))
                           length-score
                           (assigndifficultyscore length-score (vector-ref input-vec 3)))]
           [traffic-score (if (and (string? (vector-ref input-vec 4))
                                   (string-ci=? "NA" (vector-ref input-vec 4)))
                              diff-score
                              (assigntrafficscore diff-score (vector-ref input-vec 4)))]
           [rating-score (if (and (string? (vector-ref input-vec 5))
                                  (string-ci=? "NA" (vector-ref input-vec 5)))
                             traffic-score
                             (assignratingscore traffic-score (vector-ref input-vec 5)))]
           [type-score (if (and (string? (vector-ref input-vec 6))
                                (string-ci=? "NA" (vector-ref input-vec 6)))
                           rating-score
                           (assigntypescore rating-score (vector-ref input-vec 6)))]
           [feature-score (if (and (string? (vector-ref input-vec 7))
                                   (string-ci=? "NA" (vector-ref input-vec 7)))
                              type-score
                              (assignfeaturescore type-score (vector-ref input-vec 7)))]
           [activity-score (if (and (string? (vector-ref input-vec 8))
                                    (string-ci=? "NA" (vector-ref input-vec 8)))
                               feature-score
                               (assignactivityscore feature-score (vector-ref input-vec 8)))]
           [score>? (lambda (lst lst2)
                      (>= (car lst) (car lst2)))]
           [sortedfinallst (map cdr (sort activity-score score>?))])
      (if (>= (length sortedfinallst) 5)
          (map (section list-ref <> 1) (take sortedfinallst 5))
          (map (section list-ref <> 1) sortedfinallst)))))

;;;Procedure
;;;   trailmasterdirectory
;;;Parameters
;;;   name, a string
;;;Purpose
;;;   to return the trail information associated with name, to let the user search clean trails using the name of a trail
;;;Produces
;;;   trailinfo, a list containing the trail information of a single trail
;;;Preconditions
;;;   no additional
;;;Postconditions:
;;;   if name does not match the name of a trail in cleantrails, an string error message will be returned
;;;   length of trail will be in miles
(define trailmasterdirectory
  (lambda (name)
    (let kernel ([trailsremaining cleantrails])
      (cond [ (null? trailsremaining) "this is not the trail you are looking for..."]
            [ (string-ci=? name (cadr (car trailsremaining)))
              (let ([editedtrail (list->vector (car trailsremaining))])
                (vector-set! editedtrail 6 (/ (list-ref (car trailsremaining) 6) 1609.34))
                (display "the trail information is in the following format:")
                (newline)
                (display "trail ID, name, area name, city, state, latitude & longitude, length (miles), elevation gain, difficulty rating, route type, visitor usage, average rating, number of reviews, features, and activities")
                (newline)
                (vector->list editedtrail)
                )
              ]
            [ else
              (kernel (cdr trailsremaining))]))))
      
                 
