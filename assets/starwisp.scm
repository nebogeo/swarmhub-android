;; Starwisp Copyright (C) 2013 Dave Griffiths
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cattle "Cattle Slurry")
(define FYM "Farmyard Manure")
(define pig "Pig Slurry")
(define poultry "Poultry Litter")
(define normal "Normal")
(define grass-oilseed "Grassland/Winter oilseed rape")
(define sandyshallow "Sandy/Shallow")
(define mediumheavy "Medium/Heavy")
(define autumn "Autumn")
(define winter "Winter")
(define spring "Spring")
(define summer "Summer")
(define fresh "Soil incorporated, fresh")
(define other "Surface applied or old")
(define layer "Layer manure")
(define broiler "Broiler litter")
(define metric "Metric")
(define imperial "Imperial")
(define DM2 "2% DM")
(define DM4 "4% DM")
(define DM6 "6% DM")
(define DM10 "10% DM")

(define images
  (list
   (list cattle
         (list
          (list 25 "cattle_25m3")
          (list 50 "cattle_50m3")
          (list 100 "cattle_100m3")))
   (list FYM
         (list
          (list 25 "fym_25t")
          (list 50 "fym_50t")))
   (list pig
         (list
          (list 25 "pig_25m3")
          (list 50 "pig_50m3")))
   (list poultry
         (list
          (list 5 "poultry_5t")
          (list 10 "poultry_10t")))))

(define (abs n)
  (if (< n 0) (- n) n))

(define (find-image type amount)
  (define (_type images)
    (cond
     ((null? images) #f)
     ((equal? (car (car images)) type) (car images))
     (else (_type (cdr images)))))
  (define (_amount images s)
    (cond
     ((null? images) s)
     ((< (abs (- amount (car (car images))))
         (abs (- amount (car s))))
      (_amount (cdr images) (car images)))
     (else (_amount (cdr images) s))))
  (let ((type-images (cadr (_type images))))
    (cadr (_amount type-images (car type-images)))))

(define (nutrients type units amount table) (list type units amount table))
(define (nutrients-type n) (list-ref n 0))
(define (nutrients-units n) (list-ref n 1))
(define (nutrients-amount n) (list-ref n 2))
(define (nutrients-table n) (list-ref n 3))

(define (quality q n p k) (list q n p k))
(define (quality-q q) (list-ref q 0))
(define (quality-n q) (list-ref q 1))
(define (quality-p q) (list-ref q 2))
(define (quality-k q) (list-ref q 3))

;; nitrogen is based on season and crop
(define (nitrogen autumn winter spring summer)
  (list autumn winter spring summer))
(define (nitrogen-season n s)
  (cond
    ((equal? s autumn) (list-ref n 0))
    ((equal? s winter) (list-ref n 1))
    ((equal? s spring) (list-ref n 2))
    ((equal? s summer) (list-ref n 3))
    (else (error "season " s " not found") #f)))

(define (soil sandyshallow mediumheavy)
  (list sandyshallow mediumheavy))
(define (soil? s) (list? s))
(define (get-soil s t)
  (cond
    ((equal? t sandyshallow) (list-ref s 0))
    ((equal? t mediumheavy) (list-ref s 1))
    (else (error "soil type " t " not found") #f)))

(define (crop normal g)
  (list normal g))
(define (crop? c) (list? c))
(define (get-crop c t)
  (cond
    ((equal? t normal) (list-ref c 0))
    ((equal? t grass-oilseed) (list-ref c 1))
    (else (error "crop type " t " not found") #f)))


(define (find n l)
  (cond
    ((null? l) #f)
    ((equal? n (car (car l))) (car l))
    (else (find n (cdr l)))))

(define nutrients-metric
  (list
   (nutrients
    cattle "m3/ha" 100
    (list
     (quality DM2 (nitrogen (soil (crop 8 16) (crop 48 56))  48 72 56) 30 220)
     (quality DM6 (nitrogen (soil (crop 13 26) (crop 65 78)) 65 91 65) 60 290)
     (quality DM10 (nitrogen (soil (crop 18 36) (crop 72 90)) 72 90 72) 90 360)))
   (nutrients
    pig "m3/ha" 50
    (list
     (quality DM2 (nitrogen (soil (crop 15 22.5) (crop 52.5 60)) 60 82.5 82.5) 25 90)
     (quality DM4 (nitrogen (soil (crop 18 27) (crop 54 63)) 63 90 90) 45 110)
     (quality DM6 (nitrogen (soil (crop 22 33) (crop 55 66)) 66 99 99) 65 125)))
   (nutrients
    poultry "tons/ha" 10
    (list
     (quality layer (nitrogen (soil (crop 19 28.5) (crop 47.5 57)) 47.5 66.5 66.5) 84 86)
     (quality broiler (nitrogen (soil (crop 30 45) (crop 75 90)) (soil 60 75) 90 90) 150 162)))
   (nutrients
    FYM "tons/ha" 50
    (list
     (quality other (nitrogen (soil 15 30) 30 30 30) 95 360) ;; other
     (quality fresh (nitrogen (soil 15 30) 30 45 30) 95 360) ;; soil inc fresh
    ))))

(define (tons/acre->tons/ha a)
  (* a 2.47105381))

(define (gallons/acre->m3/ha a)
  (* a 0.0112336377))

(define (kg/ha->units/acre a)
  (* a 0.8))

(define (error . args)
  (display (apply string-append args))(newline))

(define (get-nutrients type amount quality season crop soil)
  (let ((nutrients (find type nutrients-metric)))
    (if (not nutrients)
        (error "nutrients type " type " not found")
        (let ((q (find quality (nutrients-table nutrients))))
          (if (not q)
              (error "quality " quality " not found")
              (get-nutrients-inner
               (nutrients-amount nutrients)
               (nutrients-units nutrients)
               q amount season crop soil))))))

(define (get-nutrients-inner quantity units quality amount season crop soil)
  (process-nutrients
   amount
   units
   quantity
   (list
    ;; nitrogen
    (let ((s (nitrogen-season (quality-n quality) season)))
      (if (not s)
          (error "season not found")
          (let ((c (if (soil? s)
                       (get-soil s soil)
                       s)))
            (if (crop? c)
                (get-crop c crop)
                c))))
    (quality-p quality)
    (quality-k quality))))

(define (imperial->metric amount units)
  (if (equal? (current-units) metric)
      amount
      (if (equal? units "m3/ha")
          (gallons/acre->m3/ha amount)
          (tons/acre->tons/ha amount))))

(define (metric->imperial amount units)
  (if (equal? (current-units) metric)
      amount
      (kg/ha->units/acre amount)))

(define (rounding a)
  (/ (round (* 100 a)) 100))

;; quantity is from the table (so I can debug easily it matches the data)
;; amount is from the slider
(define (process-nutrients amount units quantity nutrients)
  (let ((amount (imperial->metric amount units)))
    (map
     (lambda (q)
       (rounding (metric->imperial (* amount (/ q quantity)) units)))
     nutrients)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (event id type date nutrients amount quality season crop soil size units)
  (list id type date nutrients amount quality season crop soil size units))

(define (empty-event)
  (event 0 "" (list 0 0 0) (list 0 0 0) 0 "" "" "" "" 0 metric))
(define (event-id e) (list-ref e 0))
(define (event-type e) (list-ref e 1))
(define (event-date e) (list-ref e 2))
(define (event-nutrients e) (list-ref e 3))
(define (event-amount e) (list-ref e 4))
(define (event-quality e) (list-ref e 5))
(define (event-season e) (list-ref e 6))
(define (event-crop e) (list-ref e 7))
(define (event-soil e) (list-ref e 8))
(define (event-size e) (list-ref e 9))
(define (event-units e) (list-ref e 10))

(define (field name soil crop size)
  (list name soil crop '() size))

(define (empty-field)
  (field "" "" "" '()))

(define (field-name f) (list-ref f 0))
(define (field-modify-name f v) (list-replace f 0 v))
(define (field-soil f) (list-ref f 1))
(define (field-modify-soil f v) (list-replace f 1 v))
(define (field-crop f) (list-ref f 2))
(define (field-modify-crop f v) (list-replace f 2 v))
(define (field-events f) (list-ref f 3))
(define (field-modify-events f v) (list-replace f 3 v))
(define (field-size f) (list-ref f 4))
(define (field-modify-size f v) (list-replace f 4 v))

(define (field-add-event f event)
  (field-modify-events
   f
   (sort (append (field-events f) (list event))
         (lambda (a b)
           (date< (event-date a) (event-date b))))))

(define (field-remove-event f id)
  (field-modify-events
   f (filter
      (lambda (field)
        (not (equal? id (event-id field))))
      (field-events f))))

(define (state)
  (list
   (calc pig 25 DM2 autumn normal mediumheavy)
   (empty-field)
   (saved-data-load)
   (list date-day date-month date-year)
   (empty-event)
   1))

(define (state-calc s) (list-ref s 0))
(define (state-modify-calc s v) (list-replace s 0 v))
(define (state-field s) (list-ref s 1))
(define (state-modify-field s v) (list-replace s 1 v))
(define (state-saved-data s) (list-ref s 2))
(define (state-modify-saved-data s fn)
  (list-replace s 2 (saved-data-save (fn (state-saved-data s)))))
(define (state-date s) (list-ref s 3))
(define (state-modify-date s v) (list-replace s 3 v))
(define (state-event s) (list-ref s 4))
(define (state-modify-event s v) (list-replace s 4 v))
(define (state-seek-mul s) (list-ref s 5))
(define (state-modify-seek-mul s v) (list-replace s 5 v))

(define (saved-data fields)
  (list 0 fields 0 metric "your email"))
(define (saved-data-version f) (list-ref f 0))
(define (saved-data-fields f) (list-ref f 1))
(define (saved-data-modify-fields f v) (list-replace f 1 v))
(define (saved-data-next-event-id f) (list-ref f 2))
(define (saved-data-modify-next-event-id f v) (list-replace f 2 v))
(define (saved-data-units f) (list-ref f 3))
(define (saved-data-modify-units f v) (list-replace f 3 v))
(define (saved-data-email f) (list-ref f 4))
(define (saved-data-modify-email f v) (list-replace f 4 v))

(define (saved-data-modify-field fn name f)
  (saved-data-modify-fields
   f (map
      (lambda (field)
        (if (equal? (field-name field) name)
            (fn field)
            field))
      (saved-data-fields f))))

(define (fields-remove-field f name)
  (filter
   (lambda (field)
     (not (equal? name (field-name field))))
   f))

(define (saved-data-save d)
  (let ((f (open-output-file (string-append dirname "swarmhub-data.scm"))))
    (write d f)
    (close-output-port f))
  d)

(define (saved-data-load)
  (let* ((f (open-input-file (string-append dirname "swarmhub-data.scm"))))
    (if (not f)
        (saved-data-save (saved-data '()))
        (let ((r (read f)))
          (close-input-port f)
          ;; check versioning
          (if (not (number? (list-ref r 0)))
              ;; version 0
              (append (list 0) r (list "your email"))
              r)))))

(define (save-data filename d)
  (let ((f (open-output-file (string-append dirname filename))))
    (display d f)
    (close-output-port f))
  d)


(define (calc type amount quality season crop soil)
  (list type amount quality season crop soil))

(define (calc-type s) (list-ref s 0))
(define (calc-modify-type s v) (list-replace s 0 v))
(define (calc-amount s) (list-ref s 1))
(define (calc-modify-amount s v) (list-replace s 1 v))
(define (calc-quality s) (list-ref s 2))
(define (calc-modify-quality s v) (list-replace s 2 v))
(define (calc-season s) (list-ref s 3))
(define (calc-modify-season s v) (list-replace s 3 v))
(define (calc-crop s) (list-ref s 4))
(define (calc-modify-crop s v) (list-replace s 4 v))
(define (calc-soil s) (list-ref s 5))
(define (calc-modify-soil s v) (list-replace s 5 v))

(define (update-calc! pre fn)
  (mutate-state!
   (lambda (s)
     (state-modify-calc s (fn (state-calc s)))))
  (run-calc pre))

(define (update-type! pre v)
  (update-calc! pre (lambda (c) (calc-modify-type c v))))
(define (update-amount! pre v) (update-calc! pre (lambda (c) (calc-modify-amount c v))))
(define (update-quality! pre v) (update-calc! pre (lambda (c) (calc-modify-quality c v))))
(define (update-season! pre v) (update-calc! pre (lambda (c) (calc-modify-season c v))))
(define (update-crop! pre v) (update-calc! pre (lambda (c) (calc-modify-crop c v))))
(define (update-soil! pre v) (update-calc! pre (lambda (c) (calc-modify-soil c v))))

(define gstate (state))

(define (mutate-state! fn)
  (set! gstate (fn gstate)))

(define (mutate-saved-data! fn)
  (mutate-state!
   (lambda (s)
     (state-modify-saved-data
      s fn))))

(define (mutate-make-event-id!)
  (mutate-saved-data!
   (lambda (d)
     (saved-data-modify-next-event-id
      d (+ (saved-data-next-event-id d) 1))))
  (saved-data-next-event-id (state-saved-data gstate)))

(define (mutate-units! v)
  (mutate-saved-data!
   (lambda (d)
     (saved-data-modify-units d v))))

(define (current-units)
  (saved-data-units
   (state-saved-data gstate)))

(define (mutate-email! v)
  (mutate-saved-data!
   (lambda (d)
     (saved-data-modify-email d v))))

(define (current-email)
  (saved-data-email
   (state-saved-data gstate)))

(define (get-fields)
  (saved-data-fields (state-saved-data gstate)))

(define (current-field) (state-field gstate))
(define (current-date) (state-date gstate))
(define (current-calc) (state-calc gstate))
(define (current-event) (state-event gstate))
(define (current-seek-mul) (state-seek-mul gstate))

(define (mutate-current-seek-mul! a)
  (mutate-state!
   (lambda (s)
     (state-modify-seek-mul s a))))

(define (mutate-current-field! fn)
  (mutate-state!
   (lambda (s)
     (state-modify-field s (fn (state-field s))))))

(define (mutate-current-event! fn)
  (mutate-state!
   (lambda (s)
     (state-modify-event s (fn (state-event s))))))

(define (find-field name)
  (define (_ f)
    (cond
     ((null? f) #f)
     ((string=? (field-name (car f)) name)
      (car f))
     (else (_ (cdr f)))))
  (_ (get-fields)))

(define (csv-headings)
  (string-append
   "Field name, "
   "Size, "
   "Soil, "
   "Crop, "
   "Manure, "
   "Date, "
   "N, P, K, "
   "Amount, "
   "Total Amount, "
   "Quality, "
   "Season, "
   "Units\n"))

(define (stringify-event event name soil crop size)
  (string-append
   name ", "
   (number->string size) ", "
   soil ", "
   crop ", "
   (event-type event) ", "
   (date->string (event-date event)) ", "
   (number->string (list-ref (event-nutrients event) 0)) ", "
   (number->string (list-ref (event-nutrients event) 1)) ", "
   (number->string (list-ref (event-nutrients event) 2)) ", "
   (number->string (event-amount event)) ", "
   (number->string (* size (event-amount event))) ", "
   (event-quality event) ", "
   (event-season event) ", "
   (event-units event)))

(define (stringify-field field)
  (foldl
   (lambda (event str)
     (string-append
      str
      (stringify-event
       event
       (field-name field)
       (field-soil field)
       (field-crop field)
       (field-size field)) "\n"))
   ""
   (field-events field)))

(define (stringify-fields)
  (foldl
   (lambda (field str)
     (string-append str (stringify-field field) "\n"))
   (csv-headings)
   (get-fields)))

(define (calc-nutrients)
  (let* ((type (calc-type (state-calc gstate)))
         (amount (* (current-seek-mul) (calc-amount (state-calc gstate))))
         (quality (calc-quality (state-calc gstate)))
         (season (calc-season (state-calc gstate)))
         (crop (calc-crop (state-calc gstate)))
         (soil (calc-soil (state-calc gstate))))
;    (display type)(newline)
;    (display amount)(newline)
;    (display season)(newline)
;    (display quality)(newline)
;    (display crop)(newline)
;    (display soil)(newline)

    (get-nutrients type amount quality season crop soil)))

(define (run-calc prepend)
  (let ((amounts (calc-nutrients))
        (amount (* (current-seek-mul) (calc-amount (state-calc gstate))))
        (type (calc-type (state-calc gstate))))
    (list
     (update-widget 'text-view (get-id (string-append prepend "amount-value")) 'text
                    (string-append (number->string amount) " "
                                   (if (equal? (current-units) metric)
                                       (nutrients-units (find type nutrients-metric))
                                       (if (equal? (nutrients-units (find type nutrients-metric)) "m3/ha")
                                           "gallons/acre"
                                           "tons/acre"))))

     (update-widget 'text-view (get-id (string-append prepend "na"))
                    'text (number->string (list-ref amounts 0)))
     (update-widget 'text-view (get-id (string-append prepend "pa"))
                    'text (number->string (list-ref amounts 1)))
     (update-widget 'text-view (get-id (string-append prepend "ka"))
                    'text (number->string (list-ref amounts 2))))))

(define (spacer size)
  (space (layout 'fill-parent size 1 'left)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define graph-width 320)

(define (build-lines events min max colour n)
  (let ((twidth (- max min)))
    (cadr (foldl
           (lambda (event r)
             (let* ((t (date->day (event-date event)))
                    (last-point (car r))
                    (points-list (cadr r))
                    (x (* graph-width (/ (- t min) twidth)))
                    (y (- 200 (list-ref (event-nutrients event) n))))
               (list
                (list x y)
                (cons (drawlist-line
                       colour 5 (list (car last-point) (cadr last-point)
                                      x y))
                      points-list))))
           (list (list -10 200) '())
           events))))

(define (month->text m)
  (cond
   ((eqv? m 1) "January")
   ((eqv? m 2) "Febuary")
   ((eqv? m 3) "March")
   ((eqv? m 4) "April")
   ((eqv? m 5) "May")
   ((eqv? m 6) "June")
   ((eqv? m 7) "July")
   ((eqv? m 8) "August")
   ((eqv? m 9) "September")
   ((eqv? m 10) "October")
   ((eqv? m 11) "November")
   ((eqv? m 12) "December")))


(define (build-t-scale first min max)
  (define (_y year-width x y)
    (if (> (- x (/ year-width 2)) graph-width)
        '()
        (append
         (list
          (drawlist-text (number->string y)
                         (- x (/ year-width 2))
                         180 '(150 150 150) 20 "vertical")
          (drawlist-line '(0 0 0) 1
                         (list x 0 x 200)))
         (_y year-width (+ x year-width) (+ y 1)))))

  (define (_m month-width x m)
    (if (> (- x (/ month-width 2)) graph-width)
        '()
        (append
         (list
          (drawlist-text (month->text (+ m 1))
                         (- x (/ month-width 2))
                         180 '(0 0 0) 20 "vertical")
          (drawlist-line '(0 0 0) 1
                         (list x 0 x 200)))
         (_m month-width (+ x month-width) (modulo (+ m 1) 12)))))

  (let* ((twidth (- max min))
         (month-width (* (/ 30 twidth) graph-width))
         (first-month-x (* (/ (- 30 (list-ref first 0)) twidth) graph-width)))
    (if (<= twidth 0)
        '()
        (if (> month-width 20)
            (_m month-width first-month-x (- (list-ref first 1) 1))
            (_y
             (* (/ 365 twidth) graph-width)
             (* (/ (- 12 (list-ref first 1)) twidth) graph-width)
             (list-ref first 2))))))

(define (build-key)
  (let ((units (if (equal? (current-units) metric)
                   "kg/hectare"
                   "units/acre")))
    (list
     (drawlist-text units 15 140 '(0 0 0) 15 "vertical")
     (drawlist-text "150" 20 50 '(0 0 0) 10 "horizontal")
     (drawlist-text "100" 20 100 '(0 0 0) 10 "horizontal")
     (drawlist-text "50" 20 150 '(0 0 0) 10 "horizontal")
     (drawlist-text "N" 280 30 '(200 0 0) 20 "horizontal")
     (drawlist-text "P" 280 60 '(200 200 0) 20 "horizontal")
     (drawlist-text "K" 280 90 '(0 0 200) 20 "horizontal")
     )))

(define (build-graph)
  (append
   (let ((events (field-events (current-field))))
     (if (> (length events) 1)
         (let ((min (date->day (event-date (car events))))
               (max (date->day (event-date (list-ref events (- (length events) 1))))))
           (append
            (build-t-scale (event-date (car events)) min max)
            (build-lines events min max '(200 0 0) 0)
            (build-lines events min max '(200 200 0) 1)
            (build-lines events min max '(0 0 200) 2)
            (build-key)))
         (list (drawlist-text "Not enough events for graph"
                              20 105 '(0 0 0) 20 "horizontal"))))
   (list
    (drawlist-line '(0 0 0) 5 (list 0 0 320 0))
    (drawlist-line '(0 0 0) 5 (list 0 200 320 200)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (horiz . l)
  (linear-layout
   (make-id "h")
   'horizontal
   (layout 'fill-parent 'fill-parent 1 'left)
   l))

(define (vert . l)
  (linear-layout
   (make-id "v")
   'vertical
   (layout 'fill-parent 'fill-parent 1 'left)
   l))

(define (setup-for-picture-from-event)
  ;; setup the calculator values for the camera pic from the event
  (mutate-state!
   (lambda (s)
     (state-modify-calc
      s (calc-modify-amount
         (calc-modify-type
          (state-calc s)
          (event-type (current-event)))
         (event-amount (current-event)))))))

(define (update-seek-mul! manure)
  (if (and (equal? (current-units) imperial)
           (or (equal? manure cattle)
               (equal? manure pig)))
      (mutate-current-seek-mul! 100)
      (mutate-current-seek-mul! 1)))

(define (build-field-buttons)
  (if (null? (get-fields))
      (list (text-view (make-id "temp") "Add some fields" 20 fillwrap))
      (map
       (lambda (field)
         (button
          (make-id (field-name field))
          (field-name field)
          20 fillwrap
          (lambda ()
            (list (start-activity "field" 2 (field-name field))))))
       (get-fields))))

(define (build-event-buttons)
  (if (null? (field-events (current-field)))
      (list (text-view (make-id "temp") "No events yet" 15 fillwrap))
      (map
       (lambda (event)
         (button
          (make-id (string-append
                    "event-"
                    ;; need to add field to prevent clashing with other field id numbers
                    (field-name (current-field))
                    (number->string (event-id event))))
          (string-append (event-type event)
                         " "
                         (date->string (event-date event)))
          15 fillwrap
          (lambda ()
            (mutate-current-event! (lambda (ev) event))
            (list
             (start-activity "eventview" 2 "")))))
       (field-events (current-field)))))

;; just for graph so don't have to be accurate!!!
(define (date->day d)
  (+ (* (list-ref d 2) 360)
     (* (list-ref d 1) 30)
     (list-ref d 0)))

(define (date< a b)
  (cond
   ((< (list-ref a 2) (list-ref b 2)) #t)
   ((> (list-ref a 2) (list-ref b 2)) #f)
   (else ;; year is the same
    (cond
     ((< (list-ref a 1) (list-ref b 1)) #t)
     ((> (list-ref a 1) (list-ref b 1)) #f)
     (else ;; month is the same
      (cond
       ((< (list-ref a 0) (list-ref b 0)) #t)
       ((> (list-ref a 0) (list-ref b 0)) #f)
       (else #f)))))))


(define (date->string d)
  (string-append
   (number->string (list-ref d 0))
   "/"
   (number->string (list-ref d 1))
   "/"
   (number->string (list-ref d 2))))

(define (date->season d)
  (cond
   ((or
     (eqv? (list-ref d 1) 3)
     (eqv? (list-ref d 1) 4)
     (eqv? (list-ref d 1) 5)) spring)
   ((or
     (eqv? (list-ref d 1) 6)
     (eqv? (list-ref d 1) 7)
     (eqv? (list-ref d 1) 8)) summer)
   ((or
     (eqv? (list-ref d 1) 9)
     (eqv? (list-ref d 1) 10)
     (eqv? (list-ref d 1) 11)) autumn)
   ((or
     (eqv? (list-ref d 1) 12)
     (eqv? (list-ref d 1) 1)
     (eqv? (list-ref d 1) 2)) winter)))

(define centre-layout (layout 'wrap-content 'wrap-content 1 'centre))

(define-activity-list

  (activity
   "splash"
   (vert
    (text-view (make-id "splash-title") "The Farm Crap App" 40 centre-layout)
    (text-view (make-id "splash-about") "Manage your muck with the Farm Crap App" 20 centre-layout)
    (spacer 20)
    (text-view (make-id "splash-blurb") "Developed by <a href='http://fo.am'>FoAM Kernow</a> on behalf of the <a href='www.swarmhub.co.uk'>SWARM Knowledge Hub</a>, a Rural Development Programme for England (RDPE) initiative managed by <a href='http://www.duchy.ac.uk/'>Duchy College Rural Business School</a>." 20 centre-layout)
    (spacer 20)
    (image-view (make-id "about-logo") "logo" fillwrap)
    (spacer 20)
    (button (make-id "f2") "Get started!" 20 fillwrap
            (lambda () (list (start-activity-goto "main" 2 "")))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "main"
   (vert
    (text-view (make-id "title") "Farm Crap App" 40 fillwrap)
    (text-view (make-id "title") "Your fields" 30 fillwrap)
    (linear-layout
     (make-id "main-field-list")
     'vertical
     (layout 'fill-parent 'fill-parent 1 'left)
     (build-field-buttons))
    (spacer 20)
    (button (make-id "f3") "New field" 20 fillwrap
            (lambda ()
              (list
               (start-activity "newfield" 2 ""))))
    (text-view (make-id "measure-text") "Measurement units" 20 fillwrap)
    (spinner (make-id "units") (list metric imperial) fillwrap
             (lambda (v)
               (mutate-units! v)
               (list)))
    (button (make-id "f2") "Calculator" 20 fillwrap
            (lambda () (list (start-activity "calc" 2 ""))))
    (button (make-id "email-button") "Export" 20 fillwrap
            (lambda ()
              (list
               (start-activity "email" 2 ""))))
    (button (make-id "about-button") "About" 20 fillwrap
            (lambda ()
              (list
               (start-activity "about" 2 "")))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (update-widget 'spinner (get-id "units") 'selection
                     (if (equal? (current-units) metric) 0 1))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode)
     (list
      (update-widget 'linear-layout (get-id "main-field-list") 'contents
                     (build-field-buttons)))))

  (activity
   "email"
   (vert
    (text-view (make-id "title") "Export" 40 fillwrap)
    (text-view (make-id "measure-text") "Email address to field data to" 20 fillwrap)
    (edit-text (make-id "email") (current-email) 20 fillwrap
               (lambda (v)
                 (mutate-email! v)))
    (button (make-id "email-button") "Email" 20 fillwrap
            (lambda ()
              (save-data "fields.csv" (stringify-fields))
              (list
               (send-mail (current-email) "From your Crap Calculator"
                          "Please find attached your field data."
                          (list (string-append dirname "fields.csv"))))))
    (button (make-id "finished") "Done" 20 fillwrap
            (lambda () (list (finish-activity 99)))))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (update-widget 'edit-text (get-id "email") 'text (current-email))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "calc"
   (vert
    (text-view (make-id "title") "Crap Calculator" 40 fillwrap)

    (text-view (make-id "manure-text") "Manure type" 15 fillwrap)
    (spinner (make-id "manure") (list cattle FYM pig poultry) fillwrap
             (lambda (v)
               (update-seek-mul! v)
               (append
                (update-type! "c" v)
                (list
                 (update-widget 'spinner (get-id "cquality") 'array
                                (cond
                                 ((equal? v cattle) (list DM2 DM6 DM10))
                                  ((equal? v pig) (list DM2 DM4 DM6))
                                  ((equal? v poultry) (list layer broiler))
                                  ((equal? v FYM) (list fresh other))))

                 (update-widget 'image-view (get-id "example") 'image
                                (find-image (calc-type (current-calc))
                                            (calc-amount (current-calc))))))))

    (horiz
     (vert
      (text-view (make-id "soil-text") "Soil type" 15 fillwrap)
      (spinner (make-id "soil") (list sandyshallow mediumheavy) fillwrap
               (lambda (v) (update-soil! "c" v))))
     (vert
      (text-view (make-id "crop-text") "Crop type" 15 fillwrap)
      (spinner (make-id "crop") (list normal grass-oilseed) fillwrap
               (lambda (v) (update-crop! "c" v)))))

    (horiz
     (vert
      (text-view (make-id "season-text") "Season" 15 fillwrap)
      (spinner (make-id "season") (list autumn winter spring summer) fillwrap
               (lambda (v) (update-season! "c" v))))
     (vert
      (text-view (make-id "quality-text") "Quality" 15 fillwrap)
      (spinner (make-id "cquality") (list DM2 DM4 DM6) fillwrap
                (lambda (v) (update-quality! "c" v)))))

    (text-view (make-id "amount-text") "Amount" 15 fillwrap)
    (seek-bar (make-id "amount") 100 fillwrap
              (lambda (v)
                (append
                 (update-amount! "c" v)
                 (list
                  (update-widget 'image-view (get-id "example") 'image
                                 (find-image (calc-type (current-calc))
                                             (calc-amount (current-calc))))))))

    (text-view (make-id "camount-value") "4500 gallons" 30
               (layout 'wrap-content 'wrap-content 1 'centre))
    (horiz
     (text-view (make-id "nt") "N/ha" 30 fillwrap)
     (text-view (make-id "pt") "P/ha" 30 fillwrap)
     (text-view (make-id "kt") "K/ha" 30 fillwrap))
     (horiz
      (text-view (make-id "cna") "12" 30
                 (layout 'fill-parent 'fill-parent 1 'centre))
      (text-view (make-id "cpa") "75" 30
                 (layout 'fill-parent 'fill-parent 1 'centre))
      (text-view (make-id "cka") "55" 30
                 (layout 'fill-parent 'fill-parent 1 'centre)))

     (image-view (make-id "example") "test" (layout 'wrap-content 250 1 'left))

     (button (make-id "finished") "Done" 20 fillwrap
             (lambda () (list (finish-activity 99)))))

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (if (equal? (current-units) metric) (list)
         (list

          (update-widget 'text-view (get-id "nt") 'text "N/acre")
          (update-widget 'text-view (get-id "pt") 'text "P/acre")
          (update-widget 'text-view (get-id "kt") 'text "K/acre"))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "newfield"
    (vert
     (text-view (make-id "title") "Make a new field" 40 fillwrap)
     (text-view (make-id "name-txt") "Field name" 15 fillwrap)
     (edit-text (make-id "name") "" 20 fillwrap
                (lambda (v)
                  (mutate-state!
                   (lambda (s)
                     (state-modify-field
                      s (field-modify-name (state-field s) v))))))
      (text-view (make-id "soil-text") "Soil type" 15 fillwrap)
      (spinner (make-id "soil") (list sandyshallow mediumheavy) fillwrap
               (lambda (v)
                  (mutate-state!
                   (lambda (s)
                     (state-modify-field
                      s (field-modify-soil (state-field s) v))))))
      (text-view (make-id "crop-text") "Crop type" 15 fillwrap)
      (spinner (make-id "crop") (list normal grass-oilseed) fillwrap
               (lambda (v)
                 (mutate-state!
                  (lambda (s)
                    (state-modify-field
                     s (field-modify-crop (state-field s) v))))))

      (text-view (make-id "field-size-text") "Field size" 15 fillwrap)

      (seek-bar (make-id "field-size") 100 fillwrap
                (lambda (v)
                 (mutate-state!
                  (lambda (s)
                    (state-modify-field
                     s (field-modify-size (state-field s) v))))
                  (list
                   (update-widget 'text-view (get-id "field-size-amount") 'text
                                  (string-append (number->string v)
                                                 (if (equal? (current-units) metric)
                                                     " hectares" " acres"))))))

      (text-view (make-id "field-size-amount") "" 15 fillwrap)

      (horiz
       (button (make-id "save") "Save" 20 fillwrap
               (lambda ()
                 (mutate-saved-data!
                  (lambda (d)
                    (saved-data-modify-fields
                     d (append (saved-data-fields d) (list (current-field))))))
                 (list (finish-activity 99))))
       (button (make-id "cancel") "Cancel" 20 fillwrap
               (lambda () (list (finish-activity 99)))))
      )

    (lambda (activity arg)
      (activity-layout activity))
    (lambda (activity arg)
      (list
       (update-widget 'text-view (get-id "field-size-amount") 'text
                      (string-append
                       "50"
                       (if (equal? (current-units) metric)
                           " acres" " hectares")))))
    (lambda (activity) '())
    (lambda (activity) '())
    (lambda (activity) '())
    (lambda (activity) '())
    (lambda (activity requestcode resultcode) '()))

  (activity
   "field"
    (vert
     (text-view (make-id "field-title") "Long Meadow" 40 fillwrap)
     (canvas (make-id "graph")
             (layout 'fill-parent 200 1 'centre)
             (list))
     (text-view (make-id "events-txt") "Events" 20 fillwrap)
     (linear-layout
      (make-id "field-events-list")
      'vertical
      (layout 'fill-parent 'fill-parent 1 'left)
      (build-event-buttons))
     (spacer 20)
     (button (make-id "event") "New spreading event" 20 fillwrap
             (lambda ()
               (list (start-activity "fieldcalc" 2 ""))))

     (horiz
      (button (make-id "delete") "Delete" 20 (layout 'fill-parent 'wrap-content 0.7 'left)
              (lambda ()
                (list
                 (alert-dialog
                  "deleteme"
                  (string-append "Do you want to delete '" (field-name (current-field)) "'?")
                  (lambda (r)
                    (cond
                     ((zero? r) (list))
                     (else
                      (mutate-saved-data!
                       (lambda (d)
                         (saved-data-modify-fields
                          d (fields-remove-field
                             (get-fields)
                             (field-name (current-field))))))
                      (mutate-current-field! (lambda (f) (empty-field)))
                      (list (finish-activity 99)))))))))
      (button (make-id "back") "Back" 20 (layout 'fill-parent 'wrap-content 0.3 'left)
              (lambda ()
                (mutate-current-field! (lambda (f) (empty-field)))
                (list (finish-activity 99))))))

    (lambda (activity arg)
      (activity-layout activity))
    (lambda (activity arg)
      ;; load up into the current field
      (mutate-current-field! (lambda (f) (find-field arg)))
      (list
       (update-widget 'text-view (get-id "field-title") 'text arg)
       (update-widget 'linear-layout (get-id "field-events-list") 'contents
                      (build-event-buttons))
       (update-widget 'canvas (get-id "graph") 'drawlist (build-graph))))
    (lambda (activity) '())
    (lambda (activity) '())
    (lambda (activity) '())
    (lambda (activity) '())
    (lambda (activity requestcode resultcode)
      (list
       (update-widget 'linear-layout (get-id "field-events-list") 'contents
                      (build-event-buttons)))))

  (activity
   "fieldcalc"
    (vert
     (text-view (make-id "fieldcalc-title") "field name" 40 fillwrap)
     (text-view (make-id "blurb") "Enter new crap spreading event" 20 fillwrap)
     (horiz
      (text-view (make-id "fc-date-text")
                 (date->string (list date-day date-month date-year)) 20 fillwrap)
      (button (make-id "date") "Set date" 20 fillwrap
              (lambda ()
                (list (date-picker-dialog
                       "fieldcalc-date"
                       (lambda (day month year)
                         (mutate-state!
                          (lambda (s)
                            (state-modify-date s (list day (+ month 1) year))))
                         (update-season! "fc" (date->season (current-date)))
                         (list
                          (update-widget 'text-view (make-id "fc-date-text") 'text
                                         (date->string (list day (+ month 1) year))))))))))

     (horiz
      (vert
       (text-view (make-id "manure-text") "Manure type" 15 fillwrap)
       (spinner (make-id "manure") (list cattle FYM pig poultry) fillwrap
                (lambda (v)
                  (update-seek-mul! v)
                  (append
                   (update-type! "fc" v)
                   (list
                    (update-widget 'image-view (get-id "example") 'image
                                   (find-image (calc-type (current-calc))
                                               (calc-amount (current-calc))))
                    (update-widget 'spinner (get-id "quality") 'array
                                   (cond
                                    ((equal? v cattle) (list DM2 DM6 DM10))
                                    ((equal? v pig) (list DM2 DM4 DM6))
                                    ((equal? v poultry) (list layer broiler))
                                    ((equal? v FYM) (list fresh other)))))))))

      (vert
       (text-view (make-id "quality-text") "Quality" 15 fillwrap)
       (spinner (make-id "quality") (list DM2 DM4 DM6) fillwrap
                (lambda (v) (update-quality! "fc" v)))))
     (text-view (make-id "amount-text") "Amount" 15 fillwrap)
     (seek-bar (make-id "amount") 100 fillwrap
              (lambda (v)
                (cons
                 (update-widget 'image-view (get-id "example") 'image
                     (find-image (calc-type (current-calc))
                                 (calc-amount (current-calc))))
                 (update-amount! "fc" v))))
     (text-view (make-id "fcamount-value") "4500 gallons" 30
                (layout 'wrap-content 'wrap-content 1 'centre))

     (horiz
       (text-view (make-id "nt") "N/ha" 30 fillwrap)
       (text-view (make-id "pt") "P/ha" 30 fillwrap)
       (text-view (make-id "kt") "K/ha" 30 fillwrap))
     (horiz
        (text-view (make-id "fcna") "12" 30 fillwrap)
        (text-view (make-id "fcpa") "75" 30 fillwrap)
        (text-view (make-id "fcka") "55" 30 fillwrap))

     (image-view (make-id "example") "test" (layout 'wrap-content 250 1 'left))
;;     (button (make-id "camera") "Camera" 20 fillwrap
;;             (lambda () (list (start-activity "camera" 2 ""))))

     (horiz
      (button (make-id "save") "Save" 20 fillwrap
              (lambda ()

                (let ((event-id (mutate-make-event-id!)))
                  (mutate-current-field!
                   (lambda (field)
                     (display "adding event")(newline)
                     (display (field-size field))(newline)
                     (field-add-event
                      field
                      (event
                       event-id
                       (calc-type (state-calc gstate))
                       (current-date)
                       (calc-nutrients)
                       (calc-amount (state-calc gstate))
                       (calc-quality (state-calc gstate))
                       (calc-season (state-calc gstate))
                       (calc-crop (state-calc gstate))
                       (calc-soil (state-calc gstate))
                       (field-size field)
                       (current-units))))))

                (mutate-saved-data!
                 (lambda (d)
                   (saved-data-modify-field
                    (lambda (field)
                      (current-field))
                      (field-name (current-field))
                    d)))

                (list (finish-activity 99))))
      (button (make-id "cancel") "Cancel" 20 fillwrap
              (lambda () (list (finish-activity 99))))))

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (update-soil! "fc" (field-soil (current-field)))
     (update-crop! "fc" (field-crop (current-field)))
     (update-season! "fc" (date->season (current-date)))
     (append
      (if (equal? (current-units) metric) (list)
          (list
           (update-widget 'text-view (get-id "nt") 'text "N/acre")
           (update-widget 'text-view (get-id "pt") 'text "P/acre")
           (update-widget 'text-view (get-id "kt") 'text "K/acre")))
      (list
       (update-widget 'text-view (get-id "fieldcalc-title") 'text (field-name (current-field))))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "eventview"
   (let ((item (lambda (id title)
                 (horiz
                  (text-view-left (make-id (string-append id "-text")) title 20
                                  (layout 'fill-parent 'wrap-content 0.7 'left))
                  (text-view-left (make-id id) "type" 30
                                  (layout 'fill-parent 'wrap-content 0.3 'left))))))
     (vert
    (text-view (make-id "fieldview-title") "field name" 40 fillwrap)

    (item "type" "Type")
    (item "date" "Date")
    (item "eventview-amount" "Amount")
    (item "quality" "Quality")
    (item "season" "Season")
    (item "crop" "Crop")
    (item "soil" "Soil")
    (item "size" "Size")
    (item "total-amount" "Total Amount")

    (horiz
     (text-view (make-id "nt") "N/ha" 30 fillwrap)
     (text-view (make-id "pt") "P/ha" 30 fillwrap)
     (text-view (make-id "kt") "K/ha" 30 fillwrap))
     (horiz
      (text-view (make-id "fcna") "12" 30 fillwrap)
      (text-view (make-id "fcpa") "75" 30 fillwrap)
      (text-view (make-id "fcka") "55" 30 fillwrap))

     (linear-layout
      (make-id "gallery")
      'vertical
      (layout 'fill-parent 'fill-parent 1 'left)
      (list
       (button (make-id "load-gallery") "Load Gallery" 20 fillwrap
               (lambda ()
                 (let ((path (string-append
                              (field-name (current-field)) "-"
                              (number->string (event-id (current-event))) "/")))
                   (list
                    (list-files
                     (string-append "filelister-" path)
                     path
                     (lambda (images)
                       (list
                        (update-widget
                         'linear-layout (get-id "gallery") 'contents
                         (cons
                          (text-view (make-id "temp") "Gallery" 30 fillwrap)
                          (foldl
                           (lambda (image r)
                             (append
                              (list (image-view (make-id image)
                                                (string-append dirname path image)
                                                (layout 'wrap-content 240 1 'left))
                                    (spacer 10))
                              r))
                           '()
                           images))))))))))))


     (button (make-id "camera") "Camera" 20 fillwrap
             (lambda ()
               (setup-for-picture-from-event)
               (list (start-activity "camera" 2 ""))))

     (horiz
      (button (make-id "delete") "Delete" 20 (layout 'fill-parent 'wrap-content 0.7 'left)
              (lambda ()
                (list
                 (alert-dialog
                  "deleteme-event"
                  (string-append "Do you want to delete this event?")
                  (lambda (r)
                    (cond
                     ((zero? r) (list))
                     (else
                      ;; modify current field
                      (mutate-current-field!
                       (lambda (field)
                         (field-remove-event
                          field
                          (event-id (current-event)))))

                      ;; stick it in saved data
                      (mutate-saved-data!
                       (lambda (d)
                         (saved-data-modify-field
                          (lambda (field)
                            (current-field))
                          (field-name (current-field))
                          d)))

                      ;; clean out current just in case
                      (mutate-current-event! (lambda (ev) (empty-event)))
                      (list (finish-activity 99)))))))))

      (button (make-id "back") "Back" 20 (layout 'fill-parent 'wrap-content 0.3 'left)
              (lambda () (list (finish-activity 99)))))))

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (append
      (if (equal? (event-units (current-event)) metric) (list)
          (list
           (update-widget 'text-view (get-id "nt") 'text "N/acre")
           (update-widget 'text-view (get-id "pt") 'text "P/acre")
           (update-widget 'text-view (get-id "kt") 'text "K/acre")))
      (let ((type (event-type (current-event))))
        (list
         (update-widget 'text-view (get-id "fcna") 'text (list-ref (event-nutrients (current-event)) 0))
         (update-widget 'text-view (get-id "fcpa") 'text (list-ref (event-nutrients (current-event)) 1))
         (update-widget 'text-view (get-id "fcka") 'text (list-ref (event-nutrients (current-event)) 2))
         (update-widget 'text-view (get-id "type") 'text (event-type (current-event)))
         (update-widget 'text-view (get-id "date") 'text (date->string (event-date (current-event))))

         (update-widget 'text-view (get-id "eventview-amount") 'text
                        (string-append
                         (number->string (event-amount (current-event)))
                         " "
                         (if (equal? (event-units (current-event)) metric)
                             (nutrients-units (find type nutrients-metric))
                             (if (equal? (nutrients-units (find type nutrients-metric)) "m3/ha")
                                 "gallons/acre"
                                 "tons/acre"))))

         (update-widget 'text-view (get-id "total-amount") 'text
                        (string-append
                         (number->string (* (event-size (current-event))
                                            (event-amount (current-event))))
                         " "
                         (if (equal? (current-units) metric)
                             (if (equal? (nutrients-units (find type nutrients-metric)) "m3/ha")
                                 "m3" "tonnes")
                             ;; it's imperial
                             (if (equal? (nutrients-units (find type nutrients-metric)) "m3/ha")
                                 "gallons"
                                 "tonnes"))))

         (update-widget 'text-view (get-id "quality") 'text (event-quality (current-event)))
         (update-widget 'text-view (get-id "season") 'text (event-season (current-event)))
         (update-widget 'text-view (get-id "crop") 'text (event-crop (current-event)))
         (update-widget 'text-view (get-id "soil") 'text (event-soil (current-event)))
         (update-widget 'text-view (get-id "size") 'text
                        (string-append
                         (number->string (event-size (current-event))) " "
                         (if (equal? (event-units (current-event)) metric)
                             "hectares"
                             "acres")))
         (update-widget 'text-view (get-id "fieldview-title") 'text (field-name (current-field)))))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  (activity
   "camera"
   (vert
    (camera-preview (make-id "camera") (layout 295 220 1 'centre))
    (image-view (make-id "example") "test" (layout 'wrap-content 220 1 'left))
    (horiz
     (button (make-id "take-pic") "Take photo" 10 fillwrap
             (lambda ()
               (let ((path (string-append
                            (field-name (current-field))
                            "-"
                            (number->string (event-id (current-event)))
                            "/")))
                 (list
                  (make-directory path)
                  (update-widget 'camera-preview (get-id "camera") 'take-picture path)))))
     (button (make-id "back") "Back" 10 fillwrap
             (lambda ()
               (list
                (update-widget 'camera-preview (get-id "camera") 'shutdown 0)
                (finish-activity 99))))))

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list
      (update-widget 'image-view (get-id "example") 'image
                     (find-image (calc-type (current-calc))
                                 (calc-amount (current-calc))))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "about"
   (vert
    (text-view (make-id "about-title") "About" 40 fillwrap)
    (text-view (make-id "about-text")
               "Welcome to the Farm Crap App designed to help farmers make the most of your manure (slurry, FYM and poultry litter). The app contains 3 components; the calculator, the image library (to which you can add your own photos), and the record sheets. The calculator will determine the amount of crop-available key nutrients (N, P & K) within the manure at different spreading rates helping you decide how much to spread in order to meet the crop requirements, and also what this looks like."
               20 fillwrap)
    (spacer 20)
    (text-view (make-id "about-text2")
               "The image library can be used as a visual reference guide to estimate the spreading rate of manure already applied to the field and therefore calculate the amount of crop available nutrients that have been applied."
               20 fillwrap)
    (spacer 20)
    (text-view (make-id "about-text3")
               "The Farm Crap App can also be used to keep records of spreading events per field, how much manure was spread and what this translates to in terms of applied nutrients. You can also upload a photo of the spreading event. The records can be emailed to you to be stored on your computer."
               20 fillwrap)
    (spacer 20)
    (button (make-id "back") "Get started!" 20 fillwrap
            (lambda ()
              (list
               (update-widget 'camera-preview (get-id "camera") 'shutdown 0)
               (finish-activity 99)))))

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (list))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  )
