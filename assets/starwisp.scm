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

;; calculator

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
    cattle "m3" 100
    (list
     (quality "2" (nitrogen (soil (crop 8 16) (crop 48 56))  48 72 56) 30 220)
     (quality "6" (nitrogen (soil (crop 13 26) (crop 65 78)) 65 91 65) 60 290)
     (quality "10" (nitrogen (soil (crop 18 36) (crop 72 90)) 72 90 72) 90 360)))
   (nutrients
    pig "m3" 50
    (list
     (quality "2" (nitrogen (soil (crop 15 22.5) (crop 52.5 60)) 60 82.5 82.5) 25 90)
     (quality "4" (nitrogen (soil (crop 18 27) (crop 54 63)) 63 90 90) 45 110)
     (quality "6" (nitrogen (soil (crop 22 33) (crop 55 66)) 66 99 99) 65 125)))
   (nutrients
    poultry "tons" 10
    (list
     (quality "layer" (nitrogen (soil (crop 19 28.5) (crop 47.5 57)) 47.5 66.5 66.5) 84 86)
     (quality "broiler" (nitrogen (soil (crop 30 45) (crop 75 90)) (soil 60 75) 90 90) 150 162)))
   (nutrients
    FYM "tons" 50
    (list
     (quality "other" (nitrogen (soil 15 30) 30 30 30) 95 360) ;; other
     (quality "fresh" (nitrogen (soil 15 30) 30 45 30) 95 360) ;; soil inc fresh
    ))))

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
               q amount season crop soil))))))

(define (get-nutrients-inner quantity quality amount season crop soil)
  (process-nutrients
   amount
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

(define (process-nutrients amount quantity nutrients)
  (map
   (lambda (q)
     (* amount (/ q quantity)))
   nutrients))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (field name soil crop)
  (list name soil crop))
(define (field-name f) (list-ref f 0))
(define (field-modify-name f v) (list-replace f 0 v))
(define (field-soil f) (list-ref f 1))
(define (field-modify-soil f v) (list-replace f 1 v))
(define (field-crop f) (list-ref f 2))
(define (field-modify-crop f v) (list-replace f 2 v))


(define (state)
  (list
   (calc pig 25 "2" "autumn" normal mediumheavy)
   (field "" "" "")
   (saved-data-load)))

(define (state-calc s) (list-ref s 0))
(define (state-modify-calc s v) (list-replace s 0 v))
(define (state-field s) (list-ref s 1))
(define (state-modify-field s v) (list-replace s 1 v))
(define (state-saved-data s) (list-ref s 2))
(define (state-modify-saved-data s fn)
   (list-replace s 2 (saved-data-save (fn (state-saved-data s)))))

(define (saved-data fields)
  (list fields))
(define (saved-data-fields f) (list-ref f 0))
(define (saved-data-modify-fields f v) (list-replace f 0 v))

(define (saved-data-save d)
  (let ((f (open-output-file "/sdcard/swarmhub-data.scm")))
    (write d f)
    (close-output-port f))
  d)

(define (saved-data-load)
  (let* ((f (open-input-file "/sdcard/swarmhub-data.scm")))
    (if (not f)
        (saved-data-save (saved-data '()))
        (let ((r (read f)))
          (close-input-port f)
          r))))

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

(define (update-calc! fn)
  (mutate-state!
   (lambda (s)
     (state-modify-calc s (fn (state-calc s)))))
  (run-calc))

(define (update-type! v) (update-calc! (lambda (c) (calc-modify-type c v))))
(define (update-amount! v) (update-calc! (lambda (c) (calc-modify-amount c v))))
(define (update-quality! v) (update-calc! (lambda (c) (calc-modify-quality c v))))
(define (update-season! v) (update-calc! (lambda (c) (calc-modify-season c v))))
(define (update-crop! v) (update-calc! (lambda (c) (calc-modify-crop c v))))
(define (update-soil! v) (update-calc! (lambda (c) (calc-modify-soil c v))))

(define gstate (state))

(define (mutate-state! fn)
  (set! gstate (fn gstate)))

(define (run-calc)
  (let* ((type (calc-type (state-calc gstate)))
         (amount (calc-amount (state-calc gstate)))
         (quality (calc-quality (state-calc gstate)))
         (season (calc-season (state-calc gstate)))
         (crop (calc-crop (state-calc gstate)))
         (soil (calc-soil (state-calc gstate)))
         (amounts (get-nutrients type amount quality season crop soil)))
    (list
     (update-widget 'text-view (get-id "camount-value") 'text
                    (string-append (number->string amount) " "
                                   (nutrients-units (find type nutrients-metric))))
     (update-widget 'text-view (get-id "cna") 'text (number->string (list-ref amounts 0)))
     (update-widget 'text-view (get-id "cpa") 'text (number->string (list-ref amounts 1)))
     (update-widget 'text-view (get-id "cka") 'text (number->string (list-ref amounts 2))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

(define (build-field-buttons)
  (map
   (lambda (field)
     (button
      (make-id (field-name field))
      (field-name field)
      20 fillwrap
      (lambda () (list (start-activity "field" 2 (field-name field))))))
   (saved-data-fields (state-saved-data gstate))))

(define-activity-list
  (activity
   "main"
    (vert
     (text-view (make-id "title") "Swarm Hub App" 40 fillwrap)
     (text-view (make-id "title") "Your fields" 30 fillwrap)
     (linear-layout
      (make-id "fields")
      'vertical
      (layout 'fill-parent 'fill-parent 1 'left)
      (build-field-buttons))
     (button (make-id "f3") "New field" 20 fillwrap
             (lambda ()
               (list
                (start-activity "newfield" 2 ""))))
     (text-view (make-id "measure-text") "Measurements" 20 fillwrap)
     (spinner (make-id "measure") (list "Metric" "Imperial") fillwrap (lambda (v) (list)))
     (button (make-id "f2") "Calculator" 20 fillwrap
             (lambda () (list (start-activity "calc" 2 "")))))
   (lambda (activity arg)
     (display "on create")(newline)
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity)
     (display "on resume")(newline)
     (list
      (update-widget 'linear-layout (get-id "fields") 'contents
                     (build-field-buttons))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode)
     (list
      (update-widget 'linear-layout (get-id "fields") 'contents
                     (build-field-buttons)))))

  (activity
   "calc"
    (vert
     (text-view (make-id "title") "Crapp Calculator" 40 fillwrap)

     (text-view (make-id "manure-text") "Manure type" 15 fillwrap)
     (spinner (make-id "manure") (list cattle FYM pig poultry) fillwrap
              (lambda (v)
                (cons
                 (update-widget 'spinner (get-id "cquality") 'array
                                (cond
                                 ((equal? v cattle) (list "2" "6" "10"))
                                 ((equal? v pig) (list "2" "4" "6"))
                                 ((equal? v poultry) (list "layer" "broiler"))
                                 ((equal? v FYM) (list "fresh" "other"))))
                 (update-type! v))))

     (horiz
      (vert
       (text-view (make-id "soil-text") "Soil type" 15 fillwrap)
       (spinner (make-id "soil") (list sandyshallow mediumheavy) fillwrap
               (lambda (v) (update-soil! v))))
      (vert
       (text-view (make-id "crop-text") "Crop type" 15 fillwrap)
       (spinner (make-id "crop") (list normal grass-oilseed) fillwrap
                (lambda (v) (update-crop! v)))))

     (horiz
      (vert
       (text-view (make-id "season-text") "Season" 15 fillwrap)
       (spinner (make-id "season") (list autumn winter spring summer) fillwrap
               (lambda (v) (update-season! v))))
      (vert
       (text-view (make-id "quality-text") "Quality" 15 fillwrap)
       (spinner (make-id "cquality") (list "2" "4" "6") fillwrap
                (lambda (v) (update-quality! v)))))

     (text-view (make-id "amount-text") "Amount" 15 fillwrap)
     (seek-bar (make-id "amount") 100 fillwrap
               (lambda (v) (update-amount! v)))

     (text-view (make-id "camount-value") "4500 gallons" 15 fillwrap)
     ;;      (image-view (make-id "example") "test" wrap)
     (horiz
      (text-view (make-id "nt") "N" 30 fillwrap)
      (text-view (make-id "pt") "P" 30 fillwrap)
      (text-view (make-id "kt") "K" 30 fillwrap))
     (horiz
      (text-view (make-id "cna") "12" 30
                 (layout 'fill-parent 'fill-parent 1 'centre))
      (text-view (make-id "cpa") "75" 30
                 (layout 'fill-parent 'fill-parent 1 'centre))
      (text-view (make-id "cka") "55" 30
                 (layout 'fill-parent 'fill-parent 1 'centre)))
     (button (make-id "finished") "Done" 20 fillwrap
             (lambda () (list (finish-activity 99)))))

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) '())
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
      (horiz
       (button (make-id "save") "Save" 20 fillwrap
               (lambda ()
                 (display (state-field gstate))(newline)
                 (mutate-state!
                  (lambda (s)
                    (state-modify-saved-data
                     s (lambda (d)
                         (saved-data-modify-fields
                          d (cons (state-field s)
                                  (saved-data-fields d)))))))
                 (list (finish-activity 99))))
       (button (make-id "cancel") "Cancel" 20 fillwrap
               (lambda () (list (finish-activity 99)))))
      )

    (lambda (activity arg)
      (activity-layout activity))
    (lambda (activity arg) '())
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
             (list
              (drawlist-line '(255 127 127) 2 '(0 200 100 100))
              (drawlist-line '(255 127 127) 2 '(100 100 200 150))
              (drawlist-line '(127 255 127) 2 '(0 200 100 110))
              (drawlist-line '(127 255 127) 2 '(100 110 200 140))
              (drawlist-line '(127 127 255) 2 '(0 200 100 90))
              (drawlist-line '(127 127 255) 2 '(100 90 200 110))
              ))
     (text-view (make-id "events-txt") "Events" 20 fillwrap)
     (vert
      (text-view (make-id "ev1") "Pig slurry 10/07/13" 15 fillwrap)
      (text-view (make-id "ev2") "Farmyard manure 06/06/13" 15 fillwrap))
     (button (make-id "event") "New spreading event" 20 fillwrap
             (lambda () (list (start-activity "fieldcalc" 2 ""))))
     (button (make-id "back") "Back" 20 fillwrap
             (lambda () (list (finish-activity 99)))))

    (lambda (activity arg)
      (activity-layout activity))
    (lambda (activity arg)
      (display "on-start")(newline)
      (list
       (update-widget 'text-view (get-id "field-title") 'text arg)))
    (lambda (activity) '())
    (lambda (activity) '())
    (lambda (activity) '())
    (lambda (activity) '())
    (lambda (activity requestcode resultcode) '()))

  (activity
   "fieldcalc"
    (linear-layout
     (make-id "top")
     'vertical
     (layout 'fill-parent 'fill-parent 1 'left)
     (list
      (text-view (make-id "title") "Long Meadow" 40 fillwrap)
      (text-view (make-id "title") "New spreading event" 30 fillwrap)
      (text-view (make-id "manure-text") "Manure type" 15 fillwrap)
      (spinner (make-id "manure") (list "Cattle slurry" "Farmyard manure" "Pig slurry" "Poultry litter") fillwrap
               (lambda (v) (list)))
      (text-view (make-id "amount-text") "Amount" 15 fillwrap)
      (seek-bar (make-id "amount") 100 fillwrap
                (lambda (v) (list)))
      (text-view (make-id "amount-value") "4500 gallons" 15 fillwrap)
      (canvas (make-id "graph")
              (layout 'fill-parent 100 1 'centre)
              (list
               (drawlist-line '(255 127 127) 2 '(0 100 100 50))
               (drawlist-line '(255 127 127) 2 '(100 50 200 75))
               (drawlist-line '(255 127 127) 2 '(200 75 300 20))
               (drawlist-line '(127 255 127) 2 '(0 100 100 55))
               (drawlist-line '(127 255 127) 2 '(100 55 200 70))
               (drawlist-line '(127 255 127) 2 '(200 70 300 25))
               (drawlist-line '(127 127 255) 2 '(0 100 100 60))
               (drawlist-line '(127 127 255) 2 '(100 60 200 55))
               (drawlist-line '(127 127 255) 2 '(200 55 300 34))
               ))
      (image-view (make-id "example") "testhalf" wrap)
      (button (make-id "camera") "Camera" 20 fillwrap
              (lambda () (list (start-activity "camera" 2 ""))))
      (linear-layout
       (make-id "upper-out")
       'horizontal
       (layout 'fill-parent 'fill-parent 1 'left)
       (list
        (text-view (make-id "nt") "N" 30 fillwrap)
        (text-view (make-id "pt") "P" 30 fillwrap)
        (text-view (make-id "kt") "K" 30 fillwrap)))
      (linear-layout
       (make-id "lower-out")
       'horizontal
       (layout 'fill-parent 'fill-parent 1 'left)
       (list
        (text-view (make-id "na") "12" 50 fillwrap)
        (text-view (make-id "pa") "75" 50 fillwrap)
        (text-view (make-id "ka") "55" 50 fillwrap)))

      (linear-layout
       (make-id "out")
       'horizontal
       (layout 'fill-parent 'fill-parent 1 'left)
       (list
        (button (make-id "save") "Save" 20 fillwrap
                (lambda () (list (finish-activity 99))))
        (button (make-id "cancel") "Cancel" 20 fillwrap
                (lambda () (list (finish-activity 99))))))))

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "camera"
   (linear-layout
     (make-id "top")
     'vertical
     (layout 'fill-parent 'fill-parent 1 'left)
     (list
      (text-view (make-id "camera") "Camera preview image" 50 fillwrap)
      (image-view (make-id "example") "test" wrap)
      (button (make-id "back") "Back" 20 fillwrap
              (lambda () (list (finish-activity 99))))))

   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  )
