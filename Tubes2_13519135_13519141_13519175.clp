(deftemplate cancer_attributes "Breast Cancer Attributes"
    (slot mean_concave_points (type SYMBOL) (allowed-symbols lesseq more none) (default none))
    (slot worst_radius (type SYMBOL) (allowed-symbols lesseq more none) (default none))
    (slot worst_perimeter (type SYMBOL) (allowed-symbols lesseq more none) (default none))
    (slot radius_error (type SYMBOL) (allowed-symbols lesseq more none) (default none))
    (slot mean_texture (type SYMBOL) (allowed-symbols lesseq more none) (default none))
    (slot worst_texture (type SYMBOL) (allowed-symbols lesseq more none) (default none))
    (slot mean_smoothness (type SYMBOL) (allowed-symbols lesseq more none) (default none))
    (slot concave_points_error (type SYMBOL) (allowed-symbols lesseq more none) (default none))
    (slot worst_concave_points (type SYMBOL) (allowed-symbols lesseq more none) (default none))
    (slot perimeter_error (type SYMBOL) (allowed-symbols lesseq more none) (default none))
    (slot worst_area (type SYMBOL) (allowed-symbols lesseq more none) (default none))
    (slot mean_radius (type SYMBOL) (allowed-symbols lesseq more none) (default none))
    (slot cancer (type SYMBOL) (allowed-symbols yes no))
)

(deffacts condition "Conditions leading to cancer"
    ;; Cancer, LLLL
    (cancer_attributes (mean_concave_points lesseq)
                (worst_radius lesseq)
                (radius_error lesseq)
                (worst_texture lesseq)
                (cancer yes)
    )

    ;; Cancer, LLLRL
    (cancer_attributes (mean_concave_points lesseq)
                (worst_radius lesseq)
                (radius_error lesseq)
                (worst_texture more)
                (worst_area lesseq)
                (cancer yes)
    )

    ;; Not Cancer, LLLRRLL
    (cancer_attributes (mean_concave_points lesseq)
                (worst_radius lesseq)
                (radius_error lesseq)
                (worst_texture more)
                (worst_area more)
                (mean_radius lesseq)
                (mean_texture lesseq)
                (cancer no)
    )

    ;; Cancer, LLLRRLR
    (cancer_attributes (mean_concave_points lesseq)
                (worst_radius lesseq)
                (radius_error lesseq)
                (worst_texture more)
                (worst_area more)
                (mean_radius lesseq)
                (mean_texture more)
                (cancer yes)
    )

    ;; Cancer, LLLRRR
    (cancer_attributes (mean_concave_points lesseq)
                (worst_radius lesseq)
                (radius_error lesseq)
                (worst_texture more)
                (worst_area more)
                (mean_radius more)
                (cancer yes)
    )

    ;; Cancer, LLRL
    (cancer_attributes (mean_concave_points lesseq)
                (worst_radius lesseq)
                (radius_error more)
                (mean_smoothness lesseq)
                (cancer yes)
    )
    
    ;; Not Cancer, LLRR
    (cancer_attributes (mean_concave_points lesseq)
                (worst_radius lesseq)
                (radius_error more)
                (mean_smoothness more)
                (cancer no)
    )

    ;; Cancer, LRL
    (cancer_attributes (mean_concave_points lesseq)
                (worst_radius more)
                (mean_texture lesseq)
                (cancer yes)
    )

    ;; Not Cancer, LRRL
    (cancer_attributes (mean_concave_points lesseq)
                (worst_radius more)
                (mean_texture more)
                (concave_points_error lesseq)
                (cancer no)
    )

    ;; Cancer, LRRR
    (cancer_attributes (mean_concave_points lesseq)
                (worst_radius more)
                (mean_texture more)
                (concave_points_error more)
                (cancer yes)
    )

    ;; Cancer, RLLL
    (cancer_attributes (mean_concave_points more)
                (worst_perimeter lesseq)
                (worst_texture lesseq)
                (worst_concave_points lesseq)
                (cancer yes)
    
    )

    ;; Not Cancer, RLLR
    (cancer_attributes (mean_concave_points more)
                (worst_perimeter lesseq)
                (worst_texture lesseq)
                (worst_concave_points more)
                (cancer no)
    )
    
    ;; Not Cancer, RLRLL
    (cancer_attributes (mean_concave_points more)
                (worst_perimeter lesseq)
                (worst_texture more)
                (perimeter_error lesseq)
                (mean_radius lesseq)
                (cancer no)
    )

    ;; Cancer, RLRLR
    (cancer_attributes (mean_concave_points more)
                (worst_perimeter lesseq)
                (worst_texture more)
                (perimeter_error lesseq)
                (mean_radius more)
                (cancer yes)
    )

    ;; Not Cancer, RLRR
    (cancer_attributes (mean_concave_points more)
                (worst_perimeter lesseq)
                (worst_texture more)
                (perimeter_error more)
                (cancer no)
    )

    ;; Not Cancer, RR 
    (cancer_attributes (mean_concave_points more)
                (worst_perimeter more)
                (cancer no)
    )
)

(defrule check_cancer_found
    (declare (salience -100))
    (cancer_attributes(cancer ?result))
    =>
    (
        if (eq 1 (length$ (find-all-facts ((?f cancer_attributes)) *))      )
            then (  
                    if (eq ?result yes) then (print "Result : Cancer" crlf)                
                    else (eq ?result no) then (print "Result : Not Cancer" crlf)
                 )   
    )
)

(defrule remove_mean_concave_points
	(the_mean_concave_points ?x)
	?attr <- (cancer_attributes (mean_concave_points ?y))
	=>
	(
        if (eq ?x more)
		    then (if (eq ?y lesseq) then (retract ?attr))
	    else (if (eq ?x lesseq)
		    then (if (eq ?y more) then (retract ?attr))
		)
	)
)

(defrule remove_worst_radius
	(the_worst_radius ?x)
	?attr <- (cancer_attributes (worst_radius ?y))
	=>
	(
        if (eq ?x more)
		    then (if (eq ?y lesseq) then (retract ?attr))
	    else (if (eq ?x lesseq)
		    then (if (eq ?y more) then (retract ?attr))
		)
	)
)

(defrule remove_worst_perimeter
	(the_worst_perimeter ?x)
	?attr <- (cancer_attributes (worst_perimeter ?y))
	=>
	(
        if (eq ?x more)
		    then (if (eq ?y lesseq) then (retract ?attr))
	    else (if (eq ?x lesseq)
		    then (if (eq ?y more) then (retract ?attr))
		)
	)
)

(defrule remove_radius_error
	(the_radius_error ?x)
	?attr <- (cancer_attributes (radius_error ?y))
	=>
	(
        if (eq ?x more)
		    then (if (eq ?y lesseq) then (retract ?attr))
	    else (if (eq ?x lesseq)
		    then (if (eq ?y more) then (retract ?attr))
		)
	)
)

(defrule remove_mean_texture
	(the_mean_texture ?x)
	?attr <- (cancer_attributes (mean_texture ?y))
	=>
	(
        if (eq ?x more)
		    then (if (eq ?y lesseq) then (retract ?attr))
	    else (if (eq ?x lesseq)
		    then (if (eq ?y more) then (retract ?attr))
		)
	)
)

(defrule remove_worst_texture
	(the_worst_texture ?x)
	?attr <- (cancer_attributes (worst_texture ?y))
	=>
	(
        if (eq ?x more)
		    then (if (eq ?y lesseq) then (retract ?attr))
	    else (if (eq ?x lesseq)
		    then (if (eq ?y more) then (retract ?attr))
		)
	)
)

(defrule remove_mean_smoothness
	(the_mean_smoothness ?x)
	?attr <- (cancer_attributes (mean_smoothness ?y))
	=>
	(
        if (eq ?x more)
		    then (if (eq ?y lesseq) then (retract ?attr))
	    else (if (eq ?x lesseq)
		    then (if (eq ?y more) then (retract ?attr))
		)
	)
)

(defrule remove_concave_points_error
	(the_concave_points_error ?x)
	?attr <- (cancer_attributes (concave_points_error ?y))
	=>
	(
        if (eq ?x more)
		    then (if (eq ?y lesseq) then (retract ?attr))
	    else (if (eq ?x lesseq)
		    then (if (eq ?y more) then (retract ?attr))
		)
	)
)

(defrule remove_worst_concave_points
	(the_worst_concave_points ?x)
	?attr <- (cancer_attributes (worst_concave_points ?y))
	=>
	(
        if (eq ?x more)
		    then (if (eq ?y lesseq) then (retract ?attr))
	    else (if (eq ?x lesseq)
		    then (if (eq ?y more) then (retract ?attr))
		)
	)
)

(defrule remove_perimeter_error
	(the_perimeter_error ?x)
	?attr <- (cancer_attributes (perimeter_error ?y))
	=>
	(
        if (eq ?x more)
		    then (if (eq ?y lesseq) then (retract ?attr))
	    else (if (eq ?x lesseq)
		    then (if (eq ?y more) then (retract ?attr))
		)
	)
)

(defrule remove_worst_area
	(the_worst_area ?x)
	?attr <- (cancer_attributes (worst_area ?y))
	=>
	(
        if (eq ?x more)
		    then (if (eq ?y lesseq) then (retract ?attr))
	    else (if (eq ?x lesseq)
		    then (if (eq ?y more) then (retract ?attr))
		)
	)
)

(defrule remove_mean_radius
	(the_mean_radius ?x)
	?attr <- (cancer_attributes (mean_radius ?y))
	=>
	(
        if (eq ?x more)
		    then (if (eq ?y lesseq) then (retract ?attr))
	    else (if (eq ?x lesseq)
		    then (if (eq ?y more) then (retract ?attr))
		)
	)
)

(deffunction ask-question (?question)
   (printout t ?question)
   (bind ?answer (read))
   (if (or (floatp ?answer) (integerp ?answer)) then (bind ?answer ?answer))
   (while (not (or (floatp ?answer) (integerp ?answer))) do
      (printout t ?question)
      (bind ?answer (read))
      (if (floatp ?answer) then (bind ?answer ?answer)))
   ?answer)

;; Format Pertanyaan

(defrule mean_texture_question
    (the_mean_concave_points lesseq)
    (the_worst_radius lesseq)
    (the_radius_error lesseq)
    (the_worst_texture more)
    (the_worst_area more)
    (the_mean_radius lesseq)
    =>
	(bind ?ans (ask-question "What's your mean texture measurement? "))
	(
        if (> ?ans 28.79) then (assert (the_mean_texture more))
        else (assert (the_mean_texture lesseq))
    )
)

(defrule mean_radius_question 
    (the_mean_concave_points lesseq)
    (the_worst_radius lesseq)
    (the_radius_error lesseq)
    (the_worst_texture more)
    (the_worst_area more)
    =>
	(bind ?ans (ask-question "What's your mean radius measurement? "))
	(
        if (> ?ans 13.45) then (assert (the_mean_radius more))
        else (assert (the_mean_radius lesseq))
    )
)

(defrule mean_radius_question_r
    (the_mean_concave_points more)
    (the_worst_perimeter lesseq)
    (the_worst_texture more)
    (the_perimeter_error lesseq)
    =>
	(bind ?ans (ask-question "What's your mean radius measurement? "))
	(
        if (> ?ans 13.34) then (assert (the_mean_radius more))
        else (assert (the_mean_radius lesseq))
    )
)

(defrule worst_area_question
    (the_mean_concave_points lesseq)
    (the_worst_radius lesseq)
    (the_radius_error lesseq)
    (the_worst_texture more)
    =>
	(bind ?ans (ask-question "What's your worst area measurement? "))
	(
        if (> ?ans 641.60) then (assert (the_worst_area more))
        else (assert (the_worst_area lesseq))
    )
)

(defrule perimeter_error_question
    (the_mean_concave_points more)
    (the_worst_perimeter lesseq)
    (the_worst_texture more)
    =>
	(bind ?ans (ask-question "What's your perimeter error measurement? "))
	(
        if (> ?ans 1.56) then (assert (the_perimeter_error more))
        else (assert (the_perimeter_error lesseq))
    )
)

(defrule worst_concave_points_question
    (the_mean_concave_points more)
    (the_worst_perimeter lesseq)
    (the_worst_texture lesseq)
    =>
	(bind ?ans (ask-question "What's your worst concave points measurement? "))
	(
        if (> ?ans 0.17) then (assert (the_worst_concave_points more))
        else (assert (the_worst_concave_points lesseq))
    )
)

(defrule concave_points_error_question
    (the_mean_concave_points lesseq)
    (the_worst_radius more)
    (the_mean_texture more)
    =>
	(bind ?ans (ask-question "What's your concave points error measurement? "))
	(
        if (> ?ans 0.01) then (assert (the_concave_points_error more))
        else (assert (the_concave_points_error lesseq))
    )
)

(defrule mean_smoothness_question
    (the_mean_concave_points lesseq)
    (the_worst_radius lesseq)
    (the_radius_error more)
    =>
	(bind ?ans (ask-question "What's your mean smoothness measurement? "))
	(
        if (> ?ans 0.09) then (assert (the_mean_smoothness more))
        else (assert (the_mean_smoothness lesseq))
    )
)

(defrule worst_texture_question
    (the_mean_concave_points lesseq)
    (the_worst_radius lesseq)
    (the_radius_error lesseq)
    =>
	(bind ?ans (ask-question "What's your worst texture measurement? "))
	(
        if (> ?ans 30.15) then (assert (the_worst_texture more))
        else (assert (the_worst_texture lesseq))
    )
)

(defrule worst_texture_question_r
    (the_mean_concave_points more)
    (the_worst_perimeter lesseq)
    =>
	(bind ?ans (ask-question "What's your worst texture measurement? "))
	(
        if (> ?ans 25.65) then (assert (the_worst_texture more))
        else (assert (the_worst_texture lesseq))
    )
)

(defrule mean_texture_question_r
    (the_mean_concave_points lesseq)
    (the_worst_radius more)
    =>
	(bind ?ans (ask-question "What's your mean texture measurement? "))
	(
        if (> ?ans 16.19) then (assert (the_mean_texture more))
        else (assert (the_mean_texture lesseq))
    )
)

(defrule radius_error_question
    (the_mean_concave_points lesseq)
    (the_worst_radius lesseq)
    =>
	(bind ?ans (ask-question "What's your radius error measurement? "))
	(
        if (> ?ans 0.63) then (assert (the_radius_error more))
        else (assert (the_radius_error lesseq))
    )
)

(defrule worst_perimeter_question
    (the_mean_concave_points more)   
    =>
	(bind ?ans (ask-question "What's your worst perimeter measurement? "))
	(
        if (> ?ans 114.45) then (assert (the_worst_perimeter more))
        else (assert (the_worst_perimeter lesseq))
    )
)

(defrule worst_radius_question
    (the_mean_concave_points lesseq)
    =>
	(bind ?ans (ask-question "What's your worst radius measurement? "))
	(
        if (> ?ans 16.83) then (assert (the_worst_radius more))
        else (assert (the_worst_radius lesseq))
    )
)

(defrule mean_concave_points_question
    =>
	(bind ?ans (ask-question "What's your mean concave points measurement? "))
	(
        if (> ?ans 0.05) then (assert (the_mean_concave_points more))
        else (assert (the_mean_concave_points lesseq))
    )
)