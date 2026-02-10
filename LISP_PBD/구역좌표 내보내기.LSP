(defun fun:odputrowvalue (@cells @row @col @valuelist /)
	(foreach #val @valuelist
		(vlax-put-property @cells 'item @row @col #val)
		(setq @col (+ 1 @col))
	)
)

(defun get_ent_pro(en / ent k xy_list dist sp)
;;;;;;;;;;;;;  Entity 연장 및 Xy_list 반환 ;;;;;;;
(setq ent (if (= (type en) 'ENAME) (entget en) en)
	sp (cdr (assoc 10 ent))
	dist 0
)
(foreach k ent
	(if (= (car k) 10)
		(setq xy_list (append xy_list (list (cdr k)))
			dist (+ dist (distance sp (cdr k)))
			sp (cdr k)
		)
	)
)
xy_list
)

(defun c:rct (/)
  (setvar "cmdecho" 0)
  (vl-load-com)
  
	(setq #app_obj (vlax-get-object "Excel.Application"))
  (setq objtype (ssget (list (cons 0 "LWPOLYLINE")(cons 62 2))))
	(setq #rg_obj (vlax-variant-value (vlax-invoke-method #app_obj 'inputbox "좌표 형식 범위를 선택하세요" "좌표선택" nil nil nil nil nil 8)))
  
  (setq ss_num (sslength objtype))
  (setq i 0)
  (setq #rowcnt 1)
  (repeat ss_num
    (setq ss_en (entget (ssname objtype i)))
    (setq ss_ent (cdr (assoc -1 ss_en)))
    (setq repeat_num (+ 1 (fix (vlax-curve-getendparam ss_ent))))
    (setq lst (get_ent_pro ss_ent))
    ; (if (= nil (setq text_ss (ssget "cp" lst (list (cons 0 "TEXT")))))
    ;   (progn
        ; (setq Ept (Vlax-Curve-GetEndPoint ss_ent))
        (command "DRAWORDER" ss_ent "" "f")
        (vl-cmdf "zoom" "o" ss_ent "")
        (COMMAND "change" ss_ent "" "p" "c" "1" "")
        (setq t_code (cdr (assoc 1 (entget (car (entsel))))))
    ;   )
    ;   (progn
    ;     (setq t_s_num (sslength text_ss))
    ;     (if (= t_s_num 1)
    ;       (setq t_code (CDR (assoc 1 (entget (ssname text_ss 0)))))
    ;       (progn
    ;         (vl-cmdf "zoom" "c" Ept 10)
    ;         (COMMAND "change" ss_ent "" "p" "c" "1" "")
    ;         (setq t_code (cdr (assoc 1 (entget (car (entsel))))))
    ;       )
    ;     )
    ;   )
    ; )
    (setq n 0 )
    (setq count 1)
    (repeat repeat_num
      (setq #cood (vlax-curve-getpointatparam ss_ent n))
      (setq aaa (list #cood))
      (setq x_code (rtos (nth 0 #cood) 2 3))
      (setq y_code (rtos (nth 1 #cood) 2 3))
      (setq #append (append (list count) (list y_code) (list x_code) (list t_code)))
      (fun:odputrowvalue #rg_obj #rowcnt 1 #append)
      (setq #rowcnt (+ 1 #rowcnt))
      (setq n (1+ n))
      (setq count (1+ count))
      (if (and (= repeat_num count)(eq #cood (vlax-curve-getpointatparam ss_ent 0)))
        (progn
          (setq #cood (vlax-curve-getpointatparam ss_ent 0))
          (setq aaa (list #cood))
          (setq x_code (rtos (nth 0 #cood) 2 3))
          (setq y_code (rtos (nth 1 #cood) 2 3))
          (setq #append2 (append (list (1+ count)) (list y_code) (list x_code) (list t_code)))
          (fun:odputrowvalue #rg_obj #rowcnt 1 #append2)
          (setq #rowcnt (+ 1 #rowcnt))
        )
      )
      
    )
    (setq i (1+ i))
    (COMMAND "change" ss_ent "" "p" "c" "3" "")
    (setq lst nil)
  )
  (PRINC)
)


