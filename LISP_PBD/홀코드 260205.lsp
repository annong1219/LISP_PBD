(defun c:PBD ( / )
  (vl-cmdf "_opendcl")
  (setvar "cmdecho" 0)
  (vl-load-com)
  ;(dcl_Project_import mv_tools_ge nil nil)
  (dcl_LoadProject "PBD_Tools.odcl" T)
  (dcl_FORM_SHOW PBD_Tools)
  (princ)
)

;구역명
(defun c:area#OnClicked (/)
  (setq a_txt (cdr (assoc 1 (entget (car (entsel))))))
  (setq a_txt (vl-string-translate "()" "  " a_txt))
  (setq a_txt (vl-string-trim " " a_txt))
  (dcl-Control-SetText area_text a_txt)
)

;심도
(defun c:depth#OnClicked (/)
  (setq d_txt (cdr (assoc 1 (entget (car (entsel))))))
  (dcl-Control-SetText depth_text d_txt)
)

;간격
(defun c:gap#OnClicked (/)
  (setq g_txt (cdr (assoc 1 (entget (car (entsel))))))
  (dcl-Control-SetText gap_text g_txt)
)

;가로
(defun c:length_num#OnDropDown (/)
  (setq length_num_lst (list "1" "A"))
  (dcl-Control-SetList length_num length_num_lst)
)

;세로
(defun c:width_alp#OnDropDown (/)
  (setq width_alp_lst (list "A" "1"))
  (dcl-Control-SetList width_alp width_alp_lst)
)

;;가로 정렬
(defun subTextSort (ss / tH sL nL tp Y tL L) 
  (defun sAs (num x) (cdr (assoc num x)))
  (defun sE (x) (vl-remove-if 'listp (mapcar 'cadr (ssnamex x))))
  (defun sSo (x) (vl-sort x '(lambda (x1 x2) (> (cadar x1) (cadar x2)))))
  (defun sXS (x) 
    (if (= 1 (dcl-Control-GetValue button01))
      (vl-sort x '(lambda (y1 y2) (< (caaar y1) (caaar y2))))
      (vl-sort x '(lambda (y1 y2) (> (caaar y1) (caaar y2))))
    )
  )
  (defun sRan (x) (if (< (- (car x) tH) Y (+ (car x) tH)) T nil))
  (defun sLs (x) (if (sRan (car x)) (setq sL (cons x sL)) (setq tp (cons x tp))))
  (defun sLf (x) 
    (setq Y (caar x))
    (mapcar 'sLs nL)
    (setq sL (sSo sL)
          tL (cons sL tL)
          nL tp
          sL '()
          tp '()
    )
  )
  (defun sA (x) (list (cdr (assoc 1 (entget (cadr x)))) (cadr x)))
  (setq L (mapcar '(lambda (x) (list (cdr (assoc 10 (entget x))) x)) (sE ss)))
  (setq L  (sSo L)
        tH (sAs 40 (entget (cadar L)))
        nL L
  )
  (mapcar '(lambda (x) (if (member x nL) (sLf x))) L)
  (sXS tL)
)

;;세로 정렬
(defun subTextSort2 (ss / tH sL nL tp Y tL L)
  (defun sAs (num x) (cdr (assoc num x)))
  (defun sE (x) (vl-remove-if 'listp (mapcar 'cadr (ssnamex x))))
  (defun sSo (x) (vl-sort x '(lambda (x1 x2) (< (caar x1) (caar x2)))))
  (defun sXS (x) 
    (if (= 1 (dcl-Control-GetValue button03))
      (vl-sort x '(lambda (y1 y2) (< (cadaar y1) (cadaar y2))))
      (vl-sort x '(lambda (y1 y2) (> (cadaar y1) (cadaar y2))))
    )
  )
  (defun sRan (x) (if (< (- (cadr x) tH) Y (+ (cadr x) tH)) T nil)) ;; XY 정렬
  (defun sLs (x) (if (sRan (car x)) (setq sL (cons x sL)) (setq tp (cons x tp))))
  (defun sLf (x) 
    (setq Y (cadar x)) ;;cadar
    (mapcar 'sLs nL)
    (setq sL (sSo sL)
          tL (cons sL tL)
          nL tp
          sL '()
          tp '()
    )
  )
  (defun sA (x) (list (cdr (assoc 1 (entget (cadr x)))) (cadr x)))
  (setq L (mapcar '(lambda (x) (list (cdr (assoc 10 (entget x))) x)) (sE ss)))
  (setq L  (sSo L)
        tH (sAs 40 (entget (cadar L)))
        nL L
  )
  (mapcar '(lambda (x) (if (member x nL) (sLf x))) L)
  (sXS tL)
)

;문자생성
(defun TextMake (pt n ly) 
  (entmakex
    (list (cons 0 "TEXT") 
          (cons 10 pt)
          (cons 40 0.3)
          (cons 1 n)
          (cons 62 2)
          (cons 8 ly)
    )
  )
)

;텍스트 자리수 변경
(defun three_text (tx)
 (setq T_len (strlen tx))
  (cond
    ((= T_len 1)(setq tx (strcat "00" tx)))
    ((= T_len 2)(setq tx (strcat "0" tx)))
    ((= T_len 3)(setq tx tx))
  )
  tx
)

;;AA 이후 알파벳 생성
(defun LM:A++ ( a )
    (   (lambda ( f ) (vl-list->string (reverse (f (reverse (vl-string->list a)) t))))
        (lambda ( l x )
            (cond
                (   (null l) (if x '(65) '(97)))
                (   (= 090 (car l)) (cons 65 (f (cdr l)  t )))
                (   (= 122 (car l)) (cons 97 (f (cdr l) nil)))
                (   (cons (1+ (car l)) (cdr l)))
            )
        )
    )
)

(defun c:select#OnClicked (/)
  (setq lay (dcl-Control-GetText area_text))
  (setq ss (ssget '((0 . "circle"))))
  (setq txtLst (subTextSort ss))
  (setq txtLst2 (subTextSort2 ss))
  (setq n (dcl-Control-GetText length_num))
  (if (wcmatch n "~*[~0-9]*")
                (setq fun (lambda ( x ) (itoa (1+ (atoi x)))))
                (setq fun LM:A++)
  )
  (setq txt (dcl-Control-GetText width_alp))
  (if (wcmatch txt "~*[~0-9]*")
                (setq fun2 (lambda ( x ) (itoa (1+ (atoi x)))))
                (setq fun2 LM:A++)
  )
  (setq nlst nil Alst nil)
  (mapcar 
      '(lambda (x)
        (mapcar 
          '(lambda (a)
            (setq Alst (append Alst (list (list (car a) (three_text txt)))))
          )
          x
        )
        (setq txt (fun2 txt))
      )
    txtLst2
  )
  (mapcar 
    '(lambda (x)
      (mapcar 
        '(lambda (a)
          (setq nlst (append nlst (list (list (car a) (three_text n)))))
        )
        x
      )
      (setq n (fun n))
    )
    txtLst
  )
  (if (wcmatch (cadar nlst) "~*[~0-9]*")
    (setq numlst nlst txlst Alst)
    (setq numlst Alst txlst nlst)
  )
  (mapcar 
    '(lambda (x) 
       (mapcar 
         '(lambda (a) 
            (if (/= nil (member (car a) x)) 
              (progn 
                (setq pt (car a))
                (setq tx (strcat (cadr x) "-" (cadr a)))
                (TextMake pt tx lay)
              )
            )
          )
         txlst
       )
     )
    numlst
  )
  (princ)
)

;; 원본 함수에서 사용된 함수들을 미리 정의해야 합니다.
;; (defun subTextSort (ss) ...)
;; (defun subTextSort2 (ss) ...)
;; (defun three_text (tx) ...)
;; (defun TextMake (pt tx lay) ...)
;; (defun LM:A++ (s) ...)

; (defun c:select#OnClicked_Optimized (/)
;   ;; ====================================================================
;   ;; 1. 변수 초기화 및 DCL 값 가져오기
;   ;; 모든 변수를 지역변수로 선언하여 다른 함수와의 충돌을 방지합니다.
;   ;; ====================================================================
;   (vl-load-com)
;   (setq *acad (vlax-get-acad-object))
;   (setq *doc (vla-get-ActiveDocument *acad))
;   (vla-StartUndoMark *doc)

;   (princ "\n정리할 원(Circle) 객체를 선택하세요: ")
;   (if (setq ss (ssget '((0 . "CIRCLE"))))
;     (progn
;       (setq lay (dcl-Control-GetText area_text)
;             n   (dcl-Control-GetText length_num)
;             txt (dcl-Control-GetText width_alp)
;             fun (if (wcmatch n "~*[~0-9]*") '(lambda (x) (itoa (1+ (atoi x)))) 'LM:A++)
;             fun2 (if (wcmatch txt "~*[~0-9]*") '(lambda (x) (itoa (1+ (atoi x)))) 'LM:A++)
;             numlst nil
;             Alst nil
;       )

;       ;; ====================================================================
;       ;; 2. 숫자 리스트와 알파벳 리스트를 효율적으로 생성
;       ;; 'append' 대신 'cons'를 사용하고, 불필요한 mapcar 대신 foreach 사용
;       ;; ====================================================================
;       (setq txtLst (subTextSort ss))
;       (setq txtLst2 (subTextSort2 ss))

;       ;; 숫자 리스트 생성 (e.g., '((pt1 "001") (pt2 "001") (pt3 "002") ...))
;       (foreach row txtLst
;         (foreach item row
;           (setq numlst (cons (list (car item) (three_text n)) numlst))
;         )
;         (setq n (fun n))
;       )
      
;       ;; 알파벳 리스트(연관 리스트) 생성 (e.g., '((pt1 "A") (pt2 "A") (pt3 "B") ...))
;       (foreach row txtLst2
;         (foreach item row
;           (setq Alst (cons (list (car item) (three_text txt)) Alst))
;         )
;         (setq txt (fun2 txt))
;       )

;       ;; ====================================================================
;       ;; 3. 최종 텍스트 생성 (가장 큰 성능 개선 지점)
;       ;; 3중 mapcar 대신, numlst를 한번만 순회하며 Alst에서 `assoc`으로 값을 바로 찾아옴
;       ;; 복잡도: O(n*m)  ==>  O(n) 으로 개선
;       ;; ====================================================================
;       (foreach num_pair (reverse numlst) ; 정방향으로 처리하기 위해 reverse
;         (setq pt (car num_pair))
;         (setq num_str (cadr num_pair))
        
;         ;; Alst에서 현재 점(pt)과 일치하는 알파벳 쌍(alpha_pair)을 찾음
;         (if (setq alpha_pair (assoc pt Alst))
;           (progn
;             (setq alpha_str (cadr alpha_pair))
;             ;; 원본 코드의 (strcat (cadr x) "-" (cadr a)) 순서를 따름
;             (setq final_text (strcat num_str "-" alpha_str)) 
;             (TextMake pt final_text lay)
;           )
;         )
;       )
;     )
;     (princ "\n선택된 원이 없습니다.")
;   )
;   (vla-EndUndoMark *doc)
;   (princ)
; )

(defun c:export#OnClicked (/)
  (setq ss (ssget (list (cons 0 "TEXT")(cons 40 0.3))))
  (setq GE_obj (vlax-get-or-create-object "Excel.Application"))
  (setq obj2 (vlax-variant-value (vlax-invoke-method GE_obj 'inputbox "좌표 형식 범위를 선택하세요" "좌표선택" "$A$1" nil nil nil nil 8)))
  (setq i 0)
  (repeat (sslength ss)
  (setq en (entget (ssname ss i)))
  (setq pt (cdr (assoc 10 en)))
  (setq x (rtos (nth 0 pt) 2 3))
	(setq y (rtos (nth 1 pt) 2 3))
  (setq lay (cdr (assoc 8 en)))
  (setq txt (cdr (assoc 1 en)))
  (setq iap (append (list lay)(list txt)(list "")(list "")
                       (list y)(list x)))
  (setq i (1+ i))
  (fun:odputrowvalue obj2 i 1 iap)
  )
)

(defun fun:odputrowvalue ( #GE_cur @row @col #GE_ap /)
	(mapcar '(lambda (#val)
		(vlax-put-property #GE_cur 'item @row @col #val)
		(setq @col (+ 1 @col)))#GE_ap)
)