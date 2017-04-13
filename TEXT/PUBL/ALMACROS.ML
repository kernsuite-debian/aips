;-----------------------------------------------------------------------
;;  Copyright (C) 1995
;;  Associated Universities, Inc. Washington DC, USA.
;;
;;  This program is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License as
;;  published by the Free Software Foundation; either version 2 of
;;  the License, or (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public
;;  License along with this program; if not, write to the Free
;;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
;;  MA 02139, USA.
;;
;;  Correspondence concerning AIPS should be addressed as follows:
;;         Internet email: aipsmail@nrao.edu.
;;         Postal address: AIPS Project Office
;;                         National Radio Astronomy Observatory
;;                         520 Edgemont Road
;;                         Charlottesville, VA 22903-2475 USA
;-----------------------------------------------------------------------
; functions contained herein:
; renumb : renumbers a CHANGE.DOC file (asks starting one)
; txrenumb : renumbers a CHANGE.DOC after typeset (asks starting one)
; brackets:  checks that all { } are closed
; dochange: contains doachan - query to enter \cdpar, \clpar, \cspar
;                    docchan - change "1989" to "1989} {" and
;                            : docdpar, doclpar, dpcspar adds \hp stuff
;                    dodchan - change names to { } ed, edit for paired ones
;                              Moved nowhere strings, clean up top lines
; docaps : change AAA. to \hbox{AAA}. (do after doechan!!!!)
; doechan: contains doedcha  - recursive edit cdpar's
;                   doelcha  - recursive edit clpar's
;                   doescha  - recursive edit cspar's
; -------------------------------------------------------------------------
;
;  Emacs function to renumber CHANGE.DOC files
;
(defun
   (renumb counter
      (setq counter (get-tty-string "change1="))
      (while (! (error-occured (re-search-forward
         "^ [0-9][0-9][0-9][0-9]\\. ")))
         (line-to-top-of-window)
         (scroll-one-line-down)
         (backward-word)
         (sit-for 2)
         (delete-next-word)
         (insert-string (concat counter))
         (sit-for 5)
         (setq counter (+ counter 1))
      )
   )
)
;
;  Emacs function to renumber CHANGE.DOC files after type-setting
;
(defun
   (txrenumb counter
      (setq counter (get-tty-string "change1="))
      (while (! (error-occured (re-search-forward "\\c[a-z]par {")))
         (line-to-top-of-window)
         (scroll-one-line-down)
         (sit-for 2)
         (delete-next-word)
         (insert-string (concat counter))
         (sit-for 5)
         (setq counter (+ counter 1))
      )
   )
)
; Emacs function to check TEX files for paired brackets
;                                       set ( syntax to {
(defun
   (bracket-syntax
      (use-syntax-table "syn")
      (modify-syntax-entry "(}   {")
      (modify-syntax-entry "){   }")
   )
)
(defun
   (brackets   c q e
      (setq q 0)
      (bracket-syntax)
      (beginning-of-file)
      (message "look for missing close }")
      (sit-for 0)
      (while (! (error-occured (search-forward "{")))
         (backward-character)
         (set-mark)
         (setq c (forward-paren 0))
         (if (! c)
            (progn (exchange-dot-and-mark)
                   (sit-for 0)
                   (message "recursive edit, exit with ^C")
                   (recursive-edit)
                   (beginning-of-file)
                   (setq q (+ q 1))
            )
         )
      )
      (end-of-file)
      (message "look for missing open {")
      (sit-for 0)
      (while (! (error-occured (search-reverse "}")))
         (forward-character)
         (set-mark)
         (setq c (backward-paren 0))
         (if (! c)
            (progn (exchange-dot-and-mark)
                   (sit-for 0)
                   (message "recursive edit, exit with ^C")
                   (recursive-edit)
                   (end-of-file)
                   (setq q (+ q 1))
            )
         )
      )
      (if (> q 0)
         (message (concat q " brackets were fixed"))
         (message "Everything seems ok")
      )
      (novalue)
   )
)
; Emacs function to check TEX files for paired parens
;                                       set {} syntax to ()
(defun
   (paren-syntax
      (use-syntax-table "syn")
      (modify-syntax-entry "{)   (")
      (modify-syntax-entry "}(   )")
   )
)
(defun
   (parens   c q e
      (setq q 0)
      (beginning-of-file)
      (message "look for missing close )")
      (sit-for 0)
      (while (! (error-occured (search-forward "(")))
         (backward-character)
         (set-mark)
         (setq c (forward-paren 0))
         (if (! c)
            (progn (exchange-dot-and-mark)
                   (sit-for 0)
                   (message "recursive edit, exit with ^C")
                   (recursive-edit)
                   (beginning-of-file)
                   (setq q (+ q 1))
            )
         )
      )
      (end-of-file)
      (message "look for missing open (")
      (sit-for 0)
      (while (! (error-occured (search-reverse ")")))
         (forward-character)
         (set-mark)
         (setq c (backward-paren 0))
         (if (! c)
            (progn (exchange-dot-and-mark)
                   (sit-for 0)
                   (message "recursive edit, exit with ^C")
                   (recursive-edit)
                   (end-of-file)
                   (setq q (+ q 1))
            )
         )
      )
      (if (> q 0)
         (message (concat q " parens were fixed"))
         (message "Everything seems ok")
      )
      (novalue)
   )
)
(defun
   (domoved
      (end-of-file)
      (while (! (error-occured (search-reverse "        Moved")))
         (delete-white-space)
         (insert-string "    \\hp {")
         (end-of-line)
         (insert-string "}}")
         (sit-for 0)
      )
   )
)
(defun
   (docdpar
      (while (! (error-occured (search-reverse "\\cdpar")))
         (next-line 1)
         (delete-white-space)
         (insert-string "   {\\hp {")
         (search-reverse "\\cdpar")
      )
   )
)
(defun
   (docspar
      (while (! (error-occured (search-reverse "\\cspar")))
         (next-line 1)
         (delete-white-space)
         (insert-string "   {\\nhp{")
         (search-reverse "\\cspar")
         (sit-for 0)
      )
   )
)
(defun
   (doclpar
      (while (! (error-occured (search-reverse "\\clpar")))
         (next-line 1)
         (delete-white-space)
         (insert-string "   {\\nhp{")
         (search-reverse "\\clpar")
         (sit-for 0)
      )
   )
)
(defun
   (dochange
      (doachan)
      (docchan)
      (dodchan)
   )
)
(defun
   (doachan
      (while (! (error-occured (re-replace-string
         "^\\([0-9][0-9][0-9][0-9]\\)\\. *" " \\1\\.  ")))
      )
      (message "Query Replace for cdpar")
      (sit-for 20)
      (beginning-of-file)
      (sit-for 0)
      (while (! (error-occured (re-query-replace-string
"^ \\([0-9][0-9][0-9][0-9]\\)\\. *" "\\\\cdpar {\\1\\.} {")))
         (sit-for 0)
      )
      (message "Query Replace for clpar")
      (sit-for 20)
      (beginning-of-file)
      (sit-for 0)
      (while (! (error-occured (re-query-replace-string
 "^ \\([0-9][0-9][0-9][0-9]\\)\\. *" "\\\\clpar {\\1\\.} {")))
         (sit-for 0)
      )
      (message "Query Replace for cspar")
      (sit-for 20)
      (beginning-of-file)
      (sit-for 0)
      (while (! (error-occured (re-query-replace-string
"^ \\([0-9][0-9][0-9][0-9]\\)\\. *" "\\\\cspar {\\1\\.} {")))
         (sit-for 0)
      )
      (message "write file")
      (sit-for 1)
      (write-current-file "")
      (sit-for 1)
   )
)
(defun
   (domonth qmonth
      (setq qmonth (arg 1))
      (beginning-of-file)
      (message (concat "Replace months: " qmonth))
      (sit-for 1)
      (while (! (error-occured (replace-string
            (concat ".  " qmonth) (concat ".} {" qmonth))))
         (sit-for 0)
      )
   )
)
(defun
   (dobchan
      (domonth "January")
      (domonth "February")
      (domonth "March")
      (domonth "April")
      (domonth "May")
      (domonth "June")
      (domonth "July")
      (domonth "August")
      (domonth "September")
      (domonth "October")
      (domonth "November")
      (domonth "December")
      (message "write file")
      (sit-for 1)
      (write-current-file "")
      (sit-for 1)
   )
)
(defun
   (docchan
      (beginning-of-file)
      (sit-for 0)
      (message "change year 1989")
      (sit-for 1)
      (while (! (error-occured (replace-string " 1989 " " 1989} { ")))
         (sit-for 0)
      )
      (end-of-file)
      (docdpar)
      (end-of-file)
      (doclpar)
      (end-of-file)
      (docspar)
      (message "write file")
      (sit-for 1)
      (write-current-file "")
      (sit-for 1)
   )
)
(defun
   (doname qname
      (setq qname (arg 1))
      (message (concat "change names: " qname))
      (beginning-of-file)
      (sit-for 1)
      (while (! (error-occured (search-forward qname)))
         (backward-word)
         (insert-string "{")
         (forward-word)
         (insert-string "}")
      )
	)
)
(defun
   (dodchan
      (doname "Eric")
      (doname "Kerry")
      (doname "Phil")
      (doname "John")
      (doname "Neil")
      (doname "BillC")
      (doname "BillJ")
      (doname "Chris")
      (doname "Tim")
      (beginning-of-file)
      (message "check names for /{")
      (sit-for 0)
      (while (! (error-occured (search-forward "/{")))
         (sit-for 0)
         (message "recursive edit, exit with ^C")
         (recursive-edit)
      )
      (message "check names for }/")
      (sit-for 0)
      (beginning-of-file)
      (while (! (error-occured (search-forward "}/")))
         (sit-for 0)
         (message "recursive edit, exit with ^C")
         (recursive-edit)
      )
      (message "Moved nowhere. strings")
      (sit-for 1)
      (domoved)
      (beginning-of-file)
      (message "finish off the top lines 1989")
      (sit-for 10)
      (while (! (error-occured (search-forward "1989} {")))
         (sit-for 0)
         (delete-white-space)
         (search-forward "{")
         (backward-character)
         (backward-character)
         (delete-white-space)
         (insert-string "} ")
         (sit-for 0)
      )
      (message "write file")
      (sit-for 0)
      (write-current-file "")
      (sit-for 0)
   )
)
(defun
   (doechan
      (beginning-of-file)
      (doedxcha)
      (beginning-of-file)
      (doelxcha)
      (beginning-of-file)
      (doescha)
   )
)
(defun
   (doedcha
      (message "edit cdpars")
      (sit-for 1)
      (while (! (error-occured (search-forward "\\cdpar")))
         (line-to-top-of-window)
         (sit-for 0)
         (search-forward "\\hp {")
         (message "recursive edit, exit with ^C")
         (recursive-edit)
      )
      (message "write file")
      (sit-for 1)
      (write-current-file "")
      (sit-for 1)
   )
)
(defun
   (doedxcha
      (message "fully edit cdpars")
      (sit-for 1)
      (while (! (error-occured (search-forward "\\cdpar")))
         (previous-line 1)
         (set-mark)
         (next-line 2)
         (re-search-forward "\\\\c[a-z]par {\\|\\'")
         (previous-line 1)
         (narrow-region)
         (search-reverse "\\hp {")
         (backward-word)
         (end-of-line)
         (insert-string "}")
         (search-reverse "\\hp {")
         (message "query-replace(s)")
         (sit-for 0)
         (while (! (error-occured (re-query-replace-string
   " \\(AIPS\\)\\([ ]\\)" " \\\\\\1\\\\\\2")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\(AIPS\\)$" " \\\\\\1\\\\")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\(AIPS\\)\\([,;:\\.]\\)" " \\\\\\1\\2")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\([A-Z][A-Z0-9]*\\.[A-Z0-9][A-Z0-9]*\\)\\(\\.[ ]*\\)$"
   " \\\\hbox{{\\\\tt \\1}}\\2")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\([A-Z][A-Z0-9]*\\.[A-Z0-9][A-Z0-9]*\\)$"
   " {\\\\tt \\1}")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\([A-Z][A-Z0-9\\*]*\\.[A-Z0-9][A-Z0-9]*\\)\\([,;: ]\\)"
   " {\\\\tt \\1}\\2")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\([A-Z][A-Z0-9\\*]*\\.[A-Z0-9][A-Z0-9]*\\)\\(\\.[}]\\)"
   " {\\\\tt \\1}\\2")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\([A-Z][A-Z0-9\\*]*\\.[A-Z0-9][A-Z0-9]*\\)\\(\\.[ ]\\)"
   " \\\\hbox{{\\\\tt \\1}}\\2")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\([A-Z][A-Z0-9\\*]*\\)\\([,;: ]\\)" " {\\\\tt \\1}\\2")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\([A-Z][A-Z0-9\\*]*\\)\\(\\.[}]\\)" " {\\\\tt \\1}\\2")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\([A-Z][A-Z0-9\\*]*\\)\\(\\.[ ]\\)"
   " \\\\hbox{{\\\\tt \\1}}\\2")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\([A-Z][A-Z0-9\\*]*\\)$" " {\\\\tt \\1}")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\([A-Z][A-Z0-9\\*]*\\)\\(\\.\\)$"
   " \\\\hbox{{\\\\tt \\1}}\\2")))
	      )
         (search-reverse "\\cdpar {")
         (search-forward "\\hp {")
         (message "recursive edit, exit with ^C")
         (recursive-edit)
         (widen-region)
      )
      (message "write file")
      (sit-for 1)
      (write-current-file "")
      (sit-for 1)
   )
)
(defun
   (doelcha
      (message "edit clpars")
      (sit-for 1)
      (while (! (error-occured (search-forward "\\clpar")))
         (line-to-top-of-window)
         (sit-for 0)
         (search-forward "\\nhp{")
         (message "recursive edit, exit with ^C")
         (recursive-edit)
      )
      (message "write file")
      (sit-for 1)
      (write-current-file "")
      (sit-for 1)
   )
)
(defun
   (doelxcha
      (message "fully edit clpars")
      (sit-for 1)
      (while (! (error-occured (search-forward "\\clpar")))
         (previous-line 1)
         (set-mark)
         (next-line 2)
         (re-search-forward "\\\\c[a-z]par {\\|\\'")
         (previous-line 1)
         (narrow-region)
         (search-reverse "\\hp {")
         (beginning-of-line)
         (replace-string "\\hp {" "\\nhp{")
         (beginning-of-line)
         (search-reverse "\\nhp{")
         (message "query replace \\clxx { }{")
         (sit-for 0)
         (while (! (error-occured (re-query-replace-string
            "^       *\\([A-Z][A-Z0-9]*\\.*[A-Z0-9]*\\)  *"
            "    \\\\clxx{\\1}{")))
			)
         (while (! (error-occured (search-forward "\\clxx{")))
            (previous-line 1)
            (end-of-line)
            (insert-string "}")
            (search-forward "\\clxx{")
			)
         (search-reverse "\\nhp{")
         (message "query-replace(s)")
         (sit-for 0)
         (while (! (error-occured (re-query-replace-string
   " \\(AIPS\\)\\([ ]\\)" " \\\\\\1\\\\\\2")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\(AIPS\\)$" " \\\\\\1\\\\")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\(AIPS\\)\\([,;:\\.]\\)" " \\\\\\1\\2")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\([A-Z][A-Z0-9]*\\.[A-Z0-9][A-Z0-9]*\\)\\(\\.[ ]*\\)$"
   " {\\\\tt \\1}\\2")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\([A-Z][A-Z0-9]*\\.[A-Z0-9][A-Z0-9]*\\)$"
   " {\\\\tt \\1}")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\([A-Z][A-Z0-9\\*]*\\.[A-Z0-9][A-Z0-9]*\\)\\([,;: ]\\)"
   " {\\\\tt \\1}\\2")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\([A-Z][A-Z0-9\\*]*\\.[A-Z0-9][A-Z0-9]*\\)\\(\\.[}]\\)"
   " {\\\\tt \\1}\\2")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\([A-Z][A-Z0-9\\*]*\\.[A-Z0-9][A-Z0-9]*\\)\\(\\.[ ]\\)"
   " \\\\hbox{{\\\\tt \\1}}\\2")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\([A-Z][A-Z0-9\\*]*\\)\\([,;: ]\\)" " {\\\\tt \\1}\\2")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\([A-Z][A-Z0-9\\*]*\\)\\(\\.[}]\\)" " {\\\\tt \\1}\\2")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\([A-Z][A-Z0-9\\*]*\\)\\(\\.[ ]\\)"
   " \\\\hbox{{\\\\tt \\1}}\\2")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\([A-Z][A-Z0-9\\*]*\\)$" " {\\\\tt \\1}")))
	      )
         (while (! (error-occured (re-query-replace-string
   " \\([A-Z][A-Z0-9\\*]*\\)\\(\\.\\)$"
   " \\\\hbox{{\\\\tt \\1}}\\2")))
	      )
         (search-reverse "\\clpar {")
         (search-forward "\\nhp{")
         (message "recursive edit, exit with ^C")
         (recursive-edit)
         (widen-region)
      )
      (message "write file")
      (sit-for 1)
      (write-current-file "")
      (sit-for 1)
   )
)
(defun
   (doescha
      (message "edit cspars")
      (sit-for 1)
      (while (! (error-occured (search-forward "\\cspar")))
         (line-to-top-of-window)
         (sit-for 0)
         (search-forward "\\nhp{")
         (message "recursive edit, exit with ^C")
         (recursive-edit)
      )
      (message "write file")
      (sit-for 1)
      (write-current-file "")
      (sit-for 1)
   )
)
(defun
   (docaps
      (while (! (error-occured (re-query-replace-string
   " \\([A-Z][A-Z0-9\\*]*\\)\\(\\.\\)$" " \\\\hbox{\\1}\\2")))
	      )
      (while (! (error-occured (re-query-replace-string
   " \\([A-Z][A-Z0-9\\*]*\\)\\(\\. \\)" " \\\\hbox{\\1}\\2")))
	      )
	)
)
;  fixtts should not be needed if doexcha things work
(defun
   (fixtts
      (while (! (error-occured (re-query-replace-string
          " \\({\\\\tt [A-Z][A-Z0-9\\.\\*]*}\\)\\(\\.\\)$"
          " \\\\hbox{\\1}\\2")))
	      )
      (while (! (error-occured (re-query-replace-string
          " \\({\\\\tt [A-Z][A-Z0-9\\.\\*]*}\\)\\(\\. \\)"
          " \\\\hbox{\\1}\\2")))
	      )
	)
)
