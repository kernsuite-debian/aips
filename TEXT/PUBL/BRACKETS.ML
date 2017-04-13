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
