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
; Definitions for EMACS support of TEX files.
(progn
(defun	(within
		(& (<= (arg 1) (arg 2)) (<= (arg 2) (arg 3)))
	)
)
(defun	(is-alphanum p
		(setq p (arg 1))
		(| (|	(within 'A' p 'Z')
			(within 'a' p 'z')	)
			(within '0' p '9')
		)
	)
)
; Macro to surround alphanumeric text with {\tt ...}:
(defun	(textt 	(if (is-alphanum (preceding-char)) (backward-word))
		(if (is-alphanum (following-char))
			(progn
				(insert-string "{\\tt ")
				(forward-word)
				(insert-string "}")
			)
			(illegal-operation)
		)
	)
)
; Macro to surround alphanumeric text with {\it ...}:
(defun	(texit 	(if (is-alphanum (preceding-char)) (backward-word))
		(if (is-alphanum (following-char))
			(progn
				(insert-string "{\\it ")
				(forward-word)
				(insert-string "}")
			)
			(illegal-operation)
		)
	)
)
; Set up automatic "TEX-mode":
(defun	(TEX-mode
		(local-bind-to-key "texit" "\ei")
		(local-bind-to-key "texit" "\eI")
		(local-bind-to-key "textt" "\et")
		(local-bind-to-key "textt" "\eT")
		(local-bind-to-key "delete-white-space" "\ea")
		(setq mode-string "TEX")
		(setq right-margin 70)
		(setq  left-margin  1)
		(setq wrap-long-lines 1)
;			N.B.: "set-auto-fill-hook", "looking-at",
;			"modify-syntax-entry", "indent-C-procedure",
;			"send-string-to-terminal", "current-column"
		(novalue)
	)
)
(auto-execute "TEX-mode" "*.tex")
(novalue)
)
